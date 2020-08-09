//! Abstract Syntax Tree for UCUM units.

use super::dimension::base::DIMLESS;
use super::*;
use std::str::from_utf8_unchecked;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AST<'a> {
    Factor(&'a str),
    Unit {
        label: &'a [u8],
        exponent: i8,
        annotation: Option<&'a [u8]>,
    },
    Product(Box<AST<'a>>, Box<AST<'a>>),
    Division(Box<AST<'a>>, Box<AST<'a>>),
}

impl<'a> AST<'a> {
    pub fn factor(txt: &[u8]) -> Box<AST> {
        assert!(txt.is_ascii());
        let txt: &str = unsafe { from_utf8_unchecked(txt) };
        Box::new(AST::Factor(txt))
    }

    pub fn unit(label: &'a [u8], exponent: i8, annotation: Option<&'a [u8]>) -> Box<AST<'a>> {
        Box::new(AST::Unit {
            label,
            exponent,
            annotation,
        })
    }

    pub fn product(lhs: Box<AST<'a>>, rhs: Box<AST<'a>>) -> Box<AST<'a>> {
        Box::new(AST::Product(lhs, rhs))
    }

    pub fn division(lhs: Box<AST<'a>>, rhs: Box<AST<'a>>) -> Box<AST<'a>> {
        Box::new(AST::Division(lhs, rhs))
    }

    pub fn make_quantity<T>(&self, value: T, system: &UnitSystem<T>) -> UcumResult<'a, Quantity<T>>
    where
        T: Clone
            + Div<T, Output = T>
            + From<i32>
            + From<T>
            + FromStr
            + Mul<T, Output = T>
            + MulAssign,
        <T as FromStr>::Err: std::error::Error + 'static,
    {
        if let AST::Unit {
            label, exponent, ..
        } = self
        {
            if !label.is_empty() && *exponent == 1 {
                let label_str: &str = unsafe { from_utf8_unchecked(*label) };
                // handle special unit correctly
                let (pd, ud) = system
                    .get_descriptions(label_str)
                    .ok_or_else(|| UcumError::new("unrecogized unit label", label, 0))?;
                let value: T = if let Some(pd) = pd {
                    pd.value() * value
                } else {
                    value
                };
                return Ok(ud.make_quantity(value));
            }
        }
        let mut q = self.as_unit_quantity(system)?;
        q *= value;
        Ok(q)
    }

    pub fn as_unit_quantity<T>(&self, system: &UnitSystem<T>) -> UcumResult<'a, Quantity<T>>
    where
        T: Clone
            + Div<T, Output = T>
            + From<i32>
            + From<T>
            + FromStr
            + Mul<T, Output = T>
            + MulAssign,
        <T as FromStr>::Err: std::error::Error + 'static,
    {
        match self {
            AST::Factor(txt) => txt
                .parse()
                .map(|value| Quantity::new(value, DIMLESS))
                .map_err(|e| UcumError::from("Can not parse factor").with_cause(Box::new(e))),
            AST::Unit {
                label, exponent, ..
            } => {
                if label.is_empty() {
                    return Ok(Quantity::new(1.into(), DIMLESS));
                }
                let label_str: &str = unsafe { from_utf8_unchecked(*label) };
                let (pd, ud) = system
                    .get_descriptions(label_str)
                    .ok_or_else(|| UcumError::new("unrecogized unit label", label, 0))?;
                let mut q = ud.quantity();
                if let Some(pd) = pd {
                    q *= pd.value();
                }
                if *exponent != 1 {
                    q.raise(*exponent)
                }
                Ok(q)
            }
            AST::Product(lhs, rhs) => {
                Ok(lhs.as_unit_quantity(system)? * rhs.as_unit_quantity(system)?)
            }
            AST::Division(lhs, rhs) => {
                Ok(lhs.as_unit_quantity(system)? / rhs.as_unit_quantity(system)?)
            }
        }
    }
}
