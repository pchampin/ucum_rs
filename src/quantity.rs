//! A Quantity has a magnitude and a dimension.

use super::*;
use std::cmp::{Ordering, PartialOrd};
use std::fmt;
use std::ops::{DivAssign, MulAssign};

#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct Quantity<T> {
    /// The magnitude of this quantity (compared to the base units)
    magnitude: T,
    /// The dimension of this quantity
    dimension: Dimension,
}

impl<T> Quantity<T> {
    pub fn new(magnitude: T, dimension: Dimension) -> Self {
        Quantity {
            magnitude,
            dimension,
        }
    }

    #[inline]
    pub fn magnitude(&self) -> T
    where
        T: Clone,
    {
        self.magnitude.clone()
    }

    #[inline]
    pub fn borrow_magnitude(&self) -> &T {
        &self.magnitude
    }

    #[inline]
    pub fn dimension(&self) -> Dimension {
        self.dimension
    }

    pub fn cast<U>(self) -> Quantity<U>
    where
        U: From<T>,
    {
        Quantity {
            magnitude: self.magnitude.into(),
            dimension: self.dimension,
        }
    }

    pub fn raise(&mut self, power: i8)
    where
        T: Clone + Div<T, Output = T> + From<i32> + MulAssign,
    {
        self.dimension.raise(power);
        let mut tmp: T = 1.into();
        if power < 0 {
            self.magnitude = tmp.clone() / self.magnitude.clone();
        }
        std::mem::swap(&mut self.magnitude, &mut tmp);
        let mut i = power.abs();
        while i > 0 {
            self.magnitude *= tmp.clone();
            i -= 1;
        }
    }

    #[inline]
    pub fn raised(&self, power: i8) -> Quantity<T>
    where
        T: Clone + Div<T, Output = T> + From<i32> + MulAssign,
    {
        let mut r = self.clone();
        r.raise(power);
        r
    }

    pub fn invert(&mut self)
    where
        T: Clone + Div<T, Output = T> + From<i32> + MulAssign,
    {
        self.raise(-1)
    }

    #[inline]
    pub fn inverted(&self) -> Quantity<T>
    where
        T: Clone + Div<T, Output = T> + From<i32> + MulAssign,
    {
        self.raised(-1)
    }

    #[inline]
    pub fn abs(&self) -> Quantity<T>
    where
        T: Clone + From<i32> + MulAssign + PartialOrd,
    {
        let mut r = self.clone();
        if r.magnitude < 0.into() {
            r.magnitude *= (-1).into();
        }
        r
    }
}

impl<T> Mul<Quantity<T>> for f64
where
    T: Clone + From<f64> + Mul<T, Output = T>,
{
    type Output = Quantity<T>;

    fn mul(self, mut rhs: Quantity<T>) -> Quantity<T> {
        let lhs: T = self.into();
        rhs.magnitude = lhs * rhs.magnitude.clone();
        rhs
    }
}

impl<T> Mul<Quantity<T>> for i32
where
    T: Clone + From<i32> + Mul<T, Output = T>,
{
    type Output = Quantity<T>;

    fn mul(self, mut rhs: Quantity<T>) -> Quantity<T> {
        let lhs: T = self.into();
        rhs.magnitude = lhs * rhs.magnitude.clone();
        rhs
    }
}

/// # Panic
/// Both self and rhs must have a standard dimension, otherwise this operation will panic.
impl<T> Mul<Quantity<T>> for Quantity<T>
where
    T: Mul<T, Output = T>,
{
    type Output = Quantity<T>;

    fn mul(self, rhs: Quantity<T>) -> Quantity<T> {
        Quantity {
            magnitude: self.magnitude * rhs.magnitude,
            dimension: self.dimension * rhs.dimension,
        }
    }
}

/// # Panic
/// Both self and rhs must have a standard dimension, otherwise this operation will panic.
impl<T> Mul<Dimension> for Quantity<T> {
    type Output = Quantity<T>;

    fn mul(self, rhs: Dimension) -> Quantity<T> {
        Quantity {
            magnitude: self.magnitude,
            dimension: self.dimension * rhs,
        }
    }
}

impl<T, U> MulAssign<U> for Quantity<T>
where
    T: MulAssign<T> + From<U>,
{
    fn mul_assign(&mut self, rhs: U) {
        self.magnitude *= rhs.into();
    }
}

impl<T> Div<f64> for Quantity<T>
where
    T: Clone + From<f64> + Div<T, Output = T>,
{
    type Output = Quantity<T>;

    fn div(mut self, rhs: f64) -> Quantity<T> {
        let rhs: T = rhs.into();
        self.magnitude = self.magnitude / rhs;
        self
    }
}

impl<T> Div<i32> for Quantity<T>
where
    T: Clone + From<i32> + Div<T, Output = T>,
{
    type Output = Quantity<T>;

    fn div(mut self, rhs: i32) -> Quantity<T> {
        let rhs: T = rhs.into();
        self.magnitude = self.magnitude / rhs;
        self
    }
}

/// # Panic
/// Both self and rhs must have a standard dimension, otherwise this operation will panic.
impl<T> Div<Quantity<T>> for Quantity<T>
where
    T: Div<T, Output = T>,
{
    type Output = Quantity<T>;

    fn div(self, rhs: Quantity<T>) -> Quantity<T> {
        Quantity {
            magnitude: self.magnitude / rhs.magnitude,
            dimension: self.dimension / rhs.dimension,
        }
    }
}

/// # Panic
/// Both self and rhs must have a standard dimension, otherwise this operation will panic.
impl<T: fmt::Display> Div<Dimension> for Quantity<T> {
    type Output = Quantity<T>;

    fn div(self, rhs: Dimension) -> Quantity<T> {
        Quantity {
            magnitude: self.magnitude,
            dimension: self.dimension / rhs,
        }
    }
}

impl<T, U> DivAssign<U> for Quantity<T>
where
    T: DivAssign<T> + From<U>,
{
    fn div_assign(&mut self, rhs: U) {
        self.magnitude /= rhs.into();
    }
}

impl<T> PartialOrd for Quantity<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Quantity<T>) -> Option<Ordering> {
        if self.dimension == other.dimension {
            self.magnitude.partial_cmp(&other.magnitude)
        } else {
            None
        }
    }
}

/*
impl<T> PartialOrd<f64> for Quantity<T>
where
    T: PartialOrd<f64>,
{
    fn partial_cmp(&self, other: &f64) -> Option<Ordering> {
        if self.dimension == DIMLESS {
            self.magnitude.partial_cmp(&other)
        } else {
            None
        }
    }
}
*/

impl<T> fmt::Display for Quantity<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.magnitude, self.dimension)?;
        Ok(())
    }
}
