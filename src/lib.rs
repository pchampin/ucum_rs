//! UCUM is the [Unified Code for Units of Measure](https://unitsofmeasure.org/).
//!
//! This crate is a (partial) implementation of the revision 442 of the
//! [UCUM specification](http://unitsofmeasure.org/ucum.html).
//!
//! # Quick start
//! ```
//! use ucum::prelude::*;
//!
//! let system = UnitSystem::<f64>::default();
//! let q1 = system.parse("35.5 km/h")?;
//! let q2 = system.parse("1.1e1 m.s-1")?;
//! assert!(q1 < q2);
//! assert!(2*q1 > q2);
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! # Implementation status
//!
//! ## Syntax and semantics
//!
//! + [x] Parser for UCUM complex units
//! + [x] Semantics of standard units
//! + [x] Semantics of [special units](http://unitsofmeasure.org/ucum.html#section-Special-Units-on-non-ratio-Scales)
//! + [ ] Semantics of [arbitrary units](http://unitsofmeasure.org/ucum.html#section-Arbitrary-Units)
//!
//! ## Unit tables
//!
//! + [x] [Table 1: prefix symbols](http://unitsofmeasure.org/ucum.html#prefixes)
//! + [x] [Table 2: base units](http://unitsofmeasure.org/ucum.html#baseunits)
//! + [x] [Table 3: dimensionless units](http://unitsofmeasure.org/ucum.html#prefixes)
//! + [x] [Table 4: SI units](http://unitsofmeasure.org/ucum.html#prefixes)
//! + [x] [Table 5: Other units from ISO 1000, ISO 2955, and some from ANSI X3.50](http://unitsofmeasure.org/ucum.html#iso1000)
//! + [ ] [Table 6: Natural units](http://unitsofmeasure.org/ucum.html#const)
//! + [ ] [Table 7: CGS units](http://unitsofmeasure.org/ucum.html#cgs)
//! + [ ] [Table 8: International customary units](http://unitsofmeasure.org/ucum.html#intcust)
//! + [ ] [Table 9: Older U.S. “survey” lengths (also called "statute" lengths)](http://unitsofmeasure.org/ucum.html#us-lengths)
//! + [ ] [Table 10: British Imperial lengths](http://unitsofmeasure.org/ucum.html#brit-length)
//! + [ ] [Table 11: U.S. volumes including so called “dry measures”](http://unitsofmeasure.org/ucum.html#us-volumes)
//! + [ ] [Table 12: British Imperial volumes](http://unitsofmeasure.org/ucum.html#brit-volumes)
//! + [ ] [Table 13: Avoirdupois weights](http://unitsofmeasure.org/ucum.html#avoirdupois)
//! + [ ] [Table 14: Troy weights](http://unitsofmeasure.org/ucum.html#troy)
//! + [ ] [Table 15: Apothecaries' weights](http://unitsofmeasure.org/ucum.html#apoth)
//! + [ ] [Table 16: Units used in typesetting](http://unitsofmeasure.org/ucum.html#typeset)
//! + [ ] [Table 17: Other Units for Heat and Temperature](http://unitsofmeasure.org/ucum.html#heat)
//! + [ ] [Table 18: Units Used Predominantly in Clinical Medicine](http://unitsofmeasure.org/ucum.html#clinical)
//! + [ ] [Table 19: Units used in Chemical and Biomedical Laboratories](http://unitsofmeasure.org/ucum.html#chemical)
//! + [ ] [Table 20: Levels](http://unitsofmeasure.org/ucum.html#levels)
//! + [ ] [Table 21: Miscellaneous Units](http://unitsofmeasure.org/ucum.html#misc)
//! + [ ] [Table 22: Units used in Information Science and Technology](http://unitsofmeasure.org/ucum.html#infotech)
//! + [ ] [Table 23: The special prefix symbols for powers of 2](http://unitsofmeasure.org/ucum.html#infopfx)
//!
//! ## Misc
//!
//! + [ ] Full crate documentation

// uncomment this find missing documentations
//#![deny(missing_docs)]

use std::ops::{Add, Div, Mul, MulAssign, Sub};
use std::str::FromStr;

#[cfg(test)]
/// Macro to check equality of floats with a % of error
macro_rules! assert_eq_err {
    ($lhs: expr, $rhs: expr, $err: expr) => {
        let lhs = $lhs as f64;
        let rhs = $rhs as f64;
        if (1.0 - lhs / rhs).abs() >= $err {
            dbg!(lhs);
            dbg!(rhs);
            assert!(false);
        }
    };
    ($lhs: expr, $rhs: expr) => {
        assert_eq_err!($lhs, $rhs, 1e-6)
    };
}

#[cfg(test)]
/// Macro to check equality of quantities with a % of error
macro_rules! assert_eq_q {
    ($lhs: expr, $rhs: expr, $err: expr) => {
        assert_eq!($lhs.dimension(), $rhs.dimension());
        assert_eq_err!($lhs.magnitude(), $rhs.magnitude(), $err);
    };
    ($lhs: expr, $rhs: expr) => {
        assert_eq_q!($lhs, $rhs, 1e-6)
    };
}

pub mod ast;
use ast::*;

pub mod dimension;
use dimension::*;

pub mod error;
use error::*;

pub mod parser;
use parser::*;

pub mod quantity;
use quantity::*;

pub mod special;

pub mod system;
use system::*;

/// Re-exports most useful symbols from the `ucum` crate.
pub mod prelude {
    pub use crate::quantity::Quantity;
    pub use crate::system::{UnitSystem, UnitSystemFactory};
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    // prefixes
    #[test_case("1000 ms", "1 s")]
    // prefixes with exponents
    #[test_case("2 dam2", "200m2")]
    #[test_case("200 dm2", "2m2")]
    // semantic equivalent
    #[test_case("3J", "3 kg.m2/s2")]
    #[test_case("3kHz", "3000s-1")]
    // syntactic variations
    #[test_case("40 m/s", "40 m.s-1")]
    #[test_case("41 m/s2", "41 m.s-2")]
    #[test_case("42 m/s2", "42 m/(s.s)")]
    #[test_case("43 m/s2", "43 s-2/m-1")]
    #[test_case("44 m/s2", "4 s-2.11/m-1")]
    fn equivalent(txt1: &str, txt2: &str) {
        let system = UnitSystem::<f64>::default();
        let q1 = system.parse(txt1).unwrap();
        let q2 = system.parse(txt2).unwrap();
        assert_eq!(q1.dimension(), q2.dimension());
        assert_eq_err!(q1.magnitude(), q2.magnitude(), 1e-6);
    }

    #[test_case("1m", "2m")]
    #[test_case("10cm", "2m")]
    #[test_case("1e6um", "2m")]
    fn commensurable(txt1: &str, txt2: &str) {
        let system = UnitSystem::<f64>::default();
        let q1 = system.parse(txt1).unwrap();
        let q2 = system.parse(txt2).unwrap();
        assert!(q1 <= q2);
        assert!(!(q1 >= q2));
    }

    #[test_case("1m", "1g")]
    #[test_case("1J", "1g")]
    #[test_case("1J", "1W")]
    fn not_commensurable(txt1: &str, txt2: &str) {
        let system = UnitSystem::<f64>::default();
        let q1 = system.parse(txt1).unwrap();
        let q2 = system.parse(txt2).unwrap();
        assert!(!(q1 <= q2));
        assert!(!(q1 >= q2));
    }

    #[test]
    fn special_alone() {
        let system = UnitSystem::<f64>::default();
        let q1 = system.parse("0 Cel").unwrap();
        let q2 = system.parse("273.15 K").unwrap();
        assert_eq_q!(q1, q2);
    }

    #[test]
    fn special_combined() {
        let system = UnitSystem::<f64>::default();
        let u1 = system.parse("1 Cel/s").unwrap();
        let u2 = system.parse("1 K/s").unwrap();
        assert_eq!(u1, u2);
    }

    #[test]
    fn special_exponent() {
        let system = UnitSystem::<f64>::default();
        let u1 = system.parse("2 Cel2").unwrap();
        let u2 = system.parse("2 K2").unwrap();
        assert_eq!(u1, u2);
    }

    #[test]
    fn celsius_is_metric() {
        let system = UnitSystem::<f64>::default();
        let q1 = system.parse("1 kCel").unwrap();
        let q2 = system.parse("1273.15 K").unwrap();
        assert_eq_q!(q1, q2);
    }

    #[test]
    fn ten_star() {
        // check that 10 in 10* is not mistaken for a value
        let system = UnitSystem::<f64>::default();
        let q1 = system.parse("10*").unwrap();
        let q2 = system.parse("10 {dimless}").unwrap();
        assert_eq_q!(q1, q2);
    }

    #[test]
    fn ten_carret() {
        // check that 10 in 10^ is not mistaken for a value
        let system = UnitSystem::<f64>::default();
        let q1 = system.parse("10^").unwrap();
        let q2 = system.parse("10 {dimless}").unwrap();
        assert_eq_q!(q1, q2);
    }

    #[test_case("s", "s")]
    #[test_case("M", "m")]
    #[test_case("CM", "cm")]
    #[test_case("km", "km")]
    #[test_case("mG", "mg")]
    #[test_case("Ms", "ms")]
    #[test_case("mam", "Mm")]
    #[test_case("HPAL", "hPa")]
    #[test_case("Sie", "S")]
    fn case_insensitive(txt1: &str, txt2: &str) {
        let ci_system = UnitSystemFactory::<f64>::new()
            .case_sensitive(false)
            .build();
        let cs_system = UnitSystem::<f64>::default();
        let q1 = ci_system.parse(txt1).unwrap();
        let q2 = cs_system.parse(txt2).unwrap();
        assert_eq!(q1, q2);
    }
}
