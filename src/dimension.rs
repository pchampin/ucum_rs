//! Representation of the dimension of units and quantities.
//!
//! See http://unitsofmeasure.org/ucum.html#section-Semantics

use super::*;
use std::fmt;
use std::ops::Index;

/// An enum for definindex of base dimensions.
pub enum BaseDimension {
    /// The index of the dimension of the base unit 'meter'
    Length = 0,
    /// The index of the dimension of the base base unit 'second'
    Time = 1,
    /// The index of the dimension of the base unit 'gram'
    Mass = 2,
    /// The index of the dimension of the base unit 'radian'
    PlaneAngle = 3,
    /// The index of the dimension of the base unit 'Kelvin'
    Temperature = 4,
    /// The index of the dimension of the base unit 'Coulomb'
    ElectricCharge = 5,
    /// The index of the dimension of the base unit 'candela'
    LuminousIntensity = 6,
}

const DIMENSION_SIZE: usize = 7;
const SYMBOL: [&str; DIMENSION_SIZE] = ["m", "s", "g", "rad", "K", "C", "cd"];

/// The dimension of a unit or quantity.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Dimension {
    /// The dimension of standard units.
    Standard([i8; DIMENSION_SIZE]),
    // TODO actually arbitrary units can be complex, so this should rather be a hashmap<str, i8>?
    /// The dimension of [arbitrary units](http://unitsofmeasure.org/ucum.html#section-Arbitrary-Units).
    Arbitrary(&'static str),
}

use Dimension::*;

impl Dimension {
    /// The dimension of dimension-less quantities.
    ///
    /// See also the [`base`](./base/index.html) module.
    #[inline]
    pub const fn less() -> Dimension {
        Standard([0, 0, 0, 0, 0, 0, 0])
    }

    /// The dimension of lengths.
    ///
    /// See also the [`base`](./base/index.html) module.
    #[inline]
    pub const fn meter() -> Dimension {
        Standard([1, 0, 0, 0, 0, 0, 0])
    }

    /// The dimension of times.
    ///
    /// See also the [`base`](./base/index.html) module.
    #[inline]
    pub const fn second() -> Dimension {
        Standard([0, 1, 0, 0, 0, 0, 0])
    }

    /// The dimension of weights.
    ///
    /// See also the [`base`](./base/index.html) module.
    #[inline]
    pub const fn gram() -> Dimension {
        Standard([0, 0, 1, 0, 0, 0, 0])
    }

    /// The dimension of planar angles.
    ///
    /// See also the [`base`](./base/index.html) module.
    #[inline]
    pub const fn radian() -> Dimension {
        Standard([0, 0, 0, 1, 0, 0, 0])
    }

    /// The dimension of temperatures.
    ///
    /// See also the [`base`](./base/index.html) module.
    #[inline]
    pub const fn kelvin() -> Dimension {
        Standard([0, 0, 0, 0, 1, 0, 0])
    }

    /// The dimension of electric charges.
    ///
    /// See also the [`base`](./base/index.html) module.
    #[inline]
    pub const fn coulomb() -> Dimension {
        Standard([0, 0, 0, 0, 0, 1, 0])
    }

    /// The dimension of luminous intensities.
    ///
    /// See also the [`base`](./base/index.html) module.
    #[inline]
    pub const fn candela() -> Dimension {
        Standard([0, 0, 0, 0, 0, 0, 1])
    }

    /// An arbitrary dimension (see http://unitsofmeasure.org/ucum.html#section-Arbitrary-Units).
    #[inline]
    pub const fn arbitrary(name: &'static str) -> Dimension {
        Arbitrary(name)
    }

    /// Whether this dimension is [arbitrary](http://unitsofmeasure.org/ucum.html#section-Arbitrary-Units).
    #[inline]
    pub fn is_arbitrary(&self) -> bool {
        matches!(self, Arbitrary(_))
    }

    /// Raise this dimension to the given power.
    #[inline]
    pub fn raise(&mut self, power: i8) {
        match self {
            Standard(a) => {
                for ai in a.iter_mut() {
                    *ai *= power;
                }
            }
            Arbitrary(_) => unimplemented!(),
        }
    }

    /// Return a copy of this dimension raised to the given power.
    #[inline]
    pub fn raised(self, power: i8) -> Dimension {
        let mut r = self;
        r.raise(power);
        r
    }

    /// Invert this dimension.
    pub fn invert(&mut self) {
        self.raise(-1)
    }

    /// Return an inverted copy of this dimension.
    #[inline]
    pub fn inverted(&self) -> Dimension {
        self.raised(-1)
    }
}

impl Default for Dimension {
    fn default() -> Dimension {
        Dimension::less()
    }
}

impl Index<BaseDimension> for Dimension {
    type Output = i8;
    fn index(&self, i: BaseDimension) -> &i8 {
        match self {
            Standard(a) => &a[i as usize],
            Arbitrary(_) => &0,
        }
    }
}

/// # Panic
/// Both self and rhs must have a standard dimension, otherwise this operation will panic.
impl Mul<Dimension> for Dimension {
    type Output = Dimension;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn mul(self, other: Dimension) -> Self::Output {
        match (self, other) {
            (Standard(mut a1), Standard(a2)) => {
                for i in 0..DIMENSION_SIZE {
                    a1[i] += a2[i];
                }
                Standard(a1)
            }
            (Arbitrary(_), _) => panic!("Can not multiply: LHS is arbitrary"),
            (_, Arbitrary(_)) => panic!("Can not multiply: RHS is arbitrary"),
        }
    }
}

/// # Panic
/// Both self and rhs must have a standard dimension, otherwise this operation will panic.
impl Div<Dimension> for Dimension {
    type Output = Dimension;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn div(self, other: Dimension) -> Self::Output {
        match (self, other) {
            (Standard(mut a1), Standard(a2)) => {
                for i in 0..DIMENSION_SIZE {
                    a1[i] -= a2[i];
                }
                Standard(a1)
            }
            (Arbitrary(_), _) => panic!("Can not multiply: LHS is arbitrary"),
            (_, Arbitrary(_)) => panic!("Can not multiply: RHS is arbitrary"),
        }
    }
}

impl Mul<Dimension> for f64 {
    type Output = Quantity<f64>;

    fn mul(self, rhs: Dimension) -> Quantity<f64> {
        Quantity::new(self, rhs)
    }
}

impl Mul<Dimension> for i32 {
    type Output = Quantity<f64>;

    fn mul(self, rhs: Dimension) -> Quantity<f64> {
        Quantity::new(self as f64, rhs)
    }
}

impl fmt::Display for Dimension {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Standard(a) => {
                let mut first = true;
                for i in 0..DIMENSION_SIZE {
                    if a[i] == 0 {
                        continue;
                    }
                    if first {
                        first = false;
                    } else {
                        write!(f, ".")?;
                    }
                    write!(f, "{}", SYMBOL[i])?;
                    if a[i] == 1 {
                        continue;
                    }
                    let abs = if a[i] < 0 {
                        write!(f, "⁻")?;
                        -a[i]
                    } else {
                        a[i]
                    };
                    match abs {
                        1 => write!(f, "¹")?,
                        2 => write!(f, "²")?,
                        3 => write!(f, "³")?,
                        4 => write!(f, "⁴")?,
                        _ => write!(f, "{}", abs)?,
                    }
                }
                Ok(())
            }
            Arbitrary(name) => write!(f, "{}", name),
        }
    }
}

/// Static symbols for base dimensions.
pub mod base {
    use super::Dimension;
    /// The dimension of dimension-less quantities.
    pub static DIMLESS: Dimension = Dimension::less();
    /// The dimension of lengths.
    pub static M: Dimension = Dimension::meter();
    /// The dimension of times.
    pub static S: Dimension = Dimension::second();
    /// The dimension of weights.
    pub static G: Dimension = Dimension::gram();
    /// The dimension of planar angles.
    pub static RAD: Dimension = Dimension::radian();
    /// The dimension of temperatures.
    pub static K: Dimension = Dimension::kelvin();
    /// The dimension of electric charges.
    pub static C: Dimension = Dimension::coulomb();
    /// The dimension of luminous intensities.
    pub static CD: Dimension = Dimension::candela();
}

#[cfg(test)]
mod test {
    use super::*;
    use base::*;

    #[test]
    fn test_dimless() {
        let u = Dimension::less();
        assert_eq!(u[BaseDimension::Length], 0);
        assert_eq!(u[BaseDimension::Time], 0);
        assert_eq!(u[BaseDimension::Mass], 0);
        assert_eq!(u[BaseDimension::PlaneAngle], 0);
        assert_eq!(u[BaseDimension::Temperature], 0);
        assert_eq!(u[BaseDimension::ElectricCharge], 0);
        assert_eq!(u[BaseDimension::LuminousIntensity], 0);
        assert_eq!(base::DIMLESS, u);
        assert_eq!(&format!("{}", &u), "");
    }

    #[test]
    fn test_meter() {
        let m = Dimension::meter();
        assert_eq!(m[BaseDimension::Length], 1);
        assert_eq!(m[BaseDimension::Time], 0);
        assert_eq!(m[BaseDimension::Mass], 0);
        assert_eq!(m[BaseDimension::PlaneAngle], 0);
        assert_eq!(m[BaseDimension::Temperature], 0);
        assert_eq!(m[BaseDimension::ElectricCharge], 0);
        assert_eq!(m[BaseDimension::LuminousIntensity], 0);
        assert_eq!(base::M, m);
        assert_eq!(&format!("{}", &m), "m");
    }

    #[test]
    fn test_second() {
        let s = Dimension::second();
        assert_eq!(s[BaseDimension::Length], 0);
        assert_eq!(s[BaseDimension::Time], 1);
        assert_eq!(s[BaseDimension::Mass], 0);
        assert_eq!(s[BaseDimension::PlaneAngle], 0);
        assert_eq!(s[BaseDimension::Temperature], 0);
        assert_eq!(s[BaseDimension::ElectricCharge], 0);
        assert_eq!(s[BaseDimension::LuminousIntensity], 0);
        assert_eq!(base::S, s);
        assert_eq!(&format!("{}", &s), "s");
    }

    #[test]
    fn test_gram() {
        let g = Dimension::gram();
        assert_eq!(g[BaseDimension::Length], 0);
        assert_eq!(g[BaseDimension::Time], 0);
        assert_eq!(g[BaseDimension::Mass], 1);
        assert_eq!(g[BaseDimension::PlaneAngle], 0);
        assert_eq!(g[BaseDimension::Temperature], 0);
        assert_eq!(g[BaseDimension::ElectricCharge], 0);
        assert_eq!(g[BaseDimension::LuminousIntensity], 0);
        assert_eq!(base::G, g);
        assert_eq!(&format!("{}", &g), "g");
    }

    #[test]
    fn test_radian() {
        let rad = Dimension::radian();
        assert_eq!(rad[BaseDimension::Length], 0);
        assert_eq!(rad[BaseDimension::Time], 0);
        assert_eq!(rad[BaseDimension::Mass], 0);
        assert_eq!(rad[BaseDimension::PlaneAngle], 1);
        assert_eq!(rad[BaseDimension::Temperature], 0);
        assert_eq!(rad[BaseDimension::ElectricCharge], 0);
        assert_eq!(rad[BaseDimension::LuminousIntensity], 0);
        assert_eq!(base::RAD, rad);
        assert_eq!(&format!("{}", &rad), "rad");
    }

    #[test]
    fn test_kelvin() {
        let k = Dimension::kelvin();
        assert_eq!(k[BaseDimension::Length], 0);
        assert_eq!(k[BaseDimension::Time], 0);
        assert_eq!(k[BaseDimension::Mass], 0);
        assert_eq!(k[BaseDimension::PlaneAngle], 0);
        assert_eq!(k[BaseDimension::Temperature], 1);
        assert_eq!(k[BaseDimension::ElectricCharge], 0);
        assert_eq!(k[BaseDimension::LuminousIntensity], 0);
        assert_eq!(base::K, k);
        assert_eq!(&format!("{}", &k), "K");
    }

    #[test]
    fn test_coulomb() {
        let c = Dimension::coulomb();
        assert_eq!(c[BaseDimension::Length], 0);
        assert_eq!(c[BaseDimension::Time], 0);
        assert_eq!(c[BaseDimension::Mass], 0);
        assert_eq!(c[BaseDimension::PlaneAngle], 0);
        assert_eq!(c[BaseDimension::Temperature], 0);
        assert_eq!(c[BaseDimension::ElectricCharge], 1);
        assert_eq!(c[BaseDimension::LuminousIntensity], 0);
        assert_eq!(base::C, c);
        assert_eq!(&format!("{}", &c), "C");
    }

    #[test]
    fn test_candela() {
        let cd = Dimension::candela();
        assert_eq!(cd[BaseDimension::Length], 0);
        assert_eq!(cd[BaseDimension::Time], 0);
        assert_eq!(cd[BaseDimension::Mass], 0);
        assert_eq!(cd[BaseDimension::PlaneAngle], 0);
        assert_eq!(cd[BaseDimension::Temperature], 0);
        assert_eq!(cd[BaseDimension::ElectricCharge], 0);
        assert_eq!(cd[BaseDimension::LuminousIntensity], 1);
        assert_eq!(base::CD, cd);
        assert_eq!(&format!("{}", &cd), "cd");
    }

    #[test]
    fn test_m2() {
        let m2 = M * M;
        assert_eq!(m2[BaseDimension::Length], 2);
        assert_eq!(m2[BaseDimension::Time], 0);
        assert_eq!(m2[BaseDimension::Mass], 0);
        assert_eq!(m2[BaseDimension::PlaneAngle], 0);
        assert_eq!(m2[BaseDimension::Temperature], 0);
        assert_eq!(m2[BaseDimension::ElectricCharge], 0);
        assert_eq!(m2[BaseDimension::LuminousIntensity], 0);
        assert_eq!(&format!("{}", &m2), "m²");
    }

    #[test]
    fn test_ms() {
        let ms = M * S;
        assert_eq!(ms[BaseDimension::Length], 1);
        assert_eq!(ms[BaseDimension::Time], 1);
        assert_eq!(ms[BaseDimension::Mass], 0);
        assert_eq!(ms[BaseDimension::PlaneAngle], 0);
        assert_eq!(ms[BaseDimension::Temperature], 0);
        assert_eq!(ms[BaseDimension::ElectricCharge], 0);
        assert_eq!(ms[BaseDimension::LuminousIntensity], 0);
        assert_eq!(&format!("{}", &ms), "m.s");
    }

    #[test]
    fn test_mps() {
        let mps = M / S;
        assert_eq!(mps[BaseDimension::Length], 1);
        assert_eq!(mps[BaseDimension::Time], -1);
        assert_eq!(mps[BaseDimension::Mass], 0);
        assert_eq!(mps[BaseDimension::PlaneAngle], 0);
        assert_eq!(mps[BaseDimension::Temperature], 0);
        assert_eq!(mps[BaseDimension::ElectricCharge], 0);
        assert_eq!(mps[BaseDimension::LuminousIntensity], 0);
        assert_eq!(&format!("{}", &mps), "m.s⁻¹");
    }

    #[test]
    fn test_mps2() {
        let mps2 = M / (S * S);
        assert_eq!(mps2[BaseDimension::Length], 1);
        assert_eq!(mps2[BaseDimension::Time], -2);
        assert_eq!(mps2[BaseDimension::Mass], 0);
        assert_eq!(mps2[BaseDimension::PlaneAngle], 0);
        assert_eq!(mps2[BaseDimension::Temperature], 0);
        assert_eq!(mps2[BaseDimension::ElectricCharge], 0);
        assert_eq!(mps2[BaseDimension::LuminousIntensity], 0);
        assert_eq!(&format!("{}", &mps2), "m.s⁻²");
    }

    #[test]
    fn test_inverted() {
        let mm1 = M.inverted();
        assert_eq!(mm1[BaseDimension::Length], -1);
        assert_eq!(mm1[BaseDimension::Time], 0);
        assert_eq!(mm1[BaseDimension::Mass], 0);
        assert_eq!(mm1[BaseDimension::PlaneAngle], 0);
        assert_eq!(mm1[BaseDimension::Temperature], 0);
        assert_eq!(mm1[BaseDimension::ElectricCharge], 0);
        assert_eq!(mm1[BaseDimension::LuminousIntensity], 0);
        assert_eq!(&format!("{}", &mm1), "m⁻¹");

        let d = (M * M / S).inverted();
        assert_eq!(d[BaseDimension::Length], -2);
        assert_eq!(d[BaseDimension::Time], 1);
        assert_eq!(d[BaseDimension::Mass], 0);
        assert_eq!(d[BaseDimension::PlaneAngle], 0);
        assert_eq!(d[BaseDimension::Temperature], 0);
        assert_eq!(d[BaseDimension::ElectricCharge], 0);
        assert_eq!(d[BaseDimension::LuminousIntensity], 0);
        assert_eq!(&format!("{}", &d), "m⁻².s");
    }

    #[test]
    fn test_invert() {
        let mut d = M;
        d.invert();
        assert_eq!(d[BaseDimension::Length], -1);
        assert_eq!(d[BaseDimension::Time], 0);
        assert_eq!(d[BaseDimension::Mass], 0);
        assert_eq!(d[BaseDimension::PlaneAngle], 0);
        assert_eq!(d[BaseDimension::Temperature], 0);
        assert_eq!(d[BaseDimension::ElectricCharge], 0);
        assert_eq!(d[BaseDimension::LuminousIntensity], 0);
        assert_eq!(&format!("{}", &d), "m⁻¹");

        let mut d = M * M / S;
        d.invert();
        assert_eq!(d[BaseDimension::Length], -2);
        assert_eq!(d[BaseDimension::Time], 1);
        assert_eq!(d[BaseDimension::Mass], 0);
        assert_eq!(d[BaseDimension::PlaneAngle], 0);
        assert_eq!(d[BaseDimension::Temperature], 0);
        assert_eq!(d[BaseDimension::ElectricCharge], 0);
        assert_eq!(d[BaseDimension::LuminousIntensity], 0);
        assert_eq!(&format!("{}", &d), "m⁻².s");
    }
}
