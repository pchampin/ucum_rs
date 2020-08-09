//! Special units conversion funcions.
//! See http://unitsofmeasure.org/ucum.html#section-Special-Units-on-non-ratio-Scales

use super::*;
use std::fmt;

pub trait Converter<T> {
    fn to_standard(&self, value: T) -> T;
    fn from_standard(&self, value: T) -> T;
}

impl<T> fmt::Debug for Box<dyn Converter<T>> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[converter]")
    }
}

#[derive(Clone, Debug)]
pub struct Celsius<T> {
    delta: T,
}

impl<T> Celsius<T>
where
    T: Add<T, Output = T> + Clone + FromStr + Sub<T, Output = T> + 'static,
    <T as FromStr>::Err: fmt::Debug,
{
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> Box<dyn Converter<T>> {
        Box::new(Celsius {
            delta: "273.15".parse().unwrap(),
        })
    }
}

impl<T> Converter<T> for Celsius<T>
where
    T: Add<T, Output = T> + Clone + Sub<T, Output = T>,
{
    fn to_standard(&self, value: T) -> T {
        value + self.delta.clone()
    }
    fn from_standard(&self, value: T) -> T {
        value - self.delta.clone()
    }
}
