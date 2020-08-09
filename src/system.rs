//! This module initializes the tables of all UCUM units.

use super::dimension::base::*;
use super::special::*;
use super::*;
use std::collections::{BTreeMap, HashMap};

const PI: &str = "3.14159265358979323846264338327950288";
const AVOGADRO: &str = "602213670000000000000000";

pub struct UnitSystem<T> {
    units: HashMap<&'static str, UnitDescription<T>>,
    prefixes: BTreeMap<&'static str, PrefixDescription<T>>,
    case_sensitive: bool,
}

pub struct UnitSystemFactory<T> {
    // configuration
    // TODO put here fields indicating which tables to include
    // factory inners
    system: UnitSystem<T>,
    one: T,
    kilo: T,
    milli: T,
}

impl<T> Default for UnitSystemFactory<T>
where
    T: From<i32> + FromStr + 'static,
    <T as FromStr>::Err: std::fmt::Debug,
{
    fn default() -> Self {
        UnitSystemFactory {
            system: UnitSystem::new(),
            one: 1.into(),
            kilo: 1000.into(),
            milli: "0.001".parse().unwrap(),
        }
    }
}

impl<T> UnitSystemFactory<T>
where
    T: Add<T, Output = T> + Div<T, Output = T> + Mul<T, Output = T> + Sub<T, Output = T>,
    T: Clone + From<i32> + FromStr + 'static,
    <T as FromStr>::Err: std::fmt::Debug,
{
    /// Make a default unit system supporting only the base and SI units.
    pub fn new() -> Self {
        Self::default()
    }

    // configuration

    pub fn case_sensitive(mut self, case_sensitive: bool) -> Self {
        self.system.case_sensitive = case_sensitive;
        self
    }

    // TODO add options to add a selection of tables to the system
    // (and possibly to remove) some of them

    // other public methods

    pub fn build(mut self) -> UnitSystem<T> {
        self.fill_in_std_prefixes();
        self.fill_in_base();
        self.fill_in_dimensionless();
        self.fill_in_si();
        self.fill_in_iso1000_et_al();
        self.system
    }

    // private methods

    /// standard prefixes: http://unitsofmeasure.org/ucum.html#para-30
    fn fill_in_std_prefixes(&mut self) {
        self.insert_p("yotta", "Y", "Y", "YA", "1000000000000000000000000");
        self.insert_p("zetta", "Z", "Z", "ZA", "1000000000000000000000");
        self.insert_p("exa", "E", "E", "EX", "1000000000000000000");
        self.insert_p("peta", "P", "P", "PT", "1000000000000000");
        self.insert_p("tera", "T", "T", "TR", "1000000000000");
        self.insert_p("giga", "G", "G", "GA", "1000000000");
        self.insert_p("mega", "M", "M", "MA", "1000000");
        self.insert_p("kilo", "k", "k", "K", "1000");
        self.insert_p("hecto", "h", "h", "H", "100");
        self.insert_p("deka", "da", "da", "DA", "10");
        self.insert_p("deci", "d", "d", "D", "0.1");
        self.insert_p("centi", "c", "c", "C", "0.01");
        self.insert_p("milli", "m", "m", "M", "0.001");
        self.insert_p("micro", "μ", "u", "U", "0.000001");
        self.insert_p("nano", "n", "n", "N", "0.000000001");
        self.insert_p("pico", "p", "p", "P", "0.000000000001");
        self.insert_p("femtp", "f", "f", "F", "0.000000000000001");
        self.insert_p("attp", "a", "a", "A", "0.000000000000000001");
        self.insert_p("zecto", "z", "z", "ZO", "0.000000000000000000001");
        self.insert_p("yocto", "y", "y", "YO", "0.000000000000000000000001");
    }

    /// base units: http://unitsofmeasure.org/ucum.html#para-28
    fn fill_in_base(&mut self) {
        self.insert_b("meter", "m", "m", "M", M);
        self.insert_b("second", "s", "s", "S", S);
        self.insert_b("gram", "g", "g", "G", G);
        self.insert_b("radian", "rad", "rad", "RAD", RAD);
        self.insert_b("kelvin", "K", "K", "K", K);
        self.insert_b("coulomb", "C", "C", "C", C);
        self.insert_b("candela", "cd", "cd", "CD", CD);
    }

    /// dimensionless units: http://unitsofmeasure.org/ucum.html#para-29
    fn fill_in_dimensionless(&mut self) {
        let ten: T = 10.into();
        let power_of_ten = "the number ten for arbitrary powers";
        self.insert_l(power_of_ten, "10*", "10*", "10*", ten.clone(), false);
        self.insert_l(power_of_ten, "10ⁿ", "10^", "10^", ten.clone(), false);
        self.insert_l(
            "the number pi",
            "π",
            "[pi]",
            "[PI]",
            PI.parse().unwrap(),
            false,
        );
        let percent = self.one.clone() / (ten.clone() * ten);
        self.insert_l("percent", "%", "%", "%", percent, false);
        let ppth = self.milli.clone();
        self.insert_l(
            "parts per thousand",
            "ppth",
            "[ppth]",
            "[PPTH]",
            ppth.clone(),
            false,
        );
        let ppm = ppth.clone() * ppth.clone();
        self.insert_l(
            "parts per million",
            "ppm",
            "[ppm]",
            "[PPM]",
            ppm.clone(),
            false,
        );
        let ppb = ppth.clone() * ppm;
        self.insert_l(
            "parts per billion",
            "ppb",
            "[ppb]",
            "[PPB]",
            ppb.clone(),
            false,
        );
        let pptr = ppth * ppb;
        self.insert_l(
            "parts per trillion",
            "pptr",
            "[pptr]",
            "[PPTR]",
            pptr,
            false,
        );
    }

    /// SI units: http://unitsofmeasure.org/ucum.html#para-30
    fn fill_in_si(&mut self) {
        self.insert_l("mole", "mol", "mol", "MOL", AVOGADRO.parse().unwrap(), true);
        self.insert_b("steradian", "sr", "sr", "SR", RAD * RAD);
        self.insert_b("hertz", "Hz", "Hz", "HZ", S.inverted());
        let newton_dim = G * M / (S * S);
        self.insert_k("newton", "N", "N", "N", newton_dim);
        self.insert_k("pascal", "Pa", "Pa", "PAL", newton_dim / (M * M));
        let joule_dim = newton_dim * M;
        self.insert_k("joule", "J", "J", "J", joule_dim);
        self.insert_k("watt", "W", "W", "W", joule_dim / S);
        let ampere_dim = C / S;
        self.insert_b("ampère", "A", "A", "A", ampere_dim);
        let volt_dim = joule_dim / C;
        self.insert_k("volt", "V", "V", "V", volt_dim);
        self.insert_m("farad", "F", "F", "F", C / volt_dim);
        let ohm_dim = volt_dim / ampere_dim;
        self.insert_k("ohm", "Ω", "Ohm", "OHM", ohm_dim);
        self.insert_m("siemens", "S", "S", "SIE", ohm_dim.inverted());
        let weber_dim = volt_dim * S;
        self.insert_k("weber", "Wb", "Wb", "WB", weber_dim);
        self.insert_s("degree Celsius", "°C", "Cel", "CEL", K, Celsius::new());
        self.insert_k("tesla", "T", "T", "T", weber_dim / (M * M));
        self.insert_k("henry", "H", "H", "H", weber_dim / ampere_dim);
        let lumen_dim = CD / (RAD * RAD);
        self.insert_b("lumen", "lm", "lm", "LM", lumen_dim);
        self.insert_b("lux", "lx", "lx", "LX", lumen_dim / (M * M));
        self.insert_b("becquerel", "Bq", "Bq", "BQ", S.inverted());
        self.insert_b("gray", "Gy", "Gy", "GY", joule_dim / G);
        self.insert_b("sievert", "Sv", "Sv", "SV", joule_dim / G);
    }

    /// Other units from ISO1000 et al.: http://unitsofmeasure.org/ucum.html#para-31
    fn fill_in_iso1000_et_al(&mut self) {
        let pi: T = PI.parse().unwrap();
        let gon = pi.clone() / 200.into();
        self.insert_r("gone, grade", "ᵍ", "gon", "GON", gon, RAD, false, None);
        let deg = pi.clone() / 180.into();
        self.insert_r("degree", "°", "deg", "DEG", deg, RAD, false, None);
        let min = pi.clone() / 10800.into();
        self.insert_r("minute", "'", "'", "'", min, RAD, false, None);
        let sec = pi / 648000.into();
        self.insert_r("second", "''", "''", "''", sec, RAD, false, None);

        self.insert_m("liter", "l", "l", "L", M * M * M);
        if self.system.case_sensitive {
            self.insert_m("liter", "L", "L", "L", M * M * M);
        }
        self.insert_r("are", "a", "ar", "AR", 100.into(), M * M, true, None);

        self.insert_r("minute", "min", "min", "MIN", 60.into(), S, false, None);
        self.insert_r("hour", "h", "h", "HR", 3600.into(), S, false, None);

        self.insert_r("day", "d", "d", "D", (24 * 3600).into(), S, false, None);
        self.insert_r(
            "tropical year",
            "aₜ",
            "a_t",
            "ANN_T",
            "31556925.216".parse().unwrap(),
            S,
            false,
            None,
        );
        let ann_j: T = 31557600.into();
        self.insert_r(
            "mean Julian year",
            "aⱼ",
            "a_j",
            "ANN_J",
            ann_j.clone(),
            S,
            false,
            None,
        );
        self.insert_r(
            "mean Gregorian year",
            "aᵍ",
            "a_g",
            "ANN_G",
            31556952.into(),
            S,
            false,
            None,
        );
        self.insert_r("year", "a", "a", "ANN", ann_j, S, false, None);
        self.insert_r(
            "week",
            "wk",
            "wk",
            "WK",
            (7 * 24 * 3600).into(),
            S,
            false,
            None,
        );
        self.insert_r(
            "synodal month",
            "moₛ",
            "mo_s",
            "MO_S",
            "2551442.976".parse().unwrap(),
            S,
            false,
            None,
        );
        let mo_j: T = 2629800.into();
        self.insert_r(
            "mean Julian month",
            "moⱼ",
            "mo_j",
            "MO_J",
            mo_j.clone(),
            S,
            false,
            None,
        );
        self.insert_r(
            "mean Gregorian month",
            "moᵍ",
            "mo_g",
            "MO_G",
            2629746.into(),
            S,
            false,
            None,
        );
        self.insert_r("month", "mo", "mo", "MO", mo_j, S, false, None);

        self.insert_r("tonne", "t", "t", "TNE", 1_000_000.into(), G, true, None);
        self.insert_r(
            "bar",
            "bar",
            "bar",
            "BAR",
            100_000_000.into(),
            G / (M * S * S),
            true,
            None,
        );
        self.insert_r(
            "unified atomic mass unit",
            "u",
            "u",
            "AMU",
            "0.0000000000000000000000016605402".parse().unwrap(),
            G,
            true,
            None,
        );
        self.insert_r(
            "electronvolt",
            "eV",
            "eV",
            "EV",
            "0.000000000000000160217733".parse().unwrap(),
            G * M * M / (S * S),
            true,
            None,
        );
        self.insert_r(
            "astronomic unit",
            "AU",
            "AU",
            "ASU",
            "149597870691".parse().unwrap(),
            M,
            false,
            None,
        );
        self.insert_r(
            "parsec",
            "pc",
            "pc",
            "PRS",
            "30856780000000000".parse().unwrap(),
            M,
            true,
            None,
        );
    }

    // chose the appropriate key (case sensitive or case insensitive)
    fn get_key<'a>(&self, cs: &'a str, ci: &'a str) -> &'a str {
        if self.system.case_sensitive {
            cs
        } else {
            ci
        }
    }

    #[inline]
    // insert prefix
    fn insert_p(
        &mut self,
        name: &'static str,
        print: &'static str,
        cs: &'static str,
        ci: &'static str,
        value: &'static str,
    ) where
        T: FromStr,
        <T as FromStr>::Err: std::fmt::Debug,
    {
        let key = self.get_key(cs, ci);
        debug_assert!(!self.system.prefixes.contains_key(key));
        let value = value.parse().unwrap();
        let pd = PrefixDescription {
            name,
            print,
            cs,
            ci,
            value,
        };
        self.system.prefixes.insert(key, pd);
    }

    #[allow(clippy::too_many_arguments)]
    #[inline]
    /// insert unit from raw parts
    fn insert_r(
        &mut self,
        name: &'static str,
        print: &'static str,
        cs: &'static str,
        ci: &'static str,
        magnitude: T,
        dimension: Dimension,
        metric: bool,
        converter: Option<Box<dyn Converter<T>>>,
    ) {
        let key = self.get_key(cs, ci);
        debug_assert!(!self.system.units.contains_key(key));
        let quantity = Quantity::new(magnitude, dimension);
        let ud = UnitDescription {
            name,
            print,
            cs,
            ci,
            quantity,
            metric,
            converter,
        };
        self.system.units.insert(key, ud);
    }

    #[inline]
    /// insert base unit (magnitude = 1)
    fn insert_b(
        &mut self,
        name: &'static str,
        print: &'static str,
        cs: &'static str,
        ci: &'static str,
        dimension: Dimension,
    ) {
        let one = self.one.clone();
        self.insert_r(name, print, cs, ci, one, dimension, true, None)
    }

    #[inline]
    /// insert unit with a magnitude of 1000
    fn insert_k(
        &mut self,
        name: &'static str,
        print: &'static str,
        cs: &'static str,
        ci: &'static str,
        dimension: Dimension,
    ) {
        let kilo = self.kilo.clone();
        self.insert_r(name, print, cs, ci, kilo, dimension, true, None)
    }

    #[inline]
    /// insert unit with a magnitude of 1/1000
    fn insert_m(
        &mut self,
        name: &'static str,
        print: &'static str,
        cs: &'static str,
        ci: &'static str,
        dimension: Dimension,
    ) {
        let milli = self.milli.clone();
        self.insert_r(name, print, cs, ci, milli, dimension, true, None)
    }

    #[inline]
    // insert dimension-less unit
    fn insert_l(
        &mut self,
        name: &'static str,
        print: &'static str,
        cs: &'static str,
        ci: &'static str,
        magnitude: T,
        metric: bool,
    ) {
        self.insert_r(
            name,
            print,
            cs,
            ci,
            magnitude,
            Dimension::less(),
            metric,
            None,
        )
    }

    #[inline]
    // insert special unit
    fn insert_s(
        &mut self,
        name: &'static str,
        print: &'static str,
        cs: &'static str,
        ci: &'static str,
        dimension: Dimension,
        converter: Box<dyn Converter<T>>,
    ) {
        let one = self.one.clone();
        self.insert_r(name, print, cs, ci, one, dimension, true, Some(converter))
    }
}

impl<T> UnitSystem<T> {
    // public methods

    pub fn full() -> Self {
        unimplemented!("UCUM full-compliance is not implemented yet");
        // TODO implement all remaining unit- and prefix-tables.
    }

    pub fn parse<'t>(&self, txt: &'t str) -> UcumResult<'t, Quantity<T>>
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
        self.parse_bytes(txt.as_bytes())
    }

    pub fn parse_bytes<'t>(&self, txt: &'t [u8]) -> UcumResult<'t, Quantity<T>>
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
        self.parser(txt).parse_value()
    }

    pub fn parser<'s, 't>(&'s self, txt: &'t [u8]) -> Parser<'s, 't, T> {
        Parser::new(self, txt)
    }

    pub fn get_unit(&self, unit: &str) -> Option<&UnitDescription<T>> {
        if self.case_sensitive {
            self.units.get(unit)
        } else {
            let ci_label = unit.to_ascii_uppercase();
            self.units.get(&ci_label[..])
        }
    }

    pub fn get_prefix(&self, prefix: &str) -> Option<&PrefixDescription<T>> {
        if self.case_sensitive {
            self.prefixes.get(prefix)
        } else {
            let ci_label = prefix.to_ascii_uppercase();
            self.prefixes.get(&ci_label[..])
        }
    }

    pub fn get_descriptions(
        &self,
        prefixed_unit: &str,
    ) -> Option<(Option<&PrefixDescription<T>>, &UnitDescription<T>)> {
        if prefixed_unit.is_empty() {
            return None;
        }
        let mut buffer: Option<String> = None;
        let label = if self.case_sensitive {
            prefixed_unit
        } else {
            buffer.replace(prefixed_unit.to_ascii_uppercase());
            &buffer.as_ref().unwrap()[..]
        };
        if let Some(ud) = self.units.get(label) {
            return Some((None, ud));
        }
        let mut candidate_prefixes = self.prefixes.range(&label[..1]..label);
        loop {
            match candidate_prefixes.next_back() {
                None => break,
                Some((prefix, _pd)) => {
                    if label.starts_with(prefix) {
                        // we can't use _pd directly, because of lifetime issues
                        let pd = &self.prefixes[&label[..prefix.len()]];
                        if let Some(ud) = self.units.get(&label[prefix.len()..]) {
                            if !ud.metric {
                                continue;
                            }
                            return Some((Some(pd), ud));
                        }
                    }
                }
            }
        }
        None
    }

    // private methods

    fn new() -> Self {
        UnitSystem {
            units: HashMap::with_capacity(36), // the # of units in the default config
            prefixes: BTreeMap::new(),
            case_sensitive: true,
        }
    }
}

impl<T> Default for UnitSystem<T>
where
    T: Add<T, Output = T> + Div<T, Output = T> + Mul<T, Output = T> + Sub<T, Output = T>,
    T: Clone + From<i32> + FromStr + 'static,
    <T as FromStr>::Err: std::fmt::Debug,
{
    fn default() -> Self {
        UnitSystemFactory::default().build()
    }
}

impl<'a, T> std::ops::Index<&'a str> for UnitSystem<T> {
    type Output = UnitDescription<T>;
    fn index(&self, idx: &'a str) -> &UnitDescription<T> {
        &self.units[idx]
    }
}

#[derive(Debug)]
pub struct UnitDescription<T> {
    name: &'static str,
    print: &'static str,
    cs: &'static str,
    ci: &'static str,
    quantity: Quantity<T>,
    metric: bool,
    converter: Option<Box<dyn Converter<T>>>,
}

impl<T> UnitDescription<T> {
    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn print(&self) -> &'static str {
        self.print
    }

    pub fn cs(&self) -> &'static str {
        self.cs
    }

    pub fn ci(&self) -> &'static str {
        self.ci
    }

    pub fn metric(&self) -> bool {
        self.metric
    }

    pub(crate) fn quantity(&self) -> Quantity<T>
    where
        T: Clone,
    {
        self.quantity.clone()
    }

    pub fn make_quantity(&self, magnitude: T) -> Quantity<T>
    where
        T: Clone + Mul<T, Output = T>,
    {
        let factor = self.quantity.magnitude();
        if let Some(converter) = &self.converter {
            let magnitude = converter.to_standard(factor * magnitude);
            Quantity::new(magnitude, self.quantity.dimension())
        } else {
            Quantity::new(factor * magnitude, self.quantity.dimension())
        }
    }
}

#[derive(Debug)]
pub struct PrefixDescription<T> {
    name: &'static str,
    print: &'static str,
    cs: &'static str,
    ci: &'static str,
    value: T,
}

impl<T> PrefixDescription<T> {
    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn print(&self) -> &'static str {
        self.print
    }

    pub fn cs(&self) -> &'static str {
        self.cs
    }

    pub fn ci(&self) -> &'static str {
        self.ci
    }

    pub fn value(&self) -> T
    where
        T: Clone,
    {
        self.value.clone()
    }

    pub fn borrow_value(&self) -> &T {
        &self.value
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test]
    fn test_ten_star() {
        let system = UnitSystemFactory::<f64>::new().build();
        let q = system.units["10*"].make_quantity(1.0);
        assert_eq!(q.magnitude(), 10.0);
        assert_eq!(q.dimension(), Dimension::less());
    }

    #[test]
    fn test_ten_carret() {
        let system = UnitSystemFactory::<f64>::new().build();
        let q = system.units["10^"].make_quantity(1.0);
        assert_eq!(q.magnitude(), 10.0);
        assert_eq!(q.dimension(), Dimension::less());
    }

    #[test]
    fn test_percent() {
        let system = UnitSystemFactory::<f64>::new().build();
        let q = system.units["%"].make_quantity(1.0);
        assert_eq_err!(q.magnitude(), 0.01);
        assert_eq!(q.dimension(), Dimension::less());
    }

    #[test]
    fn test_ppth() {
        let system = UnitSystemFactory::<f64>::new().build();
        let q = system.units["[ppth]"].make_quantity(1.0);
        assert_eq_err!(q.magnitude(), 0.001);
        assert_eq!(q.dimension(), Dimension::less());
    }

    #[test]
    fn test_ppm() {
        let system = UnitSystemFactory::<f64>::new().build();
        let q = system.units["[ppm]"].make_quantity(1.0);
        assert_eq_err!(q.magnitude(), 1e-6);
        assert_eq!(q.dimension(), Dimension::less());
    }

    #[test]
    fn test_ppb() {
        let system = UnitSystemFactory::<f64>::new().build();
        let q = system.units["[ppb]"].make_quantity(1.0);
        assert_eq_err!(q.magnitude(), 1e-9);
        assert_eq!(q.dimension(), Dimension::less());
    }

    #[test]
    fn test_pptr() {
        let system = UnitSystemFactory::<f64>::new().build();
        let q = system.units["[pptr]"].make_quantity(1.0);
        assert_eq_err!(q.magnitude(), 1e-12);
        assert_eq!(q.dimension(), Dimension::less());
    }

    #[test]
    fn test_mole() {
        let system = UnitSystemFactory::<f64>::new().build();
        let q = system.units["mol"].make_quantity(1.0);
        assert_eq_err!(q.magnitude(), 6.0221367e23);
        assert_eq!(q.dimension(), Dimension::less());
    }

    #[test]
    fn test_si_raw() {
        let system = UnitSystemFactory::<f64>::new().build();

        let sr = system.units["sr"].make_quantity(1.0);
        assert_eq!(1 * sr, (1 * RAD) * (1 * RAD));
        assert_ne!(2 * sr, (1 * RAD) * (1 * RAD));
        assert_ne!(1 * sr, 1 * RAD);
        assert_ne!(1 * sr, 1 * M);

        let hz = system.units["Hz"].make_quantity(1.0);
        assert_eq!(1 * hz, 1 * S.inverted());
        assert_ne!(2 * hz, 1 * S.inverted());
        assert_ne!(1 * hz, 1 * S);
        assert_ne!(1 * hz, 1 * M);

        let kg = 1000 * G;
        let n = system.units["N"].make_quantity(1.0);
        assert_eq!(1 * n, 1 * kg * M / (S * S));
        assert_ne!(2 * n, 1 * kg * M / (S * S));
        assert_ne!(1 * n, 1 * kg * M / S);

        let pal = system.units["Pa"].make_quantity(1.0);
        assert_eq!(1 * pal, 1 * n / (M * M));
        assert_ne!(2 * pal, 1 * n / (M * M));
        assert_ne!(1 * pal, 1 * n / M);

        let j = system.units["J"].make_quantity(1.0);
        assert_eq!(1 * j, 1 * n * M);
        assert_ne!(2 * j, 1 * n * M);
        assert_ne!(1 * j, 1 * n * M * M);

        let w = system.units["W"].make_quantity(1.0);
        assert_eq!(1 * w, 1 * j / S);
        assert_ne!(2 * w, 1 * j / S);
        assert_ne!(1 * w, 1 * j * S);

        let a = system.units["A"].make_quantity(1.0);
        assert_eq!(1 * a, 1 * C / S);
        assert_ne!(2 * a, 1 * C / S);
        assert_ne!(1 * a, 1 * C * S);

        let v = system.units["V"].make_quantity(1.0);
        assert_eq!(1 * v, 1 * j / C);
        assert_ne!(2 * v, 1 * j / C);
        assert_ne!(1 * v, 1 * j * C);

        let ohm = system.units["Ohm"].make_quantity(1.0);
        assert_eq!(1 * ohm, 1 * v / a);
        assert_ne!(2 * ohm, 1 * v / a);
        assert_ne!(1 * ohm, 1 * v * a);

        let sie = system.units["S"].make_quantity(1.0);
        assert_eq!(1 * sie, 1 * ohm.inverted());
        assert_ne!(2 * sie, 1 * ohm.inverted());
        assert_ne!(1 * sie, 1 * ohm);

        let wb = system.units["Wb"].make_quantity(1.0);
        assert_eq!(1 * wb, 1 * v * S);
        assert_ne!(2 * wb, 1 * v * S);
        assert_ne!(1 * wb, 1 * v * S * CD);

        let zero_cel = system.units["Cel"].make_quantity(0.0);
        assert_eq!(zero_cel.dimension(), K);
        assert_eq_err!(zero_cel.magnitude(), (273.15 * K).magnitude());

        let t = system.units["T"].make_quantity(1.0);
        assert_eq!(1 * t, 1 * wb / (M * M));
        assert_ne!(2 * t, 1 * wb / (M * M));
        assert_ne!(1 * t, 1 * wb / M);

        let h = system.units["H"].make_quantity(1.0);
        assert_eq!(1 * h, 1 * wb / a);
        assert_ne!(2 * h, 1 * wb / a);
        assert_ne!(1 * h, 1 * wb / (a * a));

        let lm = system.units["lm"].make_quantity(1.0);
        assert_eq!(1 * lm, 1 * CD / sr);
        assert_ne!(2 * lm, 1 * CD / sr);
        assert_ne!(1 * lm, 1 * CD / (sr * sr));

        let lx = system.units["lx"].make_quantity(1.0);
        assert_eq!(1 * lx, 1 * lm / (M * M));
        assert_ne!(2 * lx, 1 * lm / (M * M));
        assert_ne!(1 * lx, 1 * lm / M);

        let bq = system.units["Bq"].make_quantity(1.0);
        assert_eq!(1 * bq, 1 * S.inverted());
        assert_ne!(2 * bq, 1 * S.inverted());
        assert_ne!(1 * bq, 1 * S);

        let gy = system.units["Gy"].make_quantity(1.0);
        assert_eq!(1 * gy, 1 * j / kg);
        assert_ne!(2 * gy, 1 * j / kg);
        assert_ne!(1 * gy, 1 * j / M);

        let sv = system.units["Sv"].make_quantity(1.0);
        assert_eq!(1 * sv, 1 * j / kg);
        assert_ne!(2 * sv, 1 * j / kg);
        assert_ne!(1 * sv, 1 * j / M);
    }

    // Table 4: SI units
    // http://unitsofmeasure.org/ucum.html#prefixes
    #[test_case("mol", "6.0221367 10*23")]
    #[test_case("sr", "1 rad2")]
    #[test_case("Hz", "1 s-1")]
    #[test_case("N", "1 kg.m/s2")]
    #[test_case("Pa", "1 N/m2")]
    #[test_case("J", "1 N.m")]
    #[test_case("W", "1 J/s")]
    #[test_case("A", "1 C/s")]
    #[test_case("V", "1 J/C")]
    #[test_case("F", "1 C/V")]
    #[test_case("Ohm", "1 V/A")]
    #[test_case("S", "1 Ohm-1")]
    #[test_case("Wb", "1 V.s")]
    #[test_case("0 Cel", "273.15 K")]
    #[test_case("1 Cel", "274.15 K")]
    #[test_case("100 Cel", "373.15 K")]
    #[test_case("T", "1 Wb/m2")]
    #[test_case("H", "1 Wb/A")]
    #[test_case("lm", "1 cd/sr")]
    #[test_case("lx", "1 lm/m2")]
    #[test_case("Bq", "1 s-1")]
    #[test_case("Gy", "1 J/kg")]
    #[test_case("Sv", "1 J/kg")]
    // Table 5: Other units from ISO 1000, ISO 2955, and some from ANSI X3.50
    // http://unitsofmeasure.org/ucum.html#iso1000
    #[test_case("gon", "0.9 deg")]
    #[test_case("deg", "2 [pi].rad/360")]
    #[test_case("'", "1 deg/60")]
    #[test_case("''", "1 '/60")]
    #[test_case("l", "1 dm3")]
    #[test_case("L", "1 l")]
    #[test_case("ar", "100 m2")]
    #[test_case("min", "60 s")]
    #[test_case("h", "60 min")]
    #[test_case("d", "24 h")]
    #[test_case("a_t", "365.24219 d")]
    #[test_case("a_j", "365.25 d")]
    #[test_case("a_g", "365.2425 d")]
    #[test_case("a", "1 a_j")]
    #[test_case("wk", "7 d")]
    #[test_case("mo_s", "29.53059 d")]
    #[test_case("mo_j", "1 a_j/12")]
    #[test_case("mo_g", "1 a_g/12")]
    #[test_case("mo", "1 mo_j")]
    #[test_case("t", "1e3 kg")]
    #[test_case("bar", "1e5 Pa")]
    #[test_case("u", "1.6605402e-24 g")]
    //#[test_case("eV", "1 [e].V")]  // TODO: use this once [e] is defined
    #[test_case("eV", "1.60217733e-19 C.V")]
    #[test_case("AU", "149597.870691 Mm")]
    #[test_case("pc", "3.085678e16 m")]

    fn correct_tables(txt1: &str, txt2: &str) {
        let system = UnitSystemFactory::<f64>::new().build();
        let unit = system.parse(txt1).unwrap();
        let exp = system.parse(txt2).unwrap();
        assert_eq_q!(unit, exp);
    }
}
