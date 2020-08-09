//! Parser for UCUM units and UCUM quantities (i.e. value + unit).
//!
//! See http://unitsofmeasure.org/ucum.html#section-Grammar-of-Units-and-Unit-Terms.
//!
//! Such a parser is usually used transparently through
//! [`UnitSystem.parse`](../system/struct.UnitSystem.html#method.parse)
//! but can be build with
//! [`UnitSystem.parser`](../system/struct.UnitSystem.html#method.parser)
//! in order to configure it prior to use.
//!
//! # Configuration methods
//!
//! These methods can only be called *before* any of the parsing method (see below) are called.
//!
//! * [`exhaustive(flag: bool)`](./struct.Parser.html#method.exhaustive):
//!   determines whether the parser expects to parse the whole of its underlying text (true, default)
//!   or accepts to leave the end of the text unparsed (false).
//!
//!   The remaining text can be accessed after parsing with the
//!   [`remaining`](./struct.Parser.html#method.exhaustive) method.
//!
//! * [`ignore_ws(flag: bool)`](./struct.Parser.html#method.exhaustive):
//!   determines whether the parser will ignore white spaces (true, default);
//!   otherwise (false), whitespaces will not be accepted as part of the UCUM string
//!   (causing the parser to stop or issue an error, depending on the `exhaustive` setting).
//!
//! # Parsing methods
//!
//! Only one of these parsing methods can be called.
//! After that, the parser is considered "consummed".
//!
//! * [`parse_value()`](./struct.Parser.html#method.parse_value):
//!   parse the underlying text,
//!   expecting a value followed by a unit, and returns the corresponding
//!   [`Quantity`](../quantity/struct.Quantity.html).
//!
//! * [`parse_unit()`](./struct.Parser.html#method.parse_unit):
//!   parse the underlying text,
//!   expecting a single unit (*not* preceded by a value), and returns the corresponding
//!   [`Quantity`](../quantity/struct.Quantity.html).
//!
//! * [`parse_to_ast()`](./struct.Parser.html#method.parse_value):
//!   parse the underlying text,
//!   expecting a single unit, and returns an Abstract Syntax Tree (AST)
//!   (for debugging or complex post-processing).

use super::*;
use std::str::from_utf8_unchecked;

/// A parser for UCUM units and UCUM quantities.
///
/// see [module documentation](./index.html)
pub struct Parser<'s, 't, T> {
    system: &'s UnitSystem<T>,
    txt: &'t [u8],
    pos: usize,
    options: ParserOptions,
}

#[derive(Clone, Copy, Debug)]
struct ParserOptions {
    exhaustive: bool,
    ignore_ws: bool,
}

impl Default for ParserOptions {
    fn default() -> Self {
        ParserOptions {
            exhaustive: true,
            ignore_ws: true,
        }
    }
}

impl<'s, 't, T> Parser<'s, 't, T> {
    // configuration

    /// see [module documentation](./index.html)
    pub fn exhaustive(mut self, exhaustive: bool) -> Self {
        assert!(self.pos == 0, "parser already consumed");
        self.options.exhaustive = exhaustive;
        self
    }

    /// see [module documentation](./index.html)
    pub fn ignore_ws(mut self, ignore_ws: bool) -> Self {
        assert!(self.pos == 0, "parser already consumed");
        self.options.ignore_ws = ignore_ws;
        self
    }

    // other public methods

    /// see [module documentation](./index.html)
    pub fn parse_value(&mut self) -> UcumResult<'t, Quantity<T>>
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
        assert!(self.pos == 0, "parser already consumed");
        self.maybe_consume_whitespace();
        let value = self.consume_value();
        // we know that value contains only ASCII chars
        let value: &str = unsafe { from_utf8_unchecked(value) };
        let value: Option<T> = if value.is_empty() {
            None
        } else {
            match value.parse() {
                Ok(v) => Some(v),
                Err(e) => {
                    return self.err("unable to parse value").with_cause(Box::new(e));
                }
            }
        };

        let ast = self.ast(true)?;
        if let Some(value) = value {
            ast.make_quantity(value, self.system)
        } else {
            ast.as_unit_quantity(self.system)
        }
    }

    /// see [module documentation](./index.html)
    pub fn parse_unit(&mut self) -> UcumResult<'t, Quantity<T>>
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
        assert!(self.pos == 0, "parser already consumed");
        self.ast(true)?.as_unit_quantity(self.system)
    }

    /// see [module documentation](./index.html)
    pub fn parse_to_ast(&mut self) -> UcumResult<'t, Box<AST<'t>>> {
        assert!(self.pos == 0, "parser already consumed");
        self.ast(true)
    }

    /// see [module documentation](./index.html)
    pub fn remaining(&self) -> &[u8] {
        &self.txt[self.pos..]
    }

    // private methods

    pub(crate) fn new(system: &'s UnitSystem<T>, txt: &'t [u8]) -> Self {
        Parser {
            system,
            txt,
            pos: 0,
            options: ParserOptions::default(),
        }
    }

    fn err<U>(&self, message: &'static str) -> UcumResult<'t, U> {
        Err(UcumError::new(message, self.txt, self.pos))
    }

    fn ast(&mut self, root: bool) -> UcumResult<'t, Box<AST<'t>>> {
        let txt_len = self.txt.len();
        self.maybe_consume_whitespace();
        if self.pos >= txt_len {
            return self.err("unexpected end of string while looking for term");
        }
        let mut ast = if self.txt[self.pos] == b'/' {
            AST::factor(&b"1"[..])
        } else {
            self.ast_component()?
        };
        while self.pos < txt_len {
            match self.txt[self.pos] {
                b'.' => {
                    self.pos += 1;
                    self.maybe_consume_whitespace();
                    let rhs = self.ast_component()?;
                    ast = AST::product(ast, rhs);
                }
                b'/' => {
                    self.pos += 1;
                    self.maybe_consume_whitespace();
                    let rhs = self.ast_component()?;
                    ast = AST::division(ast, rhs);
                }
                _ => break,
            }
            self.maybe_consume_whitespace();
        }
        if root {
            if self.options.exhaustive && self.pos < txt_len {
                let c = self.txt[self.pos];
                return self.err(match c {
                    MIN_CHAR..=MAX_CHAR => "spurious characters",
                    _ => "invalid character",
                });
            }
        } else if self.pos >= txt_len || self.txt[self.pos] != b')' {
            return self.err("expected closing parenthesis ')'");
        } else {
            self.pos += 1;
        }
        Ok(ast)
    }

    fn ast_component(&mut self) -> UcumResult<'t, Box<AST<'t>>> {
        self.maybe_consume_whitespace();
        if self.pos >= self.txt.len() {
            self.err("unexpected end of string while looking for component")
        } else if self.txt[self.pos] == b'(' {
            self.pos += 1;
            self.ast(false)
        } else {
            let unit = self.consume_unit()?;
            if unit.is_empty() {
                // try factor
                let factor = self.consume_factor();
                if factor.is_empty() {
                    // try lone annotation
                    let annotation = self.consume_annotation()?;
                    if annotation.is_none() {
                        self.err("expected component (unit, annotation, factor or parenthesis)")
                    } else {
                        Ok(AST::unit(unit, 1, annotation))
                    }
                } else {
                    Ok(AST::factor(factor))
                }
            } else {
                self.maybe_consume_whitespace();
                let exponent = self.consume_exponent()?;
                if exponent == 0 {
                    return self.err("exponent can not be 0");
                }
                self.maybe_consume_whitespace();
                let annotation = self.consume_annotation()?;
                Ok(AST::unit(unit, exponent, annotation))
            }
        }
    }

    fn maybe_consume_whitespace(&mut self) {
        if self.options.ignore_ws {
            let txt_len = self.txt.len();
            while self.pos < txt_len && self.txt[self.pos].is_ascii_whitespace() {
                self.pos += 1;
            }
        }
    }

    fn consume_value(&mut self) -> &[u8] {
        let txt_len = self.txt.len();
        if self.pos == txt_len {
            return &self.txt[txt_len..];
        }
        // if unit starts with "10*" or "10^", avoid considerring 10 as the value
        if self.txt[self.pos..].starts_with(b"10*") || self.txt[self.pos..].starts_with(b"10^") {
            return &self.txt[txt_len..];
        }
        enum State {
            Start,
            IntPart,
            DecPart,
            Exp,
            ExpInt,
            EndNotEmpty,
        };
        use State::*;
        let start_pos = self.pos;
        let mut state = Start;
        let mut checkpoint = self.pos;
        let mut digits = 0;
        loop {
            // Transitions
            state = match state {
                Start => match self.txt[self.pos] {
                    b'-' | b'+' => IntPart,
                    b'0'..=b'9' => {
                        digits += 1;
                        IntPart
                    }
                    b'.' => DecPart,
                    _ => break,
                },
                IntPart => match self.txt[self.pos] {
                    b'0'..=b'9' => {
                        digits += 1;
                        IntPart
                    }
                    b'.' => DecPart,
                    b'e' | b'E' => Exp,
                    _ => EndNotEmpty,
                },
                DecPart => match self.txt[self.pos] {
                    b'0'..=b'9' => {
                        digits += 1;
                        DecPart
                    }
                    b'e' | b'E' => Exp,
                    _ => EndNotEmpty,
                },
                Exp => match self.txt[self.pos] {
                    b'-' | b'+' => ExpInt,
                    b'0'..=b'9' => {
                        digits += 1;
                        ExpInt
                    }
                    _ => EndNotEmpty,
                },
                ExpInt => match self.txt[self.pos] {
                    b'0'..=b'9' => {
                        digits += 1;
                        ExpInt
                    }
                    _ => EndNotEmpty,
                },
                EndNotEmpty => {
                    if digits == 0 {
                        // no digits since checkpoint; cutting there
                        self.pos = checkpoint;
                    }
                    break;
                }
            };
            // Before entering new state
            match state {
                Exp => {
                    if digits == 0 {
                        // no digit before exponent; not a proper number
                        self.pos = start_pos;
                        break;
                    } else {
                        checkpoint = self.pos;
                        digits = 0;
                    }
                }
                EndNotEmpty => continue, // no need to increase pos
                _ => {}
            }
            self.pos += 1;
            if self.pos == txt_len {
                state = EndNotEmpty;
            }
        }
        &self.txt[start_pos..self.pos]
    }

    // as a heuristics, we consider that digits are never part of unit names,
    // expect in square brackets and in the special case of 10* and 10^.
    fn consume_unit(&mut self) -> UcumResult<'t, &'t [u8]> {
        let txt_len = self.txt.len();
        let start = self.pos;
        // special cases
        if self.txt[self.pos..].starts_with(b"10*") || self.txt[self.pos..].starts_with(b"10^") {
            self.pos += 3;
        }
        // parse unit name
        while self.pos < txt_len {
            match self.txt[self.pos] {
                b'[' => {
                    self.consume_square_brackets()?;
                }
                b'}'
                | b'{'
                | b'='
                | b'-'
                | b')'
                | b'('
                | b'.'
                | b'+'
                | b'"'
                | b'/'
                | b']'
                | b'0'..=b'9' => {
                    break;
                }
                c if c < MIN_CHAR || MAX_CHAR < c => {
                    break;
                }
                _ => {
                    self.pos += 1;
                }
            }
        }
        Ok(&self.txt[start..self.pos])
    }

    fn consume_square_brackets(&mut self) -> UcumResult<'t, ()> {
        let txt_len = self.txt.len();
        debug_assert!(self.pos < txt_len && self.txt[self.pos] == b'[');
        self.pos += 1;
        while self.pos < txt_len {
            match self.txt[self.pos] {
                b']' => {
                    self.pos += 1;
                    return Ok(());
                }
                b'[' => return self.err("square brackets must not be nested"),
                c if MIN_CHAR <= c && c <= MAX_CHAR => {
                    self.pos += 1;
                }
                _ => return self.err("invalid character in square brackets"),
            }
        }
        self.err("missing closing square bracket ']'")
    }

    fn consume_factor(&mut self) -> &'t [u8] {
        let txt_len = self.txt.len();
        let start = self.pos;
        while self.pos < txt_len && matches!(self.txt[self.pos], b'0'..=b'9') {
            self.pos += 1;
        }
        &self.txt[start..self.pos]
    }

    fn consume_exponent(&mut self) -> UcumResult<'t, i8> {
        let txt_len = self.txt.len();
        if self.pos >= txt_len || !matches!(self.txt[self.pos], b'-' | b'+' | b'0'..=b'9') {
            return Ok(1);
        }
        let start = self.pos;
        self.pos += 1;
        while self.pos < txt_len && matches!(self.txt[self.pos], b'0'..=b'9') {
            self.pos += 1;
        }
        let end = self.pos;
        // we know that txt[start..end] contains only ASCII characters
        let exp_str: &str = unsafe { from_utf8_unchecked(&self.txt[start..end]) };
        exp_str.parse::<i8>().map_err(|err| {
            UcumError::new("invalid exponent", self.txt, start).with_cause(Box::new(err))
        })
    }

    fn consume_annotation(&mut self) -> UcumResult<'t, Option<&'t [u8]>> {
        let txt_len = self.txt.len();
        if self.pos == txt_len || self.txt[self.pos] != b'{' {
            return Ok(None);
        }
        self.pos += 1;
        let start = self.pos;
        while self.pos < txt_len {
            match self.txt[self.pos] {
                b'}' => {
                    let end = self.pos;
                    self.pos += 1;
                    return Ok(Some(&self.txt[start..end]));
                }
                b'{' => return self.err("curly braces must not be nested"),
                c if MIN_CHAR <= c && c <= MAX_CHAR
                    || self.options.ignore_ws && c.is_ascii_whitespace() =>
                {
                    self.pos += 1;
                }
                _ => return self.err("invalid character in annotation"),
            }
        }
        self.err("missing closing curly bracket '}'")
    }
}

const MIN_CHAR: u8 = 33;
const MAX_CHAR: u8 = 126;

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test_case("m"; "empty")]
    #[test_case("42m"; "42")]
    #[test_case("+42m"; "p42")]
    #[test_case("-42m"; "m42")]
    #[test_case("0042m"; "0042")]
    #[test_case("3.14m"; "3p14")]
    #[test_case("+3.14m"; "p3p14")]
    #[test_case("-3.14m"; "m3p14")]
    #[test_case("1.m"; "1p")]
    #[test_case(".5m"; "p5")]
    #[test_case("+.5m"; "pp5")]
    #[test_case("-.5m"; "mp5")]
    #[test_case("9e12m"; "9e12")]
    #[test_case("9E12m"; "9ee12")]
    #[test_case("9e+12m"; "9ep12")]
    #[test_case("9e-12m"; "9em12")]
    #[test_case("3.14e3m"; "3p14e3")]
    #[test_case("-3.14e+3m"; "-3p14ep3")]
    #[test_case("+987.6543e-210m"; "p987p6543em210")]
    fn consume_value_ok(txt: &str) {
        let txt = txt.as_bytes();
        let number = &txt[..txt.len() - 1];
        let units = UnitSystem::<f64>::default();
        let mut p = units.parser(txt);
        assert_eq!(p.consume_value(), number);
        let mut p = units.parser(number);
        assert_eq!(p.consume_value(), number);
    }

    // followed by invalid
    #[test_case("42 ", "42" ; "space")]
    #[test_case("42é", "42" ; "out of range")]
    // wrong sequence of acceptable characters
    #[test_case("2+2", "2" ; "2 plus 2")]
    #[test_case("1.2.3", "1.2" ; "1p2p3")]
    #[test_case("1.2+3", "1.2" ; "1p2 plus 3")]
    #[test_case("1e2.3", "1e2" ; "1e2p3")]
    // patholohical case ("syntactically" ok, semantically wrong)
    #[test_case(".", "" ; "point")]
    #[test_case("+", "" ; "plus")]
    #[test_case("-", "" ; "minus")]
    #[test_case("+.", "" ; "plus point")]
    #[test_case("-.", "" ; "minus point")]
    #[test_case("e12", "" ; "number less exponent")]
    #[test_case(".e12", "" ; "point number less exponent")]
    #[test_case("+.e12", "" ; "plus point number less exponent")]
    #[test_case("42e", "42" ; "42e")]
    #[test_case("42e-", "42" ; "42em")]
    fn consume_value_partial(txt: &str, exp: &str) {
        let txt = txt.as_bytes();
        let exp = exp.as_bytes();
        let units = UnitSystem::<f64>::default();
        let mut p = units.parser(txt);
        assert_eq!(p.consume_value(), exp);
    }

    #[test_case(""; "empty")]
    #[test_case("m")]
    #[test_case("kg")]
    #[test_case("K")]
    #[test_case("rad")]
    #[test_case("%"; "percent")]
    #[test_case("10*"; "ten star")]
    #[test_case("10^"; "ten carret")]
    #[test_case("[pi]")]
    #[test_case("ab[c+ef]")]
    #[test_case("ab[{]")]
    fn consume_unit_ok(txt: &str) {
        let txt = txt.as_bytes();
        let units = UnitSystem::<f64>::default();
        let mut p = units.parser(txt);
        assert_eq!(p.consume_unit().unwrap(), txt);
    }

    #[test_case("m ", "m"; "space")]
    #[test_case("m2", "m"; "digit")]
    #[test_case("m+", "m"; "plus")]
    #[test_case("m-", "m"; "minus")]
    #[test_case("m.", "m"; "period")]
    #[test_case("m/", "m"; "solidus")]
    #[test_case("m{", "m"; "curly open")]
    #[test_case("m}", "m"; "curly close")]
    #[test_case("m(", "m"; "paren open")]
    #[test_case("m)", "m"; "paren close")]
    #[test_case("m]", "m"; "sqare close")]
    #[test_case("m=", "m"; "equal")]
    #[test_case("m\"", "m"; "quote")]
    #[test_case("mé", "m"; "invalid")]
    fn consume_unit_partial<'a>(txt: &'a str, exp: &str) {
        let txt = txt.as_bytes();
        let exp = exp.as_bytes();
        let units = UnitSystem::<f64>::default();
        let mut p = units.parser(txt);
        assert_eq!(p.consume_unit().unwrap(), exp);
        // trying with an empty unit and the same suffix
        let units = UnitSystem::<f64>::default();
        let mut p = units.parser(&txt[exp.len()..]);
        assert_eq!(p.consume_unit().unwrap(), &b""[..]);
    }

    #[test_case("m["; "unclosed square bracket")]
    #[test_case("m[a[b]c]"; "nested square brackets")]
    #[test_case("m[é"; "invalid character")]
    fn consume_unit_err(txt: &str) {
        let txt = txt.as_bytes();
        let units = UnitSystem::<f64>::default();
        let mut p = units.parser(txt);
        assert!(p.consume_unit().is_err());
    }

    #[test_case("", 1; "empty")]
    #[test_case("a", 1; "spurious")]
    #[test_case("{", 1; "annotation")]
    #[test_case(" ", 1; "space")]
    #[test_case("2", 2)]
    #[test_case("42", 42)]
    #[test_case("+42", 42; "plus 42")]
    #[test_case("-42", -42; "minus 42")]
    fn consume_exponent_ok(txt: &str, exp: i8) {
        let txt = txt.as_bytes();
        let units = UnitSystem::<f64>::default();
        let mut p = units.parser(txt);
        assert_eq!(p.consume_exponent().unwrap(), exp);
    }

    #[test_case("+"; "plus")]
    #[test_case("-"; "minus")]
    fn consume_exponent_err(txt: &str) {
        let txt = txt.as_bytes();
        let units = UnitSystem::<f64>::default();
        let mut p = units.parser(txt);
        assert!(p.consume_exponent().is_err());
    }

    #[test_case("{}"; "empty braces")]
    #[test_case("{a}")]
    #[test_case("{vol}")]
    #[test_case("{a+- \"=][)(}"; "special characters")]
    fn consume_annotation_ok<'a>(txt: &'a str) {
        let txt = txt.as_bytes();
        let exp = &txt[1..txt.len() - 1];
        let units = UnitSystem::<f64>::default();
        let mut p = units.parser(txt);
        assert_eq!(p.consume_annotation().unwrap(), Some(exp));
    }

    #[test_case(""; "empty")]
    #[test_case(" "; "space")]
    #[test_case("."; "period")]
    #[test_case("/"; "solidus")]
    #[test_case("a"; "letter")]
    fn consume_annotation_empty<'a>(txt: &'a str) {
        let txt = txt.as_bytes();
        let units = UnitSystem::<f64>::default();
        let mut p = units.parser(txt);
        assert_eq!(p.consume_annotation().unwrap(), None);
    }

    #[test_case("{"; "unclosed square bracket")]
    #[test_case("{a{b}c}"; "nested square brackets")]
    #[test_case("{aé"; "invalid character")]
    fn consume_annotation_err(txt: &str) {
        let txt = txt.as_bytes();
        let units = UnitSystem::<f64>::default();
        let mut p = units.parser(txt);
        assert!(p.consume_annotation().is_err());
    }

    #[test_case("42",  AST::factor(b"42"); "42")]
    #[test_case("m", AST::unit(b"m", 1, None); "m")]
    #[test_case("m2", AST::unit(b"m", 2, None); "m2")]
    #[test_case(" m 2 ", AST::unit(b"m", 2, None); "m2 with spaces")]
    #[test_case("s-1", AST::unit(b"s", -1, None); "s minus 1")]
    #[test_case("m.s", AST::product(AST::unit(b"m", 1, None), AST::unit(b"s", 1, None)); "m s")]
    #[test_case("m/s", AST::division(AST::unit(b"m", 1, None), AST::unit(b"s", 1, None)); "m per s")]
    #[test_case("/s", AST::division(AST::factor(b"1"), AST::unit(b"s", 1, None)); "per s")]
    #[test_case("rad/s", AST::division(AST::unit(b"rad", 1, None), AST::unit(b"s", 1, None)); "rad per s")]
    #[test_case("[pi].rad", AST::product(AST::unit(b"[pi]", 1, None), AST::unit(b"rad", 1, None)); "pi rad")]
    #[test_case("2.[pi].rad", AST::product(AST::product(AST::factor(b"2"), AST::unit(b"[pi]", 1, None)), AST::unit(b"rad", 1, None)); "2pi rad")]
    #[test_case("% {vol}", AST::unit(b"%", 1, Some(b"vol")); "percent vol")]
    #[test_case("{pcs}", AST::unit(b"", 1, Some(b"pcs")); "pcs")]
    #[test_case("1/2.3", AST::product(AST::division(AST::factor(b"1"), AST::factor(b"2")), AST::factor(b"3")); "half three")]
    #[test_case("1/(2.[pi])", AST::division(AST::factor(b"1"), AST::product(AST::factor(b"2"), AST::unit(b"[pi]", 1, None))); "one on 2pi")]
    #[test_case("(((1)/(2)).((3)))", AST::product(AST::division(AST::factor(b"1"), AST::factor(b"2")), AST::factor(b"3")); "many parenthesis")]
    // spurious characters
    #[test_case("m a", AST::unit(b"m", 1, None); "spurious letters after space")]
    #[test_case("m)", AST::unit(b"m", 1, None); "spurious closing paren")]
    #[test_case("m 2 {a} x", AST::unit(b"m", 2, Some(b"a")); "spurious after complex unit")]
    #[test_case("2 {a} x", AST::factor(b"2"); "spurious annotation after factor")]
    fn ast<'a>(txt: &'a str, ast: Box<AST<'a>>) {
        let txt = txt.as_bytes();
        let units = UnitSystem::<f64>::default();
        let mut p = units.parser(txt).exhaustive(false);
        assert_eq!(p.parse_to_ast().unwrap(), ast);
    }

    #[test_case("(42"; "missing closing parenthesis at eof")]
    #[test_case("(42a"; "missing closing parenthesis at spurious")]
    #[test_case("m-"; "wrong exponent")]
    #[test_case("m{{}}"; "wrong annotation")]
    #[test_case("m[[]]"; "wrong square brackets")]
    fn ast_err<'a>(txt: &'a str) {
        let txt = txt.as_bytes();
        let units = UnitSystem::<f64>::default();
        let mut p = units.parser(txt).exhaustive(false);
        assert!(p.parse_to_ast().is_err());
    }
}
