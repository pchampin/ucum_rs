//! Error type for this crate.

use std::error::Error;

/// Error generated while parsing UCUM strings.
#[derive(Debug)]
pub struct UcumError<'a> {
    message: &'static str,
    txt: &'a [u8],
    position: usize,
    cause: Option<Box<dyn Error>>,
}

impl<'a> UcumError<'a> {
    /// Build a new error with a given message, parsed string and position.
    pub fn new(message: &'static str, txt: &'a [u8], position: usize) -> Self {
        assert!(position <= txt.len());
        UcumError {
            message,
            txt,
            position,
            cause: None,
        }
    }

    /// Add parsed string and position to an existing `UcumError`.
    pub fn at_location(mut self, txt: &'a [u8], position: usize) -> Self {
        if self.txt.is_empty() {
            self.txt = txt;
            self.position = position;
        }
        self
    }

    /// Add a cause error to an existing `UcumError`.
    pub fn with_cause(mut self, cause: Box<dyn Error>) -> Self {
        self.cause = Some(cause);
        self
    }
}

impl From<&'static str> for UcumError<'_> {
    fn from(message: &'static str) -> Self {
        UcumError {
            message,
            txt: &b""[..],
            position: 0,
            cause: None,
        }
    }
}

impl std::fmt::Display for UcumError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.txt.is_empty() {
            write!(f, "UcumError: {}", self.message)?;
        } else {
            writeln!(f, "UcumError at {}: {}", self.position, self.message)?;
            let inf = if self.position < 10 {
                0
            } else {
                self.position - 10
            };
            let sup = if self.txt.len() < self.position + 10 {
                self.txt.len()
            } else {
                self.position + 10
            };
            let prefix = if inf == 0 { "" } else { "..." };
            let suffix = if sup < self.txt.len() { "..." } else { "" };
            let printable = String::from_utf8_lossy(&self.txt[inf..sup]);
            writeln!(f, "{}{}{}", prefix, printable, suffix)?;
            let spaces = vec![b' '; prefix.len() + self.position.min(10)];
            let spaces = unsafe { String::from_utf8_unchecked(spaces) };
            writeln!(f, "{}^", spaces)?;
        }
        if let Some(cause) = &self.cause {
            writeln!(f, "\ncaused by:\n{}", cause)?;
        }
        Ok(())
    }
}

impl Error for UcumError<'_> {}

/// Extend `Result<_, UcumError>` with methods to enrich the error.
pub trait ResultExt<'a> {
    /// See [`UcumError::at_location`](./struct.UcumError.html#method.at_location)
    fn at_location(self, txt: &'a [u8], position: usize) -> Self;
    /// See [`UcumError::at_location`](./struct.UcumError.html#method.with_cause)
    fn with_cause(self, cause: Box<dyn Error>) -> Self;
}

impl<'a, T> ResultExt<'a> for Result<T, UcumError<'a>> {
    fn at_location(self, txt: &'a [u8], position: usize) -> Self {
        self.map_err(|e| e.at_location(txt, position))
    }
    fn with_cause(self, cause: Box<dyn Error>) -> Self {
        self.map_err(|e| e.with_cause(cause))
    }
}

/// Type alias for result whose error type is [`UcumError`](./struct.UcumError.html).
pub type UcumResult<'a, T> = std::result::Result<T, UcumError<'a>>;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn display_no_txt() {
        let err: UcumError = "simple message".into();
        assert_eq!(&format!("{}", err), "UcumError: simple message");
    }

    #[test]
    fn display_short_txt_zero() {
        let err = UcumError::new("unexpected token", b"0123456789", 0);
        assert_eq!(
            &format!("{}", err),
            "UcumError at 0: unexpected token
0123456789
^\n"
        );
    }

    #[test]
    fn display_short_txt_near() {
        let err = UcumError::new("unexpected token", b"0123456789", 4);
        assert_eq!(
            &format!("{}", err),
            "UcumError at 4: unexpected token
0123456789
    ^\n"
        );
    }

    #[test]
    fn display_long_txt_zero() {
        let err = UcumError::new("unexpected token", b"0123456789ABCDEFGH", 0);
        assert_eq!(
            &format!("{}", err),
            "UcumError at 0: unexpected token
0123456789...
^\n"
        );
    }

    #[test]
    fn display_long_txt_near() {
        let err = UcumError::new("unexpected token", b"0123456789ABCDEFGH", 4);
        assert_eq!(
            &format!("{}", err),
            "UcumError at 4: unexpected token
0123456789ABCD...
    ^\n"
        );
    }

    #[test]
    fn display_long_txt_ten() {
        let err = UcumError::new("unexpected token", b"0123456789ABCDEFGH", 10);
        assert_eq!(
            &format!("{}", err),
            "UcumError at 10: unexpected token
0123456789ABCDEFGH
          ^\n"
        );
    }

    #[test]
    fn display_long_txt_far() {
        let err = UcumError::new("unexpected token", b"0123456789ABCDEFGH", 12);
        assert_eq!(
            &format!("{}", err),
            "UcumError at 12: unexpected token
...23456789ABCDEFGH
             ^\n"
        );
    }

    #[test]
    fn display_longer_txt_ten() {
        let err = UcumError::new("unexpected token", b"0123456789ABCDEFGHIJKLMNOP", 10);
        assert_eq!(
            &format!("{}", err),
            "UcumError at 10: unexpected token
0123456789ABCDEFGHIJ...
          ^\n"
        );
    }

    #[test]
    fn display_longer_txt_far() {
        let err = UcumError::new("unexpected token", b"0123456789ABCDEFGHIJKLMNOP", 12);
        assert_eq!(
            &format!("{}", err),
            "UcumError at 12: unexpected token
...23456789ABCDEFGHIJKL...
             ^\n"
        );
    }

    #[test]
    fn display_longer_txt_further() {
        let err = UcumError::new("unexpected token", b"0123456789ABCDEFGHIJKLMNOP", 25);
        assert_eq!(
            &format!("{}", err),
            "UcumError at 25: unexpected token
...FGHIJKLMNOP
             ^\n"
        );
    }
}
