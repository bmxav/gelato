use thiserror::Error;
use std::fmt;

#[derive(Error, Debug)]
pub enum ParseErrorKind {
    #[error("unexpected token: {0}")]
    UnexpectedToken(String),
    #[error("unterminated string: {0}")]
    UnterminatedString(String),
    #[error("invalid int literal: {0}")]
    InvalidIntLiteral(String),
    #[error("invalid string escape sequence '{1}' at position {0}")]
    InvalidEscapeSequence(usize, char),
}

#[derive(Error, Debug)]
pub struct ParseError {
    pub line: usize,
    pub col: usize,
    #[source]
    pub kind: ParseErrorKind
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} (line: {}, col: {})", self.kind, self.line, self.col)
    }
}
