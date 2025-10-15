use std::{fmt::Display, ops::Range};

#[derive(Debug)]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug, PartialEq)]
pub enum LexerErrorKind {
    InvalidAscii,
    InvalidCharacter,
    InvalidNumber,
    UnterminatedString,
    UnexpectedEOF,
}

impl Display for LexerErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use LexerErrorKind::*;
        match self {
            InvalidAscii => write!(f, "Invalid ASCII character"),
            InvalidCharacter => write!(f, "Invalid character found"),
            InvalidNumber => write!(f, "Invalid number literal"),
            UnterminatedString => write!(f, "Unterminated string literal"),
            UnexpectedEOF => write!(f, "Unexpected end of file"),
        }
    }
}
