use std::{fmt::Display, ops::Range};

use crate::report_error::ReportableError;

#[derive(Debug, Clone)]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LexerErrorKind {
    InvalidAscii,
    InvalidCharacter,
    InvalidNumber,
    UnterminatedString,
    UnexpectedEOF,
}
