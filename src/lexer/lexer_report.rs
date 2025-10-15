use crate::{
    lexer::lexer_error::{LexerError, LexerErrorKind},
    report_error::ReportableError,
};
use std::{fmt::Display, ops::Range};

impl ReportableError for LexerError {
    fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    fn report_kind_message(&self) -> &'static str {
        "Lexer Error"
    }

    fn primary_message(&self) -> String {
        format!("{}", self)
    }

    fn label_message(&self) -> String {
        self.primary_message()
    }

    fn custom_label<'a>(&self, _file_name: &'a str, _span: Range<usize>) -> Option<ariadne::Label<(&'a str, Range<usize>)>> {
        None
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use LexerErrorKind::*;
        match &self.kind {
            InvalidAscii => write!(f, "Invalid ASCII character"),
            InvalidCharacter => write!(f, "Invalid character found"),
            InvalidNumber => write!(f, "Invalid number literal"),
            UnterminatedString => write!(f, "Unterminated string literal"),
            UnexpectedEOF => write!(f, "Unexpected end of file"),
        }
    }
}
