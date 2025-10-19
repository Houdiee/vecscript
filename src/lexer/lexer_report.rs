use ariadne::{Color, Label};

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

    fn labels<'a>(&self, file_name: &'a str, span: Range<usize>) -> Vec<Label<(&'a str, Range<usize>)>> {
        vec![
            Label::new((file_name, span))
                .with_message(format!("{}", self))
                .with_color(Color::Red),
        ]
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
