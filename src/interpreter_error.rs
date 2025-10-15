use crate::{lexer_error::LexerError, parser_error::ParserError};
use ariadne::Report;
use std::ops::Range;

#[derive(Debug)]
pub enum InterpreterError {
    Lexer(LexerError),
    Parser(ParserError),
}

pub struct Diagnostic {
    pub file_name: String,
    pub source: String,
    pub error: InterpreterError,
}

impl Diagnostic {
    pub fn print_report(&self) {
        let report = self.build_report();
    }

    fn build_report(&self) -> Report<(String, Range<usize>)> {
        todo!()
    }
}
