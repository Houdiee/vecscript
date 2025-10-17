use crate::{lexer::lexer_error::LexerError, parser::parser_error::ParserError};

#[derive(Debug, Clone)]
pub enum InterpreterError {
    Lexer(LexerError),
    Parser(ParserError),
}

pub trait ToInterpreterError: Sized {
    fn to_interpreter_error(self) -> InterpreterError;
}

impl ToInterpreterError for LexerError {
    fn to_interpreter_error(self) -> InterpreterError {
        InterpreterError::Lexer(self)
    }
}

impl ToInterpreterError for ParserError {
    fn to_interpreter_error(self) -> InterpreterError {
        InterpreterError::Parser(self)
    }
}
