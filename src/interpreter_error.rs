use crate::{lexer_error::LexerError, parser_error::ParserError};

#[derive(Debug)]
pub enum InterpreterError {
    Lexer(LexerError),
    Parser(ParserError),
}
