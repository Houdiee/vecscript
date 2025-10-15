use crate::{lexer::lexer_error::LexerError, parser::parser_error::ParserError};

#[derive(Debug, Clone)]
pub enum InterpreterError {
    Lexer(LexerError),
    Parser(ParserError),
}
