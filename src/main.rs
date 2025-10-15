use crate::{lexer::Lexer, parser::Parser};

pub mod ast;
pub mod interpreter_error;
pub mod lexer;
pub mod lexer_error;
pub mod parser;
pub mod parser_error;
pub mod token;

fn main() {
    let file_name = "example";
    let source = std::fs::read_to_string(&file_name).expect("Failed to read file");

    let mut lexer = Lexer::init(&source).expect("Failed to initialize lexer");
    let (tokens, lexer_errors) = lexer.lex();

    let mut parser = Parser::init(tokens);
    let (program, parser_errors) = parser.parse();
}
