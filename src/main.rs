use crate::{lexer::Lexer, parser::Parser, token::Token};

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

fn main() {
    let file_name = "example";
    let source = std::fs::read_to_string(&file_name).unwrap();

    let mut lexer = Lexer::init(&source).expect("Failed to initialize lexer.");
    let (tokens, lexer_errors) = lexer.lex();
}
