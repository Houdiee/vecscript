use crate::{lexer::Lexer, parser::Parser, token::Token};

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

fn main() {
    let file_name = "example";
    let source = std::fs::read_to_string(&file_name).unwrap();

    let mut lexer = Lexer::init(&source).expect("Failed to initialize lexer.");
    let tokens: Vec<Token> = lexer.lex().into_iter().map(|t| t.unwrap()).collect();

    let mut parser = Parser::init(tokens);
    let program = parser.parse_program().unwrap();
    for definition in program.definitions {
        println!("{:?}", definition);
    }
}
