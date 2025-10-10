use crate::{lexer::Lexer, parser::Parser, token::Token};

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

fn main() {
    let file_name = "example";
    let source = std::fs::read_to_string(&file_name).unwrap();
    let mut lexer = Lexer::init(&source).unwrap();
    let tokens: Vec<Token> = lexer
        .scan()
        .into_iter() // Use into_iter() to consume the Vec
        .map(|t| t.expect("Lexing error occurred")) // Assuming scan() returns Vec<Result<Token, LexerError>>
        .collect();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("{:?}", ast);
}
