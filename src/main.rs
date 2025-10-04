use crate::lexer::Lexer;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

fn main() {
    let file_name = "example";
    let source = std::fs::read_to_string(&file_name).unwrap();
    let mut lexer = Lexer::init(&source).unwrap();
    let tokens = lexer.scan();
    for token in tokens {
        println!("{:?}", token);
    }
}
