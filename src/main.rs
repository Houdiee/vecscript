use crate::{lexer::lexer::Lexer, parser::parser::Parser, report_error::print_errors};

pub mod ast;
pub mod interpreter_error;
pub mod lexer;
pub mod parser;
pub mod report_error;
pub mod token;

fn main() {
    let file_name = String::from("example");
    let source = std::fs::read_to_string(&file_name).expect("Failed to read file");

    let mut lexer = Lexer::new(&source);
    let (tokens, lexer_errors) = lexer.lex();
    if !lexer_errors.is_empty() {
        print_errors(&file_name, &source, lexer_errors);
    }

    let mut parser = Parser::new(tokens);
    let (program, parser_errors) = parser.parse();
    if !parser_errors.is_empty() {
        print_errors(&file_name, &source, parser_errors);
    }
    println!("{:#?}", program);
}
