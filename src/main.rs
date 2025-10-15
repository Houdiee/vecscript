use crate::{interpreter_error::print_errors, lexer::Lexer, parser::Parser};

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

    let mut lexer = match Lexer::init(&source) {
        Ok(lexer) => lexer,
        Err(error) => {
            print_errors(&file_name, &source, [error]);
            return;
        }
    };

    let (tokens, lexer_errors) = lexer.lex();
    if !lexer_errors.is_empty() {
        print_errors(&file_name, &source, lexer_errors);
        return;
    }

    let mut parser = Parser::init(tokens);
    let (program, parser_errors) = parser.parse();
    if !parser_errors.is_empty() {
        print_errors(&file_name, &source, parser_errors);
        return;
    }
}
