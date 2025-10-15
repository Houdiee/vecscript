use crate::{
    interpreter_error::{Diagnostic, InterpreterError},
    lexer::Lexer,
    parser::Parser,
};

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
            let diagnostic = Diagnostic {
                file_name: file_name.to_string(),
                source: source.clone(),
                error: InterpreterError::Lexer(error),
            };
            diagnostic.print_report();
            return;
        }
    };

    let (tokens, lexer_errors) = lexer.lex();

    if !lexer_errors.is_empty() {
        for error in lexer_errors {
            let diagnostic = Diagnostic {
                file_name: file_name.to_string(),
                source: source.clone(),
                error: InterpreterError::Lexer(error.clone()),
            };
            diagnostic.print_report();
        }
    }
    let mut parser = Parser::init(tokens);
    let (_program, parser_errors) = parser.parse();

    if !parser_errors.is_empty() {
        for error in parser_errors {
            let diagnostic = Diagnostic {
                file_name: file_name.to_string(),
                source: source.clone(),
                error: InterpreterError::Parser(error.clone()),
            };
            diagnostic.print_report();
        }
    }
}
