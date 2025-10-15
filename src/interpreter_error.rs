use crate::{
    lexer_error::LexerError,
    parser_error::{ParserError, ParserErrorKind},
};
use ariadne::{Label, Report, ReportKind, Source};
use std::ops::Range;

#[derive(Debug)]
pub enum InterpreterError {
    Lexer(LexerError),
    Parser(ParserError),
}

pub struct Diagnostic {
    pub file_name: String,
    pub source: String,
    pub error: InterpreterError,
}

pub trait IntoInterpreterError: Sized + Clone {
    fn into_interpreter_error(self) -> InterpreterError;
}

impl IntoInterpreterError for LexerError {
    fn into_interpreter_error(self) -> InterpreterError {
        InterpreterError::Lexer(self)
    }
}

impl IntoInterpreterError for ParserError {
    fn into_interpreter_error(self) -> InterpreterError {
        InterpreterError::Parser(self)
    }
}

pub fn print_errors(file_name: &str, source: &str, errors: &[impl IntoInterpreterError]) {
    for error in errors {
        let owned_error = error.clone();

        let diagnostic = Diagnostic {
            file_name: file_name.to_string(),
            source: source.to_string(),
            error: owned_error.into_interpreter_error(),
        };
        diagnostic.print_report();
    }
}

impl Diagnostic {
    pub fn print_report(&self) {
        let report = self.build_report();
        let file_name = self.file_name.clone();
        let source = Source::from(&self.source);
        report.eprint((file_name, source)).expect("Failed to print error report")
    }

    fn build_report(&self) -> Report<(String, Range<usize>)> {
        let file_name = self.file_name.clone();

        match &self.error {
            InterpreterError::Lexer(lexer_error) => {
                let error_span = lexer_error.span.clone();
                let message = format!("{}", lexer_error.kind);

                Report::build(ReportKind::Error, (file_name.clone(), error_span.clone()))
                    .with_message(format!("Lexing Error: {}", message))
                    .with_label(
                        Label::new((file_name, error_span))
                            .with_message(message)
                            .with_color(ariadne::Color::Red),
                    )
                    .finish()
            }

            InterpreterError::Parser(parser_error) => {
                let error_span = parser_error.token.span.clone();
                let message = format!("{}", parser_error);
                let report = Report::build(ReportKind::Error, (file_name.clone(), error_span.clone())).with_message(message.clone());
                let report = report.with_label(
                    Label::new((file_name.clone(), error_span))
                        .with_message(format!("Found: {:?}", parser_error.token.kind))
                        .with_color(ariadne::Color::Red),
                );
                if let ParserErrorKind::UnexpectedToken { expected } = &parser_error.kind {
                    report.with_note(format!("Hint: Expected {}", expected)).finish()
                } else {
                    report.finish()
                }
            }
        }
    }
}
