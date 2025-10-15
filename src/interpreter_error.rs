use crate::{
    lexer_error::LexerError,
    parser_error::{ParserError, ParserErrorKind},
};
use ariadne::{Color, Label, Report, ReportKind, Source};
use std::ops::Range;

#[derive(Debug, Clone)]
pub enum InterpreterError {
    Lexer(LexerError),
    Parser(ParserError),
}

pub trait ToInterpreterError: Sized {
    fn to_interpreter_error(self) -> InterpreterError;
}

impl ToInterpreterError for LexerError {
    fn to_interpreter_error(self) -> InterpreterError {
        InterpreterError::Lexer(self)
    }
}

impl ToInterpreterError for &LexerError {
    fn to_interpreter_error(self) -> InterpreterError {
        InterpreterError::Lexer(self.clone())
    }
}

impl ToInterpreterError for ParserError {
    fn to_interpreter_error(self) -> InterpreterError {
        InterpreterError::Parser(self)
    }
}
impl ToInterpreterError for &ParserError {
    fn to_interpreter_error(self) -> InterpreterError {
        InterpreterError::Parser(self.clone())
    }
}

impl InterpreterError {
    fn primary_info(&self) -> (Range<usize>, String) {
        match self {
            InterpreterError::Lexer(err) => (err.span.clone(), format!("{}", err.kind)),
            InterpreterError::Parser(err) => (err.token.span.clone(), format!("Found: {}", err.token.kind)),
        }
    }

    pub fn build_report<'a>(&self, file_name: &'a str) -> Report<'_, (&'a str, Range<usize>)> {
        let (error_span, primary_message) = self.primary_info();
        let (report_kind, report_message) = match self {
            InterpreterError::Lexer(_) => (ReportKind::Error, "Lexing Error"),
            InterpreterError::Parser(_) => (ReportKind::Error, "Parsing Error"),
        };

        let mut report =
            Report::build(report_kind, (file_name, error_span.clone())).with_message(format!("{}: {}", report_message, primary_message));

        report = report.with_label(
            Label::new((file_name, error_span.clone()))
                .with_message(primary_message)
                .with_color(Color::Red),
        );

        if let InterpreterError::Parser(parser_error) = self {
            if let ParserErrorKind::UnexpectedToken { expected } = &parser_error.kind {
                let insertion_point = error_span.start;
                let expected_span = insertion_point..insertion_point + 1;

                report = report.with_label(
                    Label::new((file_name, expected_span))
                        .with_message(format!("Expected: {}", expected))
                        .with_color(Color::Blue),
                );

                report = report.with_note(format!("The parser was expecting: {}", expected));
            }
        }

        report.finish()
    }

    pub fn print_report<'a>(&self, file_name: &'a str, source: &'a str) {
        let report = self.build_report(file_name);
        let source_map = Source::from(source);
        report.eprint((file_name, source_map)).expect("Failed to print error report")
    }
}

pub fn print_errors(file_name: &str, source: &str, errors: impl IntoIterator<Item = impl ToInterpreterError>) {
    for error in errors.into_iter() {
        let interpreter_error = error.to_interpreter_error();
        interpreter_error.print_report(file_name, source);
    }
}
