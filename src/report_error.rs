use crate::{interpreter_error::InterpreterError, lexer::lexer_error::LexerError, parser::parser_error::ParserError};
use ariadne::{Color, Config, Label, Report, ReportKind, Source};
use std::ops::Range;

pub trait ReportableError {
    fn span(&self) -> Range<usize>;
    fn report_kind_message(&self) -> &'static str;
    fn primary_message(&self) -> String;
    fn label_message(&self) -> String;
    fn custom_label<'a>(&self, file_name: &'a str, span: Range<usize>) -> Option<Label<(&'a str, Range<usize>)>>;
}

impl ReportableError for InterpreterError {
    fn span(&self) -> Range<usize> {
        match self {
            InterpreterError::Lexer(err) => err.span(),
            InterpreterError::Parser(err) => err.span(),
        }
    }

    fn primary_message(&self) -> String {
        match self {
            InterpreterError::Lexer(err) => err.primary_message(),
            InterpreterError::Parser(err) => err.primary_message(),
        }
    }

    fn report_kind_message(&self) -> &'static str {
        match self {
            InterpreterError::Lexer(err) => err.report_kind_message(),
            InterpreterError::Parser(err) => err.report_kind_message(),
        }
    }

    fn label_message(&self) -> String {
        match self {
            InterpreterError::Lexer(err) => err.label_message(),
            InterpreterError::Parser(err) => err.label_message(),
        }
    }

    fn custom_label<'a>(&self, file_name: &'a str, span: Range<usize>) -> Option<Label<(&'a str, Range<usize>)>> {
        match self {
            InterpreterError::Lexer(err) => err.custom_label(file_name, span),
            InterpreterError::Parser(err) => err.custom_label(file_name, span),
        }
    }
}

pub fn build_report<'a>(error: impl ReportableError, file_name: &'a str) -> Report<'_, (&'a str, Range<usize>)> {
    let config = Config::default().with_index_type(ariadne::IndexType::Byte);
    let error_span = error.span();
    let primary_message = error.primary_message();
    let label_message = error.label_message();
    let report_message = error.report_kind_message();

    let mut report = Report::build(ReportKind::Error, (file_name, error_span.clone()))
        .with_config(config)
        .with_message(format!("{}: {}", report_message, primary_message));

    report = report.with_label(
        Label::new((file_name, error_span.clone()))
            .with_message(label_message)
            .with_color(Color::Red)
            .with_order(0),
    );

    if let Some(custom_label) = error.custom_label(file_name, error_span.clone()) {
        report = report.with_label(custom_label);
    }

    report.finish()
}

pub fn print_report(error: impl ReportableError, file_name: &str, source: &str) {
    let report = build_report(error, file_name);
    let source_map = Source::from(source);
    report.eprint((file_name, source_map)).expect("Failed to print error report")
}

pub fn print_errors(file_name: &str, source: &str, errors: impl IntoIterator<Item = impl ToInterpreterError>) {
    for error in errors.into_iter() {
        let interpreter_error = error.to_interpreter_error();
        print_report(interpreter_error, file_name, source);
    }
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
