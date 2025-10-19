use crate::interpreter_error::{InterpreterError, ToInterpreterError};
use ariadne::{Config, Label, Report, ReportKind, Source};
use std::ops::Range;

pub trait ReportableError {
    fn span(&self) -> Range<usize>;
    fn report_kind_message(&self) -> &'static str;
    fn primary_message(&self) -> String;
    fn labels<'a>(&self, file_name: &'a str, span: Range<usize>) -> Vec<Label<(&'a str, Range<usize>)>>;
}

impl ReportableError for InterpreterError {
    fn span(&self) -> Range<usize> {
        match self {
            InterpreterError::Lexer(err) => err.span(),
            InterpreterError::Parser(err) => err.span(),
            InterpreterError::Semantic(err) => err.span(),
        }
    }

    fn primary_message(&self) -> String {
        match self {
            InterpreterError::Lexer(err) => err.primary_message(),
            InterpreterError::Parser(err) => err.primary_message(),
            InterpreterError::Semantic(err) => err.primary_message(),
        }
    }

    fn report_kind_message(&self) -> &'static str {
        match self {
            InterpreterError::Lexer(err) => err.report_kind_message(),
            InterpreterError::Parser(err) => err.report_kind_message(),
            InterpreterError::Semantic(err) => err.report_kind_message(),
        }
    }

    fn labels<'a>(&self, file_name: &'a str, span: Range<usize>) -> Vec<Label<(&'a str, Range<usize>)>> {
        match self {
            InterpreterError::Lexer(err) => err.labels(file_name, span),
            InterpreterError::Parser(err) => err.labels(file_name, span),
            InterpreterError::Semantic(err) => err.labels(file_name, span),
        }
    }
}

pub fn build_report(error: impl ReportableError, file_name: &str) -> Report<'_, (&str, Range<usize>)> {
    let config = Config::default().with_index_type(ariadne::IndexType::Byte);
    let error_span = error.span();
    let report_message = error.report_kind_message();
    let primary_message = error.primary_message();
    let labels = error.labels(file_name, error_span.clone());

    let mut report = Report::build(ReportKind::Error, (file_name, error_span.clone()))
        .with_config(config)
        .with_message(format!("{}: {}", report_message, primary_message));

    for label in labels {
        report = report.with_label(label);
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
