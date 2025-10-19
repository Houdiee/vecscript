use ariadne::{Color, Label};

use crate::{
    report_error::ReportableError,
    semantic::semantic_error::{SemanticError, SemanticErrorKind},
};
use std::fmt::Display;

impl ReportableError for SemanticError {
    fn span(&self) -> std::ops::Range<usize> {
        self.span.clone()
    }

    fn report_kind_message(&self) -> &'static str {
        "Semantic Analysis Error"
    }

    fn primary_message(&self) -> String {
        format!("{}", self)
    }

    fn labels<'a>(&self, file_name: &'a str, span: std::ops::Range<usize>) -> Vec<Label<(&'a str, std::ops::Range<usize>)>> {
        match &self.kind {
            _ => vec![
                Label::new((file_name, span))
                    .with_message(format!("{}", self))
                    .with_color(Color::Red),
            ],
        }
    }
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use SemanticErrorKind::*;
        match &self.kind {
            UndefinedIdentifier { name } => write!(f, "Undefined identifier {name}"),
            TypeMismatch { expected, found } => write!(f, "Expected type {expected} but found {found}"),
            NonBooleanCondition => write!(f, "Condition doesn't evaluate to bool"),
        }
    }
}
