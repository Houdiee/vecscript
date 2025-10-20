use ariadne::{Color, Label};

use crate::{
    report_error::ReportableError,
    semantic::semantic_error::{SemanticError, SemanticErrorKind, TypeMismatchKind},
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
        use SemanticErrorKind::*;
        match &self.kind {
            UndefinedIdentifier { name: _ } => vec![
                Label::new((file_name, span))
                    .with_message("Undefined identifier")
                    .with_color(Color::Red),
            ],

            IdentifierAlreadyDeclared {
                name: _,
                original_location,
            } => vec![
                Label::new((file_name, original_location.clone()))
                    .with_message("Declared here")
                    .with_color(Color::Red)
                    .with_order(0),
                Label::new((file_name, span))
                    .with_message("Redeclared here")
                    .with_color(Color::Red)
                    .with_order(1),
            ],

            IncorrectArgumentCount {
                expected,
                found,
                original_location,
            } => {
                vec![
                    Label::new((file_name, original_location.clone()))
                        .with_message(format!("Takes in {expected} arguments"))
                        .with_color(Color::Red)
                        .with_order(0),
                    Label::new((file_name, span.clone()))
                        .with_message(format!("{found} arguments passed"))
                        .with_color(Color::Red)
                        .with_order(1),
                ]
            }

            TypeMismatch { kind, expected, found } => match kind {
                TypeMismatchKind::ThenElseReturn { then_location } => vec![
                    Label::new((file_name, then_location.clone()))
                        .with_message(format!("Evalutes to {expected}"))
                        .with_color(Color::Red)
                        .with_order(0),
                    Label::new((file_name, span))
                        .with_message(format!("Expected {expected}, found {found}"))
                        .with_color(Color::Red)
                        .with_order(1),
                ],

                _ => vec![
                    Label::new((file_name, span))
                        .with_message(format!("Expected {expected}, found {found}"))
                        .with_color(Color::Red),
                ],
            },

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
        use TypeMismatchKind::*;
        match &self.kind {
            UndefinedIdentifier { name } => write!(f, "Undefined identifier {name:?}"),
            NonBooleanCondition => write!(f, "Condition doesn't evaluate to bool"),
            IdentifierAlreadyDeclared {
                name,
                original_location: _,
            } => write!(f, "Identifier {name:?} already declared"),

            TypeMismatch { kind, expected, found } => match kind {
                ThenElseReturn { then_location: _ } => write!(
                    f,
                    "Mismatched types in 'if then/else' branches. Expected branch to yield type {expected}, but found {found}"
                ),
                TypeAnnotation => write!(
                    f,
                    "Mismatched type in variable assignment. Expected type {expected}, but found {found}"
                ),
                Arithmetic => write!(
                    f,
                    "Invalid types for arithmetic operation. Expected type {expected}, but found {found}"
                ),
                FunctionReturn => write!(
                    f,
                    "Function body returns the wrong type. Expected return type {expected}, but found {found}",
                ),
                Argument => write!(f, "Passed argument has the wrong type. Expected type {expected}, but found {found}"),
                InvalidOperatorUsage => write!(f, "Invalid operator usage. Expected type {expected} but found {found}"),
            },
            NonFunctionCall => write!(f, "Attempted to call a value that is not a function"),
            IncorrectArgumentCount {
                expected,
                found,
                original_location: _,
            } => {
                write!(f, "Incorrect number of arguments. Expected {expected}, but found {found}")
            }
        }
    }
}
