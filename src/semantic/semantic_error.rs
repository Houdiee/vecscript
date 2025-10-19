use crate::token::Type;
use std::ops::Range;

#[derive(Debug, Clone)]
pub enum SemanticErrorKind {
    UndefinedIdentifier {
        name: String,
    },
    IdentifierAlreadyDeclared {
        name: String,
        original_location: Range<usize>,
    },
    TypeMismatch {
        kind: TypeMismatchKind,
        expected: Type,
        found: Type,
    },
    NonBooleanCondition,
}

#[derive(Debug, Clone)]
pub enum TypeMismatchKind {
    IfElseReturn,
    TypeAnnotation,
    Arithmetic,
}

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub span: Range<usize>,
}
