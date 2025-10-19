use crate::token::Type;
use std::ops::Range;

#[derive(Debug, Clone)]
pub enum SemanticErrorKind {
    UndefinedIdentifier { name: String },
    TypeMismatch { expected: Type, found: Type },
    NonBooleanCondition,
}

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub span: Range<usize>,
}
