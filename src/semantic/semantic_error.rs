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
    NonFunctionCall,
    IncorrectArgumentCount {
        expected: usize,
        found: usize,
        function_location: Range<usize>,
    },
}

#[derive(Debug, Clone)]
pub enum TypeMismatchKind {
    ThenElseReturn,
    TypeAnnotation,
    Arithmetic,
    FunctionReturn,
}

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub span: Range<usize>,
}
