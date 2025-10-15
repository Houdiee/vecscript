use crate::token::{Token, *};

#[derive(Debug, Clone)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub token: Token,
}

#[derive(Debug, Clone)]
pub enum ParserErrorKind {
    UnexpectedToken { expected: Expected },
    InvalidUnaryOperator { operator: Operator },
    InvalidToken,
    UnexpectedEOF,
}

#[derive(Debug, Clone)]
pub enum Expected {
    ClosingDelimiter(Delimiter),
    OpeningDelimiter(Delimiter),
    Keyword(Keyword),
    VariableName,
    TypeAnnotation,
    Type,
    Assignment,
    Terminator,
    FunctionParameter,
    Identifier,
    Binding,
    ReturnType,
    Definition,
    Expression,
}
