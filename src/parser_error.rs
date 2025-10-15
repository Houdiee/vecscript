use crate::token::{Token, *};
use std::fmt::{self, Display};

#[derive(Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub token: Token,
}

#[derive(Debug)]
pub enum ParserErrorKind {
    UnexpectedToken { expected: Expected },
    UnexpectedEOF,
    InvalidExpression,
    InvalidUnaryOperator,
    InvalidToken,
}

#[derive(Debug)]
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
}

impl Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expected::*;
        match self {
            ClosingDelimiter(delim) => write!(f, "closing delimiter '{:?}'", delim),
            OpeningDelimiter(delim) => write!(f, "opening delimiter '{:?}'", delim),
            Keyword(keyword) => write!(f, "keyword '{:?}'", keyword),
            VariableName => write!(f, "a variable name"),
            TypeAnnotation => write!(f, "a type annotation (starting with ':')"),
            Type => write!(f, "a type identifier (e.g., 'Num', 'Str')"),
            Assignment => write!(f, "assignment operator '='"),
            Terminator => write!(f, "a terminator (newline)"),
            FunctionParameter => write!(f, "a function parameter"),
            Identifier => write!(f, "an identifier"),
            Binding => write!(f, "a variable or function binding"),
            ReturnType => write!(f, "a return type (starting with '->')"),
            Definition => write!(f, "a definition (starting with 'let')"),
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let token_kind_str = format!("{:?}", self.token.kind);

        match &self.kind {
            ParserErrorKind::UnexpectedToken { expected } => {
                write!(f, "Syntax Error: Expected {} but found {}", expected, token_kind_str)
            }
            ParserErrorKind::UnexpectedEOF => {
                write!(f, "Syntax Error: Unexpected End of File")
            }
            ParserErrorKind::InvalidExpression => {
                write!(f, "Syntax Error: Invalid expression starting with {}", token_kind_str,)
            }
            _ => write!(f, "Parser Error: {:?}", self.kind),
        }
    }
}
