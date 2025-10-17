use crate::{parser::parser_error::*, report_error::ReportableError};
use ariadne::{Color, Label};
use std::fmt::{self, Display};

impl ReportableError for ParserError {
    fn span(&self) -> std::ops::Range<usize> {
        self.token.span.clone()
    }
    fn report_kind_message(&self) -> &'static str {
        "Parser Error"
    }
    fn primary_message(&self) -> String {
        format!("{}", self)
    }
    fn label_message(&self) -> String {
        format!("Found {}", self.token.kind)
    }
    fn custom_label<'a>(&self, file_name: &'a str, span: std::ops::Range<usize>) -> Option<Label<(&'a str, std::ops::Range<usize>)>> {
        match &self.kind {
            ParserErrorKind::UnexpectedToken { expected } => Some(
                Label::new((file_name, span))
                    .with_message(format!("Expected {}", expected))
                    .with_color(Color::Green)
                    .with_order(-1),
            ),
            _ => None,
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let token_kind = format!("{}", self.token.kind);

        use ParserErrorKind::*;
        match &self.kind {
            UnexpectedToken { expected } => {
                write!(f, "Expected {expected} but found {token_kind}")
            }
            InvalidUnaryOperator { operator } => {
                write!(f, "Invalid unary operator {operator}")
            }
            UnexpectedEOF => {
                write!(f, "Unexpected end of file")
            }
            InvalidToken => {
                write!(f, "Invalid token {token_kind}")
            }
        }
    }
}

impl Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expected::*;
        match self {
            ClosingDelimiter(delim) => write!(f, "closing delimiter '{}'", delim),
            OpeningDelimiter(delim) => write!(f, "opening delimiter '{}'", delim),
            Keyword(keyword) => write!(f, "keyword '{}'", keyword),
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
            Expression => write!(f, "expression"),
        }
    }
}
