use std::{fmt::Display, ops::Range};

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Identifier(String),
    Keyword(Keyword),
    Delimiter(Delimiter),
    Type(Type),
    Operator(Operator),
    Number(f64),
    Bool(bool),
    String(String),
    Assign,
    Newline,
    EOF,
}

#[derive(Debug, Clone)]
pub enum Type {
    BaseType(BaseType),
    Set(BaseType),
}

#[derive(Debug, Clone)]
pub enum BaseType {
    Num,
    Str,
    Bool,
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Let,
    Where,
    In,
    If,
    Then,
    Else,
    End,
    Type,
}

#[derive(Debug, Clone)]
pub enum Delimiter {
    Range,  // ..
    Arrow,  // ->
    Colon,  // :
    Comma,  // ,
    LParen, // (
    RParen, // )
    LBrack, // [
    RBrack, // ]
    LBrace, // {
    RBrace, // }
    Pipe,   // |
}

#[derive(Debug, Copy, Clone)]
pub enum Operator {
    Is,               // is
    Not,              // not
    And,              // and
    Or,               // or
    EqualLessThan,    // <=
    EqualGreaterThan, // >=
    LessThan,         // <
    GreaterThan,      // >
    Plus,             // +
    Minus,            // -
    Multiply,         // *
    Divide,           // /
    Power,            // ^
    Modulo,           // %
}

impl Display for BaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BaseType::Num => write!(f, "num"),
            BaseType::Str => write!(f, "str"),
            BaseType::Bool => write!(f, "bool"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::BaseType(b) => write!(f, "{}", b),
            // Example: set(num)
            Type::Set(b) => write!(f, "set({})", b),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Keyword::*;
        match self {
            Let => write!(f, "let"),
            Where => write!(f, "where"),
            In => write!(f, "in"),
            If => write!(f, "if"),
            Then => write!(f, "then"),
            Else => write!(f, "else"),
            End => write!(f, "end"),
            Type => write!(f, "type"),
        }
    }
}

impl Display for Delimiter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Delimiter::*;
        match self {
            Range => write!(f, ".."),
            Arrow => write!(f, "->"),
            Colon => write!(f, ":"),
            Comma => write!(f, ","),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrack => write!(f, "["),
            RBrack => write!(f, "]"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            Pipe => write!(f, "|"),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Operator::*;
        match self {
            Is => write!(f, "is"),
            Not => write!(f, "not"),
            And => write!(f, "and"),
            Or => write!(f, "or"),
            EqualLessThan => write!(f, "<="),
            EqualGreaterThan => write!(f, ">="),
            LessThan => write!(f, "<"),
            GreaterThan => write!(f, ">"),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Multiply => write!(f, "*"),
            Divide => write!(f, "/"),
            Power => write!(f, "^"),
            Modulo => write!(f, "%"),
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenKind::*;
        match self {
            Identifier(s) => write!(f, "identifier '{}'", s),
            Keyword(k) => write!(f, "keyword '{}'", k),
            Delimiter(d) => write!(f, "delimiter '{}'", d),
            Type(t) => write!(f, "type '{}'", t),
            Operator(o) => write!(f, "operator '{}'", o),
            Number(n) => write!(f, "number '{}'", n),
            Bool(b) => write!(f, "bool '{}'", b),
            String(s) => write!(f, "string '{}", s),
            Assign => write!(f, "="),
            Newline => write!(f, "newline"),
            EOF => write!(f, "EOF"),
        }
    }
}
