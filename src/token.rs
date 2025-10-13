use std::ops::Range;

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

    // Type constructors
    Set,
}

#[derive(Debug, Clone)]
pub enum Delimiter {
    Range,       // ..
    DoubleColon, // ::
    Colon,       // :
    Comma,       // ,
    LParen,      // (
    RParen,      // )
    LBrack,      // [
    RBrack,      // ]
    LBrace,      // {
    RBrace,      // }
    Pipe,        // |
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
