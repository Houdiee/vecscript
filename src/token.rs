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
    Type(Primitive),
    Operator(Operator),
    Number(f64),
    Bool(bool),
    String(String),
    Newline,
    EOF,
}

#[derive(Debug, Clone)]
pub enum Primitive {
    Num,
    Str,
    Bool,
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Let,
    Where,
    In,
}

#[derive(Debug, Clone)]
pub enum Delimiter {
    Colon,  // :
    Comma,  // ,
    LParen, // (
    RParen, // )
    LBrack, // [
    RBrack, // ]
    LBrace, // {
    RBrace, // }
}

#[derive(Debug, Clone)]
pub enum Operator {
    Is,               // is
    Not,              // not
    EqualLessThan,    // <=
    EqualGreaterThan, // >=
    LessThan,         // <
    GreaterThan,      // >
    Equals,           // =
    Plus,             // +
    Minus,            // -
    Multiply,         // *
    Divide,           // /
    Power,            // ^
    Modulo,           // %
}
