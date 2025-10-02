use std::ops::Range;

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Range<usize>,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum TokenKind {
    Identifier,
    Keyword(Keyword),
    Delimiter(Delimiter),
    Type(Primitive),
    Operator(Operator),
    Number(f64),
    String,
    Ellipsis,
    Newline,
    EOF,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Primitive {
    Num,
    Str,
    Bool,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Keyword {
    Let,
    Where,
    Solve,
    In,
}

#[allow(unused)]
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

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Operator {
    IsNot,            // is not
    Is,               // is
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
