use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Token<'src> {
    pub kind: TokenKind<'src>,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum TokenKind<'src> {
    Identifier(&'src str),
    Keyword(Keyword),
    Delimiter(Delimiter),
    Type(Primitive),
    Operator(Operator),
    Number(f64),
    String(&'src str),
    Ellipsis,
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
    Solve,
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
