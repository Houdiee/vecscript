use crate::{parser::ParserError, token::*};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Result<Statement, ParserError>>,
}

#[derive(Debug)]
pub enum Expression {
    Number(f64),
    String(String),
    Bool(bool),
    Identifier(String),
    BinaryOp(Box<Expression>, Operator, Box<Expression>),
    UnaryOp(Operator, Box<Expression>),
}

#[derive(Debug)]
pub enum Statement {
    LetDeclaration {
        var_name: String,
        var_type: Option<Type>,
        expr: Expression,
    },
    Expression(Expression),
}
