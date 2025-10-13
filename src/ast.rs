use crate::{parser::ParserError, token::*};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Result<Statement, ParserError>>,
}

#[derive(Debug)]
pub struct Binding {
    pub var_name: String,
    pub var_type: Option<Type>,
    pub expr: Expression,
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

pub type WhereClause = Vec<Binding>;

#[derive(Debug)]
pub enum Statement {
    LetDeclaration {
        binding: Binding,
        where_clause: Option<WhereClause>,
    },
    Expression(Expression),
}
