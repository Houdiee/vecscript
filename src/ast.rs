use crate::token::*;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct Binding {
    pub var: String,
    pub var_type: Option<Type>,
    pub var_expression: Expression,
}

pub type WhereClause = Vec<Binding>;
pub type InClause = Vec<Binding>;

#[derive(Debug)]
pub enum Expression {
    Number(f64),
    Bool(bool),
    String(String),
    Identifier(String),
    BinaryOp {
        left: Box<Expression>,
        op: Operator,
        right: Box<Expression>,
    },
    UnaryOp {
        op: Operator,
        expr: Box<Expression>,
    },
    SetLiteral(Vec<Expression>),
}

#[derive(Debug)]
pub enum Statement {
    LetInDeclaration {
        bound_to: Expression,
        bindings: InClause,
    },
    LetDeclaration {
        bound_to: Vec<Binding>,
        bindings: Option<WhereClause>,
    },
    Expression(Expression),
}
