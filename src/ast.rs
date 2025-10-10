use crate::token::*;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct Binding {
    pub var: String,
    pub var_type: Option<Primitive>,
    pub var_expression: Expression,
}

#[derive(Debug)]
pub struct WhereClause {
    pub bindings: Vec<Binding>,
}

#[derive(Debug)]
pub enum Expression {
    Expression(Box<Expression>),
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
        binding: Binding,
        bound_to: Box<Expression>,
    },

    LetDeclaration {
        binding: Binding,
        where_clause: Option<WhereClause>,
    },

    SetDeclaration {
        var: String,
        var_type: Option<Primitive>,
        elements: Vec<Expression>,
    },

    Expression(Expression),
}
