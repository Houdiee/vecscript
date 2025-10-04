use crate::token::*;

pub struct Binding {
    pub var: String,
    pub var_type: Option<Primitive>,
    pub var_expression: Expression,
}

pub struct WhereClause {
    pub bindings: Vec<Binding>,
}

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

pub enum Statement {
    SolveForInDeclaration {
        var: String,
        var_type: Option<Primitive>,
        left: Expression,
        right: Expression,
    },

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
