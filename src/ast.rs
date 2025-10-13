use crate::{parser::ParserError, token::*};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Result<Statement, ParserError>>,
}

#[derive(Debug)]
pub enum Binding {
    Variable(VariableBinding),
    Function(FunctionBinding),
}

#[derive(Debug)]
pub struct VariableBinding {
    pub var_name: String,
    pub var_type: Option<Type>,
    pub expr: Expression,
}

#[derive(Debug)]
pub struct FunctionBinding {
    pub name: String,
    pub params: Vec<FunctionParameter>,
    pub return_type: Option<Type>,
    pub body: Expression,
}

#[derive(Debug)]
pub struct FunctionParameter {
    pub name: String,
    pub param_type: Option<Type>,
}

#[derive(Debug)]
pub enum Expression {
    Number(f64),
    String(String),
    Bool(bool),
    Identifier(String),
    BinaryOp(Box<Expression>, Operator, Box<Expression>),
    UnaryOp(Operator, Box<Expression>),
    FunctionCall {
        function_expr: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

pub type WhereClause = Vec<Binding>;

#[derive(Debug)]
pub enum Statement {
    LetDeclaration {
        binding: Binding,
        where_clause: Option<WhereClause>,
    },
    FunctionDeclaration {
        name: String,
        parameters: Vec<FunctionParameter>,
        return_type: Option<Type>,
        body: Box<Statement>,
    },
    Expression(Expression),
}
