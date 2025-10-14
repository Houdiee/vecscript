use crate::{parser::ParserError, token::*};

#[derive(Debug)]
pub struct Program {
    pub definitions: Vec<Result<Definition, ParserError>>,
}

#[derive(Debug)]
pub enum Definition {
    Let(LetDefinition),
}

#[derive(Debug)]
pub struct LetDefinition {
    pub binding: Binding,
}

#[derive(Debug)]
pub enum Binding {
    Variable(VariableBinding),
    Function(FunctionBinding),
}

#[derive(Debug)]
pub struct VariableBinding {
    pub name: String,
    pub var_type: TypeAnnotation,
    pub expr: Expression,
}
pub type TypeAnnotation = Option<Type>;
pub type VariableBindingList = Vec<VariableBinding>;

#[derive(Debug)]
pub struct FunctionBinding {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: TypeAnnotation,
    pub body: Expression,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: String,
    pub param_type: TypeAnnotation,
}
pub type ParameterList = Vec<Parameter>;

pub type WhereSuffix = Option<VariableBindingList>;
#[derive(Debug)]
pub enum Expression {
    Simple(SimpleExpression, WhereSuffix),
    LetIn {
        bindings: VariableBindingList,
        body: Box<Expression>,
    },
    IfElse {
        condition: Box<Expression>,
        true_branch: Box<Expression>,
        false_branch: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum SimpleExpression {
    Atom(Atom),
    BinaryOp(Box<Expression>, Operator, Box<Expression>),
    UnaryOp(Operator, Box<Expression>),
}

#[derive(Debug)]
pub enum Atom {
    Literal(Literal),
    Identifier(String),
    Parenthesized(Box<Expression>),
    Call(FunctionCall),
}

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
}

#[derive(Debug)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: ExpressionList,
}
pub type ExpressionList = Vec<Expression>;
