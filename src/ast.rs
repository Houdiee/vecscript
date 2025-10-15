use crate::token::*;

#[derive(Debug)]
pub struct Program {
    pub definitions: Vec<Definition>,
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

#[derive(Debug)]
pub enum Expression {
    Simple(SimpleExpression),
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
    BinaryOp(Box<SimpleExpression>, Operator, Box<SimpleExpression>),
    UnaryOp(Operator, Box<SimpleExpression>),
}

#[derive(Debug)]
pub enum Atom {
    Literal(Literal),
    Identifier(String),
    Parenthesized(Box<Expression>),
    FunctionCall(FunctionCall),
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
