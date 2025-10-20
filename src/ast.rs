use crate::token::*;
use std::ops::Range;

#[derive(Debug)]
pub struct Program {
    pub definitions: Vec<Definition>,
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum Definition {
    Let(Binding),
}

#[derive(Debug, Clone)]
pub enum BindingKind {
    Variable(VariableBinding),
    Function(FunctionBinding),
}
pub type Binding = Spanned<BindingKind>;

#[derive(Debug, Clone)]
pub struct VariableBinding {
    pub name: Spanned<String>,
    pub var_type: TypeAnnotation,
    pub expr: Expression,
}
pub type TypeAnnotation = Option<Spanned<Type>>;
pub type VariableBindingList = Vec<VariableBinding>;

#[derive(Debug, Clone)]
pub struct FunctionBinding {
    pub name: Spanned<String>,
    pub params: Vec<Parameter>,
    pub return_type: TypeAnnotation,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Spanned<String>,
    pub param_type: TypeAnnotation,
}
pub type ParameterList = Vec<Parameter>;

#[derive(Debug, Clone)]
pub enum ExpressionKind {
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
    DoBlock {
        expressions: ExpressionList,
    },
}
pub type Expression = Spanned<ExpressionKind>;
pub type ExpressionList = Vec<Expression>;
pub type DoSequence = Vec<Expression>;

#[derive(Debug, Clone)]
pub enum SimpleExpressionKind {
    Atom(Atom),
    BinaryOp(Box<SimpleExpression>, Operator, Box<SimpleExpression>),
    UnaryOp(Operator, Box<SimpleExpression>),
}
pub type SimpleExpression = Spanned<SimpleExpressionKind>;
pub type WhereSuffix = VariableBindingList;

#[derive(Debug, Clone)]
pub enum AtomKind {
    Literal(Literal),
    Identifier(Spanned<String>),
    Parenthesized(Box<Expression>),
    FunctionCall(FunctionCall),
}
pub type Atom = Spanned<AtomKind>;

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: Spanned<String>,
    pub arguments: ExpressionList,
}
