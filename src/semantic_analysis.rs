use crate::{ast::*, token::Type};
use std::{collections::HashMap, ops::Range};

#[derive(Debug, Clone)]
pub enum SemanticErrorKind {
    UndefinedIdentifier { name: String },
    TypeMismatch { expected: Type, found: Type },
    NonBooleanCondition,
}

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub span: Range<usize>,
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Type>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: String, item_type: Type) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, item_type);
        }
    }
    pub fn lookup(&self, name: &str) -> Option<Type> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name).cloned())
    }
}

pub fn dfs_program(program: &Program) -> Result<(), SemanticError> {
    let mut symbols = SymbolTable::new();
    dfs_definitions(&program.definitions, &mut symbols)
}

pub fn dfs_definitions(definitions: &Vec<Definition>, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    for def in definitions {
        match def {
            Definition::Let(binding) => dfs_binding(binding, symbols)?,
        }
    }
    Ok(())
}

pub fn dfs_binding(binding: &Binding, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    match &binding.kind {
        BindingKind::Variable(vb) => {
            dfs_expression(&vb.expr, symbols)?;
            let variable_type = match &vb.var_type {
                Some(t) => t.clone(),
                None => Type::Unknown,
            };
            symbols.insert(vb.name.clone(), variable_type);
        }
        BindingKind::Function(fb) => {
            let fn_type = match &fb.return_type {
                Some(t) => t.clone(),
                None => Type::Unknown,
            };
            symbols.insert(fb.name.clone(), fn_type);

            symbols.enter_scope();
            for param in &fb.params {
                let param_type = match &param.param_type {
                    Some(t) => t.clone(),
                    None => Type::Unknown,
                };
                symbols.insert(param.name.clone(), param_type);
            }
            dfs_expression(&fb.body, symbols)?;
            symbols.exit_scope();
        }
    }
    Ok(())
}

pub fn dfs_expression(expression: &Expression, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    match &expression.kind {
        ExpressionKind::Simple(simple) => dfs_simple_expression(simple, symbols),
        ExpressionKind::LetIn { bindings, body } => {
            symbols.enter_scope();
            for vb in bindings {
                dfs_expression(&vb.expr, symbols)?;
                let variable_type = match &vb.var_type {
                    Some(t) => t.clone(),
                    None => Type::Unknown,
                };
                symbols.insert(vb.name.clone(), variable_type);
            }
            dfs_expression(body.as_ref(), symbols)?;
            symbols.exit_scope();
            Ok(())
        }
        ExpressionKind::IfElse {
            condition,
            true_branch,
            false_branch,
        } => {
            dfs_expression(condition.as_ref(), symbols)?;
            dfs_expression(true_branch.as_ref(), symbols)?;
            dfs_expression(false_branch.as_ref(), symbols)?;
            Ok(())
        }
    }
}

pub fn dfs_simple_expression(simple_expression: &SimpleExpression, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    match &simple_expression.kind {
        SimpleExpressionKind::Atom(atom) => dfs_atom(atom, symbols),
        SimpleExpressionKind::BinaryOp(left, _, right) => {
            dfs_simple_expression(left.as_ref(), symbols)?;
            dfs_simple_expression(right.as_ref(), symbols)?;
            Ok(())
        }
        SimpleExpressionKind::UnaryOp(_, expr) => {
            dfs_simple_expression(expr.as_ref(), symbols)?;
            Ok(())
        }
    }
}

pub fn dfs_atom(atom: &Atom, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    match &atom.kind {
        AtomKind::Literal(_) => Ok(()),
        AtomKind::Identifier(name) => {
            if symbols.lookup(name).is_none() {
                return Err(SemanticError {
                    kind: SemanticErrorKind::UndefinedIdentifier { name: name.clone() },
                    span: atom.span.clone(),
                });
            }
            Ok(())
        }
        AtomKind::Parenthesized(expr) => dfs_expression(expr.as_ref(), symbols),
        AtomKind::FunctionCall(func_call) => {
            if symbols.lookup(&func_call.name).is_none() {
                return Err(SemanticError {
                    kind: SemanticErrorKind::UndefinedIdentifier {
                        name: func_call.name.clone(),
                    },
                    span: atom.span.clone(),
                });
            }
            for arg in &func_call.arguments {
                dfs_expression(arg, symbols)?;
            }
            Ok(())
        }
    }
}
