use crate::{ast::*, token::Type};
use std::collections::HashMap;

// --- Error Handling (Re-implemented for clean compilation) ---
// Assuming SemanticError is a unit struct for simplicity, as provided by the user.
#[derive(Debug)]
pub struct SemanticError;

// --- Symbol Table (Unchanged) ---
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
    // Assumes Type implements Clone for insertion from a borrowed context
    pub fn insert(&mut self, name: String, item_type: Type) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, item_type);
        }
    }
    pub fn lookup(&self, name: &str) -> Option<Type> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name).cloned())
    }
}
// --- End Symbol Table ---

// Main entry point for semantic analysis (Depth-First Search)
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
    // FIX: Match on the inner BindingKind
    match &binding.kind {
        BindingKind::Variable(vb) => {
            dfs_expression(&vb.expr, symbols)?;

            // Register the variable: use the explicit type if present, otherwise use Type::Unknown.
            let variable_type = match &vb.var_type {
                Some(t) => t.clone(),
                // ASSUMPTION: Type::Unknown exists in your token::Type definition.
                None => Type::Unknown,
            };
            symbols.insert(vb.name.clone(), variable_type);
        }
        BindingKind::Function(fb) => {
            // Register function name into the outer scope *before* analyzing the body (for recursion)
            let fn_type = match &fb.return_type {
                Some(t) => t.clone(),
                None => Type::Unknown,
            };
            symbols.insert(fb.name.clone(), fn_type);

            // Analyze parameters and body in a new scope
            symbols.enter_scope();
            for param in &fb.params {
                // Register parameters: use explicit type or Type::Unknown.
                let param_type = match &param.param_type {
                    Some(t) => t.clone(),
                    None => Type::Unknown,
                };
                symbols.insert(param.name.clone(), param_type);
            }
            dfs_expression(&fb.body, symbols)?;
            symbols.exit_scope();

            // Note: The function name was registered above, but the type will be verified later.
        }
    }
    Ok(())
}

pub fn dfs_expression(expression: &Expression, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    // FIX: Match on the inner ExpressionKind
    match &expression.kind {
        ExpressionKind::Simple(simple) => dfs_simple_expression(simple, symbols),
        ExpressionKind::LetIn { bindings, body } => {
            symbols.enter_scope();
            for vb in bindings {
                dfs_expression(&vb.expr, symbols)?;

                // Register the local variable: use explicit type or Type::Unknown.
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
            // (Type check: condition must be boolean)
            dfs_expression(true_branch.as_ref(), symbols)?;
            dfs_expression(false_branch.as_ref(), symbols)?;
            // (Type check: branches must have matching types)
            Ok(())
        }
    }
}

pub fn dfs_simple_expression(simple_expression: &SimpleExpression, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    // FIX: Match on the inner SimpleExpressionKind
    match &simple_expression.kind {
        SimpleExpressionKind::Atom(atom) => dfs_atom(atom, symbols),
        SimpleExpressionKind::BinaryOp(left, _, right) => {
            dfs_simple_expression(left.as_ref(), symbols)?;
            dfs_simple_expression(right.as_ref(), symbols)?;
            // (Type checking for binary operators here...)
            Ok(())
        }
        SimpleExpressionKind::UnaryOp(_, expr) => {
            dfs_simple_expression(expr.as_ref(), symbols)?;
            // (Type checking for unary operators here...)
            Ok(())
        }
    }
}

pub fn dfs_atom(atom: &Atom, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    // FIX: Match on the inner AtomKind
    match &atom.kind {
        AtomKind::Literal(_) => Ok(()),
        AtomKind::Identifier(name) => {
            if symbols.lookup(name).is_none() {
                // Error handling placeholder: Undefined variable
                return Err(SemanticError);
            }
            Ok(())
        }
        AtomKind::Parenthesized(expr) => dfs_expression(expr.as_ref(), symbols),
        AtomKind::FunctionCall(func_call) => {
            if symbols.lookup(&func_call.name).is_none() {
                // Error handling placeholder: Undefined function
                return Err(SemanticError);
            }
            for arg in &func_call.arguments {
                dfs_expression(arg, symbols)?;
            }
            // (Check argument count and types here...)
            Ok(())
        }
    }
}
