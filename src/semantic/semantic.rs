use crate::{
    ast::*,
    semantic::{
        semantic_error::{SemanticError, SemanticErrorKind},
        symbol_table::SymbolTable,
    },
    token::Type,
};

pub struct SemanticAnalyzer {
    program: Program,
    symbol_table: SymbolTable,
    errors: Vec<SemanticError>,
}

impl SemanticAnalyzer {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    pub fn analyze(mut self) -> Result<Program, Vec<SemanticError>> {
        self.dfs_program();

        if self.errors.is_empty() {
            Ok(self.program)
        } else {
            Err(self.errors)
        }
    }

    fn dfs_program(&mut self) {
        self.dfs_definitions(&self.program.definitions.clone())
    }

    fn dfs_definitions(&mut self, definitions: &Vec<Definition>) {
        for def in definitions {
            match def {
                Definition::Let(binding) => self.dfs_binding(binding),
            }
        }
    }

    fn dfs_binding(&mut self, binding: &Binding) {
        match &binding.kind {
            BindingKind::Variable(vb) => {
                self.dfs_expression(&vb.expr);
                let variable_type = match &vb.var_type {
                    Some(t) => t.clone(),
                    None => Type::Unknown,
                };
                self.symbol_table.insert(vb.name.clone(), variable_type);
            }
            BindingKind::Function(fb) => {
                let fn_type = match &fb.return_type {
                    Some(t) => t.clone(),
                    None => Type::Unknown,
                };
                self.symbol_table.insert(fb.name.clone(), fn_type);

                self.symbol_table.enter_scope();
                for param in &fb.params {
                    let param_type = match &param.param_type {
                        Some(t) => t.clone(),
                        None => Type::Unknown,
                    };
                    self.symbol_table.insert(param.name.clone(), param_type);
                }
                self.dfs_expression(&fb.body);
                self.symbol_table.exit_scope();
            }
        }
    }

    fn dfs_expression(&mut self, expression: &Expression) {
        match &expression.kind {
            ExpressionKind::Simple(simple) => self.dfs_simple_expression(simple),
            ExpressionKind::LetIn { bindings, body } => {
                self.symbol_table.enter_scope();
                for vb in bindings {
                    self.dfs_expression(&vb.expr);
                    let variable_type = match &vb.var_type {
                        Some(t) => t.clone(),
                        None => Type::Unknown,
                    };
                    self.symbol_table.insert(vb.name.clone(), variable_type);
                }
                self.dfs_expression(body.as_ref());
                self.symbol_table.exit_scope();
            }
            ExpressionKind::IfElse {
                condition,
                true_branch,
                false_branch,
            } => {
                self.dfs_expression(condition.as_ref());
                self.dfs_expression(true_branch.as_ref());
                self.dfs_expression(false_branch.as_ref());
            }
        }
    }

    fn dfs_simple_expression(&mut self, simple_expression: &SimpleExpression) {
        match &simple_expression.kind {
            SimpleExpressionKind::Atom(atom) => self.dfs_atom(atom),
            SimpleExpressionKind::BinaryOp(left, _, right) => {
                self.dfs_simple_expression(left.as_ref());
                self.dfs_simple_expression(right.as_ref());
            }
            SimpleExpressionKind::UnaryOp(_, expr) => {
                self.dfs_simple_expression(expr.as_ref());
            }
        }
    }

    fn dfs_atom(&mut self, atom: &Atom) {
        match &atom.kind {
            AtomKind::Literal(_) => {}
            AtomKind::Identifier(name) => {
                if self.symbol_table.lookup(name).is_none() {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::UndefinedIdentifier { name: name.clone() },
                        span: atom.span.clone(),
                    });
                }
            }
            AtomKind::Parenthesized(expr) => self.dfs_expression(expr.as_ref()),
            AtomKind::FunctionCall(func_call) => {
                if self.symbol_table.lookup(&func_call.name).is_none() {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::UndefinedIdentifier {
                            name: func_call.name.clone(),
                        },
                        span: atom.span.clone(),
                    });
                }
                for arg in &func_call.arguments {
                    self.dfs_expression(arg);
                }
            }
        }
    }
}
