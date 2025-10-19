use crate::{
    ast::*,
    semantic::{
        semantic_error::{SemanticError, SemanticErrorKind, TypeMismatchKind},
        symbol_table::{SymbolInfo, SymbolTable},
    },
    token::{BaseType, Type},
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
            symbol_table: SymbolTable::default(),
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
        match &binding.value {
            BindingKind::Variable(vb) => {
                self.dfs_expression(&vb.expr);
                let variable_type = match &vb.var_type {
                    TypeAnnotation::Some(t) => t.value.clone(),
                    TypeAnnotation::None => Type::Unknown,
                };
                self.insert_binding_into_symbol_table(&vb.name, variable_type);
            }
            BindingKind::Function(fb) => {
                let fn_type = match &fb.return_type {
                    TypeAnnotation::Some(t) => t.value.clone(),
                    TypeAnnotation::None => Type::Unknown,
                };
                self.insert_binding_into_symbol_table(&fb.name, fn_type.clone());

                self.symbol_table.enter_scope();
                for param in &fb.params {
                    let param_type = match &param.param_type {
                        TypeAnnotation::Some(t) => t.value.clone(),
                        TypeAnnotation::None => Type::Unknown,
                    };
                    self.insert_binding_into_symbol_table(&param.name, param_type);
                }

                self.dfs_expression(&fb.body);
                self.symbol_table.exit_scope();
            }
        }
    }

    fn dfs_expression(&mut self, expression: &Expression) -> Type {
        match &expression.value {
            ExpressionKind::Simple(simple) => self.dfs_simple_expression(simple),

            ExpressionKind::LetIn { bindings, body } => {
                self.symbol_table.enter_scope();
                for vb in bindings {
                    let evaluated_type = self.dfs_expression(&vb.expr);
                    let annotated_type = match &vb.var_type {
                        TypeAnnotation::Some(t) => t.value.clone(),
                        TypeAnnotation::None => evaluated_type.clone(),
                    };

                    if let TypeAnnotation::Some(annotation) = &vb.var_type {
                        if annotation.value != evaluated_type {
                            self.errors.push(SemanticError {
                                kind: SemanticErrorKind::TypeMismatch {
                                    kind: TypeMismatchKind::TypeAnnotation,
                                    expected: annotation.value.clone(),
                                    found: evaluated_type,
                                },
                                span: vb.expr.span.clone(),
                            });
                        }
                    }
                    self.insert_binding_into_symbol_table(&vb.name, annotated_type);
                }
                let body = self.dfs_expression(body.as_ref());
                self.symbol_table.exit_scope();
                body
            }
            ExpressionKind::IfElse {
                condition,
                true_branch,
                false_branch,
            } => {
                let condition_type = self.dfs_expression(condition);
                if condition_type != Type::BaseType(BaseType::Bool) {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::NonBooleanCondition,
                        span: condition.span.clone(),
                    });
                }

                let true_type = self.dfs_expression(true_branch.as_ref());
                let false_type = self.dfs_expression(false_branch.as_ref());
                if true_type != false_type {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::TypeMismatch {
                            kind: TypeMismatchKind::IfElseReturn,
                            expected: true_type.clone(),
                            found: false_type,
                        },
                        span: false_branch.span.clone(),
                    });
                }
                true_type
            }
        }
    }

    fn dfs_simple_expression(&mut self, simple_expression: &SimpleExpression) -> Type {
        match &simple_expression.value {
            SimpleExpressionKind::Atom(atom) => self.dfs_atom(atom),
            SimpleExpressionKind::BinaryOp(left, _, right) => {
                let left_type = self.dfs_simple_expression(left.as_ref());
                let right_type = self.dfs_simple_expression(right.as_ref());
                if left_type != right_type {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::TypeMismatch {
                            kind: TypeMismatchKind::IfElseReturn,
                            expected: left_type,
                            found: right_type.clone(),
                        },
                        span: right.span.clone(),
                    });
                }
                right_type
            }
            SimpleExpressionKind::UnaryOp(_, expr) => self.dfs_simple_expression(expr.as_ref()),
        }
    }

    fn dfs_atom(&mut self, atom: &Atom) -> Type {
        match &atom.value {
            AtomKind::Literal(literal) => match literal {
                Literal::Number(_) => Type::BaseType(BaseType::Num),
                Literal::String(_) => Type::BaseType(BaseType::Str),
                Literal::Bool(_) => Type::BaseType(BaseType::Bool),
            },

            AtomKind::Identifier(name) => match self.symbol_table.lookup(&name.value) {
                Some(info) => info.symbol_type.clone(),
                None => {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::UndefinedIdentifier { name: name.value.clone() },
                        span: name.span.clone(),
                    });
                    Type::Unknown
                }
            },

            AtomKind::Parenthesized(expr) => self.dfs_expression(expr.as_ref()),

            AtomKind::FunctionCall(func_call) => {
                let function_info = self.symbol_table.lookup(&func_call.name.value);
                if function_info.is_none() {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::UndefinedIdentifier {
                            name: func_call.name.value.clone(),
                        },
                        span: func_call.name.span.clone(),
                    });
                }
                for arg in &func_call.arguments {
                    self.dfs_expression(arg);
                }
                todo!()
            }
        }
    }

    fn insert_binding_into_symbol_table(&mut self, spanned_name: &Spanned<String>, symbol_type: Type) {
        let name = &spanned_name.value;
        let span = &spanned_name.span;

        if let Some(symbol_info) = self.symbol_table.lookup(name) {
            self.errors.push(SemanticError {
                kind: SemanticErrorKind::IdentifierAlreadyDeclared {
                    name: name.clone(),
                    original_location: symbol_info.declaration_span.clone(),
                },
                span: span.clone(),
            });
        }

        self.symbol_table.insert(
            name.clone(),
            SymbolInfo {
                symbol_type,
                declaration_span: span.clone(),
            },
        );
    }
}
