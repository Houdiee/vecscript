use crate::{
    ast::*,
    semantic::{
        semantic_error::{SemanticError, SemanticErrorKind, TypeMismatchKind},
        symbol_table::{SymbolInfo, SymbolTable},
    },
    token::{BaseType, Operator, Type},
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
                let evaluated_type = self.dfs_expression(&vb.expr);
                let variable_type = match &vb.var_type {
                    TypeAnnotation::Some(annotation) => {
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
                        annotation.value.clone()
                    }
                    TypeAnnotation::None => evaluated_type,
                };

                self.insert_binding_into_symbol_table(&vb.name, variable_type);
            }

            BindingKind::Function(fb) => {
                let param_types: Vec<Type> = fb
                    .params
                    .iter()
                    .map(|param| match &param.param_type {
                        TypeAnnotation::Some(t) => t.value.clone(),
                        TypeAnnotation::None => Type::Unknown,
                    })
                    .collect();

                let mut return_type = match &fb.return_type {
                    TypeAnnotation::Some(t) => t.value.clone(),
                    TypeAnnotation::None => Type::Unknown,
                };

                let fn_type = Type::Function(param_types.clone(), Box::new(return_type.clone()));
                self.insert_binding_into_symbol_table(&fb.name, fn_type);

                self.symbol_table.enter_scope();
                for (param, param_type) in fb.params.iter().zip(param_types.clone()) {
                    self.insert_binding_into_symbol_table(&param.name, param_type);
                }
                let body_type = self.dfs_expression(&fb.body);
                if return_type == Type::Unknown {
                    return_type = body_type.clone();
                }
                self.symbol_table.exit_scope();

                if body_type != return_type {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::TypeMismatch {
                            kind: TypeMismatchKind::FunctionReturn,
                            expected: return_type.clone(),
                            found: body_type,
                        },
                        span: fb.body.span.clone(),
                    });
                }

                let updated_fn_type = Type::Function(param_types, Box::new(return_type));
                self.update_binding_into_symbol_table(&fb.name, updated_fn_type);
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
                            kind: TypeMismatchKind::ThenElseReturn {
                                then_location: true_branch.span.clone(),
                            },
                            expected: true_type.clone(),
                            found: false_type,
                        },
                        span: false_branch.span.clone(),
                    });
                }
                true_type
            }

            ExpressionKind::DoBlock { expressions } => {
                for expr in expressions {
                    self.dfs_expression(expr);
                }
                Type::BaseType(BaseType::Nothing)
            }
        }
    }

    fn dfs_simple_expression(&mut self, simple_expression: &SimpleExpression) -> Type {
        match &simple_expression.value {
            SimpleExpressionKind::Atom(atom) => self.dfs_atom(atom),
            SimpleExpressionKind::BinaryOp(left, op, right) => {
                let left_type = self.dfs_simple_expression(left.as_ref());
                let right_type = self.dfs_simple_expression(right.as_ref());

                if left_type == Type::Unknown || right_type == Type::Unknown {
                    return Type::Unknown;
                }

                match op {
                    Operator::Plus | Operator::Minus | Operator::Multiply | Operator::Divide | Operator::Modulo => {
                        if left_type != Type::BaseType(BaseType::Num) {
                            self.errors.push(SemanticError {
                                kind: SemanticErrorKind::TypeMismatch {
                                    kind: TypeMismatchKind::Arithmetic,
                                    expected: Type::BaseType(BaseType::Num),
                                    found: left_type.clone(),
                                },
                                span: left.span.clone(),
                            });
                        }

                        if right_type != Type::BaseType(BaseType::Num) {
                            self.errors.push(SemanticError {
                                kind: SemanticErrorKind::TypeMismatch {
                                    kind: TypeMismatchKind::Arithmetic,
                                    expected: Type::BaseType(BaseType::Num),
                                    found: right_type.clone(),
                                },
                                span: right.span.clone(),
                            });
                        }
                        if left_type != Type::BaseType(BaseType::Num) || right_type != Type::BaseType(BaseType::Num) {
                            return Type::Unknown;
                        }

                        Type::BaseType(BaseType::Num)
                    }

                    Operator::Power => {
                        if left_type != Type::BaseType(BaseType::Num) || right_type != Type::BaseType(BaseType::Num) {
                            self.errors.push(SemanticError {
                                kind: SemanticErrorKind::TypeMismatch {
                                    kind: TypeMismatchKind::InvalidOperatorUsage,
                                    expected: Type::BaseType(BaseType::Num),
                                    found: left_type.clone(),
                                },
                                span: simple_expression.span.clone(),
                            });
                            return Type::Unknown;
                        }
                        Type::BaseType(BaseType::Num)
                    }

                    Operator::Is | Operator::Not => {
                        if left_type != right_type {
                            self.errors.push(SemanticError {
                                kind: SemanticErrorKind::TypeMismatch {
                                    kind: TypeMismatchKind::InvalidOperatorUsage,
                                    expected: left_type.clone(),
                                    found: right_type,
                                },
                                span: right.span.clone(),
                            });
                            return Type::Unknown;
                        }
                        Type::BaseType(BaseType::Bool)
                    }

                    Operator::And | Operator::Or => {
                        let required = Type::BaseType(BaseType::Bool);
                        let mut is_error = false;
                        if left_type != required {
                            self.errors.push(SemanticError {
                                kind: SemanticErrorKind::TypeMismatch {
                                    kind: TypeMismatchKind::InvalidOperatorUsage,
                                    expected: required.clone(),
                                    found: left_type,
                                },
                                span: left.span.clone(),
                            });
                            is_error = true;
                        }

                        if right_type != required {
                            self.errors.push(SemanticError {
                                kind: SemanticErrorKind::TypeMismatch {
                                    kind: TypeMismatchKind::InvalidOperatorUsage,
                                    expected: required.clone(),
                                    found: right_type,
                                },
                                span: right.span.clone(),
                            });
                            is_error = true;
                        }

                        if is_error { Type::Unknown } else { required }
                    }
                    _ => Type::Unknown,
                }
            }
            SimpleExpressionKind::UnaryOp(op, expr) => {
                let expr_type = self.dfs_simple_expression(expr.as_ref());

                match op {
                    Operator::Minus | Operator::Plus => {
                        if expr_type != Type::BaseType(BaseType::Num) {
                            self.errors.push(SemanticError {
                                kind: SemanticErrorKind::TypeMismatch {
                                    kind: TypeMismatchKind::InvalidOperatorUsage,
                                    expected: Type::BaseType(BaseType::Num),
                                    found: expr_type,
                                },
                                span: expr.span.clone(),
                            });
                            return Type::Unknown;
                        }
                        Type::BaseType(BaseType::Num)
                    }

                    Operator::Not => {
                        if expr_type != Type::BaseType(BaseType::Bool) {
                            self.errors.push(SemanticError {
                                kind: SemanticErrorKind::TypeMismatch {
                                    kind: TypeMismatchKind::InvalidOperatorUsage,
                                    expected: Type::BaseType(BaseType::Bool),
                                    found: expr_type,
                                },
                                span: expr.span.clone(),
                            });
                            return Type::Unknown;
                        }
                        Type::BaseType(BaseType::Bool)
                    }

                    _ => {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::TypeMismatch {
                                kind: TypeMismatchKind::InvalidOperatorUsage,
                                expected: Type::BaseType(BaseType::Bool),
                                found: expr_type,
                            },
                            span: expr.span.clone(),
                        });
                        Type::Unknown
                    }
                }
            }
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
                let function_info = self.symbol_table.lookup(&func_call.name.value).cloned();

                match function_info {
                    Some(info) => match &info.symbol_type {
                        Type::Function(param_types, return_type) => {
                            if param_types.len() != func_call.arguments.len() {
                                self.errors.push(SemanticError {
                                    kind: SemanticErrorKind::IncorrectArgumentCount {
                                        expected: param_types.len(),
                                        found: func_call.arguments.len(),
                                        original_location: info.declaration_span,
                                    },
                                    span: atom.span.clone(),
                                });
                            }

                            for (idx, arg_expr) in func_call.arguments.iter().enumerate() {
                                let arg_type = self.dfs_expression(&arg_expr);
                                if idx < param_types.len() && arg_type != param_types[idx] {
                                    self.errors.push(SemanticError {
                                        kind: SemanticErrorKind::TypeMismatch {
                                            kind: TypeMismatchKind::Argument,
                                            expected: param_types[idx].clone(),
                                            found: arg_type,
                                        },
                                        span: arg_expr.span.clone(),
                                    });
                                }
                            }

                            *return_type.clone()
                        }

                        _ => {
                            self.errors.push(SemanticError {
                                kind: SemanticErrorKind::NonFunctionCall,
                                span: func_call.name.span.clone(),
                            });
                            Type::Unknown
                        }
                    },
                    None => {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::UndefinedIdentifier {
                                name: func_call.name.value.clone(),
                            },
                            span: func_call.name.span.clone(),
                        });
                        Type::Unknown
                    }
                }
            }
        }
    }

    fn insert_binding_into_symbol_table(&mut self, spanned_name: &Spanned<String>, symbol_type: Type) {
        let name = &spanned_name.value;
        let span = &spanned_name.span;

        if let Some(symbol_info) = self.symbol_table.lookup_in_current_scope(name) {
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

    fn update_binding_into_symbol_table(&mut self, spanned_name: &Spanned<String>, symbol_type: Type) {
        let name = &spanned_name.value;
        let span = &spanned_name.span;

        self.symbol_table.insert(
            name.clone(),
            SymbolInfo {
                symbol_type,
                declaration_span: span.clone(),
            },
        );
    }
}
