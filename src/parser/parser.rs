/*
Program ::= { Definition }

Definition ::= LetDefinition TERMINATE
             | TypeDefinition TERMINATE
             ;

LetDefinition ::= LET Binding ;
Binding ::= VariableBinding | FunctionBinding ;

FunctionBinding ::= IDENTIFIER LPAREN [ ParameterList ] RPAREN [ ReturnType ] ASSIGN [ NEWLINE ] Expression ;
VariableBinding ::= IDENTIFIER [ TypeAnnotation ] ASSIGN [ NEWLINE ] Expression ;
VariableBindingList ::= VariableBinding { COMMA [ NEWLINE ] VariableBinding [ COMMA ] ;

Parameter ::= IDENTIFIER [ TypeAnnotation ] ;
ParameterList ::= Parameter { COMMA Parameter } ;

TypeAnnotation ::= COLON TYPE ;
ReturnType ::= ARROW TypeAnnotation ;

Expression ::= SimpleExpression [ WhereSuffix ]
             | LetInExpression
             | IfElseExpression
             | DoBlock
             ;

SimpleExpression ::= [ OPERATOR ] Atom { OPERATOR Atom } ;
ExpressionList ::= Expression { COMMA [ NEWLINE ] Expression } ;

WhereSuffix ::= [ NEWLINE ] WHERE [ NEWLINE ] LBRACK VariableBindingList [ NEWLINE ] RBRACK ;
DoExpression ::= DO [ NEWLINE ] ExpressionList ;

LetInExpression ::= LET VariableBindingList IN Expression ;

IfElseExpression ::= IF Expression [ NEWLINE ] THEN Expression [ NEWLINE ] ELSE Expression ;

Atom ::= NUMBER
       | STRING
       | BOOL
       | NOTHING
       | IDENTIFIER
       | LPAREN Expression RParen
       | FUNCTIONCALL
       ;


FUNCTIONCALL ::= IDENTIFIER LPAREN [ ExpressionList ] RPAREN ;
*/

use crate::{
    ast::*,
    parser::parser_error::{Expected, ParserError, ParserErrorKind},
    token::*,
};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Iterator for Parser {
    type Item = Result<Definition, ParserError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.skip_newlines();
        self.peek()?;

        match self.parse_definition() {
            Ok(definition) => Some(Ok(definition)),
            Err(e) => {
                self.synchronize();
                Some(Err(e))
            }
        }
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, position: 0 }
    }

    pub fn parse(&mut self) -> (Program, Vec<ParserError>) {
        let mut definitions = Vec::new();
        let mut errors = Vec::new();

        for definition in self.by_ref() {
            match definition {
                Ok(value) => definitions.push(value),
                Err(err) => errors.push(err),
            }
        }
        let program = Program { definitions };
        (program, errors)
    }

    fn synchronize(&mut self) {
        while let Some(current) = self.peek() {
            if matches!(&current.kind, TokenKind::Keyword(Keyword::Let)) {
                return;
            }
            if matches!(&current.kind, TokenKind::Newline) {
                self.consume();
                return;
            }
            self.consume();
        }
    }

    fn parse_definition(&mut self) -> Result<Definition, ParserError> {
        self.skip_newlines();
        let next_token = self.peek().ok_or_else(|| self.unexpected_eof_error())?.to_owned();

        let binding = match next_token.kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_let_definition()?,
            _ => {
                self.consume();
                return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedToken {
                        expected: Expected::Definition,
                    },
                    token: next_token.clone(),
                });
            }
        };

        let definition = Definition::Let(binding);
        self.parse_terminator()?;
        Ok(definition)
    }

    fn parse_let_definition(&mut self) -> Result<Binding, ParserError> {
        let _let_token = self.expect(
            |kind| matches!(kind, TokenKind::Keyword(Keyword::Let)),
            Expected::Keyword(Keyword::Let),
        )?;
        self.parse_binding()
    }

    fn parse_binding(&mut self) -> Result<Binding, ParserError> {
        let name_token = self.peek().ok_or_else(|| self.unexpected_eof_error())?.to_owned();
        let start_span = name_token.span.start;

        let peek_1th = self.peek_nth(1).ok_or_else(|| self.unexpected_eof_error())?;

        let (binding_kind, end_span) = match peek_1th.kind {
            TokenKind::Delimiter(Delimiter::Colon) | TokenKind::Assign => {
                let vb = self.parse_variable_binding()?;
                let end = vb.expr.span.end;
                (BindingKind::Variable(vb), end)
            }
            TokenKind::Delimiter(Delimiter::LParen) => {
                let fb = self.parse_function_binding()?;
                let end = fb.body.span.end;
                (BindingKind::Function(fb), end)
            }
            _ => {
                return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedToken {
                        expected: Expected::Binding,
                    },
                    token: peek_1th.clone(),
                });
            }
        };

        Ok(Spanned {
            value: binding_kind,
            span: start_span..end_span,
        })
    }

    fn parse_variable_binding(&mut self) -> Result<VariableBinding, ParserError> {
        let name_token = self.expect(|kind| matches!(kind, TokenKind::Identifier(_)), Expected::VariableName)?;
        let name = name_token.into_spanned(|kind| match kind {
            TokenKind::Identifier(s) => s,
            _ => unreachable!(),
        });

        let var_type = self.parse_type_annotation()?;
        self.expect(|kind| matches!(kind, TokenKind::Assign), Expected::Assignment)?;
        self.parse_optional_newline()?;
        let expr = self.parse_expression()?;

        Ok(VariableBinding { name, var_type, expr })
    }

    fn parse_variable_binding_list(&mut self) -> Result<VariableBindingList, ParserError> {
        let mut bindings = VariableBindingList::new();
        bindings.push(self.parse_variable_binding()?);

        loop {
            if !matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::Comma))) {
                break;
            }
            self.consume();
            self.parse_optional_newline()?;
            if !matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Identifier(_))) {
                break;
            }
            bindings.push(self.parse_variable_binding()?);
        }

        Ok(bindings)
    }

    fn parse_function_binding(&mut self) -> Result<FunctionBinding, ParserError> {
        let name_token = self.expect(|kind| matches!(kind, TokenKind::Identifier(_)), Expected::VariableName)?;
        let name = name_token.into_spanned(|kind| match kind {
            TokenKind::Identifier(s) => s,
            _ => unreachable!(),
        });

        self.expect(
            |kind| matches!(kind, TokenKind::Delimiter(Delimiter::LParen)),
            Expected::OpeningDelimiter(Delimiter::LParen),
        )?;
        let params = self.parse_parameter_list()?;
        self.expect(
            |kind| matches!(kind, TokenKind::Delimiter(Delimiter::RParen)),
            Expected::OpeningDelimiter(Delimiter::RParen),
        )?;
        let return_type = self.parse_return_type()?;
        self.expect(|kind| matches!(kind, TokenKind::Assign), Expected::Assignment)?;
        self.parse_optional_newline()?;
        let body = self.parse_expression()?;

        Ok(FunctionBinding {
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_parameter_list(&mut self) -> Result<ParameterList, ParserError> {
        let mut params = ParameterList::new();
        if matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::RParen))) {
            return Ok(params);
        }

        params.push(self.parse_parameter()?);
        while matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::Comma))) {
            self.consume();
            params.push(self.parse_parameter()?);
        }
        Ok(params)
    }

    fn parse_parameter(&mut self) -> Result<Parameter, ParserError> {
        let name_token = self.expect(|kind| matches!(kind, TokenKind::Identifier(_)), Expected::VariableName)?;
        let name = name_token.into_spanned(|kind| match kind {
            TokenKind::Identifier(s) => s,
            _ => unreachable!(),
        });

        let param_type = self.parse_type_annotation()?;
        Ok(Parameter { name, param_type })
    }

    fn parse_type_annotation(&mut self) -> Result<TypeAnnotation, ParserError> {
        if !matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::Colon))) {
            return Ok(None);
        }

        self.expect(
            |kind| matches!(kind, TokenKind::Delimiter(Delimiter::Colon)),
            Expected::TypeAnnotation,
        )?;
        let type_token = self.expect(|kind| matches!(kind, TokenKind::Type(_)), Expected::Type)?;
        let r#type = type_token.into_spanned(|kind| match kind {
            TokenKind::Type(t) => t,
            _ => unreachable!(),
        });

        Ok(TypeAnnotation::Some(r#type))
    }

    fn parse_return_type(&mut self) -> Result<TypeAnnotation, ParserError> {
        if !matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::Arrow))) {
            return Ok(None);
        }
        self.expect(|kind| matches!(kind, TokenKind::Delimiter(Delimiter::Arrow)), Expected::ReturnType)?;
        let type_token = self.expect(|kind| matches!(kind, TokenKind::Type(_)), Expected::Type)?;
        let r#type = type_token.into_spanned(|kind| match kind {
            TokenKind::Type(t) => t,
            _ => unreachable!(),
        });

        Ok(TypeAnnotation::Some(r#type))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        let next_token = self.peek().ok_or_else(|| self.unexpected_eof_error())?;
        let start_span = next_token.span.start;

        let (kind, end_span) = match next_token.kind {
            TokenKind::Keyword(Keyword::Let) => {
                let expr = self.parse_let_in_expression_kind()?;
                let end = match &expr {
                    ExpressionKind::LetIn { body, .. } => body.span.end,
                    _ => unreachable!(),
                };
                (expr, end)
            }
            TokenKind::Keyword(Keyword::If) => {
                let expr = self.parse_if_else_expression_kind()?;
                let end = match &expr {
                    ExpressionKind::IfElse { false_branch, .. } => false_branch.span.end,
                    _ => unreachable!(),
                };
                (expr, end)
            }
            TokenKind::Keyword(Keyword::Do) => {
                let expr = self.parse_do_block()?;
                let end = match &expr {
                    ExpressionKind::DoExpression { expressions } => expressions.last().unwrap().span.end,
                    _ => unreachable!(),
                };
                (expr, end)
            }
            _ => {
                let simple_expr = self.parse_simple_expression()?;
                let where_suffix = self.parse_where_suffix()?;

                let simple_as_expression = Spanned {
                    value: ExpressionKind::Simple(simple_expr.clone()),
                    span: simple_expr.span.clone(),
                };

                let (kind, end) = match where_suffix {
                    Some(bindings) => {
                        let end = bindings.last().map(|b| b.expr.span.end).unwrap_or(simple_as_expression.span.end);
                        (
                            ExpressionKind::LetIn {
                                bindings,
                                body: Box::new(simple_as_expression),
                            },
                            end,
                        )
                    }
                    None => (simple_as_expression.value, simple_as_expression.span.end),
                };
                (kind, end)
            }
        };

        Ok(Spanned {
            value: kind,
            span: start_span..end_span,
        })
    }

    fn parse_let_in_expression_kind(&mut self) -> Result<ExpressionKind, ParserError> {
        self.expect(
            |kind| matches!(kind, TokenKind::Keyword(Keyword::Let)),
            Expected::Keyword(Keyword::Let),
        )?;
        let bindings = self.parse_variable_binding_list()?;
        self.expect(
            |kind| matches!(kind, TokenKind::Keyword(Keyword::In)),
            Expected::Keyword(Keyword::In),
        )?;
        self.parse_optional_newline()?;
        let body = self.parse_expression()?;

        Ok(ExpressionKind::LetIn {
            bindings,
            body: Box::new(body),
        })
    }

    fn parse_if_else_expression_kind(&mut self) -> Result<ExpressionKind, ParserError> {
        self.expect(
            |kind| matches!(kind, TokenKind::Keyword(Keyword::If)),
            Expected::Keyword(Keyword::If),
        )?;
        let condition = self.parse_expression()?;
        self.parse_optional_newline()?;

        self.expect(
            |kind| matches!(kind, TokenKind::Keyword(Keyword::Then)),
            Expected::Keyword(Keyword::Then),
        )?;
        let true_branch = self.parse_expression()?;
        self.parse_optional_newline()?;

        let _else_token = self.expect(
            |kind| matches!(kind, TokenKind::Keyword(Keyword::Else)),
            Expected::Keyword(Keyword::Else),
        )?;
        let false_branch = self.parse_expression()?;
        self.parse_optional_newline()?;

        Ok(ExpressionKind::IfElse {
            condition: Box::new(condition),
            true_branch: Box::new(true_branch),
            false_branch: Box::new(false_branch),
        })
    }

    fn parse_where_suffix(&mut self) -> Result<Option<WhereSuffix>, ParserError> {
        if matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Newline))
            && matches!(self.peek_nth(1).map(|t| &t.kind), Some(TokenKind::Keyword(Keyword::Where)))
        {
            self.parse_optional_newline()?;
        }
        if !matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Keyword(Keyword::Where))) {
            return Ok(None);
        }
        self.expect(
            |kind| matches!(kind, TokenKind::Keyword(Keyword::Where)),
            Expected::Keyword(Keyword::Where),
        )?;
        self.expect(
            |kind| matches!(kind, TokenKind::Delimiter(Delimiter::LBrack)),
            Expected::OpeningDelimiter(Delimiter::LBrack),
        )?;
        self.parse_optional_newline()?;
        let bindings = self.parse_variable_binding_list()?;
        self.parse_optional_newline()?;
        self.expect(
            |kind| matches!(kind, TokenKind::Delimiter(Delimiter::RBrack)),
            Expected::ClosingDelimiter(Delimiter::RBrack),
        )?;
        Ok(Some(bindings))
    }

    fn parse_do_block(&mut self) -> Result<ExpressionKind, ParserError> {
        self.expect(
            |kind| matches!(kind, TokenKind::Keyword(Keyword::Do)),
            Expected::Keyword(Keyword::Do),
        )?;
        self.parse_optional_newline()?;
        let expressions = self.parse_expression_list()?;
        Ok(ExpressionKind::DoExpression { expressions })
    }

    fn parse_simple_expression(&mut self) -> Result<SimpleExpression, ParserError> {
        self.parse_simple_expression_with_min_bp(0)
    }

    fn parse_simple_expression_with_min_bp(&mut self, min_bp: BindingPower) -> Result<SimpleExpression, ParserError> {
        let mut lhs = self.parse_prefix()?;

        while let Some(next_token) = self.peek() {
            let is_implicit_multiplication = || {
                matches!(&lhs.value, SimpleExpressionKind::Atom(atom) if matches!(&atom.value, AtomKind::Literal(Literal::Number(_))))
                    && matches!(next_token.kind, TokenKind::Identifier(_))
            };

            if is_implicit_multiplication() {
                let (lbp, rbp) = get_bp(Operator::Multiply);
                if lbp < min_bp {
                    break;
                }

                let op = Operator::Multiply;

                let rhs = self.parse_simple_expression_with_min_bp(rbp)?;

                let new_span = lhs.span.start..rhs.span.end;

                lhs = Spanned {
                    value: SimpleExpressionKind::BinaryOp(Box::new(lhs), op, Box::new(rhs)),
                    span: new_span,
                };
                continue;
            }

            let op_token_kind = match &next_token.kind {
                TokenKind::Operator(op) => *op,
                _ => break,
            };

            let (lbp, rbp) = get_bp(op_token_kind);

            if lbp < min_bp {
                break;
            }

            let _op_token = self.consume().unwrap();

            let rhs = self.parse_simple_expression_with_min_bp(rbp)?;

            let new_span = lhs.span.start..rhs.span.end;

            lhs = Spanned {
                value: SimpleExpressionKind::BinaryOp(Box::new(lhs), op_token_kind, Box::new(rhs)),
                span: new_span,
            };
        }
        Ok(lhs)
    }

    fn parse_prefix(&mut self) -> Result<SimpleExpression, ParserError> {
        let next_token = self.peek().ok_or_else(|| self.unexpected_eof_error())?.to_owned();
        let start_span = next_token.span.start;

        if let TokenKind::Operator(op) = next_token.kind {
            let rbp = match get_unary_bp(op) {
                Some(rbp) => rbp,
                None => {
                    return Err(ParserError {
                        kind: ParserErrorKind::InvalidUnaryOperator { operator: op },
                        token: next_token.clone(),
                    });
                }
            };

            self.consume();
            let rhs = self.parse_simple_expression_with_min_bp(rbp)?;

            let end_span = rhs.span.end;

            return Ok(Spanned {
                value: SimpleExpressionKind::UnaryOp(op, Box::new(rhs)),
                span: start_span..end_span,
            });
        }

        let atom = self.parse_atom()?;
        let span = atom.span.clone();

        Ok(Spanned {
            value: SimpleExpressionKind::Atom(atom),
            span,
        })
    }

    fn parse_atom(&mut self) -> Result<Atom, ParserError> {
        let token = self.peek().ok_or_else(|| self.unexpected_eof_error())?.to_owned();
        let start_span = token.span.start;

        let (kind, end_span) = match token.kind {
            TokenKind::Identifier(_) => {
                if matches!(self.peek_nth(1).map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::LParen))) {
                    let func_call = self.parse_function_call()?;
                    let end = func_call.span.end;

                    return Ok(Spanned {
                        value: AtomKind::FunctionCall(func_call.value),
                        span: start_span..end,
                    });
                }

                self.consume();
                let name_spanned = token.clone().into_spanned(|kind| match kind {
                    TokenKind::Identifier(s) => s,
                    _ => unreachable!(),
                });
                (AtomKind::Identifier(name_spanned), token.span.end)
            }
            TokenKind::Number(_) => {
                self.consume();
                let literal_value = match token.kind {
                    TokenKind::Number(num) => Literal::Number(num),
                    _ => unreachable!(),
                };
                (AtomKind::Literal(literal_value), token.span.end)
            }
            TokenKind::String(_) => {
                self.consume();
                let literal_value = match token.kind {
                    TokenKind::String(s) => Literal::String(s),
                    _ => unreachable!(),
                };
                (AtomKind::Literal(literal_value), token.span.end)
            }
            TokenKind::Bool(_) => {
                self.consume();
                let literal_value = match token.kind {
                    TokenKind::Bool(b) => Literal::Bool(b),
                    _ => unreachable!(),
                };
                (AtomKind::Literal(literal_value), token.span.end)
            }
            TokenKind::Type(Type::BaseType(BaseType::Nothing)) => {
                self.consume();
                (AtomKind::Literal(Literal::Nothing), token.span.end)
            }
            TokenKind::Delimiter(Delimiter::LParen) => {
                self.consume();
                let expr = self.parse_expression()?;
                let rparen_token = self.expect(
                    |kind| matches!(kind, TokenKind::Delimiter(Delimiter::RParen)),
                    Expected::ClosingDelimiter(Delimiter::RParen),
                )?;
                let end = rparen_token.span.end;
                (AtomKind::Parenthesized(Box::new(expr)), end)
            }
            _ => {
                return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedToken {
                        expected: Expected::Expression,
                    },
                    token,
                });
            }
        };

        Ok(Spanned {
            value: kind,
            span: start_span..end_span,
        })
    }

    fn parse_function_call(&mut self) -> Result<Spanned<FunctionCall>, ParserError> {
        let name_token = self.expect(|kind| matches!(kind, TokenKind::Identifier(_)), Expected::Identifier)?;
        let start_span = name_token.span.start;

        let name = name_token.into_spanned(|kind| match kind {
            TokenKind::Identifier(s) => s,
            _ => unreachable!(),
        });

        self.expect(
            |kind| matches!(kind, TokenKind::Delimiter(Delimiter::LParen)),
            Expected::OpeningDelimiter(Delimiter::LParen),
        )?;
        let arguments = self.parse_expression_list()?;
        let rparen_token = self.expect(
            |kind| matches!(kind, TokenKind::Delimiter(Delimiter::RParen)),
            Expected::ClosingDelimiter(Delimiter::RParen),
        )?;
        let end_span = rparen_token.span.end;

        Ok(Spanned {
            value: FunctionCall { name, arguments },
            span: start_span..end_span,
        })
    }

    fn parse_expression_list(&mut self) -> Result<ExpressionList, ParserError> {
        let mut expressions = ExpressionList::new();

        if matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::RParen))) {
            return Ok(expressions);
        }

        expressions.push(self.parse_expression()?);
        while matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::Comma))) {
            self.consume();
            self.parse_optional_newline()?;
            expressions.push(self.parse_expression()?);
        }
        Ok(expressions)
    }

    fn skip_newlines(&mut self) {
        while matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Newline)) {
            self.consume();
        }
    }

    fn parse_optional_newline(&mut self) -> Result<(), ParserError> {
        if matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Newline)) {
            self.consume();
        }
        Ok(())
    }

    fn parse_terminator(&mut self) -> Result<(), ParserError> {
        if self.peek().is_some() {
            self.expect(|kind| matches!(kind, TokenKind::Newline), Expected::Terminator)?;
        }
        Ok(())
    }

    fn expect(&mut self, predicate: fn(&TokenKind) -> bool, expected: Expected) -> Result<Token, ParserError> {
        let token = self.consume().ok_or_else(|| self.unexpected_eof_error())?;
        if !predicate(&token.kind) {
            return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken { expected },
                token,
            });
        }
        Ok(token)
    }

    fn peek_nth(&self, nth: usize) -> Option<&Token> {
        if self.position + nth >= self.tokens.len() {
            return None;
        }
        Some(&self.tokens[self.position + nth])
    }

    fn peek(&self) -> Option<&Token> {
        if self.position >= self.tokens.len() {
            return None;
        }
        Some(&self.tokens[self.position])
    }

    fn consume(&mut self) -> Option<Token> {
        if self.position >= self.tokens.len() {
            return None;
        }
        self.position += 1;
        Some(self.tokens[self.position - 1].clone())
    }

    fn unexpected_eof_error(&self) -> ParserError {
        let token = match self.tokens.last() {
            Some(tok) => tok.clone(),
            None => Token {
                kind: TokenKind::EOF,
                span: 0..0,
            },
        };
        ParserError {
            kind: ParserErrorKind::UnexpectedEOF,
            token,
        }
    }
}

type BindingPower = u8;
fn get_bp(op: Operator) -> (BindingPower, BindingPower) {
    use Operator::*;
    match op {
        Power => (100, 90),
        Multiply | Divide | Modulo => (80, 70),
        Plus | Minus => (60, 50),
        LessThan | EqualLessThan | GreaterThan | EqualGreaterThan => (40, 30),
        Is | Not => (20, 10),
        And | Or => (10, 0),
    }
}

fn get_unary_bp(op: Operator) -> Option<BindingPower> {
    use Operator::*;
    match op {
        Minus | Plus => Some(79),
        _ => None,
    }
}
