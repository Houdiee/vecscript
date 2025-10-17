/*
Program ::= { Definition }
Definition ::= LetDefinition TERMINATE
            | TypeDefinition TERMINATE
            ;

LetDefinition ::= LET Binding ;

Binding ::= VariableBinding | FunctionBinding ;

VariableBinding ::= IDENTIFIER [ TypeAnnotation ] ASSIGN [ NEWLINE ] Expression ;
VariableBindingList ::= VariableBinding { COMMA [ NEWLINE ] VariableBinding [ COMMA ] ;

FunctionBinding ::= IDENTIFIER LPAREN [ ParameterList ] RPAREN [ ReturnType ] ASSIGN [ NEWLINE ] Expression ;
Parameter ::= IDENTIFIER [ TypeAnnotation ] ;
ParameterList ::= Parameter { COMMA Parameter } ;

TypeAnnotation ::= COLON TYPE ;
ReturnType ::= ARROW TypeAnnotation ;

Expression ::= SimpleExpression [ WhereSuffix ]
             | LetInExpression
             | IfElseExpression
             ;

SimpleExpression ::= [ OPERATOR ] Atom { OPERATOR Atom } ;
WhereSuffix ::= [ NEWLINE ] WHERE [ NEWLINE ] VariableBindingList [ NEWLINE ] END ;
LetInExpression ::= LET VariableBindingList IN Expression ;
IfElseExpression ::= IF Expression [ NEWLINE ] THEN Expression [ NEWLINE ] ELSE Expression ;

Atom ::= NUMBER
       | STRING
       | BOOL
       | IDENTIFIER
       | LPAREN Expression RParen
       | FUNCTIONCALL
       ;

FUNCTIONCALL ::= IDENTIFIER LPAREN [ ExpressionList ] RPAREN ;
ExpressionList ::= Expression { COMMA Expression } ;
*/

// TODO add custom type support

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
        let next_token = self.peek().ok_or_else(|| self.unexpected_eof_error())?.to_owned();
        let definition = match next_token.kind {
            TokenKind::Keyword(Keyword::Let) => Definition::Let(self.parse_let_definition()?),
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
        self.parse_terminator()?;
        Ok(definition)
    }

    fn parse_let_definition(&mut self) -> Result<LetDefinition, ParserError> {
        self.expect(
            |kind| matches!(kind, TokenKind::Keyword(Keyword::Let)),
            Expected::Keyword(Keyword::Let),
        )?;
        let binding = self.parse_binding()?;
        Ok(LetDefinition { binding })
    }

    fn parse_binding(&mut self) -> Result<Binding, ParserError> {
        let peek_1th = self.peek_nth(1).ok_or_else(|| self.unexpected_eof_error())?;
        let binding = match peek_1th.kind {
            TokenKind::Delimiter(Delimiter::Colon) | TokenKind::Assign => Binding::Variable(self.parse_variable_binding()?),
            TokenKind::Delimiter(Delimiter::LParen) => Binding::Function(self.parse_function_binding()?),
            _ => {
                return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedToken {
                        expected: Expected::Binding,
                    },
                    token: peek_1th.clone(),
                });
            }
        };
        Ok(binding)
    }

    fn parse_variable_binding(&mut self) -> Result<VariableBinding, ParserError> {
        let name_token = self.expect(|kind| matches!(kind, TokenKind::Identifier(_)), Expected::VariableName)?;
        let name = match name_token.kind {
            TokenKind::Identifier(s) => s,
            _ => unreachable!(),
        };

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
        let name = match name_token.kind {
            TokenKind::Identifier(s) => s,
            _ => unreachable!(),
        };

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
        let name = match name_token.kind {
            TokenKind::Identifier(s) => s,
            _ => unreachable!(),
        };
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
        let type_t = match type_token.kind {
            TokenKind::Type(t) => t,
            _ => unreachable!(),
        };
        Ok(TypeAnnotation::Some(type_t))
    }

    fn parse_return_type(&mut self) -> Result<TypeAnnotation, ParserError> {
        if !matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::Arrow))) {
            return Ok(None);
        }
        self.expect(|kind| matches!(kind, TokenKind::Delimiter(Delimiter::Arrow)), Expected::ReturnType)?;
        let type_token = self.expect(|kind| matches!(kind, TokenKind::Type(_)), Expected::Type)?;
        let type_t = match type_token.kind {
            TokenKind::Type(t) => t,
            _ => unreachable!(),
        };
        Ok(TypeAnnotation::Some(type_t))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        let next_token = self.peek().ok_or_else(|| self.unexpected_eof_error())?;
        match next_token.kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_let_in_expression(),
            TokenKind::Keyword(Keyword::If) => self.parse_if_else_expression(),
            _ => {
                let simple_expr = Expression::Simple(self.parse_simple_expression()?);
                let where_suffix = self.parse_where_suffix()?;
                match where_suffix {
                    Some(bindings) => Ok(Expression::LetIn {
                        bindings,
                        body: Box::new(simple_expr),
                    }),
                    None => Ok(simple_expr),
                }
            }
        }
    }

    fn parse_let_in_expression(&mut self) -> Result<Expression, ParserError> {
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
        Ok(Expression::LetIn {
            bindings,
            body: Box::new(body),
        })
    }

    fn parse_if_else_expression(&mut self) -> Result<Expression, ParserError> {
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

        self.expect(
            |kind| matches!(kind, TokenKind::Keyword(Keyword::Else)),
            Expected::Keyword(Keyword::Else),
        )?;
        let false_branch = self.parse_expression()?;
        self.parse_optional_newline()?;

        Ok(Expression::IfElse {
            condition: Box::new(condition),
            true_branch: Box::new(true_branch),
            false_branch: Box::new(false_branch),
        })
    }

    fn parse_where_suffix(&mut self) -> Result<Option<VariableBindingList>, ParserError> {
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

    fn parse_simple_expression(&mut self) -> Result<SimpleExpression, ParserError> {
        self.parse_simple_expression_with_min_bp(0)
    }

    fn parse_simple_expression_with_min_bp(&mut self, min_bp: BindingPower) -> Result<SimpleExpression, ParserError> {
        let mut lhs = self.parse_prefix()?;

        while let Some(next_token) = self.peek() {
            let is_implicit_multiplication = || {
                matches!(lhs, SimpleExpression::Atom(Atom::Literal(Literal::Number(_))))
                    && matches!(next_token.kind, TokenKind::Identifier(_))
            };

            if is_implicit_multiplication() {
                let (lbp, rbp) = get_bp(Operator::Multiply);
                if lbp < min_bp {
                    break;
                }
                let rhs = self.parse_simple_expression_with_min_bp(rbp)?;
                lhs = SimpleExpression::BinaryOp(Box::new(lhs), Operator::Multiply, Box::new(rhs));
                continue;
            }

            let op = match &next_token.kind {
                TokenKind::Operator(op) => *op,
                _ => break,
            };

            let (lbp, rbp) = get_bp(op);

            if lbp < min_bp {
                break;
            }

            self.consume();

            let rhs = self.parse_simple_expression_with_min_bp(rbp)?;
            lhs = SimpleExpression::BinaryOp(Box::new(lhs), op, Box::new(rhs));
        }
        Ok(lhs)
    }

    fn parse_prefix(&mut self) -> Result<SimpleExpression, ParserError> {
        let next_token = self.peek().ok_or_else(|| self.unexpected_eof_error())?;
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
            return Ok(SimpleExpression::UnaryOp(op, Box::new(rhs)));
        }

        let atom = self.parse_atom()?;
        Ok(SimpleExpression::Atom(atom))
    }

    fn parse_atom(&mut self) -> Result<Atom, ParserError> {
        let token = self.peek().ok_or_else(|| self.unexpected_eof_error())?.to_owned();

        let atom = match token.kind {
            TokenKind::Identifier(ident) => {
                if matches!(self.peek_nth(1).map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::LParen))) {
                    return Ok(Atom::FunctionCall(self.parse_function_call()?));
                }
                self.consume();
                Atom::Identifier(ident.clone())
            }
            TokenKind::Number(num) => {
                self.consume();
                Atom::Literal(Literal::Number(num))
            }
            TokenKind::String(str) => {
                self.consume();
                Atom::Literal(Literal::String(str.clone()))
            }
            TokenKind::Bool(bool) => {
                self.consume();
                Atom::Literal(Literal::Bool(bool))
            }
            TokenKind::Delimiter(Delimiter::LParen) => {
                self.consume();
                let expr = self.parse_expression()?;
                self.expect(
                    |kind| matches!(kind, TokenKind::Delimiter(Delimiter::RParen)),
                    Expected::ClosingDelimiter(Delimiter::RParen),
                )?;
                Atom::Parenthesized(Box::new(expr))
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
        Ok(atom)
    }

    fn parse_function_call(&mut self) -> Result<FunctionCall, ParserError> {
        let name_token = self.expect(|kind| matches!(kind, TokenKind::Identifier(_)), Expected::Identifier)?;
        let name = match name_token.kind {
            TokenKind::Identifier(s) => s,
            _ => unreachable!(),
        };

        self.expect(
            |kind| matches!(kind, TokenKind::Delimiter(Delimiter::LParen)),
            Expected::OpeningDelimiter(Delimiter::LParen),
        )?;
        let arguments = self.parse_expression_list()?;
        self.expect(
            |kind| matches!(kind, TokenKind::Delimiter(Delimiter::RParen)),
            Expected::ClosingDelimiter(Delimiter::RParen),
        )?;

        Ok(FunctionCall { name, arguments })
    }

    fn parse_expression_list(&mut self) -> Result<ExpressionList, ParserError> {
        let mut expressions = ExpressionList::new();
        expressions.push(self.parse_expression()?);
        while matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::Comma))) {
            self.consume();
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
        Minus => Some(79),
        _ => None,
    }
}
