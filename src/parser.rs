/*
Program ::= { Statement }
Statement ::= LetDeclaration
            | Expression
            ;
LetStatement ::= LetDeclaration ;
LetDeclaration::= LET IDENTIFIER [ TypeAnnotation ] ASSIGN Expression ;
TypeAnnotation ::= COLON TYPE ;

Expression ::= Atom [ OPERATOR Atom ]
Atom ::= NUMBER
       | STRING
       | BOOL
       | IDENTIFIER
       | LPAREN Expression RParen
       ;
*/

use crate::{ast::*, token::*};

#[derive(Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub token: Token,
}

#[derive(Debug)]
pub enum ParserErrorKind {
    UnexpectedToken { expected: Expected },
    UnexpectedEOF,
    InvalidExpression,
    InvalidUnaryOperator,
    InvalidToken,
}

#[derive(Debug)]
pub enum Expected {
    ClosingDelimiter(Delimiter),
    KeywordLet,
    VariableDeclaration,
    TypeAnnotation,
    Type,
    Assignment,
    Terminator,
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Iterator for Parser {
    type Item = Result<Statement, ParserError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.skip_newlines();
        self.peek()?;
        let result = self.parse_statement();
        Some(result)
    }
}

impl Parser {
    pub fn init(tokens: Vec<Token>) -> Self {
        Self { tokens, position: 0 }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParserError> {
        let mut statements = Vec::new();
        while let Some(statement) = self.next() {
            statements.push(statement);
        }
        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        let current = self.peek().unwrap();
        let statement = match &current.kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_let_statement()?,
            _ => self.parse_expression_statement()?,
        };
        self.parse_terminator()?;
        Ok(statement)
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        self.expect(|kind| matches!(kind, TokenKind::Keyword(Keyword::Let)), Expected::KeywordLet)?;

        let var_name_token = self.expect(|kind| matches!(kind, TokenKind::Identifier(_)), Expected::VariableDeclaration)?;
        let var_name = match &var_name_token.kind {
            TokenKind::Identifier(name) => name,
            _ => unreachable!(),
        }
        .to_owned();

        let var_type = self.parse_type_annotation()?;
        self.expect(|kind| matches!(kind, TokenKind::Assign), Expected::Assignment)?;
        let expr = self.parse_expression()?;
        Ok(Statement::LetDeclaration { var_name, var_type, expr })
    }

    fn parse_type_annotation(&mut self) -> Result<Option<Type>, ParserError> {
        if !matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::Colon))) {
            return Ok(None);
        }

        self.expect(
            |kind| matches!(kind, TokenKind::Delimiter(Delimiter::Colon)),
            Expected::TypeAnnotation,
        )?;
        let type_token = self.expect(|kind| matches!(kind, TokenKind::Type(_)), Expected::Type)?;
        let type_annotation = match &type_token.kind {
            TokenKind::Type(t) => t,
            _ => unreachable!(),
        };
        Ok(Some(type_annotation.to_owned()))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.parse_expression()?;
        Ok(Statement::Expression(expr))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_expression_with_min_bp(0)
    }

    fn parse_expression_with_min_bp(&mut self, min_bp: BindingPower) -> Result<Expression, ParserError> {
        let mut lhs = self.parse_atom()?;
        loop {
            let op_token = match self.peek() {
                Some(op) => op,
                None => break,
            };

            let op = match &op_token.kind {
                TokenKind::Operator(op) => op.clone(),
                _ => break,
            };

            let (lbp, rbp) = get_binding_power(op);
            if lbp < min_bp {
                break;
            }
            self.consume();

            let rhs = self.parse_expression_with_min_bp(rbp)?;
            lhs = Expression::BinaryOp(Box::new(lhs), op, Box::new(rhs));
        }
        Ok(lhs)
    }

    fn parse_atom(&mut self) -> Result<Expression, ParserError> {
        let token = match self.consume() {
            Some(t) => t,
            None => return Err(self.unexpected_eof_error()),
        };
        match token.kind {
            TokenKind::Number(num) => Ok(Expression::Number(num)),
            TokenKind::String(str) => Ok(Expression::String(str)),
            TokenKind::Bool(bool) => Ok(Expression::Bool(bool)),
            TokenKind::Identifier(ident) => Ok(Expression::Identifier(ident)),
            TokenKind::Delimiter(Delimiter::LParen) => {
                let expr = self.parse_expression()?;
                let Some(closing_token) = self.consume() else {
                    return Err(self.unexpected_eof_error());
                };
                match &closing_token.kind {
                    TokenKind::Delimiter(Delimiter::RParen) => Ok(expr),
                    _ => Err(ParserError {
                        kind: ParserErrorKind::UnexpectedToken {
                            expected: Expected::ClosingDelimiter(Delimiter::RParen),
                        },
                        token: closing_token,
                    }),
                }
            }
            TokenKind::Operator(op) => match get_prefix_binding_power(op) {
                Some(rbp) => {
                    let rhs = self.parse_expression_with_min_bp(rbp)?;
                    Ok(Expression::UnaryOp(op, Box::new(rhs)))
                }
                None => Err(ParserError {
                    kind: ParserErrorKind::InvalidUnaryOperator,
                    token: token,
                }),
            },
            _ => Err(ParserError {
                kind: ParserErrorKind::InvalidExpression,
                token: token,
            }),
        }
    }

    fn skip_newlines(&mut self) {
        while matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Newline)) {
            self.consume();
        }
    }

    fn parse_terminator(&mut self) -> Result<(), ParserError> {
        self.expect(|kind| matches!(kind, TokenKind::Newline), Expected::Terminator)?;
        Ok(())
    }

    fn expect(&mut self, predicate: fn(&TokenKind) -> bool, expected: Expected) -> Result<Token, ParserError> {
        let Some(token) = self.consume().clone() else {
            return Err(self.unexpected_eof_error());
        };
        if !predicate(&token.kind) {
            return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken { expected },
                token: token,
            });
        }
        Ok(token)
    }

    fn peek(&self) -> Option<&Token> {
        if self.position >= self.tokens.len() {
            return None;
        }
        return Some(&self.tokens[self.position]);
    }

    fn consume(&mut self) -> Option<Token> {
        if self.position >= self.tokens.len() {
            return None;
        }
        self.position += 1;
        return Some(self.tokens[self.position - 1].clone());
    }

    fn unexpected_eof_error(&self) -> ParserError {
        let token = match self.tokens.last() {
            Some(t) => t,
            None => &Token {
                kind: TokenKind::EOF,
                span: 0..0,
            },
        };
        ParserError {
            kind: ParserErrorKind::UnexpectedEOF,
            token: token.clone(),
        }
    }
}

type BindingPower = u8;
fn get_binding_power(op: Operator) -> (BindingPower, BindingPower) {
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

fn get_prefix_binding_power(op: Operator) -> Option<BindingPower> {
    use Operator::*;
    match op {
        Minus => Some(79),
        _ => None,
    }
}
