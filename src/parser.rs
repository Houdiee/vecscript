/*
Program ::= { Statement } ;

Statement ::= SolveForInDeclaration TERMINATE
            | LetInDeclaration TERMINATE
            | LetDeclaration TERMINATE
            | SetDeclaration TERMINATE
            | Expression TERMINATE ;

SolveForInDeclaration ::= SOLVE FOR IDENTIFIER [ TypeAnnotation ] IN Expression EQUALS Expression ;
LetInDeclaration      ::= LET Binding IN Expression ;
LetDeclaration        ::= LET Binding [ WhereClause ] ;
WhereClause ::= WHERE Binding { COMMA Binding } ;

SetDeclaration ::= LET IDENTIFIER [ TypeAnnotation ] EQUALS LBRACE [ Expression { COMMA Expression } ] RBRACE ;

Binding ::= IDENTIFIER [ TypeAnnotation ] EQUALS Expression ;
TypeAnnotation ::= COLON TYPE ;

Expression ::= ComparisonExpression ;
ComparisonExpression ::= ArithmeticExpression { OPERATOR ArithmeticExpression } ;
ArithmeticExpression ::= MultiplicativeExpression {( PLUS | MINUS ) MultiplicativeExpression } ;
MultiplicativeExpression ::= UnaryExpression {( MULTIPLY | DIVIDE ) UnaryExpression } ;
UnaryExpression ::= [ MINUS ] PowerExpression ;
PowerExpression ::= Atom [ POWER PowerExpression ];
Atom ::= NUMBER
         | BOOL
         | STRING
         | IDENTIFIER
         | LPAREN Expression RPAREN ;

TERMINATE ::= NEWLINE ;
*/

use crate::{ast::*, token::*};

pub enum ParserError {
    UnexpectedEndOfInput,
    UnexpectedToken { expected: Expected, got: Token },
    MissingTerminator,
}

pub enum Expected {
    ClosingDelimiter(Delimiter),
    Atom,
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

#[allow(unused)]
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, position: 0 }
    }

    pub fn parse(&mut self) -> Result<Program, ParserError> {
        todo!()
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        let current = match self.peek() {
            None => return Err(ParserError::UnexpectedEndOfInput),
            Some(token) => token,
        };

        match &current.kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_let_declaration(),
            _ => todo!(),
        }
    }

    fn parse_let_declaration(&mut self) -> Result<Statement, ParserError> {
        todo!()
    }

    fn parse_atom(&mut self) -> Result<Expression, ParserError> {
        let token = self.consume().ok_or_else(|| ParserError::UnexpectedEndOfInput)?;

        match &token.kind {
            TokenKind::Number(num) => Ok(Expression::Number(*num)),
            TokenKind::Bool(bool) => Ok(Expression::Bool(*bool)),
            TokenKind::String(str) => Ok(Expression::String(str.clone())),
            TokenKind::Identifier(ident) => Ok(Expression::Identifier(ident.clone())),
            TokenKind::Delimiter(Delimiter::LParen) => {
                let inner_expression = self.parse_expression()?;
                self.expect(
                    TokenKind::Delimiter(Delimiter::RParen),
                    Expected::ClosingDelimiter(Delimiter::RParen),
                );
                return Ok(Expression::Expression(Box::new(inner_expression)));
            }
            _ => {
                return Err(ParserError::UnexpectedToken {
                    expected: Expected::Atom,
                    got: token.clone(),
                });
            }
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        todo!()
    }

    fn expect(&mut self, kind: TokenKind, error: Expected) -> Result<&Token, ParserError> {
        let token = self.consume().ok_or_else(|| ParserError::UnexpectedEndOfInput)?;
        if matches!(&token.kind, kind) {
            Ok(token)
        } else {
            Err(ParserError::UnexpectedToken {
                expected: error,
                got: token.clone(),
            })
        }
    }

    fn peek(&self) -> Option<&Token> {
        if self.position >= self.tokens.len() {
            return None;
        }
        return Some(&self.tokens[self.position]);
    }

    fn consume(&mut self) -> Option<&Token> {
        if self.position >= self.tokens.len() {
            return None;
        }
        self.position += 1;
        return Some(&self.tokens[self.position - 1]);
    }
}
