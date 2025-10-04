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

pub enum ParserError {}

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

    fn parse_statement(&mut self) {
        todo!()
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
