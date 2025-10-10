/*
Program ::= { Statement } ;

Statement ::= SolveForInDeclaration TERMINATE
            | LetStatement TERMINATE
            | SetDeclaration TERMINATE
            | Expression TERMINATE ;

LetStatement ::= LetInDeclaration | LetDeclaration ;
SolveForInDeclaration ::= SOLVE FOR IDENTIFIER [ TypeAnnotation ] IN [ TERMINATE ] Expression EQUALS Expression ;
LetInDeclaration      ::= LET Binding IN [ TERMINATE ] Expression ;
LetDeclaration        ::= LET Binding [ WhereClause ] ;
WhereClause ::= WHERE [ TERMINATE ] Binding { COMMA [ TERMINATE ] Binding } ;

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

// TODO fix the solve for in statement
use crate::{ast::*, token::*};

#[derive(Debug)]
pub enum ParserError {
    UnexpectedEndOfInput,
    UnexpectedToken { expected: Expected, got: Token },
}

#[derive(Debug)]
pub enum Expected {
    ClosingDelimiter(Delimiter),
    Identifier,
    LetStatement,
    For,
    InClause,
    WhereClause,
    Assignment,
    SolveStatement,
    TypeAnnotation,
    Newline,
    Atom,
    ValidToken,
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
        let mut statements = Vec::new();
        while self.peek().is_some() {
            let current_kind = self.peek().map(|t| &t.kind);
            if matches!(current_kind, Some(TokenKind::EOF)) {
                break;
            }
            if matches!(current_kind, Some(TokenKind::Newline)) {
                self.consume();
                continue;
            }
            statements.push(self.statement()?);
        }

        Ok(Program { statements })
    }

    /*
    Statement ::= SolveForInDeclaration TERMINATE
                | LetInDeclaration TERMINATE
                | LetDeclaration TERMINATE
                | SetDeclaration TERMINATE
                | Expression TERMINATE ;
    */
    fn statement(&mut self) -> Result<Statement, ParserError> {
        let current = match self.peek() {
            None => return Err(ParserError::UnexpectedEndOfInput),
            Some(token) => token,
        };

        let statement = match &current.kind {
            TokenKind::Keyword(Keyword::Solve) => self.solve_for_in_declaration(),
            TokenKind::Keyword(Keyword::Let) => self.let_statement(),

            // Atoms
            TokenKind::Number(_)
            | TokenKind::Bool(_)
            | TokenKind::String(_)
            | TokenKind::Identifier(_)
            | TokenKind::Delimiter(Delimiter::LParen) => self.expression_statement(),

            _ => Err(ParserError::UnexpectedToken {
                expected: Expected::ValidToken,
                got: current.clone(),
            }),
        }?;
        self.terminate()?;
        Ok(statement)
    }

    // SolveForInDeclaration ::= SOLVE FOR IDENTIFIER [ TypeAnnotation ] IN Expression EQUALS Expression ;
    fn solve_for_in_declaration(&mut self) -> Result<Statement, ParserError> {
        self.expect(TokenKind::Keyword(Keyword::Solve), Expected::SolveStatement)?;
        self.expect(TokenKind::Keyword(Keyword::For), Expected::For)?;
        let var_token = self.expect_kind(|kind| matches!(kind, TokenKind::Identifier(_)), Expected::Identifier)?;
        let var = match &var_token.kind {
            TokenKind::Identifier(s) => s.clone(),
            _ => unreachable!(),
        };
        let var_type = self.type_annotation()?;
        self.expect(TokenKind::Keyword(Keyword::In), Expected::InClause)?;
        let left = self.expression()?;
        self.expect(TokenKind::Operator(Operator::Equals), Expected::Assignment)?;
        let right = self.expression()?;

        Ok(Statement::SolveForInDeclaration {
            var,
            var_type,
            left,
            right,
        })
    }

    // LetStatement ::= LetInDeclaration | LetDeclaration ;
    fn let_statement(&mut self) -> Result<Statement, ParserError> {
        self.expect(TokenKind::Keyword(Keyword::Let), Expected::LetStatement)?;
        let binding = self.binding()?;

        // LetInDeclaration ::= LET Binding IN [ TERMINATE ] Expression ;
        if matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Keyword(Keyword::In))) {
            self.consume();
            self.optional_terminator()?;
            let bound_to = self.expression()?;
            return Ok(Statement::LetInDeclaration {
                binding,
                bound_to: Box::new(bound_to),
            });
        }

        // LetDeclaration ::= LET Binding [ WhereClause ] ;
        let where_clause = self.where_clause()?;
        return Ok(Statement::LetDeclaration { binding, where_clause });
    }

    // WhereClause ::= WHERE [ TERMINATE ] Binding { COMMA [ TERMINATE ] Binding } ;
    fn where_clause(&mut self) -> Result<Option<WhereClause>, ParserError> {
        if !matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Keyword(Keyword::Where))) {
            return Ok(None);
        }

        self.expect(TokenKind::Keyword(Keyword::Where), Expected::WhereClause)?;
        self.optional_terminator()?;

        let mut bindings = Vec::new();
        bindings.push(self.binding()?);

        while matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::Comma))) {
            self.consume();
            self.optional_terminator()?;
            bindings.push(self.binding()?);
        }
        Ok(Some(WhereClause { bindings }))
    }

    // Binding ::= IDENTIFIER [ TypeAnnotation ] EQUALS Expression ;
    fn binding(&mut self) -> Result<Binding, ParserError> {
        let var_token = self.expect_kind(|kind| matches!(kind, TokenKind::Identifier(_)), Expected::Identifier)?;
        let var = match &var_token.kind {
            TokenKind::Identifier(s) => s.clone(),
            _ => unreachable!(),
        };
        let var_type = self.type_annotation()?;
        self.expect(TokenKind::Operator(Operator::Equals), Expected::Assignment)?;
        let var_expression = self.expression()?;

        Ok(Binding {
            var,
            var_type,
            var_expression,
        })
    }

    // TypeAnnotation ::= COLON TYPE ;
    fn type_annotation(&mut self) -> Result<Option<Primitive>, ParserError> {
        if matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Delimiter(Delimiter::Colon))) {
            self.consume();
            let type_token = self.consume().ok_or_else(|| ParserError::UnexpectedEndOfInput)?;
            match &type_token.kind {
                TokenKind::Type(primitive) => return Ok(Some(primitive.clone())),
                _ => {
                    return Err(ParserError::UnexpectedToken {
                        expected: Expected::TypeAnnotation,
                        got: type_token.clone(),
                    });
                }
            }
        }
        Ok(None)
    }

    fn expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = self.expression()?;
        return Ok(Statement::Expression(expression));
    }

    // Expression ::= ComparisonExpression ;
    fn expression(&mut self) -> Result<Expression, ParserError> {
        return self.comparison_expression();
    }

    // ComparisonExpression ::= ArithmeticExpression { OPERATOR ArithmeticExpression } ;
    fn comparison_expression(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.arithmetic_expression()?;

        while let Some(current) = self.peek() {
            if let TokenKind::Operator(op) = &current.kind {
                let op = op.clone();
                self.consume();
                let right = self.arithmetic_expression()?;
                left = Expression::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }

    // ArithmeticExpression ::= MultiplicativeExpression {( PLUS | MINUS ) MultiplicativeExpression } ;
    fn arithmetic_expression(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.multiplicative_expression()?;

        while let Some(current) = self.peek() {
            match &current.kind {
                TokenKind::Operator(op) if matches!(op, Operator::Plus) || matches!(op, Operator::Minus) => {
                    let op = op.clone();
                    self.consume();
                    let right = self.multiplicative_expression()?;
                    left = Expression::BinaryOp {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(left)
    }

    // MultiplicativeExpression ::= UnaryExpression {( MULTIPLY | DIVIDE ) UnaryExpression } ;
    fn multiplicative_expression(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.unary_expression()?;

        while let Some(current) = self.peek() {
            match &current.kind {
                TokenKind::Operator(op) if matches!(op, Operator::Multiply) || matches!(op, Operator::Divide) => {
                    let op = op.clone();
                    self.consume();
                    let right = self.multiplicative_expression()?;
                    left = Expression::BinaryOp {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(left)
    }

    // UnaryExpression ::= [ MINUS ] PowerExpression ;
    fn unary_expression(&mut self) -> Result<Expression, ParserError> {
        if matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Operator(Operator::Minus))) {
            self.consume();
            let right = self.power_expression()?;
            return Ok(Expression::UnaryOp {
                op: Operator::Minus,
                expr: Box::new(right),
            });
        }
        return self.power_expression();
    }

    // PowerExpression ::= Atom [ POWER PowerExpression ];
    fn power_expression(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.atom()?;

        while matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Operator(Operator::Power))) {
            self.consume();
            let right = self.power_expression()?;
            left = Expression::BinaryOp {
                left: Box::new(left),
                op: Operator::Power,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /* Atom ::= NUMBER
              | BOOL
              | STRING
              | IDENTIFIER
              | LPAREN Expression RPAREN ;
    */
    fn atom(&mut self) -> Result<Expression, ParserError> {
        let token = self.consume().ok_or_else(|| ParserError::UnexpectedEndOfInput)?;
        match &token.kind {
            TokenKind::Number(num) => Ok(Expression::Number(*num)),
            TokenKind::Bool(bool) => Ok(Expression::Bool(*bool)),
            TokenKind::String(str) => Ok(Expression::String(str.clone())),
            TokenKind::Identifier(ident) => Ok(Expression::Identifier(ident.clone())),
            TokenKind::Delimiter(Delimiter::LParen) => {
                let inner_expression = self.expression()?;
                self.expect(
                    TokenKind::Delimiter(Delimiter::RParen),
                    Expected::ClosingDelimiter(Delimiter::RParen),
                );
                return Ok(inner_expression);
            }
            _ => {
                return Err(ParserError::UnexpectedToken {
                    expected: Expected::Atom,
                    got: token.clone(),
                });
            }
        }
    }

    fn optional_terminator(&mut self) -> Result<(), ParserError> {
        if matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Newline)) {
            self.consume();
        }
        Ok(())
    }

    fn terminate(&mut self) -> Result<(), ParserError> {
        if matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Newline)) {
            self.consume();
            return Ok(());
        }

        match self.peek() {
            None => Ok(()),
            Some(token) => Err(ParserError::UnexpectedToken {
                expected: Expected::Newline,
                got: token.clone(),
            }),
        }
    }

    fn expect(&mut self, kind: TokenKind, error: Expected) -> Result<&Token, ParserError> {
        return self.expect_kind(|token_kind| matches!(token_kind, kind), error);
    }

    fn expect_kind(&mut self, predicate: fn(&TokenKind) -> bool, error: Expected) -> Result<&Token, ParserError> {
        let token = self.consume().ok_or_else(|| ParserError::UnexpectedEndOfInput)?;
        if predicate(&token.kind) {
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
