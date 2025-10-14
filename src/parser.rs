/*
Program ::= { Definition }
Definition ::= LetDefinition TERMINATE
            | TypeDefinition TERMINATE
            ;

LetDefinition ::= LET Binding ;

Binding ::= VariableBinding | FunctionBinding ;

VariableBinding ::= IDENTIFIER [ TypeAnnotation ] ASSIGN Expression ;
VariableBindingList ::= VariableBinding { COMMA [ TERMINATE ] VariableBinding } ;

FunctionBinding ::= IDENTIFIER LPAREN [ ParameterList ] RPAREN [ ReturnType ] ASSIGN Expression ;
Parameter ::= IDENTIFIER [ TypeAnnotation ] ;
ParameterList ::= Parameter { COMMA Parameter } ;

TypeAnnotation ::= COLON TYPE ;
ReturnType ::= ARROW TypeAnnotation ;

Expression ::= SimpleExpression [ WhereSuffix ]
             | LetInExpression
             | IfElseExpression
             ;

SimpleExpression ::= [ OPERATOR ] Atom { OPERATOR Atom } ;
WhereSuffix ::= WHERE [ TERMINATE ] VariableBindingList ;
LetInExpression ::= LET VariableBindingList IN Expression ;
IfElseExpression ::= IF Expression THEN Expression ELSE Expression ;

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
    OpeningDelimiter(Delimiter),
    KeywordLet,
    VariableName,
    TypeAnnotation,
    Type,
    Assignment,
    Terminator,
    FunctionParameter,
    Identifier,
    Binding,
    ReturnType,
    Definition,
}

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
        let result = self.parse_definition();
        Some(result)
    }
}

impl Parser {
    pub fn init(tokens: Vec<Token>) -> Self {
        Self { tokens, position: 0 }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParserError> {
        let mut definitions = Vec::new();
        while let Some(definition) = self.next() {
            definitions.push(definition);
        }
        Ok(Program { definitions })
    }

    fn parse_definition(&mut self) -> Result<Definition, ParserError> {
        let next_token = self.peek().ok_or_else(|| self.unexpected_eof_error())?;
        let definition = match next_token.kind {
            TokenKind::Keyword(Keyword::Let) => Definition::Let(self.parse_let_definition()?),
            _ => {
                return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedToken {
                        expected: Expected::Definition,
                    },
                    token: next_token.clone(),
                });
            }
        };
        Ok(definition)
    }

    fn parse_let_definition(&mut self) -> Result<LetDefinition, ParserError> {
        self.expect(|kind| matches!(kind, TokenKind::Keyword(Keyword::Let)), Expected::KeywordLet)?;
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
        let expr = self.parse_expression()?;
        Ok(VariableBinding { name, var_type, expr })
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
        let expr = match next_token.kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_let_in_expression()?,
            TokenKind::Keyword(Keyword::If) => self.parse_if_else_expression()?,
            _ => self.parse_simple_expression()?,
        };
        Ok(expr)
    }

    fn parse_let_in_expression(&mut self) -> Result<Expression, ParserError> {
        todo!()
    }

    fn parse_if_else_expression(&mut self) -> Result<Expression, ParserError> {
        todo!()
    }

    fn parse_simple_expression(&mut self) -> Result<Expression, ParserError> {
        todo!()
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
        self.expect(|kind| matches!(kind, TokenKind::Newline), Expected::Terminator)?;
        Ok(())
    }

    fn expect(&mut self, predicate: fn(&TokenKind) -> bool, expected: Expected) -> Result<Token, ParserError> {
        let token = self.consume().ok_or_else(|| self.unexpected_eof_error())?;
        if !predicate(&token.kind) {
            return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken { expected },
                token: token,
            });
        }
        Ok(token)
    }

    fn peek_nth(&self, nth: usize) -> Option<&Token> {
        if self.position + nth >= self.tokens.len() {
            return None;
        }
        return Some(&self.tokens[self.position + nth]);
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

fn get_function_call_binding_power() -> (u8, u8) {
    (150, 149)
}
