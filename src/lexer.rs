use crate::token::*;
use std::{ops::Range, str};

#[derive(Debug)]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug, PartialEq)]
pub enum LexerErrorKind {
    InvalidAscii,
    InvalidCharacter,
    InvalidNumber,
    UnterminatedString,
    UnexpectedEOF,
}

pub struct Lexer<'src> {
    source: &'src [u8],
    position: usize,
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip();
        let span_start = self.position;
        let current = self.peek()?;

        let result = match current {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.word(span_start),

            b'+' | b'-' | b'*' | b'/' | b'^' | b'%' | b'<' | b'>' => self.operator(span_start),

            b':' | b',' | b'(' | b')' | b'[' | b']' | b'{' | b'}' | b'|' | b'.' => self.delimiter(span_start),

            b'0'..=b'9' => self.digit(span_start),

            b'"' => self.string(span_start),

            b'=' => self.assign(span_start),

            b'\n' => self.newline(span_start),

            _ => {
                self.consume();
                Err(LexerError {
                    kind: LexerErrorKind::InvalidCharacter,
                    span: span_start..self.position,
                })
            }
        };
        Some(result)
    }
}

impl<'src> Lexer<'src> {
    pub fn init(source: &'src str) -> Result<Self, LexerError> {
        let source = source.as_bytes();
        if let Some(invalid_ascii_pos) = source.iter().position(|byte| !byte.is_ascii()) {
            return Err(LexerError {
                kind: LexerErrorKind::InvalidAscii,
                span: invalid_ascii_pos..invalid_ascii_pos + 1,
            });
        }

        Ok(Self { source, position: 0 })
    }

    pub fn lex(&mut self) -> Vec<Result<Token, LexerError>> {
        let mut tokens = Vec::new();
        while let Some(token_result) = self.next() {
            tokens.push(token_result);
        }
        return tokens;
    }

    fn word(&mut self, span_start: usize) -> Result<Token, LexerError> {
        self.consume();

        while let Some(byte) = self.peek() {
            if byte.is_ascii_alphanumeric() || byte == b'_' {
                self.consume();
            } else {
                break;
            }
        }

        let word_span = span_start..self.position;
        let word_slice = &self.source[word_span.clone()];
        let word = str::from_utf8(word_slice).map_err(|_| LexerError {
            kind: LexerErrorKind::InvalidAscii,
            span: word_span,
        })?;

        let kind = match word {
            // Keywords
            "let" => TokenKind::Keyword(Keyword::Let),
            "where" => TokenKind::Keyword(Keyword::Where),
            "in" => TokenKind::Keyword(Keyword::In),
            "if" => TokenKind::Keyword(Keyword::If),
            "then" => TokenKind::Keyword(Keyword::Then),
            "else" => TokenKind::Keyword(Keyword::Else),

            // Booleans
            "true" => TokenKind::Bool(true),
            "false" => TokenKind::Bool(false),

            // Primitives
            "Num" => TokenKind::Type(Type::BaseType(BaseType::Num)),
            "Str" => TokenKind::Type(Type::BaseType(BaseType::Str)),
            "Bool" => TokenKind::Type(Type::BaseType(BaseType::Bool)),

            // Operators
            "is" => TokenKind::Operator(Operator::Is),
            "not" => TokenKind::Operator(Operator::Not),
            "and" => TokenKind::Operator(Operator::And),
            "or" => TokenKind::Operator(Operator::Or),

            _ => TokenKind::Identifier(word.to_string()),
        };

        Ok(Token {
            kind,
            span: span_start..self.position,
        })
    }

    fn operator(&mut self, span_start: usize) -> Result<Token, LexerError> {
        let op = self.consume().ok_or_else(|| LexerError {
            kind: LexerErrorKind::UnexpectedEOF,
            span: span_start..self.position,
        })?;

        let op_kind = match op {
            b'+' => Operator::Plus,
            b'-' => {
                if matches!(self.peek(), Some(b'>')) {
                    self.consume();
                    return Ok(Token {
                        kind: TokenKind::Delimiter(Delimiter::Arrow),
                        span: span_start..self.position,
                    });
                } else {
                    Operator::Minus
                }
            }
            b'*' => Operator::Multiply,
            b'/' => Operator::Divide,
            b'^' => Operator::Power,
            b'%' => Operator::Modulo,
            b'<' => {
                if let Some(byte) = self.peek()
                    && byte == b'='
                {
                    self.consume();
                    Operator::EqualLessThan
                } else {
                    Operator::LessThan
                }
            }
            b'>' => {
                if let Some(byte) = self.peek()
                    && byte == b'='
                {
                    self.consume();
                    Operator::EqualGreaterThan
                } else {
                    Operator::GreaterThan
                }
            }
            _ => unreachable!("Invalid operator"),
        };

        Ok(Token {
            kind: TokenKind::Operator(op_kind),
            span: span_start..self.position,
        })
    }

    fn delimiter(&mut self, span_start: usize) -> Result<Token, LexerError> {
        let delim = self.consume().ok_or_else(|| LexerError {
            kind: LexerErrorKind::UnexpectedEOF,
            span: span_start..self.position,
        })?;

        let delim_kind = match delim {
            b':' => Delimiter::Colon,
            b',' => Delimiter::Comma,
            b'(' => Delimiter::LParen,
            b')' => Delimiter::RParen,
            b'[' => Delimiter::LBrack,
            b']' => Delimiter::RBrack,
            b'{' => Delimiter::LBrace,
            b'}' => Delimiter::RBrace,
            b'|' => Delimiter::Pipe,
            b'.' => {
                if matches!(self.peek(), Some(b'.')) {
                    self.consume();
                    Delimiter::Range
                } else {
                    return Err(LexerError {
                        kind: LexerErrorKind::InvalidCharacter,
                        span: span_start..self.position,
                    });
                }
            }
            _ => unreachable!("Invalid delimiter"),
        };

        Ok(Token {
            kind: TokenKind::Delimiter(delim_kind),
            span: span_start..self.position,
        })
    }

    fn digit(&mut self, span_start: usize) -> Result<Token, LexerError> {
        self.consume();

        // before the decimal
        while let Some(byte) = self.peek() {
            if byte.is_ascii_digit() {
                self.consume();
            } else {
                break;
            }
        }

        // after the optional decimal point
        if self.peek() == Some(b'.') {
            self.consume();
            let first_decimal = self.consume().ok_or_else(|| LexerError {
                kind: LexerErrorKind::InvalidNumber,
                span: span_start..self.position,
            })?;
            if !first_decimal.is_ascii_digit() {
                return Err(LexerError {
                    kind: LexerErrorKind::InvalidNumber,
                    span: span_start..self.position,
                });
            }

            while let Some(byte) = self.peek() {
                if byte.is_ascii_digit() {
                    self.consume();
                } else {
                    break;
                }
            }
        }

        let span = span_start..self.position;
        let number_str = str::from_utf8(&self.source[span.clone()]).map_err(|_| LexerError {
            kind: LexerErrorKind::InvalidAscii,
            span: span.clone(),
        })?;
        let number = number_str.parse::<f64>().map_err(|_| LexerError {
            kind: LexerErrorKind::InvalidNumber,
            span: span.clone(),
        })?;
        Ok(Token {
            kind: TokenKind::Number(number),
            span,
        })
    }

    fn string(&mut self, span_start: usize) -> Result<Token, LexerError> {
        self.consume();

        loop {
            match self.peek() {
                Some(b'"') => {
                    self.consume();
                    let slice = &self.source[span_start + 1..self.position - 1];
                    let string = str::from_utf8(slice).map_err(|_| LexerError {
                        kind: LexerErrorKind::InvalidAscii,
                        span: span_start..self.position,
                    })?;
                    return Ok(Token {
                        kind: TokenKind::String(string.to_string()),
                        span: span_start..self.position,
                    });
                }

                Some(b'\n') => {
                    return Err(LexerError {
                        kind: LexerErrorKind::UnterminatedString,
                        span: span_start..self.position,
                    });
                }

                Some(_) => {
                    self.consume();
                }

                None => {
                    return Err(LexerError {
                        kind: LexerErrorKind::UnterminatedString,
                        span: span_start..self.position,
                    });
                }
            }
        }
    }

    fn assign(&mut self, span_start: usize) -> Result<Token, LexerError> {
        self.consume();
        Ok(Token {
            kind: TokenKind::Assign,
            span: span_start..self.position,
        })
    }

    fn newline(&mut self, span_start: usize) -> Result<Token, LexerError> {
        self.consume();
        Ok(Token {
            kind: TokenKind::Newline,
            span: span_start..self.position,
        })
    }

    fn skip(&mut self) {
        loop {
            match self.peek() {
                Some(b' ') | Some(b'\r') | Some(b'\t') => {
                    self.consume();
                }
                Some(b'#') => {
                    self.consume();
                    while let Some(comment) = self.peek() {
                        if comment == b'\n' {
                            break;
                        }
                        self.consume();
                    }
                }
                _ => break,
            }
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.position >= self.source.len() {
            return None;
        }
        return Some(self.source[self.position]);
    }

    fn consume(&mut self) -> Option<u8> {
        if self.position >= self.source.len() {
            return None;
        }
        self.position += 1;
        return Some(self.source[self.position - 1]);
    }
}
