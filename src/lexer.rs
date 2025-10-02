use std::{ops::Range, str};

use crate::token::{Token, TokenKind};

pub struct LexerError {
    pub kind: LexerErrorKind,
    pub span: Range<usize>,
}

pub enum LexerErrorKind {
    InvalidAscii,
    InvalidCharacter,
    InvalidNumber,
    UnexpectedEOF,
}

#[allow(unused)]
pub struct Lexer<'src> {
    source: &'src [u8],
    position: usize,
}

#[allow(unused)]
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

    pub fn next(&mut self) -> Result<Token, LexerError> {
        let span_start = self.position;
        let current = match self.peek() {
            None => {
                return Ok(Token {
                    kind: TokenKind::EOF,
                    span: span_start..self.position,
                });
            }
            Some(byte) => byte,
        };

        match current {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => todo!(),

            b'0'..=b'9' => todo!(),

            b':' | b',' | b'(' | b')' | b'[' | b']' | b'{' | b'{' => todo!(),

            b'\n' => self.newline(span_start),

            _ => {
                self.consume();
                Err(LexerError {
                    kind: LexerErrorKind::InvalidCharacter,
                    span: span_start..self.position,
                })
            }
        }
    }

    fn digit(&mut self, span_start: usize) -> Result<Token, LexerError> {
        // before the decimal point
        let first_digit = self.consume().ok_or_else(|| LexerError {
            kind: LexerErrorKind::UnexpectedEOF,
            span: span_start..self.position,
        })?;

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

    fn newline(&mut self, span_start: usize) -> Result<Token, LexerError> {
        self.consume();
        Ok(Token {
            kind: TokenKind::Newline,
            span: span_start..self.position,
        })
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
