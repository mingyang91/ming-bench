use super::types::Value;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("unexpected end of input")]
    UnexpectedEof,
    #[error("unexpected character: {ch}")]
    UnexpectedChar { ch: char },
    #[error("unterminated string")]
    UnterminatedString,
}

pub struct Parser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    pub fn parse_all(&mut self) -> Result<Vec<Value>, ParseError> {
        let mut exprs = Vec::new();
        self.skip_whitespace();
        while self.pos < self.input.len() {
            exprs.push(self.parse_expr()?);
            self.skip_whitespace();
        }
        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Value, ParseError> {
        self.skip_whitespace();
        let ch = self.peek().ok_or(ParseError::UnexpectedEof)?;
        match ch {
            '"' => self.parse_string(),
            '#' => self.parse_boolean(),
            '-' if self.peek_at(1).is_some_and(|c: char| c.is_ascii_digit()) => {
                self.parse_integer()
            }
            '0'..='9' => self.parse_integer(),
            _ => Err(ParseError::UnexpectedChar { ch }),
        }
    }

    fn parse_integer(&mut self) -> Result<Value, ParseError> {
        let start = self.pos;
        if self.peek() == Some('-') {
            self.pos += 1;
        }
        while self.peek().is_some_and(|c| c.is_ascii_digit()) {
            self.pos += 1;
        }
        let n: i64 = self.input[start..self.pos]
            .parse()
            .map_err(|_| ParseError::UnexpectedEof)?;
        Ok(Value::Integer(n))
    }

    fn parse_boolean(&mut self) -> Result<Value, ParseError> {
        self.pos += 1; // skip '#'
        match self.peek() {
            Some('t') => { self.pos += 1; Ok(Value::Boolean(true)) }
            Some('f') => { self.pos += 1; Ok(Value::Boolean(false)) }
            Some(ch) => Err(ParseError::UnexpectedChar { ch }),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_string(&mut self) -> Result<Value, ParseError> {
        self.pos += 1; // skip opening '"'
        let start = self.pos;
        while let Some(ch) = self.peek() {
            match ch {
                '"' => return self.finish_string(start),
                '\\' => self.pos += 2,
                _ => self.pos += 1,
            }
        }
        Err(ParseError::UnterminatedString)
    }

    fn finish_string(&mut self, start: usize) -> Result<Value, ParseError> {
        let s = self.input[start..self.pos].to_string();
        self.pos += 1; // skip closing '"'
        Ok(Value::String(s))
    }

    fn skip_whitespace(&mut self) {
        while self.peek().is_some_and(|c| c.is_whitespace()) {
            self.pos += 1;
        }
    }

    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn peek_at(&self, offset: usize) -> Option<char> {
        self.input.get(self.pos + offset..)?.chars().next()
    }
}
