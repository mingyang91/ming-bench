use crate::scheme::error::SchemeError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
}

pub(crate) fn parse_program(input: &str) -> Result<Vec<Expr>, SchemeError> {
    let mut parser = Parser::new(input);
    let expressions = parser.parse_program()?;
    if expressions.is_empty() {
        Err(SchemeError::EmptyInput)
    } else {
        Ok(expressions)
    }
}

struct Parser<'a> {
    input: &'a str,
    index: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, index: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, SchemeError> {
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, SchemeError> {
        self.skip_ignored();

        match self.peek_char() {
            Some('"') => self.parse_string(),
            Some('#') => self.parse_boolean(),
            Some(ch) if ch == '+' || ch == '-' || ch.is_ascii_digit() => self.parse_integer(),
            Some(ch) => Err(SchemeError::InvalidTokenStart {
                ch,
                index: self.index,
            }),
            None => Err(SchemeError::UnexpectedEndOfInput {
                context: "expression",
            }),
        }
    }

    fn parse_boolean(&mut self) -> Result<Expr, SchemeError> {
        let start = self.index;
        let token = self.read_token();

        match token {
            "#t" => Ok(Expr::Boolean(true)),
            "#f" => Ok(Expr::Boolean(false)),
            _ => Err(SchemeError::InvalidBooleanLiteral {
                literal: token.to_owned(),
                index: start,
            }),
        }
    }

    fn parse_integer(&mut self) -> Result<Expr, SchemeError> {
        let start = self.index;
        let token = self.read_token();
        token
            .parse::<i64>()
            .map(Expr::Integer)
            .map_err(|_| SchemeError::InvalidInteger {
                literal: token.to_owned(),
                index: start,
            })
    }

    fn parse_string(&mut self) -> Result<Expr, SchemeError> {
        let start = self.index;
        let mut value = String::new();
        let _ = self.advance_char();

        while let Some(ch) = self.advance_char() {
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => value.push(self.parse_escape_sequence(start)?),
                _ => value.push(ch),
            }
        }

        Err(SchemeError::UnterminatedString { start })
    }

    fn parse_escape_sequence(&mut self, string_start: usize) -> Result<char, SchemeError> {
        let index = self.index;
        match self.advance_char() {
            Some('"') => Ok('"'),
            Some('\\') => Ok('\\'),
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some(escape) => Err(SchemeError::InvalidEscape { escape, index }),
            None => Err(SchemeError::UnterminatedString {
                start: string_start,
            }),
        }
    }

    fn read_token(&mut self) -> &'a str {
        let start = self.index;

        while self.peek_char().is_some_and(|ch| !is_token_delimiter(ch)) {
            let _ = self.advance_char();
        }

        &self.input[start..self.index]
    }

    fn skip_ignored(&mut self) {
        self.skip_whitespace();

        while self.peek_char() == Some(';') {
            self.skip_comment();
            self.skip_whitespace();
        }
    }

    fn skip_comment(&mut self) {
        while self.advance_char().is_some_and(|ch| ch != '\n') {}
    }

    fn skip_whitespace(&mut self) {
        while self.peek_char().is_some_and(|ch| ch.is_whitespace()) {
            let _ = self.advance_char();
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.index += ch.len_utf8();
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.index >= self.input.len()
    }
}

fn is_token_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '"' | '\'' | ';')
}
