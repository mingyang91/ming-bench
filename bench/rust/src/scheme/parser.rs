use super::number::parse_number_token;
use super::{EvalError, Expr, SourcePos};

pub(super) struct Parser<'a> {
    input: &'a str,
    pos: usize,
    line: usize,
    col: usize,
}

impl<'a> Parser<'a> {
    pub(super) fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    pub(super) fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            return Err(EvalError::EmptyInput);
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let position = self.current_position();
        match self.peek_char() {
            Some('(') => self.parse_list(position),
            Some('\'') => self.parse_quote(position),
            Some('"') => self.parse_string(position),
            Some(')') => Err(EvalError::SyntaxError {
                message: "unexpected ')'".into(),
            }
            .with_position(position)),
            Some(_) => self.parse_atom(position),
            None => Err(EvalError::UnexpectedEof.with_position(position)),
        }
    }

    fn parse_list(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.expect_char('(', position)?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();
            match self.peek_char() {
                Some(')') => {
                    self.advance_char();
                    return Ok(Expr::List(items, position));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof.with_position(self.current_position())),
            }
        }
    }

    fn parse_quote(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.expect_char('\'', position)?;
        let quoted = self.parse_expr()?;
        Ok(Expr::List(
            vec![Expr::Symbol("quote".into(), position), quoted],
            position,
        ))
    }

    fn parse_string(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.expect_char('"', position)?;
        let mut value = String::new();

        while let Some(ch) = self.advance_char() {
            match ch {
                '"' => return Ok(Expr::String(value, position)),
                '\\' => {
                    let escaped = self
                        .advance_char()
                        .ok_or_else(|| EvalError::UnexpectedEof.with_position(position))?;
                    match escaped {
                        '"' => value.push('"'),
                        '\\' => value.push('\\'),
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        other => value.push(other),
                    }
                }
                other => value.push(other),
            }
        }

        Err(EvalError::UnexpectedEof.with_position(position))
    }

    fn parse_atom(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        let token = self.read_token();
        if token.is_empty() {
            return Err(EvalError::UnexpectedEof.with_position(position));
        }

        match token {
            "#t" => Ok(Expr::Boolean(true, position)),
            "#f" => Ok(Expr::Boolean(false, position)),
            _ if token.starts_with("#\\") => {
                let value = parse_char_literal(token).ok_or_else(|| {
                    EvalError::SyntaxError {
                        message: format!("invalid character literal: {token}"),
                    }
                    .with_position(position)
                })?;
                Ok(Expr::Char(value, position))
            }
            _ => match parse_number_token(token) {
                Some(Ok(number)) => Ok(Expr::Number(number, position)),
                Some(Err(error)) => Err(EvalError::SyntaxError {
                    message: error.message(),
                }
                .with_position(position)),
                None => Ok(Expr::Symbol(token.to_string(), position)),
            },
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
                self.advance_char();
            }

            if self.peek_char() == Some(';') {
                while let Some(ch) = self.advance_char() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn read_token(&mut self) -> &'a str {
        let start = self.pos;

        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.advance_char();
        }

        &self.input[start..self.pos]
    }

    fn expect_char(&mut self, expected: char, position: SourcePos) -> Result<(), EvalError> {
        match self.advance_char() {
            Some(actual) if actual == expected => Ok(()),
            Some(actual) => Err(EvalError::SyntaxError {
                message: format!("expected '{expected}', found '{actual}'"),
            }
            .with_position(position)),
            None => Err(EvalError::UnexpectedEof.with_position(position)),
        }
    }

    fn current_position(&self) -> SourcePos {
        SourcePos::new(self.line, self.col)
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.pos += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }
}

fn parse_char_literal(token: &str) -> Option<char> {
    let suffix = token.strip_prefix("#\\")?;
    match suffix {
        "space" => Some(' '),
        "newline" => Some('\n'),
        _ => {
            let mut chars = suffix.chars();
            let ch = chars.next()?;
            if chars.next().is_none() {
                Some(ch)
            } else {
                None
            }
        }
    }
}
