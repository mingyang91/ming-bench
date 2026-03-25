use super::{parse_number_literal, syntax_error, EvalError, Expr, ExprKind, SourcePos};

pub(super) fn parse_program(input: &str) -> Result<Vec<Expr>, EvalError> {
    Parser::new(input).parse_program()
}

struct Parser<'a> {
    input: &'a str,
    offset: usize,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            offset: 0,
            line: 1,
            column: 1,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            Err(syntax_error(self.current_pos(), "empty input"))
        } else {
            Ok(exprs)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let pos = self.current_pos();

        match self.peek_char() {
            Some('(') => self.parse_list(pos),
            Some('\'') => self.parse_quote_shorthand(pos),
            Some('"') => self.parse_string(pos),
            Some('#') => self.parse_hash_literal(pos),
            Some('+') | Some('-')
                if self
                    .peek_second_char()
                    .is_some_and(|ch| ch.is_ascii_digit()) =>
            {
                self.parse_number(pos)
            }
            Some(ch) if ch.is_ascii_digit() => self.parse_number(pos),
            Some(_) => self.parse_symbol(pos),
            None => Err(unexpected_eof(pos)),
        }
    }

    fn parse_quote_shorthand(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.bump();
        let expr = self.parse_expr()?;
        Ok(Expr::new(
            pos,
            ExprKind::List(vec![Expr::new(pos, ExprKind::Symbol("quote".into())), expr]),
        ))
    }

    fn parse_list(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.bump();
        let mut items = Vec::new();

        loop {
            self.skip_ignored();
            match self.peek_char() {
                Some(')') => {
                    self.bump();
                    return Ok(Expr::new(pos, ExprKind::List(items)));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(unexpected_eof(self.current_pos())),
            }
        }
    }

    fn parse_string(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.bump();
        let mut value = String::new();

        loop {
            match self.bump() {
                Some('"') => return Ok(Expr::new(pos, ExprKind::String(value))),
                Some('\\') => {
                    let escaped = match self.bump() {
                        Some('"') => '"',
                        Some('\\') => '\\',
                        Some('n') => '\n',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some(ch) => ch,
                        None => return Err(unexpected_eof(self.current_pos())),
                    };
                    value.push(escaped);
                }
                Some(ch) => value.push(ch),
                None => return Err(unexpected_eof(self.current_pos())),
            }
        }
    }

    fn parse_boolean(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        if self.consume_literal("#t") {
            return Ok(Expr::new(pos, ExprKind::Boolean(true)));
        }

        if self.consume_literal("#f") {
            return Ok(Expr::new(pos, ExprKind::Boolean(false)));
        }

        Err(syntax_error(pos, "invalid boolean literal"))
    }

    fn parse_hash_literal(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        if self.remaining().starts_with("#\\") {
            self.bump();
            self.bump();
            return self.parse_character(pos);
        }

        self.parse_boolean(pos)
    }

    fn parse_character(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        let start = self.offset;

        while matches!(self.peek_char(), Some(ch) if !is_delimiter(ch)) {
            self.bump();
        }

        let token = &self.input[start..self.offset];
        let ch = match token {
            "space" => ' ',
            "newline" => '\n',
            _ => {
                let mut chars = token.chars();
                match (chars.next(), chars.next()) {
                    (Some(ch), None) => ch,
                    _ => return Err(syntax_error(pos, "invalid character literal")),
                }
            }
        };

        Ok(Expr::new(pos, ExprKind::Character(ch)))
    }

    fn parse_number(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        let start = self.offset;

        if matches!(self.peek_char(), Some('+') | Some('-')) {
            self.bump();
        }

        let mut saw_digit = false;
        while matches!(self.peek_char(), Some(ch) if ch.is_ascii_digit()) {
            saw_digit = true;
            self.bump();
        }

        if !saw_digit {
            return Err(syntax_error(pos, "invalid number literal"));
        }

        match self.peek_char() {
            Some('.') => {
                self.bump();

                let mut saw_fraction_digit = false;
                while matches!(self.peek_char(), Some(ch) if ch.is_ascii_digit()) {
                    saw_fraction_digit = true;
                    self.bump();
                }

                if !saw_fraction_digit {
                    return Err(syntax_error(pos, "invalid number literal"));
                }
            }
            Some('/') => {
                self.bump();

                if matches!(self.peek_char(), Some('+') | Some('-')) {
                    self.bump();
                }

                let mut saw_denominator_digit = false;
                while matches!(self.peek_char(), Some(ch) if ch.is_ascii_digit()) {
                    saw_denominator_digit = true;
                    self.bump();
                }

                if !saw_denominator_digit {
                    return Err(syntax_error(pos, "invalid number literal"));
                }
            }
            _ => {}
        }

        if self.peek_char().is_some_and(|ch| !is_delimiter(ch)) {
            return Err(syntax_error(pos, "invalid number literal"));
        }

        let token = &self.input[start..self.offset];
        let value = parse_number_literal(token)
            .map_err(|_| syntax_error(pos, format!("invalid number literal: {token}")))?;

        Ok(Expr::new(pos, ExprKind::Number(value)))
    }

    fn parse_symbol(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        let start = self.offset;

        while matches!(self.peek_char(), Some(ch) if !is_delimiter(ch)) {
            self.bump();
        }

        if start == self.offset {
            return Err(syntax_error(pos, "expected expression"));
        }

        Ok(Expr::new(
            pos,
            ExprKind::Symbol(self.input[start..self.offset].to_string()),
        ))
    }

    fn skip_ignored(&mut self) {
        loop {
            match self.peek_char() {
                Some(ch) if ch.is_whitespace() => {
                    self.bump();
                }
                Some(';') => {
                    while let Some(ch) = self.bump() {
                        if ch == '\n' {
                            break;
                        }
                    }
                }
                _ => return,
            }
        }
    }

    fn consume_literal(&mut self, literal: &str) -> bool {
        if !self.remaining().starts_with(literal) {
            return false;
        }

        let end = self.offset + literal.len();
        if self
            .input
            .get(end..)
            .and_then(|rest| rest.chars().next())
            .is_some_and(|ch| !is_delimiter(ch))
        {
            return false;
        }

        for _ in literal.chars() {
            self.bump();
        }

        true
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos::new(self.line, self.column)
    }

    fn remaining(&self) -> &'a str {
        &self.input[self.offset..]
    }

    fn peek_char(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    fn peek_second_char(&self) -> Option<char> {
        let mut chars = self.remaining().chars();
        chars.next()?;
        chars.next()
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.offset += ch.len_utf8();

        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.input.len()
    }
}

fn unexpected_eof(pos: SourcePos) -> EvalError {
    EvalError::UnexpectedEof { pos }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';')
}
