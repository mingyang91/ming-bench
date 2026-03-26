use super::{error::SourcePos, model::Expr, number::Number, EvalError};

pub(super) struct Parser<'a> {
    input: &'a str,
    pos: usize,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    pub(super) fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    pub(super) fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();

        loop {
            self.skip_ws_and_comments();
            if self.peek_char().is_none() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }

        if exprs.is_empty() {
            return Err(EvalError::EmptyInput.with_position(self.current_pos()));
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ws_and_comments();
        let pos = self.current_pos();

        match self.peek_char() {
            Some('(') => self.parse_list(pos),
            Some(')') => Err(EvalError::Syntax {
                message: "unexpected ')'".into(),
            }
            .with_position(pos)),
            Some('#') if self.input[self.pos..].starts_with("#'") => {
                self.parse_syntax_shorthand(pos)
            }
            Some('`') => self.parse_keyword_shorthand(pos, 1, "quasiquote"),
            Some(',') if self.input[self.pos..].starts_with(",@") => {
                self.parse_keyword_shorthand(pos, 2, "unquote-splicing")
            }
            Some(',') => self.parse_keyword_shorthand(pos, 1, "unquote"),
            Some('\'') => self.parse_quote_shorthand(pos),
            Some('"') => self.parse_string(pos),
            Some(_) => self.parse_atom(pos),
            None => Err(EvalError::UnexpectedEof.with_position(pos)),
        }
    }

    fn parse_list(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.bump_char();
        let mut items = Vec::new();

        loop {
            self.skip_ws_and_comments();
            match self.peek_char() {
                Some(')') => {
                    self.bump_char();
                    return Ok(Expr::List(items, pos));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof.with_position(pos)),
            }
        }
    }

    fn parse_string(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.bump_char();
        let mut value = String::new();

        loop {
            match self.bump_char() {
                Some('"') => return Ok(Expr::String(value, pos)),
                Some('\\') => match self.bump_char() {
                    Some('"') => value.push('"'),
                    Some('\\') => value.push('\\'),
                    Some('n') => value.push('\n'),
                    Some('r') => value.push('\r'),
                    Some('t') => value.push('\t'),
                    Some(other) => value.push(other),
                    None => return Err(EvalError::UnexpectedEof.with_position(pos)),
                },
                Some(ch) => value.push(ch),
                None => return Err(EvalError::UnexpectedEof.with_position(pos)),
            }
        }
    }

    fn parse_quote_shorthand(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.bump_char();
        let quoted = self.parse_expr()?;
        Ok(Expr::List(
            vec![Expr::Symbol("quote".into(), pos), quoted],
            pos,
        ))
    }

    fn parse_keyword_shorthand(
        &mut self,
        pos: SourcePos,
        prefix_len: usize,
        keyword: &str,
    ) -> Result<Expr, EvalError> {
        for _ in 0..prefix_len {
            self.bump_char();
        }

        let expr = self.parse_expr()?;
        Ok(Expr::List(
            vec![Expr::Symbol(keyword.into(), pos), expr],
            pos,
        ))
    }

    fn parse_syntax_shorthand(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.bump_char();
        self.bump_char();
        let template = self.parse_expr()?;
        Ok(Expr::List(
            vec![Expr::Symbol("syntax".into(), pos), template],
            pos,
        ))
    }

    fn parse_atom(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        let start = self.pos;
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.bump_char();
        }

        let token = &self.input[start..self.pos];
        if token.is_empty() {
            return Err(EvalError::Syntax {
                message: "expected expression".into(),
            }
            .with_position(pos));
        }

        match token {
            "#t" => Ok(Expr::Boolean(true, pos)),
            "#f" => Ok(Expr::Boolean(false, pos)),
            _ if token.starts_with("#\\") => {
                let literal = &token[2..];
                match parse_char_literal(literal) {
                    Some(ch) => Ok(Expr::Char(ch, pos)),
                    None => Err(EvalError::Syntax {
                        message: "invalid character literal".into(),
                    }
                    .with_position(pos)),
                }
            }
            _ => match Number::parse(token) {
                Some(value) => Ok(Expr::Number(value, pos)),
                None => Ok(Expr::Symbol(token.into(), pos)),
            },
        }
    }

    fn skip_ws_and_comments(&mut self) {
        loop {
            while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
                self.bump_char();
            }

            if self.peek_char() == Some(';') {
                while let Some(ch) = self.bump_char() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos::new(self.line, self.column)
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.pos += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(ch)
    }
}

fn parse_char_literal(token: &str) -> Option<char> {
    match token {
        "space" => Some(' '),
        "newline" => Some('\n'),
        _ => {
            let mut chars = token.chars();
            let ch = chars.next()?;
            if chars.next().is_none() {
                Some(ch)
            } else {
                None
            }
        }
    }
}
