use super::error::EvalError;
use super::expr::Expr;
use super::position::SourcePos;

pub(crate) struct Parser<'a> {
    input: &'a str,
    index: usize,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            input,
            index: 0,
            line: 1,
            column: 1,
        }
    }

    pub(crate) fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();
        while !self.is_at_end() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }
        if expressions.is_empty() {
            return Err(EvalError::syntax(self.current_pos(), "empty program"));
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let pos = self.current_pos();
        if self.is_at_end() {
            return Err(EvalError::syntax(pos, "unexpected end of input"));
        }

        match self.peek() {
            '(' => self.parse_list(),
            '"' => self.parse_string(),
            '#' => self.parse_hash_literal(),
            '\'' => self.parse_quote_shorthand(),
            _ => self.parse_atom(),
        }
    }

    fn parse_quote_shorthand(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.advance();
        let quoted = self.parse_expr()?;
        Ok(Expr::List(
            vec![Expr::Symbol("quote".into(), pos), quoted],
            pos,
        ))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.advance();

        let mut elements = Vec::new();
        self.skip_ignored();
        while !self.is_at_end() && self.peek() != ')' {
            elements.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if self.is_at_end() {
            return Err(EvalError::syntax(pos, "unterminated list"));
        }

        self.advance();
        Ok(Expr::List(elements, pos))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.advance();

        let mut builder = String::new();
        while !self.is_at_end() {
            let ch = self.advance();
            if ch == '"' {
                return Ok(Expr::String(builder, pos));
            }
            if ch == '\\' {
                if self.is_at_end() {
                    return Err(EvalError::syntax(pos, "unterminated string"));
                }
                let escaped = self.advance();
                match escaped {
                    '"' | '\\' => builder.push(escaped),
                    'n' => builder.push('\n'),
                    'r' => builder.push('\r'),
                    't' => builder.push('\t'),
                    _ => {
                        return Err(EvalError::syntax(
                            self.current_pos(),
                            format!("unsupported escape sequence \\{escaped}"),
                        ))
                    }
                }
            } else {
                builder.push(ch);
            }
        }

        Err(EvalError::syntax(pos, "unterminated string"))
    }

    fn parse_hash_literal(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        if self.matches_literal("#t") {
            return Ok(Expr::Boolean(true, pos));
        }
        if self.matches_literal("#f") {
            return Ok(Expr::Boolean(false, pos));
        }
        if self.starts_with("#\\") {
            return self.parse_character();
        }
        Err(EvalError::syntax(pos, "invalid hash literal"))
    }

    fn parse_character(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.advance();
        self.advance();

        let mut token = String::new();
        while !self.is_at_end() && !self.is_delimiter(self.peek()) {
            token.push(self.advance());
        }

        if token.is_empty() {
            return Err(EvalError::syntax(pos, "invalid character literal"));
        }

        let value = match token.as_str() {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            _ => {
                let mut chars = token.chars();
                let Some(ch) = chars.next() else {
                    return Err(EvalError::syntax(pos, "invalid character literal"));
                };
                if chars.next().is_some() {
                    return Err(EvalError::syntax(pos, "invalid character literal"));
                }
                ch
            }
        };

        Ok(Expr::Character(value, pos))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        let mut builder = String::new();
        while !self.is_at_end() && !self.is_delimiter(self.peek()) {
            builder.push(self.advance());
        }

        if builder.is_empty() {
            return Err(EvalError::syntax(pos, "unexpected token"));
        }

        if self.is_integer_token(&builder) {
            return builder
                .parse::<i64>()
                .map(|value| Expr::Integer(value, pos))
                .map_err(|_| EvalError::syntax(pos, format!("invalid integer literal: {builder}")));
        }

        Ok(Expr::Symbol(builder, pos))
    }

    fn matches_literal(&mut self, literal: &str) -> bool {
        if !self.starts_with(literal) {
            return false;
        }

        let end = self.index + literal.len();
        if end < self.input.len() && !self.is_delimiter(self.input.as_bytes()[end] as char) {
            return false;
        }

        for _ in 0..literal.len() {
            self.advance();
        }
        true
    }

    fn starts_with(&self, literal: &str) -> bool {
        self.input[self.index..].starts_with(literal)
    }

    fn is_integer_token(&self, token: &str) -> bool {
        let mut chars = token.chars();
        let Some(first) = chars.next() else {
            return false;
        };

        if first == '+' || first == '-' {
            return !token[1..].is_empty() && token[1..].chars().all(|ch| ch.is_ascii_digit());
        }

        first.is_ascii_digit() && chars.all(|ch| ch.is_ascii_digit())
    }

    fn skip_ignored(&mut self) {
        while !self.is_at_end() {
            let ch = self.peek();
            if ch.is_ascii_whitespace() {
                self.advance();
                continue;
            }
            if ch == ';' {
                while !self.is_at_end() && self.peek() != '\n' {
                    self.advance();
                }
                continue;
            }
            break;
        }
    }

    fn is_delimiter(&self, ch: char) -> bool {
        ch.is_ascii_whitespace() || matches!(ch, '(' | ')' | ';')
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.input.len()
    }

    fn peek(&self) -> char {
        self.input.as_bytes()[self.index] as char
    }

    fn advance(&mut self) -> char {
        let ch = self.input.as_bytes()[self.index] as char;
        self.index += 1;
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        ch
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos::new(self.line, self.column)
    }
}
