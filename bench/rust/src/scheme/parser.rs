use super::ast::{Expr, ExprKind};
use super::error::{EvalError, Position};

pub fn parse_program(input: &str) -> Result<Vec<Expr>, EvalError> {
    Parser::new(input).parse_program()
}

struct Parser {
    chars: Vec<char>,
    index: usize,
    line: usize,
    column: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            index: 0,
            line: 1,
            column: 1,
        }
    }

    fn parse_program(mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();
        while !self.is_at_end() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let pos = self.position();
        match self.peek() {
            Some('(') => self.parse_list(pos),
            Some('"') => self.parse_string(pos),
            Some('\'') => self.parse_quote(pos),
            Some(')') => Err(EvalError::syntax("unexpected ')'", pos)),
            Some(_) => self.parse_atom(pos),
            None => Err(EvalError::syntax("unexpected end of input", pos)),
        }
    }

    fn parse_quote(&mut self, pos: Position) -> Result<Expr, EvalError> {
        let _ = self.advance();
        let quoted = self.parse_expr()?;
        Ok(Expr::list(vec![Expr::symbol("quote", pos), quoted], pos))
    }

    fn parse_list(&mut self, pos: Position) -> Result<Expr, EvalError> {
        let _ = self.advance();
        let mut elements = Vec::new();
        self.skip_ignored();

        while !self.is_at_end() && self.peek() != Some(')') {
            elements.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if self.is_at_end() {
            return Err(EvalError::syntax("unterminated list", pos));
        }

        let _ = self.advance();
        Ok(Expr::list(elements, pos))
    }

    fn parse_string(&mut self, pos: Position) -> Result<Expr, EvalError> {
        let _ = self.advance();
        let mut buffer = String::new();

        while let Some(ch) = self.advance() {
            if ch == '"' {
                return Ok(Expr::new(ExprKind::String(buffer), pos));
            }

            if ch == '\\' {
                let Some(escaped) = self.advance() else {
                    return Err(EvalError::syntax("unterminated string literal", pos));
                };
                match escaped {
                    'n' => buffer.push('\n'),
                    'r' => buffer.push('\r'),
                    't' => buffer.push('\t'),
                    '"' => buffer.push('"'),
                    '\\' => buffer.push('\\'),
                    other => buffer.push(other),
                }
            } else {
                buffer.push(ch);
            }
        }

        Err(EvalError::syntax("unterminated string literal", pos))
    }

    fn parse_atom(&mut self, pos: Position) -> Result<Expr, EvalError> {
        let token = self.read_token();
        if token.is_empty() {
            return Err(EvalError::syntax("expected expression", pos));
        }

        match token.as_str() {
            "#t" => Ok(Expr::new(ExprKind::Bool(true), pos)),
            "#f" => Ok(Expr::new(ExprKind::Bool(false), pos)),
            _ if token.starts_with("#\\") => Self::parse_char_literal(&token, pos),
            _ => self.parse_number_or_symbol(token, pos),
        }
    }

    fn parse_number_or_symbol(&self, token: String, pos: Position) -> Result<Expr, EvalError> {
        if Self::is_integer_token(&token) {
            return token
                .parse::<i64>()
                .map(|value| Expr::new(ExprKind::Int(value), pos))
                .map_err(|_| EvalError::syntax(format!("invalid integer literal: {token}"), pos));
        }
        Ok(Expr::new(ExprKind::Symbol(token), pos))
    }

    fn parse_char_literal(token: &str, pos: Position) -> Result<Expr, EvalError> {
        let Some(value) = token.strip_prefix("#\\") else {
            return Err(EvalError::syntax("invalid character literal", pos));
        };

        let ch = match value {
            "space" => ' ',
            "newline" => '\n',
            _ => {
                let mut chars = value.chars();
                let Some(ch) = chars.next() else {
                    return Err(EvalError::syntax("invalid character literal", pos));
                };
                if chars.next().is_some() {
                    return Err(EvalError::syntax("invalid character literal", pos));
                }
                ch
            }
        };

        Ok(Expr::new(ExprKind::Char(ch), pos))
    }

    fn is_integer_token(token: &str) -> bool {
        if token.is_empty() {
            return false;
        }

        let mut chars = token.chars();
        let Some(first) = chars.next() else {
            return false;
        };

        if matches!(first, '+' | '-') {
            return !chars.as_str().is_empty() && chars.all(|ch| ch.is_ascii_digit());
        }

        first.is_ascii_digit() && chars.all(|ch| ch.is_ascii_digit())
    }

    fn read_token(&mut self) -> String {
        let mut token = String::new();
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            token.push(ch);
            let _ = self.advance();
        }
        token
    }

    fn skip_ignored(&mut self) {
        loop {
            let Some(ch) = self.peek() else {
                return;
            };
            if ch.is_whitespace() {
                let _ = self.advance();
                continue;
            }
            if ch == ';' {
                self.skip_comment();
                continue;
            }
            return;
        }
    }

    fn skip_comment(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == '\n' {
                return;
            }
            let _ = self.advance();
        }
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.chars.len()
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.index += 1;
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(ch)
    }

    fn position(&self) -> Position {
        Position::new(self.line, self.column)
    }
}
