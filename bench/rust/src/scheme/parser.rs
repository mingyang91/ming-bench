use crate::scheme::error::{EvalError, EvalResult};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct Span {
    pub(crate) line: usize,
    pub(crate) col: usize,
}

#[derive(Clone, Debug)]
pub(crate) struct Expr {
    pub(crate) kind: ExprKind,
    pub(crate) span: Span,
}

#[derive(Clone, Debug)]
pub(crate) enum ExprKind {
    Bool(bool),
    Int(i64),
    String(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
    Quote(Box<Expr>),
}

pub(crate) fn parse_program(input: &str) -> EvalResult<Vec<Expr>> {
    Parser::new(input).parse_program()
}

struct Parser {
    chars: Vec<char>,
    index: usize,
    line: usize,
    col: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            index: 0,
            line: 1,
            col: 1,
        }
    }

    fn parse_program(&mut self) -> EvalResult<Vec<Expr>> {
        let mut expressions = Vec::new();
        self.skip_ignored();
        while self.peek().is_some() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> EvalResult<Expr> {
        self.skip_ignored();
        let span = self.span();
        match self.peek() {
            Some('(') => self.parse_list(),
            Some('\'') => {
                self.advance();
                let quoted = self.parse_expr()?;
                Ok(Expr {
                    kind: ExprKind::Quote(Box::new(quoted)),
                    span,
                })
            }
            Some('"') => self.parse_string(),
            Some('#') => self.parse_hash_literal(),
            Some(')') => Err(self.error_here("unexpected ')'")),
            Some(_) => Ok(self.parse_atom()),
            None => Err(self.error_here("unexpected end of input")),
        }
    }

    fn parse_list(&mut self) -> EvalResult<Expr> {
        let span = self.span();
        self.expect('(')?;
        let mut items = Vec::new();
        loop {
            self.skip_ignored();
            match self.peek() {
                Some(')') => {
                    self.advance();
                    break;
                }
                Some(_) => items.push(self.parse_expr()?),
                None => {
                    return Err(EvalError::with_position(
                        "unterminated list",
                        span.line,
                        span.col,
                    ))
                }
            }
        }
        Ok(Expr {
            kind: ExprKind::List(items),
            span,
        })
    }

    fn parse_string(&mut self) -> EvalResult<Expr> {
        let span = self.span();
        self.expect('"')?;
        let mut value = String::new();
        while let Some(ch) = self.peek() {
            match ch {
                '"' => {
                    self.advance();
                    return Ok(Expr {
                        kind: ExprKind::String(value),
                        span,
                    });
                }
                '\\' => {
                    self.advance();
                    let escaped = match self.peek() {
                        Some('"') => '"',
                        Some('\\') => '\\',
                        Some('n') => '\n',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some(other) => other,
                        None => return Err(self.error_here("unterminated string escape")),
                    };
                    self.advance();
                    value.push(escaped);
                }
                other => {
                    self.advance();
                    value.push(other);
                }
            }
        }
        Err(EvalError::with_position(
            "unterminated string",
            span.line,
            span.col,
        ))
    }

    fn parse_hash_literal(&mut self) -> EvalResult<Expr> {
        let span = self.span();
        self.expect('#')?;
        match self.peek() {
            Some('t') => {
                self.advance();
                Ok(Expr {
                    kind: ExprKind::Bool(true),
                    span,
                })
            }
            Some('f') => {
                self.advance();
                Ok(Expr {
                    kind: ExprKind::Bool(false),
                    span,
                })
            }
            Some('\\') => {
                self.advance();
                let token = self.read_token();
                let character = match token.as_str() {
                    "space" => ' ',
                    "newline" => '\n',
                    _ => {
                        let mut chars = token.chars();
                        match (chars.next(), chars.next()) {
                            (Some(ch), None) => ch,
                            _ => {
                                return Err(EvalError::with_position(
                                    "invalid character literal",
                                    span.line,
                                    span.col,
                                ));
                            }
                        }
                    }
                };
                Ok(Expr {
                    kind: ExprKind::Char(character),
                    span,
                })
            }
            _ => Err(EvalError::with_position(
                "unknown # literal",
                span.line,
                span.col,
            )),
        }
    }

    fn parse_atom(&mut self) -> Expr {
        let span = self.span();
        let token = self.read_token();
        if let Some(value) = parse_integer(&token) {
            return Expr {
                kind: ExprKind::Int(value),
                span,
            };
        }
        Expr {
            kind: ExprKind::Symbol(token),
            span,
        }
    }

    fn read_token(&mut self) -> String {
        let mut token = String::new();
        while let Some(ch) = self.peek() {
            if is_delimiter(ch) {
                break;
            }
            self.advance();
            token.push(ch);
        }
        token
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek(), Some(ch) if ch.is_whitespace()) {
                self.advance();
            }

            if self.peek() == Some(';') {
                while let Some(ch) = self.peek() {
                    self.advance();
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn span(&self) -> Span {
        Span {
            line: self.line,
            col: self.col,
        }
    }

    fn expect(&mut self, expected: char) -> EvalResult<()> {
        match self.peek() {
            Some(ch) if ch == expected => {
                self.advance();
                Ok(())
            }
            _ => Err(self.error_here(format!("expected '{expected}'"))),
        }
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.chars.get(self.index).copied()?;
        self.index += 1;
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }

    fn error_here(&self, message: impl Into<String>) -> EvalError {
        EvalError::with_position(message, self.line, self.col)
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '\'' | ';')
}

fn parse_integer(token: &str) -> Option<i64> {
    let digits = token
        .strip_prefix('-')
        .or_else(|| token.strip_prefix('+'))
        .unwrap_or(token);
    if digits.is_empty() || !digits.chars().all(|ch| ch.is_ascii_digit()) {
        return None;
    }
    token.parse().ok()
}
