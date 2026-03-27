use super::{is_integer_token, EvalError, Expr, Position};
use crate::scheme::text::parse_char_literal;

pub(super) fn parse_program(input: &str) -> Result<Vec<Expr>, EvalError> {
    Parser::new(input).parse_program()
}

struct Parser<'a> {
    input: &'a str,
    pos: usize,
    line: usize,
    col: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            line: 1,
            col: 1,
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
            Err(EvalError::EmptyProgram)
        } else {
            Ok(exprs)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let pos = self.current_position();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some('\'') => {
                self.bump_char();
                let quoted = self.parse_expr()?;
                Ok(Expr::List {
                    items: vec![
                        Expr::Symbol {
                            name: "quote".to_string(),
                            pos,
                        },
                        quoted,
                    ],
                    pos,
                })
            }
            Some('"') => self.parse_string(),
            Some(')') => Err(EvalError::ParseError {
                message: "unexpected ')'".to_string(),
            }
            .with_position(pos.line, pos.col)),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::UnexpectedEof.with_position(pos.line, pos.col)),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_position();
        self.bump_char();
        let mut items = Vec::new();

        loop {
            self.skip_ignored();
            match self.peek_char() {
                Some(')') => {
                    self.bump_char();
                    return Ok(Expr::List { items, pos });
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(self.error_here(EvalError::UnexpectedEof)),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_position();
        self.bump_char();
        let mut value = String::new();

        while let Some(ch) = self.bump_char() {
            match ch {
                '"' => return Ok(Expr::String { value, pos }),
                '\\' => {
                    let Some(escaped) = self.bump_char() else {
                        return Err(self.error_here(EvalError::UnexpectedEof));
                    };
                    let ch = match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => other,
                    };
                    value.push(ch);
                }
                other => value.push(other),
            }
        }

        Err(self.error_here(EvalError::UnexpectedEof))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;
        let pos = self.current_position();
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || ch == '(' || ch == ')' || ch == ';' {
                break;
            }
            self.bump_char();
        }

        let token = &self.input[start..self.pos];
        if token.is_empty() {
            return Err(self.error_at(
                pos,
                EvalError::ParseError {
                    message: "expected expression".to_string(),
                },
            ));
        }

        if token == "#t" {
            return Ok(Expr::Bool { value: true, pos });
        }
        if token == "#f" {
            return Ok(Expr::Bool { value: false, pos });
        }
        if let Some(literal) = token.strip_prefix("#\\") {
            let Some(value) = parse_char_literal(literal) else {
                return Err(self.error_at(
                    pos,
                    EvalError::ParseError {
                        message: format!("invalid character literal: {token}"),
                    },
                ));
            };
            return Ok(Expr::Char { value, pos });
        }
        if is_integer_token(token) {
            return token
                .parse::<i64>()
                .map(|value| Expr::Int { value, pos })
                .map_err(|_| {
                    self.error_at(
                        pos,
                        EvalError::ParseError {
                            message: format!("invalid integer literal: {token}"),
                        },
                    )
                });
        }

        Ok(Expr::Symbol {
            name: token.to_string(),
            pos,
        })
    }

    fn skip_ignored(&mut self) {
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

    fn bump_char(&mut self) -> Option<char> {
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

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn current_position(&self) -> Position {
        Position {
            line: self.line,
            col: self.col,
        }
    }

    fn error_here(&self, error: EvalError) -> EvalError {
        self.error_at(self.current_position(), error)
    }

    fn error_at(&self, pos: Position, error: EvalError) -> EvalError {
        error.with_position(pos.line, pos.col)
    }
}
