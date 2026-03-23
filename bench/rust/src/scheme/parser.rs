use crate::scheme::error::EvalError;
use crate::scheme::Expr;

pub(crate) struct Parser<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        while self.pos < self.input.len() {
            let ch = self.input[self.pos];
            if ch.is_ascii_whitespace() {
                self.pos += 1;
            } else if ch == b';' {
                while self.pos < self.input.len() && self.input[self.pos] != b'\n' {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.pos < self.input.len() {
            Some(self.input[self.pos])
        } else {
            None
        }
    }

    pub(crate) fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace_and_comments();
        match self.peek() {
            None => Err(EvalError::Parse {
                message: "unexpected end of input".to_string(),
            }),
            Some(b'(') => self.parse_list(),
            Some(b'"') => self.parse_string(),
            Some(b'#') => self.parse_hash(),
            Some(b'\'') => self.parse_quote_shorthand(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            match self.peek() {
                None => {
                    return Err(EvalError::Parse {
                        message: "unterminated list".to_string(),
                    })
                }
                Some(b')') => {
                    self.pos += 1;
                    return Ok(Expr::List(items));
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip opening '"'
        let mut s = String::new();
        loop {
            if self.pos >= self.input.len() {
                return Err(EvalError::Parse {
                    message: "unterminated string".to_string(),
                });
            }
            let ch = self.input[self.pos];
            if ch == b'"' {
                self.pos += 1;
                return Ok(Expr::SchemeString(s));
            }
            if ch == b'\\' {
                self.pos += 1;
                if self.pos >= self.input.len() {
                    return Err(EvalError::Parse {
                        message: "unterminated escape in string".to_string(),
                    });
                }
                let escaped = self.input[self.pos];
                match escaped {
                    b'n' => s.push('\n'),
                    b't' => s.push('\t'),
                    b'\\' => s.push('\\'),
                    b'"' => s.push('"'),
                    _ => {
                        s.push('\\');
                        s.push(escaped as char);
                    }
                }
            } else {
                s.push(ch as char);
            }
            self.pos += 1;
        }
    }

    fn parse_hash(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '#'
        match self.peek() {
            Some(b't') => {
                self.pos += 1;
                Ok(Expr::Boolean(true))
            }
            Some(b'f') => {
                self.pos += 1;
                Ok(Expr::Boolean(false))
            }
            _ => Err(EvalError::Parse {
                message: "unexpected character after #".to_string(),
            }),
        }
    }

    fn parse_quote_shorthand(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '\''
        let inner = self.parse_expr()?;
        Ok(Expr::List(vec![Expr::Symbol("quote".to_string()), inner]))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;
        while self.pos < self.input.len() {
            let ch = self.input[self.pos];
            if ch.is_ascii_whitespace() || ch == b'(' || ch == b')' || ch == b'"' || ch == b';' {
                break;
            }
            self.pos += 1;
        }
        if self.pos == start {
            return Err(EvalError::Parse {
                message: "unexpected character".to_string(),
            });
        }
        let token = std::str::from_utf8(&self.input[start..self.pos]).expect("valid utf8 slice");
        if let Ok(n) = token.parse::<i64>() {
            Ok(Expr::Integer(n))
        } else {
            Ok(Expr::Symbol(token.to_string()))
        }
    }

    pub(crate) fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.pos >= self.input.len() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }
}
