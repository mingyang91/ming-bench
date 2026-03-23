use crate::scheme::EvalError;

/// An S-expression (parsed but not yet evaluated).
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

pub struct Parser {
    chars: Vec<char>,
    pos: usize,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    pub fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.pos >= self.chars.len() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace_and_comments();
        if self.pos >= self.chars.len() {
            return Err(EvalError::Parse {
                message: "unexpected end of input".into(),
            });
        }

        match self.chars[self.pos] {
            '(' => self.parse_list(),
            '"' => self.parse_string(),
            '#' => self.parse_hash(),
            '\'' => self.parse_quote(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '('
        let mut elems = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.pos >= self.chars.len() {
                return Err(EvalError::Parse {
                    message: "unclosed parenthesis".into(),
                });
            }
            if self.chars[self.pos] == ')' {
                self.pos += 1;
                return Ok(Expr::List(elems));
            }
            elems.push(self.parse_expr()?);
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip opening '"'
        let mut s = String::new();
        while self.pos < self.chars.len() {
            let ch = self.chars[self.pos];
            if ch == '\\' && self.pos + 1 < self.chars.len() {
                self.pos += 1;
                match self.chars[self.pos] {
                    'n' => s.push('\n'),
                    't' => s.push('\t'),
                    '\\' => s.push('\\'),
                    '"' => s.push('"'),
                    other => {
                        s.push('\\');
                        s.push(other);
                    }
                }
                self.pos += 1;
            } else if ch == '"' {
                self.pos += 1;
                return Ok(Expr::Str(s));
            } else {
                s.push(ch);
                self.pos += 1;
            }
        }
        Err(EvalError::Parse {
            message: "unclosed string".into(),
        })
    }

    fn parse_hash(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '#'
        if self.pos >= self.chars.len() {
            return Err(EvalError::Parse {
                message: "unexpected end after #".into(),
            });
        }
        match self.chars[self.pos] {
            't' => {
                self.pos += 1;
                Ok(Expr::Boolean(true))
            }
            'f' => {
                self.pos += 1;
                Ok(Expr::Boolean(false))
            }
            other => Err(EvalError::Parse {
                message: format!("unexpected character after #: {other}"),
            }),
        }
    }

    fn parse_quote(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '\''
        let inner = self.parse_expr()?;
        Ok(Expr::List(vec![Expr::Symbol("quote".into()), inner]))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;
        while self.pos < self.chars.len() && !is_delimiter(self.chars[self.pos]) {
            self.pos += 1;
        }
        let token: String = self.chars[start..self.pos].iter().collect();
        if token.is_empty() {
            return Err(EvalError::Parse {
                message: "empty token".into(),
            });
        }

        // Try integer
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Expr::Integer(n));
        }

        // Otherwise it's a symbol
        Ok(Expr::Symbol(token))
    }

    fn skip_whitespace_and_comments(&mut self) {
        while self.pos < self.chars.len() {
            if self.chars[self.pos].is_whitespace() {
                self.pos += 1;
            } else if self.chars[self.pos] == ';' {
                // Skip to end of line
                while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || ch == '(' || ch == ')' || ch == '"' || ch == ';'
}
