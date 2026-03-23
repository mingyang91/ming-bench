use crate::scheme::error::{ErrorKind, EvalError, Span};

/// The kind of an S-expression node.
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

/// A parsed S-expression with source position.
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

pub struct Parser {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
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

    fn current_span(&self) -> Span {
        Span { line: self.line, col: self.col }
    }

    fn advance(&mut self) {
        if self.pos < self.chars.len() {
            if self.chars[self.pos] == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn err(&self, message: impl Into<String>) -> EvalError {
        EvalError::new(
            ErrorKind::Parse { message: message.into() },
            self.current_span(),
        )
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace_and_comments();
        if self.pos >= self.chars.len() {
            return Err(self.err("unexpected end of input"));
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
        let span = self.current_span();
        self.advance(); // skip '('
        let mut elems = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.pos >= self.chars.len() {
                return Err(self.err("unclosed parenthesis"));
            }
            if self.chars[self.pos] == ')' {
                self.advance();
                return Ok(Expr { kind: ExprKind::List(elems), span });
            }
            elems.push(self.parse_expr()?);
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        let span = self.current_span();
        self.advance(); // skip opening '"'
        let mut s = String::new();
        while self.pos < self.chars.len() {
            let ch = self.chars[self.pos];
            if ch == '\\' && self.pos + 1 < self.chars.len() {
                self.advance();
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
                self.advance();
            } else if ch == '"' {
                self.advance();
                return Ok(Expr { kind: ExprKind::Str(s), span });
            } else {
                s.push(ch);
                self.advance();
            }
        }
        Err(self.err("unclosed string"))
    }

    fn parse_hash(&mut self) -> Result<Expr, EvalError> {
        let span = self.current_span();
        self.advance(); // skip '#'
        if self.pos >= self.chars.len() {
            return Err(self.err("unexpected end after #"));
        }
        match self.chars[self.pos] {
            't' => {
                self.advance();
                Ok(Expr { kind: ExprKind::Boolean(true), span })
            }
            'f' => {
                self.advance();
                Ok(Expr { kind: ExprKind::Boolean(false), span })
            }
            '\\' => {
                self.advance(); // skip '\'
                if self.pos >= self.chars.len() {
                    return Err(self.err("unexpected end after #\\"));
                }
                // Check for named characters (e.g., #\space, #\newline)
                let start = self.pos;
                if self.chars[self.pos].is_alphabetic() {
                    while self.pos < self.chars.len() && !is_delimiter(self.chars[self.pos]) {
                        self.advance();
                    }
                    let name: String = self.chars[start..self.pos].iter().collect();
                    if name.len() == 1 {
                        return Ok(Expr { kind: ExprKind::Char(name.chars().next().expect("single char")), span });
                    }
                    match name.as_str() {
                        "space" => Ok(Expr { kind: ExprKind::Char(' '), span }),
                        "newline" => Ok(Expr { kind: ExprKind::Char('\n'), span }),
                        "tab" => Ok(Expr { kind: ExprKind::Char('\t'), span }),
                        _ => Err(EvalError::new(
                            ErrorKind::Parse { message: format!("unknown character name: {name}") },
                            span,
                        )),
                    }
                } else {
                    let ch = self.chars[self.pos];
                    self.advance();
                    Ok(Expr { kind: ExprKind::Char(ch), span })
                }
            }
            '(' => {
                // #(...) vector literal — parse as (vector ...)
                self.advance(); // skip '('
                let mut elems = Vec::new();
                loop {
                    self.skip_whitespace_and_comments();
                    if self.pos >= self.chars.len() {
                        return Err(self.err("unclosed vector literal"));
                    }
                    if self.chars[self.pos] == ')' {
                        self.advance();
                        let mut list_elems = vec![Expr { kind: ExprKind::Symbol("vector".into()), span }];
                        list_elems.extend(elems);
                        return Ok(Expr { kind: ExprKind::List(list_elems), span });
                    }
                    elems.push(self.parse_expr()?);
                }
            }
            other => Err(EvalError::new(
                ErrorKind::Parse {
                    message: format!("unexpected character after #: {other}"),
                },
                span,
            )),
        }
    }

    fn parse_quote(&mut self) -> Result<Expr, EvalError> {
        let span = self.current_span();
        self.advance(); // skip '\''
        let inner = self.parse_expr()?;
        Ok(Expr {
            kind: ExprKind::List(vec![
                Expr { kind: ExprKind::Symbol("quote".into()), span },
                inner,
            ]),
            span,
        })
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let span = self.current_span();
        let start = self.pos;
        while self.pos < self.chars.len() && !is_delimiter(self.chars[self.pos]) {
            self.advance();
        }
        let token: String = self.chars[start..self.pos].iter().collect();
        if token.is_empty() {
            return Err(self.err("empty token"));
        }

        // Try integer
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Expr { kind: ExprKind::Integer(n), span });
        }

        // Try rational literal (e.g. 1/3, -5/2)
        if let Some(slash_pos) = token.find('/') {
            let num_str = &token[..slash_pos];
            let den_str = &token[slash_pos + 1..];
            if let (Ok(num), Ok(den)) = (num_str.parse::<i64>(), den_str.parse::<i64>()) {
                if den != 0 {
                    let g = gcd(num.unsigned_abs(), den.unsigned_abs()) as i64;
                    let (mut n, mut d) = (num / g, den / g);
                    if d < 0 {
                        n = -n;
                        d = -d;
                    }
                    if d == 1 {
                        return Ok(Expr { kind: ExprKind::Integer(n), span });
                    }
                    return Ok(Expr { kind: ExprKind::Rational(n, d), span });
                }
            }
        }

        // Try float
        if let Ok(f) = token.parse::<f64>() {
            return Ok(Expr { kind: ExprKind::Float(f), span });
        }

        // Otherwise it's a symbol
        Ok(Expr { kind: ExprKind::Symbol(token), span })
    }

    fn skip_whitespace_and_comments(&mut self) {
        while self.pos < self.chars.len() {
            if self.chars[self.pos].is_whitespace() {
                self.advance();
            } else if self.chars[self.pos] == ';' {
                while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
                    self.advance();
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

/// Greatest common divisor (Euclidean algorithm).
pub fn gcd(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}
