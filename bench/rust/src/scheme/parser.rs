use super::{Ast, AstKind, EvalError, gcd};

pub(crate) struct Parser {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
}

impl Parser {
    pub(crate) fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.chars.len() {
            if self.chars[self.pos].is_whitespace() {
                self.next_char();
            } else if self.chars[self.pos] == ';' {
                while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
                    self.next_char();
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.chars.get(self.pos).copied();
        if let Some(c) = ch {
            self.pos += 1;
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
        ch
    }

    fn parse_expr(&mut self) -> Result<Ast, EvalError> {
        self.skip_whitespace();
        let line = self.line;
        let col = self.col;
        match self.peek() {
            None => Err(EvalError::Parse("unexpected end of input".into())),
            Some('\'') => {
                self.next_char(); // consume quote
                let expr = self.parse_expr()?;
                Ok(Ast {
                    kind: AstKind::List(vec![
                        Ast { kind: AstKind::Symbol("quote".into()), line, col },
                        expr,
                    ]),
                    line,
                    col,
                })
            }
            Some('(') => self.parse_list(line, col),
            Some('"') => self.parse_string(line, col),
            Some('#') => self.parse_hash(line, col),
            _ => self.parse_atom(line, col),
        }
    }

    fn parse_list(&mut self, line: usize, col: usize) -> Result<Ast, EvalError> {
        self.next_char(); // consume '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace();
            match self.peek() {
                None => return Err(EvalError::Parse("unclosed parenthesis".into())),
                Some(')') => {
                    self.next_char();
                    return Ok(Ast { kind: AstKind::List(items), line, col });
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self, line: usize, col: usize) -> Result<Ast, EvalError> {
        self.next_char(); // consume opening '"'
        let mut s = String::new();
        loop {
            match self.next_char() {
                None => return Err(EvalError::Parse("unclosed string".into())),
                Some('"') => return Ok(Ast { kind: AstKind::Str(s), line, col }),
                Some('\\') => match self.next_char() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('\\') => s.push('\\'),
                    Some('"') => s.push('"'),
                    Some(c) => s.push(c),
                    None => return Err(EvalError::Parse("unclosed string escape".into())),
                },
                Some(c) => s.push(c),
            }
        }
    }

    fn parse_hash(&mut self, line: usize, col: usize) -> Result<Ast, EvalError> {
        self.next_char(); // consume '#'
        if let Some('(') = self.peek() {
            // Vector literal #(...)
            self.next_char(); // consume '('
            let mut items = Vec::new();
            loop {
                self.skip_whitespace();
                match self.peek() {
                    None => return Err(EvalError::Parse("unclosed vector literal".into())),
                    Some(')') => {
                        self.next_char();
                        // Represent as (vector item ...) for eval
                        let mut elems = vec![Ast { kind: AstKind::Symbol("vector".into()), line, col }];
                        elems.extend(items);
                        return Ok(Ast { kind: AstKind::List(elems), line, col });
                    }
                    _ => items.push(self.parse_expr()?),
                }
            }
        }
        if let Some('\'') = self.peek() {
            // Syntax quote #'expr -> (syntax expr)
            self.next_char(); // consume '\''
            let expr = self.parse_expr()?;
            return Ok(Ast {
                kind: AstKind::List(vec![
                    Ast { kind: AstKind::Symbol("syntax".into()), line, col },
                    expr,
                ]),
                line,
                col,
            });
        }
        match self.next_char() {
            Some('t') => {
                if self.peek().is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-' && c != '!' && c != '?') {
                    Ok(Ast { kind: AstKind::Boolean(true), line, col })
                } else {
                    Err(EvalError::Parse("invalid boolean literal".into()))
                }
            }
            Some('f') => {
                if self.peek().is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-' && c != '!' && c != '?') {
                    Ok(Ast { kind: AstKind::Boolean(false), line, col })
                } else {
                    Err(EvalError::Parse("invalid boolean literal".into()))
                }
            }
            Some('\\') => {
                // Character literal: #\x, #\space, #\newline, etc.
                match self.next_char() {
                    None => Err(EvalError::Parse("unexpected end of character literal".into())),
                    Some(c) => {
                        // Check for named characters
                        let mut name = String::new();
                        name.push(c);
                        while let Some(nc) = self.peek() {
                            if nc.is_alphabetic() {
                                name.push(nc);
                                self.next_char();
                            } else {
                                break;
                            }
                        }
                        let ch = if name.len() == 1 {
                            name.chars().next().expect("single-char name is non-empty")
                        } else {
                            match name.as_str() {
                                "space" => ' ',
                                "newline" => '\n',
                                "tab" => '\t',
                                _ => return Err(EvalError::Parse(format!("unknown character name: {}", name))),
                            }
                        };
                        Ok(Ast { kind: AstKind::Char(ch), line, col })
                    }
                }
            }
            _ => Err(EvalError::Parse("invalid hash literal".into())),
        }
    }

    fn parse_atom(&mut self, line: usize, col: usize) -> Result<Ast, EvalError> {
        let mut token = String::new();
        while let Some(c) = self.peek() {
            if c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == ';' {
                break;
            }
            token.push(c);
            self.next_char();
        }
        if token.is_empty() {
            return Err(EvalError::Parse("unexpected character".into()));
        }
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Ast { kind: AstKind::Integer(n), line, col });
        }
        // Rational literal: digits/digits (e.g. 1/3, -5/2)
        if let Some(slash) = token.find('/') {
            if let (Ok(n), Ok(d)) = (token[..slash].parse::<i64>(), token[slash+1..].parse::<i64>()) {
                if d != 0 {
                    let sign = if (n < 0) ^ (d < 0) { -1 } else { 1 };
                    let na = n.abs();
                    let da = d.abs();
                    let g = gcd(na, da);
                    let n2 = sign * (na / g);
                    let d2 = da / g;
                    if d2 == 1 {
                        return Ok(Ast { kind: AstKind::Integer(n2), line, col });
                    }
                    return Ok(Ast { kind: AstKind::Rational(n2, d2), line, col });
                }
            }
        }
        // Float literal
        if let Ok(f) = token.parse::<f64>() {
            return Ok(Ast { kind: AstKind::Float(f), line, col });
        }
        Ok(Ast { kind: AstKind::Symbol(token), line, col })
    }

    pub(crate) fn parse_all(&mut self) -> Result<Vec<Ast>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace();
            if self.pos >= self.chars.len() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        if exprs.is_empty() {
            return Err(EvalError::Parse("empty input".into()));
        }
        Ok(exprs)
    }
}
