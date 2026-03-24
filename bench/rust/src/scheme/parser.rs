use crate::scheme::error::{EvalError, Span};
use crate::scheme::{make_rational, Spanned, Value};
use std::cell::RefCell;
use std::rc::Rc;

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

    fn skip_whitespace(&mut self) {
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

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn parse_expr(&mut self) -> Result<Spanned, EvalError> {
        self.skip_whitespace();
        let span = self.current_span();
        match self.peek() {
            None => Err(EvalError::Parse("unexpected end of input".into(), span)),
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string(),
            Some('#') => self.parse_hash(),
            Some('\'') => {
                self.advance();
                let inner = self.parse_expr()?;
                Ok(Spanned::new(
                    Value::List(vec![
                        Spanned::new(Value::Symbol("quote".into()), span),
                        inner,
                    ]),
                    span,
                ))
            }
            Some(_) => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Spanned, EvalError> {
        let span = self.current_span();
        self.advance(); // skip '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace();
            match self.peek() {
                None => return Err(EvalError::Parse("unterminated list".into(), span)),
                Some(')') => {
                    self.advance();
                    return Ok(Spanned::new(Value::List(items), span));
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Spanned, EvalError> {
        let span = self.current_span();
        self.advance();
        let mut s = String::new();
        loop {
            match self.chars.get(self.pos) {
                None => return Err(EvalError::Parse("unterminated string".into(), span)),
                Some('\\') => {
                    self.advance();
                    match self.chars.get(self.pos) {
                        Some('n') => { s.push('\n'); self.advance(); }
                        Some('t') => { s.push('\t'); self.advance(); }
                        Some('\\') => { s.push('\\'); self.advance(); }
                        Some('"') => { s.push('"'); self.advance(); }
                        Some(c) => { s.push(*c); self.advance(); }
                        None => return Err(EvalError::Parse("unterminated escape".into(), span)),
                    }
                }
                Some('"') => {
                    self.advance();
                    return Ok(Spanned::new(Value::Str(s), span));
                }
                Some(c) => {
                    s.push(*c);
                    self.advance();
                }
            }
        }
    }

    fn parse_hash(&mut self) -> Result<Spanned, EvalError> {
        let span = self.current_span();
        self.advance();
        match self.peek() {
            Some('t') => {
                self.advance();
                if self.peek().is_none_or(is_delimiter) {
                    Ok(Spanned::new(Value::Boolean(true), span))
                } else {
                    Err(EvalError::Parse("invalid # literal".into(), span))
                }
            }
            Some('f') => {
                self.advance();
                if self.peek().is_none_or(is_delimiter) {
                    Ok(Spanned::new(Value::Boolean(false), span))
                } else {
                    Err(EvalError::Parse("invalid # literal".into(), span))
                }
            }
            Some('(') => {
                // #( ... ) vector literal
                self.advance(); // skip '('
                let mut items = Vec::new();
                loop {
                    self.skip_whitespace();
                    match self.peek() {
                        None => return Err(EvalError::Parse("unterminated vector literal".into(), span)),
                        Some(')') => {
                            self.advance();
                            let vals: Vec<Value> = items.iter().map(|s: &Spanned| s.val.clone()).collect();
                            return Ok(Spanned::new(Value::Vector(Rc::new(RefCell::new(vals))), span));
                        }
                        _ => items.push(self.parse_expr()?),
                    }
                }
            }
            Some('\'') => {
                self.advance(); // skip '
                let inner = self.parse_expr()?;
                Ok(Spanned::new(
                    Value::List(vec![
                        Spanned::new(Value::Symbol("syntax".into()), span),
                        inner,
                    ]),
                    span,
                ))
            }
            Some('\\') => {
                self.advance(); // skip '\'
                // Named characters
                let start = self.pos;
                while self.pos < self.chars.len() && !is_delimiter(self.chars[self.pos]) {
                    self.advance();
                }
                let name: String = self.chars[start..self.pos].iter().collect();
                let c = match name.as_str() {
                    "space" => ' ',
                    "newline" => '\n',
                    "tab" => '\t',
                    s if s.chars().count() == 1 => s.chars().next().expect("single-char string has a first char"),
                    _ => return Err(EvalError::Parse(format!("unknown character name: {name}"), span)),
                };
                Ok(Spanned::new(Value::Char(c), span))
            }
            _ => Err(EvalError::Parse("invalid # literal".into(), span)),
        }
    }

    fn parse_atom(&mut self) -> Result<Spanned, EvalError> {
        let span = self.current_span();
        let start = self.pos;
        while self.pos < self.chars.len() && !is_delimiter(self.chars[self.pos]) {
            self.advance();
        }
        let token: String = self.chars[start..self.pos].iter().collect();
        if let Ok(n) = token.parse::<i64>() {
            Ok(Spanned::new(Value::Integer(n), span))
        } else if let Some((num, den)) = token.split_once('/') {
            if let (Ok(n), Ok(d)) = (num.parse::<i64>(), den.parse::<i64>()) {
                if d == 0 {
                    return Err(EvalError::DivisionByZero(span));
                }
                Ok(Spanned::new(make_rational(n, d), span))
            } else {
                Ok(Spanned::new(Value::Symbol(token), span))
            }
        } else if token.contains('.') {
            if let Ok(f) = token.parse::<f64>() {
                Ok(Spanned::new(Value::Float(f), span))
            } else {
                Ok(Spanned::new(Value::Symbol(token), span))
            }
        } else {
            Ok(Spanned::new(Value::Symbol(token), span))
        }
    }

    pub(crate) fn parse_all(&mut self) -> Result<Vec<Spanned>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace();
            if self.pos >= self.chars.len() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }
}

fn is_delimiter(c: char) -> bool {
    c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == ';'
}

/// Parse a parameter list, returning (fixed_params, optional_rest_param).
/// Handles dot notation: `(x y . rest)` → (["x", "y"], Some("rest"))
pub(crate) fn parse_params(values: &[Spanned], context: &str, span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    // Look for dot
    let dot_pos = values.iter().position(|v| matches!(&v.val, Value::Symbol(s) if s == "."));
    if let Some(pos) = dot_pos {
        if pos + 1 != values.len() - 1 {
            return Err(EvalError::Parse(format!("{context}: malformed dot parameter list"), span));
        }
        let fixed: Result<Vec<String>, _> = values[..pos]
            .iter()
            .map(|v| match &v.val {
                Value::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type(format!("{context}: expected symbol in parameter list"), span)),
            })
            .collect();
        let rest = match &values[pos + 1].val {
            Value::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Type(format!("{context}: expected symbol after dot"), span)),
        };
        Ok((fixed?, Some(rest)))
    } else {
        let params: Result<Vec<String>, _> = values
            .iter()
            .map(|v| match &v.val {
                Value::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type(format!("{context}: expected symbol in parameter list"), span)),
            })
            .collect();
        Ok((params?, None))
    }
}
