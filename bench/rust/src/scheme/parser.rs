use crate::scheme::error::{EvalError, Span};
use crate::scheme::{make_rational, Spanned, Value};
use std::cell::RefCell;
use std::rc::Rc;

fn build_dotted_pair(items: Vec<Spanned>, tail: Value) -> Value {
    let mut result = tail;
    for item in items.into_iter().rev() {
        result = Value::Pair(Rc::new(RefCell::new((item.val, result))));
    }
    result
}

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
            Some('`') => {
                self.advance();
                let inner = self.parse_expr()?;
                Ok(Spanned::new(
                    Value::List(vec![
                        Spanned::new(Value::Symbol("quasiquote".into()), span),
                        inner,
                    ]),
                    span,
                ))
            }
            Some(',') => {
                self.advance();
                if self.peek() == Some('@') {
                    self.advance();
                    let inner = self.parse_expr()?;
                    Ok(Spanned::new(
                        Value::List(vec![
                            Spanned::new(Value::Symbol("unquote-splicing".into()), span),
                            inner,
                        ]),
                        span,
                    ))
                } else {
                    let inner = self.parse_expr()?;
                    Ok(Spanned::new(
                        Value::List(vec![
                            Spanned::new(Value::Symbol("unquote".into()), span),
                            inner,
                        ]),
                        span,
                    ))
                }
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
                Some('.') => {
                    // Check if this is a dot followed by a delimiter (dotted pair syntax)
                    // vs a symbol starting with '.' (like "...")
                    let next_pos = self.pos + 1;
                    let is_dot_notation = next_pos >= self.chars.len()
                        || is_delimiter(self.chars[next_pos]);
                    if is_dot_notation && !items.is_empty() {
                        self.advance(); // skip '.'
                        self.skip_whitespace();
                        let tail = self.parse_expr()?;
                        self.skip_whitespace();
                        if self.peek() != Some(')') {
                            return Err(EvalError::Parse("expected ) after dotted pair tail".into(), span));
                        }
                        self.advance();
                        let result = build_dotted_pair(items, tail.val);
                        return Ok(Spanned::new(result, span));
                    } else {
                        items.push(self.parse_expr()?);
                    }
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
    let params: Result<Vec<String>, _> = values
        .iter()
        .map(|v| match &v.val {
            Value::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Type(format!("{context}: expected symbol in parameter list"), span)),
        })
        .collect();
    Ok((params?, None))
}

/// Parse params from a Value that may be a List, Pair chain, or bare Symbol.
pub(crate) fn parse_params_from_value(val: &Value, context: &str, span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    match val {
        Value::List(items) => parse_params(items, context, span),
        Value::Pair(_) => {
            let mut params = Vec::new();
            let mut current = val.clone();
            loop {
                match current {
                    Value::Pair(cell) => {
                        let (car, cdr) = { let b = cell.borrow(); (b.0.clone(), b.1.clone()) };
                        match car {
                            Value::Symbol(s) => params.push(s),
                            _ => return Err(EvalError::Type(format!("{context}: expected symbol in parameter list"), span)),
                        }
                        current = cdr;
                    }
                    Value::Symbol(s) => return Ok((params, Some(s))),
                    Value::List(items) if items.is_empty() => return Ok((params, None)),
                    _ => return Err(EvalError::Type(format!("{context}: malformed parameter list"), span)),
                }
            }
        }
        Value::Symbol(s) => Ok((vec![], Some(s.clone()))),
        _ => Err(EvalError::Type(format!("{context}: expected parameter list"), span)),
    }
}
