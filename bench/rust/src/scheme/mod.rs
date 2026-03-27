pub mod error;

pub use error::EvalError;

use std::fmt;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{s}\""),
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

// ── Parser ──

struct Parser<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input: input.as_bytes(), pos: 0 }
    }

    fn skip_whitespace_and_comments(&mut self) {
        while self.pos < self.input.len() {
            let b = self.input[self.pos];
            if b.is_ascii_whitespace() {
                self.pos += 1;
            } else if b == b';' {
                while self.pos < self.input.len() && self.input[self.pos] != b'\n' {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.pos < self.input.len() { Some(self.input[self.pos]) } else { None }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace_and_comments();
        match self.peek() {
            None => Err(EvalError::Parse("unexpected end of input".into())),
            Some(b'(') => self.parse_list(),
            Some(b'"') => self.parse_string(),
            Some(b'#') => self.parse_hash(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            match self.peek() {
                None => return Err(EvalError::Parse("unterminated list".into())),
                Some(b')') => { self.pos += 1; return Ok(Expr::List(items)); }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip opening "
        let mut s = String::new();
        loop {
            if self.pos >= self.input.len() {
                return Err(EvalError::Parse("unterminated string".into()));
            }
            let b = self.input[self.pos];
            if b == b'"' { self.pos += 1; return Ok(Expr::Str(s)); }
            if b == b'\\' {
                self.pos += 1;
                if self.pos >= self.input.len() {
                    return Err(EvalError::Parse("unterminated escape".into()));
                }
                match self.input[self.pos] {
                    b'n' => s.push('\n'),
                    b't' => s.push('\t'),
                    b'\\' => s.push('\\'),
                    b'"' => s.push('"'),
                    c => { s.push('\\'); s.push(c as char); }
                }
            } else {
                s.push(b as char);
            }
            self.pos += 1;
        }
    }

    fn parse_hash(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '#'
        match self.peek() {
            Some(b't') => { self.pos += 1; Ok(Expr::Boolean(true)) }
            Some(b'f') => { self.pos += 1; Ok(Expr::Boolean(false)) }
            _ => Err(EvalError::Parse("unexpected # literal".into())),
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;
        while self.pos < self.input.len() {
            let b = self.input[self.pos];
            if b.is_ascii_whitespace() || b == b'(' || b == b')' || b == b'"' || b == b';' {
                break;
            }
            self.pos += 1;
        }
        if self.pos == start {
            return Err(EvalError::Parse("unexpected character".into()));
        }
        let token = std::str::from_utf8(&self.input[start..self.pos]).expect("input is valid UTF-8");
        // Try integer
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Expr::Integer(n));
        }
        Ok(Expr::Symbol(token.to_string()))
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.pos >= self.input.len() { break; }
            exprs.push(self.parse_expr()?);
        }
        if exprs.is_empty() {
            return Err(EvalError::Parse("empty input".into()));
        }
        Ok(exprs)
    }
}

// ── Evaluator ──

fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundVariable(name.clone())),
        Expr::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            match &items[0] {
                Expr::Symbol(op) => eval_special(op, &items[1..]),
                _ => Err(EvalError::Type("not a procedure".into())),
            }
        }
    }
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

fn expect_integer(v: &Value, context: &str) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{context}: expected integer, got {v}"))),
    }
}

fn eval_special(op: &str, args: &[Expr]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_integer(&eval(a)?, "+")?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            let first = expect_integer(&eval(&args[0])?, "-")?;
            if args.len() == 1 {
                return Ok(Value::Integer(-first));
            }
            let mut result = first;
            for a in &args[1..] {
                result -= expect_integer(&eval(a)?, "-")?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= expect_integer(&eval(a)?, "*")?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let first = expect_integer(&eval(&args[0])?, "/")?;
            if args.len() == 1 {
                if first == 0 { return Err(EvalError::DivisionByZero); }
                return Ok(Value::Integer(1 / first));
            }
            let mut result = first;
            for a in &args[1..] {
                let d = expect_integer(&eval(a)?, "/")?;
                if d == 0 { return Err(EvalError::DivisionByZero); }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" | ">" | "=" | "<=" | ">=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{op} requires 2 arguments")));
            }
            let a = expect_integer(&eval(&args[0])?, op)?;
            let b = expect_integer(&eval(&args[1])?, op)?;
            let result = match op {
                "<" => a < b,
                ">" => a > b,
                "=" => a == b,
                "<=" => a <= b,
                ">=" => a >= b,
                _ => unreachable!(),
            };
            Ok(Value::Boolean(result))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            let v = eval(&args[0])?;
            Ok(Value::Boolean(!is_truthy(&v)))
        }
        "and" => {
            let mut result = Value::Boolean(true);
            for a in args {
                result = eval(a)?;
                if !is_truthy(&result) {
                    return Ok(result);
                }
            }
            Ok(result)
        }
        "or" => {
            let mut result = Value::Boolean(false);
            for a in args {
                result = eval(a)?;
                if is_truthy(&result) {
                    return Ok(result);
                }
            }
            Ok(result)
        }
        _ => Err(EvalError::UnboundVariable(op.to_string())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
