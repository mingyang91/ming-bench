pub mod error;

pub use error::EvalError;

use std::fmt;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!("expected number, got {}", self))),
        }
    }
}

// --- Parser ---

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

struct Parser {
    tokens: Vec<String>,
    pos: usize,
}

fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => {
                tokens.push("(".into());
                i += 1;
            }
            ')' => {
                tokens.push(")".into());
                i += 1;
            }
            '"' => {
                let mut s = String::from("\"");
                i += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        i += 2;
                    } else {
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                }
                tokens.push(s);
            }
            _ => {
                let mut tok = String::new();
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
                {
                    tok.push(chars[i]);
                    i += 1;
                }
                tokens.push(tok);
            }
        }
    }
    tokens
}

impl Parser {
    fn new(input: &str) -> Self {
        Parser {
            tokens: tokenize(input),
            pos: 0,
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        if self.pos >= self.tokens.len() {
            return Err(EvalError::Parse("unexpected end of input".into()));
        }
        let tok = self.tokens[self.pos].clone();
        self.pos += 1;

        if tok == "(" {
            let mut elems = Vec::new();
            while self.pos < self.tokens.len() && self.tokens[self.pos] != ")" {
                elems.push(self.parse_expr()?);
            }
            if self.pos >= self.tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into()));
            }
            self.pos += 1; // skip ')'
            Ok(Expr::List(elems))
        } else if tok == ")" {
            Err(EvalError::Parse("unexpected ')'".into()))
        } else if tok == "#t" {
            Ok(Expr::Boolean(true))
        } else if tok == "#f" {
            Ok(Expr::Boolean(false))
        } else if tok.starts_with('"') && tok.ends_with('"') {
            let inner = &tok[1..tok.len() - 1];
            // Handle escape sequences
            let mut result = String::new();
            let chars: Vec<char> = inner.chars().collect();
            let mut j = 0;
            while j < chars.len() {
                if chars[j] == '\\' && j + 1 < chars.len() {
                    match chars[j + 1] {
                        'n' => result.push('\n'),
                        't' => result.push('\t'),
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        c => {
                            result.push('\\');
                            result.push(c);
                        }
                    }
                    j += 2;
                } else {
                    result.push(chars[j]);
                    j += 1;
                }
            }
            Ok(Expr::Str(result))
        } else if let Ok(n) = tok.parse::<i64>() {
            Ok(Expr::Integer(n))
        } else {
            Ok(Expr::Symbol(tok))
        }
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        while self.pos < self.tokens.len() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }
}

// --- Evaluator ---

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
    )
}

fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => {
            if is_builtin(name) {
                Ok(Value::Symbol(name.clone()))
            } else {
                Err(EvalError::UnboundVariable(name.clone()))
            }
        }
        Expr::List(elems) => {
            if elems.is_empty() {
                return Ok(Value::List(vec![]));
            }
            // Check for special forms
            if let Expr::Symbol(op) = &elems[0] {
                match op.as_str() {
                    "and" => return eval_and(&elems[1..]),
                    "or" => return eval_or(&elems[1..]),
                    _ => {}
                }
            }
            // Function call
            let func = eval(&elems[0])?;
            if let Value::Symbol(op) = &func {
                let args: Vec<Value> = elems[1..]
                    .iter()
                    .map(|e| eval(e))
                    .collect::<Result<_, _>>()?;
                apply_builtin(op, &args)
            } else {
                Err(EvalError::Type(format!("not a procedure: {}", func)))
            }
        }
    }
}

fn eval_and(exprs: &[Expr]) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr]) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in exprs {
        let result = eval(expr)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Value::Boolean(false))
}

fn apply_builtin(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += a.as_integer()?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-args[0].as_integer()?));
            }
            let mut result = args[0].as_integer()?;
            for a in &args[1..] {
                result -= a.as_integer()?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= a.as_integer()?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                let d = args[0].as_integer()?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                return Ok(Value::Integer(1 / d));
            }
            let mut result = args[0].as_integer()?;
            for a in &args[1..] {
                let d = a.as_integer()?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            ensure_args(op, args, 2)?;
            Ok(Value::Boolean(args[0].as_integer()? < args[1].as_integer()?))
        }
        ">" => {
            ensure_args(op, args, 2)?;
            Ok(Value::Boolean(args[0].as_integer()? > args[1].as_integer()?))
        }
        "=" => {
            ensure_args(op, args, 2)?;
            Ok(Value::Boolean(
                args[0].as_integer()? == args[1].as_integer()?,
            ))
        }
        "<=" => {
            ensure_args(op, args, 2)?;
            Ok(Value::Boolean(
                args[0].as_integer()? <= args[1].as_integer()?,
            ))
        }
        ">=" => {
            ensure_args(op, args, 2)?;
            Ok(Value::Boolean(
                args[0].as_integer()? >= args[1].as_integer()?,
            ))
        }
        "not" => {
            ensure_args(op, args, 1)?;
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        _ => Err(EvalError::UnboundVariable(op.to_string())),
    }
}

fn ensure_args(op: &str, args: &[Value], expected: usize) -> Result<(), EvalError> {
    if args.len() != expected {
        return Err(EvalError::Arity(format!(
            "{} expects {} arguments, got {}",
            op,
            expected,
            args.len()
        )));
    }
    Ok(())
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
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
