mod builtins;
pub mod error;
mod eval;
mod parser;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Source position in the input.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Span {
    pub line: usize,
    pub col: usize,
}

/// A Scheme value.
#[derive(Debug, Clone)]
pub(crate) enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Nil,
    Char(char),
    Pair(Box<Value>, Box<Value>),
    Lambda {
        name: Option<String>,
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        closure_env: Env,
    },
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Nil => write!(f, "()"),
            Value::List(items) => write!(f, "({})", fmt_list(items)),
            Value::Pair(car, cdr) => write!(f, "({car} . {cdr})"),
            Value::Lambda { .. } => write!(f, "#<procedure>"),
        }
    }
}

impl Value {
    /// Format for `display`: strings without quotes, everything else as Display.
    fn display_fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::String(s) => write!(f, "{s}"),
            Value::Char(c) => write!(f, "{c}"),
            Value::List(items) => display_fmt_list(items, f),
            other => write!(f, "{other}"),
        }
    }
}

/// Format a list of values using display semantics (no quotes on strings).
fn display_fmt_list(items: &[Value], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "(")?;
    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        item.display_fmt(f)?;
    }
    write!(f, ")")
}

/// Wrapper for display-style formatting (no quotes on strings).
pub(crate) struct DisplayValue<'a>(pub(crate) &'a Value);

impl std::fmt::Display for DisplayValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.display_fmt(f)
    }
}

/// Format a slice of values as a space-separated string.
fn fmt_list(items: &[Value]) -> String {
    items
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(" ")
}

/// Environment for variable bindings.
/// Each binding is wrapped in Rc<RefCell<>> so closures can share mutable state via `set!`.
type Env = HashMap<String, Rc<RefCell<Value>>>;

/// Look up a variable in the environment.
pub(crate) fn env_get(env: &Env, key: &str) -> Option<Value> {
    env.get(key).map(|b| b.borrow().clone())
}

/// Define a new binding in the environment (creates a fresh cell).
pub(crate) fn env_define(env: &mut Env, key: String, val: Value) {
    env.insert(key, Rc::new(RefCell::new(val)));
}

/// Mutate an existing binding. Returns an error if the variable is not bound.
pub(crate) fn env_set(env: &Env, key: &str, val: Value) -> Result<(), EvalError> {
    match env.get(key) {
        Some(binding) => {
            *binding.borrow_mut() = val;
            Ok(())
        }
        None => Err(EvalError::UnboundVariable {
            name: key.to_string(),
        }),
    }
}

/// An S-expression AST node with source position.
#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Atom(String, Span),
    List(Vec<Expr>, Span),
}

impl Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Atom(_, span) | Expr::List(_, span) => *span,
        }
    }
}

use parser::{parse_all, tokenize};

/// Parse an atom token into a Value.
fn atom_to_value(token: &str) -> Result<Value, EvalError> {
    if let Ok(n) = token.parse::<i64>() {
        return Ok(Value::Integer(n));
    }
    if token == "#t" {
        return Ok(Value::Boolean(true));
    }
    if token == "#f" {
        return Ok(Value::Boolean(false));
    }
    if token.starts_with("#\\") && token.len() > 2 {
        let char_name = &token[2..];
        let ch = match char_name {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            _ if char_name.len() == 1 => char_name.chars().next().expect("single char"),
            _ => {
                return Err(EvalError::Parse {
                    message: format!("unknown character name: {char_name}"),
                })
            }
        };
        return Ok(Value::Char(ch));
    }
    if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
        let inner = &token[1..token.len() - 1];
        return Ok(Value::String(inner.to_string()));
    }
    Ok(Value::Symbol(token.to_string()))
}

/// Convert an Expr into a quoted Value (no evaluation).
fn quote_expr(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Atom(token, _) => atom_to_value(token),
        Expr::List(items, _) if items.is_empty() => Ok(Value::Nil),
        Expr::List(items, _) => {
            let values: Vec<Value> = items.iter().map(quote_expr).collect::<Result<_, _>>()?;
            Ok(Value::List(values))
        }
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let tokens = tokenize(input);
    if tokens.is_empty() {
        return Err(EvalError::Parse {
            message: "empty input".to_string(),
        });
    }
    let exprs = parse_all(&tokens)?;
    let mut env = Env::new();
    let mut output = String::new();
    let mut last = None;
    for expr in &exprs {
        last = Some(eval::eval(expr, &mut env, &mut output)?);
    }
    Ok(last.expect("exprs is non-empty").to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let tokens = tokenize(input);
    if tokens.is_empty() {
        return Err(EvalError::Parse {
            message: "empty input".to_string(),
        });
    }
    let exprs = parse_all(&tokens)?;
    let mut env = Env::new();
    let mut output = String::new();
    let mut last = None;
    for expr in &exprs {
        last = Some(eval::eval(expr, &mut env, &mut output)?);
    }
    let result = last.expect("exprs is non-empty").to_string();
    Ok((result, output))
}

#[cfg(test)]
mod tests;
