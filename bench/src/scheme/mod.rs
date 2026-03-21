mod builtins;
pub mod error;
mod eval;
mod parser;

pub use error::EvalError;

use std::collections::HashMap;

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Nil,
    Lambda {
        name: Option<String>,
        params: Vec<String>,
        body: Expr,
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
            (Value::List(a), Value::List(b)) => a == b,
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
            Value::Nil => write!(f, "()"),
            Value::List(items) => write!(f, "({})", fmt_list(items)),
            Value::Lambda { .. } => write!(f, "#<procedure>"),
        }
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
type Env = HashMap<String, Value>;

/// An S-expression AST node.
#[derive(Debug, Clone)]
enum Expr {
    Atom(String),
    List(Vec<Expr>),
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
    if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
        let inner = &token[1..token.len() - 1];
        return Ok(Value::String(inner.to_string()));
    }
    Ok(Value::Symbol(token.to_string()))
}

/// Convert an Expr into a quoted Value (no evaluation).
fn quote_expr(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Atom(token) => atom_to_value(token),
        Expr::List(items) => {
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
    let mut last = None;
    for expr in &exprs {
        last = Some(eval::eval(expr, &mut env)?);
    }
    Ok(last.expect("exprs is non-empty").to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
