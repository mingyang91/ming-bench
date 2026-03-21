mod builtins;
pub mod error;
mod parser;

pub use error::EvalError;

use std::collections::HashMap;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Nil,
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

/// Evaluate an expression in the given environment.
fn eval(expr: &Expr, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Atom(token) => {
            // Try literal first, then variable lookup
            if token.starts_with('"')
                || token.parse::<i64>().is_ok()
                || token == "#t"
                || token == "#f"
            {
                atom_to_value(token)
            } else if let Some(val) = env.get(token) {
                Ok(val.clone())
            } else {
                // Return as symbol for now (unbound)
                Err(EvalError::UnboundVariable {
                    name: token.clone(),
                })
            }
        }
        Expr::List(items) => eval_list(items, env),
    }
}

/// Evaluate a list expression (function application or special form).
fn eval_list(items: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let [operator, args @ ..] = items else {
        return Err(EvalError::Parse {
            message: "empty list".to_string(),
        });
    };
    let Expr::Atom(op) = operator else {
        return Err(EvalError::Parse {
            message: "expected operator".to_string(),
        });
    };
    match op.as_str() {
        "define" => eval_define(args, env),
        "if" => eval_if(args, env),
        "quote" => eval_quote(args),
        "and" => eval_and(args, env),
        "or" => eval_or(args, env),
        _ => {
            let evaluated: Vec<Value> = args
                .iter()
                .map(|a| eval(a, env))
                .collect::<Result<_, _>>()?;
            apply_builtin(op, &evaluated)
        }
    }
}

/// Apply a built-in operator.
fn apply_builtin(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "+" => builtins::apply_add(args),
        "-" => builtins::apply_sub(args),
        "*" => builtins::apply_mul(args),
        "/" => builtins::apply_div(args),
        "<" => builtins::apply_compare(args, |a, b| a < b),
        ">" => builtins::apply_compare(args, |a, b| a > b),
        "=" => builtins::apply_compare(args, |a, b| a == b),
        "<=" => builtins::apply_compare(args, |a, b| a <= b),
        ">=" => builtins::apply_compare(args, |a, b| a >= b),
        "not" => builtins::apply_not(args),
        _ => Err(EvalError::UnboundVariable {
            name: op.to_string(),
        }),
    }
}

/// Evaluate `(define name value)`.
fn eval_define(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let [Expr::Atom(name), val_expr] = args else {
        return Err(EvalError::Parse {
            message: "define requires a name and a value".to_string(),
        });
    };
    let val = eval(val_expr, env)?;
    env.insert(name.clone(), val.clone());
    Ok(val)
}

/// Evaluate `(if cond then else)`.
fn eval_if(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let (cond, then_expr, else_expr) = match args {
        [c, t, e] => (c, t, Some(e)),
        [c, t] => (c, t, None),
        _ => {
            return Err(EvalError::Parse {
                message: "if requires 2 or 3 arguments".to_string(),
            })
        }
    };
    let cond_val = eval(cond, env)?;
    if cond_val != Value::Boolean(false) {
        eval(then_expr, env)
    } else if let Some(e) = else_expr {
        eval(e, env)
    } else {
        Ok(Value::Nil)
    }
}

/// Evaluate `(quote expr)`.
fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(EvalError::Parse {
            message: "quote requires exactly 1 argument".to_string(),
        });
    };
    quote_expr(expr)
}

/// Short-circuit `and`: returns last truthy value, or first falsy value.
fn eval_and(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env)?;
        if result == Value::Boolean(false) {
            return Ok(result);
        }
    }
    Ok(result)
}

/// Short-circuit `or`: returns first truthy value, or last falsy value.
fn eval_or(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if result != Value::Boolean(false) {
            return Ok(result);
        }
    }
    Ok(result)
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
        last = Some(eval(expr, &mut env)?);
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
