use std::fmt;

use crate::scheme::env::Env;
use crate::scheme::parser::Expr;

/// A Scheme value.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    SchemeString(String),
    Symbol(String),
    Nil,
    Pair(Box<Value>, Box<Value>),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
}

impl Value {
    /// Returns true if the value is truthy (everything except #f).
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::SchemeString(a), Value::SchemeString(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::SchemeString(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Nil => write!(f, "()"),
            Value::Pair(_, _) => {
                write!(f, "(")?;
                write_list(f, self)
            }
            Value::Lambda { .. } => write!(f, "#<procedure>"),
        }
    }
}

fn write_list(f: &mut fmt::Formatter<'_>, val: &Value) -> fmt::Result {
    match val {
        Value::Pair(car, cdr) => {
            write!(f, "{car}")?;
            match cdr.as_ref() {
                Value::Nil => write!(f, ")"),
                Value::Pair(_, _) => {
                    write!(f, " ")?;
                    write_list(f, cdr)
                }
                other => write!(f, " . {other})"),
            }
        }
        _ => write!(f, ")"),
    }
}
