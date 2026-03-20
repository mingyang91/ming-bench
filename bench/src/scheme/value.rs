use std::fmt;
use std::rc::Rc;

use super::env::Env;

/// A Scheme value.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        body: Vec<Value>,
        env: Rc<Env>,
    },
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

fn fmt_list(elements: &[Value], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(")?;
    for (i, elem) in elements.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write!(f, "{elem}")?;
    }
    write!(f, ")")
}

impl Value {
    /// Format as `display` would — strings without quotes.
    pub fn display_str(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            other => other.to_string(),
        }
    }

    /// Format as `write` would — strings with quotes (same as Display).
    pub fn write_str(&self) -> String {
        self.to_string()
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(elements) => fmt_list(elements, f),
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::Void => write!(f, ""),
        }
    }
}
