use std::fmt;

use crate::scheme::env::Env;
use crate::scheme::parser::Expr;

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        body: Expr,
        closure: Env,
    },
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Void, Value::Void) => true,
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
            Value::String(s) => write!(f, "\"{s}\""),
            Value::List(items) => fmt_list(f, items),
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::Void => write!(f, ""),
        }
    }
}

fn fmt_list(f: &mut fmt::Formatter<'_>, items: &[Value]) -> fmt::Result {
    write!(f, "(")?;
    for (i, item) in items.iter().enumerate() {
        if i > 0 { write!(f, " ")?; }
        write!(f, "{item}")?;
    }
    write!(f, ")")
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}
