use std::fmt;

use crate::scheme::env::Env;

/// A Scheme value.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Value>,
        env: Env,
    },
    Builtin {
        name: String,
    },
    Continuation {
        id: u64,
    },
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Builtin { name: a }, Value::Builtin { name: b }) => a == b,
            (Value::Continuation { id: a }, Value::Continuation { id: b }) => a == b,
            _ => false,
        }
    }
}

fn fmt_list(f: &mut fmt::Formatter<'_>, elems: &[Value]) -> fmt::Result {
    write!(f, "(")?;
    for (i, elem) in elems.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write!(f, "{elem}")?;
    }
    write!(f, ")")
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(elems) => fmt_list(f, elems),
            Value::Lambda { .. } | Value::Builtin { .. } | Value::Continuation { .. } => {
                write!(f, "#<procedure>")
            }
        }
    }
}
