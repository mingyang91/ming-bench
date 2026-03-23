use std::fmt;

use crate::scheme::env::Env;
use crate::scheme::error::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64, Span),
    Boolean(bool, Span),
    String(String, Span),
    Symbol(String, Span),
    List(Vec<Value>, Span),
    Closure {
        params: Vec<String>,
        body: Box<Value>,
        env: Env,
    },
    Void,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n, _) => write!(f, "{n}"),
            Value::Boolean(true, _) => write!(f, "#t"),
            Value::Boolean(false, _) => write!(f, "#f"),
            Value::String(s, _) => write!(f, "\"{s}\""),
            Value::Symbol(s, _) => write!(f, "{s}"),
            Value::List(elems, _) => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            Value::Closure { .. } => write!(f, "#<procedure>"),
            Value::Void => write!(f, "#<void>"),
        }
    }
}

impl Value {
    pub fn span(&self) -> Span {
        match self {
            Value::Integer(_, s)
            | Value::Boolean(_, s)
            | Value::String(_, s)
            | Value::Symbol(_, s)
            | Value::List(_, s) => *s,
            Value::Closure { .. } => Span::default(),
            Value::Void => Span::default(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false, _))
    }

    // Convenience constructors for runtime-created values (no source position)
    pub fn int(n: i64) -> Self {
        Value::Integer(n, Span::default())
    }
    pub fn bool(b: bool) -> Self {
        Value::Boolean(b, Span::default())
    }
    pub fn symbol(s: String) -> Self {
        Value::Symbol(s, Span::default())
    }
    pub fn list(elems: Vec<Value>) -> Self {
        Value::List(elems, Span::default())
    }
}
