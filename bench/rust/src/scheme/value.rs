use std::fmt;

use crate::scheme::env::Env;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
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
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(elems) => {
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
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}
