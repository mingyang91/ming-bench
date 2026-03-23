use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::scheme::env::Env;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    String(String),
    Symbol(String, Option<Span>),
    List(Vec<Value>, Option<Span>),
    Builtin(String),
    Closure {
        params: Vec<String>,
        body: Vec<Value>,
        env: Rc<RefCell<Env>>,
    },
    Void,
}

impl Value {
    pub fn span(&self) -> Option<Span> {
        match self {
            Value::Symbol(_, span) | Value::List(_, span) => *span,
            _ => None,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a, _), Value::Symbol(b, _)) => a == b,
            (Value::List(a, _), Value::List(b, _)) => a == b,
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (Value::Void, Value::Void) => true,
            (Value::Closure { .. }, Value::Closure { .. }) => false,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{n}"),
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{s}\""),
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
            Value::Builtin(name) => write!(f, "#<procedure:{name}>"),
            Value::Closure { .. } => write!(f, "#<procedure>"),
            Value::Void => write!(f, ""),
        }
    }
}
