use std::cell::RefCell;
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
    Char(char),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Value>,
        env: Rc<Env>,
    },
    Macro {
        literals: Vec<String>,
        rules: Vec<(Value, Value)>,
        env: Rc<Env>,
    },
    Vector(Rc<RefCell<Vec<Value>>>),
    Builtin(String),
    Continuation(u64),
    Void,
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
            (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
            (Value::Continuation(a), Value::Continuation(b)) => a == b,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

fn fmt_vector(elements: &[Value], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "#(")?;
    for (i, elem) in elements.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write!(f, "{elem}")?;
    }
    write!(f, ")")
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
            Value::Char(c) => c.to_string(),
            Value::Continuation(_) | Value::Macro { .. } => "#<continuation>".to_string(),
            Value::Vector(_) => self.to_string(),
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
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::List(elements) => fmt_list(elements, f),
            Value::Vector(cells) => fmt_vector(&cells.borrow(), f),
            Value::Lambda { .. } | Value::Builtin(_) | Value::Continuation(_) | Value::Macro { .. } => write!(f, "#<procedure>"),
            Value::Void => write!(f, ""),
        }
    }
}
