use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use crate::scheme::env::Env;
use crate::scheme::parser::Expr;

/// A Scheme value.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Str(Rc<RefCell<String>>),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Void,
    Builtin(String),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        closure_env: Rc<Env>,
    },
}

impl Value {
    /// Convenience constructor for string values.
    pub fn new_str(s: String) -> Self {
        Value::Str(Rc::new(RefCell::new(s)))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => *a.borrow() == *b.borrow(),
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Void, Value::Void) => true,
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    /// Display string for output (write-style: strings get quotes).
    pub fn to_display_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => format!("\"{}\"", s.borrow()),
            Value::Symbol(s) => s.clone(),
            Value::Char(c) => format!("#\\{}", c),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display_string()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Void => "".into(),
            Value::Builtin(name) => format!("#<procedure:{}>", name),
            Value::Lambda { .. } => "#<procedure>".into(),
        }
    }

    /// Display string for `display` — strings without quotes.
    pub fn to_display_output(&self) -> String {
        match self {
            Value::Str(s) => s.borrow().clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display_output()).collect();
                format!("({})", inner.join(" "))
            }
            other => other.to_display_string(),
        }
    }

    /// Returns true if this value is truthy (everything except #f).
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display_string())
    }
}
