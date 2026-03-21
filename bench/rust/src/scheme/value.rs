use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::scheme::env::Env;

/// A Scheme value.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Value>,
        closure: Rc<RefCell<Env>>,
    },
    Builtin(String),
    Continuation(u64),
    Vector(Rc<RefCell<Vec<Value>>>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Vec<Value>, Value)>,
        def_env: Rc<RefCell<Env>>,
    },
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (Value::Continuation(a), Value::Continuation(b)) => a == b,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

fn write_vector(f: &mut fmt::Formatter<'_>, v: &RefCell<Vec<Value>>) -> fmt::Result {
    write!(f, "#(")?;
    let items = v.borrow();
    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write!(f, "{item}")?;
    }
    write!(f, ")")
}

fn write_list(f: &mut fmt::Formatter<'_>, items: &[Value]) -> fmt::Result {
    write!(f, "(")?;
    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write!(f, "{item}")?;
    }
    write!(f, ")")
}

impl Value {
    /// Format value for `display` (no quotes on strings).
    pub fn display_value(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            other => other.to_string(),
        }
    }

    /// Deep structural equality (for `equal?`).
    pub fn deep_equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.deep_equal(y))
            }
            (Value::Vector(a), Value::Vector(b)) => {
                let a = a.borrow();
                let b = b.borrow();
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.deep_equal(y))
            }
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }

    /// Shallow equality (for `eqv?`): same as PartialEq.
    pub fn eqv(&self, other: &Self) -> bool {
        self == other
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(items) => write_list(f, items),
            Value::Vector(v) => write_vector(f, v),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Lambda { .. } | Value::Builtin(_) | Value::Continuation(_)
            | Value::Macro { .. } => {
                write!(f, "#<procedure>")
            }
            Value::Void => write!(f, "#<void>"),
        }
    }
}
