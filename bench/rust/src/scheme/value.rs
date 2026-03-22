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
    Continuation {
        id: u64,
    },
    SyntaxRules {
        literals: Vec<String>,
        rules: Vec<(Value, Value)>,
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
            (Value::Continuation { id: a }, Value::Continuation { id: b }) => a == b,
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
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
            Value::Continuation { .. } => write!(f, "#<continuation>"),
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::SyntaxRules { .. } => write!(f, "#<syntax>"),
            Value::Void => write!(f, ""),
        }
    }
}

impl Value {
    /// In Scheme, only #f is falsy.
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    /// Format for `display` — strings without quotes, chars as raw character.
    pub fn display_fmt(&self, out: &mut String) {
        match self {
            Value::Str(s) => out.push_str(s),
            Value::Char(c) => out.push(*c),
            Value::List(items) => {
                out.push('(');
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        out.push(' ');
                    }
                    item.display_fmt(out);
                }
                out.push(')');
            }
            Value::Continuation { .. } => out.push_str("#<continuation>"),
            Value::SyntaxRules { .. } => out.push_str("#<syntax>"),
            other => out.push_str(&other.to_string()),
        }
    }

    /// Format for `write` — strings with quotes (same as Display).
    pub fn write_fmt(&self, out: &mut String) {
        use std::fmt::Write;
        let _ = write!(out, "{self}");
    }
}
