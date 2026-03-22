use std::fmt;

use crate::scheme::env::Env;
use crate::scheme::parser::Expr;

/// A Scheme value.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    SchemeString(String),
    Symbol(String),
    Char(char),
    Nil,
    Pair(Box<Value>, Box<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
}

impl Value {
    /// Returns true if the value is truthy (everything except #f).
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::SchemeString(a), Value::SchemeString(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    /// Format value for `display` (no quotes on strings).
    pub fn display_fmt(&self, buf: &mut String) {
        match self {
            Value::SchemeString(s) => buf.push_str(s),
            Value::Pair(_, _) => {
                buf.push('(');
                display_list(buf, self);
            }
            Value::Builtin(_) => buf.push_str(&self.to_string()),
            other => buf.push_str(&other.to_string()),
        }
    }

    /// Format value for `write` (quotes on strings).
    pub fn write_fmt(&self, buf: &mut String) {
        buf.push_str(&self.to_string());
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::SchemeString(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Nil => write!(f, "()"),
            Value::Pair(_, _) => {
                write!(f, "(")?;
                write_list(f, self)
            }
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::Builtin(name) => write!(f, "#<procedure:{name}>"),
        }
    }
}

fn write_list(f: &mut fmt::Formatter<'_>, val: &Value) -> fmt::Result {
    match val {
        Value::Pair(car, cdr) => {
            write!(f, "{car}")?;
            match cdr.as_ref() {
                Value::Nil => write!(f, ")"),
                Value::Pair(_, _) => {
                    write!(f, " ")?;
                    write_list(f, cdr)
                }
                other => write!(f, " . {other})"),
            }
        }
        _ => write!(f, ")"),
    }
}

fn display_list(buf: &mut String, val: &Value) {
    match val {
        Value::Pair(car, cdr) => {
            car.display_fmt(buf);
            match cdr.as_ref() {
                Value::Nil => buf.push(')'),
                Value::Pair(_, _) => {
                    buf.push(' ');
                    display_list(buf, cdr);
                }
                other => {
                    buf.push_str(" . ");
                    other.display_fmt(buf);
                    buf.push(')');
                }
            }
        }
        _ => buf.push(')'),
    }
}
