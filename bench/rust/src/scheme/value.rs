use std::fmt;

use crate::scheme::env::Env;
use crate::scheme::parser::Expr;

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
        body: Expr,
        closure: Env,
    },
    Builtin(String),
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
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
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
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(items) => fmt_list(f, items),
            Value::Lambda { .. } | Value::Builtin(_) => write!(f, "#<procedure>"),
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

    /// Format for `display` — strings without quotes, chars as plain characters.
    pub fn display_fmt(&self, buf: &mut String) {
        match self {
            Value::String(s) => buf.push_str(s),
            Value::Char(c) => buf.push(*c),
            Value::List(items) => Self::display_list(items, buf),
            other => buf.push_str(&other.to_string()),
        }
    }

    fn display_list(items: &[Value], buf: &mut String) {
        buf.push('(');
        let Some((first, rest)) = items.split_first() else {
            buf.push(')');
            return;
        };
        first.display_fmt(buf);
        for item in rest {
            buf.push(' ');
            item.display_fmt(buf);
        }
        buf.push(')');
    }
}
