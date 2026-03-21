use std::collections::HashMap;
use std::fmt;

/// Source position tracking for error reporting.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String, Span),
    List(Vec<Value>, Span),
    Lambda {
        params: Vec<String>,
        body: Vec<Value>,
        env: HashMap<String, Value>,
    },
}

fn fmt_list(f: &mut fmt::Formatter<'_>, items: &[Value]) -> fmt::Result {
    write!(f, "(")?;
    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write!(f, "{item}")?;
    }
    write!(f, ")")
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Symbol(s, _) => write!(f, "{s}"),
            Value::List(items, _) => fmt_list(f, items),
            Value::Lambda { .. } => write!(f, "#<procedure>"),
        }
    }
}
