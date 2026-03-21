use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

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
    Char(char),
    Symbol(String, Span),
    List(Vec<Value>, Span),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Value>,
        env: HashMap<String, Rc<RefCell<Value>>>,
    },
    Continuation {
        id: usize,
        expr_index: usize,
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
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Symbol(s, _) => write!(f, "{s}"),
            Value::List(items, _) => fmt_list(f, items),
            Value::Lambda { .. } | Value::Continuation { .. } => write!(f, "#<procedure>"),
        }
    }
}

impl Value {
    /// Format in `display` style: strings without quotes, everything else same.
    pub fn display_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{s}"),
            Value::Char(c) => write!(f, "{c}"),
            other => write!(f, "{other}"),
        }
    }

    /// Return the display-style string representation.
    pub fn to_display_string(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            other => format!("{other}"),
        }
    }
}
