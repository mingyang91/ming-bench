use std::fmt;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(elements) => fmt_list(elements, f),
        }
    }
}
