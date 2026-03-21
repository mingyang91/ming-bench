use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::scheme::env::Env;

/// Data for a captured continuation.
#[derive(Debug, Clone)]
pub struct ContinuationData {
    pub id: u64,
    pub expr_idx: usize,
}

/// A Scheme value.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(std::string::String),
    Symbol(std::string::String),
    Char(char),
    List(Vec<Value>),
    Lambda {
        params: Vec<std::string::String>,
        rest_param: Option<std::string::String>,
        body: Vec<Value>,
        env: Rc<RefCell<Env>>,
    },
    Continuation(Rc<ContinuationData>),
    Macro {
        name: std::string::String,
        keywords: Vec<std::string::String>,
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
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Void, Value::Void) => true,
            (Value::Lambda { .. }, Value::Lambda { .. }) => false,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            (Value::Continuation(a), Value::Continuation(b)) => a.id == b.id,
            _ => false,
        }
    }
}

fn fmt_list(elems: &[Value], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(")?;
    for (i, elem) in elems.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write!(f, "{elem}")?;
    }
    write!(f, ")")
}

impl Value {
    /// Format for `display` — strings without quotes, chars as raw characters.
    pub fn display_str(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|e| e.display_str()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Macro { .. } => "#<macro>".to_string(),
            other => other.to_string(),
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
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::List(elems) => fmt_list(elems, f),
            Value::Lambda { .. } | Value::Continuation(_) => write!(f, "#<procedure>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
            Value::Void => write!(f, ""),
        }
    }
}
