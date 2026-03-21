use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::number::fmt_float;

/// Captured remaining body for a continuation in a body-init position.
#[derive(Debug, Clone)]
pub struct BodyContinuation {
    pub remaining: Vec<Value>,
    pub env: Rc<RefCell<Env>>,
}

/// Data for a captured continuation.
#[derive(Debug, Clone)]
pub struct ContinuationData {
    pub id: u64,
    pub expr_idx: usize,
    pub body_continuation: Option<BodyContinuation>,
}

/// A Scheme value.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Rational(i64, i64),
    Float(f64),
    Boolean(bool),
    String(std::string::String),
    Symbol(std::string::String),
    Char(char),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Vector(Rc<RefCell<Vec<Value>>>),
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
    /// Multiple return values from `values`.
    Values(Vec<Value>),
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
            (Value::Values(a), Value::Values(b)) => a == b,
            (Value::Void, Value::Void) => true,
            (Value::Lambda { .. }, Value::Lambda { .. }) => false,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            (Value::Continuation(a), Value::Continuation(b)) => a.id == b.id,
            _ => false,
        }
    }
}

fn fmt_values(vals: &[Value], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let [first, rest @ ..] = vals else {
        return Ok(());
    };
    write!(f, "{first}")?;
    for v in rest {
        write!(f, "\n{v}")?;
    }
    Ok(())
}

fn fmt_vector(elems: &[Value], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "#(")?;
    for (i, elem) in elems.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write!(f, "{elem}")?;
    }
    write!(f, ")")
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
            Value::Pair(car, cdr) => format!("({} . {})", car.display_str(), cdr.display_str()),
            Value::Vector(cells) => {
                let elems = cells.borrow();
                let inner: Vec<String> = elems.iter().map(|e| e.display_str()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Macro { .. } => "#<macro>".to_string(),
            Value::Values(vals) => vals.iter().map(|v| v.display_str()).collect::<Vec<_>>().join("\n"),
            other => other.to_string(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Rational(n, d) => write!(f, "{n}/{d}"),
            Value::Float(v) => write!(f, "{}", fmt_float(*v)),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::List(elems) => fmt_list(elems, f),
            Value::Pair(car, cdr) => write!(f, "({car} . {cdr})"),
            Value::Vector(cells) => fmt_vector(&cells.borrow(), f),
            Value::Values(vals) => fmt_values(vals, f),
            Value::Lambda { .. } | Value::Continuation(_) => write!(f, "#<procedure>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
            Value::Void => write!(f, ""),
        }
    }
}
