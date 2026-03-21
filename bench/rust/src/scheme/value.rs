use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::scheme::env::Env;

/// A Scheme value.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    /// Exact rational: (numerator, denominator), always reduced, denom > 0.
    Rational(i64, i64),
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
    Pair(Box<Value>, Box<Value>),
    Vector(Rc<RefCell<Vec<Value>>>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Vec<Value>, Value)>,
        def_env: Rc<RefCell<Env>>,
    },
    Void,
    /// Multiple return values from `(values ...)`.
    Values(Vec<Value>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (Value::Continuation(a), Value::Continuation(b)) => a == b,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            (Value::Values(a), Value::Values(b)) => a == b,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

fn write_pair(f: &mut fmt::Formatter<'_>, car: &Value, cdr: &Value) -> fmt::Result {
    write!(f, "({car}")?;
    let mut current = cdr;
    loop {
        match current {
            Value::Pair(a, b) => {
                write!(f, " {a}")?;
                current = b;
            }
            Value::List(items) if items.is_empty() => break,
            Value::List(items) => {
                write_list_tail(f, items)?;
                break;
            }
            other => {
                write!(f, " . {other}")?;
                break;
            }
        }
    }
    write!(f, ")")
}

fn write_list_tail(f: &mut fmt::Formatter<'_>, items: &[Value]) -> fmt::Result {
    for item in items {
        write!(f, " {item}")?;
    }
    Ok(())
}

fn write_values(f: &mut fmt::Formatter<'_>, vals: &[Value]) -> fmt::Result {
    for (i, v) in vals.iter().enumerate() {
        if i > 0 {
            writeln!(f)?;
        }
        write!(f, "{v}")?;
    }
    Ok(())
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

fn write_float(f: &mut fmt::Formatter<'_>, v: f64) -> fmt::Result {
    let s = format!("{v}");
    if v.is_finite() && !s.contains('.') {
        write!(f, "{s}.0")
    } else {
        write!(f, "{s}")
    }
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
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.deep_equal(y))
            }
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1.deep_equal(b1) && a2.deep_equal(b2),
            (Value::Vector(a), Value::Vector(b)) => {
                let a = a.borrow();
                let b = b.borrow();
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.deep_equal(y))
            }
            (Value::Values(a), Value::Values(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.deep_equal(y))
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
            Value::Float(v) => write_float(f, *v),
            Value::Rational(n, d) => write!(f, "{n}/{d}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(items) => write_list(f, items),
            Value::Pair(car, cdr) => write_pair(f, car, cdr),
            Value::Vector(v) => write_vector(f, v),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Lambda { .. } | Value::Builtin(_) | Value::Continuation(_)
            | Value::Macro { .. } => {
                write!(f, "#<procedure>")
            }
            Value::Void => write!(f, "#<void>"),
            Value::Values(vals) => write_values(f, vals),
        }
    }
}
