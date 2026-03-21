use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::parser::Expr;

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Float(f64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Char(char),
    Pair(Rc<RefCell<(Value, Value)>>),
    List(Vec<Value>),
    Vector(Rc<RefCell<Vec<Value>>>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Expr,
        closure: Env,
    },
    Builtin(String),
    Continuation { id: u64, expr_index: usize },
    Macro {
        literals: Vec<String>,
        rules: Vec<(Vec<Expr>, Expr)>,
        def_env: Env,
    },
    Record {
        type_id: u64,
        type_name: String,
        fields: Vec<(String, Value)>,
    },
    Values(Vec<Value>),
    Void,
}

/// Greatest common divisor (always positive).
pub fn gcd(mut a: i64, mut b: i64) -> i64 {
    a = a.abs();
    b = b.abs();
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Create a simplified rational, reducing to Integer when denominator is 1.
pub fn make_rational(numer: i64, denom: i64) -> Value {
    debug_assert!(denom != 0, "rational denominator must not be zero");
    let sign = if denom < 0 { -1 } else { 1 };
    let n = numer * sign;
    let d = denom * sign;
    let g = gcd(n, d);
    let (n, d) = (n / g, d / g);
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Pair(a), Value::Pair(b)) => {
                let ab = a.borrow();
                let bb = b.borrow();
                ab.0 == bb.0 && ab.1 == bb.1
            }
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Vector(a), Value::Vector(b)) => *a.borrow() == *b.borrow(),
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (
                Value::Continuation { id: a, .. },
                Value::Continuation { id: b, .. },
            ) => a == b,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            (
                Value::Record { type_id: a, fields: af, .. },
                Value::Record { type_id: b, fields: bf, .. },
            ) => a == b && af == bf,
            (Value::Values(a), Value::Values(b)) => a == b,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Rational(n, d) => write!(f, "{n}/{d}"),
            Value::Float(x) => fmt_float(f, *x),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Pair(p) => fmt_pair_chain(f, p),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(items) => fmt_list(f, items),
            Value::Vector(v) => fmt_vector(f, &v.borrow()),
            Value::Lambda { .. }
            | Value::Builtin(_)
            | Value::Continuation { .. }
            | Value::Macro { .. } => write!(f, "#<procedure>"),
            Value::Record { type_name, fields, .. } => fmt_record(f, type_name, fields),
            Value::Values(vals) => fmt_values(f, vals),
            Value::Void => write!(f, ""),
        }
    }
}

/// Format a pair chain, detecting proper lists and cycles.
fn fmt_pair_chain(f: &mut fmt::Formatter<'_>, start: &Rc<RefCell<(Value, Value)>>) -> fmt::Result {
    let mut seen = HashSet::new();
    write!(f, "(")?;

    let borrowed = start.borrow();
    write!(f, "{}", borrowed.0)?;
    seen.insert(Rc::as_ptr(start) as usize);

    let mut cur = borrowed.1.clone();
    drop(borrowed);

    loop {
        let p = match cur {
            Value::Pair(p) => p,
            Value::List(items) if items.is_empty() => { let _ = items; break; }
            other => { write!(f, " . {other}")?; break; }
        };
        let ptr = Rc::as_ptr(&p) as usize;
        if !seen.insert(ptr) {
            write!(f, " . ...")?;
            break;
        }
        let b = p.borrow();
        write!(f, " {}", b.0)?;
        cur = b.1.clone();
    }
    write!(f, ")")
}

fn fmt_values(f: &mut fmt::Formatter<'_>, vals: &[Value]) -> fmt::Result {
    let Some((first, _)) = vals.split_first() else {
        return write!(f, "");
    };
    write!(f, "{first}")
}

fn fmt_vector(f: &mut fmt::Formatter<'_>, items: &[Value]) -> fmt::Result {
    write!(f, "#(")?;
    for (i, item) in items.iter().enumerate() {
        if i > 0 { write!(f, " ")?; }
        write!(f, "{item}")?;
    }
    write!(f, ")")
}

fn fmt_record(f: &mut fmt::Formatter<'_>, type_name: &str, fields: &[(String, Value)]) -> fmt::Result {
    write!(f, "#<{type_name}")?;
    for (name, val) in fields {
        write!(f, " {name}={val}")?;
    }
    write!(f, ">")
}

fn fmt_float(f: &mut fmt::Formatter<'_>, x: f64) -> fmt::Result {
    if x.fract() == 0.0 && x.is_finite() {
        write!(f, "{x:.1}")
    } else {
        write!(f, "{x}")
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
    /// Create a new mutable pair.
    pub fn new_pair(car: Value, cdr: Value) -> Value {
        Value::Pair(Rc::new(RefCell::new((car, cdr))))
    }

    /// Build a proper list from a Vec of values (pair chain ending in nil).
    pub fn make_list(items: Vec<Value>) -> Value {
        items.into_iter().rev().fold(
            Value::List(vec![]),
            |acc, item| Value::new_pair(item, acc),
        )
    }

    /// Collect a proper list into a Vec. Returns None for improper/circular lists.
    pub fn collect_list(&self) -> Option<Vec<Value>> {
        match self {
            Value::List(items) => Some(items.clone()),
            Value::Pair(_) => collect_pair_chain(self),
            _ => None,
        }
    }

    /// Check if this is a proper list (empty or pair chain ending in nil), with cycle detection.
    pub fn is_proper_list(&self) -> bool {
        match self {
            Value::List(_) => true,
            Value::Pair(_) => self.collect_list().is_some(),
            _ => false,
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    /// Check if this value is a pair (non-empty list or dotted pair).
    pub fn is_pair(&self) -> bool {
        matches!(self, Value::List(l) if !l.is_empty()) || matches!(self, Value::Pair(_))
    }

    /// Check if this value is a number (integer, rational, or float).
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Rational(_, _) | Value::Float(_))
    }

    /// Check if this value is an exact integer (including rationals like 4/2 that simplify).
    pub fn is_integer_value(&self) -> bool {
        matches!(self, Value::Integer(_))
    }

    /// Check if this is nil (empty list).
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::List(items) if items.is_empty())
    }

    /// Format for `display` — strings without quotes, chars as plain characters.
    pub fn display_fmt(&self, buf: &mut String) {
        match self {
            Value::String(s) => buf.push_str(s),
            Value::Char(c) => buf.push(*c),
            Value::Pair(p) => display_pair_chain(p, buf),
            Value::List(items) => Self::display_list(items, buf),
            Value::Vector(v) => Self::display_vector(&v.borrow(), buf),
            Value::Continuation { .. } | Value::Macro { .. } => buf.push_str("#<procedure>"),
            Value::Record { type_name, fields, .. } => {
                Self::display_record(type_name, fields, buf);
            }
            Value::Values(vals) => Self::display_values(vals, buf),
            other => buf.push_str(&other.to_string()),
        }
    }

    fn display_record(type_name: &str, fields: &[(String, Value)], buf: &mut String) {
        buf.push_str(&format!("#<{type_name}"));
        for (name, val) in fields {
            buf.push(' ');
            buf.push_str(name);
            buf.push('=');
            val.display_fmt(buf);
        }
        buf.push('>');
    }

    fn display_vector(items: &[Value], buf: &mut String) {
        buf.push_str("#(");
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

    fn display_values(vals: &[Value], buf: &mut String) {
        if let Some((first, _)) = vals.split_first() {
            first.display_fmt(buf);
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

/// Walk a pair chain, collecting values into a Vec.
/// Returns None for improper or circular lists.
fn collect_pair_chain(start: &Value) -> Option<Vec<Value>> {
    let mut result = Vec::new();
    let mut cur = start.clone();
    let mut seen = HashSet::new();
    loop {
        let p = match cur {
            Value::Pair(p) => p,
            Value::List(items) if items.is_empty() => { let _ = items; return Some(result); }
            _ => return None,
        };
        let ptr = Rc::as_ptr(&p) as usize;
        if !seen.insert(ptr) { return None; }
        let b = p.borrow();
        result.push(b.0.clone());
        cur = b.1.clone();
    }
}

/// Display a pair chain with cycle detection for `display` output.
fn display_pair_chain(start: &Rc<RefCell<(Value, Value)>>, buf: &mut String) {
    let mut seen = HashSet::new();
    buf.push('(');

    let borrowed = start.borrow();
    borrowed.0.display_fmt(buf);
    seen.insert(Rc::as_ptr(start) as usize);

    let mut cur = borrowed.1.clone();
    drop(borrowed);

    loop {
        let p = match cur {
            Value::Pair(p) => p,
            Value::List(items) if items.is_empty() => { let _ = items; break; }
            other => { buf.push_str(" . "); other.display_fmt(buf); break; }
        };
        let ptr = Rc::as_ptr(&p) as usize;
        if !seen.insert(ptr) {
            buf.push_str(" . ...");
            break;
        }
        let b = p.borrow();
        buf.push(' ');
        b.0.display_fmt(buf);
        cur = b.1.clone();
    }
    buf.push(')');
}
