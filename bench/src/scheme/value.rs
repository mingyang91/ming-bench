use std::cell::RefCell;
use std::collections::HashSet;
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
    /// AST node for parsed s-expressions. Not used for runtime list values.
    List(Vec<Value>),
    /// A mutable cons cell with reference semantics.
    Pair(Rc<RefCell<(Value, Value)>>),
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
    /// A record instance created by `define-record-type`.
    Record {
        type_id: u64,
        type_name: std::string::String,
        fields: Vec<Value>,
    },
    /// Constructor procedure for a record type.
    RecordConstructor {
        type_id: u64,
        type_name: std::string::String,
        field_count: usize,
    },
    /// Predicate procedure for a record type.
    RecordPredicate {
        type_id: u64,
    },
    /// Accessor procedure for a record field.
    RecordAccessor {
        type_id: u64,
        field_index: usize,
    },
    Void,
}

/// Create a new cons cell (mutable pair with reference semantics).
pub fn cons(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new((car, cdr))))
}

/// The nil / empty list value.
pub fn nil() -> Value {
    Value::List(vec![])
}

/// Build a proper list (pair chain ending in nil) from elements.
pub fn vec_to_list(elems: Vec<Value>) -> Value {
    elems.into_iter().rev().fold(nil(), |acc, elem| cons(elem, acc))
}

/// Extract elements from a value representing a proper list.
/// Handles both `List(Vec)` (AST/quoted) and Pair chains ending in nil.
/// Returns `None` for improper or circular lists.
pub fn proper_list_elems(val: &Value) -> Option<Vec<Value>> {
    match val {
        Value::List(elems) => Some(elems.clone()),
        Value::Pair(_) => collect_pair_elems(val),
        _ => None,
    }
}

/// Walk a Pair chain collecting elements. Returns `None` for improper or circular lists.
fn collect_pair_elems(val: &Value) -> Option<Vec<Value>> {
    let mut result = Vec::new();
    let mut current = val.clone();
    let mut seen = HashSet::new();
    loop {
        let cell = match &current {
            Value::Pair(cell) => cell.clone(),
            Value::List(elems) if elems.is_empty() => return Some(result),
            _ => return None,
        };
        if !seen.insert(Rc::as_ptr(&cell) as usize) {
            return None;
        }
        let pair = cell.borrow();
        result.push(pair.0.clone());
        let next = pair.1.clone();
        drop(pair);
        current = next;
    }
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
            (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
            (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
            (Value::Values(a), Value::Values(b)) => a == b,
            (Value::Void, Value::Void) => true,
            (Value::Lambda { .. }, Value::Lambda { .. }) => false,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            (Value::Continuation(a), Value::Continuation(b)) => a.id == b.id,
            (
                Value::Record { type_id: a_id, fields: a_fields, .. },
                Value::Record { type_id: b_id, fields: b_fields, .. },
            ) => a_id == b_id && a_fields == b_fields,
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

/// Format a Pair chain, printing proper lists as `(1 2 3)` and improper as `(1 . 2)`.
/// Detects cycles to avoid infinite loops.
fn fmt_pair(cell: &Rc<RefCell<(Value, Value)>>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut seen = HashSet::new();
    seen.insert(Rc::as_ptr(cell) as usize);

    write!(f, "(")?;
    let pair = cell.borrow();
    write!(f, "{}", pair.0)?;
    let mut current = pair.1.clone();
    drop(pair);

    fmt_pair_tail(&mut current, &mut seen, f)?;
    write!(f, ")")
}

fn fmt_pair_tail(
    current: &mut Value,
    seen: &mut HashSet<usize>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    loop {
        let next_cell = match current {
            Value::Pair(cell) => cell.clone(),
            Value::List(elems) if elems.is_empty() => return Ok(()),
            Value::List(elems) => {
                return elems.iter().try_for_each(|elem| write!(f, " {elem}"));
            }
            other => return write!(f, " . {other}"),
        };
        if !seen.insert(Rc::as_ptr(&next_cell) as usize) {
            return write!(f, " . <cycle>");
        }
        let p = next_cell.borrow();
        write!(f, " {}", p.0)?;
        let next = p.1.clone();
        drop(p);
        *current = next;
    }
}

/// Display-format a Pair chain (strings without quotes, chars as raw).
fn display_pair(cell: &Rc<RefCell<(Value, Value)>>) -> std::string::String {
    let mut seen = HashSet::new();
    seen.insert(Rc::as_ptr(cell) as usize);

    let mut out = std::string::String::from("(");
    let pair = cell.borrow();
    out.push_str(&pair.0.display_str());
    let mut current = pair.1.clone();
    drop(pair);

    loop {
        let next_cell = match &current {
            Value::Pair(cell) => cell.clone(),
            Value::List(elems) if elems.is_empty() => break,
            Value::List(elems) => {
                let suffix: std::string::String = elems.iter().map(|e| format!(" {}", e.display_str())).collect();
                out.push_str(&suffix);
                break;
            }
            other => {
                out.push_str(" . ");
                out.push_str(&other.display_str());
                break;
            }
        };
        if !seen.insert(Rc::as_ptr(&next_cell) as usize) {
            out.push_str(" . <cycle>");
            break;
        }
        let p = next_cell.borrow();
        out.push(' ');
        out.push_str(&p.0.display_str());
        let next = p.1.clone();
        drop(p);
        current = next;
    }
    out.push(')');
    out
}

impl Value {
    /// Format for `display` — strings without quotes, chars as raw characters.
    pub fn display_str(&self) -> std::string::String {
        match self {
            Value::String(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::List(elems) => {
                let inner: Vec<std::string::String> = elems.iter().map(|e| e.display_str()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(cell) => display_pair(cell),
            Value::Vector(cells) => {
                let elems = cells.borrow();
                let inner: Vec<std::string::String> = elems.iter().map(|e| e.display_str()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Macro { .. } => "#<macro>".to_string(),
            Value::Values(vals) => vals.iter().map(|v| v.display_str()).collect::<Vec<_>>().join("\n"),
            Value::RecordConstructor { .. }
            | Value::RecordPredicate { .. }
            | Value::RecordAccessor { .. } => "#<procedure>".to_string(),
            other => other.to_string(),
        }
    }

    /// Check if this value is the empty list (nil).
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::List(elems) if elems.is_empty())
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
            Value::Pair(cell) => fmt_pair(cell, f),
            Value::Vector(cells) => fmt_vector(&cells.borrow(), f),
            Value::Values(vals) => fmt_values(vals, f),
            Value::Lambda { .. } | Value::Continuation(_) => write!(f, "#<procedure>"),
            Value::RecordConstructor { .. }
            | Value::RecordPredicate { .. }
            | Value::RecordAccessor { .. } => write!(f, "#<procedure>"),
            Value::Record { type_name, .. } => write!(f, "#<record:{type_name}>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
            Value::Void => write!(f, ""),
        }
    }
}
