use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::scheme::env::Env;

/// Visited-pair set for cycle detection in deep equality.
type VisitedPairs = Vec<(*const RefCell<(Value, Value)>, *const RefCell<(Value, Value)>)>;

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
    /// Mutable pair with shared identity via Rc.
    Pair(Rc<RefCell<(Value, Value)>>),
    Vector(Rc<RefCell<Vec<Value>>>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Vec<Value>, Value)>,
        def_env: Rc<RefCell<Env>>,
    },
    /// A syntax-case transformer macro: a lambda that takes syntax and returns syntax.
    TransformerMacro {
        transformer: Box<Value>,
    },
    Void,
    /// Multiple return values from `(values ...)`.
    Values(Vec<Value>),
    /// Record instance: (type_id, type_name, field_names, field_values).
    Record {
        type_id: u64,
        type_name: String,
        fields: Vec<(String, Value)>,
    },
}

/// Construct a mutable pair value.
pub fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new((car, cdr))))
}

/// Build a proper list (pair chain ending in nil) from a Vec of values.
pub fn list_from_vec(items: Vec<Value>) -> Value {
    items
        .into_iter()
        .rev()
        .fold(Value::List(vec![]), |acc, item| make_pair(item, acc))
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
            (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
            (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (Value::Continuation(a), Value::Continuation(b)) => a == b,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            (Value::TransformerMacro { .. }, Value::TransformerMacro { .. }) => false,
            (Value::Values(a), Value::Values(b)) => a == b,
            (Value::Void, Value::Void) => true,
            (Value::Record { type_id: a, .. }, Value::Record { type_id: b, .. }) => a == b,
            _ => false,
        }
    }
}

fn write_pair(f: &mut fmt::Formatter<'_>, pair_rc: &Rc<RefCell<(Value, Value)>>) -> fmt::Result {
    write!(f, "(")?;
    let mut visited: Vec<*const RefCell<(Value, Value)>> = Vec::new();
    let mut current = Rc::clone(pair_rc);
    let mut first = true;
    loop {
        let ptr = Rc::as_ptr(&current);
        if visited.contains(&ptr) {
            write!(f, " ...")?;
            break;
        }
        visited.push(ptr);

        if !first {
            write!(f, " ")?;
        }
        first = false;

        let (car, cdr) = {
            let inner = current.borrow();
            (inner.0.clone(), inner.1.clone())
        };

        write!(f, "{car}")?;

        match cdr {
            Value::Pair(next) => {
                current = next;
                continue;
            }
            Value::List(ref items) if items.is_empty() => break,
            Value::List(ref items) => {
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
        self.deep_equal_inner(other, &mut Vec::new())
    }

    fn deep_equal_inner(
        &self,
        other: &Self,
        visited: &mut VisitedPairs,
    ) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => {
                a.len() == b.len()
                    && a.iter().zip(b).all(|(x, y)| x.deep_equal_inner(y, visited))
            }
            (Value::Pair(a), Value::Pair(b)) => deep_equal_pairs(a, b, visited),
            // Cross-type: Pair chain vs List
            (Value::Pair(p), Value::List(items)) | (Value::List(items), Value::Pair(p)) => {
                deep_equal_pair_list(p, items, visited)
            }
            (Value::Vector(a), Value::Vector(b)) => {
                let a = a.borrow();
                let b = b.borrow();
                a.len() == b.len()
                    && a.iter()
                        .zip(b.iter())
                        .all(|(x, y)| x.deep_equal_inner(y, visited))
            }
            (Value::Values(a), Value::Values(b)) => {
                a.len() == b.len()
                    && a.iter().zip(b).all(|(x, y)| x.deep_equal_inner(y, visited))
            }
            (
                Value::Record {
                    type_id: a_id,
                    fields: a_fields,
                    ..
                },
                Value::Record {
                    type_id: b_id,
                    fields: b_fields,
                    ..
                },
            ) => {
                a_id == b_id
                    && a_fields.len() == b_fields.len()
                    && a_fields
                        .iter()
                        .zip(b_fields)
                        .all(|((_, av), (_, bv))| av.deep_equal_inner(bv, visited))
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

fn deep_equal_pairs(
    a: &Rc<RefCell<(Value, Value)>>,
    b: &Rc<RefCell<(Value, Value)>>,
    visited: &mut VisitedPairs,
) -> bool {
    if Rc::ptr_eq(a, b) {
        return true;
    }
    let pair = (Rc::as_ptr(a), Rc::as_ptr(b));
    if visited.contains(&pair) {
        return true;
    }
    visited.push(pair);
    let a_inner = a.borrow();
    let b_inner = b.borrow();
    a_inner.0.deep_equal_inner(&b_inner.0, visited)
        && a_inner.1.deep_equal_inner(&b_inner.1, visited)
}

fn deep_equal_pair_list(
    p: &Rc<RefCell<(Value, Value)>>,
    items: &[Value],
    visited: &mut VisitedPairs,
) -> bool {
    if items.is_empty() {
        return false;
    }
    let inner = p.borrow();
    inner.0.deep_equal_inner(&items[0], visited)
        && inner.1.deep_equal_inner(&Value::List(items[1..].to_vec()), visited)
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
            Value::Pair(p) => write_pair(f, p),
            Value::Vector(v) => write_vector(f, v),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Lambda { .. }
            | Value::Builtin(_)
            | Value::Continuation(_)
            | Value::Macro { .. }
            | Value::TransformerMacro { .. } => {
                write!(f, "#<procedure>")
            }
            Value::Record { type_name, .. } => write!(f, "#<record:{type_name}>"),
            Value::Void => write!(f, "#<void>"),
            Value::Values(vals) => write_values(f, vals),
        }
    }
}
