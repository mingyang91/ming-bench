use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

use crate::scheme::env::Env;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Rational(i64, i64),
    Bool(bool),
    String(String),
    Char(char),
    Symbol(String, Option<Span>),
    List(Vec<Value>, Option<Span>),
    Builtin(String),
    Closure {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Value>,
        env: Rc<RefCell<Env>>,
    },
    Pair(Rc<RefCell<(Value, Value)>>),
    Continuation(usize),
    SyntaxRules {
        literals: Vec<String>,
        rules: Vec<(Value, Value)>,
        def_env: Rc<RefCell<Env>>,
    },
    Vector(Rc<RefCell<Vec<Value>>>),
    Values(Vec<Value>),
    Record {
        type_id: usize,
        fields: Vec<Value>,
    },
    MacroTransformer {
        params: Vec<String>,
        body: Vec<Value>,
        env: Rc<RefCell<Env>>,
    },
    CaseLambda {
        clauses: Vec<(Vec<String>, Option<String>, Vec<Value>)>,
        env: Rc<RefCell<Env>>,
    },
    Void,
}

impl Value {
    pub fn span(&self) -> Option<Span> {
        match self {
            Value::Symbol(_, span) | Value::List(_, span) => *span,
            Value::Int(_) | Value::Float(_) | Value::Rational(_, _)
            | Value::Bool(_) | Value::String(_) | Value::Char(_)
            | Value::Builtin(_) | Value::Closure { .. } | Value::Pair(_)
            | Value::Continuation(_) | Value::SyntaxRules { .. }
            | Value::Vector(_) | Value::Values(_) | Value::Record { .. }
            | Value::MacroTransformer { .. } | Value::CaseLambda { .. } | Value::Void => None,
        }
    }

    /// Create a mutable pair (cons cell).
    pub fn cons(car: Value, cdr: Value) -> Value {
        Value::Pair(Rc::new(RefCell::new((car, cdr))))
    }

    /// Build a proper list from a Vec, as a chain of mutable pairs ending in nil.
    pub fn list_from_vec(items: Vec<Value>) -> Value {
        items.into_iter().rev().fold(
            Value::List(vec![], None),
            |acc, item| Value::cons(item, acc),
        )
    }

    /// Extract car of a pair or non-empty list.
    pub fn car(&self) -> Option<Value> {
        match self {
            Value::List(elems, _) if !elems.is_empty() => Some(elems[0].clone()),
            Value::Pair(cell) => Some(cell.borrow().0.clone()),
            _ => None,
        }
    }

    /// Convert a proper list (List or pair chain) to a Vec.
    /// Returns None for improper lists or cycles.
    pub fn to_vec(&self) -> Option<Vec<Value>> {
        match self {
            Value::List(elems, _) => Some(elems.clone()),
            Value::Pair(_) => pair_chain_to_vec(self),
            _ => None,
        }
    }
}

/// Walk a Pair chain collecting elements. Returns None for improper lists or cycles.
fn pair_chain_to_vec(start: &Value) -> Option<Vec<Value>> {
    let mut result = Vec::new();
    let mut cur = start.clone();
    let mut slow = start.clone();
    let mut step = 0u64;
    loop {
        match cur {
            Value::Pair(ref cell) => {
                let (car, cdr) = {
                    let borrowed = cell.borrow();
                    (borrowed.0.clone(), borrowed.1.clone())
                };
                result.push(car);
                cur = cdr;
            }
            Value::List(ref elems, _) => {
                result.extend(elems.iter().cloned());
                return Some(result);
            }
            _ => return None,
        }
        step += 1;
        if !step.is_multiple_of(2) {
            continue;
        }
        slow = match slow {
            Value::Pair(ref cell) => cell.borrow().1.clone(),
            _ => return Some(result),
        };
        if pairs_are_same(&cur, &slow) {
            return None; // cycle detected
        }
    }
}

fn pairs_are_same(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Pair(x), Value::Pair(y)) => Rc::ptr_eq(x, y),
        _ => false,
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Symbol(a, _), Value::Symbol(b, _)) => a == b,
            (Value::List(a, _), Value::List(b, _)) => a == b,
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (Value::Void, Value::Void) => true,
            (Value::Closure { .. }, Value::Closure { .. }) => false,
            (Value::CaseLambda { .. }, Value::CaseLambda { .. }) => false,
            (Value::Pair(a), Value::Pair(b)) => {
                if Rc::ptr_eq(a, b) {
                    return true;
                }
                let ab = a.borrow();
                let bb = b.borrow();
                ab.0 == bb.0 && ab.1 == bb.1
            }
            (Value::Continuation(a), Value::Continuation(b)) => a == b,
            (Value::SyntaxRules { .. }, Value::SyntaxRules { .. }) => false,
            (Value::MacroTransformer { .. }, Value::MacroTransformer { .. }) => false,
            (Value::Vector(a), Value::Vector(b)) => *a.borrow() == *b.borrow(),
            (Value::Values(a), Value::Values(b)) => a == b,
            (Value::Record { type_id: a_id, fields: a_f },
             Value::Record { type_id: b_id, fields: b_f }) => a_id == b_id && a_f == b_f,
            // Cross-type: pair chain vs non-empty List
            (Value::Pair(_), Value::List(elems, _)) | (Value::List(elems, _), Value::Pair(_))
                if !elems.is_empty() => {
                // Compare element-by-element
                let (pair_val, list_elems) = if matches!(self, Value::Pair(_)) {
                    (self, elems)
                } else {
                    (other, elems)
                };
                let mut cur = pair_val.clone();
                let mut idx = 0;
                loop {
                    match cur {
                        Value::Pair(ref cell) => {
                            if idx >= list_elems.len() {
                                return false; // pair chain is longer
                            }
                            let (car, cdr) = {
                                let b = cell.borrow();
                                (b.0.clone(), b.1.clone())
                            };
                            if car != list_elems[idx] {
                                return false;
                            }
                            idx += 1;
                            cur = cdr;
                        }
                        Value::List(ref v, _) if v.is_empty() => {
                            return idx == list_elems.len();
                        }
                        _ => return false,
                    }
                }
            }
            // Cross-type: pair chain vs empty list
            (Value::Pair(_), Value::List(v, _)) | (Value::List(v, _), Value::Pair(_))
                if v.is_empty() => false,
            (Value::Int(_), _)
            | (Value::Float(_), _)
            | (Value::Rational(_, _), _)
            | (Value::Bool(_), _)
            | (Value::String(_), _)
            | (Value::Char(_), _)
            | (Value::Symbol(_, _), _)
            | (Value::List(_, _), _)
            | (Value::Pair(_), _)
            | (Value::Builtin(_), _)
            | (Value::Closure { .. }, _)
            | (Value::Continuation(_), _)
            | (Value::SyntaxRules { .. }, _)
            | (Value::Vector(_), _)
            | (Value::Values(_), _)
            | (Value::Record { .. }, _)
            | (Value::MacroTransformer { .. }, _)
            | (Value::CaseLambda { .. }, _)
            | (Value::Void, _) => false,
        }
    }
}

/// Format a pair chain with cycle detection for Display.
fn fmt_pair(cell: &Rc<RefCell<(Value, Value)>>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(")?;
    let mut visited: HashSet<*const RefCell<(Value, Value)>> = HashSet::new();
    let mut cur_cell = Rc::clone(cell);
    let mut first = true;
    loop {
        let ptr = Rc::as_ptr(&cur_cell);
        if !first && visited.contains(&ptr) {
            write!(f, " ...")?;
            break;
        }
        visited.insert(ptr);
        let borrowed = cur_cell.borrow();
        if !first {
            write!(f, " ")?;
        }
        first = false;
        write!(f, "{}", borrowed.0)?;
        match &borrowed.1 {
            Value::List(v, _) if v.is_empty() => break,
            Value::Pair(next) => {
                let next = Rc::clone(next);
                drop(borrowed);
                cur_cell = next;
            }
            Value::List(elems, _) => {
                for elem in elems {
                    write!(f, " {elem}")?;
                }
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

/// Display a pair chain for `display` builtin (strings without quotes).
pub fn display_pair(cell: &Rc<RefCell<(Value, Value)>>) -> String {
    let mut out = String::from("(");
    let mut visited: HashSet<*const RefCell<(Value, Value)>> = HashSet::new();
    let mut cur_cell = Rc::clone(cell);
    let mut first = true;
    loop {
        let ptr = Rc::as_ptr(&cur_cell);
        if !first && visited.contains(&ptr) {
            out.push_str(" ...");
            break;
        }
        visited.insert(ptr);
        let borrowed = cur_cell.borrow();
        if !first {
            out.push(' ');
        }
        first = false;
        out.push_str(&display_value_inner(&borrowed.0));
        match &borrowed.1 {
            Value::List(v, _) if v.is_empty() => break,
            Value::Pair(next) => {
                let next = Rc::clone(next);
                drop(borrowed);
                cur_cell = next;
            }
            Value::List(elems, _) => {
                for elem in elems {
                    out.push(' ');
                    out.push_str(&display_value_inner(elem));
                }
                break;
            }
            other => {
                out.push_str(" . ");
                out.push_str(&display_value_inner(other));
                break;
            }
        }
    }
    out.push(')');
    out
}

/// Display helper: strings without quotes, chars as themselves.
pub fn display_value_inner(val: &Value) -> String {
    match val {
        Value::String(s) => s.clone(),
        Value::Char(c) => c.to_string(),
        Value::Pair(cell) => display_pair(cell),
        Value::Vector(elems) => {
            let borrowed = elems.borrow();
            let mut s = "#(".to_string();
            for (i, elem) in borrowed.iter().enumerate() {
                if i > 0 { s.push(' '); }
                s.push_str(&display_value_inner(elem));
            }
            s.push(')');
            s
        }
        other => other.to_string(),
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{n}"),
            Value::Float(v) => {
                if *v == v.floor() && v.is_finite() {
                    write!(f, "{v:.1}")
                } else {
                    write!(f, "{v}")
                }
            }
            Value::Rational(n, d) => write!(f, "{n}/{d}"),
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Char(' ') => write!(f, "#\\space"),
            Value::Char('\n') => write!(f, "#\\newline"),
            Value::Char('\t') => write!(f, "#\\tab"),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Symbol(s, _) => write!(f, "{s}"),
            Value::List(elems, _) => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            Value::Pair(cell) => fmt_pair(cell, f),
            Value::Builtin(name) => write!(f, "#<procedure:{name}>"),
            Value::Closure { .. } => write!(f, "#<procedure>"),
            Value::CaseLambda { .. } => write!(f, "#<procedure>"),
            Value::Continuation(_) => write!(f, "#<continuation>"),
            Value::SyntaxRules { .. } => write!(f, "#<syntax>"),
            Value::Vector(elems) => {
                write!(f, "#(")?;
                let borrowed = elems.borrow();
                for (i, elem) in borrowed.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            Value::Values(vals) => {
                for (i, v) in vals.iter().enumerate() {
                    if i > 0 {
                        writeln!(f)?;
                    }
                    write!(f, "{v}")?;
                }
                Ok(())
            }
            Value::Record { .. } => write!(f, "#<record>"),
            Value::MacroTransformer { .. } => write!(f, "#<syntax>"),
            Value::Void => write!(f, ""),
        }
    }
}
