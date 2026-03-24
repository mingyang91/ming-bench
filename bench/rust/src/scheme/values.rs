use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

use super::Value;

pub(super) fn values_eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

pub(super) fn values_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => std::ptr::eq(a.as_str(), b.as_str()),
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

pub(super) fn values_equal(a: &Value, b: &Value) -> bool {
    values_equal_depth(a, b, 1000)
}

fn values_equal_depth(a: &Value, b: &Value, depth: usize) -> bool {
    if depth == 0 { return false; }
    match (a, b) {
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::List(a), Value::List(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal_depth(x, y, depth - 1))
        }
        (Value::Pair(a), Value::Pair(b)) => {
            if Rc::ptr_eq(a, b) { return true; }
            let (a_car, a_cdr) = {
                let ab = a.borrow();
                (ab.0.clone(), ab.1.clone())
            };
            let (b_car, b_cdr) = {
                let bb = b.borrow();
                (bb.0.clone(), bb.1.clone())
            };
            values_equal_depth(&a_car, &b_car, depth - 1) && values_equal_depth(&a_cdr, &b_cdr, depth - 1)
        }
        // Cross-type: List vs Pair (both list-like)
        (Value::List(elems), Value::Pair(_)) | (Value::Pair(_), Value::List(elems)) if elems.is_empty() => false,
        (Value::List(_), Value::Pair(_)) | (Value::Pair(_), Value::List(_)) => {
            // Compare element-by-element using car/cdr logic
            let a_car = pair_car(a);
            let b_car = pair_car(b);
            let a_cdr = pair_cdr(a);
            let b_cdr = pair_cdr(b);
            match (a_car, b_car, a_cdr, b_cdr) {
                (Some(ac), Some(bc), Some(ad), Some(bd)) => {
                    values_equal_depth(&ac, &bc, depth - 1) && values_equal_depth(&ad, &bd, depth - 1)
                }
                _ => false,
            }
        }
        (Value::Vector(a), Value::Vector(b)) => {
            let a = a.borrow();
            let b = b.borrow();
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal_depth(x, y, depth - 1))
        }
        _ => false,
    }
}

fn pair_car(v: &Value) -> Option<Value> {
    match v {
        Value::List(elems) if !elems.is_empty() => Some(elems[0].clone()),
        Value::Pair(p) => Some(p.borrow().0.clone()),
        _ => None,
    }
}

fn pair_cdr(v: &Value) -> Option<Value> {
    match v {
        Value::List(elems) if !elems.is_empty() => Some(Value::List(elems[1..].to_vec())),
        Value::Pair(p) => Some(p.borrow().1.clone()),
        _ => None,
    }
}

fn advance_pair(v: &Value) -> Option<Value> {
    match v {
        Value::Pair(p) => Some(p.borrow().1.clone()),
        _ => None,
    }
}

pub(super) fn is_proper_list(v: &Value) -> bool {
    match v {
        Value::List(_) => true,
        Value::Pair(_) => {
            // Floyd's cycle detection
            let mut slow = v.clone();
            let mut fast = v.clone();
            loop {
                // Advance fast by 2
                for _ in 0..2 {
                    match advance_pair(&fast) {
                        Some(next) => fast = next,
                        None => return matches!(fast, Value::List(_)),
                    }
                }
                // Advance slow by 1
                match advance_pair(&slow) {
                    Some(next) => slow = next,
                    None => return matches!(slow, Value::List(_)),
                }
                // Check cycle
                if let (Value::Pair(s), Value::Pair(f)) = (&slow, &fast) {
                    if Rc::ptr_eq(s, f) {
                        return false;
                    }
                }
            }
        }
        _ => false,
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Float(v) => {
                let s = format!("{v}");
                if s.contains('.') || s.contains('e') || s.contains('E') || s.contains("inf") || s.contains("NaN") {
                    write!(f, "{s}")
                } else {
                    write!(f, "{s}.0")
                }
            }
            Value::Rational(n, d) => write!(f, "{n}/{d}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Value::Pair(p) => {
                let mut seen = HashSet::new();
                seen.insert(Rc::as_ptr(p) as usize);
                let pair = p.borrow();
                write!(f, "({}", pair.0)?;
                let mut cur = pair.1.clone();
                drop(pair);
                loop {
                    match &cur {
                        Value::Pair(p2) => {
                            let ptr = Rc::as_ptr(p2) as usize;
                            if !seen.insert(ptr) {
                                write!(f, " ...")?;
                                break;
                            }
                            let p2b = p2.borrow();
                            write!(f, " {}", p2b.0)?;
                            let next = p2b.1.clone();
                            drop(p2b);
                            cur = next;
                        }
                        Value::List(elems) if elems.is_empty() => break,
                        Value::List(elems) => {
                            for e in elems {
                                write!(f, " {e}")?;
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
            Value::Vector(v) => {
                let elems = v.borrow();
                write!(f, "#(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Value::Char(c) => match c {
                ' ' => write!(f, "#\\space"),
                '\n' => write!(f, "#\\newline"),
                '\t' => write!(f, "#\\tab"),
                _ => write!(f, "#\\{c}"),
            },
            Value::Lambda { .. } | Value::CaseLambda { .. } | Value::Continuation(..) => write!(f, "#<procedure>"),
            Value::Builtin(name) => write!(f, "#<builtin:{name}>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
            Value::Record { type_name, .. } => write!(f, "#<record:{type_name}>"),
            Value::RecordConstructor { .. }
            | Value::RecordPredicate { .. }
            | Value::RecordAccessor { .. } => write!(f, "#<procedure>"),
            Value::Values(vals) => {
                for (i, v) in vals.iter().enumerate() {
                    if i > 0 { writeln!(f)?; }
                    write!(f, "{v}")?;
                }
                Ok(())
            }
            Value::Void => write!(f, ""),
        }
    }
}

impl Value {
    /// Format for `display` — strings without quotes.
    pub(super) fn display_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Str(s) => write!(f, "{s}"),
            Value::Char(c) => write!(f, "{c}"),
            Value::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    e.display_fmt(f)?;
                }
                write!(f, ")")
            }
            Value::Pair(p) => {
                let mut seen = HashSet::new();
                seen.insert(Rc::as_ptr(p) as usize);
                let pair = p.borrow();
                write!(f, "(")?;
                pair.0.display_fmt(f)?;
                let mut cur = pair.1.clone();
                drop(pair);
                loop {
                    match &cur {
                        Value::Pair(p2) => {
                            let ptr = Rc::as_ptr(p2) as usize;
                            if !seen.insert(ptr) {
                                write!(f, " ...")?;
                                break;
                            }
                            let p2b = p2.borrow();
                            write!(f, " ")?;
                            p2b.0.display_fmt(f)?;
                            let next = p2b.1.clone();
                            drop(p2b);
                            cur = next;
                        }
                        Value::List(elems) if elems.is_empty() => break,
                        Value::List(elems) => {
                            for e in elems {
                                write!(f, " ")?;
                                e.display_fmt(f)?;
                            }
                            break;
                        }
                        other => {
                            write!(f, " . ")?;
                            other.display_fmt(f)?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Value::Vector(v) => {
                let elems = v.borrow();
                write!(f, "#(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    e.display_fmt(f)?;
                }
                write!(f, ")")
            }
            Value::Macro { .. } => write!(f, "#<macro>"),
            other => fmt::Display::fmt(other, f),
        }
    }
}

pub(super) struct DisplayValue<'a>(pub(super) &'a Value);
impl<'a> fmt::Display for DisplayValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.display_fmt(f)
    }
}
