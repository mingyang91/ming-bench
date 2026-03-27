use std::any::Any;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::scheme::env::Env;

/// Type-erased continuation data, wrapping an `Rc<Cont>` from eval.rs.
#[derive(Clone)]
pub struct ContData(pub Rc<dyn Any>);

impl fmt::Debug for ContData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<continuation>")
    }
}

static RECORD_TYPE_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

pub fn next_record_type_id() -> usize {
    RECORD_TYPE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

#[derive(Debug, Clone, PartialEq)]
pub enum RecordProcKind {
    Constructor { field_names: Vec<String> },
    Predicate,
    Accessor { field_index: usize },
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Rational(i64, i64), // numerator, denominator (always simplified, den > 0)
    Float(f64),
    Boolean(bool),
    String(String, bool), // (content, mutable)
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Value>,
        env: Rc<RefCell<Env>>,
    },
    Char(char),
    Pair(Rc<RefCell<(Value, Value)>>),
    Void,
    SyntaxRules {
        literals: Vec<String>,
        rules: Vec<(Value, Value)>,
        def_env: Rc<RefCell<Env>>,
    },
    Record {
        type_id: usize,
        fields: Vec<Value>,
    },
    RecordProc {
        type_id: usize,
        kind: RecordProcKind,
    },
    CaseLambda {
        clauses: Vec<(Vec<String>, Option<String>, Vec<Value>, Rc<RefCell<Env>>)>,
    },
    Vector(Rc<RefCell<Vec<Value>>>),
    Continuation(ContData),
    /// Multiple return values (internal; only consumed by call-with-values)
    Values(Vec<Value>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a, _), Value::String(b, _)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
            (Value::Void, Value::Void) => true,
            (Value::Lambda { .. }, Value::Lambda { .. }) => false,
            (Value::SyntaxRules { .. }, Value::SyntaxRules { .. }) => false,
            (Value::Record { type_id: a, fields: af }, Value::Record { type_id: b, fields: bf }) => a == b && af == bf,
            (Value::RecordProc { .. }, Value::RecordProc { .. }) => false,
            (Value::CaseLambda { .. }, Value::CaseLambda { .. }) => false,
            (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
            (Value::Continuation(_), Value::Continuation(_)) => false,
            (Value::Values(a), Value::Values(b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    pub fn is_self_evaluating(&self) -> bool {
        matches!(self,
            Value::Integer(_) | Value::Rational(..) | Value::Float(_)
            | Value::Boolean(_) | Value::String(..) | Value::Char(_)
            | Value::Lambda { .. } | Value::Pair(_) | Value::SyntaxRules { .. }
            | Value::Record { .. } | Value::RecordProc { .. }
            | Value::CaseLambda { .. } | Value::Vector(_) | Value::Continuation(_)
            | Value::Void | Value::Values(_)
        )
    }

    pub fn new_pair(car: Value, cdr: Value) -> Value {
        Value::Pair(Rc::new(RefCell::new((car, cdr))))
    }

    pub fn make_rational(num: i64, den: i64) -> Value {
        if den == 0 {
            panic!("rational with zero denominator");
        }
        let sign = if den < 0 { -1 } else { 1 };
        let num = num * sign;
        let den = den.abs();
        let g = gcd(num.unsigned_abs(), den as u64) as i64;
        let num = num / g;
        let den = den / g;
        if den == 1 {
            Value::Integer(num)
        } else {
            Value::Rational(num, den)
        }
    }

    pub fn to_display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Float(f) => {
                if f.fract() == 0.0 && f.is_finite() {
                    format!("{:.1}", f)
                } else {
                    format!("{}", f)
                }
            }
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::String(s, _) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<String> =
                    elems.iter().map(|v| v.to_display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Char(c) => match c {
                ' ' => "#\\space".into(),
                '\n' => "#\\newline".into(),
                '\t' => "#\\tab".into(),
                _ => format!("#\\{}", c),
            },
            Value::Pair(p) => display_pair_chain(p, false),
            Value::Continuation(_) => "#<continuation>".into(),
            Value::Lambda { .. } => "#<procedure>".into(),
            Value::SyntaxRules { .. } => "#<syntax>".into(),
            Value::Record { .. } => "#<record>".into(),
            Value::RecordProc { .. } => "#<procedure>".into(),
            Value::CaseLambda { .. } => "#<procedure>".into(),
            Value::Vector(v) => {
                let elems = v.borrow();
                let inner: Vec<String> = elems.iter().map(|v| v.to_display()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Void => "".into(),
            Value::Values(vs) => {
                if vs.is_empty() { "".into() }
                else { vs.last().unwrap().to_display() }
            }
        }
    }

    pub fn to_f64(&self) -> Option<f64> {
        match self {
            Value::Integer(n) => Some(*n as f64),
            Value::Rational(n, d) => Some(*n as f64 / *d as f64),
            Value::Float(f) => Some(*f),
            _ => None,
        }
    }

    /// Display representation (no quotes on strings, chars as raw)
    pub fn to_display_repr(&self) -> String {
        match self {
            Value::String(s, _) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::Rational(_, _) | Value::Float(_) => self.to_display(),
            Value::List(elems) => {
                let inner: Vec<String> =
                    elems.iter().map(|v| v.to_display_repr()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(p) => display_pair_chain(p, true),
            Value::CaseLambda { .. } => "#<procedure>".into(),
            Value::Vector(v) => {
                let elems = v.borrow();
                let inner: Vec<String> = elems.iter().map(|v| v.to_display_repr()).collect();
                format!("#({})", inner.join(" "))
            }
            _ => self.to_display(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

/// Display a pair chain as a proper or improper list.
/// Handles cycles by tracking visited Rc pointers.
fn display_pair_chain(p: &Rc<RefCell<(Value, Value)>>, display_mode: bool) -> String {
    let mut parts = Vec::new();
    let mut cur_rc = Rc::clone(p);
    let mut seen: Vec<*const RefCell<(Value, Value)>> = Vec::new();

    loop {
        let ptr = Rc::as_ptr(&cur_rc);
        if seen.iter().any(|s| std::ptr::eq(*s, ptr)) {
            break; // cycle detected
        }
        seen.push(ptr);

        let (car_val, cdr_val) = {
            let pair = cur_rc.borrow();
            (pair.0.clone(), pair.1.clone())
        };

        if display_mode {
            parts.push(car_val.to_display_repr());
        } else {
            parts.push(car_val.to_display());
        }

        match cdr_val {
            Value::Pair(next) => {
                cur_rc = next;
            }
            Value::List(ref elems) if elems.is_empty() => {
                return format!("({})", parts.join(" "));
            }
            Value::List(ref elems) => {
                // Non-empty List as cdr: append its elements
                for elem in elems {
                    if display_mode {
                        parts.push(elem.to_display_repr());
                    } else {
                        parts.push(elem.to_display());
                    }
                }
                return format!("({})", parts.join(" "));
            }
            other => {
                let tail = if display_mode {
                    other.to_display_repr()
                } else {
                    other.to_display()
                };
                return format!("({} . {})", parts.join(" "), tail);
            }
        }
    }

    format!("({})", parts.join(" "))
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display())
    }
}

pub fn gcd(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}
