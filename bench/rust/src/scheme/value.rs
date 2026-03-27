use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::scheme::env::Env;

static RECORD_TYPE_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn next_record_type_id() -> usize {
    RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed)
}

#[derive(Debug, Clone, PartialEq)]
pub enum RecordProcKind {
    Constructor { field_names: Vec<std::string::String> },
    Predicate,
    Accessor { field_index: usize },
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Rational(i64, i64), // numerator, denominator (always simplified, den > 0)
    Float(f64),
    Boolean(bool),
    String(std::string::String, bool), // (content, mutable)
    Symbol(std::string::String),
    List(Vec<Value>),
    Lambda {
        params: Vec<std::string::String>,
        rest_param: Option<std::string::String>,
        body: Vec<Value>,
        env: Rc<RefCell<Env>>,
    },
    Char(char),
    Pair(Box<Value>, Box<Value>),
    Void,
    SyntaxRules {
        literals: Vec<std::string::String>,
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
        clauses: Vec<(Vec<std::string::String>, Option<std::string::String>, Vec<Value>, Rc<RefCell<Env>>)>,
    },
    Vector(Rc<RefCell<Vec<Value>>>),
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
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Void, Value::Void) => true,
            (Value::Lambda { .. }, Value::Lambda { .. }) => false,
            (Value::SyntaxRules { .. }, Value::SyntaxRules { .. }) => false,
            (Value::Record { type_id: a, fields: af }, Value::Record { type_id: b, fields: bf }) => a == b && af == bf,
            (Value::RecordProc { .. }, Value::RecordProc { .. }) => false,
            (Value::CaseLambda { .. }, Value::CaseLambda { .. }) => false,
            (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Value {
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

    pub fn to_display(&self) -> std::string::String {
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
                let inner: Vec<std::string::String> =
                    elems.iter().map(|v| v.to_display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Char(c) => match c {
                ' ' => "#\\space".into(),
                '\n' => "#\\newline".into(),
                '\t' => "#\\tab".into(),
                _ => format!("#\\{}", c),
            },
            Value::Pair(a, b) => format!("({} . {})", a.to_display(), b.to_display()),
            Value::Lambda { .. } => "#<procedure>".into(),
            Value::SyntaxRules { .. } => "#<syntax>".into(),
            Value::Record { .. } => "#<record>".into(),
            Value::RecordProc { .. } => "#<procedure>".into(),
            Value::CaseLambda { .. } => "#<procedure>".into(),
            Value::Vector(v) => {
                let elems = v.borrow();
                let inner: Vec<std::string::String> = elems.iter().map(|v| v.to_display()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Void => "".into(),
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
    pub fn to_display_repr(&self) -> std::string::String {
        match self {
            Value::String(s, _) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::Rational(_, _) | Value::Float(_) => self.to_display(),
            Value::List(elems) => {
                let inner: Vec<std::string::String> =
                    elems.iter().map(|v| v.to_display_repr()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(a, b) => format!("({} . {})", a.to_display_repr(), b.to_display_repr()),
            Value::CaseLambda { .. } => "#<procedure>".into(),
            Value::Vector(v) => {
                let elems = v.borrow();
                let inner: Vec<std::string::String> = elems.iter().map(|v| v.to_display_repr()).collect();
                format!("#({})", inner.join(" "))
            }
            _ => self.to_display(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
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
