use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};
use crate::scheme::env::Env;

static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);

pub fn next_record_type_id() -> u64 {
    RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub type Pos = (usize, usize);

#[derive(Debug, Clone)]
pub enum ValueKind {
    Integer(i64),
    Rational(i64, i64), // numerator, denominator (always simplified, den > 0)
    Float(f64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Value>,
        env: Rc<Env>,
    },
    SyntaxRules {
        literals: Vec<String>,
        rules: Vec<(Value, Value)>,
        def_env: Rc<Env>,
    },
    Record {
        type_id: u64,
        type_name: String,
        fields: Vec<(String, Value)>,
    },
    RecordConstructor {
        type_id: u64,
        type_name: String,
        field_names: Vec<String>,
    },
    RecordPredicate {
        type_id: u64,
    },
    RecordAccessor {
        type_id: u64,
        type_name: String,
        field_name: String,
    },
    CaseLambda {
        clauses: Vec<(Vec<String>, Option<String>, Vec<Value>, Rc<Env>)>,
    },
    Void,
}

pub fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 { let t = b; b = a % b; a = t; }
    a
}

pub fn make_rational_kind(num: i64, den: i64) -> ValueKind {
    let (num, den) = if den < 0 { (-num, -den) } else { (num, den) };
    let g = gcd(num.abs(), den);
    let (num, den) = (num / g, den / g);
    if den == 1 { ValueKind::Integer(num) } else { ValueKind::Rational(num, den) }
}

#[derive(Debug, Clone, Copy)]
pub enum NumVal {
    Int(i64),
    Rat(i64, i64),
    Flt(f64),
}

impl NumVal {
    pub fn to_f64(self) -> f64 {
        match self {
            NumVal::Int(n) => n as f64,
            NumVal::Rat(n, d) => n as f64 / d as f64,
            NumVal::Flt(f) => f,
        }
    }

    pub fn is_inexact(self) -> bool {
        matches!(self, NumVal::Flt(_))
    }
}

#[derive(Debug, Clone)]
pub struct Value {
    pub kind: ValueKind,
    pub pos: Pos,
}

impl Value {
    pub fn new(kind: ValueKind, pos: Pos) -> Self {
        Value { kind, pos }
    }

    pub fn unpos(kind: ValueKind) -> Self {
        Value { kind, pos: (0, 0) }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self.kind, ValueKind::Boolean(false))
    }

    pub fn to_display(&self) -> String {
        match &self.kind {
            ValueKind::Integer(n) => n.to_string(),
            ValueKind::Rational(n, d) => format!("{}/{}", n, d),
            ValueKind::Float(f) => format_float(*f),
            ValueKind::Boolean(true) => "#t".to_string(),
            ValueKind::Boolean(false) => "#f".to_string(),
            ValueKind::Str(s) => format!("\"{}\"", s),
            ValueKind::Symbol(s) => s.clone(),
            ValueKind::Char(c) => format!("#\\{}", match *c {
                ' ' => "space".to_string(),
                '\n' => "newline".to_string(),
                ch => ch.to_string(),
            }),
            ValueKind::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display()).collect();
                format!("({})", inner.join(" "))
            }
            ValueKind::Lambda { .. } | ValueKind::CaseLambda { .. } => "#<procedure>".to_string(),
            ValueKind::SyntaxRules { .. } => "#<syntax>".to_string(),
            ValueKind::Record { type_name, .. } => format!("#<{}>", type_name),
            ValueKind::RecordConstructor { .. } | ValueKind::RecordPredicate { .. } | ValueKind::RecordAccessor { .. } => "#<procedure>".to_string(),
            ValueKind::Void => "".to_string(),
        }
    }

    /// Format for `display` — strings without quotes, chars as raw characters
    pub fn to_display_output(&self) -> String {
        match &self.kind {
            ValueKind::Str(s) => s.clone(),
            ValueKind::Char(c) => c.to_string(),
            ValueKind::Float(f) => format_float(*f),
            ValueKind::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display()).collect();
                format!("({})", inner.join(" "))
            }
            ValueKind::SyntaxRules { .. } => "#<syntax>".to_string(),
            ValueKind::Record { .. } | ValueKind::RecordConstructor { .. } | ValueKind::RecordPredicate { .. } | ValueKind::RecordAccessor { .. } => self.to_display(),
            _ => self.to_display(),
        }
    }

    /// Format for `write` — strings with quotes (same as to_display for most types)
    pub fn to_write_output(&self) -> String {
        self.to_display()
    }

    pub fn as_integer(&self) -> Option<i64> {
        if let ValueKind::Integer(n) = self.kind { Some(n) } else { None }
    }

    pub fn as_num(&self) -> Option<NumVal> {
        match &self.kind {
            ValueKind::Integer(n) => Some(NumVal::Int(*n)),
            ValueKind::Rational(n, d) => Some(NumVal::Rat(*n, *d)),
            ValueKind::Float(f) => Some(NumVal::Flt(*f)),
            _ => None,
        }
    }

    pub fn fmt_pos(&self) -> String {
        format!("{}:{}", self.pos.0, self.pos.1)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (&self.kind, &other.kind) {
            (ValueKind::Integer(a), ValueKind::Integer(b)) => a == b,
            (ValueKind::Rational(an, ad), ValueKind::Rational(bn, bd)) => an == bn && ad == bd,
            (ValueKind::Float(a), ValueKind::Float(b)) => a == b,
            (ValueKind::Boolean(a), ValueKind::Boolean(b)) => a == b,
            (ValueKind::Str(a), ValueKind::Str(b)) => a == b,
            (ValueKind::Symbol(a), ValueKind::Symbol(b)) => a == b,
            (ValueKind::Char(a), ValueKind::Char(b)) => a == b,
            (ValueKind::List(a), ValueKind::List(b)) => a == b,
            (ValueKind::Void, ValueKind::Void) => true,
            _ => false,
        }
    }
}

fn format_float(f: f64) -> String {
    if f.fract() == 0.0 && f.is_finite() {
        format!("{:.1}", f)
    } else {
        let s = format!("{}", f);
        s
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display())
    }
}
