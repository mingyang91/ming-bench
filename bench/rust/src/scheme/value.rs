use std::fmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

type Frame = Rc<RefCell<HashMap<String, Value>>>;

#[derive(Debug, Clone)]
pub struct LambdaData {
    pub params: Vec<String>,
    pub rest_param: Option<String>,
    pub body: Vec<crate::scheme::parser::Expr>,
    pub env: Env,
}

#[derive(Debug, Clone)]
pub struct MacroData {
    pub literals: Vec<String>,
    pub rules: Vec<(crate::scheme::parser::Expr, crate::scheme::parser::Expr)>,
    pub def_env: Env,
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Boolean(bool),
    Char(char),
    Str(String, bool),  // (content, mutable?)
    Symbol(String),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Lambda(Rc<LambdaData>),
    Macro(Rc<MacroData>),
    CaseLambda(Vec<Rc<LambdaData>>),   // multiple arity clauses
    Vector(Rc<RefCell<Vec<Value>>>),    // mutable fixed-size array
    Record(u64, Vec<Value>),           // type_id, field values
    RecordConstructor(u64, usize),     // type_id, num_fields
    RecordPredicate(u64),              // type_id
    RecordAccessor(u64, usize),        // type_id, field_index
    Void,
}

fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

impl Value {
    /// Create a simplified rational, collapsing to Integer if denominator is 1.
    pub fn make_rational(n: i64, d: i64) -> Value {
        if d == 0 {
            panic!("rational with zero denominator");
        }
        let sign = if d < 0 { -1 } else { 1 };
        let n = n * sign;
        let d = d * sign;
        let g = gcd(n, d);
        let n = n / g;
        let d = d / g;
        if d == 1 {
            Value::Integer(n)
        } else {
            Value::Rational(n, d)
        }
    }

    /// Convert to f64 for cross-type arithmetic
    pub fn to_f64(&self) -> Option<f64> {
        match self {
            Value::Integer(n) => Some(*n as f64),
            Value::Float(f) => Some(*f),
            Value::Rational(n, d) => Some(*n as f64 / *d as f64),
            _ => None,
        }
    }

    pub fn is_exact(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Rational(_, _))
    }

    pub fn is_inexact(&self) -> bool {
        matches!(self, Value::Float(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Float(_) | Value::Rational(_, _))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Str(a, _), Value::Str(b, _)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Vector(a), Value::Vector(b)) => {
                ptr::eq(a.as_ptr(), b.as_ptr()) || *a.borrow() == *b.borrow()
            }
            (Value::CaseLambda(_), Value::CaseLambda(_)) => false,
            (Value::Macro(_), Value::Macro(_)) => false,
            (Value::Record(t1, f1), Value::Record(t2, f2)) => t1 == t2 && f1 == f2,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

/// Environment: a chain of scopes with shared mutable frames.
#[derive(Debug, Clone)]
pub struct Env {
    frames: Vec<Frame>,
}

fn new_frame() -> Frame {
    Rc::new(RefCell::new(HashMap::new()))
}

impl Env {
    pub fn new() -> Self {
        Env {
            frames: vec![new_frame()],
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        for frame in self.frames.iter().rev() {
            if let Some(v) = frame.borrow().get(name).cloned() {
                return Some(v);
            }
        }
        None
    }

    pub fn define(&self, name: String, val: Value) {
        self.frames.last().unwrap().borrow_mut().insert(name, val);
    }

    /// Mutate an existing binding (searches from innermost frame outward).
    pub fn set(&self, name: &str, val: Value) -> bool {
        for frame in self.frames.iter().rev() {
            let mut f = frame.borrow_mut();
            if f.contains_key(name) {
                f.insert(name.to_string(), val);
                return true;
            }
        }
        false
    }

    pub fn push_frame(&mut self) {
        self.frames.push(new_frame());
    }

    /// Create a child env that shares all existing frames plus a new one.
    pub fn child(&self) -> Self {
        let mut frames = self.frames.clone(); // Rc clones = shared
        frames.push(new_frame());
        Env { frames }
    }
}

impl Value {
    pub fn to_display_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => {
                if f.fract() == 0.0 && f.is_finite() {
                    format!("{:.1}", f)
                } else {
                    format!("{}", f)
                }
            }
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Char(c) => format!("#\\{}", match *c {
                ' ' => "space".to_string(),
                '\n' => "newline".to_string(),
                '\t' => "tab".to_string(),
                c => c.to_string(),
            }),
            Value::Str(s, _) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display_string()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(a, b) => format!("({} . {})", a.to_display_string(), b.to_display_string()),
            Value::Lambda(_) | Value::CaseLambda(_) => "#<procedure>".into(),
            Value::Macro(_) => "#<macro>".into(),
            Value::Vector(v) => {
                let elems: Vec<String> = v.borrow().iter().map(|e| e.to_display_string()).collect();
                format!("#({})", elems.join(" "))
            }
            Value::Record(..) => "#<record>".into(),
            Value::RecordConstructor(..) | Value::RecordPredicate(_) | Value::RecordAccessor(..) => "#<procedure>".into(),
            Value::Void => "".into(),
        }
    }

    /// Format for `display` — no quotes on strings, raw chars
    pub fn to_write_string(&self) -> String {
        self.to_display_string()
    }

    /// Format for `display` — no quotes on strings, raw chars
    pub fn to_scheme_display(&self) -> String {
        match self {
            Value::Str(s, _) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::Float(f) => {
                if f.fract() == 0.0 && f.is_finite() {
                    format!("{:.1}", f)
                } else {
                    format!("{}", f)
                }
            }
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_scheme_display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(a, b) => format!("({} . {})", a.to_scheme_display(), b.to_scheme_display()),
            Value::Vector(v) => {
                let elems: Vec<String> = v.borrow().iter().map(|e| e.to_scheme_display()).collect();
                format!("#({})", elems.join(" "))
            }
            _ => self.to_display_string(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display_string())
    }
}
