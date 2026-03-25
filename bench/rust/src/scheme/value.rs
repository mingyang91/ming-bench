use std::fmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
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

pub type ConsCell = Rc<RefCell<(Value, Value)>>;

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Boolean(bool),
    Char(char),
    Str(String, bool),  // (content, mutable?)
    Symbol(String),
    Nil,                // empty list ()
    Pair(ConsCell),     // mutable shared cons cell
    Lambda(Rc<LambdaData>),
    Macro(Rc<MacroData>),
    CaseLambda(Vec<Rc<LambdaData>>),   // multiple arity clauses
    Vector(Rc<RefCell<Vec<Value>>>),    // mutable fixed-size array
    Record(u64, Vec<Value>),           // type_id, field values
    RecordConstructor(u64, usize),     // type_id, num_fields
    RecordPredicate(u64),              // type_id
    RecordAccessor(u64, usize),        // type_id, field_index
    Continuation(u64),                 // continuation id for call/cc
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

    pub fn cons(car: Value, cdr: Value) -> Value {
        Value::Pair(Rc::new(RefCell::new((car, cdr))))
    }

    pub fn from_vec(v: Vec<Value>) -> Value {
        let mut result = Value::Nil;
        for val in v.into_iter().rev() {
            result = Value::cons(val, result);
        }
        result
    }

    /// Convert a proper list (pair chain ending in Nil) to a Vec.
    /// Returns None for improper lists or non-lists.
    pub fn to_vec(&self) -> Option<Vec<Value>> {
        let mut result = Vec::new();
        let mut current = self.clone();
        let mut seen = HashSet::new();
        loop {
            match current {
                Value::Nil => return Some(result),
                Value::Pair(cell) => {
                    let ptr = Rc::as_ptr(&cell) as usize;
                    if !seen.insert(ptr) {
                        return None; // cycle detected
                    }
                    let (car, cdr) = {
                        let borrowed = cell.borrow();
                        (borrowed.0.clone(), borrowed.1.clone())
                    };
                    result.push(car);
                    current = cdr;
                }
                _ => return None,
            }
        }
    }

    /// Check if this is a proper list (pair chain ending in Nil), with cycle detection.
    pub fn is_proper_list(&self) -> bool {
        fn advance(v: &Value) -> Option<Value> {
            match v {
                Value::Pair(cell) => Some(cell.borrow().1.clone()),
                _ => None,
            }
        }
        // Tortoise and hare algorithm
        let mut slow = self.clone();
        let mut fast = self.clone();
        loop {
            // Advance fast by one
            match &fast {
                Value::Nil => return true,
                Value::Pair(_) => {}
                _ => return false,
            }
            fast = advance(&fast).unwrap();
            // Advance fast by another one
            match &fast {
                Value::Nil => return true,
                Value::Pair(_) => {}
                _ => return false,
            }
            fast = advance(&fast).unwrap();
            // Advance slow by one
            match &slow {
                Value::Pair(_) => {}
                _ => return false,
            }
            slow = advance(&slow).unwrap();
            // Check if slow and fast point to the same cell
            if let (Value::Pair(s), Value::Pair(f)) = (&slow, &fast) {
                if Rc::ptr_eq(s, f) {
                    return false; // cycle
                }
            }
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
        equal_with_cycle_check(self, other, &mut HashSet::new())
    }
}

fn equal_with_cycle_check(a: &Value, b: &Value, seen: &mut HashSet<(usize, usize)>) -> bool {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Str(a, _), Value::Str(b, _)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        (Value::Pair(a), Value::Pair(b)) => {
            if Rc::ptr_eq(a, b) {
                return true;
            }
            let key = (Rc::as_ptr(a) as usize, Rc::as_ptr(b) as usize);
            if !seen.insert(key) {
                return true; // already comparing these — assume equal to break cycle
            }
            let ab = a.borrow();
            let bb = b.borrow();
            equal_with_cycle_check(&ab.0, &bb.0, seen)
                && equal_with_cycle_check(&ab.1, &bb.1, seen)
        }
        (Value::Vector(a), Value::Vector(b)) => {
            std::ptr::eq(a.as_ptr(), b.as_ptr()) || *a.borrow() == *b.borrow()
        }
        (Value::Continuation(a), Value::Continuation(b)) => a == b,
        (Value::CaseLambda(_), Value::CaseLambda(_)) => false,
        (Value::Macro(_), Value::Macro(_)) => false,
        (Value::Record(t1, f1), Value::Record(t2, f2)) => t1 == t2 && f1 == f2,
        (Value::Void, Value::Void) => true,
        _ => false,
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

/// Format a value as a pair/list, with cycle detection.
fn fmt_pair(cell: &ConsCell, f: &mut dyn FnMut(&Value) -> String) -> String {
    let mut parts = Vec::new();
    let mut current = Value::Pair(cell.clone());
    let mut seen = HashSet::new();

    loop {
        match current {
            Value::Pair(c) => {
                let ptr = Rc::as_ptr(&c) as usize;
                if !seen.insert(ptr) {
                    parts.push("...".to_string());
                    break;
                }
                let (car, cdr) = {
                    let borrowed = c.borrow();
                    (borrowed.0.clone(), borrowed.1.clone())
                };
                parts.push(f(&car));
                current = cdr;
            }
            Value::Nil => break,
            other => {
                let last = parts.pop().unwrap();
                parts.push(format!("{} . {}", last, f(&other)));
                break;
            }
        }
    }
    format!("({})", parts.join(" "))
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
            Value::Nil => "()".into(),
            Value::Pair(cell) => fmt_pair(cell, &mut |v| v.to_display_string()),
            Value::Lambda(_) | Value::CaseLambda(_) | Value::Continuation(_) => "#<procedure>".into(),
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

    /// Format for `write` — with quotes on strings, etc.
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
            Value::Nil => "()".into(),
            Value::Pair(cell) => fmt_pair(cell, &mut |v| v.to_scheme_display()),
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
