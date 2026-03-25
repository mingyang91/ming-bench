use std::fmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type Frame = Rc<RefCell<HashMap<String, Value>>>;

#[derive(Debug, Clone)]
pub struct LambdaData {
    pub params: Vec<String>,
    pub body: Vec<crate::scheme::parser::Expr>,
    pub env: Env,
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Char(char),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda(Rc<LambdaData>),
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
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
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Char(c) => format!("#\\{}", match *c {
                ' ' => "space".to_string(),
                '\n' => "newline".to_string(),
                '\t' => "tab".to_string(),
                c => c.to_string(),
            }),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display_string()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda(_) => "#<procedure>".into(),
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
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_scheme_display()).collect();
                format!("({})", inner.join(" "))
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
