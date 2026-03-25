use std::fmt;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        body: Vec<crate::scheme::parser::Expr>,
        env: Env,
    },
    Void,
}

/// Environment: a chain of scopes.
#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    frames: Vec<HashMap<String, Value>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            frames: vec![HashMap::new()],
        }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        for frame in self.frames.iter().rev() {
            if let Some(v) = frame.get(name) {
                return Some(v);
            }
        }
        None
    }

    pub fn define(&mut self, name: String, val: Value) {
        self.frames.last_mut().unwrap().insert(name, val);
    }

    pub fn push_frame(&mut self) {
        self.frames.push(HashMap::new());
    }

    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    /// Create a child env that extends this one with a new empty frame.
    pub fn child(&self) -> Self {
        let mut new = self.clone();
        new.push_frame();
        new
    }

    /// Merge the bottom (top-level) frame from another env into ours.
    pub fn merge_top_level(&mut self, other: &Env) {
        if let Some(other_frame) = other.frames.first() {
            if let Some(self_frame) = self.frames.first_mut() {
                for (k, v) in other_frame {
                    self_frame.insert(k.clone(), v.clone());
                }
            }
        }
    }

    /// Merge all bindings from another env into ours (frame by frame).
    /// For frames that exist in both, merge bindings. Extra frames from
    /// `other` are appended.
    pub fn merge_all(&mut self, other: &Env) {
        for (i, other_frame) in other.frames.iter().enumerate() {
            if i < self.frames.len() {
                for (k, v) in other_frame {
                    self.frames[i].insert(k.clone(), v.clone());
                }
            } else {
                self.frames.push(other_frame.clone());
            }
        }
    }
}

impl Value {
    pub fn to_display_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display_string()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda { .. } => "#<procedure>".into(),
            Value::Void => "".into(),
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
