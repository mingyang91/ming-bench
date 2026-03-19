use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

// ── Environment ──────────────────────────────────────────────────────

struct EnvFrame {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

/// Lexically-scoped environment built from linked frames.
#[derive(Clone)]
pub struct Env(Rc<RefCell<EnvFrame>>);

impl Env {
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(EnvFrame {
            bindings: HashMap::new(),
            parent: None,
        })))
    }

    /// Create a child frame whose parent is `self`.
    pub fn child(&self) -> Self {
        Self(Rc::new(RefCell::new(EnvFrame {
            bindings: HashMap::new(),
            parent: Some(self.clone()),
        })))
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        let frame = self.0.borrow();
        if let Some(val) = frame.bindings.get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = frame.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn set(&self, name: String, val: Value) {
        self.0.borrow_mut().bindings.insert(name, val);
    }

    /// Mutate an existing binding, walking up the chain. Returns false if unbound.
    pub fn update(&self, name: &str, val: Value) -> bool {
        let mut frame = self.0.borrow_mut();
        if frame.bindings.contains_key(name) {
            frame.bindings.insert(name.to_string(), val);
            true
        } else if let Some(ref parent) = frame.parent {
            parent.update(name, val)
        } else {
            false
        }
    }
}

// ── Value ────────────────────────────────────────────────────────────

#[derive(Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Box<Value>,
        env: Env,
    },
    Builtin {
        name: String,
    },
    Continuation {
        id: u64,
        replay_expr: Box<Value>,
        remaining_exprs: Vec<Value>,
        env: Env,
    },
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Symbol(a), Self::Symbol(b)) => a == b,
            (Self::List(a), Self::List(b)) => a == b,
            (Self::Builtin { name: a }, Self::Builtin { name: b }) => a == b,
            (Self::Continuation { id: a, .. }, Self::Continuation { id: b, .. }) => a == b,
            (Self::Void, Self::Void) => true,
            _ => false,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(n) => write!(f, "Integer({n})"),
            Self::Boolean(b) => write!(f, "Boolean({b})"),
            Self::String(s) => write!(f, "String({s:?})"),
            Self::Symbol(s) => write!(f, "Symbol({s:?})"),
            Self::List(elems) => write!(f, "List({elems:?})"),
            Self::Lambda { params, .. } => write!(f, "Lambda({params:?})"),
            Self::Builtin { name } => write!(f, "Builtin({name:?})"),
            Self::Continuation { id, .. } => write!(f, "Continuation({id})"),
            Self::Void => write!(f, "Void"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(elems) => write_list(f, elems),
            Value::Lambda { .. } | Value::Builtin { .. } | Value::Continuation { .. } => {
                write!(f, "#<procedure>")
            }
            Value::Void => write!(f, ""),
        }
    }
}

fn write_list(f: &mut fmt::Formatter<'_>, elems: &[Value]) -> fmt::Result {
    write!(f, "(")?;
    for (i, elem) in elems.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write!(f, "{elem}")?;
    }
    write!(f, ")")
}
