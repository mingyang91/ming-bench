use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Env(Rc<RefCell<EnvInner>>);

#[derive(Debug)]
pub(super) struct EnvInner {
    bindings: HashMap<String, Expr>,
    parent: Option<Env>,
}

impl Env {
    pub fn new() -> Self {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: HashMap::new(),
            parent: None,
        })))
    }

    pub fn child(&self) -> Self {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: HashMap::new(),
            parent: Some(self.clone()),
        })))
    }

    pub fn get(&self, name: &str) -> Option<Expr> {
        let inner = self.0.borrow();
        if let Some(val) = inner.bindings.get(name) {
            Some(val.clone())
        } else if let Some(parent) = &inner.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn insert(&self, name: String, val: Expr) {
        self.0.borrow_mut().bindings.insert(name, val);
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
    Lambda {
        params: Vec<String>,
        body: Box<Expr>,
        env: Env,
    },
    Void,
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::Integer(a), Expr::Integer(b)) => a == b,
            (Expr::Boolean(a), Expr::Boolean(b)) => a == b,
            (Expr::Str(a), Expr::Str(b)) => a == b,
            (Expr::Symbol(a), Expr::Symbol(b)) => a == b,
            (Expr::List(a), Expr::List(b)) => a == b,
            (Expr::Void, Expr::Void) => true,
            _ => false,
        }
    }
}

impl Expr {
    pub fn to_display(&self) -> String {
        match self {
            Expr::Integer(n) => n.to_string(),
            Expr::Boolean(true) => "#t".into(),
            Expr::Boolean(false) => "#f".into(),
            Expr::Str(s) => format!("\"{s}\""),
            Expr::Symbol(s) => s.clone(),
            Expr::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|e| e.to_display()).collect();
                format!("({})", inner.join(" "))
            }
            Expr::Lambda { .. } => "#<procedure>".into(),
            Expr::Void => "".into(),
        }
    }
}
