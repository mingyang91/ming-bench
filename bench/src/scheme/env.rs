use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use super::value::Value;

/// Lexically-scoped environment with parent chain.
/// Uses interior mutability so closures can see bindings added after capture.
pub struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    parent: Option<Rc<Env>>,
}

impl fmt::Debug for Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Env{{...}}")
    }
}

impl Env {
    pub fn new() -> Rc<Self> {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            parent: None,
        })
    }

    pub fn child(parent: &Rc<Env>) -> Rc<Self> {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            parent: Some(Rc::clone(parent)),
        })
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.bindings
            .borrow()
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref()?.get(name))
    }

    pub fn set(&self, name: String, val: Value) {
        self.bindings.borrow_mut().insert(name, val);
    }
}
