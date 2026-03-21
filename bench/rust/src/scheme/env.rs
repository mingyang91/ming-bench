use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::value::Value;

/// A lexically-scoped environment backed by a chain of hash maps.
#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    bindings: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    /// Create a new top-level environment with no parent.
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            parent: None,
        }))
    }

    /// Create a child environment extending `parent`.
    pub fn extend(parent: &Rc<RefCell<Env>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            parent: Some(Rc::clone(parent)),
        }))
    }

    /// Look up a variable, walking the parent chain.
    pub fn get(&self, name: &str) -> Option<Value> {
        self.bindings.get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|p| p.borrow().get(name))
        })
    }

    /// Mutate an existing binding, walking the parent chain.
    /// Returns `false` if the variable is not bound in any frame.
    pub fn set(&mut self, name: &str, val: Value) -> bool {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_owned(), val);
            true
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().set(name, val)
        } else {
            false
        }
    }

    /// Define (or redefine) a variable in this frame.
    pub fn define(&mut self, name: String, val: Value) {
        self.bindings.insert(name, val);
    }
}
