use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::value::Value;

#[derive(Debug, Clone)]
pub struct Env {
    inner: Rc<RefCell<EnvInner>>,
}

#[derive(Debug)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(EnvInner {
                bindings: HashMap::new(),
                parent: None,
            })),
        }
    }

    pub fn child(&self) -> Self {
        Self {
            inner: Rc::new(RefCell::new(EnvInner {
                bindings: HashMap::new(),
                parent: Some(self.clone()),
            })),
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        let inner = self.inner.borrow();
        if let Some(val) = inner.bindings.get(name) {
            Some(val.clone())
        } else if let Some(parent) = &inner.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn define(&self, name: String, value: Value) {
        self.inner.borrow_mut().bindings.insert(name, value);
    }

    /// Mutate an existing binding in the nearest enclosing scope.
    /// Returns `false` if the variable is not bound in any scope.
    pub fn set(&self, name: &str, value: Value) -> bool {
        let mut inner = self.inner.borrow_mut();
        if inner.bindings.contains_key(name) {
            inner.bindings.insert(name.to_owned(), value);
            true
        } else if let Some(parent) = &inner.parent {
            parent.set(name, value)
        } else {
            false
        }
    }
}
