use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::value::Value;

#[derive(Debug, Clone)]
pub struct Env {
    inner: Rc<RefCell<EnvInner>>,
    output: Rc<RefCell<String>>,
}

impl PartialEq for Env {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

#[derive(Debug, Clone)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            inner: Rc::new(RefCell::new(EnvInner {
                bindings: HashMap::new(),
                parent: None,
            })),
            output: Rc::new(RefCell::new(String::new())),
        }
    }

    pub fn with_parent(parent: &Env) -> Self {
        Env {
            inner: Rc::new(RefCell::new(EnvInner {
                bindings: HashMap::new(),
                parent: Some(parent.clone()),
            })),
            output: parent.output.clone(),
        }
    }

    pub fn write_output(&self, s: &str) {
        self.output.borrow_mut().push_str(s);
    }

    pub fn take_output(&self) -> String {
        self.output.borrow_mut().split_off(0)
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        let inner = self.inner.borrow();
        if let Some(val) = inner.bindings.get(name) {
            Some(val.clone())
        } else {
            inner.parent.as_ref().and_then(|p| p.get(name))
        }
    }

    pub fn define(&self, name: String, value: Value) {
        self.inner.borrow_mut().bindings.insert(name, value);
    }

    /// Mutate an existing binding. Walks the environment chain.
    /// Returns `true` if the binding was found and updated, `false` if unbound.
    pub fn set(&self, name: &str, value: Value) -> bool {
        let mut inner = self.inner.borrow_mut();
        if inner.bindings.contains_key(name) {
            inner.bindings.insert(name.to_string(), value);
            return true;
        }
        match &inner.parent {
            Some(parent) => parent.set(name, value),
            None => false,
        }
    }
}
