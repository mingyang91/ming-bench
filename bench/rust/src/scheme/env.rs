use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::value::Value;

#[derive(Debug, Clone)]
pub struct Env {
    inner: Rc<RefCell<EnvInner>>,
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
        }
    }

    pub fn with_parent(parent: &Env) -> Self {
        Env {
            inner: Rc::new(RefCell::new(EnvInner {
                bindings: HashMap::new(),
                parent: Some(parent.clone()),
            })),
        }
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
}
