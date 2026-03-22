use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::error::{EvalError, EvalErrorKind};
use crate::scheme::value::Value;

#[derive(Debug, Clone)]
pub struct Env(Rc<RefCell<EnvInner>>);

#[derive(Debug)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl Env {
    pub fn new() -> Self {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: HashMap::new(),
            parent: None,
        })))
    }

    pub fn with_parent(parent: &Env) -> Self {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: HashMap::new(),
            parent: Some(parent.clone()),
        })))
    }

    pub fn define(&self, name: String, value: Value) {
        self.0.borrow_mut().bindings.insert(name, value);
    }

    pub fn set(&self, name: &str, value: Value) -> Result<(), EvalError> {
        let mut inner = self.0.borrow_mut();
        if inner.bindings.contains_key(name) {
            inner.bindings.insert(name.to_string(), value);
            Ok(())
        } else if let Some(parent) = &inner.parent {
            parent.set(name, value)
        } else {
            Err(EvalErrorKind::UnboundVariable {
                name: name.to_string(),
            }
            .into())
        }
    }

    pub fn lookup(&self, name: &str) -> Result<Value, EvalError> {
        let inner = self.0.borrow();
        if let Some(val) = inner.bindings.get(name) {
            Ok(val.clone())
        } else if let Some(parent) = &inner.parent {
            parent.lookup(name)
        } else {
            Err(EvalErrorKind::UnboundVariable {
                name: name.to_string(),
            }
            .into())
        }
    }
}
