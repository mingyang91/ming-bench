use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

#[derive(Debug, Clone)]
pub struct Env {
    bindings: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Env {
            bindings: HashMap::new(),
            parent: None,
        }))
    }

    pub fn with_parent(parent: &Rc<RefCell<Env>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Env {
            bindings: HashMap::new(),
            parent: Some(Rc::clone(parent)),
        }))
    }

    pub fn get(&self, name: &str) -> Result<Value, EvalError> {
        if let Some(val) = self.bindings.get(name) {
            Ok(val.clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get(name)
        } else {
            Err(EvalError::UnboundVariable { name: name.to_string() })
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn set(&mut self, name: &str, value: Value) -> Result<(), EvalError> {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_string(), value);
            Ok(())
        } else if let Some(ref parent) = self.parent {
            parent.borrow_mut().set(name, value)
        } else {
            Err(EvalError::UnboundVariable { name: name.to_string() })
        }
    }
}
