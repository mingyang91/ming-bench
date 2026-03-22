use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

#[derive(Debug)]
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
            Err(EvalError::unbound(name))
        }
    }

    pub fn set(&mut self, name: String, val: Value) {
        self.bindings.insert(name, val);
    }

    /// Mutate an existing binding, walking up the parent chain.
    /// Returns an error if the variable is not bound in any enclosing scope.
    pub fn update(&mut self, name: &str, val: Value) -> Result<(), EvalError> {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_string(), val);
            Ok(())
        } else if let Some(ref parent) = self.parent {
            parent.borrow_mut().update(name, val)
        } else {
            Err(EvalError::unbound(name))
        }
    }
}
