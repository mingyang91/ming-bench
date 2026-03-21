use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::value::Value;

#[derive(Clone)]
pub struct Environment(Rc<RefCell<Frame>>);

struct Frame {
    bindings: HashMap<String, Value>,
    parent: Option<Environment>,
}

impl Environment {
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(Frame {
            bindings: HashMap::new(),
            parent: None,
        })))
    }

    pub fn child(&self) -> Self {
        Self(Rc::new(RefCell::new(Frame {
            bindings: HashMap::new(),
            parent: Some(self.clone()),
        })))
    }

    pub fn define(&self, name: impl Into<String>, value: Value) {
        self.0.borrow_mut().bindings.insert(name.into(), value);
    }

    pub fn lookup(&self, name: &str) -> Option<Value> {
        let frame = self.0.borrow();
        if let Some(value) = frame.bindings.get(name) {
            return Some(value.clone());
        }

        frame.parent.as_ref().and_then(|parent| parent.lookup(name))
    }
}
