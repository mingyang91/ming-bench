use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::value::Value;

#[derive(Debug, Clone)]
pub(crate) struct Environment(Rc<RefCell<Frame>>);

#[derive(Debug, Default)]
struct Frame {
    parent: Option<Environment>,
    bindings: HashMap<String, Value>,
}

impl Environment {
    pub(crate) fn new() -> Self {
        Self(Rc::new(RefCell::new(Frame::default())))
    }

    pub(crate) fn child(&self) -> Self {
        Self(Rc::new(RefCell::new(Frame {
            parent: Some(self.clone()),
            bindings: HashMap::new(),
        })))
    }

    pub(crate) fn define(&self, name: &str, value: Value) {
        let _ = self.0.borrow_mut().bindings.insert(name.to_owned(), value);
    }

    pub(crate) fn get(&self, name: &str) -> Option<Value> {
        self.binding_environment(name)
            .and_then(|environment| environment.0.borrow().bindings.get(name).cloned())
    }

    pub(crate) fn set(&self, name: &str, value: Value) -> bool {
        match self.binding_environment(name) {
            Some(environment) => {
                environment.define(name, value);
                true
            }
            None => false,
        }
    }

    fn binding_environment(&self, name: &str) -> Option<Self> {
        let frame = self.0.borrow();

        if frame.bindings.contains_key(name) {
            return Some(self.clone());
        }

        let parent = frame.parent.clone();
        drop(frame);

        parent.and_then(|environment| environment.binding_environment(name))
    }
}
