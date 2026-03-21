use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::value::{Builtin, Value};

#[derive(Debug, Clone)]
pub struct Env(Rc<RefCell<Frame>>);

#[derive(Debug)]
struct Frame {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl Env {
    pub fn with_builtins() -> Self {
        let env = Self::new(None);
        Builtin::bind_all(&env);
        env
    }

    pub fn child(parent: &Self) -> Self {
        Self::new(Some(parent.clone()))
    }

    pub fn lookup(&self, name: &str) -> Option<Value> {
        let (binding, parent) = {
            let frame = self.0.borrow();
            (frame.bindings.get(name).cloned(), frame.parent.clone())
        };

        binding.or_else(|| parent.and_then(|env| env.lookup(name)))
    }

    pub fn define(&self, name: String, value: Value) {
        self.0.borrow_mut().bindings.insert(name, value);
    }

    fn new(parent: Option<Self>) -> Self {
        Self(Rc::new(RefCell::new(Frame {
            bindings: HashMap::new(),
            parent,
        })))
    }
}
