use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::value::Value;

type Binding = Rc<RefCell<Value>>;

#[derive(Clone)]
pub struct Environment(Rc<RefCell<Frame>>);

struct Frame {
    bindings: HashMap<String, Binding>,
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
        self.0
            .borrow_mut()
            .bindings
            .insert(name.into(), Rc::new(RefCell::new(value)));
    }

    pub fn lookup(&self, name: &str) -> Option<Value> {
        self.lookup_binding(name)
            .map(|binding| binding.borrow().clone())
    }

    pub fn set(&self, name: &str, value: Value) -> bool {
        let Some(binding) = self.lookup_binding(name) else {
            return false;
        };

        *binding.borrow_mut() = value;
        true
    }

    fn lookup_binding(&self, name: &str) -> Option<Binding> {
        let frame = self.0.borrow();
        let binding = frame.bindings.get(name).cloned();
        let parent = frame.parent.clone();
        drop(frame);

        binding.or_else(|| parent.and_then(|parent| parent.lookup_binding(name)))
    }
}
