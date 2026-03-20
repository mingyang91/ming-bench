use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::syntax::SyntaxRules;
use crate::scheme::value::Value;

#[derive(Debug, Clone)]
pub(crate) struct Environment(Rc<RefCell<Frame>>);

#[derive(Debug, Default)]
struct Frame {
    parent: Option<Environment>,
    bindings: HashMap<String, Value>,
    syntax_bindings: HashMap<String, Rc<SyntaxRules>>,
}

impl Environment {
    pub(crate) fn new() -> Self {
        Self(Rc::new(RefCell::new(Frame::default())))
    }

    pub(crate) fn child(&self) -> Self {
        Self(Rc::new(RefCell::new(Frame {
            parent: Some(self.clone()),
            bindings: HashMap::new(),
            syntax_bindings: HashMap::new(),
        })))
    }

    pub(crate) fn define(&self, name: &str, value: Value) {
        let _ = self.0.borrow_mut().bindings.insert(name.to_owned(), value);
    }

    pub(crate) fn define_syntax(&self, name: &str, syntax: Rc<SyntaxRules>) {
        let _ = self
            .0
            .borrow_mut()
            .syntax_bindings
            .insert(name.to_owned(), syntax);
    }

    pub(crate) fn get(&self, name: &str) -> Option<Value> {
        self.binding_environment(name)
            .and_then(|environment| environment.0.borrow().bindings.get(name).cloned())
    }

    pub(crate) fn get_syntax(&self, name: &str) -> Option<Rc<SyntaxRules>> {
        self.syntax_binding_environment(name)
            .and_then(|environment| environment.0.borrow().syntax_bindings.get(name).cloned())
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

    pub(crate) fn ptr_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
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

    fn syntax_binding_environment(&self, name: &str) -> Option<Self> {
        let frame = self.0.borrow();

        if frame.syntax_bindings.contains_key(name) {
            return Some(self.clone());
        }

        let parent = frame.parent.clone();
        drop(frame);

        parent.and_then(|environment| environment.syntax_binding_environment(name))
    }
}
