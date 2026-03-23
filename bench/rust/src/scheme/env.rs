use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use crate::scheme::value::Value;

/// A lexical environment for variable bindings, with parent chain for scoping.
#[derive(Debug, Clone)]
pub struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    parent: Option<Rc<Env>>,
}

impl Env {
    /// Create the default top-level environment with builtins.
    pub fn default_env() -> Rc<Self> {
        let mut bindings = HashMap::new();
        for name in [
            "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
            "cons", "car", "cdr", "null?", "list", "length", "append",
            "string?", "number?", "boolean?", "pair?", "symbol?",
        ] {
            bindings.insert(name.into(), Value::Builtin(name.into()));
        }
        Rc::new(Self {
            bindings: RefCell::new(bindings),
            parent: None,
        })
    }

    /// Create a child environment extending this one.
    pub fn extend(parent: &Rc<Env>, names: Vec<String>, values: Vec<Value>) -> Rc<Self> {
        let mut bindings = HashMap::new();
        for (name, val) in names.into_iter().zip(values) {
            bindings.insert(name, val);
        }
        Rc::new(Self {
            bindings: RefCell::new(bindings),
            parent: Some(Rc::clone(parent)),
        })
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.borrow().get(name) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn define(&self, name: String, value: Value) {
        self.bindings.borrow_mut().insert(name, value);
    }
}
