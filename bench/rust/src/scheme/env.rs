use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::value::{Value, ValueKind};

#[derive(Debug, Clone)]
pub struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    parent: Option<Rc<Env>>,
}

impl Env {
    pub fn new(parent: Option<Rc<Env>>) -> Rc<Env> {
        Rc::new(Env {
            bindings: RefCell::new(HashMap::new()),
            parent,
        })
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.borrow().get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn set(&self, name: String, val: Value) {
        self.bindings.borrow_mut().insert(name, val);
    }

    pub fn default_env() -> Rc<Env> {
        let env = Env::new(None);
        for name in &["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                      "cons", "car", "cdr", "null?", "list", "length", "append",
                      "string?", "number?", "boolean?", "pair?", "symbol?"] {
            env.set(name.to_string(), Value::unpos(ValueKind::Symbol(name.to_string())));
        }
        env
    }
}
