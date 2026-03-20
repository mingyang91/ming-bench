use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::error::SchemeError;
use crate::scheme::value::Value;

pub type Env = Rc<RefCell<EnvInner>>;

#[derive(Debug, Clone)]
pub struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

pub fn new_env(parent: Option<Env>) -> Env {
    Rc::new(RefCell::new(EnvInner {
        bindings: HashMap::new(),
        parent,
    }))
}

pub fn define(env: &Env, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

pub fn lookup(env: &Env, name: &str) -> Result<Value, SchemeError> {
    let val = env.borrow().bindings.get(name).cloned();
    if let Some(v) = val {
        return Ok(v);
    }
    let parent = env.borrow().parent.clone();
    match parent {
        Some(p) => lookup(&p, name),
        None => Err(SchemeError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

pub fn set(env: &Env, name: &str, val: Value) -> Result<(), SchemeError> {
    let has_key = env.borrow().bindings.contains_key(name);
    if has_key {
        env.borrow_mut().bindings.insert(name.to_string(), val);
        return Ok(());
    }
    let parent = env.borrow().parent.clone();
    match parent {
        Some(p) => set(&p, name, val),
        None => Err(SchemeError::SetUnbound {
            name: name.to_string(),
        }),
    }
}
