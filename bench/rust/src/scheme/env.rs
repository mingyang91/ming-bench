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
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            parent: None,
        }))
    }

    pub fn with_parent(parent: &Rc<RefCell<Env>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
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
            Err(EvalError::UnboundVariable(name.into()))
        }
    }

    pub fn set(&mut self, name: String, val: Value) {
        self.bindings.insert(name, val);
    }

    pub fn set_existing(env: &Rc<RefCell<Env>>, name: &str, val: Value) -> Result<(), EvalError> {
        let mut cur = Rc::clone(env);
        loop {
            {
                let mut e = cur.borrow_mut();
                if e.bindings.contains_key(name) {
                    e.bindings.insert(name.to_string(), val);
                    return Ok(());
                }
            }
            let next = {
                let e = cur.borrow();
                match e.parent {
                    Some(ref p) => Rc::clone(p),
                    None => return Err(EvalError::UnboundVariable(name.into())),
                }
            };
            cur = next;
        }
    }
}
