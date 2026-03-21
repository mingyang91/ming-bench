use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::value::Value;

#[derive(Debug, Clone)]
pub struct Env {
    bindings: Rc<RefCell<HashMap<String, Value>>>,
    parent: Option<Box<Env>>,
    output: Rc<RefCell<String>>,
    callcc_counter: Rc<Cell<u64>>,
    pending_cont: Rc<RefCell<Option<(u64, Value)>>>,
    current_expr_index: Rc<Cell<usize>>,
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl Env {
    pub fn new() -> Self {
        Env {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            parent: None,
            output: Rc::new(RefCell::new(String::new())),
            callcc_counter: Rc::new(Cell::new(0)),
            pending_cont: Rc::new(RefCell::new(None)),
            current_expr_index: Rc::new(Cell::new(0)),
        }
    }

    pub fn extend(parent: &Env) -> Self {
        Env {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            parent: Some(Box::new(parent.clone())),
            output: Rc::clone(&parent.output),
            callcc_counter: Rc::clone(&parent.callcc_counter),
            pending_cont: Rc::clone(&parent.pending_cont),
            current_expr_index: Rc::clone(&parent.current_expr_index),
        }
    }

    pub fn write_output(&self, s: &str) {
        self.output.borrow_mut().push_str(s);
    }

    pub fn take_output(&self) -> String {
        self.output.borrow_mut().split_off(0)
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.borrow().get(name) {
            return Some(val.clone());
        }
        self.parent.as_ref().and_then(|p| p.get(name))
    }

    pub fn define(&self, name: String, val: Value) {
        self.bindings.borrow_mut().insert(name, val);
    }

    /// Mutate an existing binding in the nearest frame that contains it.
    /// Returns false if the variable is not found in any frame.
    pub fn set(&self, name: &str, val: Value) -> bool {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), val);
            return true;
        }
        self.parent.as_ref().is_some_and(|p| p.set(name, val))
    }

    pub fn next_callcc_id(&self) -> u64 {
        let id = self.callcc_counter.get();
        self.callcc_counter.set(id + 1);
        id
    }

    pub fn reset_callcc_counter(&self) {
        self.callcc_counter.set(0);
    }

    pub fn set_pending_cont(&self, id: u64, value: Value) {
        *self.pending_cont.borrow_mut() = Some((id, value));
    }

    pub fn take_pending_cont(&self, id: u64) -> Option<Value> {
        let mut pending = self.pending_cont.borrow_mut();
        match &*pending {
            Some((pid, _)) if *pid == id => pending.take().map(|(_, v)| v),
            _ => None,
        }
    }

    pub fn current_expr_index(&self) -> usize {
        self.current_expr_index.get()
    }

    pub fn set_current_expr_index(&self, idx: usize) {
        self.current_expr_index.set(idx);
    }
}
