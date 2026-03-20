use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use super::value::Value;

/// Lexically-scoped environment with parent chain.
/// Uses interior mutability so closures can see bindings added after capture.
pub struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    parent: Option<Rc<Env>>,
    output: Rc<RefCell<String>>,
}

impl fmt::Debug for Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Env{{...}}")
    }
}

impl Env {
    pub fn new() -> Rc<Self> {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            parent: None,
            output: Rc::new(RefCell::new(String::new())),
        })
    }

    pub fn child(parent: &Rc<Env>) -> Rc<Self> {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            output: Rc::clone(&parent.output),
            parent: Some(Rc::clone(parent)),
        })
    }

    /// Write to the shared output buffer.
    pub fn write_output(&self, s: &str) {
        self.output.borrow_mut().push_str(s);
    }

    /// Take the accumulated output, leaving the buffer empty.
    pub fn take_output(&self) -> String {
        self.output.borrow_mut().split_off(0)
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.bindings
            .borrow()
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref()?.get(name))
    }

    pub fn set(&self, name: String, val: Value) {
        self.bindings.borrow_mut().insert(name, val);
    }

    /// Update an existing binding in the frame where it is defined.
    /// Returns `true` if the binding was found and updated.
    pub fn set_existing(&self, name: &str, val: Value) -> bool {
        let mut bindings = self.bindings.borrow_mut();
        if bindings.contains_key(name) {
            bindings.insert(name.to_string(), val);
            true
        } else {
            drop(bindings);
            self.parent
                .as_ref()
                .is_some_and(|p| p.set_existing(name, val))
        }
    }
}
