use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use crate::scheme::value::Value;

/// A lexical environment for variable bindings, with parent chain for scoping.
#[derive(Debug, Clone)]
pub struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    parent: Option<Rc<Env>>,
    output: Rc<RefCell<String>>,
}

impl Env {
    /// Create the default top-level environment with builtins.
    pub fn default_env() -> Rc<Self> {
        Self::default_env_with_output(Rc::new(RefCell::new(String::new())))
    }

    /// Create the default top-level environment with a shared output buffer.
    pub fn default_env_with_output(output: Rc<RefCell<String>>) -> Rc<Self> {
        let mut bindings = HashMap::new();
        for name in [
            "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
            "cons", "car", "cdr", "null?", "list", "length", "append",
            "string?", "number?", "boolean?", "pair?", "symbol?",
            "display", "write", "newline",
            "string-append", "string-length", "substring",
            "string->number", "number->string",
            "symbol->string", "string->symbol",
            "string-ref", "string-copy", "string-set!", "char?",
        ] {
            bindings.insert(name.into(), Value::Builtin(name.into()));
        }
        Rc::new(Self {
            bindings: RefCell::new(bindings),
            parent: None,
            output,
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
            output: Rc::clone(&parent.output),
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

    /// Mutate an existing binding (for set!). Returns false if unbound.
    pub fn set(&self, name: &str, value: Value) -> bool {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), value);
            true
        } else if let Some(parent) = &self.parent {
            parent.set(name, value)
        } else {
            false
        }
    }

    /// Write to the output buffer.
    pub fn write_output(&self, s: &str) {
        self.output.borrow_mut().push_str(s);
    }

    /// Get the accumulated output.
    pub fn take_output(&self) -> String {
        self.output.borrow().clone()
    }
}
