use std::collections::HashMap;
use crate::scheme::value::Value;

/// A lexical environment for variable bindings.
#[derive(Debug, Clone)]
pub struct Env {
    bindings: HashMap<String, Value>,
}

impl Env {
    /// Create the default top-level environment with builtins.
    pub fn default_env() -> Self {
        let mut bindings = HashMap::new();
        for name in ["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not", "and", "or"] {
            bindings.insert(name.into(), Value::Builtin(name.into()));
        }
        Self { bindings }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.bindings.get(name)
    }
}
