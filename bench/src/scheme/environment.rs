use std::collections::HashMap;

use crate::scheme::value::Value;

#[derive(Debug, Default)]
pub(crate) struct Environment {
    bindings: HashMap<String, Value>,
}

impl Environment {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn define(&mut self, name: &str, value: Value) {
        let _ = self.bindings.insert(name.to_owned(), value);
    }

    pub(crate) fn get(&self, name: &str) -> Option<Value> {
        self.bindings.get(name).cloned()
    }
}
