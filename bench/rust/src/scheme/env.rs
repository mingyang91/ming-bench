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

    pub fn set_existing(&self, name: &str, val: Value) -> bool {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), val);
            true
        } else if let Some(ref parent) = self.parent {
            parent.set_existing(name, val)
        } else {
            false
        }
    }

    pub fn default_env() -> Rc<Env> {
        let env = Env::new(None);
        for name in &["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                      "cons", "car", "cdr", "null?", "list", "length", "append",
                      "string?", "number?", "boolean?", "pair?", "symbol?", "char?",
                      "display", "write", "newline",
                      "string-append", "string-length", "substring",
                      "string->number", "number->string",
                      "symbol->string", "string->symbol", "string-ref",
                      "string-copy", "apply",
                      // L09
                      "abs", "modulo", "remainder", "quotient",
                      "min", "max", "expt",
                      "zero?", "positive?", "negative?", "odd?", "even?",
                      "list-ref", "list-tail", "list?", "assoc", "map", "equal?", "eqv?", "eq?",
                      "char-alphabetic?", "char-numeric?",
                      "char-upcase", "char-downcase", "char=?", "char<?",
                      "string=?", "string<?", "string-ci=?",
                      "string-upcase", "string-downcase",
                      "integer?", "char->integer", "integer->char",
                      "make-string", "for-each", "reverse",
                      // L11
                      "procedure?",
                      "exact?", "inexact?", "rational?",
                      "exact->inexact", "inexact->exact",
                      "numerator", "denominator"] {
            env.set(name.to_string(), Value::unpos(ValueKind::Symbol(name.to_string())));
        }
        env
    }
}
