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
            "display", "write", "newline", "apply",
            "string-append", "string-length", "substring",
            "string->number", "number->string",
            "symbol->string", "string->symbol",
            "string-ref", "string-copy", "string-set!", "char?",
            "string->list", "list->string", "char->integer", "integer->char",
            "call/cc", "call-with-current-continuation",
            "eq?", "equal?", "map",
            "abs", "modulo", "remainder", "quotient",
            "min", "max", "expt",
            "zero?", "positive?", "negative?", "odd?", "even?",
            "list-ref", "list-tail", "list?", "assoc",
            "char-alphabetic?", "char-numeric?",
            "char-upcase", "char-downcase",
            "char=?", "char<?",
            "string=?", "string<?", "string-ci=?",
            "string-upcase", "string-downcase",
            "eqv?",
            "vector", "make-vector", "vector-ref", "vector-set!",
            "vector-length", "vector?", "vector->list", "list->vector",
            "dynamic-wind", "reverse",
            "raise", "with-exception-handler",
            "values", "call-with-values",
            "exact?", "inexact?", "exact->inexact", "inexact->exact",
            "numerator", "denominator", "rational?", "integer?",
            "set-car!", "set-cdr!",
            // c[ad]{2}r
            "caar", "cadr", "cdar", "cddr",
            // c[ad]{3}r
            "caaar", "caadr", "cadar", "caddr", "cdaar", "cdadr", "cddar", "cdddr",
            // c[ad]{4}r
            "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "caddar", "cadddr",
            "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
            "for-each", "member", "memq", "memv", "assq", "assv",
            "procedure?", "complex?", "real?",
            // char predicates and comparisons
            "char-lower-case?", "char-upper-case?", "char-whitespace?",
            "char<=?", "char>=?", "char>?",
            "char-ci=?", "char-ci<?", "char-ci>?", "char-ci<=?", "char-ci>=?",
            // string comparisons
            "string>?", "string>=?", "string<=?",
            "string-ci<?", "string-ci>?", "string-ci<=?", "string-ci>=?",
            // math
            "floor", "ceiling", "round", "truncate",
            "sqrt", "sin", "cos", "tan", "asin", "acos", "atan", "exp", "log",
            "gcd", "lcm",
            // string constructors
            "make-string", "string",
            // I/O
            "write-char",
            "call-with-input-file", "call-with-output-file",
            "open-input-file", "open-output-file",
            "close-input-port", "close-output-port",
            "current-input-port", "current-output-port",
            "input-port?", "output-port?",
            "read", "read-char", "peek-char", "eof-object?",
            "syntax->datum", "datum->syntax",
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

    /// Return the parent environment, or self if there is no parent.
    pub fn parent_or_self(this: &Rc<Env>) -> Rc<Env> {
        match &this.parent {
            Some(p) => Rc::clone(p),
            None => Rc::clone(this),
        }
    }

    /// Create a minimal empty env (no builtins). Used internally for syntax expansion.
    pub fn empty_for_subst() -> Rc<Self> {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            parent: None,
            output: Rc::new(RefCell::new(String::new())),
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
