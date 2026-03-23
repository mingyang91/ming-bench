use std::any::Any;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use crate::scheme::env::Env;
use crate::scheme::parser::Expr;

/// Opaque wrapper for a captured continuation stack.
#[derive(Clone)]
pub struct CapturedCont(pub Rc<dyn Any>);

impl fmt::Debug for CapturedCont {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<continuation>")
    }
}

/// Whether a Scheme string can be mutated via `string-set!`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringMutability {
    Immutable,
    Mutable,
}

/// A Scheme value.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Str(Rc<RefCell<String>>, StringMutability),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Void,
    Builtin(String),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        closure_env: Rc<Env>,
    },
    Continuation(CapturedCont),
    /// A fixed-size mutable vector.
    Vector(Rc<RefCell<Vec<Value>>>),
    /// A dotted pair (improper list): (a . b) where b is not a list.
    DottedPair(Box<Value>, Box<Value>),
    SyntaxRules {
        literals: Vec<String>,
        rules: Vec<(Vec<Expr>, Expr)>,
        def_env: Rc<Env>,
    },
    /// Multiple return values from `(values ...)`.
    MultipleValues(Vec<Value>),
}

impl Value {
    /// Convenience constructor for immutable string values (literals).
    pub fn new_str(s: String) -> Self {
        Value::Str(Rc::new(RefCell::new(s)), StringMutability::Immutable)
    }

    /// Convenience constructor for mutable string values (string-copy results).
    pub fn new_mutable_str(s: String) -> Self {
        Value::Str(Rc::new(RefCell::new(s)), StringMutability::Mutable)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a, _), Value::Str(b, _)) => *a.borrow() == *b.borrow(),
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Vector(a), Value::Vector(b)) => *a.borrow() == *b.borrow(),
            (Value::Void, Value::Void) => true,
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (Value::DottedPair(a1, b1), Value::DottedPair(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::Continuation(_), Value::Continuation(_)) => false,
            (Value::SyntaxRules { .. }, Value::SyntaxRules { .. }) => false,
            (Value::MultipleValues(a), Value::MultipleValues(b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    /// Display string for output (write-style: strings get quotes).
    pub fn to_display_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s, _) => format!("\"{}\"", s.borrow()),
            Value::Symbol(s) => s.clone(),
            Value::Char(c) => format!("#\\{}", c),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display_string()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Vector(v) => {
                let inner: Vec<String> = v.borrow().iter().map(|e| e.to_display_string()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::DottedPair(a, b) => format!("({} . {})", a.to_display_string(), b.to_display_string()),
            Value::Void => "".into(),
            Value::Builtin(name) => format!("#<procedure:{}>", name),
            Value::Lambda { .. } => "#<procedure>".into(),
            Value::Continuation(_) => "#<continuation>".into(),
            Value::SyntaxRules { .. } => "#<macro>".into(),
            Value::MultipleValues(vals) => {
                let inner: Vec<String> = vals.iter().map(|v| v.to_display_string()).collect();
                format!("#<values: {}>", inner.join(" "))
            }
        }
    }

    /// Display string for `display` — strings without quotes.
    pub fn to_display_output(&self) -> String {
        match self {
            Value::Str(s, _) => s.borrow().clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display_output()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Vector(v) => {
                let inner: Vec<String> = v.borrow().iter().map(|e| e.to_display_output()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::DottedPair(a, b) => format!("({} . {})", a.to_display_output(), b.to_display_output()),
            Value::Continuation(_) => "#<continuation>".into(),
            Value::SyntaxRules { .. } => "#<macro>".into(),
            Value::MultipleValues(_) => self.to_display_string(),
            other => other.to_display_string(),
        }
    }

    /// Returns true if this value is truthy (everything except #f).
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display_string())
    }
}
