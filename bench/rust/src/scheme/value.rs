use std::fmt;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Void,
    Builtin(String),
}

impl Value {
    /// Display string for output (write-style: strings get quotes).
    pub fn to_display_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_display_string()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Void => "".into(),
            Value::Builtin(name) => format!("#<procedure:{}>", name),
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
