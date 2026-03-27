use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(std::string::String),
    Symbol(std::string::String),
    List(Vec<Value>),
    Void,
}

impl Value {
    pub fn to_display(&self) -> std::string::String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::String(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<std::string::String> =
                    elems.iter().map(|v| v.to_display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Void => "".into(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display())
    }
}
