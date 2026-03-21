use crate::scheme::error::EvalError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl Value {
    pub fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(value) => render_boolean(*value),
            Self::String(value) => format!("\"{}\"", escape_string_contents(value)),
        }
    }

    pub fn expect_number(&self) -> Result<i64, EvalError> {
        match self {
            Self::Integer(value) => Ok(*value),
            _ => Err(EvalError::TypeMismatch {
                expected: "number",
                found: self.type_name(),
            }),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Integer(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
        }
    }
}

fn render_boolean(value: bool) -> String {
    if value {
        "#t".into()
    } else {
        "#f".into()
    }
}

fn escape_string_contents(value: &str) -> String {
    value.chars().fold(String::new(), |mut escaped, ch| {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(ch),
        }
        escaped
    })
}
