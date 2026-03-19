use std::fmt::{self, Display, Formatter};

use crate::scheme::parser::Expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl From<Expr> for Value {
    fn from(expression: Expr) -> Self {
        match expression {
            Expr::Integer(value) => Self::Integer(value),
            Expr::Boolean(value) => Self::Boolean(value),
            Expr::String(value) => Self::String(value),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(value) => write!(f, "{value}"),
            Self::Boolean(value) => f.write_str(if *value { "#t" } else { "#f" }),
            Self::String(value) => write_string(value, f),
        }
    }
}

fn write_string(value: &str, f: &mut Formatter<'_>) -> fmt::Result {
    f.write_str("\"")?;

    for ch in value.chars() {
        match ch {
            '"' => f.write_str("\\\"")?,
            '\\' => f.write_str("\\\\")?,
            '\n' => f.write_str("\\n")?,
            '\r' => f.write_str("\\r")?,
            '\t' => f.write_str("\\t")?,
            _ => write!(f, "{ch}")?,
        }
    }

    f.write_str("\"")
}
