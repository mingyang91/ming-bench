use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

use crate::scheme::environment::Environment;
use crate::scheme::parser::Expr;
use crate::scheme::procedure::Procedure;

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Procedure(Rc<Procedure>),
    Void,
}

impl Value {
    pub(crate) fn from_literal(expression: &Expr) -> Option<Self> {
        match expression {
            Expr::Integer(value) => Some(Self::Integer(*value)),
            Expr::Boolean(value) => Some(Self::Boolean(*value)),
            Expr::String(value) => Some(Self::String(value.clone())),
            Expr::Symbol(_) | Expr::List(_) => None,
        }
    }

    pub(crate) fn from_quoted_expr(expression: &Expr) -> Self {
        match expression {
            Expr::Integer(value) => Self::Integer(*value),
            Expr::Boolean(value) => Self::Boolean(*value),
            Expr::String(value) => Self::String(value.clone()),
            Expr::Symbol(value) => Self::Symbol(value.clone()),
            Expr::List(values) => Self::List(values.iter().map(Self::from_quoted_expr).collect()),
        }
    }

    pub(crate) fn procedure(
        parameters: Vec<String>,
        body: Vec<Expr>,
        environment: Environment,
    ) -> Self {
        Self::Procedure(Rc::new(Procedure::new(parameters, body, environment)))
    }

    pub(crate) fn as_procedure(&self) -> Option<&Procedure> {
        match self {
            Self::Procedure(procedure) => Some(procedure.as_ref()),
            _ => None,
        }
    }

    pub(crate) fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    pub(crate) fn kind(&self) -> &'static str {
        match self {
            Self::Integer(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(value) => write!(f, "{value}"),
            Self::Boolean(value) => f.write_str(if *value { "#t" } else { "#f" }),
            Self::String(value) => write_string(value, f),
            Self::Symbol(value) => f.write_str(value),
            Self::List(values) => write_list(values, f),
            Self::Procedure(_) => f.write_str("#<procedure>"),
            Self::Void => f.write_str("#<void>"),
        }
    }
}

fn write_list(values: &[Value], f: &mut Formatter<'_>) -> fmt::Result {
    f.write_str("(")?;

    for (index, value) in values.iter().enumerate() {
        if index > 0 {
            f.write_str(" ")?;
        }

        write!(f, "{value}")?;
    }

    f.write_str(")")
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
