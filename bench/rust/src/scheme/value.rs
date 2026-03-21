use std::fmt::{self, Formatter};

use crate::scheme::ast::Expr;
use crate::scheme::ast::SourceLocation;
use crate::scheme::builtins::BuiltinProcedure;
use crate::scheme::environment::Environment;
use crate::scheme::error::EvalError;

#[derive(Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    EmptyList,
    Pair(Box<Value>, Box<Value>),
    Builtin(BuiltinProcedure),
    Closure(Closure),
    Void,
}

#[derive(Clone)]
pub struct Closure {
    pub name: Option<String>,
    pub parameters: Vec<String>,
    pub body: Vec<Expr>,
    pub environment: Environment,
}

impl Closure {
    pub fn new(
        name: Option<String>,
        parameters: Vec<String>,
        body: Vec<Expr>,
        environment: Environment,
    ) -> Self {
        Self {
            name,
            parameters,
            body,
            environment,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.render())
    }
}

impl Value {
    pub fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(value) => render_boolean(*value),
            Self::String(value) => format!("\"{}\"", escape_string_contents(value)),
            Self::Symbol(value) => value.clone(),
            Self::EmptyList => "()".into(),
            Self::Pair(car, cdr) => render_pair(car, cdr),
            Self::Builtin(procedure) => format!("#<procedure:{}>", procedure.name()),
            Self::Closure(closure) => render_closure(closure),
            Self::Void => "#<void>".into(),
        }
    }

    pub fn expect_number(&self, location: SourceLocation) -> Result<i64, EvalError> {
        match self {
            Self::Integer(value) => Ok(*value),
            _ => Err(EvalError::TypeMismatch {
                location,
                expected: "number",
                found: self.type_name(),
            }),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Integer(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::EmptyList => "null",
            Self::Pair(_, _) => "pair",
            Self::Builtin(_) | Self::Closure(_) => "procedure",
            Self::Void => "void",
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

fn render_closure(closure: &Closure) -> String {
    closure.name.as_ref().map_or_else(
        || "#<procedure>".into(),
        |name| format!("#<procedure:{name}>"),
    )
}

fn render_pair(car: &Value, cdr: &Value) -> String {
    let mut rendered = String::from("(");
    render_pair_contents(car, cdr, &mut rendered);
    rendered.push(')');
    rendered
}

fn render_pair_contents(car: &Value, cdr: &Value, rendered: &mut String) {
    rendered.push_str(&car.render());

    match cdr {
        Value::EmptyList => {}
        Value::Pair(item, remainder) => {
            rendered.push(' ');
            render_pair_contents(item, remainder, rendered);
        }
        other => {
            rendered.push_str(" . ");
            rendered.push_str(&other.render());
        }
    }
}
