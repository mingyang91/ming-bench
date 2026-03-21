use std::fmt::{self, Formatter};

use crate::scheme::ast::Expr;
use crate::scheme::ast::SourceLocation;
use crate::scheme::builtins::BuiltinProcedure;
use crate::scheme::environment::Environment;
use crate::scheme::error::EvalError;
use crate::scheme::string_value::SchemeString;

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

#[derive(Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(SchemeString),
    Character(char),
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
    pub rest_parameter: Option<String>,
    pub body: Vec<Expr>,
    pub environment: Environment,
}

impl Closure {
    pub fn new(
        name: Option<String>,
        parameters: Vec<String>,
        rest_parameter: Option<String>,
        body: Vec<Expr>,
        environment: Environment,
    ) -> Self {
        Self {
            name,
            parameters,
            rest_parameter,
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
    pub fn immutable_string(value: impl Into<String>) -> Self {
        Self::String(SchemeString::immutable(value))
    }

    pub fn render(&self) -> String {
        self.render_with_mode(RenderMode::Write)
    }

    pub fn render_display(&self) -> String {
        self.render_with_mode(RenderMode::Display)
    }

    fn render_with_mode(&self, mode: RenderMode) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(value) => render_boolean(*value),
            Self::String(value) => render_string(value, mode),
            Self::Character(value) => render_character(*value, mode),
            Self::Symbol(value) => value.clone(),
            Self::EmptyList => "()".into(),
            Self::Pair(car, cdr) => render_pair(car, cdr, mode),
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

    pub fn expect_string(&self, location: SourceLocation) -> Result<&SchemeString, EvalError> {
        match self {
            Self::String(value) => Ok(value),
            _ => Err(EvalError::TypeMismatch {
                location,
                expected: "string",
                found: self.type_name(),
            }),
        }
    }

    pub fn expect_char(&self, location: SourceLocation) -> Result<char, EvalError> {
        match self {
            Self::Character(value) => Ok(*value),
            _ => Err(EvalError::TypeMismatch {
                location,
                expected: "char",
                found: self.type_name(),
            }),
        }
    }

    pub fn expect_symbol(&self, location: SourceLocation) -> Result<&str, EvalError> {
        match self {
            Self::Symbol(value) => Ok(value),
            _ => Err(EvalError::TypeMismatch {
                location,
                expected: "symbol",
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
            Self::Character(_) => "char",
            Self::Symbol(_) => "symbol",
            Self::EmptyList => "null",
            Self::Pair(_, _) => "pair",
            Self::Builtin(_) | Self::Closure(_) => "procedure",
            Self::Void => "void",
        }
    }
}

pub fn list_from_values(values: &[Value]) -> Value {
    values
        .iter()
        .rev()
        .cloned()
        .fold(Value::EmptyList, |tail, value| {
            Value::Pair(Box::new(value), Box::new(tail))
        })
}

fn render_boolean(value: bool) -> String {
    if value {
        "#t".into()
    } else {
        "#f".into()
    }
}

fn render_string(value: &SchemeString, mode: RenderMode) -> String {
    let rendered = value.as_string();

    match mode {
        RenderMode::Write => format!("\"{}\"", escape_string_contents(&rendered)),
        RenderMode::Display => rendered,
    }
}

fn render_character(value: char, mode: RenderMode) -> String {
    match mode {
        RenderMode::Display => value.to_string(),
        RenderMode::Write => match value {
            ' ' => "#\\space".into(),
            '\n' => "#\\newline".into(),
            _ => format!("#\\{value}"),
        },
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

fn render_pair(car: &Value, cdr: &Value, mode: RenderMode) -> String {
    let mut rendered = String::from("(");
    render_pair_contents(car, cdr, mode, &mut rendered);
    rendered.push(')');
    rendered
}

fn render_pair_contents(car: &Value, cdr: &Value, mode: RenderMode, rendered: &mut String) {
    rendered.push_str(&car.render_with_mode(mode));

    match cdr {
        Value::EmptyList => {}
        Value::Pair(item, remainder) => {
            rendered.push(' ');
            render_pair_contents(item, remainder, mode, rendered);
        }
        other => {
            rendered.push_str(" . ");
            rendered.push_str(&other.render_with_mode(mode));
        }
    }
}
