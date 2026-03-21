use std::collections::HashSet;
use std::fmt::{self, Formatter};
use std::rc::Rc;

use crate::scheme::ast::Expr;
use crate::scheme::ast::SourceLocation;
use crate::scheme::builtins::BuiltinProcedure;
use crate::scheme::continuation::CapturedContinuation;
use crate::scheme::environment::Environment;
use crate::scheme::error::{ArgCount, EvalError};
use crate::scheme::number::Number;
use crate::scheme::pair_value::SchemePair;
use crate::scheme::record::{RecordProcedure, SchemeRecord};
use crate::scheme::string_value::SchemeString;
use crate::scheme::vector_value::SchemeVector;

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

#[derive(Clone)]
pub(crate) struct MultipleValues {
    values: Vec<Value>,
    location: SourceLocation,
}

#[derive(Clone)]
pub(crate) enum SyntaxValue {
    Single(Expr),
    Repeated(Vec<Expr>),
}

impl MultipleValues {
    fn new(values: Vec<Value>, location: SourceLocation) -> Self {
        Self { values, location }
    }

    fn len(&self) -> usize {
        self.values.len()
    }

    fn location(&self) -> SourceLocation {
        self.location
    }

    pub(crate) fn values(&self) -> &[Value] {
        &self.values
    }

    fn into_values(self) -> Vec<Value> {
        self.values
    }
}

#[derive(Clone)]
pub enum Value {
    Number(Number),
    Boolean(bool),
    String(SchemeString),
    Character(char),
    Symbol(String),
    Syntax(SyntaxValue),
    EmptyList,
    Pair(SchemePair),
    Vector(SchemeVector),
    Record(SchemeRecord),
    Builtin(BuiltinProcedure),
    RecordProcedure(RecordProcedure),
    CallWithCurrentContinuation,
    CallWithValues,
    DynamicWind,
    Raise,
    ValuesProcedure,
    WithExceptionHandler,
    MultipleValues(MultipleValues),
    Continuation(Rc<CapturedContinuation>),
    Closure(Closure),
    DefinitionEnvironment(Environment),
    Void,
    Uninitialized,
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

    pub fn multiple(values: Vec<Value>, location: SourceLocation) -> Self {
        debug_assert_ne!(
            values.len(),
            1,
            "multiple-values carrier should only represent zero or multiple values"
        );
        Self::MultipleValues(MultipleValues::new(values, location))
    }

    pub fn pair(car: Value, cdr: Value) -> Self {
        Self::Pair(SchemePair::new(car, cdr))
    }

    pub(crate) fn syntax(expression: Expr) -> Self {
        Self::Syntax(SyntaxValue::Single(expression))
    }

    pub(crate) fn repeated_syntax(expressions: Vec<Expr>) -> Self {
        Self::Syntax(SyntaxValue::Repeated(expressions))
    }

    pub fn into_values(self) -> Vec<Value> {
        match self {
            Self::MultipleValues(values) => values.into_values(),
            value => vec![value],
        }
    }

    pub fn expect_single(self) -> Result<Self, EvalError> {
        match self {
            Self::MultipleValues(values) => Err(EvalError::WrongValueCount {
                location: values.location(),
                expected: ArgCount::Exactly(1),
                got: values.len(),
            }),
            value => Ok(value),
        }
    }

    pub fn render(&self) -> String {
        self.render_with_mode(RenderMode::Write)
    }

    pub fn render_display(&self) -> String {
        self.render_with_mode(RenderMode::Display)
    }

    fn render_with_mode(&self, mode: RenderMode) -> String {
        render_value(self, mode, &mut HashSet::new())
    }

    pub fn expect_number(&self, location: SourceLocation) -> Result<Number, EvalError> {
        match self {
            Self::Number(value) => Ok(*value),
            _ => Err(EvalError::TypeMismatch {
                location,
                expected: "number",
                found: self.type_name(),
            }),
        }
    }

    pub fn expect_integer(&self, location: SourceLocation) -> Result<i64, EvalError> {
        let number = self.expect_number(location)?;
        number.integer_value().ok_or(EvalError::TypeMismatch {
            location,
            expected: "integer",
            found: self.type_name(),
        })
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

    pub(crate) fn expect_syntax(
        &self,
        location: SourceLocation,
    ) -> Result<&SyntaxValue, EvalError> {
        match self {
            Self::Syntax(value) => Ok(value),
            _ => Err(EvalError::ExpectedSyntaxObject {
                location,
                found: self.type_name(),
            }),
        }
    }

    pub(crate) fn expect_single_syntax(&self, location: SourceLocation) -> Result<Expr, EvalError> {
        match self.expect_syntax(location)? {
            SyntaxValue::Single(expression) => Ok(expression.clone()),
            SyntaxValue::Repeated(_) => Err(EvalError::ExpectedSyntaxObject {
                location,
                found: "syntax sequence",
            }),
        }
    }

    pub fn expect_pair(&self, location: SourceLocation) -> Result<SchemePair, EvalError> {
        match self {
            Self::Pair(value) => Ok(value.clone()),
            _ => Err(EvalError::TypeMismatch {
                location,
                expected: "pair",
                found: self.type_name(),
            }),
        }
    }

    pub fn expect_vector(&self, location: SourceLocation) -> Result<&SchemeVector, EvalError> {
        match self {
            Self::Vector(value) => Ok(value),
            _ => Err(EvalError::TypeMismatch {
                location,
                expected: "vector",
                found: self.type_name(),
            }),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Number(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
            Self::Character(_) => "char",
            Self::Symbol(_) => "symbol",
            Self::Syntax(_) => "syntax",
            Self::EmptyList => "null",
            Self::Pair(_) => "pair",
            Self::Vector(_) => "vector",
            Self::Record(_) => "record",
            Self::Builtin(_)
            | Self::RecordProcedure(_)
            | Self::CallWithCurrentContinuation
            | Self::CallWithValues
            | Self::DynamicWind
            | Self::Raise
            | Self::ValuesProcedure
            | Self::WithExceptionHandler
            | Self::Continuation(_)
            | Self::Closure(_) => "procedure",
            Self::MultipleValues(_) => "values",
            Self::DefinitionEnvironment(_) => "internal",
            Self::Void => "void",
            Self::Uninitialized => "uninitialized",
        }
    }
}

pub fn list_from_values(values: &[Value]) -> Value {
    values
        .iter()
        .rev()
        .cloned()
        .fold(Value::EmptyList, |tail, value| Value::pair(value, tail))
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

fn render_multiple_values(values: &MultipleValues) -> String {
    format!("#<values:{}>", values.values().len())
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

fn render_value(value: &Value, mode: RenderMode, seen_pairs: &mut HashSet<usize>) -> String {
    match value {
        Value::Number(value) => value.render(),
        Value::Boolean(value) => render_boolean(*value),
        Value::String(value) => render_string(value, mode),
        Value::Character(value) => render_character(*value, mode),
        Value::Symbol(value) => value.clone(),
        Value::Syntax(_) => "#<syntax>".into(),
        Value::EmptyList => "()".into(),
        Value::Pair(pair) => render_pair(pair, mode, seen_pairs),
        Value::Vector(vector) => render_vector(vector, mode, seen_pairs),
        Value::Record(record) => record.render(),
        Value::Builtin(procedure) => format!("#<procedure:{}>", procedure.name()),
        Value::RecordProcedure(procedure) => format!("#<procedure:{}>", procedure.name()),
        Value::CallWithCurrentContinuation => "#<procedure:call/cc>".into(),
        Value::CallWithValues => "#<procedure:call-with-values>".into(),
        Value::DynamicWind => "#<procedure:dynamic-wind>".into(),
        Value::Raise => "#<procedure:raise>".into(),
        Value::ValuesProcedure => "#<procedure:values>".into(),
        Value::WithExceptionHandler => "#<procedure:with-exception-handler>".into(),
        Value::MultipleValues(values) => render_multiple_values(values),
        Value::Continuation(_) => "#<continuation>".into(),
        Value::Closure(closure) => render_closure(closure),
        Value::DefinitionEnvironment(_) => "#<syntax-context>".into(),
        Value::Void => "#<void>".into(),
        Value::Uninitialized => "#<uninitialized>".into(),
    }
}

fn render_pair(pair: &SchemePair, mode: RenderMode, seen_pairs: &mut HashSet<usize>) -> String {
    let id = pair.id();
    if !seen_pairs.insert(id) {
        return "#<cycle>".into();
    }

    let (car, cdr) = pair.parts();
    let mut rendered = String::from("(");
    render_pair_contents(&car, &cdr, mode, seen_pairs, &mut rendered);
    rendered.push(')');
    seen_pairs.remove(&id);
    rendered
}

fn render_vector(
    vector: &SchemeVector,
    mode: RenderMode,
    seen_pairs: &mut HashSet<usize>,
) -> String {
    let contents = vector
        .items()
        .into_iter()
        .map(|item| render_value(&item, mode, seen_pairs))
        .collect::<Vec<_>>()
        .join(" ");

    format!("#({contents})")
}

fn render_pair_contents(
    car: &Value,
    cdr: &Value,
    mode: RenderMode,
    seen_pairs: &mut HashSet<usize>,
    rendered: &mut String,
) {
    rendered.push_str(&render_value(car, mode, seen_pairs));

    match cdr {
        Value::EmptyList => {}
        Value::Pair(pair) => {
            let id = pair.id();
            if !seen_pairs.insert(id) {
                rendered.push_str(" . #<cycle>");
                return;
            }

            let (item, remainder) = pair.parts();
            rendered.push(' ');
            render_pair_contents(&item, &remainder, mode, seen_pairs, rendered);
            seen_pairs.remove(&id);
        }
        other => {
            rendered.push_str(" . ");
            rendered.push_str(&render_value(other, mode, seen_pairs));
        }
    }
}
