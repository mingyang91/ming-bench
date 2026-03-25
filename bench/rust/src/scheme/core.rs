use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::error::EvalError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Position {
    pub(crate) line: usize,
    pub(crate) col: usize,
}

impl Position {
    pub(crate) fn attach(self, error: EvalError) -> EvalError {
        error.with_position(self.line, self.col)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Bool(bool, Position),
    Int(i64, Position),
    String(String, Position),
    Char(char, Position),
    Symbol(String, Position),
    List(Vec<Expr>, Position),
}

impl Expr {
    pub(crate) fn pos(&self) -> Position {
        match self {
            Self::Bool(_, pos)
            | Self::Int(_, pos)
            | Self::String(_, pos)
            | Self::Char(_, pos)
            | Self::Symbol(_, pos)
            | Self::List(_, pos) => *pos,
        }
    }
}

pub(crate) type StringRef = Rc<RefCell<String>>;
pub(crate) type EnvRef = Rc<RefCell<Environment>>;

#[derive(Clone)]
pub(crate) enum Value {
    Bool(bool),
    Int(i64),
    String(StringRef),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Procedure(Rc<Procedure>),
    Void,
}

impl Value {
    pub(crate) fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    pub(crate) fn type_name(&self) -> &'static str {
        match self {
            Self::Bool(_) => "boolean",
            Self::Int(_) => "number",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::Char(_) => "character",
            Self::List(_) => "list",
            Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    pub(crate) fn render(&self) -> String {
        render_value(self, RenderMode::Write)
    }

    pub(crate) fn render_display(&self) -> String {
        render_value(self, RenderMode::Display)
    }

    pub(crate) fn render_for_error(&self) -> String {
        match self {
            Self::Void => "#<void>".into(),
            _ => self.render(),
        }
    }
}

#[derive(Clone)]
pub(crate) enum Procedure {
    Builtin(BuiltinProcedure),
    Lambda(LambdaProcedure),
}

#[derive(Clone, Copy)]
pub(crate) struct BuiltinProcedure {
    pub(crate) name: &'static str,
    pub(crate) func: fn(&[Value], &mut Runtime) -> Result<Value, EvalError>,
}

#[derive(Clone)]
pub(crate) struct LambdaProcedure {
    pub(crate) name: Option<String>,
    pub(crate) params: Vec<String>,
    pub(crate) body: Vec<Expr>,
    pub(crate) env: EnvRef,
}

pub(crate) struct Environment {
    bindings: HashMap<String, Value>,
    parent: Option<EnvRef>,
}

impl Environment {
    pub(crate) fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            parent,
        }))
    }

    pub(crate) fn define(env: &EnvRef, name: String, value: Value) {
        env.borrow_mut().bindings.insert(name, value);
    }

    pub(crate) fn lookup(env: &EnvRef, name: &str) -> Option<Value> {
        let borrowed = env.borrow();
        if let Some(value) = borrowed.bindings.get(name).cloned() {
            return Some(value);
        }

        let parent = borrowed.parent.clone();
        drop(borrowed);
        parent.and_then(|parent| Self::lookup(&parent, name))
    }

    pub(crate) fn set(env: &EnvRef, name: &str, value: Value) -> bool {
        let mut borrowed = env.borrow_mut();
        if let Some(slot) = borrowed.bindings.get_mut(name) {
            *slot = value;
            return true;
        }

        let parent = borrowed.parent.clone();
        drop(borrowed);
        parent
            .map(|parent| Self::set(&parent, name, value))
            .unwrap_or(false)
    }
}

#[derive(Default)]
pub(crate) struct Runtime {
    output: String,
}

impl Runtime {
    pub(crate) fn display(&mut self, value: &Value) {
        self.output.push_str(&value.render_display());
    }

    pub(crate) fn write(&mut self, value: &Value) {
        self.output.push_str(&value.render());
    }

    pub(crate) fn newline(&mut self) {
        self.output.push('\n');
    }

    pub(crate) fn into_output(self) -> String {
        self.output
    }
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

pub(crate) fn make_string(text: impl Into<String>) -> Value {
    Value::String(Rc::new(RefCell::new(text.into())))
}

pub(crate) fn make_lambda(
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: &EnvRef,
) -> Value {
    Value::Procedure(Rc::new(Procedure::Lambda(LambdaProcedure {
        name,
        params,
        body,
        env: env.clone(),
    })))
}

pub(crate) fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Bool(value, _) => Value::Bool(*value),
        Expr::Int(value, _) => Value::Int(*value),
        Expr::String(value, _) => make_string(value.clone()),
        Expr::Char(value, _) => Value::Char(*value),
        Expr::Symbol(value, _) => Value::Symbol(value.clone()),
        Expr::List(items, _) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn render_value(value: &Value, mode: RenderMode) -> String {
    match value {
        Value::Bool(true) => "#t".into(),
        Value::Bool(false) => "#f".into(),
        Value::Int(value) => value.to_string(),
        Value::String(value) => {
            let text = value.borrow();
            match mode {
                RenderMode::Write => render_string(text.as_str()),
                RenderMode::Display => text.as_str().to_string(),
            }
        }
        Value::Symbol(value) => value.clone(),
        Value::Char(value) => match mode {
            RenderMode::Write => render_char(*value),
            RenderMode::Display => value.to_string(),
        },
        Value::List(values) => render_list(values, mode),
        Value::Procedure(_) => "#<procedure>".into(),
        Value::Void => String::new(),
    }
}

fn render_string(value: &str) -> String {
    let mut rendered = String::with_capacity(value.len() + 2);
    rendered.push('"');

    for ch in value.chars() {
        match ch {
            '"' => rendered.push_str("\\\""),
            '\\' => rendered.push_str("\\\\"),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            other => rendered.push(other),
        }
    }

    rendered.push('"');
    rendered
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".into(),
        '\n' => "#\\newline".into(),
        other => format!("#\\{other}"),
    }
}

fn render_list(values: &[Value], mode: RenderMode) -> String {
    let mut rendered = String::from("(");

    for (index, value) in values.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&render_value(value, mode));
    }

    rendered.push(')');
    rendered
}
