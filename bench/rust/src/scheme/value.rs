use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::error::EvalError;
use super::parser::Expr;

#[derive(Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    Nil,
    Pair(Box<(Value, Value)>),
    Builtin(Builtin),
    Closure(Rc<Closure>),
    Void,
}

#[derive(Clone, Copy)]
pub struct Builtin {
    pub name: &'static str,
    pub func: fn(&[Value]) -> Result<Value, EvalError>,
}

#[derive(Clone)]
pub struct Closure {
    pub params: Vec<String>,
    pub body: Vec<Expr>,
    pub env: Env,
}

#[derive(Clone)]
pub struct Env(Rc<EnvData>);

struct EnvData {
    parent: Option<Env>,
    bindings: RefCell<HashMap<String, Value>>,
}

impl Env {
    pub fn new_root() -> Self {
        Self(Rc::new(EnvData {
            parent: None,
            bindings: RefCell::new(HashMap::new()),
        }))
    }

    pub fn child(&self) -> Self {
        Self(Rc::new(EnvData {
            parent: Some(self.clone()),
            bindings: RefCell::new(HashMap::new()),
        }))
    }

    pub fn define(&self, name: impl Into<String>, value: Value) {
        self.0.bindings.borrow_mut().insert(name.into(), value);
    }

    pub fn get(&self, name: &str) -> Result<Value, EvalError> {
        if let Some(value) = self.0.bindings.borrow().get(name) {
            return Ok(value.clone());
        }

        match &self.0.parent {
            Some(parent) => parent.get(name),
            None => Err(EvalError::UnboundVariable {
                name: name.to_string(),
            }),
        }
    }
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) => "number",
            Value::Bool(_) => "boolean",
            Value::String(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::Nil | Value::Pair(_) => "pair",
            Value::Builtin(_) | Value::Closure(_) => "procedure",
            Value::Void => "void",
        }
    }
}

pub fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

pub fn list_from_vec(values: Vec<Value>) -> Value {
    values
        .into_iter()
        .rev()
        .fold(Value::Nil, |tail, head| Value::Pair(Box::new((head, tail))))
}

pub fn render_value(value: &Value) -> String {
    match value {
        Value::Int(number) => number.to_string(),
        Value::Bool(true) => "#t".into(),
        Value::Bool(false) => "#f".into(),
        Value::String(text) => render_string(text),
        Value::Symbol(name) => name.clone(),
        Value::Nil => "()".into(),
        Value::Pair(_) => render_pair(value),
        Value::Builtin(_) | Value::Closure(_) => "#<procedure>".into(),
        Value::Void => "#<void>".into(),
    }
}

fn render_string(text: &str) -> String {
    let mut rendered = String::with_capacity(text.len() + 2);
    rendered.push('"');

    for ch in text.chars() {
        match ch {
            '\\' => rendered.push_str("\\\\"),
            '"' => rendered.push_str("\\\""),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            other => rendered.push(other),
        }
    }

    rendered.push('"');
    rendered
}

fn render_pair(value: &Value) -> String {
    let mut rendered = String::from("(");
    let mut cursor = value;
    let mut first = true;

    loop {
        match cursor {
            Value::Pair(cell) => {
                if !first {
                    rendered.push(' ');
                }

                rendered.push_str(&render_value(&cell.0));
                cursor = &cell.1;
                first = false;
            }
            Value::Nil => {
                rendered.push(')');
                return rendered;
            }
            other => {
                rendered.push_str(" . ");
                rendered.push_str(&render_value(other));
                rendered.push(')');
                return rendered;
            }
        }
    }
}
