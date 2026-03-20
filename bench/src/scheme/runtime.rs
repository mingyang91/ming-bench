use std::rc::Rc;

use crate::scheme::ast::{BindingKey, Expr, ExprRef};
use crate::scheme::env::{CellRef, EnvRef};
use crate::scheme::error::EvalError;
use crate::scheme::macros::MacroBindings;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BindingName {
    pub display: String,
    pub key: BindingKey,
}

#[derive(Clone, Debug)]
pub enum Parameters {
    Fixed(Vec<BindingName>),
    Variadic {
        fixed: Vec<BindingName>,
        rest: BindingName,
    },
}

#[derive(Clone, Debug)]
pub struct Lambda {
    pub params: Parameters,
    pub body: Vec<ExprRef>,
    pub env: EnvRef,
}

#[derive(Clone, Debug)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    NumberEq,
    LessEqual,
    Not,
    Cons,
    Car,
    Cdr,
    NullPredicate,
    List,
    Length,
    StringPredicate,
    NumberPredicate,
    BooleanPredicate,
    PairPredicate,
    SymbolPredicate,
    Apply,
    CallCc,
}

#[derive(Clone, Debug)]
pub enum Procedure {
    Builtin(Builtin),
    Lambda(Lambda),
    Continuation(ContRef),
}

#[derive(Clone, Debug)]
pub struct Pair {
    pub car: Value,
    pub cdr: Value,
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    Nil,
    Pair(Rc<Pair>),
    Procedure(Rc<Procedure>),
    Void,
}

pub type ContRef = Rc<Continuation>;

#[derive(Clone, Debug)]
pub struct CondClause {
    pub test: Option<ExprRef>,
    pub body: Vec<ExprRef>,
}

#[derive(Clone, Debug)]
pub enum LetKind {
    Plain(Vec<BindingName>),
    Named {
        name: BindingName,
        params: Vec<BindingName>,
    },
}

#[derive(Clone, Debug)]
pub enum Continuation {
    Done,
    ProcedureReturn {
        next: ContRef,
    },
    CallCcReturn {
        current: ContRef,
        caller: ContRef,
    },
    Program {
        remaining: Vec<ExprRef>,
        env: EnvRef,
        macros: MacroBindings,
        next: ContRef,
    },
    Sequence {
        remaining: Vec<ExprRef>,
        env: EnvRef,
        next: ContRef,
    },
    If {
        consequent: ExprRef,
        alternate: ExprRef,
        env: EnvRef,
        next: ContRef,
    },
    Define {
        cell: CellRef,
        next: ContRef,
    },
    Set {
        cell: CellRef,
        next: ContRef,
    },
    ApplicationOperator {
        operands: Vec<ExprRef>,
        env: EnvRef,
        next: ContRef,
    },
    ApplicationArgument {
        operator: Value,
        evaluated: Vec<Value>,
        remaining: Vec<ExprRef>,
        env: EnvRef,
        next: ContRef,
    },
    And {
        remaining: Vec<ExprRef>,
        env: EnvRef,
        next: ContRef,
    },
    Or {
        remaining: Vec<ExprRef>,
        env: EnvRef,
        next: ContRef,
    },
    Cond {
        body: Vec<ExprRef>,
        remaining: Vec<CondClause>,
        env: EnvRef,
        next: ContRef,
    },
    Let {
        kind: LetKind,
        remaining_inits: Vec<ExprRef>,
        evaluated: Vec<Value>,
        body: Vec<ExprRef>,
        outer_env: EnvRef,
        next: ContRef,
    },
}

pub fn bool_value(flag: bool) -> Value {
    Value::Bool(flag)
}

pub fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

pub fn pair_value(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(Pair { car, cdr }))
}

pub fn list_from_values(items: &[Value]) -> Value {
    items
        .iter()
        .rev()
        .cloned()
        .fold(Value::Nil, |tail, value| pair_value(value, tail))
}

pub fn values_from_list(value: &Value, operation: &str) -> Result<Vec<Value>, EvalError> {
    let mut elements = Vec::new();
    let mut current = value.clone();
    loop {
        match current {
            Value::Nil => return Ok(elements),
            Value::Pair(pair) => {
                elements.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            _ => {
                return Err(EvalError::ImproperList {
                    operation: operation.to_owned(),
                });
            }
        }
    }
}

pub fn quoted_value(expr: &ExprRef) -> Value {
    match expr.as_ref() {
        Expr::Number(value) => Value::Number(*value),
        Expr::Bool(value) => Value::Bool(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(identifier) => Value::Symbol(identifier.name().to_owned()),
        Expr::List(items) => quote_list(items),
        Expr::DottedList(items, tail) => quote_dotted(items, tail),
    }
}

pub fn render_value(value: &Value) -> String {
    match value {
        Value::Number(number) => number.to_string(),
        Value::Bool(true) => "#t".to_owned(),
        Value::Bool(false) => "#f".to_owned(),
        Value::String(text) => format!("\"{text}\""),
        Value::Symbol(symbol) => symbol.clone(),
        Value::Nil => "()".to_owned(),
        Value::Pair(pair) => render_pair(pair),
        Value::Procedure(closure) => render_procedure(closure),
        Value::Void => "#<void>".to_owned(),
    }
}

pub fn value_type(value: &Value) -> &'static str {
    match value {
        Value::Number(_) => "number",
        Value::Bool(_) => "boolean",
        Value::String(_) => "string",
        Value::Symbol(_) => "symbol",
        Value::Nil => "null",
        Value::Pair(_) => "pair",
        Value::Procedure(_) => "procedure",
        Value::Void => "void",
    }
}

impl Builtin {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::NumberEq => "=",
            Self::LessEqual => "<=",
            Self::Not => "not",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::NullPredicate => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::StringPredicate => "string?",
            Self::NumberPredicate => "number?",
            Self::BooleanPredicate => "boolean?",
            Self::PairPredicate => "pair?",
            Self::SymbolPredicate => "symbol?",
            Self::Apply => "apply",
            Self::CallCc => "call/cc",
        }
    }
}

fn quote_list(items: &[ExprRef]) -> Value {
    items
        .iter()
        .rev()
        .map(quoted_value)
        .fold(Value::Nil, |tail, value| pair_value(value, tail))
}

fn quote_dotted(items: &[ExprRef], tail: &ExprRef) -> Value {
    items
        .iter()
        .rev()
        .map(quoted_value)
        .fold(quoted_value(tail), |rest, value| pair_value(value, rest))
}

fn render_pair(pair: &Pair) -> String {
    let mut parts = vec![render_value(&pair.car)];
    let mut tail = pair.cdr.clone();
    loop {
        match tail {
            Value::Nil => return format!("({})", parts.join(" ")),
            Value::Pair(next) => {
                parts.push(render_value(&next.car));
                tail = next.cdr.clone();
            }
            other => return format!("({} . {})", parts.join(" "), render_value(&other)),
        }
    }
}

fn render_procedure(procedure: &Procedure) -> String {
    match procedure {
        Procedure::Builtin(builtin) => format!("#<procedure:{}>", builtin.name()),
        Procedure::Lambda(_) => "#<procedure>".to_owned(),
        Procedure::Continuation(_) => "#<continuation>".to_owned(),
    }
}
