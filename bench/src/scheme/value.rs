use std::fmt;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::SchemeError;
use crate::scheme::cont::Frame;

// ---------------------------------------------------------------------------
// Lambda data
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct LambdaData {
    pub params: Vec<String>,
    pub rest: Option<String>,
    pub body: Vec<Value>,
    pub env: Env,
}

// ---------------------------------------------------------------------------
// Macro data
// ---------------------------------------------------------------------------

pub struct MacroData {
    pub literals: Vec<String>,
    pub rules: Vec<(Value, Value)>,
    pub def_env: Env,
}

impl fmt::Debug for MacroData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MacroData({} rules)", self.rules.len())
    }
}

impl Clone for MacroData {
    fn clone(&self) -> Self {
        Self {
            literals: self.literals.clone(),
            rules: self.rules.clone(),
            def_env: self.def_env.clone(),
        }
    }
}

// ---------------------------------------------------------------------------
// Built-in wrapper
// ---------------------------------------------------------------------------

pub type BuiltinFn = fn(&[Value]) -> Result<Value, SchemeError>;

#[derive(Clone)]
pub struct Builtin {
    pub name: &'static str,
    pub func: BuiltinFn,
}

impl fmt::Debug for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<builtin:{}>", self.name)
    }
}

// ---------------------------------------------------------------------------
// Value
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Pair(Rc<Value>, Rc<Value>),
    Nil,
    Void,
    Lambda(Rc<LambdaData>),
    Builtin(Builtin),
    Continuation(Rc<Vec<Frame>>),
    SyntaxRules(Rc<MacroData>),
}

// ---------------------------------------------------------------------------
// Display
// ---------------------------------------------------------------------------

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(n) => write!(f, "{n}"),
            Self::Boolean(true) => write!(f, "#t"),
            Self::Boolean(false) => write!(f, "#f"),
            Self::Str(s) => write!(f, "\"{s}\""),
            Self::Symbol(s) => write!(f, "{s}"),
            Self::Nil => write!(f, "()"),
            Self::Void => write!(f, "#<void>"),
            Self::Lambda(_) => write!(f, "#<procedure>"),
            Self::Builtin(b) => write!(f, "#<builtin:{}>", b.name),
            Self::Continuation(_) => write!(f, "#<continuation>"),
            Self::SyntaxRules(_) => write!(f, "#<macro>"),
            Self::Pair(car, cdr) => write_pair(f, car, cdr),
        }
    }
}

fn write_pair(f: &mut fmt::Formatter<'_>, car: &Value, cdr: &Value) -> fmt::Result {
    write!(f, "({car}")?;
    write_tail(f, cdr)?;
    write!(f, ")")
}

fn write_tail(f: &mut fmt::Formatter<'_>, val: &Value) -> fmt::Result {
    match val {
        Value::Nil => Ok(()),
        Value::Pair(car, cdr) => {
            write!(f, " {car}")?;
            write_tail(f, cdr)
        }
        other => write!(f, " . {other}"),
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Integer(_) => "integer",
            Self::Boolean(_) => "boolean",
            Self::Str(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::Pair(_, _) => "pair",
            Self::Nil => "nil",
            Self::Void => "void",
            Self::Lambda(_) => "procedure",
            Self::Builtin(_) => "builtin",
            Self::Continuation(_) => "continuation",
            Self::SyntaxRules(_) => "macro",
        }
    }

    pub fn as_integer(&self, op: &str) -> Result<i64, SchemeError> {
        match self {
            Self::Integer(n) => Ok(*n),
            other => Err(SchemeError::TypeError {
                op: op.into(),
                expected: "integer".into(),
                got: other.type_name().into(),
            }),
        }
    }
}

/// Convert a Scheme list (nested Pairs ending in Nil) to a Vec.
pub fn to_vec(val: &Value) -> Result<Vec<Value>, SchemeError> {
    let mut result = Vec::new();
    let mut cur = val.clone();
    loop {
        match cur {
            Value::Nil => return Ok(result),
            Value::Pair(car, cdr) => {
                result.push((*car).clone());
                cur = (*cdr).clone();
            }
            _ => {
                return Err(SchemeError::BadSyntax {
                    form: "improper list".into(),
                });
            }
        }
    }
}

/// Build a Scheme list from a Vec.
pub fn from_vec(vals: Vec<Value>) -> Value {
    vals.into_iter()
        .rev()
        .fold(Value::Nil, |acc, v| Value::Pair(Rc::new(v), Rc::new(acc)))
}
