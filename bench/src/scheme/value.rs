use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

use crate::scheme::builtin::Builtin;
use crate::scheme::continuation::Continuation;
use crate::scheme::environment::Environment;
use crate::scheme::parser::Expr;
use crate::scheme::procedure::{Parameters, Procedure};

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    EmptyList,
    Pair(Box<Pair>),
    Builtin(Builtin),
    Procedure(Rc<Procedure>),
    Continuation(Rc<Continuation>),
    Void,
}

impl Value {
    pub(crate) fn from_literal(expression: &Expr) -> Option<Self> {
        match expression {
            Expr::Integer(value) => Some(Self::Integer(*value)),
            Expr::Boolean(value) => Some(Self::Boolean(*value)),
            Expr::String(value) => Some(Self::String(value.clone())),
            Expr::Symbol(_) | Expr::ScopedSymbol { .. } | Expr::List(_) => None,
        }
    }

    pub(crate) fn from_quoted_expr(expression: &Expr) -> Self {
        match expression {
            Expr::Integer(value) => Self::Integer(*value),
            Expr::Boolean(value) => Self::Boolean(*value),
            Expr::String(value) => Self::String(value.clone()),
            Expr::Symbol(value) | Expr::ScopedSymbol { name: value, .. } => {
                Self::Symbol(value.clone())
            }
            Expr::List(values) => Self::list(values.iter().map(Self::from_quoted_expr).collect()),
        }
    }

    pub(crate) fn procedure(
        parameters: Parameters,
        body: Vec<Expr>,
        environment: Environment,
    ) -> Self {
        Self::Procedure(Rc::new(Procedure::new(parameters, body, environment)))
    }

    pub(crate) fn continuation(continuation: Continuation) -> Self {
        Self::Continuation(Rc::new(continuation))
    }

    pub(crate) fn pair(car: Value, cdr: Value) -> Self {
        Self::Pair(Box::new(Pair::new(car, cdr)))
    }

    pub(crate) fn list(values: Vec<Value>) -> Self {
        values
            .into_iter()
            .rev()
            .fold(Self::EmptyList, |cdr, car| Self::pair(car, cdr))
    }

    pub(crate) fn is_null(&self) -> bool {
        matches!(self, Self::EmptyList)
    }

    pub(crate) fn is_string(&self) -> bool {
        matches!(self, Self::String(_))
    }

    pub(crate) fn is_number(&self) -> bool {
        matches!(self, Self::Integer(_))
    }

    pub(crate) fn is_boolean(&self) -> bool {
        matches!(self, Self::Boolean(_))
    }

    pub(crate) fn is_pair(&self) -> bool {
        matches!(self, Self::Pair(_))
    }

    pub(crate) fn is_symbol(&self) -> bool {
        matches!(self, Self::Symbol(_))
    }

    pub(crate) fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    pub(crate) fn list_length(&self) -> Result<usize, Self> {
        let mut length = 0;
        let mut rest = self;

        while let Self::Pair(pair) = rest {
            length += 1;
            rest = pair.cdr();
        }

        if matches!(rest, Self::EmptyList) {
            Ok(length)
        } else {
            Err(rest.clone())
        }
    }

    pub(crate) fn into_list_elements(self) -> Result<Vec<Self>, Self> {
        let mut elements = Vec::new();
        let mut rest = self;

        while let Self::Pair(pair) = rest {
            let Pair { car, cdr } = *pair;
            elements.push(car);
            rest = cdr;
        }

        if matches!(rest, Self::EmptyList) {
            Ok(elements)
        } else {
            Err(rest)
        }
    }

    pub(crate) fn kind(&self) -> &'static str {
        match self {
            Self::Integer(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::EmptyList => "empty list",
            Self::Pair(_) => "pair",
            Self::Builtin(_) => "procedure",
            Self::Procedure(_) => "procedure",
            Self::Continuation(_) => "procedure",
            Self::Void => "void",
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Pair {
    car: Value,
    cdr: Value,
}

impl Pair {
    fn new(car: Value, cdr: Value) -> Self {
        Self { car, cdr }
    }

    pub(crate) fn car(&self) -> &Value {
        &self.car
    }

    pub(crate) fn cdr(&self) -> &Value {
        &self.cdr
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(value) => write!(f, "{value}"),
            Self::Boolean(value) => f.write_str(if *value { "#t" } else { "#f" }),
            Self::String(value) => write_string(value, f),
            Self::Symbol(value) => f.write_str(value),
            Self::EmptyList => f.write_str("()"),
            Self::Pair(pair) => write_pair(pair, f),
            Self::Builtin(_) => f.write_str("#<procedure>"),
            Self::Procedure(_) => f.write_str("#<procedure>"),
            Self::Continuation(_) => f.write_str("#<procedure>"),
            Self::Void => f.write_str("#<void>"),
        }
    }
}

fn write_pair(pair: &Pair, f: &mut Formatter<'_>) -> fmt::Result {
    f.write_str("(")?;
    write!(f, "{}", pair.car())?;

    let mut rest = pair.cdr();
    loop {
        match rest {
            Value::EmptyList => return f.write_str(")"),
            Value::Pair(next_pair) => {
                write!(f, " {}", next_pair.car())?;
                rest = next_pair.cdr();
            }
            value => {
                write!(f, " . {value})")?;
                return Ok(());
            }
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
