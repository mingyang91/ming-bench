pub mod error;
mod eval;
mod parse;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct Span {
    line: usize,
    col: usize,
}

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Char(char),
    Pair(Box<Value>, Box<Value>),
    Nil,
    Lambda {
        params: Vec<String>,
        body: Box<Value>,
        closure: Env,
    },
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::String(s) => format!("\"{s}\""),
            Value::Symbol(s) => s.clone(),
            Value::Char(c) => format!("#\\{c}"),
            Value::Nil => "()".to_string(),
            Value::Lambda { .. } => "#<procedure>".to_string(),
            Value::Pair(..) => self.fmt_list(false),
        }
    }

    /// Human-readable display (no quotes around strings).
    fn display_human(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::Pair(..) => self.fmt_list(true),
            _ => self.display(),
        }
    }

    fn display_for(&self, human: bool) -> String {
        if human { self.display_human() } else { self.display() }
    }

    fn fmt_list(&self, human: bool) -> String {
        let mut out = String::from("(");
        self.display_list_inner(&mut out, human);
        out.push(')');
        out
    }

    fn display_list_inner(&self, out: &mut String, human: bool) {
        let Value::Pair(car, cdr) = self else {
            unreachable!("display_list_inner called on non-pair");
        };
        out.push_str(&car.display_for(human));
        match cdr.as_ref() {
            Value::Nil => {}
            Value::Pair(..) => {
                out.push(' ');
                cdr.display_list_inner(out, human);
            }
            other => {
                out.push_str(" . ");
                out.push_str(&other.display_for(human));
            }
        }
    }

    fn to_list_vec(&self) -> Option<Vec<Value>> {
        let mut result = Vec::new();
        let mut current = self;
        while let Value::Pair(car, cdr) = current {
            result.push(car.as_ref().clone());
            current = cdr.as_ref();
        }
        matches!(current, Value::Nil).then_some(result)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            _ => false,
        }
    }
}

// --- Environment ---

type Env = HashMap<String, Rc<RefCell<Value>>>;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (result, _output) = eval_str_with_output(input)?;
    Ok(result)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse::parse_all(input)?;
    let mut env = Env::new();
    let mut output = String::new();
    let mut last = None;
    for (expr, span) in &exprs {
        last = Some(
            eval::eval(expr, &mut env, &mut output).map_err(|e| EvalError::AtPosition {
                line: span.line,
                col: span.col,
                source: Box::new(e),
            })?,
        );
    }
    let last = last.ok_or(EvalError::EmptyInput)?;
    Ok((last.display(), output))
}

#[cfg(test)]
mod tests;
