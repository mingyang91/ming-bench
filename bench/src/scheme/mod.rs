pub mod error;
mod eval;
mod parse;

pub use error::EvalError;

use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Span {
    line: usize,
    col: usize,
}

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
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
            Value::Nil => "()".to_string(),
            Value::Lambda { .. } => "#<procedure>".to_string(),
            Value::Pair(..) => {
                let mut out = String::from("(");
                self.display_list_inner(&mut out);
                out.push(')');
                out
            }
        }
    }

    fn display_list_inner(&self, out: &mut String) {
        let Value::Pair(car, cdr) = self else {
            unreachable!("display_list_inner called on non-pair");
        };
        out.push_str(&car.display());
        match cdr.as_ref() {
            Value::Nil => {}
            Value::Pair(..) => {
                out.push(' ');
                cdr.display_list_inner(out);
            }
            other => {
                out.push_str(" . ");
                out.push_str(&other.display());
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

// --- Environment ---

type Env = HashMap<String, Value>;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse::parse_all(input)?;
    let mut env = Env::new();
    let mut last = None;
    for (expr, span) in &exprs {
        last = Some(eval::eval(expr, &mut env).map_err(|e| EvalError::AtPosition {
            line: span.line,
            col: span.col,
            source: Box::new(e),
        })?);
    }
    let last = last.ok_or(EvalError::EmptyInput)?;
    Ok(last.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
