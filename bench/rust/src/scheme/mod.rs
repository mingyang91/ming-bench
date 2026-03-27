pub mod error;
pub(crate) mod env;
mod parser;
mod eval;
mod value;

pub use error::EvalError;
use env::Env;
use parser::Parser;
use eval::{eval, Output};
use value::Value;

use std::cell::RefCell;
use std::rc::Rc;

const BUILTINS: &[&str] = &[
    "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
    "cons", "car", "cdr", "list", "length", "null?", "append",
    "number?", "boolean?", "string?", "symbol?", "pair?", "char?",
    "display", "write", "newline",
    "string-append", "string-length", "substring",
    "string->number", "number->string",
    "symbol->string", "string->symbol", "string-ref", "string-copy",
    "apply",
    // L09
    "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
    "zero?", "positive?", "negative?", "odd?", "even?",
    "list-ref", "list-tail", "list?", "assoc", "eq?", "equal?", "map",
    "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
    "char=?", "char<?",
    "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
    // L11
    "exact?", "inexact?", "exact->inexact", "inexact->exact",
    "numerator", "denominator", "integer?", "rational?",
    // L13
    "procedure?",
    // L14
    "eqv?", "vector", "make-vector", "vector-ref", "vector-length",
    "vector?", "vector->list", "list->vector",
    "for-each",
    // L15
    "string->list", "list->string", "char->integer", "integer->char",
];

fn make_env() -> Rc<RefCell<Env>> {
    let env = Env::new();
    for name in BUILTINS {
        env.borrow_mut().set(name.to_string(), Value::Symbol(name.to_string()));
    }
    env
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = Parser::new(input).parse_all()?;
    let env = make_env();
    let out: Output = Rc::new(RefCell::new(String::new()));
    let mut result = Value::Boolean(false);
    for (expr, line, col) in exprs {
        result = eval(&expr, &env, &out).map_err(|e| EvalError::WithPosition {
            error: Box::new(e),
            line,
            col,
        })?;
    }
    Ok(result.to_display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = Parser::new(input).parse_all()?;
    let env = make_env();
    let out: Output = Rc::new(RefCell::new(String::new()));
    let mut result = Value::Boolean(false);
    for (expr, line, col) in exprs {
        result = eval(&expr, &env, &out).map_err(|e| EvalError::WithPosition {
            error: Box::new(e),
            line,
            col,
        })?;
    }
    let output = out.borrow().clone();
    Ok((result.to_display(), output))
}

#[cfg(test)]
mod tests;
