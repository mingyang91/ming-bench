pub mod error;
pub(crate) mod env;
mod parser;
mod eval;
mod value;

pub use error::EvalError;
use env::Env;
use parser::Parser;
use eval::{eval_sequence, eval_sequence_with_limit, Output};
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
    // L17
    "set-car!", "set-cdr!",
    "caar", "cadr", "cdar", "cddr",
    "caaar", "caadr", "caddr", "cdddr", "caddar",
    "reverse", "assq", "assv", "memq", "memv", "member",
    "gcd", "lcm", "truncate", "round", "floor", "ceiling",
    "make-string", "string",
    "string>?", "string<=?", "string>=?",
    "vector-set!", "string-set!",
    // L18
    "call/cc", "call-with-current-continuation",
    // L21
    "values", "call-with-values",
    // L22
    "syntax->datum", "datum->syntax",
    // L26
    "error",
    // cxr handled dynamically in apply_builtin
];

fn make_env() -> Rc<RefCell<Env>> {
    let env = Env::new();
    for name in BUILTINS {
        env.borrow_mut().set(name.to_string(), Value::Builtin(name.to_string()));
    }
    env
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = Parser::new(input).parse_all()?;
    let env = make_env();
    let out: Output = Rc::new(RefCell::new(String::new()));
    if exprs.is_empty() {
        return Ok(Value::Boolean(false).to_display());
    }
    // Build position map: position of each top-level expression
    let positions: Vec<(usize, usize)> = exprs.iter().map(|(_, l, c)| (*l, *c)).collect();
    let all_exprs: Vec<Value> = exprs.into_iter().map(|(e, _, _)| e).collect();
    // Use eval_sequence to keep all expressions in a single CEK pass
    // (so call/cc continuations span across top-level expressions).
    let result = eval_sequence(&all_exprs, &env, &out).map_err(|e| {
        // Attach position from the last expression as a fallback
        let (line, col) = positions.last().copied().unwrap_or((1, 1));
        EvalError::WithPosition { error: Box::new(e), line, col }
    })?;
    Ok(result.to_display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = Parser::new(input).parse_all()?;
    let env = make_env();
    let out: Output = Rc::new(RefCell::new(String::new()));
    if exprs.is_empty() {
        return Ok((Value::Boolean(false).to_display(), String::new()));
    }
    let positions: Vec<(usize, usize)> = exprs.iter().map(|(_, l, c)| (*l, *c)).collect();
    let all_exprs: Vec<Value> = exprs.into_iter().map(|(e, _, _)| e).collect();
    let result = eval_sequence(&all_exprs, &env, &out).map_err(|e| {
        let (line, col) = positions.last().copied().unwrap_or((1, 1));
        EvalError::WithPosition { error: Box::new(e), line, col }
    })?;
    let output = out.borrow().clone();
    Ok((result.to_display_repr(), output))
}

/// Evaluate Scheme expressions with a step budget.
/// Each eval dispatch counts as one step. Exceeding the budget returns an error.
pub fn eval_str_with_limit(input: &str, max_steps: usize) -> Result<String, EvalError> {
    let exprs = Parser::new(input).parse_all()?;
    let env = make_env();
    let out: Output = Rc::new(RefCell::new(String::new()));
    if exprs.is_empty() {
        return Ok(Value::Boolean(false).to_display());
    }
    let positions: Vec<(usize, usize)> = exprs.iter().map(|(_, l, c)| (*l, *c)).collect();
    let all_exprs: Vec<Value> = exprs.into_iter().map(|(e, _, _)| e).collect();
    let result = eval_sequence_with_limit(&all_exprs, &env, &out, max_steps).map_err(|e| {
        let (line, col) = positions.last().copied().unwrap_or((1, 1));
        EvalError::WithPosition { error: Box::new(e), line, col }
    })?;
    Ok(result.to_display())
}

#[cfg(test)]
mod tests;
