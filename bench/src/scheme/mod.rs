mod apply;
mod builtins;
pub mod error;
mod forms;
mod macros;
pub mod parser;
pub mod value;

pub use error::EvalError;
use apply::apply_step;
pub(crate) use apply::{apply, eval_to_integer, is_truthy};
use builtins::{apply_builtin, is_builtin};
use macros::{eval_define_syntax, expand_macro};
use forms::{
    eval_and_step, eval_begin_step, eval_cond_step, eval_define, eval_if_step,
    eval_lambda, eval_let_step, eval_or_step, eval_quote, eval_set, eval_string_set,
    eval_callcc,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use value::{Span, Value};

type Env = HashMap<String, Rc<RefCell<Value>>>;

/// Trampoline result for tail-call optimization.
pub(crate) enum Bounce {
    Done(Value),
    /// Re-evaluate expression in the current env (no env change).
    Continue(Value),
    /// Re-evaluate expression in a new owned env (from apply/let).
    ReplaceEnv { expr: Value, env: Env },
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse {
            message: "empty input".to_string(),
            span: Span { line: 1, col: 1 },
        });
    }
    let mut env = Env::new();
    let mut output = String::new();
    let last = eval_exprs(&exprs, &mut env, &mut output)?;
    Ok(last.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse {
            message: "empty input".to_string(),
            span: Span { line: 1, col: 1 },
        });
    }
    let mut env = Env::new();
    let mut output = String::new();
    let last = eval_exprs(&exprs, &mut env, &mut output)?;
    Ok((last.to_string(), output))
}

/// Run the expression sequence with continuation restart loop.
fn eval_exprs(exprs: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(false);
    let mut start_index = 0;
    loop {
        match eval_expr_sequence(exprs, start_index, env, output, &mut last)? {
            Some(idx) => start_index = idx,
            None => return Ok(last),
        }
    }
}

/// Evaluate expressions from `start_index`, returning `Some(restart_index)` on
/// continuation invocation or `None` when all expressions complete.
fn eval_expr_sequence(
    exprs: &[Value],
    start_index: usize,
    env: &mut Env,
    output: &mut String,
    last: &mut Value,
) -> Result<Option<usize>, EvalError> {
    for (i, expr) in exprs.iter().enumerate().skip(start_index) {
        env.insert(
            "\x00ei".to_string(),
            Rc::new(RefCell::new(Value::Integer(i as i64))),
        );
        match eval(expr, env, output) {
            Ok(v) => *last = v,
            Err(EvalError::ContinuationInvoked {
                id,
                value,
                expr_index,
            }) => {
                store_continuation_override(env, id, *value);
                return Ok(Some(expr_index));
            }
            Err(e) => return Err(e),
        }
    }
    Ok(None)
}

fn store_continuation_override(env: &mut Env, id: usize, value: Value) {
    env.insert(
        "\x00co".to_string(),
        Rc::new(RefCell::new(Value::List(
            vec![Value::Integer(id as i64), value],
            Span::default(),
        ))),
    );
}

/// Trampoline-based eval: loops on tail calls instead of recursing.
pub(crate) fn eval(value: &Value, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let mut current = value.clone();
    let mut owned_env: Option<Env> = None;

    loop {
        let active_env = owned_env.as_mut().unwrap_or(env);
        match eval_step(&current, active_env, output)? {
            Bounce::Done(v) => return Ok(v),
            Bounce::Continue(expr) => current = expr,
            Bounce::ReplaceEnv {
                expr,
                env: new_env,
            } => {
                current = expr;
                owned_env = Some(new_env);
            }
        }
    }
}

/// One step of the trampoline: evaluate without recursing for tail positions.
fn eval_step(value: &Value, env: &mut Env, output: &mut String) -> Result<Bounce, EvalError> {
    match value {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Char(_)
        | Value::Lambda { .. } | Value::Continuation { .. } | Value::Macro { .. } => {
            Ok(Bounce::Done(value.clone()))
        }
        Value::Symbol(name, span) => env
            .get(name)
            .map(|rc| Bounce::Done(rc.borrow().clone()))
            .or_else(|| {
                if is_builtin(name) || name == "apply" || name == "call/cc" || name == "call-with-current-continuation" {
                    Some(Bounce::Done(Value::Symbol(name.clone(), *span)))
                } else {
                    None
                }
            })
            .ok_or_else(|| EvalError::UnboundVariable {
                name: name.clone(),
                span: *span,
            }),
        Value::List(items, span) => eval_list_step(items, *span, env, output),
    }
}

fn eval_list_step(
    items: &[Value],
    span: Span,
    env: &mut Env,
    output: &mut String,
) -> Result<Bounce, EvalError> {
    let [operator, args @ ..] = items else {
        return Err(EvalError::Parse {
            message: "empty application".to_string(),
            span,
        });
    };

    if let Value::Symbol(name, _) = operator {
        match name.as_str() {
            "define" => return eval_define(args, env, span, output).map(Bounce::Done),
            "if" => return eval_if_step(args, env, span, output),
            "quote" => return eval_quote(args, span).map(Bounce::Done),
            "and" => return eval_and_step(args, env, output),
            "or" => return eval_or_step(args, env, output),
            "lambda" => return eval_lambda(args, env, span).map(Bounce::Done),
            "let" => return eval_let_step(args, env, span, output),
            "begin" => return eval_begin_step(args, env, span, output),
            "cond" => return eval_cond_step(args, env, span, output),
            "set!" => return eval_set(args, env, span, output).map(Bounce::Done),
            "string-set!" => return eval_string_set(args, env, span, output).map(Bounce::Done),
            "call/cc" | "call-with-current-continuation" => {
                return eval_callcc(args, env, span, output);
            }
            "define-syntax" => {
                return eval_define_syntax(args, env, span).map(Bounce::Done);
            }
            _ => {}
        }
    }

    // Macro expansion: check if operator resolves to a macro
    if let Some(expanded) = try_expand_macro(operator, args, env, span)? {
        return Ok(Bounce::Continue(expanded));
    }

    if let Value::Symbol(name, _) = operator {
        if is_builtin(name) {
            return apply_builtin(name, args, env, span, output).map(Bounce::Done);
        }
    }

    let proc = eval(operator, env, output)?;
    let evaluated_args: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env, output))
        .collect::<Result<_, _>>()?;

    apply_step(proc, &evaluated_args, env, span, output)
}

fn try_expand_macro(
    operator: &Value,
    args: &[Value],
    env: &mut Env,
    span: Span,
) -> Result<Option<Value>, EvalError> {
    let Value::Symbol(name, _) = operator else {
        return Ok(None);
    };
    let Some(cell) = env.get(name).cloned() else {
        return Ok(None);
    };
    let val = cell.borrow().clone();
    if !matches!(&val, Value::Macro { .. }) {
        return Ok(None);
    }
    expand_macro(&val, args, env, span).map(Some)
}

#[cfg(test)]
mod tests;
