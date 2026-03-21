mod builtins;
pub mod error;
mod forms;
pub mod parser;
pub mod value;

pub use error::EvalError;
use builtins::{apply_builtin, is_builtin};
use forms::{
    eval_and, eval_begin_step, eval_body_step, eval_cond_step, eval_define, eval_if_step,
    eval_lambda, eval_let_step, eval_or, eval_quote, eval_string_set,
};
use std::collections::HashMap;
use value::{Span, Value};

type Env = HashMap<String, Value>;

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
    let mut last = Value::Boolean(false);
    for expr in &exprs {
        last = eval(expr, &mut env, &mut output)?;
    }
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
    let mut last = Value::Boolean(false);
    for expr in &exprs {
        last = eval(expr, &mut env, &mut output)?;
    }
    Ok((last.to_string(), output))
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
        | Value::Lambda { .. } => Ok(Bounce::Done(value.clone())),
        Value::Symbol(name, span) => env
            .get(name)
            .cloned()
            .map(Bounce::Done)
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
            "and" => return eval_and(args, env, output).map(Bounce::Done),
            "or" => return eval_or(args, env, output).map(Bounce::Done),
            "lambda" => return eval_lambda(args, env, span).map(Bounce::Done),
            "let" => return eval_let_step(args, env, span, output),
            "begin" => return eval_begin_step(args, env, span, output),
            "cond" => return eval_cond_step(args, env, span, output),
            "string-set!" => return eval_string_set(args, env, span, output).map(Bounce::Done),
            _ => {}
        }
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

/// Tail-call-aware apply: returns Bounce instead of recursing into eval_body.
fn apply_step(
    proc: Value,
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Bounce, EvalError> {
    match &proc {
        Value::Lambda {
            params,
            body,
            env: captured_env,
        } => {
            if args.len() != params.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len(),
                    got: args.len(),
                    span,
                });
            }
            let mut local_env = env.clone();
            local_env.extend(captured_env.clone());
            for (param, arg) in params.iter().zip(args) {
                local_env.insert(param.clone(), arg.clone());
            }
            eval_body_step(body, Value::Boolean(false), &mut local_env, output)
                .map(|b| match b {
                    Bounce::Continue(expr) => Bounce::ReplaceEnv {
                        expr,
                        env: local_env,
                    },
                    other => other,
                })
        }
        other => Err(EvalError::TypeError {
            expected: "procedure".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

/// Non-tail apply for use by builtins (map, etc.).
pub(crate) fn apply(
    proc: Value,
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    match apply_step(proc, args, env, span, output)? {
        Bounce::Done(v) => Ok(v),
        Bounce::Continue(expr) => eval(&expr, env, output),
        Bounce::ReplaceEnv {
            expr,
            env: mut new_env,
        } => eval(&expr, &mut new_env, output),
    }
}

pub(crate) fn eval_to_integer(
    value: &Value,
    env: &mut Env,
    output: &mut String,
) -> Result<i64, EvalError> {
    let result = eval(value, env, output)?;
    match &result {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::TypeError {
            expected: "integer".to_string(),
            got: format!("{other}"),
            span: span_of(value),
        }),
    }
}

/// Extract the span from a Value if it carries one, otherwise return default.
fn span_of(value: &Value) -> Span {
    match value {
        Value::Symbol(_, span) | Value::List(_, span) => *span,
        _ => Span::default(),
    }
}

pub(crate) fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Boolean(false))
}

#[cfg(test)]
mod tests;
