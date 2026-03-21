use super::{eval, Bounce, Env};
use crate::scheme::builtins::{self, is_builtin};
use crate::scheme::error::EvalError;
use crate::scheme::forms::{eval_body_step, handle_callcc};
use crate::scheme::value::{Span, Value};
use std::cell::RefCell;
use std::rc::Rc;

/// Tail-call-aware apply: returns Bounce instead of recursing into eval_body.
pub(crate) fn apply_step(
    proc: Value,
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Bounce, EvalError> {
    match &proc {
        Value::Lambda {
            params,
            rest_param,
            body,
            env: captured_env,
        } => {
            let arg_count_mismatch = if rest_param.is_some() {
                args.len() < params.len()
            } else {
                args.len() != params.len()
            };
            if arg_count_mismatch {
                return Err(EvalError::WrongArgCount {
                    expected: params.len(),
                    got: args.len(),
                    span,
                });
            }
            let mut local_env = env.clone();
            local_env.extend(captured_env.iter().map(|(k, v)| (k.clone(), Rc::clone(v))));
            for (param, arg) in params.iter().zip(args) {
                local_env.insert(param.clone(), Rc::new(RefCell::new(arg.clone())));
            }
            if let Some(rest_name) = rest_param {
                let rest_args = args[params.len()..].to_vec();
                local_env.insert(
                    rest_name.clone(),
                    Rc::new(RefCell::new(Value::List(rest_args, Span::default()))),
                );
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
        Value::Symbol(name, _)
            if name == "call/cc" || name == "call-with-current-continuation" =>
        {
            let [proc] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            handle_callcc(proc.clone(), env, span, output)
        }
        Value::Symbol(name, _) if is_builtin(name) || name == "apply" => {
            builtins::call_builtin_values(name, args, env, span, output).map(Bounce::Done)
        }
        Value::Continuation { id, expr_index } => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            Err(EvalError::ContinuationInvoked {
                id: *id,
                value: Box::new(arg.clone()),
                expr_index: *expr_index,
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
pub(crate) fn span_of(value: &Value) -> Span {
    match value {
        Value::Symbol(_, span) | Value::List(_, span) => *span,
        _ => Span::default(),
    }
}

pub(crate) fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Boolean(false))
}
