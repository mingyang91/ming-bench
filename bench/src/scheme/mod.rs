mod builtins;
pub mod error;
mod forms;
pub mod parser;
pub mod value;

pub use error::EvalError;
use builtins::{apply_builtin, is_builtin};
use forms::{eval_and, eval_begin, eval_body, eval_cond, eval_define, eval_if, eval_lambda, eval_let, eval_or, eval_quote};
use std::collections::HashMap;
use value::Value;

type Env = HashMap<String, Value>;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse {
            message: "empty input".to_string(),
        });
    }
    let mut env = Env::new();
    let mut last = Value::Boolean(false);
    for expr in &exprs {
        last = eval(expr, &mut env)?;
    }
    Ok(last.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

pub(crate) fn eval(value: &Value, env: &mut Env) -> Result<Value, EvalError> {
    match value {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Lambda { .. } => {
            Ok(value.clone())
        }
        Value::Symbol(name) => env.get(name).cloned().ok_or_else(|| EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Value::List(items) => eval_list(items, env),
    }
}

fn eval_list(items: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [operator, args @ ..] = items else {
        return Err(EvalError::Parse {
            message: "empty application".to_string(),
        });
    };

    // Handle special forms first (unevaluated operator)
    if let Value::Symbol(name) = operator {
        match name.as_str() {
            "define" => return eval_define(args, env),
            "if" => return eval_if(args, env),
            "quote" => return eval_quote(args),
            "and" => return eval_and(args, env),
            "or" => return eval_or(args, env),
            "lambda" => return eval_lambda(args, env),
            "let" => return eval_let(args, env),
            "begin" => return eval_begin(args, env),
            "cond" => return eval_cond(args, env),
            _ => {}
        }
    }

    // Try builtin functions for known symbol names not in env
    if let Value::Symbol(name) = operator {
        if is_builtin(name) {
            return apply_builtin(name, args, env);
        }
    }

    // Evaluate operator and apply
    let proc = eval(operator, env)?;
    let evaluated_args: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env))
        .collect::<Result<_, _>>()?;

    apply(proc, &evaluated_args, env)
}

fn apply(proc: Value, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    match proc {
        Value::Lambda {
            params,
            body,
            env: captured_env,
        } => {
            if args.len() != params.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len(),
                    got: args.len(),
                });
            }
            // Caller's env as base (provides global defs for recursion),
            // captured env overlays (lexical scoping), params on top.
            let mut local_env = env.clone();
            local_env.extend(captured_env);
            for (param, arg) in params.iter().zip(args) {
                local_env.insert(param.clone(), arg.clone());
            }
            eval_body(&body, Value::Boolean(false), &mut local_env)
        }
        other => Err(EvalError::TypeError {
            expected: "procedure".to_string(),
            got: format!("{other}"),
        }),
    }
}

pub(crate) fn eval_to_integer(value: &Value, env: &mut Env) -> Result<i64, EvalError> {
    match eval(value, env)? {
        Value::Integer(n) => Ok(n),
        other => Err(EvalError::TypeError {
            expected: "integer".to_string(),
            got: format!("{other}"),
        }),
    }
}

pub(crate) fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Boolean(false))
}

#[cfg(test)]
mod tests;
