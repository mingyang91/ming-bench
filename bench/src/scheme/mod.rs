mod builtins;
pub mod error;
pub mod parser;
pub mod value;

pub use error::EvalError;
use builtins::{apply_builtin, is_builtin};
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

fn eval_define(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    match args {
        // (define x expr)
        [Value::Symbol(name), expr] => {
            let val = eval(expr, env)?;
            env.insert(name.clone(), val);
            Ok(Value::Symbol(name.clone()))
        }
        // (define (f params...) body...)
        [Value::List(signature), body @ ..] if !signature.is_empty() && !body.is_empty() => {
            let [Value::Symbol(name), param_vals @ ..] = signature.as_slice() else {
                return Err(EvalError::Parse {
                    message: "define: first element of signature must be a symbol".to_string(),
                });
            };
            let params: Vec<String> = param_vals
                .iter()
                .map(|v| match v {
                    Value::Symbol(s) => Ok(s.clone()),
                    other => Err(EvalError::TypeError {
                        expected: "symbol".to_string(),
                        got: format!("{other}"),
                    }),
                })
                .collect::<Result<_, _>>()?;
            let lambda = Value::Lambda {
                params,
                body: body.to_vec(),
                env: env.clone(),
            };
            env.insert(name.clone(), lambda);
            Ok(Value::Symbol(name.clone()))
        }
        _ => Err(EvalError::Parse {
            message: "define requires a symbol and an expression".to_string(),
        }),
    }
}

fn eval_if(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [condition, consequent, alternative] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 3,
            got: args.len(),
        });
    };
    let cond_val = eval(condition, env)?;
    if is_truthy(&cond_val) {
        eval(consequent, env)
    } else {
        eval(alternative, env)
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    Ok(expr.clone())
}

fn eval_lambda(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    let [Value::List(param_list), body @ ..] = args else {
        return Err(EvalError::Parse {
            message: "lambda requires a parameter list and body".to_string(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::Parse {
            message: "lambda requires a body".to_string(),
        });
    }
    let params: Vec<String> = param_list
        .iter()
        .map(|v| match v {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(EvalError::TypeError {
                expected: "symbol".to_string(),
                got: format!("{other}"),
            }),
        })
        .collect::<Result<_, _>>()?;
    Ok(Value::Lambda {
        params,
        body: body.to_vec(),
        env: env.clone(),
    })
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
            let mut result = Value::Boolean(false);
            for expr in &body {
                result = eval(expr, &mut local_env)?;
            }
            Ok(result)
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

fn eval_and(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

#[cfg(test)]
mod tests;
