mod builtins;
pub mod error;
mod forms;
pub mod parser;
pub mod value;

pub use error::EvalError;
use builtins::{apply_builtin, is_builtin};
use forms::{
    eval_and, eval_begin, eval_body, eval_cond, eval_define, eval_if, eval_lambda, eval_let,
    eval_or, eval_quote, eval_string_set,
};
use std::collections::HashMap;
use value::{Span, Value};

type Env = HashMap<String, Value>;

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

pub(crate) fn eval(value: &Value, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    match value {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Char(_)
        | Value::Lambda { .. } => Ok(value.clone()),
        Value::Symbol(name, span) => {
            env.get(name).cloned().ok_or_else(|| EvalError::UnboundVariable {
                name: name.clone(),
                span: *span,
            })
        }
        Value::List(items, span) => eval_list(items, *span, env, output),
    }
}

fn eval_list(
    items: &[Value],
    span: Span,
    env: &mut Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [operator, args @ ..] = items else {
        return Err(EvalError::Parse {
            message: "empty application".to_string(),
            span,
        });
    };

    // Handle special forms first (unevaluated operator)
    if let Value::Symbol(name, _) = operator {
        match name.as_str() {
            "define" => return eval_define(args, env, span, output),
            "if" => return eval_if(args, env, span, output),
            "quote" => return eval_quote(args, span),
            "and" => return eval_and(args, env, output),
            "or" => return eval_or(args, env, output),
            "lambda" => return eval_lambda(args, env, span),
            "let" => return eval_let(args, env, span, output),
            "begin" => return eval_begin(args, env, span, output),
            "cond" => return eval_cond(args, env, span, output),
            "string-set!" => return eval_string_set(args, env, span, output),
            _ => {}
        }
    }

    // Try builtin functions for known symbol names not in env
    if let Value::Symbol(name, _) = operator {
        if is_builtin(name) {
            return apply_builtin(name, args, env, span, output);
        }
    }

    // Evaluate operator and apply
    let proc = eval(operator, env, output)?;
    let evaluated_args: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env, output))
        .collect::<Result<_, _>>()?;

    apply(proc, &evaluated_args, env, span, output)
}

pub(crate) fn apply(
    proc: Value,
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
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
            // Caller's env as base (provides global defs for recursion),
            // captured env overlays (lexical scoping), params on top.
            let mut local_env = env.clone();
            local_env.extend(captured_env.clone());
            for (param, arg) in params.iter().zip(args) {
                local_env.insert(param.clone(), arg.clone());
            }
            eval_body(body, Value::Boolean(false), &mut local_env, output)
        }
        other => Err(EvalError::TypeError {
            expected: "procedure".to_string(),
            got: format!("{other}"),
            span,
        }),
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
