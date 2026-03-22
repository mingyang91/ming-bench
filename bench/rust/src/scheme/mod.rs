mod builtins;
pub mod error;
mod forms;
mod parser;

pub use error::EvalError;

use forms::{eval_cond, eval_let};
use builtins::{
    builtin_add, builtin_and, builtin_append, builtin_car, builtin_cdr, builtin_cmp,
    builtin_cons, builtin_div, builtin_length, builtin_list, builtin_mul, builtin_not,
    builtin_null, builtin_or, builtin_sub, builtin_type_pred,
};
use parser::{parse_all, Span};
use std::collections::HashMap;

type Env = HashMap<String, Value>;

/// A Scheme value.
#[derive(Debug, Clone)]
pub(crate) enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String, Span),
    List(Vec<Value>, Span),
    Lambda {
        name: Option<String>,
        params: Vec<String>,
        body: Vec<Value>,
        closure_env: Env,
    },
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self, context: &str) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::TypeError {
                message: format!("{context}: expected number, got {}", other.type_name()),
                line: 0,
                col: 0,
            }),
        }
    }

    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::Str(_) => "string",
            Value::Symbol(..) => "symbol",
            Value::List(..) => "list",
            Value::Lambda { .. } => "procedure",
        }
    }

    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s, _) => s.clone(),
            Value::List(items, _) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda { .. } => "#<procedure>".to_string(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Value::Symbol(_, span) | Value::List(_, span) => *span,
            Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Lambda { .. } => (0, 0),
        }
    }
}

// --- Evaluator ---

fn default_env() -> Env {
    Env::new()
}

pub(crate) fn eval(expr: &Value, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Lambda { .. } => {
            Ok(expr.clone())
        }
        Value::Symbol(name, span) => {
            env.get(name).cloned().ok_or_else(|| EvalError::UnboundVariable {
                name: name.clone(),
                line: span.0,
                col: span.1,
            })
        }
        Value::List(items, span) => {
            let (line, col) = *span;
            if items.is_empty() {
                return Err(EvalError::Parse {
                    message: "empty application".to_string(),
                    line,
                    col,
                });
            }

            // Check for special forms
            if let Value::Symbol(op, _) = &items[0] {
                match op.as_str() {
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity {
                                procedure: "quote".to_string(),
                                expected: "1".to_string(),
                                got: items.len() - 1,
                                line,
                                col,
                            });
                        }
                        return Ok(items[1].clone());
                    }
                    "if" => {
                        if items.len() < 3 || items.len() > 4 {
                            return Err(EvalError::Parse {
                                message: "if requires 2 or 3 arguments".to_string(),
                                line,
                                col,
                            });
                        }
                        let cond = eval(&items[1], env)?;
                        if cond.is_truthy() {
                            return eval(&items[2], env);
                        } else if items.len() == 4 {
                            return eval(&items[3], env);
                        }
                        return Ok(Value::Boolean(false));
                    }
                    "define" => {
                        if items.len() < 3 {
                            return Err(EvalError::Parse {
                                message: "define requires at least 2 arguments".to_string(),
                                line,
                                col,
                            });
                        }
                        match &items[1] {
                            Value::Symbol(name, _) => {
                                let mut val = eval(&items[2], env)?;
                                // Tag lambdas with their name for self-recursion
                                if let Value::Lambda {
                                    name: ref mut n, ..
                                } = val
                                {
                                    *n = Some(name.clone());
                                }
                                env.insert(name.clone(), val);
                                return Ok(Value::Boolean(false));
                            }
                            Value::List(sig, _) => {
                                // (define (f params...) body...)
                                if sig.is_empty() {
                                    return Err(EvalError::Parse {
                                        message: "define: empty signature".to_string(),
                                        line,
                                        col,
                                    });
                                }
                                let name = match &sig[0] {
                                    Value::Symbol(n, _) => n.clone(),
                                    other => {
                                        return Err(EvalError::TypeError {
                                            message: format!(
                                                "define: expected symbol for name, got {}",
                                                other.type_name()
                                            ),
                                            line,
                                            col,
                                        })
                                    }
                                };
                                let params: Vec<String> = sig[1..]
                                    .iter()
                                    .map(|p| match p {
                                        Value::Symbol(s, _) => Ok(s.clone()),
                                        other => Err(EvalError::TypeError {
                                            message: format!(
                                                "define: expected symbol for parameter, got {}",
                                                other.type_name()
                                            ),
                                            line,
                                            col,
                                        }),
                                    })
                                    .collect::<Result<_, _>>()?;
                                let body: Vec<Value> = items[2..].to_vec();
                                let closure = Value::Lambda {
                                    name: Some(name.clone()),
                                    params,
                                    body,
                                    closure_env: env.clone(),
                                };
                                env.insert(name, closure);
                                return Ok(Value::Boolean(false));
                            }
                            other => {
                                return Err(EvalError::TypeError {
                                    message: format!(
                                        "define: expected symbol or list, got {}",
                                        other.type_name()
                                    ),
                                    line,
                                    col,
                                })
                            }
                        }
                    }
                    "lambda" => {
                        if items.len() < 3 {
                            return Err(EvalError::Parse {
                                message: "lambda requires params and body".to_string(),
                                line,
                                col,
                            });
                        }
                        let params = match &items[1] {
                            Value::List(param_list, _) => param_list
                                .iter()
                                .map(|p| match p {
                                    Value::Symbol(s, _) => Ok(s.clone()),
                                    other => Err(EvalError::TypeError {
                                        message: format!(
                                            "lambda: expected symbol for parameter, got {}",
                                            other.type_name()
                                        ),
                                        line,
                                        col,
                                    }),
                                })
                                .collect::<Result<Vec<_>, _>>()?,
                            other => {
                                return Err(EvalError::TypeError {
                                    message: format!(
                                        "lambda: expected parameter list, got {}",
                                        other.type_name()
                                    ),
                                    line,
                                    col,
                                })
                            }
                        };
                        let body: Vec<Value> = items[2..].to_vec();
                        return Ok(Value::Lambda {
                            name: None,
                            params,
                            body,
                            closure_env: env.clone(),
                        });
                    }
                    "+" => return builtin_add(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "-" => return builtin_sub(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "*" => return builtin_mul(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "/" => return builtin_div(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "<" => return builtin_cmp(&items[1..], env, "<").map_err(|e| e.with_position(line, col)),
                    ">" => return builtin_cmp(&items[1..], env, ">").map_err(|e| e.with_position(line, col)),
                    "=" => return builtin_cmp(&items[1..], env, "=").map_err(|e| e.with_position(line, col)),
                    "<=" => return builtin_cmp(&items[1..], env, "<=").map_err(|e| e.with_position(line, col)),
                    ">=" => return builtin_cmp(&items[1..], env, ">=").map_err(|e| e.with_position(line, col)),
                    "not" => return builtin_not(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "and" => return builtin_and(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "or" => return builtin_or(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "cons" => return builtin_cons(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "car" => return builtin_car(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "cdr" => return builtin_cdr(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "null?" => return builtin_null(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "list" => return builtin_list(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "length" => return builtin_length(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "append" => return builtin_append(&items[1..], env).map_err(|e| e.with_position(line, col)),
                    "string?" | "number?" | "boolean?" | "pair?" | "symbol?" => {
                        return builtin_type_pred(&items[1..], env, op.as_str())
                            .map_err(|e| e.with_position(line, col));
                    }
                    "let" => return eval_let(&items[1..], env, (line, col)),
                    "begin" => {
                        let mut result = Value::Boolean(false);
                        for expr in &items[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                    "cond" => return eval_cond(&items[1..], env),
                    _ => {}
                }
            }

            // Function application
            let func = eval(&items[0], env)?;
            let args = eval_args(&items[1..], env)?;
            apply_function(&func, &args).map_err(|e| e.with_position(line, col))
        }
    }
}

fn apply_function(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Lambda {
            name,
            params,
            body,
            closure_env,
        } => {
            if params.len() != args.len() {
                return Err(EvalError::Arity {
                    procedure: name.as_deref().unwrap_or("lambda").to_string(),
                    expected: params.len().to_string(),
                    got: args.len(),
                    line: 0,
                    col: 0,
                });
            }
            let mut local_env = closure_env.clone();
            // Inject self-reference for recursion
            if let Some(fn_name) = name {
                local_env.insert(fn_name.clone(), func.clone());
            }
            for (param, arg) in params.iter().zip(args.iter()) {
                local_env.insert(param.clone(), arg.clone());
            }
            // Scan for internal defines and pre-bind them so they're mutually visible
            let mut internal_defs = Vec::new();
            let mut body_start = 0;
            for (i, expr) in body.iter().enumerate() {
                if let Value::List(items, _) = expr {
                    if let Some(Value::Symbol(s, _)) = items.first() {
                        if s == "define" {
                            internal_defs.push(i);
                            body_start = i + 1;
                            continue;
                        }
                    }
                }
                break;
            }

            if !internal_defs.is_empty() {
                // First pass: evaluate all internal defines
                for &idx in &internal_defs {
                    eval(&body[idx], &mut local_env)?;
                }
                // Patch closures of all defined lambdas to see each other
                let snapshot = local_env.clone();
                for val in local_env.values_mut() {
                    if let Value::Lambda { closure_env, .. } = val {
                        for (k, v) in &snapshot {
                            closure_env.entry(k.clone()).or_insert_with(|| v.clone());
                        }
                    }
                }
            }

            let mut result = Value::Boolean(false);
            for expr in &body[body_start..] {
                result = eval(expr, &mut local_env)?;
            }
            Ok(result)
        }
        other => Err(EvalError::TypeError {
            message: format!("not a procedure: {}", other.display()),
            line: 0,
            col: 0,
        }),
    }
}

fn eval_args(args: &[Value], env: &mut Env) -> Result<Vec<Value>, EvalError> {
    args.iter().map(|a| eval(a, env)).collect()
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse {
            message: "no expressions".to_string(),
            line: 0,
            col: 0,
        });
    }
    let mut env = default_env();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &mut env)?;
    }
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
