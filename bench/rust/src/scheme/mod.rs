mod builtins;
pub mod error;
mod parser;

pub use error::EvalError;

use builtins::{
    builtin_add, builtin_and, builtin_append, builtin_car, builtin_cdr, builtin_cmp,
    builtin_cons, builtin_div, builtin_length, builtin_list, builtin_mul, builtin_not,
    builtin_null, builtin_or, builtin_sub, builtin_type_pred,
};
use parser::parse_all;
use std::collections::HashMap;

type Env = HashMap<String, Value>;

/// A Scheme value.
#[derive(Debug, Clone)]
pub(crate) enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
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
            }),
        }
    }

    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::Str(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::List(_) => "list",
            Value::Lambda { .. } => "procedure",
        }
    }

    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda { .. } => "#<procedure>".to_string(),
        }
    }
}

// --- Evaluator ---

fn default_env() -> Env {
    Env::new()
}

fn eval(expr: &Value, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Lambda { .. } => Ok(expr.clone()),
        Value::Symbol(name) => env.get(name).cloned().ok_or_else(|| EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Value::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse {
                    message: "empty application".to_string(),
                });
            }

            // Check for special forms
            if let Value::Symbol(op) = &items[0] {
                match op.as_str() {
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity {
                                procedure: "quote".to_string(),
                                expected: "1".to_string(),
                                got: items.len() - 1,
                            });
                        }
                        return Ok(items[1].clone());
                    }
                    "if" => {
                        if items.len() < 3 || items.len() > 4 {
                            return Err(EvalError::Parse {
                                message: "if requires 2 or 3 arguments".to_string(),
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
                            });
                        }
                        match &items[1] {
                            Value::Symbol(name) => {
                                let mut val = eval(&items[2], env)?;
                                // Tag lambdas with their name for self-recursion
                                if let Value::Lambda { name: ref mut n, .. } = val {
                                    *n = Some(name.clone());
                                }
                                env.insert(name.clone(), val);
                                return Ok(Value::Boolean(false));
                            }
                            Value::List(sig) => {
                                // (define (f params...) body...)
                                if sig.is_empty() {
                                    return Err(EvalError::Parse {
                                        message: "define: empty signature".to_string(),
                                    });
                                }
                                let name = match &sig[0] {
                                    Value::Symbol(n) => n.clone(),
                                    other => return Err(EvalError::TypeError {
                                        message: format!("define: expected symbol for name, got {}", other.type_name()),
                                    }),
                                };
                                let params: Vec<String> = sig[1..].iter().map(|p| {
                                    match p {
                                        Value::Symbol(s) => Ok(s.clone()),
                                        other => Err(EvalError::TypeError {
                                            message: format!("define: expected symbol for parameter, got {}", other.type_name()),
                                        }),
                                    }
                                }).collect::<Result<_, _>>()?;
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
                            other => return Err(EvalError::TypeError {
                                message: format!("define: expected symbol or list, got {}", other.type_name()),
                            }),
                        }
                    }
                    "lambda" => {
                        if items.len() < 3 {
                            return Err(EvalError::Parse {
                                message: "lambda requires params and body".to_string(),
                            });
                        }
                        let params = match &items[1] {
                            Value::List(param_list) => {
                                param_list.iter().map(|p| {
                                    match p {
                                        Value::Symbol(s) => Ok(s.clone()),
                                        other => Err(EvalError::TypeError {
                                            message: format!("lambda: expected symbol for parameter, got {}", other.type_name()),
                                        }),
                                    }
                                }).collect::<Result<Vec<_>, _>>()?
                            }
                            other => return Err(EvalError::TypeError {
                                message: format!("lambda: expected parameter list, got {}", other.type_name()),
                            }),
                        };
                        let body: Vec<Value> = items[2..].to_vec();
                        return Ok(Value::Lambda {
                            name: None,
                            params,
                            body,
                            closure_env: env.clone(),
                        });
                    }
                    "+" => return builtin_add(&items[1..], env),
                    "-" => return builtin_sub(&items[1..], env),
                    "*" => return builtin_mul(&items[1..], env),
                    "/" => return builtin_div(&items[1..], env),
                    "<" => return builtin_cmp(&items[1..], env, "<"),
                    ">" => return builtin_cmp(&items[1..], env, ">"),
                    "=" => return builtin_cmp(&items[1..], env, "="),
                    "<=" => return builtin_cmp(&items[1..], env, "<="),
                    ">=" => return builtin_cmp(&items[1..], env, ">="),
                    "not" => return builtin_not(&items[1..], env),
                    "and" => return builtin_and(&items[1..], env),
                    "or" => return builtin_or(&items[1..], env),
                    "cons" => return builtin_cons(&items[1..], env),
                    "car" => return builtin_car(&items[1..], env),
                    "cdr" => return builtin_cdr(&items[1..], env),
                    "null?" => return builtin_null(&items[1..], env),
                    "list" => return builtin_list(&items[1..], env),
                    "length" => return builtin_length(&items[1..], env),
                    "append" => return builtin_append(&items[1..], env),
                    "string?" | "number?" | "boolean?" | "pair?" | "symbol?" => {
                        return builtin_type_pred(&items[1..], env, op.as_str());
                    }
                    "let" => return eval_let(&items[1..], env),
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
            apply_function(&func, &args)
        }
    }
}

fn eval_let(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse {
            message: "let: missing bindings".to_string(),
        });
    }

    // Named let: (let name ((var init) ...) body ...)
    if let Value::Symbol(name) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::Parse {
                message: "named let: missing bindings or body".to_string(),
            });
        }
        let bindings = match &args[1] {
            Value::List(b) => b,
            other => return Err(EvalError::TypeError {
                message: format!("named let: expected bindings list, got {}", other.type_name()),
            }),
        };
        let mut params = Vec::new();
        let mut init_vals = Vec::new();
        for binding in bindings {
            match binding {
                Value::List(pair) if pair.len() == 2 => {
                    match &pair[0] {
                        Value::Symbol(s) => params.push(s.clone()),
                        other => return Err(EvalError::TypeError {
                            message: format!("let: expected symbol, got {}", other.type_name()),
                        }),
                    }
                    init_vals.push(eval(&pair[1], env)?);
                }
                other => return Err(EvalError::Parse {
                    message: format!("let: bad binding: {}", other.display()),
                }),
            }
        }
        let body: Vec<Value> = args[2..].to_vec();
        let lambda = Value::Lambda {
            name: Some(name.clone()),
            params,
            body,
            closure_env: env.clone(),
        };
        let mut local_env = env.clone();
        local_env.insert(name.clone(), lambda.clone());
        // Apply the named lambda with initial values
        match &lambda {
            Value::Lambda { params, body, .. } => {
                for (param, val) in params.iter().zip(init_vals.iter()) {
                    local_env.insert(param.clone(), val.clone());
                }
                let mut result = Value::Boolean(false);
                for expr in body {
                    result = eval(expr, &mut local_env)?;
                }
                Ok(result)
            }
            _ => unreachable!(),
        }
    } else {
        // Regular let: (let ((var init) ...) body ...)
        let bindings = match &args[0] {
            Value::List(b) => b,
            other => return Err(EvalError::TypeError {
                message: format!("let: expected bindings list, got {}", other.type_name()),
            }),
        };
        if args.len() < 2 {
            return Err(EvalError::Parse {
                message: "let: missing body".to_string(),
            });
        }
        let mut local_env = env.clone();
        for binding in bindings {
            match binding {
                Value::List(pair) if pair.len() == 2 => {
                    let name = match &pair[0] {
                        Value::Symbol(s) => s.clone(),
                        other => return Err(EvalError::TypeError {
                            message: format!("let: expected symbol, got {}", other.type_name()),
                        }),
                    };
                    let val = eval(&pair[1], env)?;
                    local_env.insert(name, val);
                }
                other => return Err(EvalError::Parse {
                    message: format!("let: bad binding: {}", other.display()),
                }),
            }
        }
        let mut result = Value::Boolean(false);
        for expr in &args[1..] {
            result = eval(expr, &mut local_env)?;
        }
        Ok(result)
    }
}

fn eval_cond(clauses: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    for clause in clauses {
        match clause {
            Value::List(items) if !items.is_empty() => {
                // Check for else clause
                if let Value::Symbol(s) = &items[0] {
                    if s == "else" {
                        let mut result = Value::Boolean(false);
                        for expr in &items[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&items[0], env)?;
                if test.is_truthy() {
                    if items.len() == 1 {
                        return Ok(test);
                    }
                    let mut result = Value::Boolean(false);
                    for expr in &items[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            other => return Err(EvalError::Parse {
                message: format!("cond: bad clause: {}", other.display()),
            }),
        }
    }
    Ok(Value::Boolean(false))
}

fn apply_function(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { name, params, body, closure_env } => {
            if params.len() != args.len() {
                return Err(EvalError::Arity {
                    procedure: name.as_deref().unwrap_or("lambda").to_string(),
                    expected: params.len().to_string(),
                    got: args.len(),
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
                if let Value::List(items) = expr {
                    if let Some(Value::Symbol(s)) = items.first() {
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
