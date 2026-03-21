use super::{atom_to_value, builtins, quote_expr, Env, EvalError, Expr, Value};

/// Evaluate an expression in the given environment.
pub fn eval(expr: &Expr, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Atom(token) => {
            // Try literal first, then variable lookup
            if token.starts_with('"')
                || token.parse::<i64>().is_ok()
                || token == "#t"
                || token == "#f"
            {
                atom_to_value(token)
            } else if let Some(val) = env.get(token) {
                Ok(val.clone())
            } else {
                Err(EvalError::UnboundVariable {
                    name: token.clone(),
                })
            }
        }
        Expr::List(items) => eval_list(items, env),
    }
}

/// Evaluate a list expression (function application or special form).
fn eval_list(items: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let [operator, args @ ..] = items else {
        return Err(EvalError::Parse {
            message: "empty list".to_string(),
        });
    };
    // Check for special forms (operator must be an atom)
    if let Expr::Atom(op) = operator {
        match op.as_str() {
            "define" => return eval_define(args, env),
            "if" => return eval_if(args, env),
            "quote" => return eval_quote(args),
            "and" => return eval_and(args, env),
            "or" => return eval_or(args, env),
            "lambda" => return eval_lambda(args, env),
            "let" => return eval_let(args, env),
            "begin" => return eval_body(args, env),
            "cond" => return eval_cond(args, env),
            _ => {}
        }
    }
    // General function application
    let evaluated: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env))
        .collect::<Result<_, _>>()?;
    // Try evaluating operator; fall back to builtin for atoms
    match eval(operator, env) {
        Ok(func) => apply_func(&func, &evaluated),
        Err(EvalError::UnboundVariable { ref name }) => apply_builtin(name, &evaluated),
        Err(e) => Err(e),
    }
}

/// Apply a built-in operator.
fn apply_builtin(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "+" => builtins::apply_add(args),
        "-" => builtins::apply_sub(args),
        "*" => builtins::apply_mul(args),
        "/" => builtins::apply_div(args),
        "<" => builtins::apply_compare(args, |a, b| a < b),
        ">" => builtins::apply_compare(args, |a, b| a > b),
        "=" => builtins::apply_compare(args, |a, b| a == b),
        "<=" => builtins::apply_compare(args, |a, b| a <= b),
        ">=" => builtins::apply_compare(args, |a, b| a >= b),
        "not" => builtins::apply_not(args),
        "cons" => builtins::apply_cons(args),
        "car" => builtins::apply_car(args),
        "cdr" => builtins::apply_cdr(args),
        "null?" => builtins::apply_null(args),
        "list" => builtins::apply_list(args),
        "length" => builtins::apply_length(args),
        _ => Err(EvalError::UnboundVariable {
            name: op.to_string(),
        }),
    }
}

/// Apply a function value (lambda or builtin lookup).
fn apply_func(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { .. } => apply_lambda(func, args),
        Value::Symbol(name) => apply_builtin(name, args),
        other => Err(EvalError::TypeError {
            expected: "procedure".to_string(),
            got: format!("{other}"),
        }),
    }
}

/// Evaluate a sequence of body expressions, returning the last result.
fn eval_body(body: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Nil;
    for expr in body {
        result = eval(expr, env)?;
    }
    Ok(result)
}

/// Apply a lambda closure to arguments.
fn apply_lambda(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    let Value::Lambda { name, params, body, closure_env } = func else {
        unreachable!("apply_lambda called with non-lambda");
    };
    if params.len() != args.len() {
        return Err(EvalError::WrongArgCount {
            expected: params.len(),
            got: args.len(),
        });
    }
    let mut local_env = closure_env.clone();
    // Inject self-reference for recursion
    if let Some(n) = name {
        local_env.insert(n.clone(), func.clone());
    }
    for (param, arg) in params.iter().zip(args) {
        local_env.insert(param.clone(), arg.clone());
    }
    eval_body(body, &mut local_env)
}

/// Evaluate `(lambda (params...) body...)`.
fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [params_expr, body @ ..] = args else {
        return Err(EvalError::Parse {
            message: "lambda requires params and body".to_string(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::Parse {
            message: "lambda requires params and body".to_string(),
        });
    }
    let Expr::List(param_exprs) = params_expr else {
        return Err(EvalError::Parse {
            message: "lambda params must be a list".to_string(),
        });
    };
    let params: Vec<String> = param_exprs
        .iter()
        .map(|e| match e {
            Expr::Atom(name) => Ok(name.clone()),
            _ => Err(EvalError::Parse {
                message: "lambda param must be a symbol".to_string(),
            }),
        })
        .collect::<Result<_, _>>()?;
    Ok(Value::Lambda {
        name: None,
        params,
        body: body.to_vec(),
        closure_env: env.clone(),
    })
}

/// Evaluate `(define name value)` or `(define (name params...) body...)`.
fn eval_define(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let [target, body @ ..] = args else {
        return Err(EvalError::Parse {
            message: "define requires a name and a value".to_string(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::Parse {
            message: "define requires a name and a value".to_string(),
        });
    }
    match target {
        Expr::Atom(name) => {
            let [value_expr] = body else {
                return Err(EvalError::Parse {
                    message: "define variable form takes exactly one value".to_string(),
                });
            };
            let val = eval(value_expr, env)?;
            env.insert(name.clone(), val.clone());
            Ok(val)
        }
        Expr::List(parts) => {
            let [Expr::Atom(name), param_exprs @ ..] = parts.as_slice() else {
                return Err(EvalError::Parse {
                    message: "define function form requires a name".to_string(),
                });
            };
            let params: Vec<String> = param_exprs
                .iter()
                .map(|e| match e {
                    Expr::Atom(s) => Ok(s.clone()),
                    _ => Err(EvalError::Parse {
                        message: "parameter must be a symbol".to_string(),
                    }),
                })
                .collect::<Result<_, _>>()?;
            let lambda = Value::Lambda {
                name: Some(name.clone()),
                params,
                body: body.to_vec(),
                closure_env: env.clone(),
            };
            env.insert(name.clone(), lambda);
            Ok(Value::Nil)
        }
    }
}

/// Evaluate `(if cond then else)`.
fn eval_if(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let (cond, then_expr, else_expr) = match args {
        [c, t, e] => (c, t, Some(e)),
        [c, t] => (c, t, None),
        _ => {
            return Err(EvalError::Parse {
                message: "if requires 2 or 3 arguments".to_string(),
            })
        }
    };
    let cond_val = eval(cond, env)?;
    if cond_val != Value::Boolean(false) {
        eval(then_expr, env)
    } else if let Some(e) = else_expr {
        eval(e, env)
    } else {
        Ok(Value::Nil)
    }
}

/// Evaluate `(quote expr)`.
fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(EvalError::Parse {
            message: "quote requires exactly 1 argument".to_string(),
        });
    };
    quote_expr(expr)
}

/// Short-circuit `and`: returns last truthy value, or first falsy value.
fn eval_and(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env)?;
        if result == Value::Boolean(false) {
            return Ok(result);
        }
    }
    Ok(result)
}

/// Evaluate `(let ((var val) ...) body...)`.
fn eval_let(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let [bindings_expr, body @ ..] = args else {
        return Err(EvalError::Parse {
            message: "let requires bindings and body".to_string(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::Parse {
            message: "let requires bindings and body".to_string(),
        });
    }
    let Expr::List(bindings) = bindings_expr else {
        return Err(EvalError::Parse {
            message: "let bindings must be a list".to_string(),
        });
    };
    // Evaluate all values in the outer env, then bind simultaneously
    let pairs: Vec<(String, Value)> = bindings
        .iter()
        .map(|b| {
            let Expr::List(pair) = b else {
                return Err(EvalError::Parse {
                    message: "let binding must be a list".to_string(),
                });
            };
            let [Expr::Atom(name), val_expr] = pair.as_slice() else {
                return Err(EvalError::Parse {
                    message: "let binding must be (name value)".to_string(),
                });
            };
            let val = eval(val_expr, env)?;
            Ok((name.clone(), val))
        })
        .collect::<Result<_, _>>()?;
    let mut local_env = env.clone();
    for (name, val) in pairs {
        local_env.insert(name, val);
    }
    eval_body(body, &mut local_env)
}

/// Evaluate `(cond (test expr) ... (else expr))`.
fn eval_cond(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    for clause in args {
        let Expr::List(parts) = clause else {
            return Err(EvalError::Parse {
                message: "cond clause must be a list".to_string(),
            });
        };
        let [test_expr, body @ ..] = parts.as_slice() else {
            return Err(EvalError::Parse {
                message: "cond clause must have a test and body".to_string(),
            });
        };
        // Check for else clause
        if matches!(test_expr, Expr::Atom(s) if s == "else") {
            return eval_body(body, env);
        }
        let test_val = eval(test_expr, env)?;
        if test_val != Value::Boolean(false) {
            return eval_body(body, env);
        }
    }
    Ok(Value::Nil)
}

/// Short-circuit `or`: returns first truthy value, or last falsy value.
fn eval_or(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if result != Value::Boolean(false) {
            return Ok(result);
        }
    }
    Ok(result)
}
