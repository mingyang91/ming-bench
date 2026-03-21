use super::{
    atom_to_value, builtins, env_define, env_get, env_set, quote_expr, DisplayValue, Env,
    EvalError, Expr, Span, Value,
};

/// Result of one evaluation step: either a final value or a tail call to continue.
enum Trampoline {
    Done(Value),
    Continue(Expr, Env),
}

/// Extract an unbound variable name from an error (possibly wrapped in AtPosition).
fn as_unbound_variable(err: &EvalError) -> Option<&str> {
    match err {
        EvalError::UnboundVariable { name } => Some(name),
        EvalError::AtPosition { source, .. } => as_unbound_variable(source),
        _ => None,
    }
}

/// Wrap an error with source position, unless it already has one.
fn with_span(span: Span, err: EvalError) -> EvalError {
    match err {
        EvalError::AtPosition { .. } => err,
        _ => EvalError::AtPosition {
            line: span.line,
            col: span.col,
            source: Box::new(err),
        },
    }
}

/// Evaluate an expression in the given environment (trampoline loop for TCO).
pub fn eval(expr: &Expr, env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let span = expr.span();
    match eval_step(expr, env, out).map_err(|e| with_span(span, e))? {
        Trampoline::Done(val) => Ok(val),
        Trampoline::Continue(mut cur_expr, mut cur_env) => loop {
            let span = cur_expr.span();
            match eval_step(&cur_expr, &mut cur_env, out).map_err(|e| with_span(span, e))? {
                Trampoline::Done(val) => return Ok(val),
                Trampoline::Continue(next_expr, next_env) => {
                    cur_expr = next_expr;
                    cur_env = next_env;
                }
            }
        },
    }
}

/// One step of evaluation. Returns Continue for tail-position expressions.
fn eval_step(expr: &Expr, env: &mut Env, out: &mut String) -> Result<Trampoline, EvalError> {
    match expr {
        Expr::Atom(token, _) => {
            if token.starts_with('"')
                || token.parse::<i64>().is_ok()
                || token == "#t"
                || token == "#f"
                || token.starts_with("#\\")
            {
                atom_to_value(token).map(Trampoline::Done)
            } else if let Some(val) = env_get(env, token) {
                Ok(Trampoline::Done(val))
            } else {
                Err(EvalError::UnboundVariable {
                    name: token.clone(),
                })
            }
        }
        Expr::List(items, _) => eval_list_step(items, env, out),
    }
}

/// Evaluate a list expression, returning Continue for tail-position function calls.
fn eval_list_step(
    items: &[Expr],
    env: &mut Env,
    out: &mut String,
) -> Result<Trampoline, EvalError> {
    let [operator, args @ ..] = items else {
        return Err(EvalError::Parse {
            message: "empty list".to_string(),
        });
    };
    // Check for special forms (operator must be an atom)
    if let Expr::Atom(op, _) = operator {
        match op.as_str() {
            "define" => return eval_define(args, env, out).map(Trampoline::Done),
            "if" => return eval_if_step(args, env, out),
            "quote" => return eval_quote(args).map(Trampoline::Done),
            "and" => return eval_and(args, env, out).map(Trampoline::Done),
            "or" => return eval_or(args, env, out).map(Trampoline::Done),
            "lambda" => return eval_lambda(args, env).map(Trampoline::Done),
            "let" => return eval_let_step(args, env, out),
            "begin" => return eval_body_step(args, env, out),
            "cond" => return eval_cond_step(args, env, out),
            "display" => return eval_display(args, env, out).map(Trampoline::Done),
            "write" => return eval_write(args, env, out).map(Trampoline::Done),
            "newline" => return eval_newline(args, out).map(Trampoline::Done),
            "set!" => return eval_set(args, env, out).map(Trampoline::Done),
            "string-set!" => return eval_string_set(args, env, out).map(Trampoline::Done),
            _ => {}
        }
    }
    // Higher-order builtins that need function application
    if let Expr::Atom(op, _) = operator {
        if op == "map" && !env.contains_key("map") {
            return eval_builtin_map(args, env, out).map(Trampoline::Done);
        }
    }
    // General function application
    let evaluated: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env, out))
        .collect::<Result<_, _>>()?;
    // Try evaluating operator; fall back to builtin for atoms
    match eval(operator, env, out) {
        Ok(func @ Value::Lambda { .. }) => apply_lambda_step(&func, &evaluated, env, out),
        Ok(Value::Symbol(ref name)) => apply_builtin(name, &evaluated).map(Trampoline::Done),
        Ok(ref other) => Err(EvalError::TypeError {
            expected: "procedure".to_string(),
            got: format!("{other}"),
        }),
        Err(ref e) if as_unbound_variable(e).is_some() => {
            let name = as_unbound_variable(e).expect("checked above").to_string();
            apply_builtin(&name, &evaluated).map(Trampoline::Done)
        }
        Err(e) => Err(e),
    }
}

/// Set up a lambda's local env and return Continue for its body (tail call).
fn apply_lambda_step(
    func: &Value,
    args: &[Value],
    caller_env: &Env,
    out: &mut String,
) -> Result<Trampoline, EvalError> {
    let Value::Lambda {
        name,
        params,
        body,
        closure_env,
    } = func
    else {
        unreachable!("apply_lambda_step called with non-lambda");
    };
    if params.len() != args.len() {
        return Err(EvalError::WrongArgCount {
            expected: params.len(),
            got: args.len(),
        });
    }
    // Build local env: caller env as base (for mutual recursion), overlay closure, bind params
    let mut local_env = caller_env.clone();
    for (k, v) in closure_env {
        local_env.insert(k.clone(), v.clone());
    }
    // Inject self-reference for recursion
    if let Some(n) = name {
        env_define(&mut local_env, n.clone(), func.clone());
    }
    for (param, arg) in params.iter().zip(args) {
        env_define(&mut local_env, param.clone(), arg.clone());
    }
    eval_body_step(body, &mut local_env, out)
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
        "string?" => builtins::apply_type_predicate(args, |v| matches!(v, Value::String(_))),
        "number?" => builtins::apply_type_predicate(args, |v| matches!(v, Value::Integer(_))),
        "boolean?" => builtins::apply_type_predicate(args, |v| matches!(v, Value::Boolean(_))),
        "pair?" => builtins::apply_type_predicate(args, |v| match v {
            Value::Pair(_, _) => true,
            Value::List(items) => !items.is_empty(),
            _ => false,
        }),
        "symbol?" => builtins::apply_type_predicate(args, |v| matches!(v, Value::Symbol(_))),
        "char?" => builtins::apply_is_char(args),
        "string-length" => builtins::apply_string_length(args),
        "string-ref" => builtins::apply_string_ref(args),
        "string-append" => builtins::apply_string_append(args),
        "substring" => builtins::apply_substring(args),
        "string->number" => builtins::apply_string_to_number(args),
        "number->string" => builtins::apply_number_to_string(args),
        "symbol->string" => builtins::apply_symbol_to_string(args),
        "string->symbol" => builtins::apply_string_to_symbol(args),
        "string-copy" => builtins::apply_string_copy(args),
        "string->list" => builtins::apply_string_to_list(args),
        "list->string" => builtins::apply_list_to_string(args),
        "char->integer" => builtins::apply_char_to_integer(args),
        "integer->char" => builtins::apply_integer_to_char(args),
        // map is handled in eval_list_step as a higher-order function
        _ => Err(EvalError::UnboundVariable {
            name: op.to_string(),
        }),
    }
}

/// Apply a function value (lambda or builtin lookup) — fully resolves (no TCO).
fn apply_func(func: &Value, args: &[Value], out: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { .. } => apply_lambda(func, args, out),
        Value::Symbol(name) => apply_builtin(name, args),
        other => Err(EvalError::TypeError {
            expected: "procedure".to_string(),
            got: format!("{other}"),
        }),
    }
}

/// Evaluate a sequence of body expressions, returning the last result (no TCO).
fn eval_body(body: &[Expr], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Nil;
    for expr in body {
        result = eval(expr, env, out)?;
    }
    Ok(result)
}

/// Evaluate body expressions: all-but-last fully, last as Continue (TCO).
fn eval_body_step(
    body: &[Expr],
    env: &mut Env,
    out: &mut String,
) -> Result<Trampoline, EvalError> {
    let [rest @ .., last] = body else {
        return Ok(Trampoline::Done(Value::Nil));
    };
    for expr in rest {
        eval(expr, env, out)?;
    }
    Ok(Trampoline::Continue(last.clone(), env.clone()))
}

/// Apply a lambda closure to arguments (fully resolves — used by map).
fn apply_lambda(func: &Value, args: &[Value], out: &mut String) -> Result<Value, EvalError> {
    let Value::Lambda {
        name,
        params,
        body,
        closure_env,
    } = func
    else {
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
        env_define(&mut local_env, n.clone(), func.clone());
    }
    for (param, arg) in params.iter().zip(args) {
        env_define(&mut local_env, param.clone(), arg.clone());
    }
    eval_body(body, &mut local_env, out)
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
    let Expr::List(param_exprs, _) = params_expr else {
        return Err(EvalError::Parse {
            message: "lambda params must be a list".to_string(),
        });
    };
    let params: Vec<String> = param_exprs
        .iter()
        .map(|e| match e {
            Expr::Atom(name, _) => Ok(name.clone()),
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
fn eval_define(args: &[Expr], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
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
        Expr::Atom(name, _) => {
            let [value_expr] = body else {
                return Err(EvalError::Parse {
                    message: "define variable form takes exactly one value".to_string(),
                });
            };
            let val = eval(value_expr, env, out)?;
            env_define(env, name.clone(), val.clone());
            Ok(val)
        }
        Expr::List(parts, _) => {
            let [Expr::Atom(name, _), param_exprs @ ..] = parts.as_slice() else {
                return Err(EvalError::Parse {
                    message: "define function form requires a name".to_string(),
                });
            };
            let params: Vec<String> = param_exprs
                .iter()
                .map(|e| match e {
                    Expr::Atom(s, _) => Ok(s.clone()),
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
            env_define(env, name.clone(), lambda);
            Ok(Value::Nil)
        }
    }
}

/// Evaluate `(if cond then else)` — returns Continue for the chosen branch (TCO).
fn eval_if_step(
    args: &[Expr],
    env: &mut Env,
    out: &mut String,
) -> Result<Trampoline, EvalError> {
    let (cond, then_expr, else_expr) = match args {
        [c, t, e] => (c, t, Some(e)),
        [c, t] => (c, t, None),
        _ => {
            return Err(EvalError::Parse {
                message: "if requires 2 or 3 arguments".to_string(),
            })
        }
    };
    let cond_val = eval(cond, env, out)?;
    if cond_val != Value::Boolean(false) {
        Ok(Trampoline::Continue(then_expr.clone(), env.clone()))
    } else if let Some(e) = else_expr {
        Ok(Trampoline::Continue(e.clone(), env.clone()))
    } else {
        Ok(Trampoline::Done(Value::Nil))
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
fn eval_and(args: &[Expr], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env, out)?;
        if result == Value::Boolean(false) {
            return Ok(result);
        }
    }
    Ok(result)
}

/// Evaluate `(let ((var val) ...) body...)` — returns Continue for body (TCO).
fn eval_let_step(
    args: &[Expr],
    env: &mut Env,
    out: &mut String,
) -> Result<Trampoline, EvalError> {
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
    let Expr::List(bindings, _) = bindings_expr else {
        return Err(EvalError::Parse {
            message: "let bindings must be a list".to_string(),
        });
    };
    // Evaluate all values in the outer env, then bind simultaneously
    let pairs: Vec<(String, Value)> = bindings
        .iter()
        .map(|b| {
            let Expr::List(pair, _) = b else {
                return Err(EvalError::Parse {
                    message: "let binding must be a list".to_string(),
                });
            };
            let [Expr::Atom(name, _), val_expr] = pair.as_slice() else {
                return Err(EvalError::Parse {
                    message: "let binding must be (name value)".to_string(),
                });
            };
            let val = eval(val_expr, env, out)?;
            Ok((name.clone(), val))
        })
        .collect::<Result<_, _>>()?;
    let mut local_env = env.clone();
    for (name, val) in pairs {
        env_define(&mut local_env, name, val);
    }
    eval_body_step(body, &mut local_env, out)
}

/// Evaluate `(cond (test expr) ... (else expr))` — returns Continue for body (TCO).
fn eval_cond_step(
    args: &[Expr],
    env: &mut Env,
    out: &mut String,
) -> Result<Trampoline, EvalError> {
    for clause in args {
        let Expr::List(parts, _) = clause else {
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
        if matches!(test_expr, Expr::Atom(s, _) if s == "else") {
            return eval_body_step(body, env, out);
        }
        let test_val = eval(test_expr, env, out)?;
        if test_val != Value::Boolean(false) {
            return eval_body_step(body, env, out);
        }
    }
    Ok(Trampoline::Done(Value::Nil))
}

/// Short-circuit `or`: returns first truthy value, or last falsy value.
fn eval_or(args: &[Expr], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env, out)?;
        if result != Value::Boolean(false) {
            return Ok(result);
        }
    }
    Ok(result)
}

/// Evaluate `(display expr)` — prints value without quotes on strings.
fn eval_display(args: &[Expr], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(expr, env, out)?;
    use std::fmt::Write;
    write!(out, "{}", DisplayValue(&val)).expect("write to String cannot fail");
    Ok(Value::Nil)
}

/// Evaluate `(write expr)` — prints value with quotes on strings.
fn eval_write(args: &[Expr], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(expr, env, out)?;
    use std::fmt::Write;
    write!(out, "{val}").expect("write to String cannot fail");
    Ok(Value::Nil)
}

/// Evaluate builtin `(map func list)`.
fn eval_builtin_map(args: &[Expr], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let [func_expr, list_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let func = eval(func_expr, env, out)?;
    let list_val = eval(list_expr, env, out)?;
    let items = match &list_val {
        Value::Nil => return Ok(Value::Nil),
        Value::List(items) => items,
        _ => {
            return Err(EvalError::TypeError {
                expected: "list".to_string(),
                got: format!("{list_val}"),
            })
        }
    };
    let results: Vec<Value> = items
        .iter()
        .map(|item| apply_func(&func, std::slice::from_ref(item), out))
        .collect::<Result<_, _>>()?;
    if results.is_empty() {
        Ok(Value::Nil)
    } else {
        Ok(Value::List(results))
    }
}

/// Evaluate `(set! name value)` — mutate an existing binding.
fn eval_set(args: &[Expr], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let [Expr::Atom(name, _), value_expr] = args else {
        return Err(EvalError::Parse {
            message: "set! requires a variable name and a value".to_string(),
        });
    };
    let val = eval(value_expr, env, out)?;
    env_set(env, name, val)?;
    Ok(Value::Nil)
}

/// Evaluate `(string-set! ...)` — strings are immutable in R7RS.
fn eval_string_set(_args: &[Expr], _env: &mut Env, _out: &mut String) -> Result<Value, EvalError> {
    Err(EvalError::Immutable {
        message: "strings are immutable".to_string(),
    })
}

/// Evaluate `(newline)` — prints a newline character.
fn eval_newline(args: &[Expr], out: &mut String) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: 0,
            got: args.len(),
        });
    }
    out.push('\n');
    Ok(Value::Nil)
}
