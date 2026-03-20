use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Signals whether to return a value or continue the trampoline loop.
enum TailAction {
    Return(Value),
    TailCall { expr: Value, env: Env },
}

pub fn eval(expr: &Value, env: &Env) -> Result<Value, EvalError> {
    let mut current_expr = expr.clone();
    let mut current_env = env.clone();

    loop {
        match current_expr {
            Value::Integer(_) | Value::Boolean(_) | Value::Str(_) => return Ok(current_expr),
            Value::Symbol(ref name) => {
                return current_env.get(name).ok_or(EvalError::UnboundVariable {
                    name: name.clone(),
                });
            }
            Value::Lambda { .. } => return Ok(current_expr),
            Value::List(ref elems) => match eval_list_trampoline(elems, &current_env)? {
                TailAction::Return(val) => return Ok(val),
                TailAction::TailCall { expr, env } => {
                    current_expr = expr;
                    current_env = env;
                }
            },
        }
    }
}

fn eval_list_trampoline(elems: &[Value], env: &Env) -> Result<TailAction, EvalError> {
    if elems.is_empty() {
        return Err(EvalError::Parse {
            msg: "empty application".into(),
        });
    }

    if let Value::Symbol(name) = &elems[0] {
        if let Some(action) = eval_special_form(name, &elems[1..], env)? {
            return Ok(action);
        }
    }

    eval_application(elems, env)
}

fn eval_special_form(
    name: &str,
    args: &[Value],
    env: &Env,
) -> Result<Option<TailAction>, EvalError> {
    let action = match name {
        "define" => TailAction::Return(eval_define(args, env)?),
        "set!" => TailAction::Return(eval_set(args, env)?),
        "quote" => TailAction::Return(eval_quote(args)?),
        "lambda" => TailAction::Return(eval_lambda(args, env)?),
        "if" => eval_if_tail(args, env)?,
        "begin" => eval_begin_tail(args, env)?,
        "and" => eval_and_tail(args, env)?,
        "or" => eval_or_tail(args, env)?,
        "let" => eval_let_tail(args, env)?,
        "cond" => eval_cond_tail(args, env)?,
        _ => return Ok(None),
    };
    Ok(Some(action))
}

fn eval_application(elems: &[Value], env: &Env) -> Result<TailAction, EvalError> {
    let args: Vec<Value> = elems[1..]
        .iter()
        .map(|e| eval(e, env))
        .collect::<Result<Vec<_>, _>>()?;

    if let Value::Symbol(name) = &elems[0] {
        if is_builtin(name) {
            return Ok(TailAction::Return(call_builtin(name, &args)?));
        }
    }

    let op_val = eval(&elems[0], env)?;
    apply_lambda_tail(&op_val, &args)
}

fn apply_lambda_tail(op_val: &Value, args: &[Value]) -> Result<TailAction, EvalError> {
    let Value::Lambda {
        params,
        body,
        env: closure_env,
    } = op_val
    else {
        return Err(EvalError::NotAProcedure {
            value: op_val.to_string(),
        });
    };

    if params.len() != args.len() {
        return Err(EvalError::ArityError {
            name: "#<procedure>".into(),
            expected: params.len(),
            actual: args.len(),
        });
    }

    let call_env = closure_env.child();
    for (param, arg) in params.iter().zip(args.iter()) {
        call_env.define(param.clone(), arg.clone());
    }

    tail_from_body(body, &call_env)
}

fn tail_from_body(body: &[Value], env: &Env) -> Result<TailAction, EvalError> {
    if body.is_empty() {
        return Ok(TailAction::Return(Value::Symbol("void".into())));
    }
    for expr in &body[..body.len() - 1] {
        eval(expr, env)?;
    }
    Ok(TailAction::TailCall {
        expr: body[body.len() - 1].clone(),
        env: env.clone(),
    })
}

fn tail_from_body_or_default(
    body: &[Value],
    env: &Env,
    default: Value,
) -> Result<TailAction, EvalError> {
    if body.is_empty() {
        return Ok(TailAction::Return(default));
    }
    tail_from_body(body, env)
}

fn eval_if_tail(args: &[Value], env: &Env) -> Result<TailAction, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Parse {
            msg: "if requires 2 or 3 arguments".into(),
        });
    }
    let cond = eval(&args[0], env)?;
    if !is_falsy(&cond) {
        Ok(TailAction::TailCall {
            expr: args[1].clone(),
            env: env.clone(),
        })
    } else if args.len() == 3 {
        Ok(TailAction::TailCall {
            expr: args[2].clone(),
            env: env.clone(),
        })
    } else {
        Ok(TailAction::Return(Value::Symbol("void".into())))
    }
}

fn eval_begin_tail(args: &[Value], env: &Env) -> Result<TailAction, EvalError> {
    tail_from_body(args, env)
}

fn eval_and_tail(args: &[Value], env: &Env) -> Result<TailAction, EvalError> {
    if args.is_empty() {
        return Ok(TailAction::Return(Value::Boolean(true)));
    }
    for expr in &args[..args.len() - 1] {
        let result = eval(expr, env)?;
        if is_falsy(&result) {
            return Ok(TailAction::Return(result));
        }
    }
    Ok(TailAction::TailCall {
        expr: args[args.len() - 1].clone(),
        env: env.clone(),
    })
}

fn eval_or_tail(args: &[Value], env: &Env) -> Result<TailAction, EvalError> {
    if args.is_empty() {
        return Ok(TailAction::Return(Value::Boolean(false)));
    }
    for expr in &args[..args.len() - 1] {
        let result = eval(expr, env)?;
        if !is_falsy(&result) {
            return Ok(TailAction::Return(result));
        }
    }
    Ok(TailAction::TailCall {
        expr: args[args.len() - 1].clone(),
        env: env.clone(),
    })
}

fn eval_let_tail(args: &[Value], env: &Env) -> Result<TailAction, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            msg: "let requires bindings and body".into(),
        });
    }
    let Value::List(bindings) = &args[0] else {
        return Err(EvalError::Parse {
            msg: "let bindings must be a list".into(),
        });
    };
    let let_env = env.child();
    for binding in bindings {
        let Value::List(pair) = binding else {
            return Err(EvalError::Parse {
                msg: "let binding must be a list".into(),
            });
        };
        if pair.len() != 2 {
            return Err(EvalError::Parse {
                msg: "let binding must have exactly 2 elements".into(),
            });
        }
        let Value::Symbol(bname) = &pair[0] else {
            return Err(EvalError::Parse {
                msg: "let binding name must be a symbol".into(),
            });
        };
        let val = eval(&pair[1], env)?;
        let_env.define(bname.clone(), val);
    }
    tail_from_body(&args[1..], &let_env)
}

fn eval_cond_tail(clauses: &[Value], env: &Env) -> Result<TailAction, EvalError> {
    for clause in clauses {
        let Value::List(parts) = clause else {
            return Err(EvalError::Parse {
                msg: "cond clause must be a list".into(),
            });
        };
        if parts.is_empty() {
            return Err(EvalError::Parse {
                msg: "cond clause cannot be empty".into(),
            });
        }
        let is_else = matches!(&parts[0], Value::Symbol(s) if s == "else");
        let test = if is_else {
            Value::Boolean(true)
        } else {
            eval(&parts[0], env)?
        };
        if !is_falsy(&test) {
            return tail_from_body_or_default(&parts[1..], env, test);
        }
    }
    Ok(TailAction::Return(Value::Symbol("void".into())))
}

fn eval_lambda(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            msg: "lambda requires params and body".into(),
        });
    }
    let Value::List(param_list) = &args[0] else {
        return Err(EvalError::Parse {
            msg: "lambda params must be a list".into(),
        });
    };
    let params: Vec<String> = param_list
        .iter()
        .map(|p| match p {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(EvalError::Parse {
                msg: format!("lambda param must be a symbol, got {other}"),
            }),
        })
        .collect::<Result<Vec<_>, _>>()?;
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        env: env.clone(),
    })
}

fn eval_define(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            msg: "define requires at least 2 arguments".into(),
        });
    }
    match &args[0] {
        Value::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Parse {
                    msg: "define requires exactly 2 arguments".into(),
                });
            }
            let val = eval(&args[1], env)?;
            env.define(name.clone(), val);
        }
        Value::List(name_and_params) => {
            if name_and_params.is_empty() {
                return Err(EvalError::Parse {
                    msg: "define function form requires a name".into(),
                });
            }
            let Value::Symbol(name) = &name_and_params[0] else {
                return Err(EvalError::Parse {
                    msg: "define function name must be a symbol".into(),
                });
            };
            let params: Vec<String> = name_and_params[1..]
                .iter()
                .map(|p| match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    other => Err(EvalError::Parse {
                        msg: format!("param must be a symbol, got {other}"),
                    }),
                })
                .collect::<Result<Vec<_>, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env.define(name.clone(), lambda);
        }
        other => {
            return Err(EvalError::Parse {
                msg: format!("define target must be a symbol or list, got {other}"),
            });
        }
    }
    Ok(Value::Symbol("void".into()))
}

fn eval_set(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Parse {
            msg: "set! requires exactly 2 arguments".into(),
        });
    }
    let Value::Symbol(name) = &args[0] else {
        return Err(EvalError::Parse {
            msg: "set! target must be a symbol".into(),
        });
    };
    let val = eval(&args[1], env)?;
    if !env.set(name, val) {
        return Err(EvalError::UnboundVariable {
            name: name.clone(),
        });
    }
    Ok(Value::Symbol("void".into()))
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Parse {
            msg: "quote requires exactly 1 argument".into(),
        });
    }
    Ok(args[0].clone())
}

fn expect_integer(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::TypeError {
            expected: "integer".into(),
            got: other.to_string(),
        }),
    }
}

fn is_falsy(val: &Value) -> bool {
    matches!(val, Value::Boolean(false))
}

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | "not" | "cons" | "car" | "cdr"
            | "null?" | "list" | "length" | "string?" | "number?" | "boolean?" | "pair?"
            | "symbol?"
    )
}

fn call_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => arith_add(args),
        "-" => arith_sub(name, args),
        "*" => arith_mul(args),
        "/" => arith_div(name, args),
        "<" => cmp_lt(args),
        ">" => cmp_gt(args),
        "=" => cmp_eq(args),
        "<=" => cmp_le(args),
        "not" => eval_not(name, args),
        "cons" => builtin_cons(name, args),
        "car" => builtin_car(name, args),
        "cdr" => builtin_cdr(name, args),
        "null?" => builtin_null(name, args),
        "list" => Ok(Value::List(args.to_vec())),
        "length" => builtin_length(name, args),
        "string?" => builtin_type_pred(name, args, |v| matches!(v, Value::Str(_))),
        "number?" => builtin_type_pred(name, args, |v| matches!(v, Value::Integer(_))),
        "boolean?" => builtin_type_pred(name, args, |v| matches!(v, Value::Boolean(_))),
        "pair?" => builtin_type_pred(name, args, |v| {
            matches!(v, Value::List(elems) if !elems.is_empty())
        }),
        "symbol?" => builtin_type_pred(name, args, |v| matches!(v, Value::Symbol(_))),
        _ => unreachable!(),
    }
}

fn arith_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum: i64 = 0;
    for arg in args {
        sum += expect_integer(arg)?;
    }
    Ok(Value::Integer(sum))
}

fn arith_sub(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::ArityError {
            name: name.into(),
            expected: 1,
            actual: 0,
        });
    }
    let first = expect_integer(&args[0])?;
    if args.len() == 1 {
        return Ok(Value::Integer(-first));
    }
    let mut result = first;
    for arg in &args[1..] {
        result -= expect_integer(arg)?;
    }
    Ok(Value::Integer(result))
}

fn arith_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product: i64 = 1;
    for arg in args {
        product *= expect_integer(arg)?;
    }
    Ok(Value::Integer(product))
}

fn arith_div(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::ArityError {
            name: name.into(),
            expected: 1,
            actual: 0,
        });
    }
    let first = expect_integer(&args[0])?;
    if args.len() == 1 {
        if first == 0 {
            return Err(EvalError::DivisionByZero);
        }
        return Ok(Value::Integer(1 / first));
    }
    let mut result = first;
    for arg in &args[1..] {
        let divisor = expect_integer(arg)?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= divisor;
    }
    Ok(Value::Integer(result))
}

fn cmp_lt(args: &[Value]) -> Result<Value, EvalError> {
    let a = expect_integer(&args[0])?;
    let b = expect_integer(&args[1])?;
    Ok(Value::Boolean(a < b))
}

fn cmp_gt(args: &[Value]) -> Result<Value, EvalError> {
    let a = expect_integer(&args[0])?;
    let b = expect_integer(&args[1])?;
    Ok(Value::Boolean(a > b))
}

fn cmp_eq(args: &[Value]) -> Result<Value, EvalError> {
    let a = expect_integer(&args[0])?;
    let b = expect_integer(&args[1])?;
    Ok(Value::Boolean(a == b))
}

fn cmp_le(args: &[Value]) -> Result<Value, EvalError> {
    let a = expect_integer(&args[0])?;
    let b = expect_integer(&args[1])?;
    Ok(Value::Boolean(a <= b))
}

fn eval_not(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArityError {
            name: name.into(),
            expected: 1,
            actual: args.len(),
        });
    }
    Ok(Value::Boolean(is_falsy(&args[0])))
}

fn builtin_cons(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::ArityError {
            name: name.into(),
            expected: 2,
            actual: args.len(),
        });
    }
    match &args[1] {
        Value::List(elems) => {
            let mut new_list = vec![args[0].clone()];
            new_list.extend(elems.iter().cloned());
            Ok(Value::List(new_list))
        }
        _ => Err(EvalError::TypeError {
            expected: "list".into(),
            got: args[1].to_string(),
        }),
    }
}

fn builtin_car(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArityError {
            name: name.into(),
            expected: 1,
            actual: args.len(),
        });
    }
    match &args[0] {
        Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
        Value::List(_) => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: "()".into(),
        }),
        other => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: other.to_string(),
        }),
    }
}

fn builtin_cdr(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArityError {
            name: name.into(),
            expected: 1,
            actual: args.len(),
        });
    }
    match &args[0] {
        Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
        Value::List(_) => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: "()".into(),
        }),
        other => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: other.to_string(),
        }),
    }
}

fn builtin_null(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArityError {
            name: name.into(),
            expected: 1,
            actual: args.len(),
        });
    }
    Ok(Value::Boolean(
        matches!(&args[0], Value::List(elems) if elems.is_empty()),
    ))
}

fn builtin_type_pred(
    name: &str,
    args: &[Value],
    pred: impl Fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArityError {
            name: name.into(),
            expected: 1,
            actual: args.len(),
        });
    }
    Ok(Value::Boolean(pred(&args[0])))
}

fn builtin_length(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArityError {
            name: name.into(),
            expected: 1,
            actual: args.len(),
        });
    }
    match &args[0] {
        Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
        other => Err(EvalError::TypeError {
            expected: "list".into(),
            got: other.to_string(),
        }),
    }
}
