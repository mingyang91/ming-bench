use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::value::Value;

pub fn eval(expr: &Value, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_, _)
        | Value::Boolean(_, _)
        | Value::String(_, _)
        | Value::Closure { .. } => Ok(expr.clone()),
        Value::Symbol(name, span) => {
            if let Some(val) = env.get(name) {
                Ok(val)
            } else {
                match name.as_str() {
                    "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
                    | "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length"
                    | "append" | "pair?" | "string?" | "number?" | "boolean?"
                    | "symbol?" | "zero?" | "positive?" | "negative?"
                    | "even?" | "odd?" | "abs" | "min" | "max" | "modulo"
                    | "remainder" | "quotient" => Ok(expr.clone()),
                    _ => Err(EvalError::UnboundVariable {
                        name: name.clone(),
                        span: *span,
                    }),
                }
            }
        }
        Value::List(elems, span) => eval_list(elems, *span, env),
        Value::Void => Ok(Value::Void),
    }
}

fn eval_list(elems: &[Value], list_span: Span, env: &Env) -> Result<Value, EvalError> {
    if elems.is_empty() {
        return Err(EvalError::Parse {
            message: "empty application".to_string(),
            span: list_span,
        });
    }

    let head = &elems[0];

    // Check for special forms
    if let Value::Symbol(name, _) = head {
        match name.as_str() {
            "and" => return eval_and(&elems[1..], env),
            "or" => return eval_or(&elems[1..], env),
            "not" => return eval_not(&elems[1..], list_span, env),
            "if" => return eval_if(&elems[1..], list_span, env),
            "define" => return eval_define(&elems[1..], list_span, env),
            "quote" => return eval_quote(&elems[1..], list_span),
            "lambda" => return eval_lambda(&elems[1..], list_span, env),
            "let" => return eval_let(&elems[1..], list_span, env),
            "begin" => return eval_begin(&elems[1..], env),
            "cond" => return eval_cond(&elems[1..], env),
            _ => {}
        }
    }

    // Evaluate all elements
    let op = eval(head, env)?;
    let args: Vec<Value> = elems[1..]
        .iter()
        .map(|e| eval(e, env))
        .collect::<Result<_, _>>()?;

    apply(&op, &args, list_span)
}

fn apply(op: &Value, args: &[Value], call_span: Span) -> Result<Value, EvalError> {
    match op {
        Value::Symbol(name, _) => apply_builtin(name, args, call_span),
        Value::Closure {
            params,
            body,
            env: closure_env,
        } => {
            if params.len() != args.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len().to_string(),
                    got: args.len(),
                    span: call_span,
                });
            }
            let local_env = Env::with_parent(closure_env);
            for (param, arg) in params.iter().zip(args.iter()) {
                local_env.define(param.clone(), arg.clone());
            }
            eval(body, &local_env)
        }
        other => Err(EvalError::NotAProcedure {
            value: other.to_string(),
            span: other.span(),
        }),
    }
}

fn apply_builtin(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match name {
        "+" => arith_add(args),
        "-" => arith_sub(args, span),
        "*" => arith_mul(args),
        "/" => arith_div(args, span),
        "<" => cmp_lt(args),
        ">" => cmp_gt(args),
        "=" => cmp_eq(args),
        "<=" => cmp_le(args),
        ">=" => cmp_ge(args),
        "cons" => builtin_cons(args, span),
        "car" => builtin_car(args, span),
        "cdr" => builtin_cdr(args, span),
        "null?" => builtin_null(args, span),
        "list" => builtin_list(args),
        "length" => builtin_length(args, span),
        "append" => builtin_append(args),
        "pair?" => builtin_pair(args, span),
        "string?" => Ok(Value::bool(matches!(args, [Value::String(_, _)]))),
        "number?" => Ok(Value::bool(matches!(args, [Value::Integer(_, _)]))),
        "boolean?" => Ok(Value::bool(matches!(args, [Value::Boolean(_, _)]))),
        "symbol?" => Ok(Value::bool(matches!(args, [Value::Symbol(_, _)]))),
        "zero?" => match args {
            [Value::Integer(n, _)] => Ok(Value::bool(*n == 0)),
            _ => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: args
                    .first()
                    .map_or("nothing", |_| "non-integer")
                    .to_string(),
                span: args.first().map_or(span, |v| v.span()),
            }),
        },
        "positive?" => match args {
            [Value::Integer(n, _)] => Ok(Value::bool(*n > 0)),
            _ => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: "non-integer".to_string(),
                span: args.first().map_or(span, |v| v.span()),
            }),
        },
        "negative?" => match args {
            [Value::Integer(n, _)] => Ok(Value::bool(*n < 0)),
            _ => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: "non-integer".to_string(),
                span: args.first().map_or(span, |v| v.span()),
            }),
        },
        "even?" => match args {
            [Value::Integer(n, _)] => Ok(Value::bool(*n % 2 == 0)),
            _ => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: "non-integer".to_string(),
                span: args.first().map_or(span, |v| v.span()),
            }),
        },
        "odd?" => match args {
            [Value::Integer(n, _)] => Ok(Value::bool(*n % 2 != 0)),
            _ => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: "non-integer".to_string(),
                span: args.first().map_or(span, |v| v.span()),
            }),
        },
        "abs" => match args {
            [Value::Integer(n, _)] => Ok(Value::int(n.abs())),
            _ => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: "non-integer".to_string(),
                span: args.first().map_or(span, |v| v.span()),
            }),
        },
        "min" => arith_min(args, span),
        "max" => arith_max(args, span),
        "modulo" => arith_modulo(args, span),
        "remainder" => arith_remainder(args, span),
        "quotient" => arith_div(args, span),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
            span,
        }),
    }
}

fn eval_if(args: &[Value], form_span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::WrongArgCount {
            expected: "2 or 3".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_define(args: &[Value], form_span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    match &args[0] {
        // (define x expr)
        Value::Symbol(name, _) => {
            let val = eval(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body)
        Value::List(elems, _) => {
            if elems.is_empty() {
                return Err(EvalError::Parse {
                    message: "empty define function name".to_string(),
                    span: form_span,
                });
            }
            let Value::Symbol(name, _) = &elems[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "symbol".to_string(),
                    got: elems[0].to_string(),
                    span: elems[0].span(),
                });
            };
            let params: Vec<String> = elems[1..]
                .iter()
                .map(|e| match e {
                    Value::Symbol(s, _) => Ok(s.clone()),
                    other => Err(EvalError::TypeMismatch {
                        expected: "symbol".to_string(),
                        got: other.to_string(),
                        span: other.span(),
                    }),
                })
                .collect::<Result<_, _>>()?;
            let body = if args.len() == 2 {
                args[1].clone()
            } else {
                Value::list(
                    std::iter::once(Value::symbol("begin".to_string()))
                        .chain(args[1..].iter().cloned())
                        .collect(),
                )
            };
            let closure = Value::Closure {
                params,
                body: Box::new(body),
                env: env.clone(),
            };
            env.define(name.clone(), closure);
            Ok(Value::Void)
        }
        other => Err(EvalError::TypeMismatch {
            expected: "symbol or list".to_string(),
            got: other.to_string(),
            span: other.span(),
        }),
    }
}

fn eval_quote(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    Ok(args[0].clone())
}

fn eval_lambda(args: &[Value], form_span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    let Value::List(param_list, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "parameter list".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    let params: Vec<String> = param_list
        .iter()
        .map(|e| match e {
            Value::Symbol(s, _) => Ok(s.clone()),
            other => Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: other.to_string(),
                span: other.span(),
            }),
        })
        .collect::<Result<_, _>>()?;
    let body = if args.len() == 2 {
        args[1].clone()
    } else {
        Value::list(
            std::iter::once(Value::symbol("begin".to_string()))
                .chain(args[1..].iter().cloned())
                .collect(),
        )
    };
    Ok(Value::Closure {
        params,
        body: Box::new(body),
        env: env.clone(),
    })
}

fn require_integers(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|v| match v {
            Value::Integer(n, _) => Ok(*n),
            other => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: format!("{other}"),
                span: other.span(),
            }),
        })
        .collect()
}

fn arith_add(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::int(nums.iter().sum()))
}

fn arith_sub(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "at least 1".to_string(),
            got: 0,
            span: form_span,
        });
    }
    if nums.len() == 1 {
        return Ok(Value::int(-nums[0]));
    }
    let result = nums[1..].iter().fold(nums[0], |acc, n| acc - n);
    Ok(Value::int(result))
}

fn arith_mul(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::int(nums.iter().product()))
}

fn arith_div(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: nums.len(),
            span: form_span,
        });
    }
    let mut result = nums[0];
    for &n in &nums[1..] {
        if n == 0 {
            return Err(EvalError::DivisionByZero { span: form_span });
        }
        result /= n;
    }
    Ok(Value::int(result))
}

fn cmp_lt(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::bool(nums.windows(2).all(|w| w[0] < w[1])))
}

fn cmp_gt(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::bool(nums.windows(2).all(|w| w[0] > w[1])))
}

fn cmp_eq(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::bool(nums.windows(2).all(|w| w[0] == w[1])))
}

fn cmp_le(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::bool(nums.windows(2).all(|w| w[0] <= w[1])))
}

fn eval_and(exprs: &[Value], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::bool(true));
    }
    let mut result = Value::bool(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Value], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::bool(false));
    }
    let mut result = Value::bool(false);
    for expr in exprs {
        result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_not(args: &[Value], form_span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    let val = eval(&args[0], env)?;
    Ok(Value::bool(!val.is_truthy()))
}

fn eval_begin(exprs: &[Value], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in exprs {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_let(args: &[Value], form_span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
            span: form_span,
        });
    }

    // Named let: (let name ((var init) ...) body ...)
    if let Value::Symbol(name, _) = &args[0] {
        let Value::List(bindings, _) = &args[1] else {
            return Err(EvalError::TypeMismatch {
                expected: "binding list".to_string(),
                got: args[1].to_string(),
                span: args[1].span(),
            });
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for binding in bindings {
            let Value::List(pair, _) = binding else {
                return Err(EvalError::TypeMismatch {
                    expected: "binding pair".to_string(),
                    got: binding.to_string(),
                    span: binding.span(),
                });
            };
            if pair.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    expected: "2".to_string(),
                    got: pair.len(),
                    span: binding.span(),
                });
            }
            let Value::Symbol(var, _) = &pair[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "symbol".to_string(),
                    got: pair[0].to_string(),
                    span: pair[0].span(),
                });
            };
            params.push(var.clone());
            inits.push(eval(&pair[1], env)?);
        }
        let body = if args.len() == 3 {
            args[2].clone()
        } else {
            Value::list(
                std::iter::once(Value::symbol("begin".to_string()))
                    .chain(args[2..].iter().cloned())
                    .collect(),
            )
        };
        let local_env = Env::with_parent(env);
        let closure = Value::Closure {
            params: params.clone(),
            body: Box::new(body),
            env: local_env.clone(),
        };
        local_env.define(name.clone(), closure);
        for (param, init) in params.iter().zip(inits.iter()) {
            local_env.define(param.clone(), init.clone());
        }
        let mut result = Value::Void;
        for expr in &args[2..] {
            result = eval(expr, &local_env)?;
        }
        return Ok(result);
    }

    // Regular let: (let ((var init) ...) body ...)
    let Value::List(bindings, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "binding list".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        });
    };
    let local_env = Env::with_parent(env);
    for binding in bindings {
        let Value::List(pair, _) = binding else {
            return Err(EvalError::TypeMismatch {
                expected: "binding pair".to_string(),
                got: binding.to_string(),
                span: binding.span(),
            });
        };
        if pair.len() != 2 {
            return Err(EvalError::WrongArgCount {
                expected: "2".to_string(),
                got: pair.len(),
                span: binding.span(),
            });
        }
        let Value::Symbol(var, _) = &pair[0] else {
            return Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: pair[0].to_string(),
                span: pair[0].span(),
            });
        };
        let val = eval(&pair[1], env)?;
        local_env.define(var.clone(), val);
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Value], env: &Env) -> Result<Value, EvalError> {
    for clause in clauses {
        let Value::List(parts, _) = clause else {
            return Err(EvalError::TypeMismatch {
                expected: "cond clause".to_string(),
                got: clause.to_string(),
                span: clause.span(),
            });
        };
        if parts.is_empty() {
            return Err(EvalError::Parse {
                message: "empty cond clause".to_string(),
                span: clause.span(),
            });
        }
        // else clause
        if let Value::Symbol(s, _) = &parts[0] {
            if s == "else" {
                let mut result = Value::Void;
                for expr in &parts[1..] {
                    result = eval(expr, env)?;
                }
                return Ok(result);
            }
        }
        let test = eval(&parts[0], env)?;
        if test.is_truthy() {
            if parts.len() == 1 {
                return Ok(test);
            }
            let mut result = Value::Void;
            for expr in &parts[1..] {
                result = eval(expr, env)?;
            }
            return Ok(result);
        }
    }
    Ok(Value::Void)
}

fn cmp_ge(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::bool(nums.windows(2).all(|w| w[0] >= w[1])))
}

fn builtin_cons(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    match &args[1] {
        Value::List(elems, _) => {
            let mut new_list = vec![args[0].clone()];
            new_list.extend(elems.iter().cloned());
            Ok(Value::list(new_list))
        }
        _ => {
            // Improper pair — store as 2-element tagged structure for now
            Ok(Value::list(vec![
                args[0].clone(),
                Value::symbol(".".to_string()),
                args[1].clone(),
            ]))
        }
    }
}

fn builtin_car(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    match &args[0] {
        Value::List(elems, _) if !elems.is_empty() => Ok(elems[0].clone()),
        _ => Err(EvalError::TypeMismatch {
            expected: "pair".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        }),
    }
}

fn builtin_cdr(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    match &args[0] {
        Value::List(elems, _) if !elems.is_empty() => {
            Ok(Value::list(elems[1..].to_vec()))
        }
        _ => Err(EvalError::TypeMismatch {
            expected: "pair".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        }),
    }
}

fn builtin_null(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    Ok(Value::bool(matches!(
        &args[0],
        Value::List(elems, _) if elems.is_empty()
    )))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::list(args.to_vec()))
}

fn builtin_length(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    match &args[0] {
        Value::List(elems, _) => Ok(Value::int(elems.len() as i64)),
        _ => Err(EvalError::TypeMismatch {
            expected: "list".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
        }),
    }
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        if i == args.len() - 1 {
            // Last argument can be any value (for improper lists), but for proper lists:
            match arg {
                Value::List(elems, _) => result.extend(elems.iter().cloned()),
                other => {
                    if result.is_empty() {
                        return Ok(other.clone());
                    }
                    result.push(other.clone());
                }
            }
        } else {
            match arg {
                Value::List(elems, _) => result.extend(elems.iter().cloned()),
                _ => {
                    return Err(EvalError::TypeMismatch {
                        expected: "list".to_string(),
                        got: arg.to_string(),
                        span: arg.span(),
                    });
                }
            }
        }
    }
    Ok(Value::list(result))
}

fn builtin_pair(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span: form_span,
        });
    }
    Ok(Value::bool(matches!(
        &args[0],
        Value::List(elems, _) if !elems.is_empty()
    )))
}

fn arith_min(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    nums.iter()
        .copied()
        .min()
        .map(Value::int)
        .ok_or(EvalError::WrongArgCount {
            expected: "at least 1".to_string(),
            got: 0,
            span: form_span,
        })
}

fn arith_max(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    nums.iter()
        .copied()
        .max()
        .map(Value::int)
        .ok_or(EvalError::WrongArgCount {
            expected: "at least 1".to_string(),
            got: 0,
            span: form_span,
        })
}

fn arith_modulo(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: nums.len(),
            span: form_span,
        });
    }
    if nums[1] == 0 {
        return Err(EvalError::DivisionByZero { span: form_span });
    }
    Ok(Value::int(((nums[0] % nums[1]) + nums[1]) % nums[1]))
}

fn arith_remainder(args: &[Value], form_span: Span) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: nums.len(),
            span: form_span,
        });
    }
    if nums[1] == 0 {
        return Err(EvalError::DivisionByZero { span: form_span });
    }
    Ok(Value::int(nums[0] % nums[1]))
}
