use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::value::Value;

/// Evaluate non-tail expressions of `and`. Returns `Some(value)` for short-circuit, `None` for tail.
fn eval_and_prefix(exprs: &[Value], env: &Env) -> Result<Option<Value>, EvalError> {
    for expr in exprs {
        let result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(Some(result));
        }
    }
    Ok(None)
}

/// Evaluate non-tail expressions of `or`. Returns `Some(value)` for short-circuit, `None` for tail.
fn eval_or_prefix(exprs: &[Value], env: &Env) -> Result<Option<Value>, EvalError> {
    for expr in exprs {
        let result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(Some(result));
        }
    }
    Ok(None)
}

pub fn eval(expr: &Value, env: &Env) -> Result<Value, EvalError> {
    let mut current_expr = expr.clone();
    let mut current_env = env.clone();

    loop {
        match &current_expr {
            Value::Integer(_, _)
            | Value::Boolean(_, _)
            | Value::String(_, _)
            | Value::Char(_, _)
            | Value::Closure { .. } => return Ok(current_expr),
            Value::Symbol(name, span) => {
                if let Some(val) = current_env.get(name) {
                    return Ok(val);
                }
                return match name.as_str() {
                    "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
                    | "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length"
                    | "append" | "pair?" | "string?" | "number?" | "boolean?"
                    | "symbol?" | "zero?" | "positive?" | "negative?"
                    | "even?" | "odd?" | "abs" | "min" | "max" | "modulo"
                    | "remainder" | "quotient"
                    | "display" | "write" | "newline"
                    | "string-append" | "string-length" | "substring"
                    | "string->number" | "number->string"
                    | "symbol->string" | "string->symbol"
                    | "string-ref" | "string-set!" | "string-copy"
                    | "char?" => Ok(current_expr.clone()),
                    _ => Err(EvalError::UnboundVariable {
                        name: name.clone(),
                        span: *span,
                    }),
                };
            }
            Value::List(elems, span) => {
                let list_span = *span;
                if elems.is_empty() {
                    return Err(EvalError::Parse {
                        message: "empty application".to_string(),
                        span: list_span,
                    });
                }

                let head = &elems[0];

                // Check for special forms — handle tail positions inline
                if let Value::Symbol(name, _) = head {
                    match name.as_str() {
                        "and" => {
                            let exprs = &elems[1..];
                            if exprs.is_empty() { return Ok(Value::bool(true)); }
                            if let Some(v) = eval_and_prefix(&exprs[..exprs.len() - 1], &current_env)? {
                                return Ok(v);
                            }
                            current_expr = exprs[exprs.len() - 1].clone();
                            continue;
                        }
                        "or" => {
                            let exprs = &elems[1..];
                            if exprs.is_empty() { return Ok(Value::bool(false)); }
                            if let Some(v) = eval_or_prefix(&exprs[..exprs.len() - 1], &current_env)? {
                                return Ok(v);
                            }
                            current_expr = exprs[exprs.len() - 1].clone();
                            continue;
                        }
                        "not" => return eval_not(&elems[1..], list_span, &current_env),
                        "if" => {
                            let args = &elems[1..];
                            if args.len() < 2 || args.len() > 3 {
                                return Err(EvalError::WrongArgCount {
                                    expected: "2 or 3".to_string(),
                                    got: args.len(),
                                    span: list_span,
                                });
                            }
                            let cond = eval(&args[0], &current_env)?;
                            if cond.is_truthy() {
                                current_expr = args[1].clone();
                                continue;
                            } else if args.len() == 3 {
                                current_expr = args[2].clone();
                                continue;
                            } else {
                                return Ok(Value::Void);
                            }
                        }
                        "define" => return eval_define(&elems[1..], list_span, &current_env),
                        "quote" => return eval_quote(&elems[1..], list_span),
                        "lambda" => return eval_lambda(&elems[1..], list_span, &current_env),
                        "let" => {
                            let args = &elems[1..];
                            if args.len() < 2 {
                                return Err(EvalError::WrongArgCount {
                                    expected: "at least 2".to_string(),
                                    got: args.len(),
                                    span: list_span,
                                });
                            }
                            // Named let
                            if let Value::Symbol(loop_name, _) = &args[0] {
                                let (new_env, body) = setup_named_let(loop_name, &args[1..], list_span, &current_env)?;
                                current_expr = body;
                                current_env = new_env;
                                continue;
                            }
                            // Regular let
                            let new_env = setup_let(&args[0], list_span, &current_env)?;
                            let body = &args[1..];
                            for expr in &body[..body.len() - 1] {
                                eval(expr, &new_env)?;
                            }
                            current_expr = body[body.len() - 1].clone();
                            current_env = new_env;
                            continue;
                        }
                        "begin" => {
                            let exprs = &elems[1..];
                            if exprs.is_empty() {
                                return Ok(Value::Void);
                            }
                            for expr in &exprs[..exprs.len() - 1] {
                                eval(expr, &current_env)?;
                            }
                            current_expr = exprs[exprs.len() - 1].clone();
                            continue;
                        }
                        "cond" => {
                            match eval_cond_tail(&elems[1..], &current_env)? {
                                TailAction::Result(v) => return Ok(v),
                                TailAction::TailCall(expr, env) => {
                                    current_expr = expr;
                                    current_env = env;
                                    continue;
                                }
                            }
                        }
                        _ => {}
                    }
                }

                // Evaluate all elements (function application)
                let op = eval(head, &current_env)?;
                let args: Vec<Value> = elems[1..]
                    .iter()
                    .map(|e| eval(e, &current_env))
                    .collect::<Result<_, _>>()?;

                // Tail-call for closures
                match op {
                    Value::Symbol(ref name, _) => return apply_builtin(name, &args, list_span, &current_env),
                    Value::Closure {
                        ref params,
                        ref body,
                        env: ref closure_env,
                    } => {
                        if params.len() != args.len() {
                            return Err(EvalError::WrongArgCount {
                                expected: params.len().to_string(),
                                got: args.len(),
                                span: list_span,
                            });
                        }
                        let local_env = Env::with_parent(closure_env);
                        for (param, arg) in params.iter().zip(args.iter()) {
                            local_env.define(param.clone(), arg.clone());
                        }
                        current_expr = *body.clone();
                        current_env = local_env;
                        continue;
                    }
                    other => {
                        return Err(EvalError::NotAProcedure {
                            value: other.to_string(),
                            span: other.span(),
                        });
                    }
                }
            }
            Value::Void => return Ok(Value::Void),
        }
    }
}

enum TailAction {
    Result(Value),
    TailCall(Value, Env),
}

/// Set up named let: returns (env, body_expr) for tail-call loop
fn setup_named_let(name: &str, args: &[Value], form_span: Span, env: &Env) -> Result<(Env, Value), EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: 1,
            span: form_span,
        });
    }
    let Value::List(bindings, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "binding list".to_string(),
            got: args[0].to_string(),
            span: args[0].span(),
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
    let body = if args.len() == 2 {
        args[1].clone()
    } else {
        Value::list(
            std::iter::once(Value::symbol("begin".to_string()))
                .chain(args[1..].iter().cloned())
                .collect(),
        )
    };
    let local_env = Env::with_parent(env);
    let closure = Value::Closure {
        params: params.clone(),
        body: Box::new(body.clone()),
        env: local_env.clone(),
    };
    local_env.define(name.to_string(), closure);
    for (param, init) in params.iter().zip(inits.iter()) {
        local_env.define(param.clone(), init.clone());
    }
    Ok((local_env, body))
}

/// Set up regular let bindings, return the new env
fn setup_let(bindings_expr: &Value, _form_span: Span, env: &Env) -> Result<Env, EvalError> {
    let Value::List(bindings, _) = bindings_expr else {
        return Err(EvalError::TypeMismatch {
            expected: "binding list".to_string(),
            got: bindings_expr.to_string(),
            span: bindings_expr.span(),
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
    Ok(local_env)
}

/// Evaluate cond, returning either a result or a tail-call action for the last expression
fn eval_cond_tail(clauses: &[Value], env: &Env) -> Result<TailAction, EvalError> {
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
                for expr in &parts[1..parts.len() - 1] {
                    eval(expr, env)?;
                }
                if parts.len() > 1 {
                    return Ok(TailAction::TailCall(parts[parts.len() - 1].clone(), env.clone()));
                }
                return Ok(TailAction::Result(Value::Void));
            }
        }
        let test = eval(&parts[0], env)?;
        if test.is_truthy() {
            if parts.len() == 1 {
                return Ok(TailAction::Result(test));
            }
            for expr in &parts[1..parts.len() - 1] {
                eval(expr, env)?;
            }
            return Ok(TailAction::TailCall(parts[parts.len() - 1].clone(), env.clone()));
        }
    }
    Ok(TailAction::Result(Value::Void))
}

fn apply_builtin(name: &str, args: &[Value], span: Span, env: &Env) -> Result<Value, EvalError> {
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
        "display" => builtin_display(args, span, env),
        "write" => builtin_write(args, span, env),
        "newline" => builtin_newline(args, span, env),
        "string-append" => builtin_string_append(args),
        "string-length" => builtin_string_length(args, span),
        "substring" => builtin_substring(args, span),
        "string->number" => builtin_string_to_number(args, span),
        "number->string" => builtin_number_to_string(args, span),
        "symbol->string" => builtin_symbol_to_string(args, span),
        "string->symbol" => builtin_string_to_symbol(args, span),
        "string-ref" => builtin_string_ref(args, span),
        "string-set!" => builtin_string_set(args, span),
        "string-copy" => builtin_string_copy(args, span),
        "char?" => Ok(Value::bool(matches!(args, [Value::Char(_, _)]))),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
            span,
        }),
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

fn builtin_display(args: &[Value], span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    env.write_output(&args[0].display_string());
    Ok(Value::Void)
}

fn builtin_write(args: &[Value], span: Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    env.write_output(&args[0].to_string());
    Ok(Value::Void)
}

fn builtin_newline(args: &[Value], span: Span, env: &Env) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "0".to_string(),
            got: args.len(),
            span,
        });
    }
    env.write_output("\n");
    Ok(Value::Void)
}

fn builtin_string_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = String::new();
    for arg in args {
        match arg {
            Value::String(s, _) => result.push_str(&s.borrow()),
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "string".to_string(),
                    got: format!("{other}"),
                    span: other.span(),
                });
            }
        }
    }
    Ok(Value::string(result))
}

fn builtin_string_length(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    match &args[0] {
        Value::String(s, _) => Ok(Value::int(s.borrow().chars().count() as i64)),
        other => Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{other}"),
            span: other.span(),
        }),
    }
}

fn builtin_substring(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            expected: "3".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(s, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let Value::Integer(start, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    let Value::Integer(end, _) = &args[2] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: format!("{}", args[2]),
            span: args[2].span(),
        });
    };
    let borrowed = s.borrow();
    let chars: Vec<char> = borrowed.chars().collect();
    let start = *start as usize;
    let end = *end as usize;
    let sub: String = chars[start..end].iter().collect();
    Ok(Value::string(sub))
}

fn builtin_string_to_number(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    match &args[0] {
        Value::String(s, _) => match s.borrow().parse::<i64>() {
            Ok(n) => Ok(Value::int(n)),
            Err(_) => Ok(Value::bool(false)),
        },
        other => Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{other}"),
            span: other.span(),
        }),
    }
}

fn builtin_number_to_string(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    match &args[0] {
        Value::Integer(n, _) => Ok(Value::string(n.to_string())),
        other => Err(EvalError::TypeMismatch {
            expected: "number".to_string(),
            got: format!("{other}"),
            span: other.span(),
        }),
    }
}

fn builtin_symbol_to_string(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    match &args[0] {
        Value::Symbol(s, _) => Ok(Value::string(s.clone())),
        other => Err(EvalError::TypeMismatch {
            expected: "symbol".to_string(),
            got: format!("{other}"),
            span: other.span(),
        }),
    }
}

fn builtin_string_to_symbol(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    match &args[0] {
        Value::String(s, _) => Ok(Value::symbol(s.borrow().clone())),
        other => Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{other}"),
            span: other.span(),
        }),
    }
}

fn builtin_string_ref(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(s, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let Value::Integer(idx, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    let c = s.borrow().chars().nth(*idx as usize).ok_or_else(|| EvalError::TypeMismatch {
        expected: "valid index".to_string(),
        got: format!("index {idx} out of bounds"),
        span,
    })?;
    Ok(Value::Char(c, Span::default()))
}

fn builtin_string_set(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            expected: "3".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(s, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    let Value::Integer(idx, _) = &args[1] else {
        return Err(EvalError::TypeMismatch {
            expected: "integer".to_string(),
            got: format!("{}", args[1]),
            span: args[1].span(),
        });
    };
    let Value::Char(c, _) = &args[2] else {
        return Err(EvalError::TypeMismatch {
            expected: "char".to_string(),
            got: format!("{}", args[2]),
            span: args[2].span(),
        });
    };
    let idx = *idx as usize;
    let mut borrowed = s.borrow_mut();
    let chars: Vec<char> = borrowed.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::TypeMismatch {
            expected: "valid index".to_string(),
            got: format!("index {idx} out of bounds"),
            span,
        });
    }
    let mut new_chars = chars;
    new_chars[idx] = *c;
    *borrowed = new_chars.into_iter().collect();
    Ok(Value::Void)
}

fn builtin_string_copy(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
            span,
        });
    }
    let Value::String(s, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "string".to_string(),
            got: format!("{}", args[0]),
            span: args[0].span(),
        });
    };
    Ok(Value::string(s.borrow().clone()))
}
