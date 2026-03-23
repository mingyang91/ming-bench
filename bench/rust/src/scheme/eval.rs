use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

pub fn eval(expr: &Value, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Closure { .. } => {
            Ok(expr.clone())
        }
        Value::Symbol(name) => {
            if let Some(val) = env.get(name) {
                Ok(val)
            } else {
                match name.as_str() {
                    "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
                    | "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length"
                    | "append" | "pair?" | "string?" | "number?" | "boolean?"
                    | "symbol?" | "zero?" | "positive?" | "negative?"
                    | "even?" | "odd?" | "abs" | "min" | "max" | "modulo"
                    | "remainder" | "quotient" => {
                        Ok(expr.clone())
                    }
                    _ => Err(EvalError::UnboundVariable {
                        name: name.clone(),
                    }),
                }
            }
        }
        Value::List(elems) => eval_list(elems, env),
        Value::Void => Ok(Value::Void),
    }
}

fn eval_list(elems: &[Value], env: &Env) -> Result<Value, EvalError> {
    if elems.is_empty() {
        return Err(EvalError::Parse {
            message: "empty application".to_string(),
        });
    }

    let head = &elems[0];

    // Check for special forms
    if let Value::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_and(&elems[1..], env),
            "or" => return eval_or(&elems[1..], env),
            "not" => return eval_not(&elems[1..], env),
            "if" => return eval_if(&elems[1..], env),
            "define" => return eval_define(&elems[1..], env),
            "quote" => return eval_quote(&elems[1..]),
            "lambda" => return eval_lambda(&elems[1..], env),
            "let" => return eval_let(&elems[1..], env),
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

    apply(&op, &args)
}

fn apply(op: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        Value::Symbol(name) => apply_builtin(name, args),
        Value::Closure {
            params,
            body,
            env: closure_env,
        } => {
            if params.len() != args.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len().to_string(),
                    got: args.len(),
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
        }),
    }
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => arith_add(args),
        "-" => arith_sub(args),
        "*" => arith_mul(args),
        "/" => arith_div(args),
        "<" => cmp_lt(args),
        ">" => cmp_gt(args),
        "=" => cmp_eq(args),
        "<=" => cmp_le(args),
        ">=" => cmp_ge(args),
        "cons" => builtin_cons(args),
        "car" => builtin_car(args),
        "cdr" => builtin_cdr(args),
        "null?" => builtin_null(args),
        "list" => builtin_list(args),
        "length" => builtin_length(args),
        "append" => builtin_append(args),
        "pair?" => builtin_pair(args),
        "string?" => Ok(Value::Boolean(matches!(args, [Value::String(_)]))),
        "number?" => Ok(Value::Boolean(matches!(args, [Value::Integer(_)]))),
        "boolean?" => Ok(Value::Boolean(matches!(args, [Value::Boolean(_)]))),
        "symbol?" => Ok(Value::Boolean(matches!(args, [Value::Symbol(_)]))),
        "zero?" => match args {
            [Value::Integer(n)] => Ok(Value::Boolean(*n == 0)),
            _ => Err(EvalError::TypeMismatch { expected: "integer".to_string(), got: args.first().map_or("nothing", |_| "non-integer").to_string() }),
        },
        "positive?" => match args {
            [Value::Integer(n)] => Ok(Value::Boolean(*n > 0)),
            _ => Err(EvalError::TypeMismatch { expected: "integer".to_string(), got: "non-integer".to_string() }),
        },
        "negative?" => match args {
            [Value::Integer(n)] => Ok(Value::Boolean(*n < 0)),
            _ => Err(EvalError::TypeMismatch { expected: "integer".to_string(), got: "non-integer".to_string() }),
        },
        "even?" => match args {
            [Value::Integer(n)] => Ok(Value::Boolean(*n % 2 == 0)),
            _ => Err(EvalError::TypeMismatch { expected: "integer".to_string(), got: "non-integer".to_string() }),
        },
        "odd?" => match args {
            [Value::Integer(n)] => Ok(Value::Boolean(*n % 2 != 0)),
            _ => Err(EvalError::TypeMismatch { expected: "integer".to_string(), got: "non-integer".to_string() }),
        },
        "abs" => match args {
            [Value::Integer(n)] => Ok(Value::Integer(n.abs())),
            _ => Err(EvalError::TypeMismatch { expected: "integer".to_string(), got: "non-integer".to_string() }),
        },
        "min" => arith_min(args),
        "max" => arith_max(args),
        "modulo" => arith_modulo(args),
        "remainder" => arith_remainder(args),
        "quotient" => arith_div(args),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn eval_if(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::WrongArgCount {
            expected: "2 or 3".to_string(),
            got: args.len(),
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

fn eval_define(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }
    match &args[0] {
        // (define x expr)
        Value::Symbol(name) => {
            let val = eval(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body)
        Value::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse {
                    message: "empty define function name".to_string(),
                });
            }
            let Value::Symbol(name) = &elems[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "symbol".to_string(),
                    got: elems[0].to_string(),
                });
            };
            let params: Vec<String> = elems[1..]
                .iter()
                .map(|e| match e {
                    Value::Symbol(s) => Ok(s.clone()),
                    other => Err(EvalError::TypeMismatch {
                        expected: "symbol".to_string(),
                        got: other.to_string(),
                    }),
                })
                .collect::<Result<_, _>>()?;
            let body = if args.len() == 2 {
                args[1].clone()
            } else {
                Value::List(
                    std::iter::once(Value::Symbol("begin".to_string()))
                        .chain(args[1..].iter().cloned())
                        .collect(),
                )
            };
            // Capture env *by reference* (Rc clone) so the closure sees
            // itself after we define it — enabling recursion.
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
        }),
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
        });
    }
    Ok(args[0].clone())
}

fn eval_lambda(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }
    let Value::List(param_list) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "parameter list".to_string(),
            got: args[0].to_string(),
        });
    };
    let params: Vec<String> = param_list
        .iter()
        .map(|e| match e {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: other.to_string(),
            }),
        })
        .collect::<Result<_, _>>()?;
    let body = if args.len() == 2 {
        args[1].clone()
    } else {
        Value::List(
            std::iter::once(Value::Symbol("begin".to_string()))
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
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: format!("{other}"),
            }),
        })
        .collect()
}

fn arith_add(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Integer(nums.iter().sum()))
}

fn arith_sub(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "at least 1".to_string(),
            got: 0,
        });
    }
    if nums.len() == 1 {
        return Ok(Value::Integer(-nums[0]));
    }
    let result = nums[1..].iter().fold(nums[0], |acc, n| acc - n);
    Ok(Value::Integer(result))
}

fn arith_mul(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Integer(nums.iter().product()))
}

fn arith_div(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: nums.len(),
        });
    }
    let mut result = nums[0];
    for &n in &nums[1..] {
        if n == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= n;
    }
    Ok(Value::Integer(result))
}

fn cmp_lt(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| w[0] < w[1])))
}

fn cmp_gt(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| w[0] > w[1])))
}

fn cmp_eq(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| w[0] == w[1])))
}

fn cmp_le(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| w[0] <= w[1])))
}

fn eval_and(exprs: &[Value], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
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
        return Ok(Value::Boolean(false));
    }
    let mut result = Value::Boolean(false);
    for expr in exprs {
        result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_not(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
        });
    }
    let val = eval(&args[0], env)?;
    Ok(Value::Boolean(!val.is_truthy()))
}

fn eval_begin(exprs: &[Value], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in exprs {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_let(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }

    // Named let: (let name ((var init) ...) body ...)
    if let Value::Symbol(name) = &args[0] {
        let Value::List(bindings) = &args[1] else {
            return Err(EvalError::TypeMismatch {
                expected: "binding list".to_string(),
                got: args[1].to_string(),
            });
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for binding in bindings {
            let Value::List(pair) = binding else {
                return Err(EvalError::TypeMismatch {
                    expected: "binding pair".to_string(),
                    got: binding.to_string(),
                });
            };
            if pair.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    expected: "2".to_string(),
                    got: pair.len(),
                });
            }
            let Value::Symbol(var) = &pair[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "symbol".to_string(),
                    got: pair[0].to_string(),
                });
            };
            params.push(var.clone());
            inits.push(eval(&pair[1], env)?);
        }
        let body = if args.len() == 3 {
            args[2].clone()
        } else {
            Value::List(
                std::iter::once(Value::Symbol("begin".to_string()))
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
        let body_exprs = &args[2..];
        let mut result = Value::Void;
        for expr in body_exprs {
            result = eval(expr, &local_env)?;
        }
        return Ok(result);
    }

    // Regular let: (let ((var init) ...) body ...)
    let Value::List(bindings) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "binding list".to_string(),
            got: args[0].to_string(),
        });
    };
    let local_env = Env::with_parent(env);
    for binding in bindings {
        let Value::List(pair) = binding else {
            return Err(EvalError::TypeMismatch {
                expected: "binding pair".to_string(),
                got: binding.to_string(),
            });
        };
        if pair.len() != 2 {
            return Err(EvalError::WrongArgCount {
                expected: "2".to_string(),
                got: pair.len(),
            });
        }
        let Value::Symbol(var) = &pair[0] else {
            return Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: pair[0].to_string(),
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
        let Value::List(parts) = clause else {
            return Err(EvalError::TypeMismatch {
                expected: "cond clause".to_string(),
                got: clause.to_string(),
            });
        };
        if parts.is_empty() {
            return Err(EvalError::Parse {
                message: "empty cond clause".to_string(),
            });
        }
        // else clause
        if let Value::Symbol(s) = &parts[0] {
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
    Ok(Value::Boolean(nums.windows(2).all(|w| w[0] >= w[1])))
}

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: args.len(),
        });
    }
    match &args[1] {
        Value::List(elems) => {
            let mut new_list = vec![args[0].clone()];
            new_list.extend(elems.iter().cloned());
            Ok(Value::List(new_list))
        }
        _ => {
            // Improper pair — store as 2-element tagged structure for now
            Ok(Value::List(vec![args[0].clone(), Value::Symbol(".".to_string()), args[1].clone()]))
        }
    }
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
        });
    }
    match &args[0] {
        Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
        _ => Err(EvalError::TypeMismatch {
            expected: "pair".to_string(),
            got: args[0].to_string(),
        }),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
        });
    }
    match &args[0] {
        Value::List(elems) if !elems.is_empty() => {
            Ok(Value::List(elems[1..].to_vec()))
        }
        _ => Err(EvalError::TypeMismatch {
            expected: "pair".to_string(),
            got: args[0].to_string(),
        }),
    }
}

fn builtin_null(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
        });
    }
    Ok(Value::Boolean(matches!(&args[0], Value::List(elems) if elems.is_empty())))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
        });
    }
    match &args[0] {
        Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
        _ => Err(EvalError::TypeMismatch {
            expected: "list".to_string(),
            got: args[0].to_string(),
        }),
    }
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        if i == args.len() - 1 {
            // Last argument can be any value (for improper lists), but for proper lists:
            match arg {
                Value::List(elems) => result.extend(elems.iter().cloned()),
                other => {
                    if result.is_empty() {
                        return Ok(other.clone());
                    }
                    result.push(other.clone());
                }
            }
        } else {
            match arg {
                Value::List(elems) => result.extend(elems.iter().cloned()),
                _ => {
                    return Err(EvalError::TypeMismatch {
                        expected: "list".to_string(),
                        got: arg.to_string(),
                    });
                }
            }
        }
    }
    Ok(Value::List(result))
}

fn builtin_pair(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
        });
    }
    Ok(Value::Boolean(matches!(&args[0], Value::List(elems) if !elems.is_empty())))
}

fn arith_min(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    nums.iter().copied().min().map(Value::Integer).ok_or(EvalError::WrongArgCount {
        expected: "at least 1".to_string(),
        got: 0,
    })
}

fn arith_max(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    nums.iter().copied().max().map(Value::Integer).ok_or(EvalError::WrongArgCount {
        expected: "at least 1".to_string(),
        got: 0,
    })
}

fn arith_modulo(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: nums.len(),
        });
    }
    if nums[1] == 0 {
        return Err(EvalError::DivisionByZero);
    }
    Ok(Value::Integer(((nums[0] % nums[1]) + nums[1]) % nums[1]))
}

fn arith_remainder(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".to_string(),
            got: nums.len(),
        });
    }
    if nums[1] == 0 {
        return Err(EvalError::DivisionByZero);
    }
    Ok(Value::Integer(nums[0] % nums[1]))
}
