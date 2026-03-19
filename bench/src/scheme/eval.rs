use super::types::{Env, Value};

pub fn eval(expr: &Value, env: &Env) -> Result<Value, String> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(expr.clone()),
        Value::Symbol(name) => env
            .get(name)
            .ok_or_else(|| format!("unbound variable: {name}")),
        Value::List(elems) => eval_list(elems, env),
        _ => Err(format!("cannot evaluate: {expr}")),
    }
}

fn eval_list(elems: &[Value], env: &Env) -> Result<Value, String> {
    if elems.is_empty() {
        return Err("empty application".into());
    }
    // Check for special forms
    if let Value::Symbol(s) = &elems[0] {
        match s.as_str() {
            "define" => return eval_define(&elems[1..], env),
            "if" => return eval_if(&elems[1..], env),
            "quote" => return eval_quote(&elems[1..]),
            "and" => return eval_and(&elems[1..], env),
            "or" => return eval_or(&elems[1..], env),
            "lambda" => return eval_lambda(&elems[1..], env),
            _ => {}
        }
    }
    // Procedure call
    eval_call(elems, env)
}

fn eval_call(elems: &[Value], env: &Env) -> Result<Value, String> {
    // If operator is a symbol, try env first, then primitive fallback
    if let Value::Symbol(name) = &elems[0] {
        let args = eval_args(&elems[1..], env)?;
        return match env.get(name) {
            Some(proc) => apply(&proc, &args),
            None => apply_primitive(name, &args),
        };
    }
    // Operator is an expression (e.g. a lambda form) — evaluate it
    let proc = eval(&elems[0], env)?;
    let args = eval_args(&elems[1..], env)?;
    apply(&proc, &args)
}

fn eval_args(exprs: &[Value], env: &Env) -> Result<Vec<Value>, String> {
    exprs.iter().map(|e| eval(e, env)).collect()
}

fn apply(proc: &Value, args: &[Value]) -> Result<Value, String> {
    match proc {
        Value::Lambda { params, body, env } => {
            if params.len() != args.len() {
                return Err(format!(
                    "expected {} arguments, got {}",
                    params.len(),
                    args.len()
                ));
            }
            let child = env.child();
            for (param, arg) in params.iter().zip(args) {
                child.set(param.clone(), arg.clone());
            }
            eval(body, &child)
        }
        other => Err(format!("not a procedure: {other}")),
    }
}

// ── Special forms ────────────────────────────────────────────────────

fn eval_define(args: &[Value], env: &Env) -> Result<Value, String> {
    if args.len() != 2 {
        return Err(format!("define requires 2 arguments, got {}", args.len()));
    }
    match &args[0] {
        Value::Symbol(name) => {
            let val = eval(&args[1], env)?;
            env.set(name.clone(), val);
            Ok(Value::Void)
        }
        Value::List(elems) if !elems.is_empty() => {
            // (define (name params...) body) sugar
            let name = match &elems[0] {
                Value::Symbol(s) => s.clone(),
                other => return Err(format!("define: expected symbol, got {other}")),
            };
            let params = extract_params(&elems[1..])?;
            let lambda = Value::Lambda {
                params,
                body: Box::new(args[1].clone()),
                env: env.clone(),
            };
            env.set(name, lambda);
            Ok(Value::Void)
        }
        other => Err(format!("define: expected symbol or list, got {other}")),
    }
}

fn eval_lambda(args: &[Value], env: &Env) -> Result<Value, String> {
    if args.len() != 2 {
        return Err(format!("lambda requires 2 arguments, got {}", args.len()));
    }
    let params = match &args[0] {
        Value::List(elems) => extract_params(elems)?,
        other => return Err(format!("lambda: expected parameter list, got {other}")),
    };
    Ok(Value::Lambda {
        params,
        body: Box::new(args[1].clone()),
        env: env.clone(),
    })
}

fn extract_params(elems: &[Value]) -> Result<Vec<String>, String> {
    elems
        .iter()
        .map(|e| match e {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(format!("expected parameter name, got {other}")),
        })
        .collect()
}

fn eval_if(args: &[Value], env: &Env) -> Result<Value, String> {
    if args.len() < 2 || args.len() > 3 {
        return Err(format!("if requires 2-3 arguments, got {}", args.len()));
    }
    let cond = eval(&args[0], env)?;
    if is_truthy(&cond) {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err(format!("quote requires 1 argument, got {}", args.len()));
    }
    Ok(args[0].clone())
}

fn eval_and(exprs: &[Value], env: &Env) -> Result<Value, String> {
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Value], env: &Env) -> Result<Value, String> {
    let mut result = Value::Boolean(false);
    for expr in exprs {
        result = eval(expr, env)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

// ── Primitives ───────────────────────────────────────────────────────

fn apply_primitive(op: &str, args: &[Value]) -> Result<Value, String> {
    match op {
        "cons" | "car" | "cdr" | "null?" | "list" | "length" => apply_list_primitive(op, args),
        "not" => {
            if args.len() != 1 {
                return Err(format!("not requires 1 argument, got {}", args.len()));
            }
            Ok(Value::Boolean(!is_truthy(&args[0])))
        }
        "<" | ">" | "=" | "<=" | ">=" => {
            let nums = require_nums(args)?;
            if nums.len() != 2 {
                return Err(format!("{op} requires 2 arguments, got {}", nums.len()));
            }
            let result = match op {
                "<" => nums[0] < nums[1],
                ">" => nums[0] > nums[1],
                "=" => nums[0] == nums[1],
                "<=" => nums[0] <= nums[1],
                ">=" => nums[0] >= nums[1],
                _ => unreachable!(),
            };
            Ok(Value::Boolean(result))
        }
        _ => {
            let nums = require_nums(args)?;
            match op {
                "+" => Ok(Value::Integer(nums.iter().sum())),
                "-" => match nums.len() {
                    0 => Err("- requires at least 1 argument".into()),
                    1 => Ok(Value::Integer(-nums[0])),
                    _ => Ok(Value::Integer(nums[0] - nums[1..].iter().sum::<i64>())),
                },
                "*" => Ok(Value::Integer(nums.iter().product())),
                "/" => checked_div(&nums),
                _ => Err(format!("unknown procedure: {op}")),
            }
        }
    }
}

fn require_nums(args: &[Value]) -> Result<Vec<i64>, String> {
    args.iter()
        .map(|v| match v {
            Value::Integer(n) => Ok(*n),
            other => Err(format!("expected number, got: {other}")),
        })
        .collect()
}

fn apply_list_primitive(op: &str, args: &[Value]) -> Result<Value, String> {
    match op {
        "cons" => {
            if args.len() != 2 {
                return Err(format!("cons requires 2 arguments, got {}", args.len()));
            }
            match &args[1] {
                Value::List(elems) => {
                    let mut new = vec![args[0].clone()];
                    new.extend(elems.iter().cloned());
                    Ok(Value::List(new))
                }
                _ => Err(format!("cons: second argument must be a list, got {}", args[1])),
            }
        }
        "car" => match &args[0] {
            Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
            _ => Err(format!("car: expected non-empty list, got {}", args[0])),
        },
        "cdr" => match &args[0] {
            Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
            _ => Err(format!("cdr: expected non-empty list, got {}", args[0])),
        },
        "null?" => {
            Ok(Value::Boolean(matches!(&args[0], Value::List(elems) if elems.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => match &args[0] {
            Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
            _ => Err(format!("length: expected list, got {}", args[0])),
        },
        _ => unreachable!(),
    }
}

fn checked_div(nums: &[i64]) -> Result<Value, String> {
    match nums.len() {
        0 => Err("/ requires at least 1 argument".into()),
        1 => Ok(Value::Integer(1 / nums[0])),
        _ => nums[1..]
            .iter()
            .try_fold(nums[0], |acc, &n| {
                if n == 0 {
                    Err("division by zero".into())
                } else {
                    Ok(acc / n)
                }
            })
            .map(Value::Integer),
    }
}
