use super::types::{Env, Value};

enum Trampoline {
    Done(Value),
    Bounce { expr: Value, env: Env },
}

pub fn eval(expr: &Value, env: &Env) -> Result<Value, String> {
    let mut current_expr = expr.clone();
    let mut current_env = env.clone();

    loop {
        match eval_inner(&current_expr, &current_env)? {
            Trampoline::Done(val) => return Ok(val),
            Trampoline::Bounce { expr, env } => {
                current_expr = expr;
                current_env = env;
            }
        }
    }
}

fn eval_inner(expr: &Value, env: &Env) -> Result<Trampoline, String> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(Trampoline::Done(expr.clone())),
        Value::Symbol(name) => env
            .get(name)
            .or_else(|| resolve_builtin(name))
            .map(Trampoline::Done)
            .ok_or_else(|| format!("unbound variable: {name}")),
        Value::List(elems) => eval_list(elems, env),
        _ => Err(format!("cannot evaluate: {expr}")),
    }
}

fn eval_list(elems: &[Value], env: &Env) -> Result<Trampoline, String> {
    if elems.is_empty() {
        return Err("empty application".into());
    }
    if let Value::Symbol(s) = &elems[0] {
        match s.as_str() {
            "define" => return eval_define(&elems[1..], env).map(Trampoline::Done),
            "if" => return eval_if(&elems[1..], env),
            "quote" => return eval_quote(&elems[1..]).map(Trampoline::Done),
            "and" => return eval_and(&elems[1..], env),
            "or" => return eval_or(&elems[1..], env),
            "lambda" => return eval_lambda(&elems[1..], env).map(Trampoline::Done),
            "begin" => return eval_begin(&elems[1..], env),
            "let" => return eval_let(&elems[1..], env),
            "cond" => return eval_cond(&elems[1..], env),
            "set!" => return eval_set(&elems[1..], env).map(Trampoline::Done),
            _ => {}
        }
    }
    eval_call(elems, env)
}

fn eval_call(elems: &[Value], env: &Env) -> Result<Trampoline, String> {
    if let Value::Symbol(name) = &elems[0] {
        let args = eval_args(&elems[1..], env)?;
        return match env.get(name) {
            Some(proc) => apply_tco(&proc, &args),
            None => apply_builtin(name, &args),
        };
    }
    let proc = eval(&elems[0], env)?;
    let args = eval_args(&elems[1..], env)?;
    apply_tco(&proc, &args)
}

fn eval_args(exprs: &[Value], env: &Env) -> Result<Vec<Value>, String> {
    exprs.iter().map(|e| eval(e, env)).collect()
}

/// TCO apply: returns Bounce for the lambda body instead of evaluating it.
fn apply_tco(proc: &Value, args: &[Value]) -> Result<Trampoline, String> {
    match proc {
        Value::Lambda {
            params,
            rest_param,
            body,
            env,
        } => {
            validate_arity(params.len(), rest_param.is_some(), args.len())?;
            let child = bind_args(env, params, rest_param.as_deref(), args);
            Ok(Trampoline::Bounce {
                expr: *body.clone(),
                env: child,
            })
        }
        Value::Builtin { name } => apply_builtin(name, args),
        other => Err(format!("not a procedure: {other}")),
    }
}

fn validate_arity(expected: usize, variadic: bool, actual: usize) -> Result<(), String> {
    if variadic {
        if actual < expected {
            return Err(format!("expected at least {expected} arguments, got {actual}"));
        }
    } else if expected != actual {
        return Err(format!("expected {expected} arguments, got {actual}"));
    }
    Ok(())
}

fn bind_args(env: &Env, params: &[String], rest_param: Option<&str>, args: &[Value]) -> Env {
    let child = env.child();
    for (param, arg) in params.iter().zip(args) {
        child.set(param.clone(), arg.clone());
    }
    if let Some(rest_name) = rest_param {
        child.set(rest_name.to_string(), Value::List(args[params.len()..].to_vec()));
    }
    child
}

// ── Special forms ────────────────────────────────────────────────────

fn eval_define(args: &[Value], env: &Env) -> Result<Value, String> {
    if args.len() < 2 {
        return Err(format!("define requires at least 2 arguments, got {}", args.len()));
    }
    match &args[0] {
        Value::Symbol(name) => {
            let val = eval(&args[1], env)?;
            env.set(name.clone(), val);
            Ok(Value::Void)
        }
        Value::List(elems) if !elems.is_empty() => {
            let name = match &elems[0] {
                Value::Symbol(s) => s.clone(),
                other => return Err(format!("define: expected symbol, got {other}")),
            };
            let (params, rest_param) = extract_params(&elems[1..])?;
            let body = wrap_body(&args[1..]);
            let lambda = Value::Lambda {
                params,
                rest_param,
                body: Box::new(body),
                env: env.clone(),
            };
            env.set(name, lambda);
            Ok(Value::Void)
        }
        other => Err(format!("define: expected symbol or list, got {other}")),
    }
}

fn eval_lambda(args: &[Value], env: &Env) -> Result<Value, String> {
    if args.len() < 2 {
        return Err(format!("lambda requires at least 2 arguments, got {}", args.len()));
    }
    let (params, rest_param) = match &args[0] {
        Value::List(elems) => extract_params(elems)?,
        other => return Err(format!("lambda: expected parameter list, got {other}")),
    };
    let body = wrap_body(&args[1..]);
    Ok(Value::Lambda {
        params,
        rest_param,
        body: Box::new(body),
        env: env.clone(),
    })
}

fn wrap_body(exprs: &[Value]) -> Value {
    if exprs.len() == 1 {
        exprs[0].clone()
    } else {
        let mut elems = vec![Value::Symbol("begin".into())];
        elems.extend(exprs.iter().cloned());
        Value::List(elems)
    }
}

fn extract_params(elems: &[Value]) -> Result<(Vec<String>, Option<String>), String> {
    // Look for dot notation: (a b . rest)
    if let Some(dot_pos) = elems.iter().position(|e| matches!(e, Value::Symbol(s) if s == ".")) {
        if dot_pos + 2 != elems.len() {
            return Err("malformed dotted parameter list".into());
        }
        let params = elems[..dot_pos]
            .iter()
            .map(|e| match e {
                Value::Symbol(s) => Ok(s.clone()),
                other => Err(format!("expected parameter name, got {other}")),
            })
            .collect::<Result<Vec<_>, _>>()?;
        let rest = match &elems[dot_pos + 1] {
            Value::Symbol(s) => s.clone(),
            other => return Err(format!("expected parameter name, got {other}")),
        };
        Ok((params, Some(rest)))
    } else {
        let params = elems
            .iter()
            .map(|e| match e {
                Value::Symbol(s) => Ok(s.clone()),
                other => Err(format!("expected parameter name, got {other}")),
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok((params, None))
    }
}

fn eval_if(args: &[Value], env: &Env) -> Result<Trampoline, String> {
    if args.len() < 2 || args.len() > 3 {
        return Err(format!("if requires 2-3 arguments, got {}", args.len()));
    }
    let cond = eval(&args[0], env)?;
    if is_truthy(&cond) {
        Ok(Trampoline::Bounce {
            expr: args[1].clone(),
            env: env.clone(),
        })
    } else if args.len() == 3 {
        Ok(Trampoline::Bounce {
            expr: args[2].clone(),
            env: env.clone(),
        })
    } else {
        Ok(Trampoline::Done(Value::Void))
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err(format!("quote requires 1 argument, got {}", args.len()));
    }
    Ok(args[0].clone())
}

fn eval_and(exprs: &[Value], env: &Env) -> Result<Trampoline, String> {
    if exprs.is_empty() {
        return Ok(Trampoline::Done(Value::Boolean(true)));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let result = eval(expr, env)?;
        if !is_truthy(&result) {
            return Ok(Trampoline::Done(result));
        }
    }
    Ok(Trampoline::Bounce {
        expr: exprs[exprs.len() - 1].clone(),
        env: env.clone(),
    })
}

fn eval_or(exprs: &[Value], env: &Env) -> Result<Trampoline, String> {
    if exprs.is_empty() {
        return Ok(Trampoline::Done(Value::Boolean(false)));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let result = eval(expr, env)?;
        if is_truthy(&result) {
            return Ok(Trampoline::Done(result));
        }
    }
    Ok(Trampoline::Bounce {
        expr: exprs[exprs.len() - 1].clone(),
        env: env.clone(),
    })
}

fn eval_begin(exprs: &[Value], env: &Env) -> Result<Trampoline, String> {
    if exprs.is_empty() {
        return Ok(Trampoline::Done(Value::Void));
    }
    for expr in &exprs[..exprs.len() - 1] {
        eval(expr, env)?;
    }
    Ok(Trampoline::Bounce {
        expr: exprs[exprs.len() - 1].clone(),
        env: env.clone(),
    })
}

fn eval_let(args: &[Value], env: &Env) -> Result<Trampoline, String> {
    if args.len() < 2 {
        return Err(format!("let requires at least 2 arguments, got {}", args.len()));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let Value::Symbol(name) = &args[0] {
        return eval_named_let(name, &args[1..], env);
    }
    let bindings = match &args[0] {
        Value::List(elems) => elems,
        other => return Err(format!("let: expected bindings list, got {other}")),
    };
    let child = env.child();
    for binding in bindings {
        let pair = match binding {
            Value::List(elems) if elems.len() == 2 => elems,
            _ => return Err(format!("let: invalid binding: {binding}")),
        };
        let name = match &pair[0] {
            Value::Symbol(s) => s.clone(),
            other => return Err(format!("let: expected symbol, got {other}")),
        };
        let val = eval(&pair[1], env)?;
        child.set(name, val);
    }
    let body = wrap_body(&args[1..]);
    Ok(Trampoline::Bounce {
        expr: body,
        env: child,
    })
}

fn eval_named_let(name: &str, args: &[Value], env: &Env) -> Result<Trampoline, String> {
    if args.len() < 2 {
        return Err(format!("named let requires bindings and body, got {} args", args.len()));
    }
    let binding_list = match &args[0] {
        Value::List(elems) => elems,
        other => return Err(format!("named let: expected bindings list, got {other}")),
    };
    let mut params = Vec::new();
    let mut init_vals = Vec::new();
    for binding in binding_list {
        let pair = match binding {
            Value::List(elems) if elems.len() == 2 => elems,
            _ => return Err(format!("named let: invalid binding: {binding}")),
        };
        match &pair[0] {
            Value::Symbol(s) => params.push(s.clone()),
            other => return Err(format!("named let: expected symbol, got {other}")),
        }
        init_vals.push(eval(&pair[1], env)?);
    }
    let body = wrap_body(&args[1..]);
    // Create env where the lambda can see itself for recursion
    let loop_env = env.child();
    let lambda = Value::Lambda {
        params: params.clone(),
        rest_param: None,
        body: Box::new(body.clone()),
        env: loop_env.clone(),
    };
    loop_env.set(name.to_string(), lambda);
    // Bind initial values and bounce into the body
    let call_env = loop_env.child();
    for (param, val) in params.iter().zip(init_vals) {
        call_env.set(param.clone(), val);
    }
    Ok(Trampoline::Bounce {
        expr: body,
        env: call_env,
    })
}

fn eval_set(args: &[Value], env: &Env) -> Result<Value, String> {
    if args.len() != 2 {
        return Err(format!("set! requires 2 arguments, got {}", args.len()));
    }
    let name = match &args[0] {
        Value::Symbol(s) => s,
        other => return Err(format!("set!: expected symbol, got {other}")),
    };
    let val = eval(&args[1], env)?;
    if env.update(name, val) {
        Ok(Value::Void)
    } else {
        Err(format!("set!: unbound variable: {name}"))
    }
}

fn eval_cond(clauses: &[Value], env: &Env) -> Result<Trampoline, String> {
    for clause in clauses {
        let elems = match clause {
            Value::List(elems) if !elems.is_empty() => elems,
            _ => return Err(format!("cond: invalid clause: {clause}")),
        };
        if matches!(&elems[0], Value::Symbol(s) if s == "else") {
            return eval_begin(&elems[1..], env);
        }
        let test = eval(&elems[0], env)?;
        if !is_truthy(&test) {
            continue;
        }
        if elems[1..].is_empty() {
            return Ok(Trampoline::Done(test));
        }
        return eval_begin(&elems[1..], env);
    }
    Ok(Trampoline::Done(Value::Void))
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

// ── Primitives ───────────────────────────────────────────────────────

fn apply_primitive(op: &str, args: &[Value]) -> Result<Value, String> {
    match op {
        "cons" | "car" | "cdr" | "null?" | "list" | "length" => apply_list_primitive(op, args),
        "boolean?" | "number?" | "pair?" | "string?" | "symbol?" => {
            if args.len() != 1 {
                return Err(format!("{op} requires 1 argument, got {}", args.len()));
            }
            Ok(Value::Boolean(match op {
                "boolean?" => matches!(args[0], Value::Boolean(_)),
                "number?" => matches!(args[0], Value::Integer(_)),
                "pair?" => matches!(&args[0], Value::List(e) if !e.is_empty()),
                "string?" => matches!(args[0], Value::String(_)),
                "symbol?" => matches!(args[0], Value::Symbol(_)),
                _ => unreachable!(),
            }))
        }
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

const BUILTINS: &[&str] = &[
    "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "cons", "car", "cdr", "null?", "list",
    "length", "boolean?", "number?", "pair?", "string?", "symbol?", "not", "apply",
];

fn resolve_builtin(name: &str) -> Option<Value> {
    if BUILTINS.contains(&name) {
        Some(Value::Builtin {
            name: name.to_string(),
        })
    } else {
        None
    }
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Trampoline, String> {
    if name == "apply" {
        return builtin_apply(args);
    }
    apply_primitive(name, args).map(Trampoline::Done)
}

fn builtin_apply(args: &[Value]) -> Result<Trampoline, String> {
    if args.len() < 2 {
        return Err(format!("apply requires at least 2 arguments, got {}", args.len()));
    }
    let proc = &args[0];
    let last = match &args[args.len() - 1] {
        Value::List(elems) => elems.clone(),
        other => return Err(format!("apply: last argument must be a list, got {other}")),
    };
    let mut combined = args[1..args.len() - 1].to_vec();
    combined.extend(last);
    apply_tco(proc, &combined)
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
