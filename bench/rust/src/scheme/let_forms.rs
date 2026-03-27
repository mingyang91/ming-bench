use super::{
    apply_proc, env_define, env_lookup, env_set, eval, new_frame, Env, EvalError, Expr, ExprKind,
    Value,
};

pub(super) fn eval_let(
    args: &[Expr],
    env: &mut Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into()));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 3 {
            return Err(EvalError::Arity(
                "named let requires bindings and body".into(),
            ));
        }
        let bindings = match &args[1].kind {
            ExprKind::List(items) => items,
            _ => return Err(EvalError::Type("let: expected bindings list".into())),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env, output)?);
                    } else {
                        return Err(EvalError::Type(
                            "let: binding name must be symbol".into(),
                        ));
                    }
                }
                _ => return Err(EvalError::Type("let: invalid binding".into())),
            }
        }
        let body = args[2..].to_vec();
        let mut let_env = env.clone();
        let frame = new_frame();
        let_env.push(frame.clone());
        let proc = Value::Procedure(params.clone(), None, body, let_env.clone());
        frame.borrow_mut().insert(name.clone(), proc);
        let func = env_lookup(&let_env, name)?;
        return apply_proc(&func, &inits, output);
    }
    // Regular let: (let ((var init) ...) body ...)
    let bindings = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("let: expected bindings list".into())),
    };
    let frame = new_frame();
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], env, output)?;
                    frame.borrow_mut().insert(s.clone(), val);
                } else {
                    return Err(EvalError::Type(
                        "let: binding name must be symbol".into(),
                    ));
                }
            }
            _ => return Err(EvalError::Type("let: invalid binding".into())),
        }
    }
    env.push(frame);
    let mut result = Value::Boolean(false);
    for expr in &args[1..] {
        result = eval(expr, env, output)?;
    }
    env.pop();
    Ok(result)
}

pub(super) fn eval_let_star(
    args: &[Expr],
    env: &mut Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let* requires bindings and body".into()));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("let*: expected bindings list".into())),
    };
    let frame = new_frame();
    env.push(frame);
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], env, output)?;
                    env_define(env, s.clone(), val);
                } else {
                    return Err(EvalError::Type(
                        "let*: binding name must be symbol".into(),
                    ));
                }
            }
            _ => return Err(EvalError::Type("let*: invalid binding".into())),
        }
    }
    let mut result = Value::Boolean(false);
    for expr in &args[1..] {
        result = eval(expr, env, output)?;
    }
    env.pop();
    Ok(result)
}

pub(super) fn eval_letrec(
    args: &[Expr],
    env: &mut Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(
            "letrec requires bindings and body".into(),
        ));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("letrec: expected bindings list".into())),
    };
    let frame = new_frame();
    env.push(frame);
    // First pass: bind all names to undefined (we use #f as placeholder)
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    env_define(env, s.clone(), Value::Boolean(false));
                } else {
                    return Err(EvalError::Type(
                        "letrec: binding name must be symbol".into(),
                    ));
                }
            }
            _ => return Err(EvalError::Type("letrec: invalid binding".into())),
        }
    }
    // Second pass: evaluate inits and set!
    for b in bindings {
        if let ExprKind::List(pair) = &b.kind {
            if let ExprKind::Symbol(s) = &pair[0].kind {
                let val = eval(&pair[1], env, output)?;
                env_set(env, s, val)?;
            }
        }
    }
    let mut result = Value::Boolean(false);
    for expr in &args[1..] {
        result = eval(expr, env, output)?;
    }
    env.pop();
    Ok(result)
}

pub(super) fn eval_letrec_star(
    args: &[Expr],
    env: &mut Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(
            "letrec* requires bindings and body".into(),
        ));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("letrec*: expected bindings list".into())),
    };
    let frame = new_frame();
    env.push(frame);
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], env, output)?;
                    env_define(env, s.clone(), val);
                } else {
                    return Err(EvalError::Type(
                        "letrec*: binding name must be symbol".into(),
                    ));
                }
            }
            _ => return Err(EvalError::Type("letrec*: invalid binding".into())),
        }
    }
    let mut result = Value::Boolean(false);
    for expr in &args[1..] {
        result = eval(expr, env, output)?;
    }
    env.pop();
    Ok(result)
}
