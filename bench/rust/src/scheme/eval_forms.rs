//! TCO-aware special form handlers extracted from the main eval loop.
//!
//! Each function returns `Tco::Tail(expr, env)` when the caller should
//! continue the trampoline, or `Tco::Done(val)` for an immediate result.

use super::{eval, vec_to_cons, Env, EvalError, Expr, ExprKind, Span, Val};

/// Signals whether the eval trampoline should continue or return.
pub(crate) enum Tco {
    Done(Val),
    Tail(Expr, Env),
}

/// Dispatch a TCO special form by name.
pub(crate) fn dispatch_tco_form(
    op: &str,
    elems: &mut Vec<Expr>,
    env: &Env,
    span: Span,
) -> Result<Tco, EvalError> {
    match op {
        "begin" => eval_begin(elems, env),
        "and" => eval_and(elems, env),
        "or" => eval_or(elems, env),
        "cond" => eval_cond(elems, env),
        "let" => eval_let(elems, env, span),
        _ => unreachable!(),
    }
}

/// `(begin expr ...)`
pub(crate) fn eval_begin(
    elems: &mut Vec<Expr>,
    env: &Env,
) -> Result<Tco, EvalError> {
    let len = elems.len();
    if len <= 1 {
        return Ok(Tco::Done(Val::Void));
    }
    for elem in elems.iter().take(len - 1).skip(1) {
        eval(elem, env)?;
    }
    Ok(Tco::Tail(elems.swap_remove(len - 1), env.clone()))
}

/// `(and expr ...)`
pub(crate) fn eval_and(
    elems: &mut Vec<Expr>,
    env: &Env,
) -> Result<Tco, EvalError> {
    let len = elems.len();
    if len <= 1 {
        return Ok(Tco::Done(Val::Bool(true)));
    }
    for elem in elems.iter().take(len - 1).skip(1) {
        let v = eval(elem, env)?;
        if !v.is_truthy() {
            return Ok(Tco::Done(v));
        }
    }
    Ok(Tco::Tail(elems.swap_remove(len - 1), env.clone()))
}

/// `(or expr ...)`
pub(crate) fn eval_or(
    elems: &mut Vec<Expr>,
    env: &Env,
) -> Result<Tco, EvalError> {
    let len = elems.len();
    if len <= 1 {
        return Ok(Tco::Done(Val::Bool(false)));
    }
    for elem in elems.iter().take(len - 1).skip(1) {
        let v = eval(elem, env)?;
        if v.is_truthy() {
            return Ok(Tco::Done(v));
        }
    }
    Ok(Tco::Tail(elems.swap_remove(len - 1), env.clone()))
}

/// `(cond clause ...)`
pub(crate) fn eval_cond(
    elems: &[Expr],
    env: &Env,
) -> Result<Tco, EvalError> {
    for clause_expr in &elems[1..] {
        let parts = match &clause_expr.kind {
            ExprKind::List(parts) if !parts.is_empty() => parts,
            _ => {
                return Err(EvalError::Parse(format!(
                    "cond: invalid clause at {}",
                    clause_expr.span
                )))
            }
        };

        // Check for else clause
        if matches!(&parts[0].kind, ExprKind::Symbol(s) if s == "else") {
            return eval_cond_body(&parts[1..], env);
        }

        let test = eval(&parts[0], env)?;
        if test.is_truthy() {
            if parts.len() <= 1 {
                return Ok(Tco::Done(test));
            }
            return eval_cond_body(&parts[1..], env);
        }
    }
    Ok(Tco::Done(Val::Void))
}

fn eval_cond_body(body: &[Expr], env: &Env) -> Result<Tco, EvalError> {
    if body.is_empty() {
        return Ok(Tco::Done(Val::Void));
    }
    for e in &body[..body.len() - 1] {
        eval(e, env)?;
    }
    let tail = body.last().expect("body verified non-empty").clone();
    Ok(Tco::Tail(tail, env.clone()))
}

/// `(let ...)` — handles both regular and named let.
pub(crate) fn eval_let(
    elems: &mut Vec<Expr>,
    env: &Env,
    span: Span,
) -> Result<Tco, EvalError> {
    if elems.len() < 2 {
        return Err(EvalError::Parse(format!(
            "let: missing arguments at {span}"
        )));
    }

    // Named let: (let name ((var init) ...) body ...)
    if matches!(&elems[1].kind, ExprKind::Symbol(_)) {
        return eval_named_let(elems, env, span);
    }

    // Regular let: (let ((var init) ...) body ...)
    eval_regular_let(elems, env, span)
}

fn eval_named_let(
    elems: &[Expr],
    env: &Env,
    span: Span,
) -> Result<Tco, EvalError> {
    let name = match &elems[1].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => unreachable!(),
    };
    if elems.len() < 3 {
        return Err(EvalError::Parse(format!(
            "let: missing bindings at {span}"
        )));
    }
    let (params, inits) = parse_let_bindings(&elems[2], env, span)?;
    let body: Vec<Expr> = elems[3..].to_vec();
    let new_env = env.push();
    let lambda = Val::Lambda {
        params: params.clone(),
        rest_param: None,
        body: body.clone(),
        env: new_env.clone(),
    };
    new_env.define(name, lambda);
    for (p, v) in params.iter().zip(inits.iter()) {
        new_env.define(p.clone(), v.clone());
    }
    if body.is_empty() {
        return Ok(Tco::Done(Val::Void));
    }
    for e in &body[..body.len() - 1] {
        eval(e, &new_env)?;
    }
    let tail = body.into_iter().last().expect("body verified non-empty");
    Ok(Tco::Tail(tail, new_env))
}

fn parse_let_bindings(
    bindings_expr: &Expr,
    env: &Env,
    span: Span,
) -> Result<(Vec<String>, Vec<Val>), EvalError> {
    let bindings = match &bindings_expr.kind {
        ExprKind::List(b) => b,
        _ => {
            return Err(EvalError::Parse(format!(
                "let: expected bindings list at {span}"
            )))
        }
    };
    let mut params = Vec::new();
    let mut inits = Vec::new();
    for b in bindings {
        let pair = match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => pair,
            _ => {
                return Err(EvalError::Parse(format!(
                    "let: invalid binding at {span}"
                )))
            }
        };
        let name = match &pair[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => {
                return Err(EvalError::Parse(format!(
                    "let: expected variable name at {span}"
                )))
            }
        };
        params.push(name);
        inits.push(eval(&pair[1], env)?);
    }
    Ok((params, inits))
}

fn eval_regular_let(
    elems: &mut Vec<Expr>,
    env: &Env,
    span: Span,
) -> Result<Tco, EvalError> {
    let new_env = env.push();
    let bindings = match &elems[1].kind {
        ExprKind::List(b) => b,
        _ => {
            return Err(EvalError::Parse(format!(
                "let: expected bindings list at {span}"
            )))
        }
    };
    for b in bindings {
        let pair = match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => pair,
            _ => {
                return Err(EvalError::Parse(format!(
                    "let: invalid binding at {span}"
                )))
            }
        };
        let name = match &pair[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => {
                return Err(EvalError::Parse(format!(
                    "let: expected variable name at {span}"
                )))
            }
        };
        let val = eval(&pair[1], env)?;
        new_env.define(name, val);
    }
    if elems.len() <= 2 {
        return Ok(Tco::Done(Val::Void));
    }
    let tail_idx = elems.len() - 1;
    for elem in elems.iter().take(tail_idx).skip(2) {
        eval(elem, &new_env)?;
    }
    Ok(Tco::Tail(elems.swap_remove(tail_idx), new_env))
}

/// Apply a lambda body with TCO: eval all but the last expr, return last as tail.
pub(crate) fn eval_body_tco(
    body: Vec<Expr>,
    env: &Env,
) -> Result<Tco, EvalError> {
    if body.is_empty() {
        return Ok(Tco::Done(Val::Void));
    }
    for e in &body[..body.len() - 1] {
        eval(e, env)?;
    }
    let tail = body.into_iter().last().expect("body verified non-empty");
    Ok(Tco::Tail(tail, env.clone()))
}

/// Bind lambda params and rest param into a new environment frame.
pub(crate) fn bind_lambda_args(
    params: &[String],
    rest_param: &Option<String>,
    args: &[Val],
    lambda_env: &Env,
    span: Span,
) -> Result<Env, EvalError> {
    if let Some(ref _rest) = rest_param {
        if args.len() < params.len() {
            return Err(EvalError::Arity(format!(
                "expected at least {} arguments, got {} at {span}",
                params.len(),
                args.len()
            )));
        }
    } else if args.len() != params.len() {
        return Err(EvalError::Arity(format!(
            "expected {} arguments, got {} at {span}",
            params.len(),
            args.len()
        )));
    }
    let new_env = lambda_env.push();
    for (p, a) in params.iter().zip(args.iter()) {
        new_env.define(p.clone(), a.clone());
    }
    if let Some(rest) = rest_param {
        new_env.define(rest.clone(), vec_to_cons(args[params.len()..].to_vec()));
    }
    Ok(new_env)
}

/// Dispatch a case-lambda call: find the matching clause and apply it with TCO.
pub(crate) fn apply_case_lambda(
    clauses: Vec<(Vec<String>, Option<String>, Vec<Expr>)>,
    args: &[Val],
    lambda_env: &Env,
    span: Span,
) -> Result<Tco, EvalError> {
    for (params, rest_param, body) in clauses {
        let matches = if rest_param.is_some() {
            args.len() >= params.len()
        } else {
            args.len() == params.len()
        };
        if matches {
            let new_env = bind_lambda_args(&params, &rest_param, args, lambda_env, span)?;
            return eval_body_tco(body, &new_env);
        }
    }
    Err(EvalError::Arity(format!(
        "no matching clause for {} arguments",
        args.len()
    )))
}
