//! CEK machine helpers for binding forms (let, letrec, define, do, case, cond).

use super::{
    enter_body_cek, eval, CekState, Env, EvalError, Expr, ExprKind, KFrame, Span, Val,
};
use super::special_forms::{expr_to_val, parse_params, vals_eqv};

/// `(define ...)` in the CEK machine.
pub(super) fn cek_define(args: &[Expr], env: Env, span: Span, kont: &mut Vec<KFrame>) -> Result<CekState, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("define: missing arguments at {span}")));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("define: expected 2 arguments at {span}")));
            }
            kont.push(KFrame::Define { name: name.clone(), env: env.clone() });
            Ok(CekState::Eval(args[1].clone(), env))
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse(format!("define: empty signature at {span}")));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse(format!("define: expected symbol at {span}"))),
            };
            let (params, rest_param) = parse_params(&sig[1..], span)?;
            let body = args[1..].to_vec();
            let lambda = Val::Lambda { params, rest_param, body, env: env.clone() };
            env.define(name, lambda);
            Ok(CekState::ApplyK(Val::Void))
        }
        ExprKind::DottedList(sig, rest_expr) => {
            // (define (f x . rest) body...)
            if sig.is_empty() {
                return Err(EvalError::Parse(format!("define: empty signature at {span}")));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse(format!("define: expected symbol at {span}"))),
            };
            let (params, _) = parse_params(&sig[1..], span)?;
            let rest_param = match &rest_expr.kind {
                ExprKind::Symbol(s) => Some(s.clone()),
                _ => return Err(EvalError::Parse(format!("define: rest param must be symbol at {span}"))),
            };
            let body = args[1..].to_vec();
            let lambda = Val::Lambda { params, rest_param, body, env: env.clone() };
            env.define(name, lambda);
            Ok(CekState::ApplyK(Val::Void))
        }
        _ => Err(EvalError::Parse(format!("define: expected symbol or list at {span}"))),
    }
}

/// `(cond ...)` — evaluate tests via nested eval, enter matching body via CEK.
pub(super) fn cek_cond(clauses: &[Expr], env: &Env, kont: &mut Vec<KFrame>) -> Result<CekState, EvalError> {
    for clause_expr in clauses {
        let parts = match &clause_expr.kind {
            ExprKind::List(parts) if !parts.is_empty() => parts,
            _ => return Err(EvalError::Parse(format!("cond: invalid clause at {}", clause_expr.span))),
        };
        if matches!(&parts[0].kind, ExprKind::Symbol(s) if s == "else") {
            return enter_body_cek(&parts[1..], env, kont);
        }
        let test = eval(&parts[0], env)?;
        if test.is_truthy() {
            if parts.len() <= 1 {
                return Ok(CekState::ApplyK(test));
            }
            // Handle (test => proc) form: call proc with test result
            if parts.len() == 3 && matches!(&parts[1].kind, ExprKind::Symbol(s) if s == "=>") {
                let proc = eval(&parts[2], env)?;
                return Ok(CekState::ApplyK(super::apply_val(&proc, &[test], env)?));
            }
            return enter_body_cek(&parts[1..], env, kont);
        }
    }
    Ok(CekState::ApplyK(Val::Void))
}

/// `(let ...)` — handles both regular and named let.
pub(super) fn cek_let(elems: &[Expr], env: &Env, span: Span, kont: &mut Vec<KFrame>) -> Result<CekState, EvalError> {
    if elems.len() < 2 {
        return Err(EvalError::Parse(format!("let: missing arguments at {span}")));
    }
    // Named let
    if matches!(&elems[1].kind, ExprKind::Symbol(_)) {
        return cek_named_let(elems, env, span, kont);
    }
    // Regular let — evaluate bindings through the CEK machine so call/cc
    // inside init expressions captures the outer continuation.
    let new_env = env.push();
    let bindings = match &elems[1].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse(format!("let: expected bindings list at {span}"))),
    };
    let mut parsed: Vec<(String, Expr)> = Vec::new();
    for b in bindings {
        let pair = match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => pair,
            _ => return Err(EvalError::Parse(format!("let: invalid binding at {span}"))),
        };
        let name = match &pair[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Parse(format!("let: expected variable name at {span}"))),
        };
        parsed.push((name, pair[1].clone()));
    }
    let body = elems[2..].to_vec();
    if parsed.is_empty() {
        return enter_body_cek(&body, &new_env, kont);
    }
    // Reverse so we can pop from the end efficiently.
    parsed.reverse();
    let (first_name, first_init) = parsed.pop().expect("parsed confirmed non-empty above");
    kont.push(KFrame::LetBindInit {
        name: first_name,
        remaining: parsed,
        body,
        new_env,
        init_env: env.clone(),
    });
    Ok(CekState::Eval(first_init, env.clone()))
}

fn cek_named_let(elems: &[Expr], env: &Env, span: Span, kont: &mut Vec<KFrame>) -> Result<CekState, EvalError> {
    let name = match &elems[1].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => unreachable!(),
    };
    if elems.len() < 3 {
        return Err(EvalError::Parse(format!("let: missing bindings at {span}")));
    }
    let bindings = match &elems[2].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse(format!("let: expected bindings list at {span}"))),
    };
    let mut params = Vec::new();
    let mut inits = Vec::new();
    for b in bindings {
        let pair = match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => pair,
            _ => return Err(EvalError::Parse(format!("let: invalid binding at {span}"))),
        };
        let p = match &pair[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Parse(format!("let: expected variable name at {span}"))),
        };
        params.push(p);
        inits.push(eval(&pair[1], env)?);
    }
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
    enter_body_cek(&body, &new_env, kont)
}

/// `(let* ...)` — sequential bindings via nested eval, body via CEK.
pub(super) fn cek_let_star(args: &[Expr], env: &Env, span: Span, kont: &mut Vec<KFrame>) -> Result<CekState, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("let*: missing arguments at {span}")));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse(format!("let*: expected bindings list at {span}"))),
    };
    let new_env = env.push();
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], &new_env)?;
                    new_env.define(s.clone(), val);
                } else {
                    return Err(EvalError::Parse(format!("let*: expected variable name at {span}")));
                }
            }
            _ => return Err(EvalError::Parse(format!("let*: invalid binding at {span}"))),
        }
    }
    enter_body_cek(&args[1..], &new_env, kont)
}

/// `(letrec ...)` — nested eval for inits, body via CEK.
pub(super) fn cek_letrec(args: &[Expr], env: &Env, span: Span, kont: &mut Vec<KFrame>) -> Result<CekState, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("letrec: missing arguments at {span}")));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse(format!("letrec: expected bindings list at {span}"))),
    };
    let new_env = env.push();
    let mut names = Vec::new();
    let mut init_exprs = Vec::new();
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    names.push(s.clone());
                    init_exprs.push(&pair[1]);
                    new_env.define(s.clone(), Val::Void);
                } else {
                    return Err(EvalError::Parse(format!("letrec: expected variable name at {span}")));
                }
            }
            _ => return Err(EvalError::Parse(format!("letrec: invalid binding at {span}"))),
        }
    }
    for (name, init_expr) in names.iter().zip(init_exprs.iter()) {
        let val = eval(init_expr, &new_env)?;
        new_env.set(name, val)?;
    }
    enter_body_cek(&args[1..], &new_env, kont)
}

/// `(letrec* ...)` — sequential letrec, body via CEK.
pub(super) fn cek_letrec_star(args: &[Expr], env: &Env, span: Span, kont: &mut Vec<KFrame>) -> Result<CekState, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("letrec*: missing arguments at {span}")));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse(format!("letrec*: expected bindings list at {span}"))),
    };
    let new_env = env.push();
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], &new_env)?;
                    new_env.define(s.clone(), val);
                } else {
                    return Err(EvalError::Parse(format!("letrec*: expected variable name at {span}")));
                }
            }
            _ => return Err(EvalError::Parse(format!("letrec*: invalid binding at {span}"))),
        }
    }
    enter_body_cek(&args[1..], &new_env, kont)
}

/// `(do ...)` — iterative loop.
pub(super) fn cek_do(args: &[Expr], env: &Env, span: Span, _kont: &mut Vec<KFrame>) -> Result<CekState, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse(format!("do: expected at least 2 arguments at {span}")));
    }
    let var_specs = match &args[0].kind {
        ExprKind::List(v) => v,
        _ => return Err(EvalError::Parse(format!("do: expected variable list at {span}"))),
    };
    let test_clause = match &args[1].kind {
        ExprKind::List(t) => t,
        _ => return Err(EvalError::Parse(format!("do: expected test clause at {span}"))),
    };
    if test_clause.is_empty() {
        return Err(EvalError::Parse(format!("do: empty test clause at {span}")));
    }
    struct DoVar<'a> { name: String, step: Option<&'a Expr> }
    let mut vars = Vec::new();
    let do_env = env.push();
    for spec in var_specs {
        match &spec.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                let name = match &parts[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("do: expected variable name at {span}"))),
                };
                let init = eval(&parts[1], env)?;
                let step = if parts.len() >= 3 { Some(&parts[2]) } else { None };
                do_env.define(name.clone(), init);
                vars.push(DoVar { name, step });
            }
            _ => return Err(EvalError::Parse(format!("do: invalid variable spec at {span}"))),
        }
    }
    loop {
        let test_result = eval(&test_clause[0], &do_env)?;
        if test_result.is_truthy() {
            if test_clause.len() > 1 {
                let mut result = Val::Void;
                for expr in &test_clause[1..] {
                    result = eval(expr, &do_env)?;
                }
                return Ok(CekState::ApplyK(result));
            }
            return Ok(CekState::ApplyK(Val::Void));
        }
        for expr in &args[2..] {
            eval(expr, &do_env)?;
        }
        let new_vals: Vec<Option<Val>> = vars.iter().map(|v| {
            match v.step {
                Some(step_expr) => Ok(Some(eval(step_expr, &do_env)?)),
                None => Ok(None),
            }
        }).collect::<Result<_, EvalError>>()?;
        for (v, new_val) in vars.iter().zip(new_vals.into_iter()) {
            if let Some(val) = new_val {
                do_env.set(&v.name, val)?;
            }
        }
    }
}

/// `(case ...)` — nested eval for key/body.
pub(super) fn cek_case(args: &[Expr], env: &Env, span: Span, kont: &mut Vec<KFrame>) -> Result<CekState, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("case: missing arguments at {span}")));
    }
    let key = eval(&args[0], env)?;
    for clause in &args[1..] {
        match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => {
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        return enter_body_cek(&parts[1..], env, kont);
                    }
                }
                let datums = match &parts[0].kind {
                    ExprKind::List(d) => d,
                    _ => return Err(EvalError::Parse(format!("case: expected datum list at {span}"))),
                };
                for datum in datums {
                    let datum_val = expr_to_val(datum)?;
                    if vals_eqv(&key, &datum_val) {
                        return enter_body_cek(&parts[1..], env, kont);
                    }
                }
            }
            _ => return Err(EvalError::Parse(format!("case: invalid clause at {span}"))),
        }
    }
    Ok(CekState::ApplyK(Val::Void))
}
