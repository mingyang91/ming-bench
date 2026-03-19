use super::builtins::{call_builtin_on_values, eval_builtin, is_builtin, is_false};
use super::eval;
use super::expr::{Env, Expr};
use super::forms::{eval_define, eval_lambda, eval_quote, parse_let_binding};

/// Trampoline result: either a final value or a tail call to continue.
pub enum Bounce {
    Done(Expr),
    TailCall { expr: Expr, env: Env },
}

pub fn eval_step(expr: &Expr, env: &Env) -> Result<Bounce, String> {
    match expr {
        Expr::Integer(_) | Expr::Boolean(_) | Expr::Str(_) => Ok(Bounce::Done(expr.clone())),
        Expr::Symbol(name) => env
            .get(name)
            .map(Bounce::Done)
            .ok_or_else(|| format!("unbound variable: {name}")),
        Expr::Lambda { .. } | Expr::Builtin(_) => Ok(Bounce::Done(expr.clone())),
        Expr::List(elems) => eval_list_step(elems, env),
        _ => Err(format!("cannot evaluate: {}", expr.to_display())),
    }
}

fn eval_list_step(elems: &[Expr], env: &Env) -> Result<Bounce, String> {
    if elems.is_empty() {
        return Err("empty application".into());
    }
    if let Some(bounce) = try_special_form(elems, env)? {
        return Ok(bounce);
    }
    let proc = eval(&elems[0], env)?;
    let args: Vec<Expr> = elems[1..]
        .iter()
        .map(|a| eval(a, env))
        .collect::<Result<_, _>>()?;
    apply_proc(&proc, &args)
}

fn try_special_form(elems: &[Expr], env: &Env) -> Result<Option<Bounce>, String> {
    let Expr::Symbol(ref op) = elems[0] else {
        return Ok(None);
    };
    let args = &elems[1..];
    match op.as_str() {
        "define" => eval_define(args, env).map(|v| Some(Bounce::Done(v))),
        "set!" => {
            if args.len() != 2 {
                return Err("set! requires exactly two arguments".into());
            }
            let name = match &args[0] {
                Expr::Symbol(s) => s.clone(),
                _ => return Err("set!: first argument must be a symbol".into()),
            };
            let val = eval(&args[1], env)?;
            env.set(&name, val)?;
            Ok(Some(Bounce::Done(Expr::Void)))
        }
        "quote" => eval_quote(args).map(|v| Some(Bounce::Done(v))),
        "lambda" => eval_lambda(args, env).map(|v| Some(Bounce::Done(v))),
        "if" => bounce_if(args, env).map(Some),
        "begin" => bounce_begin(args, env).map(Some),
        "let" => bounce_let(args, env).map(Some),
        "cond" => bounce_cond(args, env).map(Some),
        "and" => bounce_and(args, env).map(Some),
        "or" => bounce_or(args, env).map(Some),
        name if is_builtin(name) => eval_builtin(name, args, env).map(|v| Some(Bounce::Done(v))),
        _ => Ok(None),
    }
}

fn bounce_if(args: &[Expr], env: &Env) -> Result<Bounce, String> {
    if args.len() < 2 || args.len() > 3 {
        return Err("if requires 2 or 3 arguments".into());
    }
    let cond = eval(&args[0], env)?;
    if !is_false(&cond) {
        Ok(Bounce::TailCall { expr: args[1].clone(), env: env.clone() })
    } else if args.len() == 3 {
        Ok(Bounce::TailCall { expr: args[2].clone(), env: env.clone() })
    } else {
        Ok(Bounce::Done(Expr::Void))
    }
}

fn bounce_begin(args: &[Expr], env: &Env) -> Result<Bounce, String> {
    if args.is_empty() {
        return Ok(Bounce::Done(Expr::Void));
    }
    for arg in &args[..args.len() - 1] {
        eval(arg, env)?;
    }
    Ok(Bounce::TailCall { expr: args[args.len() - 1].clone(), env: env.clone() })
}

fn bounce_let(args: &[Expr], env: &Env) -> Result<Bounce, String> {
    if args.len() < 2 {
        return Err("let requires bindings and body".into());
    }
    // Named let: (let name ((var val) ...) body ...)
    if let Expr::Symbol(ref name) = args[0] {
        return bounce_named_let(name, &args[1..], env);
    }
    let Expr::List(ref bindings) = args[0] else {
        return Err("let bindings must be a list".into());
    };
    let let_env = env.child();
    for binding in bindings {
        let (name, val) = parse_let_binding(binding, env)?;
        let_env.insert(name, val);
    }
    let body = &args[1..];
    for body_expr in &body[..body.len() - 1] {
        eval(body_expr, &let_env)?;
    }
    Ok(Bounce::TailCall { expr: body[body.len() - 1].clone(), env: let_env })
}

fn bounce_named_let(name: &str, args: &[Expr], env: &Env) -> Result<Bounce, String> {
    if args.len() < 2 {
        return Err("named let requires bindings and body".into());
    }
    let Expr::List(ref bindings) = args[0] else {
        return Err("let bindings must be a list".into());
    };
    let mut params = Vec::new();
    let mut init_vals = Vec::new();
    for binding in bindings {
        let (p, v) = parse_let_binding(binding, env)?;
        params.push(p);
        init_vals.push(v);
    }
    let body = if args[1..].len() == 1 {
        args[1].clone()
    } else {
        let mut begin_elems = vec![Expr::Symbol("begin".into())];
        begin_elems.extend_from_slice(&args[1..]);
        Expr::List(begin_elems)
    };
    let let_env = env.child();
    let lambda = Expr::Lambda {
        params: params.clone(),
        rest: None,
        body: Box::new(body.clone()),
        env: let_env.clone(),
    };
    let_env.insert(name.to_string(), lambda);
    for (p, v) in params.iter().zip(init_vals.iter()) {
        let_env.insert(p.clone(), v.clone());
    }
    Ok(Bounce::TailCall { expr: body, env: let_env })
}

fn bounce_cond(args: &[Expr], env: &Env) -> Result<Bounce, String> {
    for clause in args {
        let Expr::List(elems) = clause else {
            return Err("cond clause must be a list".into());
        };
        if elems.len() < 2 {
            return Err("cond clause must have test and expression".into());
        }
        let is_else = matches!(&elems[0], Expr::Symbol(s) if s == "else");
        if is_else || !is_false(&eval(&elems[0], env)?) {
            return bounce_cond_body(&elems[1..], env);
        }
    }
    Ok(Bounce::Done(Expr::Void))
}

fn bounce_cond_body(exprs: &[Expr], env: &Env) -> Result<Bounce, String> {
    for expr in &exprs[..exprs.len() - 1] {
        eval(expr, env)?;
    }
    Ok(Bounce::TailCall { expr: exprs[exprs.len() - 1].clone(), env: env.clone() })
}

fn bounce_and(args: &[Expr], env: &Env) -> Result<Bounce, String> {
    if args.is_empty() {
        return Ok(Bounce::Done(Expr::Boolean(true)));
    }
    for arg in &args[..args.len() - 1] {
        let val = eval(arg, env)?;
        if is_false(&val) {
            return Ok(Bounce::Done(val));
        }
    }
    Ok(Bounce::TailCall { expr: args[args.len() - 1].clone(), env: env.clone() })
}

fn bounce_or(args: &[Expr], env: &Env) -> Result<Bounce, String> {
    if args.is_empty() {
        return Ok(Bounce::Done(Expr::Boolean(false)));
    }
    for arg in &args[..args.len() - 1] {
        let val = eval(arg, env)?;
        if !is_false(&val) {
            return Ok(Bounce::Done(val));
        }
    }
    Ok(Bounce::TailCall { expr: args[args.len() - 1].clone(), env: env.clone() })
}

pub fn apply_proc(proc: &Expr, args: &[Expr]) -> Result<Bounce, String> {
    match proc {
        Expr::Lambda { params, rest, body, env: captured_env } => {
            apply_lambda(params, rest.as_deref(), body, captured_env, args)
        }
        Expr::Builtin(name) if name == "apply" => eval_apply_values(args),
        Expr::Builtin(name) => {
            let result = call_builtin_on_values(name, args.to_vec())?;
            Ok(Bounce::Done(result))
        }
        _ => Err(format!("not a procedure: {}", proc.to_display())),
    }
}

fn apply_lambda(
    params: &[String],
    rest: Option<&str>,
    body: &Expr,
    captured_env: &Env,
    args: &[Expr],
) -> Result<Bounce, String> {
    if rest.is_some() {
        if args.len() < params.len() {
            return Err(format!("expected at least {} args, got {}", params.len(), args.len()));
        }
    } else if args.len() != params.len() {
        return Err(format!("expected {} arguments, got {}", params.len(), args.len()));
    }
    let call_env = captured_env.child();
    for (param, arg) in params.iter().zip(args.iter()) {
        call_env.insert(param.clone(), arg.clone());
    }
    if let Some(rest_name) = rest {
        call_env.insert(rest_name.to_string(), Expr::List(args[params.len()..].to_vec()));
    }
    Ok(Bounce::TailCall { expr: body.clone(), env: call_env })
}

fn eval_apply_values(args: &[Expr]) -> Result<Bounce, String> {
    if args.len() < 2 {
        return Err("apply requires at least two arguments".into());
    }
    let proc = &args[0];
    let last = &args[args.len() - 1];
    let Expr::List(tail_args) = last else {
        return Err(format!("apply: last argument must be a list, got {}", last.to_display()));
    };
    let mut all_args: Vec<Expr> = args[1..args.len() - 1].to_vec();
    all_args.extend_from_slice(tail_args);
    apply_proc(proc, &all_args)
}
