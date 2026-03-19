use super::builtins::{eval_builtin, is_builtin, is_false};
use super::eval;
use super::expr::{Env, Expr};
use super::forms::{eval_cond, eval_define, eval_lambda, eval_quote, parse_let_binding};

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
        Expr::Lambda { .. } => Ok(Bounce::Done(expr.clone())),
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
        "cond" => eval_cond(args, env).map(|v| Some(Bounce::Done(v))),
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

pub fn apply_proc(proc: &Expr, args: &[Expr]) -> Result<Bounce, String> {
    let Expr::Lambda { params, body, env: captured_env } = proc else {
        return Err(format!("not a procedure: {}", proc.to_display()));
    };
    if args.len() != params.len() {
        return Err(format!("expected {} arguments, got {}", params.len(), args.len()));
    }
    let call_env = captured_env.child();
    for (param, arg) in params.iter().zip(args.iter()) {
        call_env.insert(param.clone(), arg.clone());
    }
    Ok(Bounce::TailCall { expr: *body.clone(), env: call_env })
}
