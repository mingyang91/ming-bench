use super::eval;
use crate::scheme::builtins::is_false;
use crate::scheme::expr::{Env, Expr};

pub fn eval_lambda(args: &[Expr], env: &Env) -> Result<Expr, String> {
    if args.len() < 2 {
        return Err("lambda requires params and body".into());
    }
    let params = match &args[0] {
        Expr::List(elems) => {
            let mut params = Vec::new();
            for e in elems {
                match e {
                    Expr::Symbol(s) => params.push(s.clone()),
                    _ => return Err("lambda params must be symbols".into()),
                }
            }
            params
        }
        _ => return Err("lambda requires a parameter list".into()),
    };
    let body = if args.len() == 2 {
        args[1].clone()
    } else {
        // Implicit begin for multiple body expressions
        let mut begin_elems = vec![Expr::Symbol("begin".into())];
        begin_elems.extend_from_slice(&args[1..]);
        Expr::List(begin_elems)
    };
    Ok(Expr::Lambda {
        params,
        body: Box::new(body),
        env: env.clone(),
    })
}

pub fn eval_quote(args: &[Expr]) -> Result<Expr, String> {
    if args.len() != 1 {
        return Err("quote requires exactly one argument".into());
    }
    Ok(args[0].clone())
}

pub fn eval_define(args: &[Expr], env: &Env) -> Result<Expr, String> {
    match &args[0] {
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err("define requires exactly two arguments".into());
            }
            let val = eval(&args[1], env)?;
            env.insert(name.clone(), val);
            Ok(Expr::Void)
        }
        Expr::List(elems) => {
            if elems.is_empty() {
                return Err("define: empty function signature".into());
            }
            if args.len() < 2 {
                return Err("define: missing body".into());
            }
            let name = match &elems[0] {
                Expr::Symbol(s) => s.clone(),
                _ => return Err("define: function name must be a symbol".into()),
            };
            let params: Vec<String> = elems[1..]
                .iter()
                .map(|e| match e {
                    Expr::Symbol(s) => Ok(s.clone()),
                    _ => Err("define: params must be symbols".to_string()),
                })
                .collect::<Result<_, String>>()?;
            let body = if args.len() == 2 {
                args[1].clone()
            } else {
                let mut begin_elems = vec![Expr::Symbol("begin".into())];
                begin_elems.extend_from_slice(&args[1..]);
                Expr::List(begin_elems)
            };
            let lambda = Expr::Lambda {
                params,
                body: Box::new(body),
                env: env.clone(),
            };
            env.insert(name, lambda);
            Ok(Expr::Void)
        }
        _ => Err(format!(
            "define expects a symbol or list, got {}",
            args[0].to_display()
        )),
    }
}

pub fn eval_if(args: &[Expr], env: &Env) -> Result<Expr, String> {
    if args.len() < 2 || args.len() > 3 {
        return Err("if requires 2 or 3 arguments".into());
    }
    let cond = eval(&args[0], env)?;
    if !is_false(&cond) {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Expr::Void)
    }
}

pub fn eval_let(args: &[Expr], env: &Env) -> Result<Expr, String> {
    if args.len() < 2 {
        return Err("let requires bindings and body".into());
    }
    let Expr::List(bindings) = &args[0] else {
        return Err("let bindings must be a list".into());
    };
    let let_env = env.child();
    for binding in bindings {
        let (name, val) = parse_let_binding(binding, env)?;
        let_env.insert(name, val);
    }
    let mut result = Expr::Void;
    for body_expr in &args[1..] {
        result = eval(body_expr, &let_env)?;
    }
    Ok(result)
}

fn parse_let_binding(binding: &Expr, env: &Env) -> Result<(String, Expr), String> {
    let Expr::List(pair) = binding else {
        return Err("let binding must be (name value)".into());
    };
    if pair.len() != 2 {
        return Err("let binding must be (name value)".into());
    }
    let Expr::Symbol(name) = &pair[0] else {
        return Err("let binding name must be a symbol".into());
    };
    let val = eval(&pair[1], env)?;
    Ok((name.clone(), val))
}

pub fn eval_cond(args: &[Expr], env: &Env) -> Result<Expr, String> {
    for clause in args {
        let Expr::List(elems) = clause else {
            return Err("cond clause must be a list".into());
        };
        if elems.len() < 2 {
            return Err("cond clause must have test and expression".into());
        }
        let is_else = matches!(&elems[0], Expr::Symbol(s) if s == "else");
        if is_else || !is_false(&eval(&elems[0], env)?) {
            return eval_body(&elems[1..], env);
        }
    }
    Ok(Expr::Void)
}

fn eval_body(exprs: &[Expr], env: &Env) -> Result<Expr, String> {
    let mut result = Expr::Void;
    for expr in exprs {
        result = eval(expr, env)?;
    }
    Ok(result)
}

pub fn apply(proc: &Expr, args: &[Expr]) -> Result<Expr, String> {
    match proc {
        Expr::Lambda {
            params,
            body,
            env: captured_env,
        } => {
            if args.len() != params.len() {
                return Err(format!(
                    "expected {} arguments, got {}",
                    params.len(),
                    args.len()
                ));
            }
            let call_env = captured_env.child();
            for (param, arg) in params.iter().zip(args.iter()) {
                call_env.insert(param.clone(), arg.clone());
            }
            eval(body, &call_env)
        }
        _ => Err(format!("not a procedure: {}", proc.to_display())),
    }
}
