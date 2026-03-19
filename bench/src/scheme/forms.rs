use super::eval;
use crate::scheme::expr::{Env, Expr};

pub fn parse_params_with_rest(elems: &[Expr]) -> Result<(Vec<String>, Option<String>), String> {
    let dot_pos = elems.iter().position(|e| matches!(e, Expr::Symbol(s) if s == "."));
    let Some(dp) = dot_pos else {
        return collect_symbol_params(elems).map(|p| (p, None));
    };
    if dp + 2 != elems.len() {
        return Err("invalid dot notation in parameters".into());
    }
    let fixed = collect_symbol_params(&elems[..dp])?;
    let rest = match &elems[dp + 1] {
        Expr::Symbol(r) => r.clone(),
        _ => return Err("rest parameter must be a symbol".into()),
    };
    Ok((fixed, Some(rest)))
}

fn collect_symbol_params(elems: &[Expr]) -> Result<Vec<String>, String> {
    elems
        .iter()
        .map(|e| match e {
            Expr::Symbol(s) => Ok(s.clone()),
            _ => Err("parameters must be symbols".into()),
        })
        .collect()
}

pub fn eval_lambda(args: &[Expr], env: &Env) -> Result<Expr, String> {
    if args.len() < 2 {
        return Err("lambda requires params and body".into());
    }
    let (params, rest) = match &args[0] {
        Expr::List(elems) => parse_params_with_rest(elems)?,
        _ => return Err("lambda requires a parameter list".into()),
    };
    let body = if args.len() == 2 {
        args[1].clone()
    } else {
        let mut begin_elems = vec![Expr::Symbol("begin".into())];
        begin_elems.extend_from_slice(&args[1..]);
        Expr::List(begin_elems)
    };
    Ok(Expr::Lambda {
        params,
        rest,
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
            let (params, rest) = parse_params_with_rest(&elems[1..])?;
            let body = if args.len() == 2 {
                args[1].clone()
            } else {
                let mut begin_elems = vec![Expr::Symbol("begin".into())];
                begin_elems.extend_from_slice(&args[1..]);
                Expr::List(begin_elems)
            };
            let lambda = Expr::Lambda {
                params,
                rest,
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

pub fn parse_let_binding(binding: &Expr, env: &Env) -> Result<(String, Expr), String> {
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


