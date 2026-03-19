mod builtins;
mod expr;
mod parser;

use builtins::{eval_builtin, is_false, is_builtin};
use expr::{Env, Expr};
use parser::Parser;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use cs61a_bench::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, String> {
    let tokens = parser::tokenize(input)?;
    let mut parser = Parser::new(&tokens);
    let exprs = parser.parse_all()?;

    if exprs.is_empty() {
        return Err("no expression".into());
    }

    let env = Env::new();
    let mut result = Expr::Void;
    for expr in exprs {
        result = eval(&expr, &env)?;
    }

    match result {
        Expr::Void => Err("no displayable value".into()),
        _ => Ok(result.to_display()),
    }
}

fn eval(expr: &Expr, env: &Env) -> Result<Expr, String> {
    match expr {
        Expr::Integer(_) | Expr::Boolean(_) | Expr::Str(_) => Ok(expr.clone()),
        Expr::Symbol(name) => env
            .get(name)
            .ok_or_else(|| format!("unbound variable: {name}")),
        Expr::List(elems) => eval_list(elems, env),
        Expr::Lambda { .. } => Ok(expr.clone()),
        _ => Err(format!("cannot evaluate: {}", expr.to_display())),
    }
}

fn eval_list(elems: &[Expr], env: &Env) -> Result<Expr, String> {
    if elems.is_empty() {
        return Err("empty application".into());
    }
    if let Expr::Symbol(op) = &elems[0] {
        match op.as_str() {
            "define" => return eval_define(&elems[1..], env),
            "if" => return eval_if(&elems[1..], env),
            "quote" => return eval_quote(&elems[1..]),
            "lambda" => return eval_lambda(&elems[1..], env),
            name if is_builtin(name) => return eval_builtin(name, &elems[1..], env),
            _ => {}
        }
    }
    let proc = eval(&elems[0], env)?;
    let args: Vec<Expr> = elems[1..]
        .iter()
        .map(|a| eval(a, env))
        .collect::<Result<_, _>>()?;
    apply(&proc, &args)
}

fn apply(proc: &Expr, args: &[Expr]) -> Result<Expr, String> {
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

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Expr, String> {
    if args.len() != 2 {
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
    Ok(Expr::Lambda {
        params,
        body: Box::new(args[1].clone()),
        env: env.clone(),
    })
}

fn eval_quote(args: &[Expr]) -> Result<Expr, String> {
    if args.len() != 1 {
        return Err("quote requires exactly one argument".into());
    }
    Ok(args[0].clone())
}

fn eval_define(args: &[Expr], env: &Env) -> Result<Expr, String> {
    if args.len() != 2 {
        return Err("define requires exactly two arguments".into());
    }
    match &args[0] {
        Expr::Symbol(name) => {
            let val = eval(&args[1], env)?;
            env.insert(name.clone(), val);
            Ok(Expr::Void)
        }
        Expr::List(elems) => {
            // (define (f x y) body) => (define f (lambda (x y) body))
            if elems.is_empty() {
                return Err("define: empty function signature".into());
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
            let lambda = Expr::Lambda {
                params,
                body: Box::new(args[1].clone()),
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

fn eval_if(args: &[Expr], env: &Env) -> Result<Expr, String> {
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

#[cfg(test)]
mod tests;
