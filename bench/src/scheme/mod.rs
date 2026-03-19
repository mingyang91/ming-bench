mod builtins;
mod expr;
mod parser;

use builtins::{eval_builtin, is_false, Env};
use expr::Expr;
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

    let mut env = Env::new();
    let mut result = Expr::Void;
    for expr in exprs {
        result = eval(&expr, &mut env)?;
    }

    match result {
        Expr::Void => Err("no displayable value".into()),
        _ => Ok(result.to_display()),
    }
}

fn eval(expr: &Expr, env: &mut Env) -> Result<Expr, String> {
    match expr {
        Expr::Integer(_) | Expr::Boolean(_) | Expr::Str(_) => Ok(expr.clone()),
        Expr::Symbol(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| format!("unbound variable: {name}")),
        Expr::List(elems) => {
            if elems.is_empty() {
                return Err("empty application".into());
            }
            match &elems[0] {
                Expr::Symbol(op) => match op.as_str() {
                    "define" => eval_define(&elems[1..], env),
                    "if" => eval_if(&elems[1..], env),
                    "quote" => eval_quote(&elems[1..]),
                    _ => eval_builtin(op, &elems[1..], env),
                },
                _ => Err(format!("not a procedure: {}", elems[0].to_display())),
            }
        }
        _ => Err(format!("cannot evaluate: {}", expr.to_display())),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Expr, String> {
    if args.len() != 1 {
        return Err("quote requires exactly one argument".into());
    }
    Ok(args[0].clone())
}

fn eval_define(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.len() != 2 {
        return Err("define requires exactly two arguments".into());
    }
    match &args[0] {
        Expr::Symbol(name) => {
            let val = eval(&args[1], env)?;
            env.insert(name.clone(), val);
            Ok(Expr::Void)
        }
        _ => Err(format!(
            "define expects a symbol, got {}",
            args[0].to_display()
        )),
    }
}

fn eval_if(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
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
