mod builtins;
mod expr;
mod forms;
mod parser;

use builtins::{eval_builtin, is_builtin};
use expr::{Env, Expr};
use forms::{apply, eval_cond, eval_define, eval_if, eval_lambda, eval_let, eval_quote};
use parser::Parser;

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

pub(crate) fn eval(expr: &Expr, env: &Env) -> Result<Expr, String> {
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
            "begin" => return eval_begin(&elems[1..], env),
            "let" => return eval_let(&elems[1..], env),
            "cond" => return eval_cond(&elems[1..], env),
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

fn eval_begin(args: &[Expr], env: &Env) -> Result<Expr, String> {
    let mut result = Expr::Void;
    for arg in args {
        result = eval(arg, env)?;
    }
    Ok(result)
}

#[cfg(test)]
mod tests;
