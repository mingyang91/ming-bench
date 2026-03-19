use super::expr::{Env, Expr};
use super::eval;

pub fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | "not" | "and" | "or"
            | "cons" | "car" | "cdr" | "null?" | "list" | "length"
    )
}

pub fn is_false(expr: &Expr) -> bool {
    matches!(expr, Expr::Boolean(false))
}

pub fn eval_builtin(op: &str, args: &[Expr], env: &Env) -> Result<Expr, String> {
    match op {
        "+" | "-" | "*" | "/" => eval_arithmetic(op, args, env),
        "<" | ">" | "=" | "<=" => eval_comparison(op, args, env),
        "not" => {
            if args.len() != 1 {
                return Err("not requires exactly one argument".into());
            }
            let val = eval(&args[0], env)?;
            Ok(Expr::Boolean(is_false(&val)))
        }
        "and" => eval_and(args, env),
        "or" => eval_or(args, env),
        "cons" => eval_cons(args, env),
        "car" => eval_car(args, env),
        "cdr" => eval_cdr(args, env),
        "null?" => eval_null(args, env),
        "list" => eval_list_builtin(args, env),
        "length" => eval_length(args, env),
        _ => Err(format!("unknown procedure: {op}")),
    }
}

fn eval_comparison(op: &str, args: &[Expr], env: &Env) -> Result<Expr, String> {
    if args.len() != 2 {
        return Err(format!("{op} requires exactly two arguments"));
    }
    let a = match eval(&args[0], env)? {
        Expr::Integer(n) => n,
        other => return Err(format!("expected number, got {}", other.to_display())),
    };
    let b = match eval(&args[1], env)? {
        Expr::Integer(n) => n,
        other => return Err(format!("expected number, got {}", other.to_display())),
    };
    let result = match op {
        "<" => a < b,
        ">" => a > b,
        "=" => a == b,
        "<=" => a <= b,
        _ => unreachable!(),
    };
    Ok(Expr::Boolean(result))
}

pub fn eval_and(args: &[Expr], env: &Env) -> Result<Expr, String> {
    let mut result = Expr::Boolean(true);
    for arg in args {
        result = eval(arg, env)?;
        if is_false(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

pub fn eval_or(args: &[Expr], env: &Env) -> Result<Expr, String> {
    let mut result = Expr::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if !is_false(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_arithmetic(op: &str, args: &[Expr], env: &Env) -> Result<Expr, String> {
    let vals: Vec<i64> = args
        .iter()
        .map(|a| match eval(a, env)? {
            Expr::Integer(n) => Ok(n),
            other => Err(format!("expected number, got {}", other.to_display())),
        })
        .collect::<Result<_, _>>()?;

    if vals.is_empty() {
        return match op {
            "+" => Ok(Expr::Integer(0)),
            "*" => Ok(Expr::Integer(1)),
            _ => Err(format!("{op} requires at least one argument")),
        };
    }

    let result = match op {
        "+" => vals.iter().sum(),
        "*" => vals.iter().product(),
        "-" => {
            if vals.len() == 1 {
                -vals[0]
            } else {
                vals[1..].iter().fold(vals[0], |acc, &v| acc - v)
            }
        }
        "/" => {
            if vals.len() == 1 {
                return Err("/ requires at least two arguments".into());
            }
            checked_div(&vals)?
        }
        _ => unreachable!(),
    };
    Ok(Expr::Integer(result))
}

fn checked_div(vals: &[i64]) -> Result<i64, String> {
    vals[1..].iter().try_fold(vals[0], |acc, &v| {
        if v == 0 {
            return Err("division by zero".into());
        }
        Ok(acc / v)
    })
}

fn eval_cons(args: &[Expr], env: &Env) -> Result<Expr, String> {
    if args.len() != 2 {
        return Err("cons requires exactly two arguments".into());
    }
    let car = eval(&args[0], env)?;
    let cdr = eval(&args[1], env)?;
    match cdr {
        Expr::List(mut elems) => {
            elems.insert(0, car);
            Ok(Expr::List(elems))
        }
        _ => Err(format!("cons: second argument must be a list, got {}", cdr.to_display())),
    }
}

fn eval_car(args: &[Expr], env: &Env) -> Result<Expr, String> {
    if args.len() != 1 {
        return Err("car requires exactly one argument".into());
    }
    match eval(&args[0], env)? {
        Expr::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
        Expr::List(_) => Err("car: empty list".into()),
        other => Err(format!("car: expected pair, got {}", other.to_display())),
    }
}

fn eval_cdr(args: &[Expr], env: &Env) -> Result<Expr, String> {
    if args.len() != 1 {
        return Err("cdr requires exactly one argument".into());
    }
    match eval(&args[0], env)? {
        Expr::List(elems) if !elems.is_empty() => Ok(Expr::List(elems[1..].to_vec())),
        Expr::List(_) => Err("cdr: empty list".into()),
        other => Err(format!("cdr: expected pair, got {}", other.to_display())),
    }
}

fn eval_null(args: &[Expr], env: &Env) -> Result<Expr, String> {
    if args.len() != 1 {
        return Err("null? requires exactly one argument".into());
    }
    let val = eval(&args[0], env)?;
    Ok(Expr::Boolean(matches!(val, Expr::List(ref elems) if elems.is_empty())))
}

fn eval_list_builtin(args: &[Expr], env: &Env) -> Result<Expr, String> {
    let elems: Vec<Expr> = args
        .iter()
        .map(|a| eval(a, env))
        .collect::<Result<_, _>>()?;
    Ok(Expr::List(elems))
}

fn eval_length(args: &[Expr], env: &Env) -> Result<Expr, String> {
    if args.len() != 1 {
        return Err("length requires exactly one argument".into());
    }
    match eval(&args[0], env)? {
        Expr::List(elems) => Ok(Expr::Integer(elems.len() as i64)),
        other => Err(format!("length: expected list, got {}", other.to_display())),
    }
}
