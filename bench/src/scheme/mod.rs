mod expr;
mod parser;

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

    let mut result = Expr::Void;
    for expr in exprs {
        result = eval(&expr)?;
    }

    match result {
        Expr::Void => Err("no displayable value".into()),
        _ => Ok(result.to_display()),
    }
}

fn eval(expr: &Expr) -> Result<Expr, String> {
    match expr {
        Expr::Integer(_) | Expr::Boolean(_) | Expr::Str(_) => Ok(expr.clone()),
        Expr::List(elems) => {
            if elems.is_empty() {
                return Err("empty application".into());
            }
            match &elems[0] {
                Expr::Symbol(op) => eval_builtin(op, &elems[1..]),
                _ => Err(format!("not a procedure: {}", elems[0].to_display())),
            }
        }
        _ => Err(format!("cannot evaluate: {}", expr.to_display())),
    }
}

fn eval_builtin(op: &str, args: &[Expr]) -> Result<Expr, String> {
    match op {
        "+" | "-" | "*" | "/" => eval_arithmetic(op, args),
        _ => Err(format!("unknown procedure: {op}")),
    }
}

fn eval_arithmetic(op: &str, args: &[Expr]) -> Result<Expr, String> {
    let vals: Vec<i64> = args
        .iter()
        .map(|a| match eval(a)? {
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

#[cfg(test)]
mod tests;
