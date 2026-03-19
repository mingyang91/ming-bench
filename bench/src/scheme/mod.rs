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
        _ => Err(format!("cannot evaluate: {}", expr.to_display())),
    }
}

#[cfg(test)]
mod tests;
