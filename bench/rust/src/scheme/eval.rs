use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::parser::Expr;
use crate::scheme::value::Value;

/// Evaluate a parsed expression in the given environment.
pub fn eval_expr(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => env.get(name).cloned().ok_or_else(|| EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Expr::List(elems) => eval_list(elems, env),
    }
}

fn eval_list(elems: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if elems.is_empty() {
        return Ok(Value::List(Vec::new()));
    }

    // Check for special forms first
    if let Expr::Symbol(name) = &elems[0] {
        match name.as_str() {
            "and" => return eval_and(&elems[1..], env),
            "or" => return eval_or(&elems[1..], env),
            _ => {}
        }
    }

    let func = eval_expr(&elems[0], env)?;
    let args: Vec<Value> = elems[1..]
        .iter()
        .map(|e| eval_expr(e, env))
        .collect::<Result<Vec<_>, _>>()?;

    apply_builtin(&func, &args)
}

fn eval_and(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval_expr(expr, env)?;
        if !val.is_truthy() {
            return Ok(val);
        }
    }
    eval_expr(&exprs[exprs.len() - 1], env)
}

fn eval_or(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval_expr(expr, env)?;
        if val.is_truthy() {
            return Ok(val);
        }
    }
    eval_expr(&exprs[exprs.len() - 1], env)
}

fn apply_builtin(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    let name = match func {
        Value::Builtin(name) => name.as_str(),
        other => {
            return Err(EvalError::NotAProcedure {
                value: other.to_display_string(),
            });
        }
    };

    match name {
        "+" => arith_variadic(args, 0, |a, b| Ok(a + b)),
        "-" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            if args.len() == 1 {
                let n = require_int(&args[0])?;
                return Ok(Value::Integer(-n));
            }
            let first = require_int(&args[0])?;
            let rest_sum: i64 = args[1..]
                .iter()
                .map(require_int)
                .collect::<Result<Vec<_>, _>>()?
                .iter()
                .sum();
            Ok(Value::Integer(first - rest_sum))
        }
        "*" => arith_variadic(args, 1, |a, b| Ok(a * b)),
        "/" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let first = require_int(&args[0])?;
            if args.len() == 1 {
                if first == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                return Ok(Value::Integer(1 / first));
            }
            let mut result = first;
            for arg in &args[1..] {
                let n = require_int(arg)?;
                if n == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= n;
            }
            Ok(Value::Integer(result))
        }
        "<" => compare_nums(args, |a, b| a < b),
        ">" => compare_nums(args, |a, b| a > b),
        "=" => compare_nums(args, |a, b| a == b),
        "<=" => compare_nums(args, |a, b| a <= b),
        ">=" => compare_nums(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        _ => Err(EvalError::NotAProcedure {
            value: format!("#<procedure:{}>", name),
        }),
    }
}

fn require_int(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::TypeMismatch {
            expected: "integer".into(),
            got: other.to_display_string(),
        }),
    }
}

fn arith_variadic(
    args: &[Value],
    identity: i64,
    op: impl Fn(i64, i64) -> Result<i64, EvalError>,
) -> Result<Value, EvalError> {
    let mut result = identity;
    for arg in args {
        let n = require_int(arg)?;
        result = op(result, n)?;
    }
    Ok(Value::Integer(result))
}

fn compare_nums(args: &[Value], cmp: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    }
    for pair in args.windows(2) {
        let a = require_int(&pair[0])?;
        let b = require_int(&pair[1])?;
        if !cmp(a, b) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}
