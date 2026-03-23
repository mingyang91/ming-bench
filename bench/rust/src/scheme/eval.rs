use std::rc::Rc;
use crate::scheme::env::Env;
use crate::scheme::error::{ErrorKind, EvalError};
use crate::scheme::parser::{Expr, ExprKind};
use crate::scheme::value::Value;

/// Evaluate a parsed expression in the given environment.
pub fn eval_expr(expr: &Expr, env: &Rc<Env>) -> Result<Value, EvalError> {
    eval_expr_inner(expr, env).map_err(|e| e.with_span(expr.span))
}

fn eval_expr_inner(expr: &Expr, env: &Rc<Env>) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Symbol(name) => env.get(name).ok_or_else(|| {
            ErrorKind::UnboundVariable { name: name.clone() }.into()
        }),
        ExprKind::List(elems) => eval_list(elems, env),
    }
}

fn eval_list(elems: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if elems.is_empty() {
        return Ok(Value::List(Vec::new()));
    }

    // Check for special forms first
    if let ExprKind::Symbol(name) = &elems[0].kind {
        match name.as_str() {
            "and" => return eval_and(&elems[1..], env),
            "or" => return eval_or(&elems[1..], env),
            "if" => return eval_if(&elems[1..], env),
            "define" => return eval_define(&elems[1..], env),
            "quote" => return eval_quote(&elems[1..]),
            "lambda" => return eval_lambda(&elems[1..], env),
            "let" => return eval_let(&elems[1..], env),
            "begin" => return eval_begin(&elems[1..], env),
            "cond" => return eval_cond(&elems[1..], env),
            _ => {}
        }
    }

    let func = eval_expr(&elems[0], env)?;
    let args: Vec<Value> = elems[1..]
        .iter()
        .map(|e| eval_expr(e, env))
        .collect::<Result<Vec<_>, _>>()?;

    apply_function(&func, &args)
}

fn eval_if(args: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(ErrorKind::BadSyntax {
            form: "if".into(),
            message: "expected 2 or 3 arguments".into(),
        }.into());
    }
    let cond = eval_expr(&args[0], env)?;
    if cond.is_truthy() {
        eval_expr(&args[1], env)
    } else if args.len() == 3 {
        eval_expr(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_define(args: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(ErrorKind::BadSyntax {
            form: "define".into(),
            message: "missing name".into(),
        }.into());
    }
    match &args[0].kind {
        // (define x expr)
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(ErrorKind::BadSyntax {
                    form: "define".into(),
                    message: "expected (define name expr)".into(),
                }.into());
            }
            let val = eval_expr(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...)
        ExprKind::List(name_and_params) => {
            if name_and_params.is_empty() {
                return Err(ErrorKind::BadSyntax {
                    form: "define".into(),
                    message: "missing function name".into(),
                }.into());
            }
            let name = match &name_and_params[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(ErrorKind::BadSyntax {
                    form: "define".into(),
                    message: "function name must be a symbol".into(),
                }.into()),
            };
            let params: Vec<String> = name_and_params[1..]
                .iter()
                .map(|e| match &e.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::from(ErrorKind::BadSyntax {
                        form: "define".into(),
                        message: "parameter must be a symbol".into(),
                    })),
                })
                .collect::<Result<Vec<_>, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                body,
                closure_env: Rc::clone(env),
            };
            env.define(name, lambda);
            Ok(Value::Void)
        }
        _ => Err(ErrorKind::BadSyntax {
            form: "define".into(),
            message: "invalid define syntax".into(),
        }.into()),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(ErrorKind::BadSyntax {
            form: "quote".into(),
            message: "expected 1 argument".into(),
        }.into());
    }
    expr_to_value(&args[0])
}

fn expr_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Symbol(s) => Ok(Value::Symbol(s.clone())),
        ExprKind::List(elems) => {
            let vals: Vec<Value> = elems
                .iter()
                .map(expr_to_value)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::List(vals))
        }
    }
}

fn eval_lambda(args: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "lambda".into(),
            message: "expected (lambda (params) body...)".into(),
        }.into());
    }
    let params = match &args[0].kind {
        ExprKind::List(param_exprs) => {
            param_exprs
                .iter()
                .map(|e| match &e.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::from(ErrorKind::BadSyntax {
                        form: "lambda".into(),
                        message: "parameter must be a symbol".into(),
                    })),
                })
                .collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(ErrorKind::BadSyntax {
            form: "lambda".into(),
            message: "expected parameter list".into(),
        }.into()),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        closure_env: Rc::clone(env),
    })
}

fn eval_and(exprs: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
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

fn eval_or(exprs: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
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

fn eval_let(args: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(ErrorKind::BadSyntax {
            form: "let".into(),
            message: "missing bindings".into(),
        }.into());
    }
    // Named let: (let name ((var init) ...) body...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 3 {
            return Err(ErrorKind::BadSyntax {
                form: "let".into(),
                message: "expected (let name ((var init) ...) body...)".into(),
            }.into());
        }
        let bindings_expr = match &args[1].kind {
            ExprKind::List(b) => b,
            _ => return Err(ErrorKind::BadSyntax {
                form: "let".into(),
                message: "bindings must be a list".into(),
            }.into()),
        };
        let mut params = Vec::new();
        let mut init_vals = Vec::new();
        for binding in bindings_expr {
            match &binding.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    match &pair[0].kind {
                        ExprKind::Symbol(var) => {
                            params.push(var.clone());
                            init_vals.push(eval_expr(&pair[1], env)?);
                        }
                        _ => return Err(ErrorKind::BadSyntax {
                            form: "let".into(),
                            message: "binding name must be a symbol".into(),
                        }.into()),
                    }
                }
                _ => return Err(ErrorKind::BadSyntax {
                    form: "let".into(),
                    message: "each binding must be (var init)".into(),
                }.into()),
            }
        }
        let body = args[2..].to_vec();
        let lambda = Value::Lambda {
            params: params.clone(),
            body,
            closure_env: Rc::clone(env),
        };
        // Create env with the named function bound so it can recurse
        let func_env = Env::extend(env, vec![name.clone()], vec![lambda.clone()]);
        // Re-create lambda with func_env as closure so it sees itself
        let recursive_lambda = Value::Lambda {
            params,
            body: args[2..].to_vec(),
            closure_env: Rc::clone(&func_env),
        };
        func_env.define(name.clone(), recursive_lambda.clone());
        return apply_function(&recursive_lambda, &init_vals);
    }
    // Regular let: (let ((var init) ...) body...)
    let bindings_expr = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(ErrorKind::BadSyntax {
            form: "let".into(),
            message: "bindings must be a list".into(),
        }.into()),
    };
    if args.len() < 2 {
        return Err(ErrorKind::BadSyntax {
            form: "let".into(),
            message: "missing body".into(),
        }.into());
    }
    let mut names = Vec::new();
    let mut vals = Vec::new();
    for binding in bindings_expr {
        match &binding.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                match &pair[0].kind {
                    ExprKind::Symbol(var) => {
                        names.push(var.clone());
                        vals.push(eval_expr(&pair[1], env)?);
                    }
                    _ => return Err(ErrorKind::BadSyntax {
                        form: "let".into(),
                        message: "binding name must be a symbol".into(),
                    }.into()),
                }
            }
            _ => return Err(ErrorKind::BadSyntax {
                form: "let".into(),
                message: "each binding must be (var init)".into(),
            }.into()),
        }
    }
    let local_env = Env::extend(env, names, vals);
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval_expr(expr, &local_env)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval_expr(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &Rc<Env>) -> Result<Value, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(elems) if !elems.is_empty() => {
                // Check for else clause
                if let ExprKind::Symbol(s) = &elems[0].kind {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &elems[1..] {
                            result = eval_expr(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval_expr(&elems[0], env)?;
                if test.is_truthy() {
                    let mut result = test;
                    if elems.len() > 1 {
                        for expr in &elems[1..] {
                            result = eval_expr(expr, env)?;
                        }
                    }
                    return Ok(result);
                }
            }
            _ => return Err(ErrorKind::BadSyntax {
                form: "cond".into(),
                message: "each clause must be a list".into(),
            }.into()),
        }
    }
    Ok(Value::Void)
}

pub fn apply_function(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(name) => apply_builtin(name, args),
        Value::Lambda { params, body, closure_env } => {
            if args.len() != params.len() {
                return Err(ErrorKind::WrongArgCount {
                    expected: params.len(),
                    got: args.len(),
                }.into());
            }
            let local_env = Env::extend(closure_env, params.clone(), args.to_vec());
            let mut result = Value::Void;
            for expr in body {
                result = eval_expr(expr, &local_env)?;
            }
            Ok(result)
        }
        other => Err(ErrorKind::NotAProcedure {
            value: other.to_display_string(),
        }.into()),
    }
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => arith_variadic(args, 0, |a, b| Ok(a + b)),
        "-" => {
            if args.is_empty() {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: 0 }.into());
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
                return Err(ErrorKind::WrongArgCount { expected: 1, got: 0 }.into());
            }
            let first = require_int(&args[0])?;
            if args.len() == 1 {
                if first == 0 {
                    return Err(ErrorKind::DivisionByZero.into());
                }
                return Ok(Value::Integer(1 / first));
            }
            let mut result = first;
            for arg in &args[1..] {
                let n = require_int(arg)?;
                if n == 0 {
                    return Err(ErrorKind::DivisionByZero.into());
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
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
            }
            match &args[1] {
                Value::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => {
                    Ok(Value::List(vec![args[0].clone(), args[1].clone()]))
                }
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(ErrorKind::TypeMismatch {
                    expected: "pair".into(),
                    got: args[0].to_display_string(),
                }.into()),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
                _ => Err(ErrorKind::TypeMismatch {
                    expected: "pair".into(),
                    got: args[0].to_display_string(),
                }.into()),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(elems) if elems.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            match &args[0] {
                Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                _ => Err(ErrorKind::TypeMismatch {
                    expected: "list".into(),
                    got: args[0].to_display_string(),
                }.into()),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for arg in args {
                match arg {
                    Value::List(elems) => result.extend(elems.iter().cloned()),
                    _ => return Err(ErrorKind::TypeMismatch {
                        expected: "list".into(),
                        got: arg.to_display_string(),
                    }.into()),
                }
            }
            Ok(Value::List(result))
        }
        "string?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(elems) if !elems.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(ErrorKind::WrongArgCount { expected: 1, got: args.len() }.into());
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        _ => Err(ErrorKind::NotAProcedure {
            value: format!("#<procedure:{}>", name),
        }.into()),
    }
}

fn require_int(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(ErrorKind::TypeMismatch {
            expected: "integer".into(),
            got: other.to_display_string(),
        }.into()),
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
        return Err(ErrorKind::WrongArgCount { expected: 2, got: args.len() }.into());
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
