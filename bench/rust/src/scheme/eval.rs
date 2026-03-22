use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, EvalErrorKind, Span};
use crate::scheme::parser::{Expr, ExprKind};
use crate::scheme::value::Value;

/// Evaluate an expression in the given environment.
pub fn eval(expr: &Expr, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::SchemeString(s) => Ok(Value::SchemeString(s.clone())),
        ExprKind::Symbol(name) => {
            env.lookup(name).map_err(|e| e.with_span(&expr.span))
        }
        ExprKind::List(elements) => eval_list(elements, &expr.span, env, output),
    }
}

fn eval_list(
    elements: &[Expr],
    list_span: &Span,
    env: &Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    if elements.is_empty() {
        return Err(EvalErrorKind::Parse {
            message: "empty application".into(),
        }
        .at(list_span));
    }

    // Check for special forms
    if let ExprKind::Symbol(name) = &elements[0].kind {
        let kw_span = &elements[0].span;
        match name.as_str() {
            "if" => return eval_if(&elements[1..], kw_span, env, output),
            "define" => return eval_define(&elements[1..], kw_span, env, output),
            "quote" => return eval_quote(&elements[1..], kw_span),
            "lambda" => return eval_lambda(&elements[1..], kw_span, env),
            "and" => return eval_and(&elements[1..], env, output),
            "or" => return eval_or(&elements[1..], env, output),
            "not" => return eval_not(&elements[1..], kw_span, env, output),
            "let" => return eval_let(&elements[1..], kw_span, env, output),
            "begin" => return eval_begin(&elements[1..], env, output),
            "cond" => return eval_cond(&elements[1..], kw_span, env, output),
            _ => {}
        }
    }

    // Check for builtin functions by name before evaluating
    if let ExprKind::Symbol(name) = &elements[0].kind {
        if is_builtin(name) {
            let args: Vec<Value> = elements[1..]
                .iter()
                .map(|e| eval(e, env, output))
                .collect::<Result<Vec<_>, _>>()?;
            return eval_builtin(name, &args, &elements[0].span, output);
        }
    }

    // Evaluate operator and arguments
    let op = eval(&elements[0], env, output)?;
    let args: Vec<Value> = elements[1..]
        .iter()
        .map(|e| eval(e, env, output))
        .collect::<Result<Vec<_>, _>>()?;

    apply(&op, &args, list_span, output)
}

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-"
            | "*"
            | "/"
            | "<"
            | ">"
            | "="
            | "<="
            | ">="
            | "cons"
            | "car"
            | "cdr"
            | "null?"
            | "list"
            | "length"
            | "pair?"
            | "string?"
            | "number?"
            | "boolean?"
            | "symbol?"
            | "char?"
            | "display"
            | "write"
            | "newline"
            | "string-append"
            | "string-length"
            | "substring"
            | "string->number"
            | "number->string"
            | "symbol->string"
            | "string->symbol"
            | "string-ref"
    )
}

fn apply(
    op: &Value,
    args: &[Value],
    span: &Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    match op {
        Value::Lambda {
            params,
            body,
            env: closure_env,
        } => {
            if args.len() != params.len() {
                return Err(EvalErrorKind::Arity {
                    name: "#<procedure>".into(),
                    expected: params.len().to_string(),
                    got: args.len(),
                }
                .at(span));
            }
            let call_env = Env::with_parent(closure_env);
            for (param, arg) in params.iter().zip(args.iter()) {
                call_env.define(param.clone(), arg.clone());
            }
            let mut result = Value::Nil;
            for expr in body {
                result = eval(expr, &call_env, output)?;
            }
            Ok(result)
        }
        _ => Err(EvalErrorKind::NotAProcedure {
            value: op.to_string(),
        }
        .at(span)),
    }
}

fn eval_builtin(
    name: &str,
    args: &[Value],
    span: &Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    match name {
        "+" => eval_add(args, span),
        "-" => eval_sub(args, name, span),
        "*" => eval_mul(args, span),
        "/" => eval_div(args, name, span),
        "<" => eval_cmp(args, name, span, |a, b| a < b),
        ">" => eval_cmp(args, name, span, |a, b| a > b),
        "=" => eval_cmp(args, name, span, |a, b| a == b),
        "<=" => eval_cmp(args, name, span, |a, b| a <= b),
        ">=" => eval_cmp(args, name, span, |a, b| a >= b),
        "cons" => eval_cons(args, span),
        "car" => eval_car(args, span),
        "cdr" => eval_cdr(args, span),
        "null?" => eval_null_pred(args, span),
        "list" => eval_list_builtin(args),
        "length" => eval_length(args, span),
        "pair?" => Ok(Value::Boolean(
            matches!(args.first(), Some(Value::Pair(_, _))) && args.len() == 1,
        )),
        "string?" => Ok(Value::Boolean(
            matches!(args.first(), Some(Value::SchemeString(_))) && args.len() == 1,
        )),
        "number?" => Ok(Value::Boolean(
            matches!(args.first(), Some(Value::Integer(_))) && args.len() == 1,
        )),
        "boolean?" => Ok(Value::Boolean(
            matches!(args.first(), Some(Value::Boolean(_))) && args.len() == 1,
        )),
        "symbol?" => Ok(Value::Boolean(
            matches!(args.first(), Some(Value::Symbol(_))) && args.len() == 1,
        )),
        "char?" => Ok(Value::Boolean(
            matches!(args.first(), Some(Value::Char(_))) && args.len() == 1,
        )),
        "display" => eval_display(args, span, output),
        "write" => eval_write(args, span, output),
        "newline" => eval_newline(args, span, output),
        "string-append" => eval_string_append(args, span),
        "string-length" => eval_string_length(args, span),
        "substring" => eval_substring(args, span),
        "string->number" => eval_string_to_number(args, span),
        "number->string" => eval_number_to_string(args, span),
        "symbol->string" => eval_symbol_to_string(args, span),
        "string->symbol" => eval_string_to_symbol(args, span),
        "string-ref" => eval_string_ref(args, span),
        _ => Err(EvalErrorKind::UnboundVariable {
            name: name.to_string(),
        }
        .at(span)),
    }
}

fn eval_if(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalErrorKind::Parse {
            message: "if requires 2 or 3 arguments".into(),
        }
        .at(span));
    }
    let cond = eval(&args[0], env, output)?;
    if cond.is_truthy() {
        eval(&args[1], env, output)
    } else if args.len() == 3 {
        eval(&args[2], env, output)
    } else {
        Ok(Value::Nil)
    }
}

fn eval_define(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalErrorKind::Parse {
            message: "define requires at least 2 arguments".into(),
        }
        .at(span));
    }

    match &args[0].kind {
        // (define x expr)
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalErrorKind::Parse {
                    message: "define requires exactly 2 arguments".into(),
                }
                .at(span));
            }
            let val = eval(&args[1], env, output)?;
            env.define(name.clone(), val);
            Ok(Value::Nil)
        }
        // (define (f params...) body...)
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalErrorKind::Parse {
                    message: "define: empty signature".into(),
                }
                .at(span));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(n) => n.clone(),
                _ => {
                    return Err(EvalErrorKind::Parse {
                        message: "define: expected symbol as function name".into(),
                    }
                    .at(span))
                }
            };
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|e| match &e.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalErrorKind::Parse {
                        message: "define: expected symbol as parameter".into(),
                    }
                    .at(span)),
                })
                .collect::<Result<Vec<_>, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env.define(name, lambda);
            Ok(Value::Nil)
        }
        _ => Err(EvalErrorKind::Parse {
            message: "define: expected symbol or list".into(),
        }
        .at(span)),
    }
}

fn eval_quote(args: &[Expr], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Parse {
            message: "quote requires exactly 1 argument".into(),
        }
        .at(span));
    }
    expr_to_value(&args[0])
}

fn expr_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::SchemeString(s) => Ok(Value::SchemeString(s.clone())),
        ExprKind::Symbol(s) => Ok(Value::Symbol(s.clone())),
        ExprKind::List(elements) => {
            let mut result = Value::Nil;
            for elem in elements.iter().rev() {
                let val = expr_to_value(elem)?;
                result = Value::Pair(Box::new(val), Box::new(result));
            }
            Ok(result)
        }
    }
}

fn eval_lambda(args: &[Expr], span: &Span, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalErrorKind::Parse {
            message: "lambda requires parameters and body".into(),
        }
        .at(span));
    }
    let params = match &args[0].kind {
        ExprKind::List(param_exprs) => param_exprs
            .iter()
            .map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalErrorKind::Parse {
                    message: "lambda: expected symbol as parameter".into(),
                }
                .at(span)),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "lambda: expected parameter list".into(),
            }
            .at(span))
        }
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        env: env.clone(),
    })
}

fn eval_and(args: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env, output)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(false));
    }
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env, output)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_not(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "not".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let val = eval(&args[0], env, output)?;
    Ok(Value::Boolean(!val.is_truthy()))
}

fn require_integer(v: &Value, span: &Span) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalErrorKind::Type {
            expected: "integer".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn require_string<'a>(v: &'a Value, span: &Span) -> Result<&'a str, EvalError> {
    match v {
        Value::SchemeString(s) => Ok(s.as_str()),
        other => Err(EvalErrorKind::Type {
            expected: "string".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_add(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    let mut sum: i64 = 0;
    for arg in args {
        sum += require_integer(arg, span)?;
    }
    Ok(Value::Integer(sum))
}

fn eval_sub(args: &[Value], name: &str, span: &Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalErrorKind::Arity {
            name: name.into(),
            expected: "at least 1".into(),
            got: 0,
        }
        .at(span));
    }
    let first = require_integer(&args[0], span)?;
    if args.len() == 1 {
        return Ok(Value::Integer(-first));
    }
    let mut result = first;
    for arg in &args[1..] {
        result -= require_integer(arg, span)?;
    }
    Ok(Value::Integer(result))
}

fn eval_mul(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    let mut product: i64 = 1;
    for arg in args {
        product *= require_integer(arg, span)?;
    }
    Ok(Value::Integer(product))
}

fn eval_div(args: &[Value], name: &str, span: &Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalErrorKind::Arity {
            name: name.into(),
            expected: "at least 1".into(),
            got: 0,
        }
        .at(span));
    }
    let first = require_integer(&args[0], span)?;
    if args.len() == 1 {
        if first == 0 {
            return Err(EvalErrorKind::DivisionByZero.at(span));
        }
        return Ok(Value::Integer(1 / first));
    }
    let mut result = first;
    for arg in &args[1..] {
        let divisor = require_integer(arg, span)?;
        if divisor == 0 {
            return Err(EvalErrorKind::DivisionByZero.at(span));
        }
        result /= divisor;
    }
    Ok(Value::Integer(result))
}

fn eval_cmp(
    args: &[Value],
    name: &str,
    span: &Span,
    cmp: fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalErrorKind::Arity {
            name: name.into(),
            expected: "at least 2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let mut prev = require_integer(&args[0], span)?;
    for arg in &args[1..] {
        let curr = require_integer(arg, span)?;
        if !cmp(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

fn eval_cons(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "cons".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    Ok(Value::Pair(
        Box::new(args[0].clone()),
        Box::new(args[1].clone()),
    ))
}

fn eval_car(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "car".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Pair(car, _) => Ok(*car.clone()),
        other => Err(EvalErrorKind::Type {
            expected: "pair".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_cdr(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "cdr".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Pair(_, cdr) => Ok(*cdr.clone()),
        other => Err(EvalErrorKind::Type {
            expected: "pair".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_null_pred(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "null?".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    Ok(Value::Boolean(matches!(&args[0], Value::Nil)))
}

fn eval_list_builtin(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Nil;
    for arg in args.iter().rev() {
        result = Value::Pair(Box::new(arg.clone()), Box::new(result));
    }
    Ok(result)
}

fn eval_length(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "length".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let mut count: i64 = 0;
    let mut current = &args[0];
    loop {
        match current {
            Value::Nil => return Ok(Value::Integer(count)),
            Value::Pair(_, cdr) => {
                count += 1;
                current = cdr;
            }
            other => {
                return Err(EvalErrorKind::Type {
                    expected: "proper list".into(),
                    got: format!("{other}"),
                }
                .at(span))
            }
        }
    }
}

fn eval_let(
    args: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalErrorKind::Parse {
            message: "let requires bindings and body".into(),
        }
        .at(span));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "let: expected binding list".into(),
            }
            .at(span))
        }
    };
    let let_env = Env::with_parent(env);
    for binding in bindings {
        match &binding.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => {
                        return Err(EvalErrorKind::Parse {
                            message: "let: expected symbol in binding".into(),
                        }
                        .at(span))
                    }
                };
                let val = eval(&pair[1], env, output)?;
                let_env.define(name, val);
            }
            _ => {
                return Err(EvalErrorKind::Parse {
                    message: "let: invalid binding".into(),
                }
                .at(span))
            }
        }
    }
    let mut result = Value::Nil;
    for expr in &args[1..] {
        result = eval(expr, &let_env, output)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Nil;
    for expr in args {
        result = eval(expr, env, output)?;
    }
    Ok(result)
}

fn eval_cond(
    clauses: &[Expr],
    span: &Span,
    env: &Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                // Check for else clause
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        let mut result = Value::Nil;
                        for expr in &parts[1..] {
                            result = eval(expr, env, output)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env, output)?;
                if test.is_truthy() {
                    let mut result = Value::Nil;
                    for expr in &parts[1..] {
                        result = eval(expr, env, output)?;
                    }
                    return Ok(result);
                }
            }
            _ => {
                return Err(EvalErrorKind::Parse {
                    message: "cond: invalid clause".into(),
                }
                .at(span))
            }
        }
    }
    Ok(Value::Nil)
}

// ===== L05: I/O builtins =====

fn eval_display(args: &[Value], span: &Span, output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "display".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    args[0].display_fmt(output);
    Ok(Value::Nil)
}

fn eval_write(args: &[Value], span: &Span, output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "write".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    args[0].write_fmt(output);
    Ok(Value::Nil)
}

fn eval_newline(args: &[Value], span: &Span, output: &mut String) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalErrorKind::Arity {
            name: "newline".into(),
            expected: "0".into(),
            got: args.len(),
        }
        .at(span));
    }
    output.push('\n');
    Ok(Value::Nil)
}

// ===== L05: String/Symbol builtins =====

fn eval_string_append(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    let mut result = String::new();
    for arg in args {
        result.push_str(require_string(arg, span)?);
    }
    Ok(Value::SchemeString(result))
}

fn eval_string_length(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "string-length".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let s = require_string(&args[0], span)?;
    Ok(Value::Integer(s.len() as i64))
}

fn eval_substring(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalErrorKind::Arity {
            name: "substring".into(),
            expected: "3".into(),
            got: args.len(),
        }
        .at(span));
    }
    let s = require_string(&args[0], span)?;
    let start = require_integer(&args[1], span)? as usize;
    let end = require_integer(&args[2], span)? as usize;
    if start > end || end > s.len() {
        return Err(EvalErrorKind::Type {
            expected: "valid substring indices".into(),
            got: format!("start={start}, end={end}, length={}", s.len()),
        }
        .at(span));
    }
    Ok(Value::SchemeString(s[start..end].to_string()))
}

fn eval_string_to_number(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "string->number".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let s = require_string(&args[0], span)?;
    match s.parse::<i64>() {
        Ok(n) => Ok(Value::Integer(n)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn eval_number_to_string(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "number->string".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let n = require_integer(&args[0], span)?;
    Ok(Value::SchemeString(n.to_string()))
}

fn eval_symbol_to_string(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "symbol->string".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    match &args[0] {
        Value::Symbol(s) => Ok(Value::SchemeString(s.clone())),
        other => Err(EvalErrorKind::Type {
            expected: "symbol".into(),
            got: format!("{other}"),
        }
        .at(span)),
    }
}

fn eval_string_to_symbol(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalErrorKind::Arity {
            name: "string->symbol".into(),
            expected: "1".into(),
            got: args.len(),
        }
        .at(span));
    }
    let s = require_string(&args[0], span)?;
    Ok(Value::Symbol(s.to_string()))
}

fn eval_string_ref(args: &[Value], span: &Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalErrorKind::Arity {
            name: "string-ref".into(),
            expected: "2".into(),
            got: args.len(),
        }
        .at(span));
    }
    let s = require_string(&args[0], span)?;
    let idx = require_integer(&args[1], span)? as usize;
    match s.chars().nth(idx) {
        Some(c) => Ok(Value::Char(c)),
        None => Err(EvalErrorKind::Type {
            expected: "valid string index".into(),
            got: format!("index {idx} for string of length {}", s.len()),
        }
        .at(span)),
    }
}
