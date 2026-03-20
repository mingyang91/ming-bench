pub mod error;

pub use error::EvalError;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
}

#[derive(Debug, Clone, PartialEq)]
struct Expr {
    kind: ExprKind,
    line: usize,
    col: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum ExprKind {
    Value(Value),
    List(Vec<Expr>),
}

type Env = HashMap<String, Value>;

fn slice_offset(original: &str, slice: &str) -> usize {
    slice.as_ptr() as usize - original.as_ptr() as usize
}

fn offset_to_pos(original: &str, offset: usize) -> (usize, usize) {
    let prefix = &original[..offset];
    let line = prefix.matches('\n').count() + 1;
    let col = match prefix.rfind('\n') {
        Some(nl) => offset - nl,
        None => offset + 1,
    };
    (line, col)
}

fn err_at(msg: impl std::fmt::Display, line: usize, col: usize) -> EvalError {
    EvalError::Parse(format!("{} at {}:{}", msg, line, col))
}

impl Value {
    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::Char(c) => format!("#\\{}", c),
            Value::List(items) => {
                let parts: Vec<String> = items.iter().map(|v| v.to_scheme_string()).collect();
                format!("({})", parts.join(" "))
            }
            Value::Lambda { .. } => "#<procedure>".to_string(),
        }
    }

    fn display_string(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            other => other.to_scheme_string(),
        }
    }
}

fn parse_atom(input: &str) -> Value {
    if let Ok(n) = input.parse::<i64>() {
        return Value::Integer(n);
    }
    if input == "#t" {
        return Value::Boolean(true);
    }
    if input == "#f" {
        return Value::Boolean(false);
    }
    if input.starts_with("#\\") && input.len() > 2 {
        let rest = &input[2..];
        match rest {
            "space" => return Value::Char(' '),
            "newline" => return Value::Char('\n'),
            "tab" => return Value::Char('\t'),
            _ => {
                let mut chars = rest.chars();
                if let Some(c) = chars.next() {
                    if chars.next().is_none() {
                        return Value::Char(c);
                    }
                }
            }
        }
    }
    Value::Symbol(input.to_string())
}

fn parse_string(input: &str) -> Result<(Value, &str), EvalError> {
    let mut chars = input.char_indices();
    while let Some((i, ch)) = chars.next() {
        if ch == '\\' {
            chars.next();
        } else if ch == '"' {
            let s = &input[..i];
            return Ok((Value::Str(s.to_string()), &input[i + 1..]));
        }
    }
    Err(EvalError::Parse("unterminated string".to_string()))
}

fn parse_expr<'a>(input: &'a str, original: &str) -> Result<(Expr, &'a str), EvalError> {
    let trimmed = input.trim_start();
    if trimmed.is_empty() {
        return Err(EvalError::Parse("unexpected end of input".to_string()));
    }
    let offset = slice_offset(original, trimmed);
    let (line, col) = offset_to_pos(original, offset);

    if trimmed.starts_with('\'') {
        let (inner, rest) = parse_expr(&trimmed[1..], original)?;
        let quote_sym = Expr {
            kind: ExprKind::Value(Value::Symbol("quote".to_string())),
            line,
            col,
        };
        return Ok((
            Expr {
                kind: ExprKind::List(vec![quote_sym, inner]),
                line,
                col,
            },
            rest,
        ));
    }
    if trimmed.starts_with('"') {
        let (val, rest) = parse_string(&trimmed[1..])?;
        return Ok((Expr { kind: ExprKind::Value(val), line, col }, rest));
    }
    if trimmed.starts_with('(') {
        return parse_list(&trimmed[1..], original, line, col);
    }
    let end = trimmed
        .find(|c: char| c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == '\'')
        .unwrap_or(trimmed.len());
    let token = &trimmed[..end];
    let rest = &trimmed[end..];
    Ok((Expr { kind: ExprKind::Value(parse_atom(token)), line, col }, rest))
}

fn parse_list<'a>(
    input: &'a str,
    original: &str,
    list_line: usize,
    list_col: usize,
) -> Result<(Expr, &'a str), EvalError> {
    let mut items = Vec::new();
    let mut rest = input;
    loop {
        rest = rest.trim_start();
        if rest.is_empty() {
            return Err(EvalError::Parse("unterminated list".to_string()));
        }
        if rest.starts_with(')') {
            return Ok((
                Expr {
                    kind: ExprKind::List(items),
                    line: list_line,
                    col: list_col,
                },
                &rest[1..],
            ));
        }
        let (expr, r) = parse_expr(rest, original)?;
        items.push(expr);
        rest = r;
    }
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Value(v) => v.clone(),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
    }
}

fn eval(expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Value(Value::Symbol(name)) => {
            if let Some(v) = env.get(name) {
                Ok(v.clone())
            } else {
                Err(err_at(format!("undefined variable: {}", name), expr.line, expr.col))
            }
        }
        ExprKind::Value(v) => Ok(v.clone()),
        ExprKind::List(items) => {
            if items.is_empty() {
                return Err(err_at("empty application", expr.line, expr.col));
            }
            if let ExprKind::Value(Value::Symbol(name)) = &items[0].kind {
                match name.as_str() {
                    "define" => eval_define(items, expr, env, output),
                    "if" => eval_if(items, expr, env, output),
                    "quote" => {
                        if items.len() != 2 {
                            return Err(err_at("quote requires 1 argument", expr.line, expr.col));
                        }
                        Ok(expr_to_value(&items[1]))
                    }
                    "lambda" => eval_lambda(items, expr, env),
                    "begin" => {
                        let mut result = Value::Symbol("".to_string());
                        for e in &items[1..] {
                            result = eval(e, env, output)?;
                        }
                        Ok(result)
                    }
                    "let" => eval_let(items, expr, env, output),
                    "cond" => eval_cond(items, expr, env, output),
                    "and" => eval_and(&items[1..], env, output),
                    "or" => eval_or(&items[1..], env, output),
                    "string-set!" => eval_string_set(items, expr, env, output),
                    "display" => {
                        if items.len() != 2 {
                            return Err(err_at("display requires 1 argument", expr.line, expr.col));
                        }
                        let val = eval(&items[1], env, output)?;
                        output.push_str(&val.display_string());
                        Ok(Value::Symbol("".to_string()))
                    }
                    "write" => {
                        if items.len() != 2 {
                            return Err(err_at("write requires 1 argument", expr.line, expr.col));
                        }
                        let val = eval(&items[1], env, output)?;
                        output.push_str(&val.to_scheme_string());
                        Ok(Value::Symbol("".to_string()))
                    }
                    "newline" => {
                        if items.len() != 1 {
                            return Err(err_at("newline takes no arguments", expr.line, expr.col));
                        }
                        output.push('\n');
                        Ok(Value::Symbol("".to_string()))
                    }
                    _ => {
                        if let Some(func_val) = env.get(name).cloned() {
                            if let Value::Lambda { .. } = &func_val {
                                let mut eval_args = Vec::new();
                                for a in &items[1..] {
                                    eval_args.push(eval(a, env, output)?);
                                }
                                return apply_lambda(
                                    &func_val,
                                    &eval_args,
                                    Some(&*env),
                                    expr.line,
                                    expr.col,
                                    output,
                                );
                            }
                        }
                        apply_builtin(name, &items[1..], env, expr.line, expr.col, output)
                    }
                }
            } else {
                let func_val = eval(&items[0], env, output)?;
                match func_val {
                    Value::Lambda { .. } => {
                        let mut eval_args = Vec::new();
                        for a in &items[1..] {
                            eval_args.push(eval(a, env, output)?);
                        }
                        apply_lambda(&func_val, &eval_args, None, expr.line, expr.col, output)
                    }
                    _ => Err(err_at(
                        format!("not a procedure: {}", func_val.to_scheme_string()),
                        expr.line,
                        expr.col,
                    )),
                }
            }
        }
    }
}

fn eval_define(items: &[Expr], expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if items.len() < 3 {
        return Err(err_at("define requires 2 arguments", expr.line, expr.col));
    }
    match &items[1].kind {
        ExprKind::Value(Value::Symbol(s)) => {
            if items.len() != 3 {
                return Err(err_at("define requires 2 arguments", expr.line, expr.col));
            }
            let val = eval(&items[2], env, output)?;
            env.insert(s.clone(), val);
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(err_at("define: empty signature", expr.line, expr.col));
            }
            let fname = match &sig[0].kind {
                ExprKind::Value(Value::Symbol(s)) => s.clone(),
                _ => {
                    return Err(err_at(
                        "define: name must be a symbol",
                        expr.line,
                        expr.col,
                    ))
                }
            };
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|e| match &e.kind {
                    ExprKind::Value(Value::Symbol(s)) => Ok(s.clone()),
                    _ => Err(err_at(
                        "define: parameter must be a symbol",
                        e.line,
                        e.col,
                    )),
                })
                .collect::<Result<_, _>>()?;
            let body = items[2..].to_vec();
            let lambda = Value::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env.insert(fname, lambda);
        }
        _ => {
            return Err(err_at(
                "define: first argument must be a symbol or list",
                expr.line,
                expr.col,
            ))
        }
    }
    Ok(Value::Symbol("".to_string()))
}

fn eval_if(items: &[Expr], expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if items.len() < 3 || items.len() > 4 {
        return Err(err_at(
            "if requires 2 or 3 arguments",
            expr.line,
            expr.col,
        ));
    }
    let cond = eval(&items[1], env, output)?;
    if !is_falsy(&cond) {
        eval(&items[2], env, output)
    } else if items.len() == 4 {
        eval(&items[3], env, output)
    } else {
        Ok(Value::Symbol("".to_string()))
    }
}

fn eval_lambda(items: &[Expr], expr: &Expr, env: &mut Env) -> Result<Value, EvalError> {
    if items.len() < 3 {
        return Err(err_at(
            "lambda requires params and body",
            expr.line,
            expr.col,
        ));
    }
    let params = match &items[1].kind {
        ExprKind::List(p) => p
            .iter()
            .map(|e| match &e.kind {
                ExprKind::Value(Value::Symbol(s)) => Ok(s.clone()),
                _ => Err(err_at(
                    "lambda: parameter must be a symbol",
                    e.line,
                    e.col,
                )),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => {
            return Err(err_at(
                "lambda: first argument must be a parameter list",
                expr.line,
                expr.col,
            ))
        }
    };
    let body = items[2..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        env: env.clone(),
    })
}

fn eval_let(items: &[Expr], expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if items.len() < 3 {
        return Err(err_at(
            "let requires bindings and body",
            expr.line,
            expr.col,
        ));
    }
    let bindings = match &items[1].kind {
        ExprKind::List(b) => b,
        _ => {
            return Err(err_at(
                "let: first argument must be a list of bindings",
                expr.line,
                expr.col,
            ))
        }
    };
    let mut local_env = env.clone();
    for binding in bindings {
        match &binding.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let bname = match &pair[0].kind {
                    ExprKind::Value(Value::Symbol(s)) => s.clone(),
                    _ => {
                        return Err(err_at(
                            "let: binding name must be a symbol",
                            pair[0].line,
                            pair[0].col,
                        ))
                    }
                };
                let val = eval(&pair[1], env, output)?;
                local_env.insert(bname, val);
            }
            _ => {
                return Err(err_at(
                    "let: each binding must be (name value)",
                    binding.line,
                    binding.col,
                ))
            }
        }
    }
    let mut result = Value::Symbol("".to_string());
    for e in &items[2..] {
        result = eval(e, &mut local_env, output)?;
    }
    Ok(result)
}

fn eval_cond(items: &[Expr], expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let _ = expr; // position available if needed
    for clause in &items[1..] {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                if matches!(&parts[0].kind, ExprKind::Value(Value::Symbol(s)) if s == "else") {
                    let mut result = Value::Symbol("".to_string());
                    for e in &parts[1..] {
                        result = eval(e, env, output)?;
                    }
                    return Ok(result);
                }
                let test = eval(&parts[0], env, output)?;
                if !is_falsy(&test) {
                    let mut result = Value::Symbol("".to_string());
                    for e in &parts[1..] {
                        result = eval(e, env, output)?;
                    }
                    return Ok(result);
                }
            }
            _ => {
                return Err(err_at(
                    "cond: invalid clause",
                    clause.line,
                    clause.col,
                ))
            }
        }
    }
    Ok(Value::Symbol("".to_string()))
}

fn eval_string_set(items: &[Expr], expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if items.len() != 4 {
        return Err(err_at("string-set! requires 3 arguments", expr.line, expr.col));
    }
    let var_name = match &items[1].kind {
        ExprKind::Value(Value::Symbol(s)) => s.clone(),
        _ => return Err(err_at("string-set!: first argument must be a variable", expr.line, expr.col)),
    };
    let idx = expect_integer(&eval(&items[2], env, output)?, expr.line, expr.col)? as usize;
    let ch = match eval(&items[3], env, output)? {
        Value::Char(c) => c,
        _ => return Err(err_at("string-set!: third argument must be a char", expr.line, expr.col)),
    };
    let s = match env.get(&var_name) {
        Some(Value::Str(s)) => s.clone(),
        _ => return Err(err_at("string-set!: variable must contain a string", expr.line, expr.col)),
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(err_at("string-set!: index out of range", expr.line, expr.col));
    }
    chars[idx] = ch;
    let new_s: String = chars.into_iter().collect();
    env.insert(var_name, Value::Str(new_s));
    Ok(Value::Symbol("".to_string()))
}

enum EvalResult {
    Done(Value),
    TailCall { func: Value, args: Vec<Value> },
}

fn eval_tail(expr: &Expr, env: &mut Env, output: &mut String) -> Result<EvalResult, EvalError> {
    match &expr.kind {
        ExprKind::List(items) if !items.is_empty() => {
            if let ExprKind::Value(Value::Symbol(name)) = &items[0].kind {
                match name.as_str() {
                    "if" => {
                        if items.len() < 3 || items.len() > 4 {
                            return Err(err_at("if requires 2 or 3 arguments", expr.line, expr.col));
                        }
                        let cond = eval(&items[1], env, output)?;
                        if !is_falsy(&cond) {
                            eval_tail(&items[2], env, output)
                        } else if items.len() == 4 {
                            eval_tail(&items[3], env, output)
                        } else {
                            Ok(EvalResult::Done(Value::Symbol("".to_string())))
                        }
                    }
                    "begin" => {
                        if items.len() <= 1 {
                            return Ok(EvalResult::Done(Value::Symbol("".to_string())));
                        }
                        for e in &items[1..items.len() - 1] {
                            eval(e, env, output)?;
                        }
                        eval_tail(&items[items.len() - 1], env, output)
                    }
                    "cond" => {
                        for clause in &items[1..] {
                            match &clause.kind {
                                ExprKind::List(parts) if parts.len() >= 2 => {
                                    if matches!(&parts[0].kind, ExprKind::Value(Value::Symbol(s)) if s == "else")
                                    {
                                        for e in &parts[1..parts.len() - 1] {
                                            eval(e, env, output)?;
                                        }
                                        return eval_tail(&parts[parts.len() - 1], env, output);
                                    }
                                    let test = eval(&parts[0], env, output)?;
                                    if !is_falsy(&test) {
                                        for e in &parts[1..parts.len() - 1] {
                                            eval(e, env, output)?;
                                        }
                                        return eval_tail(&parts[parts.len() - 1], env, output);
                                    }
                                }
                                _ => {
                                    return Err(err_at(
                                        "cond: invalid clause",
                                        clause.line,
                                        clause.col,
                                    ))
                                }
                            }
                        }
                        Ok(EvalResult::Done(Value::Symbol("".to_string())))
                    }
                    "let" => {
                        if items.len() < 3 {
                            return Err(err_at(
                                "let requires bindings and body",
                                expr.line,
                                expr.col,
                            ));
                        }
                        let bindings = match &items[1].kind {
                            ExprKind::List(b) => b,
                            _ => {
                                return Err(err_at(
                                    "let: first argument must be a list of bindings",
                                    expr.line,
                                    expr.col,
                                ))
                            }
                        };
                        let mut local_env = env.clone();
                        for binding in bindings {
                            match &binding.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    let bname = match &pair[0].kind {
                                        ExprKind::Value(Value::Symbol(s)) => s.clone(),
                                        _ => {
                                            return Err(err_at(
                                                "let: binding name must be a symbol",
                                                pair[0].line,
                                                pair[0].col,
                                            ))
                                        }
                                    };
                                    let val = eval(&pair[1], env, output)?;
                                    local_env.insert(bname, val);
                                }
                                _ => {
                                    return Err(err_at(
                                        "let: each binding must be (name value)",
                                        binding.line,
                                        binding.col,
                                    ))
                                }
                            }
                        }
                        for e in &items[2..items.len() - 1] {
                            eval(e, &mut local_env, output)?;
                        }
                        eval_tail(&items[items.len() - 1], &mut local_env, output)
                    }
                    "define" | "quote" | "lambda" | "and" | "or" | "string-set!" | "display"
                    | "write" | "newline" => Ok(EvalResult::Done(eval(expr, env, output)?)),
                    _ => {
                        if let Some(func_val) = env.get(name).cloned() {
                            if let Value::Lambda { .. } = &func_val {
                                let mut eval_args = Vec::new();
                                for a in &items[1..] {
                                    eval_args.push(eval(a, env, output)?);
                                }
                                return Ok(EvalResult::TailCall {
                                    func: func_val,
                                    args: eval_args,
                                });
                            }
                        }
                        Ok(EvalResult::Done(apply_builtin(
                            name,
                            &items[1..],
                            env,
                            expr.line,
                            expr.col,
                            output,
                        )?))
                    }
                }
            } else {
                let func_val = eval(&items[0], env, output)?;
                if let Value::Lambda { .. } = &func_val {
                    let mut eval_args = Vec::new();
                    for a in &items[1..] {
                        eval_args.push(eval(a, env, output)?);
                    }
                    return Ok(EvalResult::TailCall {
                        func: func_val,
                        args: eval_args,
                    });
                }
                Ok(EvalResult::Done(eval(expr, env, output)?))
            }
        }
        _ => Ok(EvalResult::Done(eval(expr, env, output)?)),
    }
}

fn apply_lambda(
    func: &Value,
    args: &[Value],
    caller_env: Option<&Env>,
    call_line: usize,
    call_col: usize,
    output: &mut String,
) -> Result<Value, EvalError> {
    let mut current_func = func.clone();
    let mut current_args = args.to_vec();
    let mut owned_caller_env = caller_env.cloned();

    loop {
        if let Value::Lambda { params, body, env } = &current_func {
            if params.len() != current_args.len() {
                return Err(err_at(
                    format!(
                        "expected {} arguments, got {}",
                        params.len(),
                        current_args.len()
                    ),
                    call_line,
                    call_col,
                ));
            }
            let mut local_env = env.clone();
            if let Some(ref ce) = owned_caller_env {
                for (k, v) in ce {
                    if !local_env.contains_key(k) {
                        local_env.insert(k.clone(), v.clone());
                    }
                }
            }
            for (p, a) in params.iter().zip(current_args.iter()) {
                local_env.insert(p.clone(), a.clone());
            }

            if body.is_empty() {
                return Ok(Value::Symbol("".to_string()));
            }

            for expr in &body[..body.len() - 1] {
                eval(expr, &mut local_env, output)?;
            }

            match eval_tail(&body[body.len() - 1], &mut local_env, output)? {
                EvalResult::Done(v) => return Ok(v),
                EvalResult::TailCall {
                    func: next_func,
                    args: next_args,
                } => {
                    owned_caller_env = Some(local_env);
                    current_func = next_func;
                    current_args = next_args;
                }
            }
        } else {
            return Err(err_at("not a procedure", call_line, call_col));
        }
    }
}

fn apply_builtin(
    name: &str,
    args: &[Expr],
    env: &mut Env,
    call_line: usize,
    call_col: usize,
    output: &mut String,
) -> Result<Value, EvalError> {
    let mut eval_args = Vec::with_capacity(args.len());
    for a in args {
        eval_args.push(eval(a, env, output)?);
    }
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in &eval_args {
                sum += expect_integer(a, call_line, call_col)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if eval_args.is_empty() {
                return Err(err_at(
                    "- requires at least 1 argument",
                    call_line,
                    call_col,
                ));
            }
            if eval_args.len() == 1 {
                return Ok(Value::Integer(-expect_integer(
                    &eval_args[0],
                    call_line,
                    call_col,
                )?));
            }
            let mut result = expect_integer(&eval_args[0], call_line, call_col)?;
            for a in &eval_args[1..] {
                result -= expect_integer(a, call_line, call_col)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in &eval_args {
                product *= expect_integer(a, call_line, call_col)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if eval_args.is_empty() {
                return Err(err_at(
                    "/ requires at least 1 argument",
                    call_line,
                    call_col,
                ));
            }
            let mut result = expect_integer(&eval_args[0], call_line, call_col)?;
            for a in &eval_args[1..] {
                let d = expect_integer(a, call_line, call_col)?;
                if d == 0 {
                    return Err(err_at("division by zero", call_line, call_col));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if eval_args.len() != 2 {
                return Err(err_at("< requires 2 arguments", call_line, call_col));
            }
            Ok(Value::Boolean(
                expect_integer(&eval_args[0], call_line, call_col)?
                    < expect_integer(&eval_args[1], call_line, call_col)?,
            ))
        }
        ">" => {
            if eval_args.len() != 2 {
                return Err(err_at("> requires 2 arguments", call_line, call_col));
            }
            Ok(Value::Boolean(
                expect_integer(&eval_args[0], call_line, call_col)?
                    > expect_integer(&eval_args[1], call_line, call_col)?,
            ))
        }
        "=" => {
            if eval_args.len() != 2 {
                return Err(err_at("= requires 2 arguments", call_line, call_col));
            }
            Ok(Value::Boolean(
                expect_integer(&eval_args[0], call_line, call_col)?
                    == expect_integer(&eval_args[1], call_line, call_col)?,
            ))
        }
        "<=" => {
            if eval_args.len() != 2 {
                return Err(err_at("<= requires 2 arguments", call_line, call_col));
            }
            Ok(Value::Boolean(
                expect_integer(&eval_args[0], call_line, call_col)?
                    <= expect_integer(&eval_args[1], call_line, call_col)?,
            ))
        }
        ">=" => {
            if eval_args.len() != 2 {
                return Err(err_at(">= requires 2 arguments", call_line, call_col));
            }
            Ok(Value::Boolean(
                expect_integer(&eval_args[0], call_line, call_col)?
                    >= expect_integer(&eval_args[1], call_line, call_col)?,
            ))
        }
        "not" => {
            if eval_args.len() != 1 {
                return Err(err_at("not requires 1 argument", call_line, call_col));
            }
            Ok(Value::Boolean(is_falsy(&eval_args[0])))
        }
        "cons" => {
            if eval_args.len() != 2 {
                return Err(err_at("cons requires 2 arguments", call_line, call_col));
            }
            match &eval_args[1] {
                Value::List(items) => {
                    let mut new_list = vec![eval_args[0].clone()];
                    new_list.extend(items.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => Err(err_at(
                    "cons: second argument must be a list",
                    call_line,
                    call_col,
                )),
            }
        }
        "car" => {
            if eval_args.len() != 1 {
                return Err(err_at("car requires 1 argument", call_line, call_col));
            }
            match &eval_args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                _ => Err(err_at(
                    "car: argument must be a non-empty list",
                    call_line,
                    call_col,
                )),
            }
        }
        "cdr" => {
            if eval_args.len() != 1 {
                return Err(err_at("cdr requires 1 argument", call_line, call_col));
            }
            match &eval_args[0] {
                Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
                _ => Err(err_at(
                    "cdr: argument must be a non-empty list",
                    call_line,
                    call_col,
                )),
            }
        }
        "null?" => {
            if eval_args.len() != 1 {
                return Err(err_at("null? requires 1 argument", call_line, call_col));
            }
            Ok(Value::Boolean(
                matches!(&eval_args[0], Value::List(items) if items.is_empty()),
            ))
        }
        "list" => Ok(Value::List(eval_args)),
        "map" => {
            if eval_args.len() != 2 {
                return Err(err_at("map requires 2 arguments", call_line, call_col));
            }
            let func = eval_args[0].clone();
            match &eval_args[1] {
                Value::List(items) => {
                    let mut result = Vec::new();
                    for item in items {
                        let val = apply_lambda(&func, &[item.clone()], Some(&*env), call_line, call_col, output)?;
                        result.push(val);
                    }
                    Ok(Value::List(result))
                }
                _ => Err(err_at("map: second argument must be a list", call_line, call_col)),
            }
        }
        "length" => {
            if eval_args.len() != 1 {
                return Err(err_at("length requires 1 argument", call_line, call_col));
            }
            match &eval_args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(err_at(
                    "length: argument must be a list",
                    call_line,
                    call_col,
                )),
            }
        }
        "string?" => {
            if eval_args.len() != 1 {
                return Err(err_at(
                    "string? requires 1 argument",
                    call_line,
                    call_col,
                ));
            }
            Ok(Value::Boolean(matches!(&eval_args[0], Value::Str(_))))
        }
        "number?" => {
            if eval_args.len() != 1 {
                return Err(err_at(
                    "number? requires 1 argument",
                    call_line,
                    call_col,
                ));
            }
            Ok(Value::Boolean(matches!(&eval_args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if eval_args.len() != 1 {
                return Err(err_at(
                    "boolean? requires 1 argument",
                    call_line,
                    call_col,
                ));
            }
            Ok(Value::Boolean(matches!(&eval_args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if eval_args.len() != 1 {
                return Err(err_at(
                    "pair? requires 1 argument",
                    call_line,
                    call_col,
                ));
            }
            Ok(Value::Boolean(
                matches!(&eval_args[0], Value::List(items) if !items.is_empty()),
            ))
        }
        "symbol?" => {
            if eval_args.len() != 1 {
                return Err(err_at(
                    "symbol? requires 1 argument",
                    call_line,
                    call_col,
                ));
            }
            Ok(Value::Boolean(matches!(&eval_args[0], Value::Symbol(_))))
        }
        "char?" => {
            if eval_args.len() != 1 {
                return Err(err_at("char? requires 1 argument", call_line, call_col));
            }
            Ok(Value::Boolean(matches!(&eval_args[0], Value::Char(_))))
        }
        "string-append" => {
            let mut result = String::new();
            for a in &eval_args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(err_at("string-append: expected string", call_line, call_col)),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if eval_args.len() != 1 {
                return Err(err_at("string-length requires 1 argument", call_line, call_col));
            }
            match &eval_args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(err_at("string-length: expected string", call_line, call_col)),
            }
        }
        "substring" => {
            if eval_args.len() != 3 {
                return Err(err_at("substring requires 3 arguments", call_line, call_col));
            }
            let s = match &eval_args[0] {
                Value::Str(s) => s,
                _ => return Err(err_at("substring: expected string", call_line, call_col)),
            };
            let start = expect_integer(&eval_args[1], call_line, call_col)? as usize;
            let end = expect_integer(&eval_args[2], call_line, call_col)? as usize;
            if start > end || end > s.len() {
                return Err(err_at("substring: index out of range", call_line, call_col));
            }
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if eval_args.len() != 1 {
                return Err(err_at("string->number requires 1 argument", call_line, call_col));
            }
            match &eval_args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(err_at("string->number: expected string", call_line, call_col)),
            }
        }
        "number->string" => {
            if eval_args.len() != 1 {
                return Err(err_at("number->string requires 1 argument", call_line, call_col));
            }
            let n = expect_integer(&eval_args[0], call_line, call_col)?;
            Ok(Value::Str(n.to_string()))
        }
        "symbol->string" => {
            if eval_args.len() != 1 {
                return Err(err_at("symbol->string requires 1 argument", call_line, call_col));
            }
            match &eval_args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(err_at("symbol->string: expected symbol", call_line, call_col)),
            }
        }
        "string->symbol" => {
            if eval_args.len() != 1 {
                return Err(err_at("string->symbol requires 1 argument", call_line, call_col));
            }
            match &eval_args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(err_at("string->symbol: expected string", call_line, call_col)),
            }
        }
        "string-copy" => {
            if eval_args.len() != 1 {
                return Err(err_at("string-copy requires 1 argument", call_line, call_col));
            }
            match &eval_args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(err_at("string-copy: expected string", call_line, call_col)),
            }
        }
        "string-ref" => {
            if eval_args.len() != 2 {
                return Err(err_at("string-ref requires 2 arguments", call_line, call_col));
            }
            let s = match &eval_args[0] {
                Value::Str(s) => s,
                _ => return Err(err_at("string-ref: expected string", call_line, call_col)),
            };
            let idx = expect_integer(&eval_args[1], call_line, call_col)? as usize;
            match s.chars().nth(idx) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(err_at("string-ref: index out of range", call_line, call_col)),
            }
        }
        "string->list" => {
            if eval_args.len() != 1 {
                return Err(err_at("string->list requires 1 argument", call_line, call_col));
            }
            match &eval_args[0] {
                Value::Str(s) => Ok(Value::List(s.chars().map(Value::Char).collect())),
                _ => Err(err_at("string->list: expected string", call_line, call_col)),
            }
        }
        "list->string" => {
            if eval_args.len() != 1 {
                return Err(err_at("list->string requires 1 argument", call_line, call_col));
            }
            match &eval_args[0] {
                Value::List(items) => {
                    let mut s = String::new();
                    for item in items {
                        match item {
                            Value::Char(c) => s.push(*c),
                            _ => return Err(err_at("list->string: list must contain only characters", call_line, call_col)),
                        }
                    }
                    Ok(Value::Str(s))
                }
                _ => Err(err_at("list->string: expected list", call_line, call_col)),
            }
        }
        "char->integer" => {
            if eval_args.len() != 1 {
                return Err(err_at("char->integer requires 1 argument", call_line, call_col));
            }
            match &eval_args[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => Err(err_at("char->integer: expected char", call_line, call_col)),
            }
        }
        "integer->char" => {
            if eval_args.len() != 1 {
                return Err(err_at("integer->char requires 1 argument", call_line, call_col));
            }
            let n = expect_integer(&eval_args[0], call_line, call_col)?;
            match char::from_u32(n as u32) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(err_at("integer->char: invalid code point", call_line, call_col)),
            }
        }
        _ => Err(err_at(
            format!("unknown procedure: {}", name),
            call_line,
            call_col,
        )),
    }
}

fn is_falsy(val: &Value) -> bool {
    matches!(val, Value::Boolean(false))
}

fn eval_and(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env, output)?;
        if is_falsy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(false));
    }
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env, output)?;
        if !is_falsy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn expect_integer(val: &Value, line: usize, col: usize) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(err_at(
            format!("expected integer, got {}", other.to_scheme_string()),
            line,
            col,
        )),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut remaining = input;
    let mut last_value = None;
    let mut env = Env::new();
    let mut output = String::new();
    while !remaining.trim().is_empty() {
        let (expr, rest) = parse_expr(remaining, input)?;
        last_value = Some(eval(&expr, &mut env, &mut output)?);
        remaining = rest;
    }
    match last_value {
        Some(v) => Ok(v.to_scheme_string()),
        None => Err(EvalError::Parse("empty input".to_string())),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut remaining = input;
    let mut last_value = None;
    let mut env = Env::new();
    let mut output = String::new();
    while !remaining.trim().is_empty() {
        let (expr, rest) = parse_expr(remaining, input)?;
        last_value = Some(eval(&expr, &mut env, &mut output)?);
        remaining = rest;
    }
    match last_value {
        Some(v) => Ok((v.to_scheme_string(), output)),
        None => Err(EvalError::Parse("empty input".to_string())),
    }
}

#[cfg(test)]
mod tests;
