pub mod error;

pub use error::EvalError;

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
    Continuation {
        line: usize,
        col: usize,
        expr_index: usize,
    },
}

thread_local! {
    static CONT_RETURN_VALUE: RefCell<Option<Value>> = RefCell::new(None);
    static CALLCC_REPLAY: RefCell<Option<(usize, usize, Value)>> = RefCell::new(None);
    static CURRENT_EXPR_INDEX: Cell<usize> = Cell::new(0);
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Lambda { params: p1, body: b1, .. }, Value::Lambda { params: p2, body: b2, .. }) => {
                p1 == p2 && b1 == b2
            }
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (Value::Continuation { line: l1, col: c1, .. }, Value::Continuation { line: l2, col: c2, .. }) => {
                l1 == l2 && c1 == c2
            }
            _ => false,
        }
    }
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

#[derive(Debug, Clone)]
struct Env {
    bindings: Rc<RefCell<HashMap<String, Value>>>,
    parent: Option<Box<Env>>,
}

impl Env {
    fn new() -> Self {
        Env {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            parent: None,
        }
    }

    fn child(&self) -> Self {
        Env {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            parent: Some(Box::new(self.clone())),
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.bindings.borrow().get(name).cloned() {
            return Some(v);
        }
        self.parent.as_ref().and_then(|p| p.get(name))
    }

    fn define(&self, name: String, val: Value) {
        self.bindings.borrow_mut().insert(name, val);
    }

    fn set(&self, name: &str, val: Value) -> bool {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), val);
            true
        } else if let Some(ref parent) = self.parent {
            parent.set(name, val)
        } else {
            false
        }
    }
}

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
            Value::Builtin(name) => format!("#<builtin:{}>", name),
            Value::Continuation { .. } => "#<continuation>".to_string(),
        }
    }

    fn display_string(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::Builtin(name) => format!("#<builtin:{}>", name),
            Value::Continuation { .. } => "#<continuation>".to_string(),
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

fn eval(expr: &Expr, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Value(Value::Symbol(name)) => {
            if let Some(v) = env.get(name) {
                Ok(v)
            } else if is_builtin_name(name) {
                Ok(Value::Builtin(name.clone()))
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
                    "set!" => eval_set(items, expr, env, output),
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
                    "call/cc" | "call-with-current-continuation" => {
                        if items.len() != 2 {
                            return Err(err_at("call/cc requires 1 argument", expr.line, expr.col));
                        }
                        let func = eval(&items[1], env, output)?;
                        eval_callcc(&func, expr.line, expr.col, output)
                    }
                    _ => {
                        if let Some(func_val) = env.get(name) {
                            match &func_val {
                                Value::Lambda { .. } => {
                                    let mut eval_args = Vec::new();
                                    for a in &items[1..] {
                                        eval_args.push(eval(a, env, output)?);
                                    }
                                    return apply_lambda(
                                        &func_val,
                                        &eval_args,
                                        expr.line,
                                        expr.col,
                                        output,
                                    );
                                }
                                Value::Builtin(bname) => {
                                    let bname = bname.clone();
                                    return apply_builtin(
                                        &bname,
                                        &items[1..],
                                        env,
                                        expr.line,
                                        expr.col,
                                        output,
                                    );
                                }
                                Value::Continuation { .. } => {
                                    let mut eval_args = Vec::new();
                                    for a in &items[1..] {
                                        eval_args.push(eval(a, env, output)?);
                                    }
                                    return call_value(
                                        &func_val,
                                        eval_args,
                                        expr.line,
                                        expr.col,
                                        output,
                                    );
                                }
                                _ => {}
                            }
                        }
                        apply_builtin(name, &items[1..], env, expr.line, expr.col, output)
                    }
                }
            } else {
                let func_val = eval(&items[0], env, output)?;
                let mut eval_args = Vec::new();
                for a in &items[1..] {
                    eval_args.push(eval(a, env, output)?);
                }
                call_value(&func_val, eval_args, expr.line, expr.col, output)
            }
        }
    }
}

fn eval_set(items: &[Expr], expr: &Expr, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if items.len() != 3 {
        return Err(err_at("set! requires 2 arguments", expr.line, expr.col));
    }
    let name = match &items[1].kind {
        ExprKind::Value(Value::Symbol(s)) => s.clone(),
        _ => return Err(err_at("set!: first argument must be a symbol", expr.line, expr.col)),
    };
    let val = eval(&items[2], env, output)?;
    if !env.set(&name, val) {
        return Err(err_at(format!("set!: unbound variable: {}", name), expr.line, expr.col));
    }
    Ok(Value::Symbol("".to_string()))
}

fn parse_params(param_exprs: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i].kind {
            ExprKind::Value(Value::Symbol(s)) if s == "." => {
                if i + 1 < param_exprs.len() {
                    match &param_exprs[i + 1].kind {
                        ExprKind::Value(Value::Symbol(rp)) => {
                            rest_param = Some(rp.clone());
                        }
                        _ => {
                            return Err(err_at(
                                "rest parameter must be a symbol",
                                param_exprs[i + 1].line,
                                param_exprs[i + 1].col,
                            ))
                        }
                    }
                    i += 2;
                } else {
                    return Err(err_at(
                        "expected rest parameter after .",
                        param_exprs[i].line,
                        param_exprs[i].col,
                    ));
                }
            }
            ExprKind::Value(Value::Symbol(s)) => {
                params.push(s.clone());
                i += 1;
            }
            _ => {
                return Err(err_at(
                    "parameter must be a symbol",
                    param_exprs[i].line,
                    param_exprs[i].col,
                ))
            }
        }
    }
    Ok((params, rest_param))
}

fn eval_define(items: &[Expr], expr: &Expr, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if items.len() < 3 {
        return Err(err_at("define requires 2 arguments", expr.line, expr.col));
    }
    match &items[1].kind {
        ExprKind::Value(Value::Symbol(s)) => {
            if items.len() != 3 {
                return Err(err_at("define requires 2 arguments", expr.line, expr.col));
            }
            let val = eval(&items[2], env, output)?;
            env.define(s.clone(), val);
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
            let (params, rest_param) = parse_params(&sig[1..])?;
            let body = items[2..].to_vec();
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                env: env.clone(),
            };
            env.define(fname, lambda);
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

fn eval_if(items: &[Expr], expr: &Expr, env: &Env, output: &mut String) -> Result<Value, EvalError> {
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

fn eval_lambda(items: &[Expr], expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    if items.len() < 3 {
        return Err(err_at(
            "lambda requires params and body",
            expr.line,
            expr.col,
        ));
    }
    let (params, rest_param) = match &items[1].kind {
        ExprKind::List(p) => parse_params(p)?,
        ExprKind::Value(Value::Symbol(s)) => {
            // (lambda args body) — single symbol catches all args
            (vec![], Some(s.clone()))
        }
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
        rest_param,
        body,
        env: env.clone(),
    })
}

fn eval_let(items: &[Expr], expr: &Expr, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if items.len() < 3 {
        return Err(err_at(
            "let requires bindings and body",
            expr.line,
            expr.col,
        ));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Value(Value::Symbol(loop_name)) = &items[1].kind {
        if items.len() < 4 {
            return Err(err_at(
                "named let requires bindings and body",
                expr.line,
                expr.col,
            ));
        }
        let bindings = match &items[2].kind {
            ExprKind::List(b) => b,
            _ => {
                return Err(err_at(
                    "named let: second argument must be a list of bindings",
                    expr.line,
                    expr.col,
                ))
            }
        };
        let mut params = Vec::new();
        let mut init_vals = Vec::new();
        for binding in bindings {
            match &binding.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    let bname = match &pair[0].kind {
                        ExprKind::Value(Value::Symbol(s)) => s.clone(),
                        _ => {
                            return Err(err_at(
                                "named let: binding name must be a symbol",
                                pair[0].line,
                                pair[0].col,
                            ))
                        }
                    };
                    let val = eval(&pair[1], env, output)?;
                    params.push(bname);
                    init_vals.push(val);
                }
                _ => {
                    return Err(err_at(
                        "named let: each binding must be (name value)",
                        binding.line,
                        binding.col,
                    ))
                }
            }
        }
        let body = items[3..].to_vec();
        let lambda = Value::Lambda {
            params: params.clone(),
            rest_param: None,
            body,
            env: env.clone(),
        };
        // The lambda's env needs to include itself for recursion
        let local_env = env.child();
        local_env.define(loop_name.clone(), lambda.clone());
        // Update the lambda to close over the env that includes itself
        let lambda = Value::Lambda {
            params,
            rest_param: None,
            body: items[3..].to_vec(),
            env: local_env.clone(),
        };
        local_env.define(loop_name.clone(), lambda.clone());
        return apply_lambda(&lambda, &init_vals, expr.line, expr.col, output);
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
    let local_env = env.child();
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
                local_env.define(bname, val);
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
        result = eval(e, &local_env, output)?;
    }
    Ok(result)
}

fn eval_cond(items: &[Expr], expr: &Expr, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let _ = expr;
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

fn eval_string_set(items: &[Expr], expr: &Expr, env: &Env, output: &mut String) -> Result<Value, EvalError> {
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
        Some(Value::Str(s)) => s,
        _ => return Err(err_at("string-set!: variable must contain a string", expr.line, expr.col)),
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(err_at("string-set!: index out of range", expr.line, expr.col));
    }
    chars[idx] = ch;
    let new_s: String = chars.into_iter().collect();
    env.set(&var_name, Value::Str(new_s));
    Ok(Value::Symbol("".to_string()))
}

enum EvalResult {
    Done(Value),
    TailCall { func: Value, args: Vec<Value> },
}

fn eval_tail(expr: &Expr, env: &Env, output: &mut String) -> Result<EvalResult, EvalError> {
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
                        // Named let falls through to eval (it uses apply_lambda trampoline)
                        if matches!(&items[1].kind, ExprKind::Value(Value::Symbol(_))) {
                            return Ok(EvalResult::Done(eval(expr, env, output)?));
                        }
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
                        let local_env = env.child();
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
                                    local_env.define(bname, val);
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
                            eval(e, &local_env, output)?;
                        }
                        eval_tail(&items[items.len() - 1], &local_env, output)
                    }
                    "and" => {
                        if items.len() <= 1 {
                            return Ok(EvalResult::Done(Value::Boolean(true)));
                        }
                        for arg in &items[1..items.len() - 1] {
                            let val = eval(arg, env, output)?;
                            if is_falsy(&val) {
                                return Ok(EvalResult::Done(val));
                            }
                        }
                        eval_tail(&items[items.len() - 1], env, output)
                    }
                    "or" => {
                        if items.len() <= 1 {
                            return Ok(EvalResult::Done(Value::Boolean(false)));
                        }
                        for arg in &items[1..items.len() - 1] {
                            let val = eval(arg, env, output)?;
                            if !is_falsy(&val) {
                                return Ok(EvalResult::Done(val));
                            }
                        }
                        eval_tail(&items[items.len() - 1], env, output)
                    }
                    "define" | "set!" | "quote" | "lambda" | "string-set!" | "display"
                    | "write" | "newline" | "call/cc" | "call-with-current-continuation" => {
                        Ok(EvalResult::Done(eval(expr, env, output)?))
                    }
                    _ => {
                        if let Some(func_val) = env.get(name) {
                            match &func_val {
                                Value::Lambda { .. } => {
                                    let mut eval_args = Vec::new();
                                    for a in &items[1..] {
                                        eval_args.push(eval(a, env, output)?);
                                    }
                                    return Ok(EvalResult::TailCall {
                                        func: func_val,
                                        args: eval_args,
                                    });
                                }
                                Value::Builtin(bname) => {
                                    let bname = bname.clone();
                                    return Ok(EvalResult::Done(apply_builtin(
                                        &bname,
                                        &items[1..],
                                        env,
                                        expr.line,
                                        expr.col,
                                        output,
                                    )?));
                                }
                                Value::Continuation { .. } => {
                                    let mut eval_args = Vec::new();
                                    for a in &items[1..] {
                                        eval_args.push(eval(a, env, output)?);
                                    }
                                    return Ok(EvalResult::Done(call_value(
                                        &func_val,
                                        eval_args,
                                        expr.line,
                                        expr.col,
                                        output,
                                    )?));
                                }
                                _ => {}
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
                match &func_val {
                    Value::Lambda { .. } => {
                        let mut eval_args = Vec::new();
                        for a in &items[1..] {
                            eval_args.push(eval(a, env, output)?);
                        }
                        Ok(EvalResult::TailCall {
                            func: func_val,
                            args: eval_args,
                        })
                    }
                    Value::Builtin(bname) => {
                        let mut eval_args = Vec::new();
                        for a in &items[1..] {
                            eval_args.push(eval(a, env, output)?);
                        }
                        Ok(EvalResult::Done(call_builtin_values(
                            bname, eval_args, expr.line, expr.col, output,
                        )?))
                    }
                    _ => Ok(EvalResult::Done(eval(expr, env, output)?)),
                }
            }
        }
        _ => Ok(EvalResult::Done(eval(expr, env, output)?)),
    }
}

fn apply_lambda(
    func: &Value,
    args: &[Value],
    call_line: usize,
    call_col: usize,
    output: &mut String,
) -> Result<Value, EvalError> {
    let mut current_func = func.clone();
    let mut current_args = args.to_vec();

    loop {
        if let Value::Lambda { params, rest_param, body, env } = &current_func {
            if let Some(ref rp) = rest_param {
                if current_args.len() < params.len() {
                    return Err(err_at(
                        format!(
                            "expected at least {} arguments, got {}",
                            params.len(),
                            current_args.len()
                        ),
                        call_line,
                        call_col,
                    ));
                }
            } else if params.len() != current_args.len() {
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
            let local_env = env.child();
            for (p, a) in params.iter().zip(current_args.iter()) {
                local_env.define(p.clone(), a.clone());
            }
            if let Some(ref rp) = rest_param {
                let rest_vals = current_args[params.len()..].to_vec();
                local_env.define(rp.clone(), Value::List(rest_vals));
            }

            if body.is_empty() {
                return Ok(Value::Symbol("".to_string()));
            }

            for expr in &body[..body.len() - 1] {
                eval(expr, &local_env, output)?;
            }

            match eval_tail(&body[body.len() - 1], &local_env, output)? {
                EvalResult::Done(v) => return Ok(v),
                EvalResult::TailCall {
                    func: next_func,
                    args: next_args,
                } => {
                    current_func = next_func;
                    current_args = next_args;
                }
            }
        } else {
            return Err(err_at("not a procedure", call_line, call_col));
        }
    }
}

fn is_builtin_name(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
            | "cons" | "car" | "cdr" | "null?" | "list" | "map" | "length"
            | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
            | "string-append" | "string-length" | "substring"
            | "string->number" | "number->string" | "symbol->string" | "string->symbol"
            | "string-copy" | "string-ref" | "string->list" | "list->string"
            | "char->integer" | "integer->char"
            | "apply" | "equal?" | "eq?" | "abs" | "modulo" | "remainder"
            | "append" | "reverse" | "list-ref" | "filter" | "for-each"
            | "zero?" | "positive?" | "negative?" | "even?" | "odd?"
            | "min" | "max" | "list?" | "procedure?"
            | "char-alphabetic?" | "char-numeric?" | "char-whitespace?"
            | "char-upper-case?" | "char-lower-case?"
            | "char-upcase" | "char-downcase"
            | "make-string" | "string"
            | "display" | "write" | "newline"
            | "call/cc" | "call-with-current-continuation"
    )
}

fn call_builtin_values(
    name: &str,
    eval_args: Vec<Value>,
    call_line: usize,
    call_col: usize,
    output: &mut String,
) -> Result<Value, EvalError> {
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
                        let val = call_value(&func, vec![item.clone()], call_line, call_col, output)?;
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
        "apply" => {
            if eval_args.len() < 2 {
                return Err(err_at("apply requires at least 2 arguments", call_line, call_col));
            }
            let func = eval_args[0].clone();
            let last = &eval_args[eval_args.len() - 1];
            let tail_list = match last {
                Value::List(items) => items.clone(),
                _ => return Err(err_at("apply: last argument must be a list", call_line, call_col)),
            };
            let mut final_args: Vec<Value> = eval_args[1..eval_args.len() - 1].to_vec();
            final_args.extend(tail_list);
            call_value(&func, final_args, call_line, call_col, output)
        }
        "call/cc" | "call-with-current-continuation" => {
            if eval_args.len() != 1 {
                return Err(err_at("call/cc requires 1 argument", call_line, call_col));
            }
            eval_callcc(&eval_args[0], call_line, call_col, output)
        }
        _ => Err(err_at(
            format!("unknown procedure: {}", name),
            call_line,
            call_col,
        )),
    }
}

fn eval_callcc(
    func: &Value,
    line: usize,
    col: usize,
    output: &mut String,
) -> Result<Value, EvalError> {
    // Check if we're in replay mode for this call/cc position
    let replay = CALLCC_REPLAY.with(|r| {
        let r = r.borrow();
        if let Some((rl, rc, ref v)) = *r {
            if rl == line && rc == col {
                Some(v.clone())
            } else {
                None
            }
        } else {
            None
        }
    });
    if let Some(v) = replay {
        CALLCC_REPLAY.with(|r| *r.borrow_mut() = None);
        return Ok(v);
    }

    let expr_idx = CURRENT_EXPR_INDEX.with(|c| c.get());
    let cont = Value::Continuation { line, col, expr_index: expr_idx };

    match call_value(func, vec![cont], line, col, output) {
        Ok(v) => Ok(v),
        Err(EvalError::ContinuationReturn { line: l, col: c, .. })
            if l == line && c == col =>
        {
            let val = CONT_RETURN_VALUE.with(|v| v.borrow_mut().take())
                .expect("continuation value missing");
            Ok(val)
        }
        Err(e) => Err(e),
    }
}

fn call_value(
    func: &Value,
    args: Vec<Value>,
    call_line: usize,
    call_col: usize,
    output: &mut String,
) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { .. } => apply_lambda(func, &args, call_line, call_col, output),
        Value::Builtin(name) => call_builtin_values(name, args, call_line, call_col, output),
        Value::Continuation { line, col, expr_index } => {
            if args.len() != 1 {
                return Err(err_at("continuation requires 1 argument", call_line, call_col));
            }
            CONT_RETURN_VALUE.with(|v| *v.borrow_mut() = Some(args[0].clone()));
            Err(EvalError::ContinuationReturn {
                line: *line,
                col: *col,
                expr_index: *expr_index,
            })
        }
        _ => Err(err_at(
            format!("not a procedure: {}", func.to_scheme_string()),
            call_line,
            call_col,
        )),
    }
}

fn apply_builtin(
    name: &str,
    args: &[Expr],
    env: &Env,
    call_line: usize,
    call_col: usize,
    output: &mut String,
) -> Result<Value, EvalError> {
    let mut eval_args = Vec::with_capacity(args.len());
    for a in args {
        eval_args.push(eval(a, env, output)?);
    }
    call_builtin_values(name, eval_args, call_line, call_col, output)
}

fn is_falsy(val: &Value) -> bool {
    matches!(val, Value::Boolean(false))
}

fn eval_and(args: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
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

fn eval_or(args: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
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

fn eval_exprs(input: &str) -> Result<(Value, String), EvalError> {
    let mut remaining = input;
    let mut exprs = Vec::new();
    while !remaining.trim().is_empty() {
        let (expr, rest) = parse_expr(remaining, input)?;
        exprs.push(expr);
        remaining = rest;
    }
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".to_string()));
    }

    let env = Env::new();
    let mut output = String::new();
    let mut last_value = Value::Symbol("".to_string());
    let mut i = 0;
    while i < exprs.len() {
        CURRENT_EXPR_INDEX.with(|c| c.set(i));
        match eval(&exprs[i], &env, &mut output) {
            Ok(v) => {
                last_value = v;
                i += 1;
            }
            Err(EvalError::ContinuationReturn { line, col, expr_index }) => {
                let value = CONT_RETURN_VALUE.with(|v| v.borrow_mut().take())
                    .expect("continuation value missing");
                CALLCC_REPLAY.with(|r| *r.borrow_mut() = Some((line, col, value)));
                i = expr_index;
            }
            Err(e) => return Err(e),
        }
    }
    Ok((last_value, output))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (v, _) = eval_exprs(input)?;
    Ok(v.to_scheme_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (v, output) = eval_exprs(input)?;
    Ok((v.to_scheme_string(), output))
}

#[cfg(test)]
mod tests;
