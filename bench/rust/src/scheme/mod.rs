pub mod error;

pub use error::EvalError;

use std::collections::HashMap;

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Void,
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
}

thread_local! {
    static OUTPUT_BUFFER: std::cell::RefCell<String> = std::cell::RefCell::new(String::new());
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Char(c) => format!("#\\{}", c),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Void => "#<void>".to_string(),
            Value::Lambda { .. } => "#<procedure>".to_string(),
        }
    }

    /// Display without quotes (for `display`).
    fn display_write(&self, write_mode: bool) -> String {
        match self {
            Value::Str(s) if write_mode => format!("\"{}\"", s),
            Value::Str(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_write(write_mode)).collect();
                format!("({})", inner.join(" "))
            }
            _ => self.display(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// --- Environment ---

type Env = std::rc::Rc<std::cell::RefCell<EnvInner>>;

#[derive(Debug, Clone)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

fn new_env(parent: Option<Env>) -> Env {
    std::rc::Rc::new(std::cell::RefCell::new(EnvInner {
        bindings: HashMap::new(),
        parent,
    }))
}

fn env_get(env: &Env, name: &str) -> Option<Value> {
    let inner = env.borrow();
    if let Some(v) = inner.bindings.get(name) {
        Some(v.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        None
    }
}

fn env_set(env: &Env, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

// --- Parser ---

fn skip_whitespace(input: &[u8], pos: usize) -> usize {
    let mut i = pos;
    while i < input.len() {
        if input[i].is_ascii_whitespace() {
            i += 1;
        } else if input[i] == b';' {
            while i < input.len() && input[i] != b'\n' {
                i += 1;
            }
        } else {
            break;
        }
    }
    i
}

/// Compute (line, col) from byte offset. Both 1-based.
fn line_col(input: &[u8], offset: usize) -> (u32, u32) {
    let mut line = 1u32;
    let mut col = 1u32;
    for &b in &input[..offset.min(input.len())] {
        if b == b'\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Expr>),
}

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    line: u32,
    col: u32,
}

impl Expr {
    fn new(kind: ExprKind, line: u32, col: u32) -> Self {
        Expr { kind, line, col }
    }
}

fn parse_expr(input: &[u8], pos: usize) -> Result<(Expr, usize), EvalError> {
    let i = skip_whitespace(input, pos);
    if i >= input.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let (l, c) = line_col(input, i);

    match input[i] {
        b'(' => parse_list(input, i),
        b'\'' => {
            let (expr, next) = parse_expr(input, i + 1)?;
            Ok((
                Expr::new(
                    ExprKind::List(vec![
                        Expr::new(ExprKind::Symbol("quote".into()), l, c),
                        expr,
                    ]),
                    l,
                    c,
                ),
                next,
            ))
        }
        b'"' => parse_string(input, i),
        b'#' => {
            if i + 1 < input.len() {
                match input[i + 1] {
                    b't' => Ok((Expr::new(ExprKind::Boolean(true), l, c), i + 2)),
                    b'f' => Ok((Expr::new(ExprKind::Boolean(false), l, c), i + 2)),
                    b'\\' => {
                        // Character literal: #\x, #\space, #\newline, #\tab
                        if i + 2 >= input.len() {
                            return Err(EvalError::Parse(format!(
                                "{l}:{c}: unexpected end after #\\"
                            )));
                        }
                        let start = i + 2;
                        let mut end = start;
                        while end < input.len() && input[end].is_ascii_alphabetic() {
                            end += 1;
                        }
                        if end == start {
                            // Single non-alpha char like #\( or #\)
                            let ch = input[start] as char;
                            Ok((Expr::new(ExprKind::Char(ch), l, c), start + 1))
                        } else {
                            let name = std::str::from_utf8(&input[start..end]).unwrap();
                            if name.len() == 1 {
                                Ok((Expr::new(ExprKind::Char(name.chars().next().unwrap()), l, c), end))
                            } else {
                                let ch = match name.to_lowercase().as_str() {
                                    "space" => ' ',
                                    "newline" => '\n',
                                    "tab" => '\t',
                                    _ => {
                                        return Err(EvalError::Parse(format!(
                                            "{l}:{c}: unknown character name: {name}"
                                        )))
                                    }
                                };
                                Ok((Expr::new(ExprKind::Char(ch), l, c), end))
                            }
                        }
                    }
                    _ => Err(EvalError::Parse(format!(
                        "{l}:{c}: unexpected character after #"
                    ))),
                }
            } else {
                Err(EvalError::Parse(format!(
                    "{l}:{c}: unexpected end after #"
                )))
            }
        }
        _ => parse_atom(input, i),
    }
}

fn parse_string(input: &[u8], pos: usize) -> Result<(Expr, usize), EvalError> {
    let (l, c) = line_col(input, pos);
    let mut i = pos + 1;
    let mut s = String::new();
    while i < input.len() && input[i] != b'"' {
        if input[i] == b'\\' && i + 1 < input.len() {
            i += 1;
            match input[i] {
                b'n' => s.push('\n'),
                b't' => s.push('\t'),
                b'\\' => s.push('\\'),
                b'"' => s.push('"'),
                ch => {
                    s.push('\\');
                    s.push(ch as char);
                }
            }
        } else {
            s.push(input[i] as char);
        }
        i += 1;
    }
    if i >= input.len() {
        return Err(EvalError::Parse(format!("{l}:{c}: unterminated string")));
    }
    Ok((Expr::new(ExprKind::Str(s), l, c), i + 1))
}

fn parse_list(input: &[u8], pos: usize) -> Result<(Expr, usize), EvalError> {
    let (l, c) = line_col(input, pos);
    let mut i = pos + 1;
    let mut items = Vec::new();
    loop {
        i = skip_whitespace(input, i);
        if i >= input.len() {
            return Err(EvalError::Parse(format!("{l}:{c}: unterminated list")));
        }
        if input[i] == b')' {
            return Ok((Expr::new(ExprKind::List(items), l, c), i + 1));
        }
        let (expr, next) = parse_expr(input, i)?;
        items.push(expr);
        i = next;
    }
}

fn is_symbol_char(b: u8) -> bool {
    !b.is_ascii_whitespace() && b != b'(' && b != b')' && b != b'"' && b != b';'
}

fn parse_atom(input: &[u8], pos: usize) -> Result<(Expr, usize), EvalError> {
    let (l, c) = line_col(input, pos);
    let mut i = pos;
    while i < input.len() && is_symbol_char(input[i]) {
        i += 1;
    }
    if i == pos {
        return Err(EvalError::Parse(format!(
            "{l}:{c}: unexpected character: {}",
            input[pos] as char
        )));
    }
    let token = std::str::from_utf8(&input[pos..i]).unwrap();
    if let Ok(n) = token.parse::<i64>() {
        return Ok((Expr::new(ExprKind::Integer(n), l, c), i));
    }
    Ok((Expr::new(ExprKind::Symbol(token.to_string()), l, c), i))
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let bytes = input.as_bytes();
    let mut pos = 0;
    let mut exprs = Vec::new();
    loop {
        pos = skip_whitespace(bytes, pos);
        if pos >= bytes.len() {
            break;
        }
        let (expr, next) = parse_expr(bytes, pos)?;
        exprs.push(expr);
        pos = next;
    }
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    Ok(exprs)
}

// --- Evaluator ---

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
    }
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
            | "not"
            | "cons"
            | "car"
            | "cdr"
            | "null?"
            | "list"
            | "length"
            | "append"
            | "string?"
            | "number?"
            | "boolean?"
            | "pair?"
            | "symbol?"
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
            | "string-copy"
            | "char?"
    )
}

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let (el, ec) = (expr.line, expr.col);
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Symbol(name) => {
            if is_builtin(name) {
                return Ok(Value::Symbol(name.clone()));
            }
            env_get(env, name)
                .ok_or_else(|| EvalError::UnboundVariable(name.clone()).at(el, ec))
        }
        ExprKind::List(items) => {
            if items.is_empty() {
                return Ok(Value::List(vec![]));
            }
            // Special forms
            if let ExprKind::Symbol(op) = &items[0].kind {
                match op.as_str() {
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("quote requires 1 argument".into())
                                .at(el, ec));
                        }
                        return Ok(expr_to_value(&items[1]));
                    }
                    "if" => {
                        if items.len() < 3 || items.len() > 4 {
                            return Err(EvalError::Arity(
                                "if requires 2 or 3 arguments".into(),
                            )
                            .at(el, ec));
                        }
                        let cond = eval(&items[1], env)?;
                        return if cond.is_truthy() {
                            eval(&items[2], env)
                        } else if items.len() == 4 {
                            eval(&items[3], env)
                        } else {
                            Ok(Value::Void)
                        };
                    }
                    "define" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity(
                                "define requires at least 2 arguments".into(),
                            )
                            .at(el, ec));
                        }
                        match &items[1].kind {
                            ExprKind::Symbol(name) => {
                                let val = eval(&items[2], env)?;
                                env_set(env, name.clone(), val);
                                return Ok(Value::Void);
                            }
                            ExprKind::List(sig) => {
                                if sig.is_empty() {
                                    return Err(EvalError::Parse(
                                        "define: empty signature".into(),
                                    )
                                    .at(el, ec));
                                }
                                let name = match &sig[0].kind {
                                    ExprKind::Symbol(s) => s.clone(),
                                    _ => {
                                        return Err(EvalError::Type(
                                            "define: expected symbol".into(),
                                        )
                                        .at(el, ec))
                                    }
                                };
                                let params: Vec<String> = sig[1..]
                                    .iter()
                                    .map(|e| match &e.kind {
                                        ExprKind::Symbol(s) => Ok(s.clone()),
                                        _ => Err(EvalError::Type(
                                            "expected symbol in params".into(),
                                        )
                                        .at(el, ec)),
                                    })
                                    .collect::<Result<_, _>>()?;
                                let body = items[2..].to_vec();
                                let lambda = Value::Lambda {
                                    params,
                                    body,
                                    env: env.clone(),
                                };
                                env_set(env, name, lambda);
                                return Ok(Value::Void);
                            }
                            _ => {
                                return Err(EvalError::Type(
                                    "define: expected symbol or list".into(),
                                )
                                .at(el, ec))
                            }
                        }
                    }
                    "lambda" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity(
                                "lambda requires at least 2 arguments".into(),
                            )
                            .at(el, ec));
                        }
                        let params = match &items[1].kind {
                            ExprKind::List(param_exprs) => param_exprs
                                .iter()
                                .map(|e| match &e.kind {
                                    ExprKind::Symbol(s) => Ok(s.clone()),
                                    _ => Err(EvalError::Type(
                                        "expected symbol in params".into(),
                                    )
                                    .at(el, ec)),
                                })
                                .collect::<Result<Vec<_>, _>>()?,
                            _ => {
                                return Err(EvalError::Type(
                                    "lambda: expected parameter list".into(),
                                )
                                .at(el, ec))
                            }
                        };
                        let body = items[2..].to_vec();
                        return Ok(Value::Lambda {
                            params,
                            body,
                            env: env.clone(),
                        });
                    }
                    "and" => {
                        if items.len() == 1 {
                            return Ok(Value::Boolean(true));
                        }
                        let mut result = Value::Boolean(true);
                        for a in &items[1..] {
                            result = eval(a, env)?;
                            if !result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        if items.len() == 1 {
                            return Ok(Value::Boolean(false));
                        }
                        let mut result = Value::Boolean(false);
                        for a in &items[1..] {
                            result = eval(a, env)?;
                            if result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "let" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity(
                                "let requires at least 2 arguments".into(),
                            )
                            .at(el, ec));
                        }
                        // Named let: (let name ((var init) ...) body ...)
                        if let ExprKind::Symbol(loop_name) = &items[1].kind {
                            let bindings = match &items[2].kind {
                                ExprKind::List(bs) => bs,
                                _ => {
                                    return Err(EvalError::Type(
                                        "let: expected bindings list".into(),
                                    )
                                    .at(el, ec))
                                }
                            };
                            let mut params = Vec::new();
                            let mut init_vals = Vec::new();
                            for b in bindings {
                                match &b.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        if let ExprKind::Symbol(s) = &pair[0].kind {
                                            params.push(s.clone());
                                            init_vals.push(eval(&pair[1], env)?);
                                        } else {
                                            return Err(EvalError::Type(
                                                "let: expected symbol".into(),
                                            )
                                            .at(el, ec));
                                        }
                                    }
                                    _ => {
                                        return Err(EvalError::Type(
                                            "let: invalid binding".into(),
                                        )
                                        .at(el, ec))
                                    }
                                }
                            }
                            let body = items[3..].to_vec();
                            let loop_lambda = Value::Lambda {
                                params: params.clone(),
                                body,
                                env: env.clone(),
                            };
                            let local_env = new_env(Some(env.clone()));
                            env_set(&local_env, loop_name.clone(), loop_lambda.clone());
                            if let Value::Lambda {
                                params: p,
                                body: b,
                                ..
                            } = loop_lambda
                            {
                                let self_lambda = Value::Lambda {
                                    params: p,
                                    body: b,
                                    env: local_env.clone(),
                                };
                                env_set(&local_env, loop_name.clone(), self_lambda);
                            }
                            let func = env_get(&local_env, loop_name).unwrap();
                            return apply(func, init_vals).map_err(|e| e.at(el, ec));
                        }
                        // Regular let: (let ((var init) ...) body ...)
                        let bindings = match &items[1].kind {
                            ExprKind::List(bs) => bs,
                            _ => {
                                return Err(EvalError::Type(
                                    "let: expected bindings list".into(),
                                )
                                .at(el, ec))
                            }
                        };
                        let local_env = new_env(Some(env.clone()));
                        for b in bindings {
                            match &b.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    if let ExprKind::Symbol(s) = &pair[0].kind {
                                        let val = eval(&pair[1], env)?;
                                        env_set(&local_env, s.clone(), val);
                                    } else {
                                        return Err(EvalError::Type(
                                            "let: expected symbol".into(),
                                        )
                                        .at(el, ec));
                                    }
                                }
                                _ => {
                                    return Err(EvalError::Type(
                                        "let: invalid binding".into(),
                                    )
                                    .at(el, ec))
                                }
                            }
                        }
                        let mut result = Value::Void;
                        for e in &items[2..] {
                            result = eval(e, &local_env)?;
                        }
                        return Ok(result);
                    }
                    "string-set!" => {
                        if items.len() != 4 {
                            return Err(EvalError::Arity(
                                "string-set! requires 3 arguments".into(),
                            )
                            .at(el, ec));
                        }
                        // Evaluate the string variable name
                        let var_name = match &items[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => {
                                return Err(EvalError::Type(
                                    "string-set!: first argument must be a variable".into(),
                                )
                                .at(el, ec))
                            }
                        };
                        let idx = as_integer(&eval(&items[2], env)?)? as usize;
                        let ch = match eval(&items[3], env)? {
                            Value::Char(c) => c,
                            _ => {
                                return Err(EvalError::Type(
                                    "string-set!: third argument must be a character".into(),
                                )
                                .at(el, ec))
                            }
                        };
                        // Mutate the string in the environment
                        fn env_mutate_string(
                            env: &Env,
                            name: &str,
                            idx: usize,
                            ch: char,
                        ) -> Result<(), EvalError> {
                            let mut inner = env.borrow_mut();
                            if let Some(val) = inner.bindings.get_mut(name) {
                                if let Value::Str(ref mut s) = val {
                                    let mut chars: Vec<char> = s.chars().collect();
                                    if idx >= chars.len() {
                                        return Err(EvalError::Type(
                                            "string-set!: index out of range".into(),
                                        ));
                                    }
                                    chars[idx] = ch;
                                    *s = chars.into_iter().collect();
                                    return Ok(());
                                }
                                return Err(EvalError::Type(
                                    "string-set!: not a string".into(),
                                ));
                            }
                            drop(inner);
                            let inner = env.borrow();
                            if let Some(ref parent) = inner.parent {
                                return env_mutate_string(parent, name, idx, ch);
                            }
                            Err(EvalError::UnboundVariable(name.to_string()))
                        }
                        env_mutate_string(env, &var_name, idx, ch)?;
                        return Ok(Value::Void);
                    }
                    "begin" => {
                        let mut result = Value::Void;
                        for e in &items[1..] {
                            result = eval(e, env)?;
                        }
                        return Ok(result);
                    }
                    "cond" => {
                        for clause in &items[1..] {
                            match &clause.kind {
                                ExprKind::List(parts) if !parts.is_empty() => {
                                    if let ExprKind::Symbol(s) = &parts[0].kind {
                                        if s == "else" {
                                            let mut result = Value::Void;
                                            for e in &parts[1..] {
                                                result = eval(e, env)?;
                                            }
                                            return Ok(result);
                                        }
                                    }
                                    let test = eval(&parts[0], env)?;
                                    if test.is_truthy() {
                                        let mut result = test;
                                        for e in &parts[1..] {
                                            result = eval(e, env)?;
                                        }
                                        return Ok(result);
                                    }
                                }
                                _ => {
                                    return Err(EvalError::Type(
                                        "cond: invalid clause".into(),
                                    )
                                    .at(el, ec))
                                }
                            }
                        }
                        return Ok(Value::Void);
                    }
                    _ => {} // fall through to procedure call
                }
            }
            // Procedure call
            let func = eval(&items[0], env)?;
            let args: Vec<Value> = items[1..]
                .iter()
                .map(|a| eval(a, env))
                .collect::<Result<_, _>>()?;
            apply(func, args).map_err(|e| e.at(el, ec))
        }
    }
}

fn apply(func: Value, args: Vec<Value>) -> Result<Value, EvalError> {
    match func {
        Value::Symbol(ref name) if is_builtin(name) => eval_builtin(name, &args),
        Value::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}",
                    params.len(),
                    args.len()
                )));
            }
            let local_env = new_env(Some(env));
            for (p, a) in params.iter().zip(args) {
                env_set(&local_env, p.clone(), a);
            }
            let mut result = Value::Void;
            for expr in &body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type("not a procedure".into())),
    }
}

fn eval_builtin(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_integer(a)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            let first = as_integer(&args[0])?;
            if args.len() == 1 {
                return Ok(Value::Integer(-first));
            }
            let mut result = first;
            for a in &args[1..] {
                result -= as_integer(a)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_integer(a)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let first = as_integer(&args[0])?;
            if args.len() == 1 {
                if first == 0 {
                    return Err(EvalError::DivisionByZero(String::new()));
                }
                return Ok(Value::Integer(1 / first));
            }
            let mut result = first;
            for a in &args[1..] {
                let d = as_integer(a)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero(String::new()));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => val_cmp_op(args, |a, b| a < b),
        ">" => val_cmp_op(args, |a, b| a > b),
        "=" => val_cmp_op(args, |a, b| a == b),
        "<=" => val_cmp_op(args, |a, b| a <= b),
        ">=" => val_cmp_op(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("cons requires 2 arguments".into()));
            }
            match &args[1] {
                Value::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => {
                    // Improper pair - represent as 2-element list for now
                    Ok(Value::List(vec![args[0].clone(), args[1].clone()]))
                }
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                _ => Err(EvalError::Type("car: not a pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => {
                    Ok(Value::List(items[1..].to_vec()))
                }
                _ => Err(EvalError::Type("cdr: not a pair".into())),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("null? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(EvalError::Type("length: not a list".into())),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for a in args {
                match a {
                    Value::List(items) => result.extend(items.iter().cloned()),
                    _ => return Err(EvalError::Type("append: not a list".into())),
                }
            }
            Ok(Value::List(result))
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("number? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("boolean? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("pair? requires 1 argument".into()));
            }
            Ok(Value::Boolean(
                matches!(&args[0], Value::List(items) if !items.is_empty()),
            ))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("symbol? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("display requires 1 argument".into()));
            }
            let text = args[0].display_write(false);
            OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(&text));
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("write requires 1 argument".into()));
            }
            let text = args[0].display_write(true);
            OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(&text));
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity("newline requires 0 arguments".into()));
            }
            OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push('\n'));
            Ok(Value::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type("string-append: not a string".into())),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string-length requires 1 argument".into()));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type("string-length: not a string".into())),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity("substring requires 3 arguments".into()));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type("substring: not a string".into())),
            };
            let start = as_integer(&args[1])? as usize;
            let end = as_integer(&args[2])? as usize;
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string->number requires 1 argument".into()));
            }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type("string->number: not a string".into())),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("number->string requires 1 argument".into()));
            }
            let n = as_integer(&args[0])?;
            Ok(Value::Str(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("symbol->string requires 1 argument".into()));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type("symbol->string: not a symbol".into())),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string->symbol requires 1 argument".into()));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type("string->symbol: not a string".into())),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string-ref requires 2 arguments".into()));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type("string-ref: not a string".into())),
            };
            let idx = as_integer(&args[1])? as usize;
            Ok(Value::Char(s.chars().nth(idx).ok_or_else(|| {
                EvalError::Type("string-ref: index out of range".into())
            })?))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string-copy requires 1 argument".into()));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type("string-copy: not a string".into())),
            }
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("char? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        _ => Err(EvalError::UnboundVariable(op.to_string())),
    }
}

fn as_integer(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type("expected integer".into())),
    }
}

fn val_cmp_op(args: &[Value], f: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(
            "comparison requires at least 2 arguments".into(),
        ));
    }
    let mut prev = as_integer(&args[0])?;
    for a in &args[1..] {
        let cur = as_integer(a)?;
        if !f(prev, cur) {
            return Ok(Value::Boolean(false));
        }
        prev = cur;
    }
    Ok(Value::Boolean(true))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let env = new_env(None);
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &env)?;
    }
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().clear());
    let exprs = parse_all(input)?;
    let env = new_env(None);
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &env)?;
    }
    let output = OUTPUT_BUFFER.with(|buf| buf.borrow().clone());
    Ok((result.display(), output))
}

#[cfg(test)]
mod tests;
