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
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
    Continuation(Vec<KontFrame>),
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
            Value::Builtin(_) => "#<procedure>".to_string(),
            Value::Continuation(_) => "#<continuation>".to_string(),
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

fn env_set_existing(env: &Env, name: &str, val: Value) -> bool {
    let has_key = env.borrow().bindings.contains_key(name);
    if has_key {
        env.borrow_mut().bindings.insert(name.to_string(), val);
        true
    } else {
        let parent = env.borrow().parent.clone();
        if let Some(ref p) = parent {
            env_set_existing(p, name, val)
        } else {
            false
        }
    }
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
            | "apply"
            | "call/cc"
            | "call-with-current-continuation"
    )
}

/// Parse a parameter list that may contain dot notation for rest params.
/// Returns (fixed_params, rest_param).
fn parse_params(exprs: &[Expr], el: u32, ec: u32) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < exprs.len() {
        match &exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= exprs.len() {
                    return Err(EvalError::Parse("expected parameter after dot".into()).at(el, ec));
                }
                match &exprs[i + 1].kind {
                    ExprKind::Symbol(rest) => rest_param = Some(rest.clone()),
                    _ => return Err(EvalError::Type("expected symbol after dot".into()).at(el, ec)),
                }
                i += 2;
                break;
            }
            ExprKind::Symbol(s) => {
                params.push(s.clone());
                i += 1;
            }
            _ => return Err(EvalError::Type("expected symbol in params".into()).at(el, ec)),
        }
    }
    Ok((params, rest_param))
}

// --- CEK Machine (Continuation-based evaluator) ---

/// Continuation frame for the CEK machine.
#[derive(Clone, Debug)]
enum KontFrame {
    /// Sequence: discard current value, evaluate remaining expressions.
    Seq { rest: Vec<Expr>, env: Env },
    /// If: condition was evaluated, pick branch.
    If { then_br: Expr, else_br: Option<Expr>, env: Env },
    /// Define: value was evaluated, bind to name.
    Define { name: String, env: Env },
    /// Set!: value was evaluated, update existing binding.
    Set { name: String, env: Env, el: u32, ec: u32 },
    /// Function position was evaluated; now evaluate arguments right-to-left.
    EvalFunc { arg_exprs: Vec<Expr>, env: Env, el: u32, ec: u32 },
    /// Evaluating arguments right-to-left.
    /// `remaining`: args not yet evaluated (original order, pop from end).
    /// `done`: values evaluated so far (reversed order; reversed at end).
    EvalArg { func: Value, remaining: Vec<Expr>, done: Vec<Value>, env: Env, el: u32, ec: u32 },
    /// And: short-circuit evaluation.
    And { rest: Vec<Expr>, env: Env },
    /// Or: short-circuit evaluation.
    Or { rest: Vec<Expr>, env: Env },
    /// Let: evaluating binding values left-to-right.
    LetBind { name: String, done: Vec<(String, Value)>, remaining: Vec<(String, Expr)>, body: Vec<Expr>, outer: Env },
    /// Named let: evaluating init values left-to-right.
    NamedLetBind { loop_name: String, all_params: Vec<String>, done_vals: Vec<Value>, remaining_inits: Vec<Expr>, body: Vec<Expr>, outer: Env },
    /// Cond: test was evaluated; decide whether to run body or try next clause.
    CondClause { body: Vec<Expr>, rest_clauses: Vec<Expr>, env: Env, el: u32, ec: u32 },
    /// String-set!: index expression was evaluated.
    StrSetIdx { var: String, ch_expr: Expr, env: Env, el: u32, ec: u32 },
    /// String-set!: char expression was evaluated.
    StrSetCh { var: String, idx: usize, env: Env, el: u32, ec: u32 },
    /// call/cc: the function argument was evaluated.
    CallCC { el: u32, ec: u32 },
}

/// Control state of the CEK machine.
enum Ctrl {
    /// Evaluate an expression in an environment.
    Eval(Expr, Env),
    /// A value has been produced; apply it to the continuation.
    Val(Value),
}

/// Set up evaluation of a body (sequence of expressions) with proper tail position.
fn eval_body(body: &[Expr], env: Env, kont: &mut Vec<KontFrame>) -> Ctrl {
    if body.is_empty() {
        Ctrl::Val(Value::Void)
    } else if body.len() == 1 {
        Ctrl::Eval(body[0].clone(), env)
    } else {
        kont.push(KontFrame::Seq { rest: body[1..].to_vec(), env: env.clone() });
        Ctrl::Eval(body[0].clone(), env)
    }
}

/// Start evaluating cond clauses.
fn eval_cond(clauses: &[Expr], env: Env, el: u32, ec: u32, kont: &mut Vec<KontFrame>) -> Result<Ctrl, EvalError> {
    if clauses.is_empty() {
        return Ok(Ctrl::Val(Value::Void));
    }
    match &clauses[0].kind {
        ExprKind::List(parts) if !parts.is_empty() => {
            let is_else = matches!(&parts[0].kind, ExprKind::Symbol(ref s) if s == "else");
            if is_else {
                Ok(eval_body(&parts[1..], env, kont))
            } else {
                let body = parts[1..].to_vec();
                let rest = clauses[1..].to_vec();
                kont.push(KontFrame::CondClause { body, rest_clauses: rest, env: env.clone(), el, ec });
                Ok(Ctrl::Eval(parts[0].clone(), env))
            }
        }
        _ => Err(EvalError::Type("cond: invalid clause".into()).at(el, ec)),
    }
}

/// Apply a function value to evaluated arguments within the CEK machine.
fn apply_func(func: Value, args: Vec<Value>, kont: &mut Vec<KontFrame>, el: u32, ec: u32) -> Result<Ctrl, EvalError> {
    match func {
        Value::Lambda { params, rest_param, body, env } => {
            bind_lambda_args(&params, &rest_param, &args, &env, el, ec)?;
            let local_env = new_env(Some(env));
            for (p, a) in params.iter().zip(&args) {
                env_set(&local_env, p.clone(), a.clone());
            }
            if let Some(ref rest) = rest_param {
                let rest_args = if args.len() > params.len() {
                    args[params.len()..].to_vec()
                } else {
                    Vec::new()
                };
                env_set(&local_env, rest.clone(), Value::List(rest_args));
            }
            Ok(eval_body(&body, local_env, kont))
        }
        Value::Builtin(ref name) if name == "call/cc" || name == "call-with-current-continuation" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("call/cc requires 1 argument".into()).at(el, ec));
            }
            let cont_val = Value::Continuation(kont.clone());
            let f = args.into_iter().next().unwrap();
            apply_func(f, vec![cont_val], kont, el, ec)
        }
        Value::Builtin(ref name) if name == "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("apply requires at least 2 arguments".into()).at(el, ec));
            }
            let func = args[0].clone();
            let last = &args[args.len() - 1];
            let tail_args = match last {
                Value::List(items) => items.clone(),
                _ => return Err(EvalError::Type("apply: last argument must be a list".into()).at(el, ec)),
            };
            let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            all_args.extend(tail_args);
            apply_func(func, all_args, kont, el, ec)
        }
        Value::Builtin(ref name) => {
            let result = eval_builtin(name, &args).map_err(|e| e.at(el, ec))?;
            Ok(Ctrl::Val(result))
        }
        Value::Continuation(saved) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("continuation requires 1 argument".into()).at(el, ec));
            }
            *kont = saved;
            Ok(Ctrl::Val(args.into_iter().next().unwrap()))
        }
        _ => Err(EvalError::Type("not a procedure".into()).at(el, ec)),
    }
}

/// Main CEK machine loop.
fn run_cek(initial_ctrl: Ctrl, initial_kont: Vec<KontFrame>) -> Result<Value, EvalError> {
    let mut ctrl = initial_ctrl;
    let mut kont = initial_kont;

    loop {
        ctrl = match ctrl {
            Ctrl::Eval(expr, env) => {
                let (el, ec) = (expr.line, expr.col);
                match expr.kind {
                    ExprKind::Integer(n) => Ctrl::Val(Value::Integer(n)),
                    ExprKind::Boolean(b) => Ctrl::Val(Value::Boolean(b)),
                    ExprKind::Str(s) => Ctrl::Val(Value::Str(s)),
                    ExprKind::Char(c) => Ctrl::Val(Value::Char(c)),
                    ExprKind::Symbol(ref name) => {
                        if is_builtin(name) {
                            Ctrl::Val(Value::Builtin(name.clone()))
                        } else {
                            match env_get(&env, name) {
                                Some(v) => Ctrl::Val(v),
                                None => return Err(EvalError::UnboundVariable(name.clone()).at(el, ec)),
                            }
                        }
                    }
                    ExprKind::List(items) => {
                        if items.is_empty() {
                            Ctrl::Val(Value::List(vec![]))
                        } else {
                            // Extract operator name if it's a symbol (for special form dispatch)
                            let op_name: Option<String> = match &items[0].kind {
                                ExprKind::Symbol(s) => Some(s.clone()),
                                _ => None,
                            };
                            match op_name.as_deref() {
                                Some("quote") => {
                                    if items.len() != 2 {
                                        return Err(EvalError::Arity("quote requires 1 argument".into()).at(el, ec));
                                    }
                                    Ctrl::Val(expr_to_value(&items[1]))
                                }
                                Some("if") => {
                                    if items.len() < 3 || items.len() > 4 {
                                        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()).at(el, ec));
                                    }
                                    let else_br = if items.len() == 4 { Some(items[3].clone()) } else { None };
                                    kont.push(KontFrame::If { then_br: items[2].clone(), else_br, env: env.clone() });
                                    Ctrl::Eval(items[1].clone(), env)
                                }
                                Some("define") => {
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity("define requires at least 2 arguments".into()).at(el, ec));
                                    }
                                    match &items[1].kind {
                                        ExprKind::Symbol(name) => {
                                            kont.push(KontFrame::Define { name: name.clone(), env: env.clone() });
                                            Ctrl::Eval(items[2].clone(), env)
                                        }
                                        ExprKind::List(sig) => {
                                            if sig.is_empty() {
                                                return Err(EvalError::Parse("define: empty signature".into()).at(el, ec));
                                            }
                                            let name = match &sig[0].kind {
                                                ExprKind::Symbol(s) => s.clone(),
                                                _ => return Err(EvalError::Type("define: expected symbol".into()).at(el, ec)),
                                            };
                                            let (params, rest_param) = parse_params(&sig[1..], el, ec)?;
                                            let body = items[2..].to_vec();
                                            let lambda = Value::Lambda { params, rest_param, body, env: env.clone() };
                                            env_set(&env, name, lambda);
                                            Ctrl::Val(Value::Void)
                                        }
                                        _ => return Err(EvalError::Type("define: expected symbol or list".into()).at(el, ec)),
                                    }
                                }
                                Some("set!") => {
                                    if items.len() != 3 {
                                        return Err(EvalError::Arity("set! requires exactly 2 arguments".into()).at(el, ec));
                                    }
                                    let name = match &items[1].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Type("set!: expected symbol".into()).at(el, ec)),
                                    };
                                    kont.push(KontFrame::Set { name, env: env.clone(), el, ec });
                                    Ctrl::Eval(items[2].clone(), env)
                                }
                                Some("lambda") => {
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity("lambda requires at least 2 arguments".into()).at(el, ec));
                                    }
                                    let (params, rest_param) = match &items[1].kind {
                                        ExprKind::List(param_exprs) => parse_params(param_exprs, el, ec)?,
                                        ExprKind::Symbol(s) => (Vec::new(), Some(s.clone())),
                                        _ => return Err(EvalError::Type("lambda: expected parameter list".into()).at(el, ec)),
                                    };
                                    let body = items[2..].to_vec();
                                    Ctrl::Val(Value::Lambda { params, rest_param, body, env })
                                }
                                Some("and") => {
                                    if items.len() == 1 {
                                        Ctrl::Val(Value::Boolean(true))
                                    } else if items.len() == 2 {
                                        Ctrl::Eval(items[1].clone(), env)
                                    } else {
                                        kont.push(KontFrame::And { rest: items[2..].to_vec(), env: env.clone() });
                                        Ctrl::Eval(items[1].clone(), env)
                                    }
                                }
                                Some("or") => {
                                    if items.len() == 1 {
                                        Ctrl::Val(Value::Boolean(false))
                                    } else if items.len() == 2 {
                                        Ctrl::Eval(items[1].clone(), env)
                                    } else {
                                        kont.push(KontFrame::Or { rest: items[2..].to_vec(), env: env.clone() });
                                        Ctrl::Eval(items[1].clone(), env)
                                    }
                                }
                                Some("let") => {
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity("let requires at least 2 arguments".into()).at(el, ec));
                                    }
                                    // Named let: (let name ((var init) ...) body ...)
                                    if let ExprKind::Symbol(ref loop_name) = items[1].kind {
                                        let bindings_expr = match &items[2].kind {
                                            ExprKind::List(bs) => bs,
                                            _ => return Err(EvalError::Type("let: expected bindings list".into()).at(el, ec)),
                                        };
                                        let mut params = Vec::new();
                                        let mut inits = Vec::new();
                                        for b in bindings_expr {
                                            match &b.kind {
                                                ExprKind::List(pair) if pair.len() == 2 => {
                                                    if let ExprKind::Symbol(s) = &pair[0].kind {
                                                        params.push(s.clone());
                                                        inits.push(pair[1].clone());
                                                    } else {
                                                        return Err(EvalError::Type("let: expected symbol".into()).at(el, ec));
                                                    }
                                                }
                                                _ => return Err(EvalError::Type("let: invalid binding".into()).at(el, ec)),
                                            }
                                        }
                                        let body = items[3..].to_vec();
                                        if inits.is_empty() {
                                            let local_env = new_env(Some(env));
                                            let loop_lambda = Value::Lambda { params: vec![], rest_param: None, body: body.clone(), env: local_env.clone() };
                                            env_set(&local_env, loop_name.clone(), loop_lambda);
                                            eval_body(&body, local_env, &mut kont)
                                        } else {
                                            let mut remaining_inits = inits;
                                            let first_init = remaining_inits.remove(0);
                                            kont.push(KontFrame::NamedLetBind {
                                                loop_name: loop_name.clone(),
                                                all_params: params,
                                                done_vals: vec![],
                                                remaining_inits,
                                                body,
                                                outer: env.clone(),
                                            });
                                            Ctrl::Eval(first_init, env)
                                        }
                                    } else {
                                        // Regular let: (let ((var init) ...) body ...)
                                        let bindings_expr = match &items[1].kind {
                                            ExprKind::List(bs) => bs,
                                            _ => return Err(EvalError::Type("let: expected bindings list".into()).at(el, ec)),
                                        };
                                        let mut bindings = Vec::new();
                                        for b in bindings_expr {
                                            match &b.kind {
                                                ExprKind::List(pair) if pair.len() == 2 => {
                                                    if let ExprKind::Symbol(s) = &pair[0].kind {
                                                        bindings.push((s.clone(), pair[1].clone()));
                                                    } else {
                                                        return Err(EvalError::Type("let: expected symbol".into()).at(el, ec));
                                                    }
                                                }
                                                _ => return Err(EvalError::Type("let: invalid binding".into()).at(el, ec)),
                                            }
                                        }
                                        let body = items[2..].to_vec();
                                        if bindings.is_empty() {
                                            let local_env = new_env(Some(env));
                                            eval_body(&body, local_env, &mut kont)
                                        } else {
                                            let mut bindings = bindings;
                                            let (first_name, first_expr) = bindings.remove(0);
                                            kont.push(KontFrame::LetBind {
                                                name: first_name,
                                                done: vec![],
                                                remaining: bindings,
                                                body,
                                                outer: env.clone(),
                                            });
                                            Ctrl::Eval(first_expr, env)
                                        }
                                    }
                                }
                                Some("begin") => {
                                    if items.len() <= 1 {
                                        Ctrl::Val(Value::Void)
                                    } else {
                                        eval_body(&items[1..], env, &mut kont)
                                    }
                                }
                                Some("cond") => {
                                    eval_cond(&items[1..], env, el, ec, &mut kont)?
                                }
                                Some("string-set!") => {
                                    if items.len() != 4 {
                                        return Err(EvalError::Arity("string-set! requires 3 arguments".into()).at(el, ec));
                                    }
                                    let var_name = match &items[1].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Type("string-set!: first argument must be a variable".into()).at(el, ec)),
                                    };
                                    kont.push(KontFrame::StrSetIdx { var: var_name, ch_expr: items[3].clone(), env: env.clone(), el, ec });
                                    Ctrl::Eval(items[2].clone(), env)
                                }
                                Some("call/cc") | Some("call-with-current-continuation") => {
                                    if items.len() != 2 {
                                        return Err(EvalError::Arity("call/cc requires 1 argument".into()).at(el, ec));
                                    }
                                    kont.push(KontFrame::CallCC { el, ec });
                                    Ctrl::Eval(items[1].clone(), env)
                                }
                                _ => {
                                    // Procedure call: eval function, then args right-to-left
                                    let func_expr = items[0].clone();
                                    let arg_exprs = items[1..].to_vec();
                                    kont.push(KontFrame::EvalFunc { arg_exprs, env: env.clone(), el, ec });
                                    Ctrl::Eval(func_expr, env)
                                }
                            }
                        }
                    }
                }
            }
            Ctrl::Val(val) => {
                if let Some(frame) = kont.pop() {
                    match frame {
                        KontFrame::Seq { mut rest, env } => {
                            // Discard val, evaluate next
                            if rest.len() == 1 {
                                Ctrl::Eval(rest.remove(0), env)
                            } else {
                                let first = rest.remove(0);
                                kont.push(KontFrame::Seq { rest, env: env.clone() });
                                Ctrl::Eval(first, env)
                            }
                        }
                        KontFrame::If { then_br, else_br, env } => {
                            if val.is_truthy() {
                                Ctrl::Eval(then_br, env)
                            } else if let Some(eb) = else_br {
                                Ctrl::Eval(eb, env)
                            } else {
                                Ctrl::Val(Value::Void)
                            }
                        }
                        KontFrame::Define { name, env } => {
                            env_set(&env, name, val);
                            Ctrl::Val(Value::Void)
                        }
                        KontFrame::Set { name, env, el, ec } => {
                            if !env_set_existing(&env, &name, val) {
                                return Err(EvalError::UnboundVariable(name).at(el, ec));
                            }
                            Ctrl::Val(Value::Void)
                        }
                        KontFrame::EvalFunc { mut arg_exprs, env, el, ec } => {
                            // val is the evaluated function; now evaluate args right-to-left
                            if arg_exprs.is_empty() {
                                apply_func(val, vec![], &mut kont, el, ec)?
                            } else {
                                let last = arg_exprs.pop().unwrap();
                                kont.push(KontFrame::EvalArg { func: val, remaining: arg_exprs, done: vec![], env: env.clone(), el, ec });
                                Ctrl::Eval(last, env)
                            }
                        }
                        KontFrame::EvalArg { func, mut remaining, mut done, env, el, ec } => {
                            done.push(val);
                            if remaining.is_empty() {
                                done.reverse();
                                apply_func(func, done, &mut kont, el, ec)?
                            } else {
                                let next = remaining.pop().unwrap();
                                kont.push(KontFrame::EvalArg { func, remaining, done, env: env.clone(), el, ec });
                                Ctrl::Eval(next, env)
                            }
                        }
                        KontFrame::And { mut rest, env } => {
                            if !val.is_truthy() {
                                Ctrl::Val(val)
                            } else if rest.len() == 1 {
                                Ctrl::Eval(rest.remove(0), env)
                            } else {
                                let first = rest.remove(0);
                                kont.push(KontFrame::And { rest, env: env.clone() });
                                Ctrl::Eval(first, env)
                            }
                        }
                        KontFrame::Or { mut rest, env } => {
                            if val.is_truthy() {
                                Ctrl::Val(val)
                            } else if rest.len() == 1 {
                                Ctrl::Eval(rest.remove(0), env)
                            } else {
                                let first = rest.remove(0);
                                kont.push(KontFrame::Or { rest, env: env.clone() });
                                Ctrl::Eval(first, env)
                            }
                        }
                        KontFrame::LetBind { name, mut done, mut remaining, body, outer } => {
                            done.push((name, val));
                            if remaining.is_empty() {
                                let local_env = new_env(Some(outer));
                                for (n, v) in done {
                                    env_set(&local_env, n, v);
                                }
                                eval_body(&body, local_env, &mut kont)
                            } else {
                                let (next_name, next_expr) = remaining.remove(0);
                                kont.push(KontFrame::LetBind { name: next_name, done, remaining, body, outer: outer.clone() });
                                Ctrl::Eval(next_expr, outer)
                            }
                        }
                        KontFrame::NamedLetBind { loop_name, all_params, mut done_vals, mut remaining_inits, body, outer } => {
                            done_vals.push(val);
                            if remaining_inits.is_empty() {
                                let local_env = new_env(Some(outer));
                                let loop_lambda = Value::Lambda {
                                    params: all_params.clone(),
                                    rest_param: None,
                                    body: body.clone(),
                                    env: local_env.clone(),
                                };
                                env_set(&local_env, loop_name, loop_lambda);
                                let call_env = new_env(Some(local_env));
                                for (p, v) in all_params.iter().zip(done_vals) {
                                    env_set(&call_env, p.clone(), v);
                                }
                                eval_body(&body, call_env, &mut kont)
                            } else {
                                let next_init = remaining_inits.remove(0);
                                kont.push(KontFrame::NamedLetBind { loop_name, all_params, done_vals, remaining_inits, body, outer: outer.clone() });
                                Ctrl::Eval(next_init, outer)
                            }
                        }
                        KontFrame::CondClause { body, rest_clauses, env, el, ec } => {
                            if val.is_truthy() {
                                if body.is_empty() {
                                    Ctrl::Val(val)
                                } else {
                                    eval_body(&body, env, &mut kont)
                                }
                            } else if rest_clauses.is_empty() {
                                Ctrl::Val(Value::Void)
                            } else {
                                eval_cond(&rest_clauses, env, el, ec, &mut kont)?
                            }
                        }
                        KontFrame::StrSetIdx { var, ch_expr, env, el, ec } => {
                            let idx = as_integer(&val).map_err(|e| e.at(el, ec))? as usize;
                            kont.push(KontFrame::StrSetCh { var, idx, env: env.clone(), el, ec });
                            Ctrl::Eval(ch_expr, env)
                        }
                        KontFrame::StrSetCh { var, idx, env, el, ec } => {
                            let ch = match val {
                                Value::Char(c) => c,
                                _ => return Err(EvalError::Type("string-set!: third argument must be a character".into()).at(el, ec)),
                            };
                            env_mutate_string(&env, &var, idx, ch).map_err(|e| e.at(el, ec))?;
                            Ctrl::Val(Value::Void)
                        }
                        KontFrame::CallCC { el, ec } => {
                            // val is the function to call with the continuation
                            let cont_val = Value::Continuation(kont.clone());
                            apply_func(val, vec![cont_val], &mut kont, el, ec)?
                        }
                    }
                } else {
                    return Ok(val);
                }
            }
        };
    }
}

fn env_mutate_string(env: &Env, name: &str, idx: usize, ch: char) -> Result<(), EvalError> {
    let mut inner = env.borrow_mut();
    if let Some(val) = inner.bindings.get_mut(name) {
        if let Value::Str(ref mut s) = val {
            let mut chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type("string-set!: index out of range".into()));
            }
            chars[idx] = ch;
            *s = chars.into_iter().collect();
            return Ok(());
        }
        return Err(EvalError::Type("string-set!: not a string".into()));
    }
    drop(inner);
    let inner = env.borrow();
    if let Some(ref parent) = inner.parent {
        return env_mutate_string(parent, name, idx, ch);
    }
    Err(EvalError::UnboundVariable(name.to_string()))
}

fn bind_lambda_args(
    params: &[String],
    rest_param: &Option<String>,
    args: &[Value],
    _env: &Env,
    el: u32,
    ec: u32,
) -> Result<(), EvalError> {
    if rest_param.is_some() {
        if args.len() < params.len() {
            return Err(EvalError::Arity(format!(
                "expected at least {} arguments, got {}",
                params.len(),
                args.len()
            ))
            .at(el, ec));
        }
    } else if args.len() != params.len() {
        return Err(EvalError::Arity(format!(
            "expected {} arguments, got {}",
            params.len(),
            args.len()
        ))
        .at(el, ec));
    }
    Ok(())
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
    let mut kont = Vec::new();
    if exprs.len() > 1 {
        kont.push(KontFrame::Seq { rest: exprs[1..].to_vec(), env: env.clone() });
    }
    let ctrl = Ctrl::Eval(exprs[0].clone(), env);
    let result = run_cek(ctrl, kont)?;
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().clear());
    let exprs = parse_all(input)?;
    let env = new_env(None);
    let mut kont = Vec::new();
    if exprs.len() > 1 {
        kont.push(KontFrame::Seq { rest: exprs[1..].to_vec(), env: env.clone() });
    }
    let ctrl = Ctrl::Eval(exprs[0].clone(), env);
    let result = run_cek(ctrl, kont)?;
    let output = OUTPUT_BUFFER.with(|buf| buf.borrow().clone());
    Ok((result.display(), output))
}

#[cfg(test)]
mod tests;
