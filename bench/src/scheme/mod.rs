pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

type Pos = (usize, usize);

#[derive(Debug, Clone, PartialEq)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

type Env = Rc<RefCell<EnvInner>>;

/// Data captured by a first-class continuation.
#[derive(Debug, Clone, PartialEq)]
struct ContinuationData {
    replay_expr: Value,
    replay_pos: Pos,
    remaining_exprs: Vec<(Value, Pos)>,
    env: Env,
}

/// A Scheme value.
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
        rest_param: Option<String>,
        body: Vec<Value>,
        env: Env,
    },
    Builtin(String),
    Continuation(Rc<ContinuationData>),
    Macro {
        rules: Vec<(Value, Value)>,
        def_env: Env,
    },
    Vector(Rc<RefCell<Vec<Value>>>),
}

thread_local! {
    static OUTPUT_BUFFER: RefCell<String> = RefCell::new(String::new());
    static CONT_RETURN: RefCell<Option<Value>> = RefCell::new(None);
    static CONT_CONTEXT: RefCell<Option<(Value, Pos, Vec<(Value, Pos)>, Env)>> = RefCell::new(None);
    static CONT_INVOKE_DATA: RefCell<Option<(Rc<ContinuationData>, Value)>> = RefCell::new(None);
}

fn output_write(s: &str) {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(s));
}

fn output_take() -> String {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().split_off(0))
}

fn new_env(parent: Option<Env>) -> Env {
    Rc::new(RefCell::new(EnvInner {
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
    let mut inner = env.borrow_mut();
    if inner.bindings.contains_key(name) {
        inner.bindings.insert(name.to_string(), val);
        true
    } else if let Some(ref parent) = inner.parent {
        env_set_existing(parent, name, val)
    } else {
        false
    }
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::Char(c) => match c {
                ' ' => "#\\space".to_string(),
                '\n' => "#\\newline".to_string(),
                '\t' => "#\\tab".to_string(),
                _ => format!("#\\{}", c),
            },
            Value::Lambda { .. } | Value::Builtin(_) | Value::Continuation(_) | Value::Macro { .. } => "#<procedure>".to_string(),
            Value::Vector(v) => {
                let inner: Vec<String> = v.borrow().iter().map(|v| v.display()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
        }
    }

    /// Display without quotes around strings (used by Scheme `display`).
    fn display_unquoted(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.display_unquoted()).collect();
                format!("({})", inner.join(" "))
            }
            _ => self.display(),
        }
    }
}

#[derive(Debug, Clone)]
struct Token {
    text: String,
    line: usize,
    col: usize,
}

fn runtime_err(pos: Pos, msg: impl std::fmt::Display) -> EvalError {
    EvalError::Runtime(format!("{}:{}: {}", pos.0, pos.1, msg))
}

/// Tokenize input into a list of tokens with position info.
fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line = 1usize;
    let mut col = 1usize;
    while i < chars.len() {
        match chars[i] {
            '\n' => {
                line += 1;
                col = 1;
                i += 1;
            }
            ' ' | '\t' | '\r' => {
                col += 1;
                i += 1;
            }
            '"' => {
                let start_line = line;
                let start_col = col;
                let mut s = String::new();
                s.push('"');
                i += 1;
                col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        i += 2;
                        col += 2;
                    } else {
                        if chars[i] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                    col += 1;
                }
                tokens.push(Token {
                    text: s,
                    line: start_line,
                    col: start_col,
                });
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' | ')' | '\'' => {
                tokens.push(Token {
                    text: chars[i].to_string(),
                    line,
                    col,
                });
                i += 1;
                col += 1;
            }
            _ => {
                let start_line = line;
                let start_col = col;
                let mut s = String::new();
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
                {
                    s.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token {
                    text: s,
                    line: start_line,
                    col: start_col,
                });
            }
        }
    }
    tokens
}

/// Parse a single expression from the token stream.
fn parse(tokens: &[Token], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".to_string()));
    }
    let tok = &tokens[pos];
    let token = &tok.text;

    // Boolean literals
    if token == "#t" || token == "#true" {
        return Ok((Value::Boolean(true), pos + 1));
    }
    if token == "#f" || token == "#false" {
        return Ok((Value::Boolean(false), pos + 1));
    }

    // String literal
    if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
        let inner = &token[1..token.len() - 1];
        // Process escape sequences
        let mut s = String::new();
        let cs: Vec<char> = inner.chars().collect();
        let mut j = 0;
        while j < cs.len() {
            if cs[j] == '\\' && j + 1 < cs.len() {
                match cs[j + 1] {
                    'n' => s.push('\n'),
                    't' => s.push('\t'),
                    '\\' => s.push('\\'),
                    '"' => s.push('"'),
                    other => {
                        s.push('\\');
                        s.push(other);
                    }
                }
                j += 2;
            } else {
                s.push(cs[j]);
                j += 1;
            }
        }
        return Ok((Value::Str(s), pos + 1));
    }

    // Integer literal
    if let Ok(n) = token.parse::<i64>() {
        return Ok((Value::Integer(n), pos + 1));
    }

    // List
    if token == "(" {
        let mut elems = Vec::new();
        let mut p = pos + 1;
        loop {
            if p >= tokens.len() {
                return Err(EvalError::Parse(format!(
                    "{}:{}: unclosed parenthesis",
                    tok.line, tok.col
                )));
            }
            if tokens[p].text == ")" {
                return Ok((Value::List(elems), p + 1));
            }
            let (val, next) = parse(tokens, p)?;
            elems.push(val);
            p = next;
        }
    }

    // Quote shorthand
    if token == "'" {
        let (inner, next) = parse(tokens, pos + 1)?;
        return Ok((
            Value::List(vec![Value::Symbol("quote".to_string()), inner]),
            next,
        ));
    }

    // Character literal: #\x, #\space, #\newline
    if token.starts_with("#\\") && token.len() >= 3 {
        let char_name = &token[2..];
        let c = match char_name {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            _ if char_name.len() == 1 => char_name.chars().next().unwrap(),
            _ => return Err(EvalError::Parse(format!("unknown character: {}", token))),
        };
        return Ok((Value::Char(c), pos + 1));
    }

    // Symbol
    Ok((Value::Symbol(token.clone()), pos + 1))
}

/// Parse all expressions from input, returning values with their positions.
fn parse_all(input: &str) -> Result<Vec<(Value, Pos)>, EvalError> {
    let tokens = tokenize(input);
    let mut exprs = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let p = (tokens[pos].line, tokens[pos].col);
        let (expr, next) = parse(&tokens, pos)?;
        exprs.push((expr, p));
        pos = next;
    }
    Ok(exprs)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".to_string()));
    }
    let env = new_env(None);
    eval_top_level(&exprs, &env).map(|v| v.display())
}

/// Evaluate a sequence of top-level expressions, handling continuation invocations.
fn eval_top_level(exprs: &[(Value, Pos)], env: &Env) -> Result<Value, EvalError> {
    let mut current_exprs = exprs.to_vec();
    let mut current_env = env.clone();

    'restart: loop {
        let mut result = Value::Boolean(false);
        let mut i = 0;
        while i < current_exprs.len() {
            // Set continuation context so call/cc can capture the current position
            CONT_CONTEXT.with(|c| {
                *c.borrow_mut() = Some((
                    current_exprs[i].0.clone(),
                    current_exprs[i].1,
                    current_exprs[i + 1..].to_vec(),
                    current_env.clone(),
                ));
            });

            match eval(current_exprs[i].0.clone(), &current_env, current_exprs[i].1) {
                Ok(v) => {
                    result = v;
                    i += 1;
                }
                Err(EvalError::ContinuationInvoke) => {
                    // A continuation was invoked — replay from its capture point
                    let (data, value) = CONT_INVOKE_DATA
                        .with(|d| d.borrow_mut().take())
                        .expect("ContinuationInvoke without data");
                    // If there's already a pending CONT_RETURN that no call/cc consumed,
                    // a continuation was invoked during another continuation's replay.
                    // Clear it to prevent the wrong call/cc from consuming it.
                    let had_pending = CONT_RETURN.with(|c| c.borrow().is_some());
                    if had_pending {
                        CONT_RETURN.with(|c| *c.borrow_mut() = None);
                    } else {
                        CONT_RETURN.with(|c| *c.borrow_mut() = Some(value));
                    }
                    let mut replay = vec![(data.replay_expr.clone(), data.replay_pos)];
                    replay.extend(data.remaining_exprs.iter().cloned());
                    current_exprs = replay;
                    current_env = data.env.clone();
                    continue 'restart;
                }
                Err(e) => return Err(e),
            }
        }
        return Ok(result);
    }
}

fn eval(mut expr: Value, env: &Env, pos: Pos) -> Result<Value, EvalError> {
    let mut current_env = env.clone();
    let current_pos = pos;

    loop {
        let current = std::mem::replace(&mut expr, Value::Boolean(false));
        match current {
            Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Char(_) | Value::Lambda { .. } | Value::Builtin(_) | Value::Continuation(_) | Value::Macro { .. } | Value::Vector(_) => return Ok(current),
            Value::Symbol(s) => {
                return env_get(&current_env, &s)
                    .or_else(|| if is_builtin(&s) { Some(Value::Builtin(s.clone())) } else { None })
                    .ok_or_else(|| runtime_err(current_pos, format!("unbound symbol: {}", s)));
            }
            Value::List(elems) => {
                if elems.is_empty() {
                    return Err(runtime_err(current_pos, "empty application"));
                }
                let first = &elems[0];
                match first {
                    Value::Symbol(op) => match op.as_str() {
                        "set!" => {
                            if elems.len() != 3 {
                                return Err(runtime_err(current_pos, "set! requires 2 arguments"));
                            }
                            let name = match &elems[1] {
                                Value::Symbol(s) => s.clone(),
                                _ => return Err(runtime_err(current_pos, "set!: first argument must be a symbol")),
                            };
                            let val = eval(elems[2].clone(), &current_env, current_pos)?;
                            if !env_set_existing(&current_env, &name, val.clone()) {
                                return Err(runtime_err(current_pos, format!("set!: unbound variable: {}", name)));
                            }
                            return Ok(val);
                        }
                        "define" => {
                            if elems.len() < 3 {
                                return Err(runtime_err(current_pos, "define requires 2 arguments"));
                            }
                            match &elems[1] {
                                Value::Symbol(name) => {
                                    let val = eval(elems[2].clone(), &current_env, current_pos)?;
                                    env_set(&current_env, name.clone(), val.clone());
                                    return Ok(val);
                                }
                                Value::List(sig) => {
                                    if sig.is_empty() {
                                        return Err(runtime_err(current_pos, "define: empty signature"));
                                    }
                                    let name = match &sig[0] {
                                        Value::Symbol(s) => s.clone(),
                                        _ => {
                                            return Err(runtime_err(current_pos, "define: expected symbol"))
                                        }
                                    };
                                    let (params, rest_param) = parse_formals(&sig[1..], current_pos)?;
                                    let lambda = Value::Lambda {
                                        params,
                                        rest_param,
                                        body: elems[2..].to_vec(),
                                        env: current_env.clone(),
                                    };
                                    env_set(&current_env, name, lambda.clone());
                                    return Ok(lambda);
                                }
                                _ => return Err(runtime_err(
                                    current_pos,
                                    "define: first argument must be a symbol or list",
                                )),
                            }
                        }
                        "lambda" => {
                            if elems.len() < 3 {
                                return Err(runtime_err(
                                    current_pos,
                                    "lambda requires at least 2 arguments",
                                ));
                            }
                            let (params, rest_param) = match &elems[1] {
                                Value::List(ps) => parse_formals(ps, current_pos)?,
                                Value::Symbol(s) => {
                                    // (lambda args body) — single symbol captures all
                                    (vec![], Some(s.clone()))
                                }
                                _ => {
                                    return Err(runtime_err(
                                        current_pos,
                                        "lambda: expected parameter list",
                                    ))
                                }
                            };
                            return Ok(Value::Lambda {
                                params,
                                rest_param,
                                body: elems[2..].to_vec(),
                                env: current_env.clone(),
                            });
                        }
                        "if" => {
                            if elems.len() < 3 || elems.len() > 4 {
                                return Err(runtime_err(current_pos, "if requires 2 or 3 arguments"));
                            }
                            let cond = eval(elems[1].clone(), &current_env, current_pos)?;
                            if cond != Value::Boolean(false) {
                                // TCO: tail position
                                expr = elems[2].clone();
                                continue;
                            } else if elems.len() == 4 {
                                expr = elems[3].clone();
                                continue;
                            } else {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        "quote" => {
                            if elems.len() != 2 {
                                return Err(runtime_err(current_pos, "quote requires 1 argument"));
                            }
                            return Ok(elems[1].clone());
                        }
                        "and" => {
                            if elems.len() <= 1 {
                                return Ok(Value::Boolean(true));
                            }
                            for arg in &elems[1..elems.len() - 1] {
                                let result = eval(arg.clone(), &current_env, current_pos)?;
                                if result == Value::Boolean(false) {
                                    return Ok(Value::Boolean(false));
                                }
                            }
                            // TCO: last arg is tail position
                            expr = elems.last().unwrap().clone();
                            continue;
                        }
                        "or" => {
                            if elems.len() <= 1 {
                                return Ok(Value::Boolean(false));
                            }
                            for arg in &elems[1..elems.len() - 1] {
                                let result = eval(arg.clone(), &current_env, current_pos)?;
                                if result != Value::Boolean(false) {
                                    return Ok(result);
                                }
                            }
                            expr = elems.last().unwrap().clone();
                            continue;
                        }
                        "begin" => {
                            if elems.len() <= 1 {
                                return Ok(Value::Boolean(false));
                            }
                            for e in &elems[1..elems.len() - 1] {
                                eval(e.clone(), &current_env, current_pos)?;
                            }
                            // TCO: last expr is tail position
                            expr = elems.last().unwrap().clone();
                            continue;
                        }
                        "let" => {
                            if elems.len() < 3 {
                                return Err(runtime_err(current_pos, "let requires bindings and body"));
                            }
                            // Named let: (let name ((var init) ...) body ...)
                            if let Value::Symbol(loop_name) = &elems[1] {
                                if elems.len() < 4 {
                                    return Err(runtime_err(current_pos, "named let requires bindings and body"));
                                }
                                let loop_name = loop_name.clone();
                                let bindings = match &elems[2] {
                                    Value::List(bs) => bs,
                                    _ => return Err(runtime_err(current_pos, "let: expected bindings list")),
                                };
                                let mut param_names = Vec::new();
                                let mut init_vals = Vec::new();
                                for b in bindings {
                                    match b {
                                        Value::List(pair) if pair.len() == 2 => {
                                            let pname = match &pair[0] {
                                                Value::Symbol(s) => s.clone(),
                                                _ => return Err(runtime_err(current_pos, "let: expected symbol")),
                                            };
                                            let val = eval(pair[1].clone(), &current_env, current_pos)?;
                                            param_names.push(pname);
                                            init_vals.push(val);
                                        }
                                        _ => return Err(runtime_err(current_pos, "let: invalid binding")),
                                    }
                                }
                                let body = elems[3..].to_vec();
                                let let_env = new_env(Some(current_env.clone()));
                                // Bind the loop name to a lambda
                                let lambda = Value::Lambda {
                                    params: param_names.clone(),
                                    rest_param: None,
                                    body: body.clone(),
                                    env: let_env.clone(),
                                };
                                env_set(&let_env, loop_name, lambda);
                                // Bind initial values
                                for (pname, val) in param_names.iter().zip(init_vals.iter()) {
                                    env_set(&let_env, pname.clone(), val.clone());
                                }
                                // Eval body with TCO
                                for e in &body[..body.len().saturating_sub(1)] {
                                    eval(e.clone(), &let_env, current_pos)?;
                                }
                                expr = body.last().cloned().unwrap_or(Value::Boolean(false));
                                current_env = let_env;
                                continue;
                            }
                            let bindings = match &elems[1] {
                                Value::List(bs) => bs,
                                _ => {
                                    return Err(runtime_err(current_pos, "let: expected bindings list"))
                                }
                            };
                            let let_env = new_env(Some(current_env.clone()));
                            for b in bindings {
                                match b {
                                    Value::List(pair) if pair.len() == 2 => {
                                        let name = match &pair[0] {
                                            Value::Symbol(s) => s.clone(),
                                            _ => {
                                                return Err(runtime_err(
                                                    current_pos,
                                                    "let: expected symbol",
                                                ))
                                            }
                                        };
                                        let val = eval(pair[1].clone(), &current_env, current_pos)?;
                                        env_set(&let_env, name, val);
                                    }
                                    _ => {
                                        return Err(runtime_err(current_pos, "let: invalid binding"))
                                    }
                                }
                            }
                            // Eval all but last, then TCO on last
                            for e in &elems[2..elems.len() - 1] {
                                eval(e.clone(), &let_env, current_pos)?;
                            }
                            expr = elems.last().unwrap().clone();
                            current_env = let_env;
                            continue;
                        }
                        "letrec" => {
                            if elems.len() < 3 {
                                return Err(runtime_err(current_pos, "letrec requires bindings and body"));
                            }
                            let bindings = match &elems[1] {
                                Value::List(bs) => bs,
                                _ => return Err(runtime_err(current_pos, "letrec: expected bindings list")),
                            };
                            let let_env = new_env(Some(current_env.clone()));
                            // First pass: bind all names to uninitialized placeholder
                            let mut names = Vec::new();
                            let mut init_exprs = Vec::new();
                            for b in bindings {
                                match b {
                                    Value::List(pair) if pair.len() == 2 => {
                                        let name = match &pair[0] {
                                            Value::Symbol(s) => s.clone(),
                                            _ => return Err(runtime_err(current_pos, "letrec: expected symbol")),
                                        };
                                        env_set(&let_env, name.clone(), Value::Boolean(false));
                                        names.push(name);
                                        init_exprs.push(pair[1].clone());
                                    }
                                    _ => return Err(runtime_err(current_pos, "letrec: invalid binding")),
                                }
                            }
                            // Second pass: eval all inits in let_env, then assign
                            let mut vals = Vec::new();
                            for init in &init_exprs {
                                vals.push(eval(init.clone(), &let_env, current_pos)?);
                            }
                            for (name, val) in names.into_iter().zip(vals) {
                                env_set(&let_env, name, val);
                            }
                            for e in &elems[2..elems.len() - 1] {
                                eval(e.clone(), &let_env, current_pos)?;
                            }
                            expr = elems.last().unwrap().clone();
                            current_env = let_env;
                            continue;
                        }
                        "letrec*" => {
                            if elems.len() < 3 {
                                return Err(runtime_err(current_pos, "letrec* requires bindings and body"));
                            }
                            let bindings = match &elems[1] {
                                Value::List(bs) => bs,
                                _ => return Err(runtime_err(current_pos, "letrec*: expected bindings list")),
                            };
                            let let_env = new_env(Some(current_env.clone()));
                            for b in bindings {
                                match b {
                                    Value::List(pair) if pair.len() == 2 => {
                                        let name = match &pair[0] {
                                            Value::Symbol(s) => s.clone(),
                                            _ => return Err(runtime_err(current_pos, "letrec*: expected symbol")),
                                        };
                                        let val = eval(pair[1].clone(), &let_env, current_pos)?;
                                        env_set(&let_env, name, val);
                                    }
                                    _ => return Err(runtime_err(current_pos, "letrec*: invalid binding")),
                                }
                            }
                            for e in &elems[2..elems.len() - 1] {
                                eval(e.clone(), &let_env, current_pos)?;
                            }
                            expr = elems.last().unwrap().clone();
                            current_env = let_env;
                            continue;
                        }
                        "cond" => {
                            let mut found = false;
                            for clause in &elems[1..] {
                                match clause {
                                    Value::List(parts) if parts.len() >= 2 => {
                                        if parts[0] == Value::Symbol("else".to_string()) {
                                            for e in &parts[1..parts.len() - 1] {
                                                eval(e.clone(), &current_env, current_pos)?;
                                            }
                                            expr = parts.last().unwrap().clone();
                                            found = true;
                                            break;
                                        }
                                        let test = eval(parts[0].clone(), &current_env, current_pos)?;
                                        if test != Value::Boolean(false) {
                                            for e in &parts[1..parts.len() - 1] {
                                                eval(e.clone(), &current_env, current_pos)?;
                                            }
                                            expr = parts.last().unwrap().clone();
                                            found = true;
                                            break;
                                        }
                                    }
                                    _ => {
                                        return Err(runtime_err(current_pos, "cond: invalid clause"))
                                    }
                                }
                            }
                            if found {
                                continue;
                            }
                            return Ok(Value::Boolean(false));
                        }
                        "case" => {
                            if elems.len() < 3 {
                                return Err(runtime_err(current_pos, "case requires at least 2 arguments"));
                            }
                            let key = eval(elems[1].clone(), &current_env, current_pos)?;
                            let mut found = false;
                            for clause in &elems[2..] {
                                match clause {
                                    Value::List(parts) if parts.len() >= 2 => {
                                        if parts[0] == Value::Symbol("else".to_string()) {
                                            for e in &parts[1..parts.len() - 1] {
                                                eval(e.clone(), &current_env, current_pos)?;
                                            }
                                            expr = parts.last().unwrap().clone();
                                            found = true;
                                            break;
                                        }
                                        // parts[0] is a list of datums
                                        if let Value::List(datums) = &parts[0] {
                                            if datums.iter().any(|d| eqv_values(&key, d)) {
                                                for e in &parts[1..parts.len() - 1] {
                                                    eval(e.clone(), &current_env, current_pos)?;
                                                }
                                                expr = parts.last().unwrap().clone();
                                                found = true;
                                                break;
                                            }
                                        } else {
                                            return Err(runtime_err(current_pos, "case: invalid clause"));
                                        }
                                    }
                                    _ => {
                                        return Err(runtime_err(current_pos, "case: invalid clause"))
                                    }
                                }
                            }
                            if found {
                                continue;
                            }
                            return Ok(Value::Boolean(false));
                        }
                        "call/cc" | "call-with-current-continuation" => {
                            if elems.len() != 2 {
                                return Err(runtime_err(current_pos, "call/cc requires 1 argument"));
                            }
                            let pending = CONT_RETURN.with(|c| c.borrow_mut().take());
                            if let Some(val) = pending {
                                return Ok(val);
                            }
                            let proc = eval(elems[1].clone(), &current_env, current_pos)?;
                            return handle_callcc(&proc, &current_env, current_pos);
                        }
                        "define-syntax" => {
                            if elems.len() != 3 {
                                return Err(runtime_err(current_pos, "define-syntax requires 2 arguments"));
                            }
                            let name = match &elems[1] {
                                Value::Symbol(s) => s.clone(),
                                _ => return Err(runtime_err(current_pos, "define-syntax: expected symbol")),
                            };
                            let sr = match &elems[2] {
                                Value::List(l) => l,
                                _ => return Err(runtime_err(current_pos, "define-syntax: expected syntax-rules")),
                            };
                            if sr.is_empty() || sr[0] != Value::Symbol("syntax-rules".to_string()) {
                                return Err(runtime_err(current_pos, "define-syntax: expected syntax-rules"));
                            }
                            // sr[1] is the literals list (ignored for now)
                            let mut rules = Vec::new();
                            for rule in &sr[2..] {
                                match rule {
                                    Value::List(r) if r.len() == 2 => {
                                        rules.push((r[0].clone(), r[1].clone()));
                                    }
                                    _ => return Err(runtime_err(current_pos, "define-syntax: invalid rule")),
                                }
                            }
                            let macro_val = Value::Macro {
                                rules,
                                def_env: current_env.clone(),
                            };
                            env_set(&current_env, name, macro_val.clone());
                            return Ok(macro_val);
                        }
                        "string-set!" => {
                            return Err(runtime_err(current_pos, "string-set!: strings are immutable"));
                        }
                        "apply" | "map" => {
                            let mut args = Vec::new();
                            for arg in &elems[1..] {
                                args.push(eval(arg.clone(), &current_env, current_pos)?);
                            }
                            return call_builtin(op, args, &current_env, current_pos);
                        }
                        _ => {
                            // Check for macro before evaluating args
                            if let Some(Value::Macro { rules, def_env }) = env_get(&current_env, op) {
                                let (expanded, hygiene) = expand_macro(&elems, &rules, &def_env, current_pos)?;
                                let hyg_env = new_env(Some(current_env.clone()));
                                for (gs, val) in hygiene {
                                    env_set(&hyg_env, gs, val);
                                }
                                expr = expanded;
                                current_env = hyg_env;
                                continue;
                            }
                            let mut args = Vec::new();
                            for arg in &elems[1..] {
                                args.push(eval(arg.clone(), &current_env, current_pos)?);
                            }
                            // Try as variable (user-defined procedure) first, then builtin
                            if let Some(proc) = env_get(&current_env, op) {
                                match proc {
                                    Value::Lambda { params, rest_param, body, env: closure_env } => {
                                        let call_env = bind_args(op, &params, &rest_param, &args, &closure_env, current_pos)?;
                                        for e in &body[..body.len().saturating_sub(1)] {
                                            eval(e.clone(), &call_env, current_pos)?;
                                        }
                                        expr = body.last().cloned().unwrap_or(Value::Boolean(false));
                                        current_env = call_env;
                                        continue;
                                    }
                                    Value::Builtin(ref name) if name == "call/cc" || name == "call-with-current-continuation" => {
                                        if args.len() != 1 {
                                            return Err(runtime_err(current_pos, "call/cc requires 1 argument"));
                                        }
                                        let pending = CONT_RETURN.with(|c| c.borrow_mut().take());
                                        if let Some(val) = pending {
                                            return Ok(val);
                                        }
                                        return handle_callcc(&args[0], &current_env, current_pos);
                                    }
                                    Value::Builtin(name) => {
                                        return call_builtin(&name, args, &current_env, current_pos);
                                    }
                                    Value::Continuation(data) => {
                                        return invoke_continuation(&data, &args, current_pos);
                                    }
                                    _ => return Err(runtime_err(current_pos, format!("{} is not a procedure", op))),
                                }
                            } else {
                                return apply_builtin_vals(op, &args, current_pos);
                            }
                        }
                    },
                    _ => {
                        // Evaluate the operator position (e.g., ((lambda ...) args))
                        let proc = eval(elems[0].clone(), &current_env, current_pos)?;
                        let mut args = Vec::new();
                        for arg in &elems[1..] {
                            args.push(eval(arg.clone(), &current_env, current_pos)?);
                        }
                        match proc {
                            Value::Lambda { params, rest_param, body, env: closure_env } => {
                                let call_env = bind_args("<anonymous>", &params, &rest_param, &args, &closure_env, current_pos)?;
                                for e in &body[..body.len().saturating_sub(1)] {
                                    eval(e.clone(), &call_env, current_pos)?;
                                }
                                expr = body.last().cloned().unwrap_or(Value::Boolean(false));
                                current_env = call_env;
                                continue;
                            }
                            Value::Builtin(ref name) if name == "call/cc" || name == "call-with-current-continuation" => {
                                if args.len() != 1 {
                                    return Err(runtime_err(current_pos, "call/cc requires 1 argument"));
                                }
                                let pending = CONT_RETURN.with(|c| c.borrow_mut().take());
                                if let Some(val) = pending {
                                    return Ok(val);
                                }
                                return handle_callcc(&args[0], &current_env, current_pos);
                            }
                            Value::Builtin(name) => {
                                return call_builtin(&name, args, &current_env, current_pos);
                            }
                            Value::Continuation(data) => {
                                return invoke_continuation(&data, &args, current_pos);
                            }
                            _ => return Err(runtime_err(current_pos, "<anonymous> is not a procedure")),
                        }
                    }
                }
            }
        }
    }
}

fn apply_proc(
    proc: &Value,
    name: &str,
    args: &[Value],
    _env: &Env,
    pos: Pos,
) -> Result<Value, EvalError> {
    match proc {
        Value::Lambda {
            params,
            rest_param,
            body,
            env: closure_env,
        } => {
            let call_env = bind_args(name, params, rest_param, args, closure_env, pos)?;
            let mut result = Value::Boolean(false);
            for expr in body.iter() {
                result = eval(expr.clone(), &call_env, pos)?;
            }
            Ok(result)
        }
        Value::Builtin(bname) => {
            apply_builtin_vals(bname, args, pos)
        }
        Value::Continuation(data) => {
            invoke_continuation(data, args, pos)
        }
        _ => Err(runtime_err(
            pos,
            format!("{} is not a procedure", name),
        )),
    }
}

/// Parse a formals list, handling dotted rest params like (x y . rest).
fn parse_formals(formals: &[Value], pos: Pos) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < formals.len() {
        match &formals[i] {
            Value::Symbol(s) if s == "." => {
                if i + 1 >= formals.len() || i + 2 < formals.len() {
                    return Err(runtime_err(pos, "malformed dotted parameter list"));
                }
                match &formals[i + 1] {
                    Value::Symbol(rest) => rest_param = Some(rest.clone()),
                    _ => return Err(runtime_err(pos, "expected symbol after dot in formals")),
                }
                break;
            }
            Value::Symbol(s) => params.push(s.clone()),
            _ => return Err(runtime_err(pos, "expected symbol in parameter list")),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

/// Bind arguments to parameters, handling rest params.
fn bind_args(
    name: &str,
    params: &[String],
    rest_param: &Option<String>,
    args: &[Value],
    closure_env: &Env,
    pos: Pos,
) -> Result<Env, EvalError> {
    if let Some(_) = rest_param {
        if args.len() < params.len() {
            return Err(runtime_err(pos, format!("{}: expected at least {} arguments, got {}", name, params.len(), args.len())));
        }
    } else if args.len() != params.len() {
        return Err(runtime_err(pos, format!("{}: expected {} arguments, got {}", name, params.len(), args.len())));
    }
    let call_env = new_env(Some(closure_env.clone()));
    for (param, arg) in params.iter().zip(args.iter()) {
        env_set(&call_env, param.clone(), arg.clone());
    }
    if let Some(rest) = rest_param {
        env_set(&call_env, rest.clone(), Value::List(args[params.len()..].to_vec()));
    }
    Ok(call_env)
}

/// Invoke a continuation: stores data in thread-local and returns ContinuationInvoke error.
fn invoke_continuation(data: &Rc<ContinuationData>, args: &[Value], pos: Pos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(runtime_err(pos, "continuation requires 1 argument"));
    }
    CONT_INVOKE_DATA.with(|d| *d.borrow_mut() = Some((data.clone(), args[0].clone())));
    Err(EvalError::ContinuationInvoke)
}

/// Handle call/cc: check for pending return or create a continuation and call the proc.
fn handle_callcc(proc: &Value, env: &Env, pos: Pos) -> Result<Value, EvalError> {
    // Check for pending return (we're in a replay)
    let pending = CONT_RETURN.with(|c| c.borrow_mut().take());
    if let Some(val) = pending {
        return Ok(val);
    }

    // Create continuation from current context
    let ctx = CONT_CONTEXT.with(|c| c.borrow().clone());
    let cont = match ctx {
        Some((replay_expr, replay_pos, remaining, cont_env)) => {
            Value::Continuation(Rc::new(ContinuationData {
                replay_expr,
                replay_pos,
                remaining_exprs: remaining,
                env: cont_env,
            }))
        }
        None => return Err(runtime_err(pos, "call/cc: no continuation context")),
    };

    // Call proc with the continuation
    apply_proc(proc, "call/cc", &[cont], env, pos)
}

// ===== Macro expansion (define-syntax / syntax-rules) =====

static GENSYM_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    format!("{}__gs{}", base, n)
}

fn is_special_form(s: &str) -> bool {
    matches!(s,
        "set!" | "define" | "lambda" | "if" | "quote" | "and" | "or" | "begin" |
        "let" | "letrec" | "letrec*" | "cond" | "case" | "call/cc" | "call-with-current-continuation" | "string-set!" |
        "apply" | "map" | "define-syntax" | "syntax-rules" | "quasiquote" |
        "unquote" | "unquote-splicing" | "else"
    )
}

/// Collect pattern variable names from a syntax-rules pattern.
fn collect_pattern_vars(pattern: &Value, vars: &mut HashSet<String>, skip_first: bool) {
    match pattern {
        Value::List(elems) => {
            for (i, elem) in elems.iter().enumerate() {
                if i == 0 && skip_first {
                    continue;
                }
                collect_pattern_vars(elem, vars, false);
            }
        }
        Value::Symbol(s) if s != "..." && s != "_" => {
            vars.insert(s.clone());
        }
        _ => {}
    }
}

enum PatBinding {
    One(Value),
    Many(Vec<Value>),
}

/// Match an input form against a syntax-rules pattern.
fn match_pattern(pattern: &Value, input: &Value) -> Option<HashMap<String, PatBinding>> {
    let mut bindings = HashMap::new();
    if match_inner(pattern, input, &mut bindings, true) {
        Some(bindings)
    } else {
        None
    }
}

fn match_inner(pattern: &Value, input: &Value, bindings: &mut HashMap<String, PatBinding>, top: bool) -> bool {
    match (pattern, input) {
        (Value::List(pat), Value::List(inp)) => match_list(pat, inp, bindings, top),
        (Value::Symbol(s), _) if s == "_" => true,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::Symbol(s), _) if !top => {
            bindings.insert(s.clone(), PatBinding::One(input.clone()));
            true
        }
        _ => false,
    }
}

fn match_list(pat: &[Value], inp: &[Value], bindings: &mut HashMap<String, PatBinding>, top: bool) -> bool {
    let mut pi = 0;
    let mut ii = 0;
    while pi < pat.len() {
        // Check for variadic: pattern[pi] followed by ...
        if pi + 1 < pat.len() && pat[pi + 1] == Value::Symbol("...".to_string()) {
            let var_name = match &pat[pi] {
                Value::Symbol(s) => s.clone(),
                _ => return false,
            };
            let remaining_pat = pat.len() - pi - 2;
            let available = if ii <= inp.len() { inp.len() - ii } else { return false };
            if available < remaining_pat {
                return false;
            }
            let var_count = available - remaining_pat;
            let values: Vec<Value> = inp[ii..ii + var_count].to_vec();
            bindings.insert(var_name, PatBinding::Many(values));
            ii += var_count;
            pi += 2;
        } else {
            if ii >= inp.len() {
                return false;
            }
            if pi == 0 && top {
                // Skip macro name (first element at top level)
                pi += 1;
                ii += 1;
                continue;
            }
            if !match_inner(&pat[pi], &inp[ii], bindings, false) {
                return false;
            }
            pi += 1;
            ii += 1;
        }
    }
    ii == inp.len()
}

/// Expand a macro: try each rule, return expanded form + hygiene bindings.
fn expand_macro(
    input: &[Value],
    rules: &[(Value, Value)],
    def_env: &Env,
    pos: Pos,
) -> Result<(Value, Vec<(String, Value)>), EvalError> {
    let input_val = Value::List(input.to_vec());
    for (pattern, template) in rules {
        if let Some(bindings) = match_pattern(pattern, &input_val) {
            // Collect pattern variable names
            let mut pat_vars = HashSet::new();
            collect_pattern_vars(pattern, &mut pat_vars, true);

            // Collect free symbols in template and generate renames
            let mut renames: HashMap<String, String> = HashMap::new();
            collect_free_syms(template, &pat_vars, &mut renames);

            // Build hygiene bindings (gensym → def_env value)
            let mut hygiene = Vec::new();
            for (original, gs) in &renames {
                if let Some(val) = env_get(def_env, original) {
                    hygiene.push((gs.clone(), val));
                }
            }

            // Substitute template
            let expanded = subst_template(template, &bindings, &renames, pos)?;
            return Ok((expanded, hygiene));
        }
    }
    Err(runtime_err(pos, "no matching syntax-rules pattern"))
}

/// Collect free symbols in a template (not pattern vars, not special forms, not builtins).
fn collect_free_syms(
    template: &Value,
    pat_vars: &HashSet<String>,
    renames: &mut HashMap<String, String>,
) {
    match template {
        Value::Symbol(s) if s == "..." || pat_vars.contains(s) => {}
        Value::Symbol(s) if is_special_form(s) || is_builtin(s) => {}
        Value::Symbol(s) => {
            if !renames.contains_key(s) {
                renames.insert(s.clone(), gensym(s));
            }
        }
        Value::List(elems) => {
            for elem in elems {
                collect_free_syms(elem, pat_vars, renames);
            }
        }
        _ => {}
    }
}

/// Substitute pattern variables and renames into a template.
fn subst_template(
    template: &Value,
    bindings: &HashMap<String, PatBinding>,
    renames: &HashMap<String, String>,
    pos: Pos,
) -> Result<Value, EvalError> {
    match template {
        Value::Symbol(s) => {
            if let Some(b) = bindings.get(s) {
                match b {
                    PatBinding::One(v) => Ok(v.clone()),
                    PatBinding::Many(_) => Err(runtime_err(pos, format!("unexpected variadic use of {}", s))),
                }
            } else if let Some(gs) = renames.get(s) {
                Ok(Value::Symbol(gs.clone()))
            } else {
                Ok(template.clone())
            }
        }
        Value::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() && elems[i + 1] == Value::Symbol("...".to_string()) {
                    // Variadic splice
                    match &elems[i] {
                        Value::Symbol(s) if bindings.contains_key(s) => {
                            if let Some(PatBinding::Many(vs)) = bindings.get(s) {
                                result.extend(vs.iter().cloned());
                            }
                        }
                        _ => {
                            result.push(subst_template(&elems[i], bindings, renames, pos)?);
                        }
                    }
                    i += 2;
                } else {
                    result.push(subst_template(&elems[i], bindings, renames, pos)?);
                    i += 1;
                }
            }
            Ok(Value::List(result))
        }
        _ => Ok(template.clone()),
    }
}

fn eqv_values(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        _ => std::ptr::eq(a as *const _, b as *const _),
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(xs), Value::List(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| values_equal(x, y))
        }
        _ => false,
    }
}

fn is_builtin(name: &str) -> bool {
    matches!(name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" |
        "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length" |
        "string?" | "number?" | "boolean?" | "pair?" | "symbol?" |
        "display" | "write" | "newline" |
        "string-append" | "string-length" | "substring" |
        "string->number" | "number->string" | "symbol->string" | "string->symbol" |
        "string-ref" | "string-copy" | "char?" | "string->list" | "list->string" |
        "string=?" | "string<?" | "string-ci=?" | "string-upcase" | "string-downcase" |
        "char->integer" | "integer->char" |
        "char=?" | "char<?" | "char-alphabetic?" | "char-numeric?" |
        "char-upcase" | "char-downcase" |
        "eq?" | "eqv?" | "equal?" |
        "apply" | "map" |
        "call/cc" | "call-with-current-continuation" |
        "vector" | "make-vector" | "vector-ref" | "vector-set!" |
        "vector-length" | "vector?" | "vector->list" | "list->vector" |
        "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt" |
        "zero?" | "positive?" | "negative?" | "odd?" | "even?" |
        "list-ref" | "list-tail" | "list?" | "assoc"
    )
}

fn call_builtin(name: &str, args: Vec<Value>, env: &Env, pos: Pos) -> Result<Value, EvalError> {
    match name {
        "call/cc" | "call-with-current-continuation" => {
            if args.len() != 1 {
                return Err(runtime_err(pos, "call/cc requires 1 argument"));
            }
            let pending = CONT_RETURN.with(|c| c.borrow_mut().take());
            if let Some(val) = pending {
                return Ok(val);
            }
            handle_callcc(&args[0], env, pos)
        }
        "apply" => {
            if args.len() < 2 {
                return Err(runtime_err(pos, "apply requires at least 2 arguments"));
            }
            let proc = args[0].clone();
            let last = match &args[args.len() - 1] {
                Value::List(l) => l.clone(),
                _ => return Err(runtime_err(pos, "apply: last argument must be a list")),
            };
            let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            all_args.extend(last);
            match &proc {
                Value::Lambda { params, rest_param, body, env: closure_env } => {
                    let call_env = bind_args("apply", params, rest_param, &all_args, closure_env, pos)?;
                    let mut result = Value::Boolean(false);
                    for expr in body.iter() {
                        result = eval(expr.clone(), &call_env, pos)?;
                    }
                    Ok(result)
                }
                Value::Builtin(bname) => apply_builtin_vals(bname, &all_args, pos),
                Value::Continuation(data) => invoke_continuation(data, &all_args, pos),
                _ => Err(runtime_err(pos, "apply: first argument must be a procedure")),
            }
        }
        "map" => {
            if args.len() < 2 {
                return Err(runtime_err(pos, "map requires at least 2 arguments"));
            }
            let func = &args[0];
            let mut lists: Vec<&Vec<Value>> = Vec::new();
            for arg in &args[1..] {
                match arg {
                    Value::List(items) => lists.push(items),
                    _ => return Err(runtime_err(pos, "map: arguments must be lists")),
                }
            }
            let len = lists[0].len();
            let mut results = Vec::new();
            for i in 0..len {
                let map_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                results.push(apply_proc(func, "map", &map_args, env, pos)?);
            }
            Ok(Value::List(results))
        }
        _ => apply_builtin_vals(name, &args, pos),
    }
}

fn apply_builtin_vals(op: &str, vals: &[Value], pos: Pos) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for v in vals {
                sum += expect_int(v, pos)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if vals.is_empty() {
                return Err(runtime_err(pos, "- requires at least 1 argument"));
            }
            if vals.len() == 1 {
                return Ok(Value::Integer(-expect_int(&vals[0], pos)?));
            }
            let mut result = expect_int(&vals[0], pos)?;
            for v in &vals[1..] {
                result -= expect_int(v, pos)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for v in vals {
                product *= expect_int(v, pos)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if vals.is_empty() {
                return Err(runtime_err(pos, "/ requires at least 1 argument"));
            }
            let mut result = expect_int(&vals[0], pos)?;
            for v in &vals[1..] {
                let d = expect_int(v, pos)?;
                if d == 0 {
                    return Err(runtime_err(pos, "division by zero"));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "< requires 2 arguments"));
            }
            Ok(Value::Boolean(
                expect_int(&vals[0], pos)? < expect_int(&vals[1], pos)?,
            ))
        }
        ">" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "> requires 2 arguments"));
            }
            Ok(Value::Boolean(
                expect_int(&vals[0], pos)? > expect_int(&vals[1], pos)?,
            ))
        }
        "=" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "= requires 2 arguments"));
            }
            Ok(Value::Boolean(
                expect_int(&vals[0], pos)? == expect_int(&vals[1], pos)?,
            ))
        }
        "<=" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "<= requires 2 arguments"));
            }
            Ok(Value::Boolean(
                expect_int(&vals[0], pos)? <= expect_int(&vals[1], pos)?,
            ))
        }
        ">=" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, ">= requires 2 arguments"));
            }
            Ok(Value::Boolean(
                expect_int(&vals[0], pos)? >= expect_int(&vals[1], pos)?,
            ))
        }
        "not" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "not requires 1 argument"));
            }
            Ok(Value::Boolean(vals[0] == Value::Boolean(false)))
        }
        "cons" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "cons requires 2 arguments"));
            }
            match &vals[1] {
                Value::List(tail) => {
                    let mut new_list = vec![vals[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => Ok(Value::List(vec![
                    vals[0].clone(),
                    Value::Symbol(".".to_string()),
                    vals[1].clone(),
                ])),
            }
        }
        "car" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "car requires 1 argument"));
            }
            match &vals[0] {
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(runtime_err(pos, "car: not a pair")),
            }
        }
        "cdr" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "cdr requires 1 argument"));
            }
            match &vals[0] {
                Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
                _ => Err(runtime_err(pos, "cdr: not a pair")),
            }
        }
        "null?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "null? requires 1 argument"));
            }
            Ok(Value::Boolean(
                matches!(&vals[0], Value::List(elems) if elems.is_empty()),
            ))
        }
        "list" => Ok(Value::List(vals.to_vec())),
        "length" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "length requires 1 argument"));
            }
            match &vals[0] {
                Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                _ => Err(runtime_err(pos, "length: not a list")),
            }
        }
        "list-ref" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "list-ref requires 2 arguments"));
            }
            let idx = expect_int(&vals[1], pos)? as usize;
            match &vals[0] {
                Value::List(elems) => {
                    if idx < elems.len() {
                        Ok(elems[idx].clone())
                    } else {
                        Err(runtime_err(pos, "list-ref: index out of range"))
                    }
                }
                _ => Err(runtime_err(pos, "list-ref: not a list")),
            }
        }
        "list-tail" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "list-tail requires 2 arguments"));
            }
            let idx = expect_int(&vals[1], pos)? as usize;
            match &vals[0] {
                Value::List(elems) => {
                    if idx <= elems.len() {
                        Ok(Value::List(elems[idx..].to_vec()))
                    } else {
                        Err(runtime_err(pos, "list-tail: index out of range"))
                    }
                }
                _ => Err(runtime_err(pos, "list-tail: not a list")),
            }
        }
        "list?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "list? requires 1 argument"));
            }
            match &vals[0] {
                Value::List(elems) => {
                    // A dotted pair like (cons 1 2) is stored as [1, Symbol("."), 2]
                    let is_proper = elems.is_empty()
                        || elems.len() < 3
                        || !matches!(&elems[elems.len() - 2], Value::Symbol(s) if s == ".");
                    Ok(Value::Boolean(is_proper))
                }
                _ => Ok(Value::Boolean(false)),
            }
        }
        "assoc" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "assoc requires 2 arguments"));
            }
            let key = &vals[0];
            match &vals[1] {
                Value::List(alist) => {
                    for entry in alist {
                        if let Value::List(pair) = entry {
                            if !pair.is_empty() && values_equal(&pair[0], key) {
                                return Ok(entry.clone());
                            }
                        }
                    }
                    Ok(Value::Boolean(false))
                }
                _ => Err(runtime_err(pos, "assoc: second argument must be a list")),
            }
        }
        "string?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "string? requires 1 argument"));
            }
            Ok(Value::Boolean(matches!(&vals[0], Value::Str(_))))
        }
        "number?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "number? requires 1 argument"));
            }
            Ok(Value::Boolean(matches!(&vals[0], Value::Integer(_))))
        }
        "boolean?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "boolean? requires 1 argument"));
            }
            Ok(Value::Boolean(matches!(&vals[0], Value::Boolean(_))))
        }
        "pair?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "pair? requires 1 argument"));
            }
            Ok(Value::Boolean(
                matches!(&vals[0], Value::List(elems) if !elems.is_empty()),
            ))
        }
        "symbol?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "symbol? requires 1 argument"));
            }
            Ok(Value::Boolean(matches!(&vals[0], Value::Symbol(_))))
        }
        "eq?" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "eq? requires 2 arguments"));
            }
            Ok(Value::Boolean(eqv_values(&vals[0], &vals[1])))
        }
        "eqv?" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "eqv? requires 2 arguments"));
            }
            Ok(Value::Boolean(eqv_values(&vals[0], &vals[1])))
        }
        "equal?" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "equal? requires 2 arguments"));
            }
            Ok(Value::Boolean(values_equal(&vals[0], &vals[1])))
        }
        "display" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "display requires 1 argument"));
            }
            output_write(&vals[0].display_unquoted());
            Ok(Value::Boolean(false))
        }
        "write" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "write requires 1 argument"));
            }
            output_write(&vals[0].display());
            Ok(Value::Boolean(false))
        }
        "newline" => {
            if !vals.is_empty() {
                return Err(runtime_err(pos, "newline takes no arguments"));
            }
            output_write("\n");
            Ok(Value::Boolean(false))
        }
        "string-append" => {
            let mut result = String::new();
            for v in vals {
                match v {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(runtime_err(pos, "string-append: expected string")),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "string-length requires 1 argument"));
            }
            match &vals[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(runtime_err(pos, "string-length: expected string")),
            }
        }
        "substring" => {
            if vals.len() != 3 {
                return Err(runtime_err(pos, "substring requires 3 arguments"));
            }
            let s = match &vals[0] {
                Value::Str(s) => s,
                _ => return Err(runtime_err(pos, "substring: expected string")),
            };
            let start = expect_int(&vals[1], pos)? as usize;
            let end = expect_int(&vals[2], pos)? as usize;
            if start > end || end > s.len() {
                return Err(runtime_err(pos, "substring: index out of bounds"));
            }
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "string->number requires 1 argument"));
            }
            match &vals[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(runtime_err(pos, "string->number: expected string")),
            }
        }
        "number->string" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "number->string requires 1 argument"));
            }
            let n = expect_int(&vals[0], pos)?;
            Ok(Value::Str(n.to_string()))
        }
        "symbol->string" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "symbol->string requires 1 argument"));
            }
            match &vals[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(runtime_err(pos, "symbol->string: expected symbol")),
            }
        }
        "string->symbol" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "string->symbol requires 1 argument"));
            }
            match &vals[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(runtime_err(pos, "string->symbol: expected string")),
            }
        }
        "string-ref" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "string-ref requires 2 arguments"));
            }
            let s = match &vals[0] {
                Value::Str(s) => s,
                _ => return Err(runtime_err(pos, "string-ref: expected string")),
            };
            let idx = expect_int(&vals[1], pos)? as usize;
            if idx >= s.len() {
                return Err(runtime_err(pos, "string-ref: index out of bounds"));
            }
            Ok(Value::Char(s.chars().nth(idx).unwrap()))
        }
        "string-copy" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "string-copy requires 1 argument"));
            }
            match &vals[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(runtime_err(pos, "string-copy: expected string")),
            }
        }
        "char?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "char? requires 1 argument"));
            }
            Ok(Value::Boolean(matches!(&vals[0], Value::Char(_))))
        }
        "string->list" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "string->list requires 1 argument"));
            }
            match &vals[0] {
                Value::Str(s) => Ok(Value::List(s.chars().map(Value::Char).collect())),
                _ => Err(runtime_err(pos, "string->list: expected string")),
            }
        }
        "list->string" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "list->string requires 1 argument"));
            }
            match &vals[0] {
                Value::List(elems) => {
                    let mut s = String::new();
                    for v in elems {
                        match v {
                            Value::Char(c) => s.push(*c),
                            _ => return Err(runtime_err(pos, "list->string: expected list of characters")),
                        }
                    }
                    Ok(Value::Str(s))
                }
                _ => Err(runtime_err(pos, "list->string: expected list")),
            }
        }
        "string=?" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "string=? requires 2 arguments"));
            }
            match (&vals[0], &vals[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(runtime_err(pos, "string=?: expected strings")),
            }
        }
        "string<?" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "string<? requires 2 arguments"));
            }
            match (&vals[0], &vals[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(runtime_err(pos, "string<?: expected strings")),
            }
        }
        "string-ci=?" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "string-ci=? requires 2 arguments"));
            }
            match (&vals[0], &vals[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
                _ => Err(runtime_err(pos, "string-ci=?: expected strings")),
            }
        }
        "string-upcase" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "string-upcase requires 1 argument"));
            }
            match &vals[0] {
                Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
                _ => Err(runtime_err(pos, "string-upcase: expected string")),
            }
        }
        "string-downcase" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "string-downcase requires 1 argument"));
            }
            match &vals[0] {
                Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
                _ => Err(runtime_err(pos, "string-downcase: expected string")),
            }
        }
        "char->integer" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "char->integer requires 1 argument"));
            }
            match &vals[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => Err(runtime_err(pos, "char->integer: expected char")),
            }
        }
        "integer->char" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "integer->char requires 1 argument"));
            }
            let n = expect_int(&vals[0], pos)?;
            Ok(Value::Char(char::from_u32(n as u32).unwrap_or('\u{FFFD}')))
        }
        "char=?" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "char=? requires 2 arguments"));
            }
            match (&vals[0], &vals[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(runtime_err(pos, "char=?: expected characters")),
            }
        }
        "char<?" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "char<? requires 2 arguments"));
            }
            match (&vals[0], &vals[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(runtime_err(pos, "char<?: expected characters")),
            }
        }
        "char-alphabetic?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "char-alphabetic? requires 1 argument"));
            }
            match &vals[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(runtime_err(pos, "char-alphabetic?: expected char")),
            }
        }
        "char-numeric?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "char-numeric? requires 1 argument"));
            }
            match &vals[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(runtime_err(pos, "char-numeric?: expected char")),
            }
        }
        "char-upcase" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "char-upcase requires 1 argument"));
            }
            match &vals[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => Err(runtime_err(pos, "char-upcase: expected char")),
            }
        }
        "char-downcase" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "char-downcase requires 1 argument"));
            }
            match &vals[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                _ => Err(runtime_err(pos, "char-downcase: expected char")),
            }
        }
        "vector" => {
            Ok(Value::Vector(Rc::new(RefCell::new(vals.to_vec()))))
        }
        "make-vector" => {
            if vals.is_empty() || vals.len() > 2 {
                return Err(runtime_err(pos, "make-vector requires 1 or 2 arguments"));
            }
            let n = expect_int(&vals[0], pos)? as usize;
            let fill = if vals.len() == 2 { vals[1].clone() } else { Value::Integer(0) };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; n]))))
        }
        "vector-ref" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "vector-ref requires 2 arguments"));
            }
            match &vals[0] {
                Value::Vector(v) => {
                    let idx = expect_int(&vals[1], pos)? as usize;
                    let vec = v.borrow();
                    vec.get(idx).cloned().ok_or_else(|| runtime_err(pos, "vector-ref: index out of bounds"))
                }
                _ => Err(runtime_err(pos, "vector-ref: expected vector")),
            }
        }
        "vector-set!" => {
            if vals.len() != 3 {
                return Err(runtime_err(pos, "vector-set! requires 3 arguments"));
            }
            match &vals[0] {
                Value::Vector(v) => {
                    let idx = expect_int(&vals[1], pos)? as usize;
                    let mut vec = v.borrow_mut();
                    if idx >= vec.len() {
                        return Err(runtime_err(pos, "vector-set!: index out of bounds"));
                    }
                    vec[idx] = vals[2].clone();
                    Ok(Value::Boolean(false))
                }
                _ => Err(runtime_err(pos, "vector-set!: expected vector")),
            }
        }
        "vector-length" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "vector-length requires 1 argument"));
            }
            match &vals[0] {
                Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
                _ => Err(runtime_err(pos, "vector-length: expected vector")),
            }
        }
        "vector?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "vector? requires 1 argument"));
            }
            Ok(Value::Boolean(matches!(&vals[0], Value::Vector(_))))
        }
        "vector->list" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "vector->list requires 1 argument"));
            }
            match &vals[0] {
                Value::Vector(v) => Ok(Value::List(v.borrow().clone())),
                _ => Err(runtime_err(pos, "vector->list: expected vector")),
            }
        }
        "list->vector" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "list->vector requires 1 argument"));
            }
            match &vals[0] {
                Value::List(l) => Ok(Value::Vector(Rc::new(RefCell::new(l.clone())))),
                _ => Err(runtime_err(pos, "list->vector: expected list")),
            }
        }
        "abs" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "abs requires 1 argument"));
            }
            Ok(Value::Integer(expect_int(&vals[0], pos)?.abs()))
        }
        "modulo" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "modulo requires 2 arguments"));
            }
            let a = expect_int(&vals[0], pos)?;
            let b = expect_int(&vals[1], pos)?;
            if b == 0 {
                return Err(runtime_err(pos, "modulo: division by zero"));
            }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "remainder requires 2 arguments"));
            }
            let a = expect_int(&vals[0], pos)?;
            let b = expect_int(&vals[1], pos)?;
            if b == 0 {
                return Err(runtime_err(pos, "remainder: division by zero"));
            }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "quotient requires 2 arguments"));
            }
            let a = expect_int(&vals[0], pos)?;
            let b = expect_int(&vals[1], pos)?;
            if b == 0 {
                return Err(runtime_err(pos, "quotient: division by zero"));
            }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if vals.is_empty() {
                return Err(runtime_err(pos, "min requires at least 1 argument"));
            }
            let mut result = expect_int(&vals[0], pos)?;
            for v in &vals[1..] {
                let n = expect_int(v, pos)?;
                if n < result { result = n; }
            }
            Ok(Value::Integer(result))
        }
        "max" => {
            if vals.is_empty() {
                return Err(runtime_err(pos, "max requires at least 1 argument"));
            }
            let mut result = expect_int(&vals[0], pos)?;
            for v in &vals[1..] {
                let n = expect_int(v, pos)?;
                if n > result { result = n; }
            }
            Ok(Value::Integer(result))
        }
        "expt" => {
            if vals.len() != 2 {
                return Err(runtime_err(pos, "expt requires 2 arguments"));
            }
            let base = expect_int(&vals[0], pos)?;
            let exp = expect_int(&vals[1], pos)?;
            if exp < 0 {
                return Err(runtime_err(pos, "expt: negative exponent"));
            }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "zero? requires 1 argument"));
            }
            Ok(Value::Boolean(expect_int(&vals[0], pos)? == 0))
        }
        "positive?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "positive? requires 1 argument"));
            }
            Ok(Value::Boolean(expect_int(&vals[0], pos)? > 0))
        }
        "negative?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "negative? requires 1 argument"));
            }
            Ok(Value::Boolean(expect_int(&vals[0], pos)? < 0))
        }
        "odd?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "odd? requires 1 argument"));
            }
            Ok(Value::Boolean(expect_int(&vals[0], pos)? % 2 != 0))
        }
        "even?" => {
            if vals.len() != 1 {
                return Err(runtime_err(pos, "even? requires 1 argument"));
            }
            Ok(Value::Boolean(expect_int(&vals[0], pos)? % 2 == 0))
        }
        _ => Err(runtime_err(pos, format!("unknown procedure: {}", op))),
    }
}

fn expect_int(v: &Value, pos: Pos) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(runtime_err(pos, "expected integer")),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    output_take();
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".to_string()));
    }
    let env = new_env(None);
    let result = eval_top_level(&exprs, &env)?;
    let output = output_take();
    Ok((result.display(), output))
}

#[cfg(test)]
mod tests;
