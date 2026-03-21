pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

thread_local! {
    static OUTPUT_BUFFER: RefCell<String> = RefCell::new(String::new());
}

fn output_write(s: &str) {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(s));
}

fn output_take() -> String {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().split_off(0))
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
        body: Vec<Value>,
        env: Env,
    },
}

#[derive(Debug, Clone, PartialEq)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

type Env = Rc<RefCell<EnvInner>>;

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

fn env_set_existing(env: &Env, name: &str, val: Value) {
    let mut inner = env.borrow_mut();
    if inner.bindings.contains_key(name) {
        inner.bindings.insert(name.to_string(), val);
    } else if let Some(ref parent) = inner.parent {
        env_set_existing(parent, name, val);
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
            Value::Char(c) => format!("#\\{}", c),
            Value::Lambda { .. } => "#<procedure>".to_string(),
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

type Pos = (usize, usize);

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
    let mut result = Value::Boolean(false);
    for (expr, pos) in exprs {
        result = eval(expr, &env, pos)?;
    }
    Ok(result.display())
}

fn eval(mut expr: Value, env: &Env, pos: Pos) -> Result<Value, EvalError> {
    let mut current_env = env.clone();
    let current_pos = pos;

    loop {
        let current = std::mem::replace(&mut expr, Value::Boolean(false));
        match current {
            Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Char(_) | Value::Lambda { .. } => return Ok(current),
            Value::Symbol(s) => {
                return env_get(&current_env, &s).ok_or_else(|| runtime_err(current_pos, format!("unbound symbol: {}", s)));
            }
            Value::List(elems) => {
                if elems.is_empty() {
                    return Err(runtime_err(current_pos, "empty application"));
                }
                let first = &elems[0];
                match first {
                    Value::Symbol(op) => match op.as_str() {
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
                                    let params: Result<Vec<String>, _> = sig[1..]
                                        .iter()
                                        .map(|v| match v {
                                            Value::Symbol(s) => Ok(s.clone()),
                                            _ => Err(runtime_err(
                                                current_pos,
                                                "define: expected symbol in params",
                                            )),
                                        })
                                        .collect();
                                    let lambda = Value::Lambda {
                                        params: params?,
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
                            let params = match &elems[1] {
                                Value::List(ps) => {
                                    let mut names = Vec::new();
                                    for p in ps {
                                        match p {
                                            Value::Symbol(s) => names.push(s.clone()),
                                            _ => {
                                                return Err(runtime_err(
                                                    current_pos,
                                                    "lambda: expected symbol in params",
                                                ))
                                            }
                                        }
                                    }
                                    names
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
                        "string-set!" => {
                            return Err(runtime_err(current_pos, "string-set!: strings are immutable"));
                        }
                        "map" => {
                            if elems.len() != 3 {
                                return Err(runtime_err(current_pos, "map requires 2 arguments"));
                            }
                            let func = eval(elems[1].clone(), &current_env, current_pos)?;
                            let lst = eval(elems[2].clone(), &current_env, current_pos)?;
                            match lst {
                                Value::List(items) => {
                                    let mut results = Vec::new();
                                    for item in &items {
                                        results.push(apply_proc(&func, "map", &[item.clone()], &current_env, current_pos)?);
                                    }
                                    return Ok(Value::List(results));
                                }
                                _ => return Err(runtime_err(current_pos, "map: second argument must be a list")),
                            }
                        }
                        _ => {
                            let mut args = Vec::new();
                            for arg in &elems[1..] {
                                args.push(eval(arg.clone(), &current_env, current_pos)?);
                            }
                            // Try as variable (user-defined procedure) first, then builtin
                            if let Some(proc) = env_get(&current_env, op) {
                                match proc {
                                    Value::Lambda { params, body, env: closure_env } => {
                                        if args.len() != params.len() {
                                            return Err(runtime_err(
                                                current_pos,
                                                format!("{}: expected {} arguments, got {}", op, params.len(), args.len()),
                                            ));
                                        }
                                        let call_env = new_env(Some(closure_env));
                                        for (param, arg) in params.iter().zip(args.iter()) {
                                            env_set(&call_env, param.clone(), arg.clone());
                                        }
                                        // Eval all but last body expr, then TCO on last
                                        for e in &body[..body.len().saturating_sub(1)] {
                                            eval(e.clone(), &call_env, current_pos)?;
                                        }
                                        expr = body.last().cloned().unwrap_or(Value::Boolean(false));
                                        current_env = call_env;
                                        continue;
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
                            Value::Lambda { params, body, env: closure_env } => {
                                if args.len() != params.len() {
                                    return Err(runtime_err(
                                        current_pos,
                                        format!("<anonymous>: expected {} arguments, got {}", params.len(), args.len()),
                                    ));
                                }
                                let call_env = new_env(Some(closure_env));
                                for (param, arg) in params.iter().zip(args.iter()) {
                                    env_set(&call_env, param.clone(), arg.clone());
                                }
                                for e in &body[..body.len().saturating_sub(1)] {
                                    eval(e.clone(), &call_env, current_pos)?;
                                }
                                expr = body.last().cloned().unwrap_or(Value::Boolean(false));
                                current_env = call_env;
                                continue;
                            }
                            _ => return Err(runtime_err(current_pos, format!("<anonymous> is not a procedure"))),
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
            body,
            env: closure_env,
        } => {
            if args.len() != params.len() {
                return Err(runtime_err(
                    pos,
                    format!(
                        "{}: expected {} arguments, got {}",
                        name,
                        params.len(),
                        args.len()
                    ),
                ));
            }
            let call_env = new_env(Some(closure_env.clone()));
            for (param, arg) in params.iter().zip(args.iter()) {
                env_set(&call_env, param.clone(), arg.clone());
            }
            let mut result = Value::Boolean(false);
            for expr in body.iter() {
                result = eval(expr.clone(), &call_env, pos)?;
            }
            Ok(result)
        }
        _ => Err(runtime_err(
            pos,
            format!("{} is not a procedure", name),
        )),
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
    // Clear any prior output
    output_take();
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".to_string()));
    }
    let env = new_env(None);
    let mut result = Value::Boolean(false);
    for (expr, pos) in exprs {
        result = eval(expr, &env, pos)?;
    }
    let output = output_take();
    Ok((result.display(), output))
}

#[cfg(test)]
mod tests;
