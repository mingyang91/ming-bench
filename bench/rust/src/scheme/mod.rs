pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct EnvFrame {
    bindings: HashMap<String, Value>,
    parent: Option<EnvRef>,
}

type EnvRef = Rc<RefCell<EnvFrame>>;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Char(char),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: EnvRef,
    },
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Void, Value::Void) => true,
            (Value::Lambda { .. }, Value::Lambda { .. }) => false,
            _ => false,
        }
    }
}

impl Value {
    /// `write` representation: strings are quoted
    fn display_scheme(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Char(c) => format!("#\\{c}"),
            Value::Str(s) => format!("\"{s}\""),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_scheme()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda { .. } => "#<procedure>".to_string(),
            Value::Void => "".to_string(),
        }
    }

    /// `display` representation: strings are unquoted
    fn display_output(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            other => other.display_scheme(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// --- Positions ---

#[derive(Debug, Clone, Copy, PartialEq)]
struct Pos {
    line: usize,
    col: usize,
}

impl Pos {
    fn new(line: usize, col: usize) -> Self {
        Pos { line, col }
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// --- Tokenizer ---

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
}

#[derive(Debug, Clone)]
struct SpannedToken {
    token: Token,
    pos: Pos,
}

fn tokenize(input: &str) -> Result<Vec<SpannedToken>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line = 1usize;
    let mut col = 1usize;

    while i < chars.len() {
        match chars[i] {
            '\n' => { line += 1; col = 1; i += 1; }
            ' ' | '\t' | '\r' => { col += 1; i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => { tokens.push(SpannedToken { token: Token::LParen, pos: Pos::new(line, col) }); i += 1; col += 1; }
            ')' => { tokens.push(SpannedToken { token: Token::RParen, pos: Pos::new(line, col) }); i += 1; col += 1; }
            '"' => {
                let start_pos = Pos::new(line, col);
                i += 1; col += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1; col += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            c => { s.push('\\'); s.push(c); }
                        }
                    } else if chars[i] == '\n' {
                        s.push('\n');
                        line += 1; col = 0;
                    } else {
                        s.push(chars[i]);
                    }
                    i += 1; col += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse(format!("{start_pos}: unterminated string")));
                }
                i += 1; col += 1;
                tokens.push(SpannedToken { token: Token::Str(s), pos: start_pos });
            }
            '\'' => {
                tokens.push(SpannedToken { token: Token::Symbol("'".to_string()), pos: Pos::new(line, col) });
                i += 1; col += 1;
            }
            '#' => {
                let start_pos = Pos::new(line, col);
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => { tokens.push(SpannedToken { token: Token::Boolean(true), pos: start_pos }); i += 2; col += 2; }
                        'f' => { tokens.push(SpannedToken { token: Token::Boolean(false), pos: start_pos }); i += 2; col += 2; }
                        '\\' => {
                            // Character literal: #\x or #\space, #\newline, #\tab
                            i += 2; col += 2;
                            if i >= chars.len() {
                                return Err(EvalError::Parse(format!("{start_pos}: incomplete character literal")));
                            }
                            // Collect the name
                            let name_start = i;
                            while i < chars.len() && !chars[i].is_whitespace() && chars[i] != ')' && chars[i] != '(' {
                                i += 1; col += 1;
                            }
                            let name: String = chars[name_start..i].iter().collect();
                            let ch = match name.as_str() {
                                "space" => ' ',
                                "newline" => '\n',
                                "tab" => '\t',
                                s if s.len() == 1 => s.chars().next().expect("single-char string guaranteed non-empty"),
                                _ => return Err(EvalError::Parse(format!("{start_pos}: unknown character name: {name}"))),
                            };
                            tokens.push(SpannedToken { token: Token::Char(ch), pos: start_pos });
                        }
                        _ => return Err(EvalError::Parse(format!("{start_pos}: unexpected #-literal")))
                    }
                } else {
                    return Err(EvalError::Parse(format!("{start_pos}: unexpected #")));
                }
            }
            _ => {
                let start_pos = Pos::new(line, col);
                let start = i;
                while i < chars.len() && !matches!(chars[i], ' '|'\t'|'\n'|'\r'|'('|')'|'"'|';') {
                    i += 1; col += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push(SpannedToken { token: Token::Integer(n), pos: start_pos });
                } else {
                    tokens.push(SpannedToken { token: Token::Symbol(word), pos: start_pos });
                }
            }
        }
    }
    Ok(tokens)
}

// --- Parser ---

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    pos: Pos,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

impl Expr {
    fn new(kind: ExprKind, pos: Pos) -> Self {
        Expr { kind, pos }
    }
}

fn parse_tokens(tokens: &[SpannedToken], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let spos = tokens[*pos].pos;
    match &tokens[*pos].token {
        Token::LParen => {
            *pos += 1;
            let mut items = Vec::new();
            while *pos < tokens.len() && tokens[*pos].token != Token::RParen {
                items.push(parse_tokens(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse(format!("{spos}: missing closing paren")));
            }
            *pos += 1;
            Ok(Expr::new(ExprKind::List(items), spos))
        }
        Token::RParen => Err(EvalError::Parse(format!("{spos}: unexpected )"))),
        Token::Integer(n) => { let n = *n; *pos += 1; Ok(Expr::new(ExprKind::Integer(n), spos)) }
        Token::Boolean(b) => { let b = *b; *pos += 1; Ok(Expr::new(ExprKind::Boolean(b), spos)) }
        Token::Str(s) => { let s = s.clone(); *pos += 1; Ok(Expr::new(ExprKind::Str(s), spos)) }
        Token::Char(c) => { let c = *c; *pos += 1; Ok(Expr::new(ExprKind::Char(c), spos)) }
        Token::Symbol(s) if s == "'" => {
            *pos += 1;
            let inner = parse_tokens(tokens, pos)?;
            Ok(Expr::new(ExprKind::List(vec![
                Expr::new(ExprKind::Symbol("quote".to_string()), spos),
                inner,
            ]), spos))
        }
        Token::Symbol(s) => { let s = s.clone(); *pos += 1; Ok(Expr::new(ExprKind::Symbol(s), spos)) }
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_tokens(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// --- Evaluator ---

fn new_env(parent: Option<EnvRef>) -> EnvRef {
    Rc::new(RefCell::new(EnvFrame {
        bindings: HashMap::new(),
        parent,
    }))
}

fn env_get(env: &EnvRef, name: &str) -> Option<Value> {
    let frame = env.borrow();
    if let Some(val) = frame.bindings.get(name) {
        Some(val.clone())
    } else if let Some(parent) = &frame.parent {
        env_get(parent, name)
    } else {
        None
    }
}

fn env_set(env: &EnvRef, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

fn eval_expr(expr: &Expr, env: &EnvRef, out: &RefCell<String>) -> Result<Value, EvalError> {
    let p = expr.pos;
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Symbol(name) => {
            env_get(env, name)
                .ok_or_else(|| EvalError::UnboundVariable(format!("{p}: {name}")))
        }
        ExprKind::List(items) => {
            if items.is_empty() {
                return Ok(Value::List(vec![]));
            }
            if let ExprKind::Symbol(op) = &items[0].kind {
                match op.as_str() {
                    "and" => return eval_and(&items[1..], env, out),
                    "or" => return eval_or(&items[1..], env, out),
                    "define" => return eval_define(&items[1..], env, p, out),
                    "if" => return eval_if(&items[1..], env, p, out),
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity(format!("{p}: quote requires 1 argument")));
                        }
                        return Ok(expr_to_value(&items[1]));
                    }
                    "lambda" => return eval_lambda(&items[1..], env, p),
                    "let" => return eval_let(&items[1..], env, p, out),
                    "begin" => return eval_begin(&items[1..], env, out),
                    "cond" => return eval_cond(&items[1..], env, out),
                    "set!" => {
                        if items.len() != 3 {
                            return Err(EvalError::Arity(format!("{p}: set! requires 2 arguments")));
                        }
                        let name = match &items[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Type(format!("{p}: set!: expected symbol"))),
                        };
                        let val = eval_expr(&items[2], env, out)?;
                        if !env_set_existing(env, &name, val) {
                            return Err(EvalError::UnboundVariable(format!("{p}: {name}")));
                        }
                        return Ok(Value::Void);
                    }
                    "string-set!" => return eval_string_set(&items[1..], env, p, out),
                    _ => {}
                }
            }
            let func = eval_expr(&items[0], env, out)?;
            let args: Result<Vec<Value>, _> = items[1..].iter().map(|e| eval_expr(e, env, out)).collect();
            let args = args?;
            apply_func(&func, &args, p, out)
        }
    }
}

fn eval_and(exprs: &[Expr], env: &EnvRef, out: &RefCell<String>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    for (i, expr) in exprs.iter().enumerate() {
        let val = eval_expr(expr, env, out)?;
        if !val.is_truthy() || i == exprs.len() - 1 {
            return Ok(val);
        }
    }
    unreachable!()
}

fn eval_or(exprs: &[Expr], env: &EnvRef, out: &RefCell<String>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for (i, expr) in exprs.iter().enumerate() {
        let val = eval_expr(expr, env, out)?;
        if val.is_truthy() || i == exprs.len() - 1 {
            return Ok(val);
        }
    }
    unreachable!()
}

fn eval_define(args: &[Expr], env: &EnvRef, p: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!("{p}: define requires at least 2 arguments")));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{p}: define requires 2 arguments")));
            }
            let val = eval_expr(&args[1], env, out)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse(format!("{p}: define: empty signature")));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type(format!("{p}: define: expected symbol as function name"))),
            };
            let (params, rest_param) = parse_params(&sig[1..], p)?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda { params, rest_param, body, env: env.clone() };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type(format!("{p}: define: expected symbol or list"))),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef, p: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity(format!("{p}: if requires 2 or 3 arguments")));
    }
    let cond = eval_expr(&args[0], env, out)?;
    if cond.is_truthy() {
        eval_expr(&args[1], env, out)
    } else if args.len() == 3 {
        eval_expr(&args[2], env, out)
    } else {
        Ok(Value::Void)
    }
}

fn parse_params(sig: &[Expr], p: Pos) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < sig.len() {
        match &sig[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 != sig.len() - 1 {
                    return Err(EvalError::Parse(format!("{p}: malformed dot in parameter list")));
                }
                match &sig[i + 1].kind {
                    ExprKind::Symbol(r) => rest_param = Some(r.clone()),
                    _ => return Err(EvalError::Type(format!("{p}: expected symbol after dot"))),
                }
                break;
            }
            ExprKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(EvalError::Type(format!("{}: expected symbol in params", sig[i].pos))),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_lambda(args: &[Expr], env: &EnvRef, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{p}: lambda requires params and body")));
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(items) => parse_params(items, p)?,
        ExprKind::Symbol(s) => {
            // (lambda rest body) — single rest param captures all args
            (vec![], Some(s.clone()))
        }
        _ => return Err(EvalError::Type(format!("{}: lambda: expected parameter list", args[0].pos))),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda { params, rest_param, body, env: env.clone() })
}

fn eval_let(args: &[Expr], env: &EnvRef, p: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{p}: let requires bindings and body")));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 3 {
            return Err(EvalError::Arity(format!("{p}: named let requires bindings and body")));
        }
        let bindings_expr = match &args[1].kind {
            ExprKind::List(items) => items,
            _ => return Err(EvalError::Type(format!("{p}: let: expected bindings list"))),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings_expr {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        params.push(s.clone());
                        inits.push(eval_expr(&pair[1], env, out)?);
                    } else {
                        return Err(EvalError::Type(format!("{}: let: expected symbol in binding", pair[0].pos)));
                    }
                }
                _ => return Err(EvalError::Type(format!("{}: let: expected (var init) binding", b.pos))),
            }
        }
        let body = args[2..].to_vec();
        let local_env = new_env(Some(env.clone()));
        let lambda = Value::Lambda { params: params.clone(), rest_param: None, body, env: local_env.clone() };
        env_set(&local_env, name.clone(), lambda);
        for (param, init) in params.iter().zip(inits.iter()) {
            env_set(&local_env, param.clone(), init.clone());
        }
        let mut result = Value::Void;
        for expr in &args[2..] {
            result = eval_expr(expr, &local_env, out)?;
        }
        return Ok(result);
    }
    let bindings_expr = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type(format!("{p}: let: expected bindings list"))),
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval_expr(&pair[1], env, out)?;
                    env_set(&local_env, s.clone(), val);
                } else {
                    return Err(EvalError::Type(format!("{}: let: expected symbol in binding", pair[0].pos)));
                }
            }
            _ => return Err(EvalError::Type(format!("{}: let: expected (var init) binding", b.pos))),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval_expr(expr, &local_env, out)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &EnvRef, out: &RefCell<String>) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval_expr(expr, env, out)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &EnvRef, out: &RefCell<String>) -> Result<Value, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(items) if !items.is_empty() => {
                if let ExprKind::Symbol(s) = &items[0].kind {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &items[1..] {
                            result = eval_expr(expr, env, out)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval_expr(&items[0], env, out)?;
                if test.is_truthy() {
                    let mut result = test;
                    for expr in &items[1..] {
                        result = eval_expr(expr, env, out)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Type(format!("{}: cond: expected clause list", clause.pos))),
        }
    }
    Ok(Value::Void)
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
    }
}

fn apply_func(func: &Value, args: &[Value], call_pos: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { params, rest_param, body, env } => {
            if rest_param.is_some() {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "{call_pos}: expected at least {} args, got {}", params.len(), args.len()
                    )));
                }
            } else if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: expected {} args, got {}", params.len(), args.len()
                )));
            }
            let local_env = new_env(Some(env.clone()));
            for (param, arg) in params.iter().zip(args.iter()) {
                env_set(&local_env, param.clone(), arg.clone());
            }
            if let Some(rest) = rest_param {
                let rest_args = if args.len() > params.len() {
                    args[params.len()..].to_vec()
                } else {
                    vec![]
                };
                env_set(&local_env, rest.clone(), Value::List(rest_args));
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval_expr(expr, &local_env, out)?;
            }
            Ok(result)
        }
        Value::Symbol(s) => apply_builtin_by_name(s, args, call_pos, out),
        _ => Err(EvalError::Type(format!("{call_pos}: not a procedure: {}", func.display_scheme()))),
    }
}

fn apply_string_builtin(name: &str, args: &[Value], call_pos: Pos) -> Result<Value, EvalError> {
    match name {
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type(format!("{call_pos}: string-append: expected string"))),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string-length requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type(format!("{call_pos}: string-length: expected string"))),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity(format!("{call_pos}: substring requires 3 arguments")));
            }
            match &args[0] {
                Value::Str(s) => {
                    let start = expect_int(&args[1], call_pos)? as usize;
                    let end = expect_int(&args[2], call_pos)? as usize;
                    if end > s.len() || start > end {
                        return Err(EvalError::Type(format!("{call_pos}: substring: index out of range")));
                    }
                    Ok(Value::Str(s[start..end].to_string()))
                }
                _ => Err(EvalError::Type(format!("{call_pos}: substring: expected string"))),
            }
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string->number requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type(format!("{call_pos}: string->number: expected string"))),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: number->string requires 1 argument")));
            }
            Ok(Value::Str(expect_int(&args[0], call_pos)?.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: symbol->string requires 1 argument")));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!("{call_pos}: symbol->string: expected symbol"))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string->symbol requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type(format!("{call_pos}: string->symbol: expected string"))),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: string-ref requires 2 arguments")));
            }
            match &args[0] {
                Value::Str(s) => {
                    let idx = expect_int(&args[1], call_pos)? as usize;
                    if idx >= s.len() {
                        return Err(EvalError::Type(format!("{call_pos}: string-ref: index out of range")));
                    }
                    Ok(Value::Char(s.as_bytes()[idx] as char))
                }
                _ => Err(EvalError::Type(format!("{call_pos}: string-ref: expected string"))),
            }
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: char? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string-copy requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!("{call_pos}: string-copy: expected string"))),
            }
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

fn apply_builtin_by_name(name: &str, args: &[Value], call_pos: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_int(a, call_pos)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: - requires at least 1 argument")));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-expect_int(&args[0], call_pos)?));
            }
            let mut result = expect_int(&args[0], call_pos)?;
            for a in &args[1..] {
                result -= expect_int(a, call_pos)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= expect_int(a, call_pos)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("{call_pos}: / requires at least 2 arguments")));
            }
            let mut result = expect_int(&args[0], call_pos)?;
            for a in &args[1..] {
                let d = expect_int(a, call_pos)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero(format!("{call_pos}: division by zero")));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: < requires 2 arguments")));
            }
            Ok(Value::Boolean(expect_int(&args[0], call_pos)? < expect_int(&args[1], call_pos)?))
        }
        ">" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: > requires 2 arguments")));
            }
            Ok(Value::Boolean(expect_int(&args[0], call_pos)? > expect_int(&args[1], call_pos)?))
        }
        "=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: = requires 2 arguments")));
            }
            Ok(Value::Boolean(expect_int(&args[0], call_pos)? == expect_int(&args[1], call_pos)?))
        }
        "<=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: <= requires 2 arguments")));
            }
            Ok(Value::Boolean(expect_int(&args[0], call_pos)? <= expect_int(&args[1], call_pos)?))
        }
        ">=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: >= requires 2 arguments")));
            }
            Ok(Value::Boolean(expect_int(&args[0], call_pos)? >= expect_int(&args[1], call_pos)?))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: not requires 1 argument")));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: cons requires 2 arguments")));
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
                return Err(EvalError::Arity(format!("{call_pos}: car requires 1 argument")));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                _ => Err(EvalError::Type(format!("{call_pos}: car: expected non-empty list"))),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: cdr requires 1 argument")));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
                _ => Err(EvalError::Type(format!("{call_pos}: cdr: expected non-empty list"))),
            }
        }
        "list" => {
            Ok(Value::List(args.to_vec()))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: length requires 1 argument")));
            }
            match &args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(EvalError::Type(format!("{call_pos}: length: expected list"))),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: null? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: number? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: boolean? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: pair? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty())))
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: symbol? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "append" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: append requires 2 arguments")));
            }
            match (&args[0], &args[1]) {
                (Value::List(a), Value::List(b)) => {
                    let mut result = a.clone();
                    result.extend(b.iter().cloned());
                    Ok(Value::List(result))
                }
                _ => Err(EvalError::Type(format!("{call_pos}: append: expected lists"))),
            }
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: display requires 1 argument")));
            }
            let s = args[0].display_output();
            out.borrow_mut().push_str(&s);
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: write requires 1 argument")));
            }
            let s = args[0].display_scheme();
            out.borrow_mut().push_str(&s);
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: newline requires 0 arguments")));
            }
            out.borrow_mut().push('\n');
            Ok(Value::Void)
        }
        "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref"
        | "char?" | "string-copy" => apply_string_builtin(name, args, call_pos),
        "apply" => {
            // (apply proc arg1 ... args-list)
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("{call_pos}: apply requires at least 2 arguments")));
            }
            let func = &args[0];
            let last = &args[args.len() - 1];
            let tail = match last {
                Value::List(items) => items.clone(),
                _ => return Err(EvalError::Type(format!("{call_pos}: apply: last argument must be a list"))),
            };
            let mut final_args: Vec<Value> = args[1..args.len()-1].to_vec();
            final_args.extend(tail);
            apply_func(func, &final_args, call_pos, out)
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

fn env_set_existing(env: &EnvRef, name: &str, val: Value) -> bool {
    let mut frame = env.borrow_mut();
    if frame.bindings.contains_key(name) {
        frame.bindings.insert(name.to_string(), val);
        return true;
    }
    if let Some(parent) = &frame.parent {
        env_set_existing(parent, name, val)
    } else {
        false
    }
}

fn eval_string_set(args: &[Expr], env: &EnvRef, p: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!("{p}: string-set! requires 3 arguments")));
    }
    let var_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(format!("{p}: string-set!: first argument must be a variable"))),
    };
    let idx = eval_expr(&args[1], env, out)?;
    let idx = match idx {
        Value::Integer(n) => n as usize,
        _ => return Err(EvalError::Type(format!("{p}: string-set!: expected integer index"))),
    };
    let ch = eval_expr(&args[2], env, out)?;
    let ch = match ch {
        Value::Char(c) => c,
        _ => return Err(EvalError::Type(format!("{p}: string-set!: expected char"))),
    };
    let s = env_get(env, &var_name)
        .ok_or_else(|| EvalError::UnboundVariable(format!("{p}: {var_name}")))?;
    match s {
        Value::Str(mut string) => {
            if idx >= string.len() {
                return Err(EvalError::Type(format!("{p}: string-set!: index out of range")));
            }
            // SAFETY: replacing ASCII-range byte with a char
            unsafe { string.as_bytes_mut()[idx] = ch as u8; }
            env_set_existing(env, &var_name, Value::Str(string));
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type(format!("{p}: string-set!: expected string"))),
    }
}

fn expect_int(val: &Value, call_pos: Pos) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{call_pos}: expected integer, got {}", val.display_scheme()))),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = new_env(None);
    for name in ["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                  "cons", "car", "cdr", "list", "length", "null?",
                  "number?", "boolean?", "pair?", "string?", "symbol?", "append",
                  "display", "write", "newline",
                  "string-append", "string-length", "substring",
                  "string->number", "number->string",
                  "symbol->string", "string->symbol",
                  "string-ref", "char?", "string-copy", "apply"] {
        env_set(&env, name.to_string(), Value::Symbol(name.to_string()));
    }
    let out = RefCell::new(String::new());
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval_expr(expr, &env, &out)?;
    }
    Ok(result.display_scheme())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = new_env(None);
    for name in ["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                  "cons", "car", "cdr", "list", "length", "null?",
                  "number?", "boolean?", "pair?", "string?", "symbol?", "append",
                  "display", "write", "newline",
                  "string-append", "string-length", "substring",
                  "string->number", "number->string",
                  "symbol->string", "string->symbol",
                  "string-ref", "char?", "string-copy", "apply"] {
        env_set(&env, name.to_string(), Value::Symbol(name.to_string()));
    }
    let out = RefCell::new(String::new());
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval_expr(expr, &env, &out)?;
    }
    let output = out.into_inner();
    Ok((result.display_scheme(), output))
}

#[cfg(test)]
mod tests;
