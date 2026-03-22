pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// ── Span ──

#[derive(Debug, Clone, Copy)]
struct Span {
    line: usize,
    col: usize,
}

impl Span {
    fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }

    fn fmt(&self, msg: &str) -> String {
        format!("{}:{}: {}", self.line, self.col, msg)
    }
}

// ── Value ──

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
}

type Output = Rc<RefCell<std::string::String>>;

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(b) => if *b { "#t".into() } else { "#f".into() },
            Value::Char(c) => format!("#\\{}", c),
            Value::String(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda { .. } => "#<procedure>".into(),
        }
    }

    /// Format for `display` — no quotes on strings, chars as raw chars
    fn display_output(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            _ => self.display(),
        }
    }

    /// Format for `write` — strings get quotes
    fn write_output(&self) -> String {
        self.display()
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// ── Environment ──

type Env = Rc<RefCell<EnvInner>>;

struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl std::fmt::Debug for EnvInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EnvInner")
            .field("bindings", &self.bindings.keys().collect::<Vec<_>>())
            .finish()
    }
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

// ── Parser ──

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

struct Token {
    text: String,
    span: Span,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;
    while i < chars.len() {
        match chars[i] {
            '\n' => { i += 1; line += 1; col = 1; }
            ' ' | '\t' | '\r' => { i += 1; col += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => {
                tokens.push(Token { text: "(".into(), span: Span::new(line, col) });
                i += 1; col += 1;
            }
            ')' => {
                tokens.push(Token { text: ")".into(), span: Span::new(line, col) });
                i += 1; col += 1;
            }
            '\'' => {
                tokens.push(Token { text: "'".into(), span: Span::new(line, col) });
                i += 1; col += 1;
            }
            '"' => {
                let start_col = col;
                let mut s = std::string::String::new();
                s.push('"');
                i += 1; col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        i += 2; col += 2;
                    } else {
                        if chars[i] == '\n' { line += 1; col = 1; } else { col += 1; }
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1; col += 1;
                }
                tokens.push(Token { text: s, span: Span::new(line, start_col) });
            }
            _ => {
                let start_col = col;
                let start = i;
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '\'') {
                    i += 1; col += 1;
                }
                tokens.push(Token {
                    text: chars[start..i].iter().collect(),
                    span: Span::new(line, start_col),
                });
            }
        }
    }
    tokens
}

fn parse_tokens(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let span = tokens[*pos].span;
    let text = &tokens[*pos].text;
    if text == "'" {
        *pos += 1;
        let inner = parse_tokens(tokens, pos)?;
        Ok(Expr {
            kind: ExprKind::List(vec![
                Expr { kind: ExprKind::Symbol("quote".into()), span },
                inner,
            ]),
            span,
        })
    } else if text == "(" {
        *pos += 1;
        let mut list = Vec::new();
        while *pos < tokens.len() && tokens[*pos].text != ")" {
            list.push(parse_tokens(tokens, pos)?);
        }
        if *pos >= tokens.len() {
            return Err(EvalError::Parse(span.fmt("missing closing paren")));
        }
        *pos += 1;
        Ok(Expr { kind: ExprKind::List(list), span })
    } else if text == ")" {
        Err(EvalError::Parse(span.fmt("unexpected )")))
    } else {
        *pos += 1;
        Ok(parse_atom(text, span))
    }
}

fn parse_atom(token: &str, span: Span) -> Expr {
    let kind = if token == "#t" {
        ExprKind::Boolean(true)
    } else if token == "#f" {
        ExprKind::Boolean(false)
    } else if token.starts_with("#\\") {
        let ch_str = &token[2..];
        let ch = match ch_str {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().unwrap(),
            _ => return Expr { kind: ExprKind::Symbol(token.into()), span },
        };
        ExprKind::Char(ch)
    } else if token.starts_with('"') && token.ends_with('"') {
        let inner = &token[1..token.len() - 1];
        ExprKind::String(inner.into())
    } else if let Ok(n) = token.parse::<i64>() {
        ExprKind::Integer(n)
    } else {
        ExprKind::Symbol(token.into())
    };
    Expr { kind, span }
}

fn parse(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_tokens(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ── Evaluator ──

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::String(s) => Value::String(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
    }
}

fn eval(expr: &Expr, env: &Env, out: &Output) -> Result<Value, EvalError> {
    let mut cur_expr = expr.clone();
    let mut cur_env = env.clone();

    loop {
        let span = cur_expr.span;
        let kind = cur_expr.kind.clone();
        match kind {
            ExprKind::Integer(n) => return Ok(Value::Integer(n)),
            ExprKind::Boolean(b) => return Ok(Value::Boolean(b)),
            ExprKind::Char(c) => return Ok(Value::Char(c)),
            ExprKind::String(s) => return Ok(Value::String(s)),
            ExprKind::Symbol(ref name) => {
                return env_get(&cur_env, name)
                    .ok_or_else(|| EvalError::UnboundVariable(span.fmt(name)));
            }
            ExprKind::List(items) => {
                if items.is_empty() {
                    return Err(EvalError::Type(span.fmt("empty application")));
                }
                if let ExprKind::Symbol(ref op) = items[0].kind {
                    match op.as_str() {
                        "define" => return eval_define(&items[1..], &cur_env, span, out),
                        "if" => {
                            let args = &items[1..];
                            if args.len() < 2 || args.len() > 3 {
                                return Err(EvalError::Arity(span.fmt("if requires 2 or 3 arguments")));
                            }
                            let cond = eval(&args[0], &cur_env, out)?;
                            if cond.is_truthy() {
                                cur_expr = args[1].clone();
                                continue;
                            } else if args.len() == 3 {
                                cur_expr = args[2].clone();
                                continue;
                            } else {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        "quote" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity(span.fmt("quote requires 1 argument")));
                            }
                            return Ok(expr_to_value(&items[1]));
                        }
                        "lambda" => return eval_lambda(&items[1..], &cur_env, span),
                        "let" => {
                            let args = &items[1..];
                            if args.len() < 2 {
                                return Err(EvalError::Arity(span.fmt("let requires bindings and body")));
                            }
                            // Named let: (let name ((var init) ...) body ...)
                            if let ExprKind::Symbol(ref name) = args[0].kind {
                                let bindings = match &args[1].kind {
                                    ExprKind::List(b) => b,
                                    _ => return Err(EvalError::Type(span.fmt("named let: expected binding list"))),
                                };
                                let mut params = Vec::new();
                                let mut init_vals = Vec::new();
                                for binding in bindings {
                                    match &binding.kind {
                                        ExprKind::List(pair) if pair.len() == 2 => {
                                            let pname = match &pair[0].kind {
                                                ExprKind::Symbol(s) => s.clone(),
                                                _ => return Err(EvalError::Type(binding.span.fmt("let: expected symbol"))),
                                            };
                                            let val = eval(&pair[1], &cur_env, out)?;
                                            params.push(pname);
                                            init_vals.push(val);
                                        }
                                        _ => return Err(EvalError::Type(binding.span.fmt("let: bad binding"))),
                                    }
                                }
                                let body = args[2..].to_vec();
                                let local_env = new_env(Some(cur_env.clone()));
                                let lambda = Value::Lambda { params: params.clone(), body: body.clone(), env: local_env.clone() };
                                env_set(&local_env, name.clone(), lambda);
                                for (p, v) in params.iter().zip(init_vals.iter()) {
                                    env_set(&local_env, p.clone(), v.clone());
                                }
                                for expr in &body[..body.len() - 1] {
                                    eval(expr, &local_env, out)?;
                                }
                                cur_expr = body.last().unwrap().clone();
                                cur_env = local_env;
                                continue;
                            }
                            // Regular let
                            let bindings = match &args[0].kind {
                                ExprKind::List(b) => b,
                                _ => return Err(EvalError::Type(span.fmt("let: expected binding list"))),
                            };
                            let local_env = new_env(Some(cur_env.clone()));
                            for binding in bindings {
                                match &binding.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let name = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(EvalError::Type(binding.span.fmt("let: expected symbol"))),
                                        };
                                        let val = eval(&pair[1], &cur_env, out)?;
                                        env_set(&local_env, name, val);
                                    }
                                    _ => return Err(EvalError::Type(binding.span.fmt("let: bad binding"))),
                                }
                            }
                            let body = &args[1..];
                            for expr in &body[..body.len() - 1] {
                                eval(expr, &local_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            cur_env = local_env;
                            continue;
                        }
                        "begin" => {
                            let body = &items[1..];
                            if body.is_empty() {
                                return Ok(Value::Boolean(false));
                            }
                            for item in &body[..body.len() - 1] {
                                eval(item, &cur_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            continue;
                        }
                        "cond" => {
                            let clauses = &items[1..];
                            let mut found = false;
                            for clause in clauses {
                                match &clause.kind {
                                    ExprKind::List(parts) if parts.len() >= 2 => {
                                        let is_else = matches!(&parts[0].kind, ExprKind::Symbol(s) if s == "else");
                                        if is_else || eval(&parts[0], &cur_env, out)?.is_truthy() {
                                            for expr in &parts[1..parts.len() - 1] {
                                                eval(expr, &cur_env, out)?;
                                            }
                                            cur_expr = parts.last().unwrap().clone();
                                            found = true;
                                            break;
                                        }
                                    }
                                    _ => return Err(EvalError::Type(clause.span.fmt("cond: bad clause"))),
                                }
                            }
                            if found {
                                continue;
                            }
                            return Ok(Value::Boolean(false));
                        }
                        "and" => {
                            let args = &items[1..];
                            if args.is_empty() {
                                return Ok(Value::Boolean(true));
                            }
                            for arg in &args[..args.len() - 1] {
                                let val = eval(arg, &cur_env, out)?;
                                if !val.is_truthy() {
                                    return Ok(val);
                                }
                            }
                            cur_expr = args.last().unwrap().clone();
                            continue;
                        }
                        "or" => {
                            let args = &items[1..];
                            if args.is_empty() {
                                return Ok(Value::Boolean(false));
                            }
                            for arg in &args[..args.len() - 1] {
                                let val = eval(arg, &cur_env, out)?;
                                if val.is_truthy() {
                                    return Ok(val);
                                }
                            }
                            cur_expr = args.last().unwrap().clone();
                            continue;
                        }
                        "set!" => return eval_set(&items[1..], &cur_env, span, out),
                        "string-set!" => return eval_string_set(&items[1..], &cur_env, span, out),
                        "display" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity(span.fmt("display requires 1 argument")));
                            }
                            let val = eval(&items[1], &cur_env, out)?;
                            out.borrow_mut().push_str(&val.display_output());
                            return Ok(Value::Boolean(false));
                        }
                        "write" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity(span.fmt("write requires 1 argument")));
                            }
                            let val = eval(&items[1], &cur_env, out)?;
                            out.borrow_mut().push_str(&val.write_output());
                            return Ok(Value::Boolean(false));
                        }
                        "newline" => {
                            if items.len() != 1 {
                                return Err(EvalError::Arity(span.fmt("newline takes 0 arguments")));
                            }
                            out.borrow_mut().push('\n');
                            return Ok(Value::Boolean(false));
                        }
                        "cons" | "car" | "cdr" | "null?" | "list" | "length"
                        | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
                        | "string-append" | "string-length" | "substring"
                        | "string->number" | "number->string"
                        | "symbol->string" | "string->symbol"
                        | "string-ref" | "string-copy"
                        | "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not" => {
                            let args: Vec<Value> = items[1..].iter().map(|a| eval(a, &cur_env, out)).collect::<Result<_, _>>()?;
                            return eval_builtin(op, &args, span);
                        }
                        _ => {}
                    }
                }
                // Function application with TCO
                let func = eval(&items[0], &cur_env, out)?;
                let args: Vec<Value> = items[1..].iter().map(|a| eval(a, &cur_env, out)).collect::<Result<_, _>>()?;
                match func {
                    Value::Lambda { params, body, env: closure_env } => {
                        if args.len() != params.len() {
                            return Err(EvalError::Arity(span.fmt(&format!(
                                "expected {} arguments, got {}", params.len(), args.len()
                            ))));
                        }
                        let local_env = new_env(Some(closure_env));
                        for (p, a) in params.iter().zip(args.iter()) {
                            env_set(&local_env, p.clone(), a.clone());
                        }
                        for expr in &body[..body.len() - 1] {
                            eval(expr, &local_env, out)?;
                        }
                        cur_expr = body.last().unwrap().clone();
                        cur_env = local_env;
                        continue;
                    }
                    _ => return Err(EvalError::Type(span.fmt("not a procedure"))),
                }
            }
        }
    }
}

fn eval_define(args: &[Expr], env: &Env, span: Span, out: &Output) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(span.fmt("define requires arguments")));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(span.fmt("define requires a value")));
            }
            let val = eval(&args[1], env, out)?;
            env_set(env, name.clone(), val);
            Ok(Value::Boolean(false))
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Arity(span.fmt("define: empty signature")));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(n) => n.clone(),
                _ => return Err(EvalError::Type(span.fmt("define: expected symbol"))),
            };
            let params: Vec<String> = sig[1..].iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type(e.span.fmt("define: expected parameter name"))),
            }).collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda { params, body, env: env.clone() };
            env_set(env, name, lambda);
            Ok(Value::Boolean(false))
        }
        _ => Err(EvalError::Type(span.fmt("define: expected symbol or list"))),
    }
}


fn eval_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(span.fmt("lambda requires params and body")));
    }
    let params = match &args[0].kind {
        ExprKind::List(param_exprs) => {
            param_exprs.iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type(e.span.fmt("lambda: expected parameter name"))),
            }).collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Type(span.fmt("lambda: expected parameter list"))),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda { params, body, env: env.clone() })
}


fn eval_set(args: &[Expr], env: &Env, span: Span, out: &Output) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(span.fmt("set! requires 2 arguments")));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(span.fmt("set!: expected symbol"))),
    };
    let val = eval(&args[1], env, out)?;
    fn env_update(env: &Env, name: &str, val: Value) -> Result<(), ()> {
        let mut inner = env.borrow_mut();
        if inner.bindings.contains_key(name) {
            inner.bindings.insert(name.to_string(), val);
            Ok(())
        } else if let Some(ref parent) = inner.parent {
            env_update(parent, name, val)
        } else {
            Err(())
        }
    }
    env_update(env, &name, val).map_err(|_| EvalError::UnboundVariable(span.fmt(&name)))?;
    Ok(Value::Boolean(false))
}

fn eval_string_set(args: &[Expr], env: &Env, span: Span, out: &Output) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(span.fmt("string-set! requires 3 arguments")));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(span.fmt("string-set!: expected symbol"))),
    };
    let idx = match eval(&args[1], env, out)? {
        Value::Integer(n) => n as usize,
        _ => return Err(EvalError::Type(span.fmt("string-set!: expected integer index"))),
    };
    let ch = match eval(&args[2], env, out)? {
        Value::Char(c) => c,
        _ => return Err(EvalError::Type(span.fmt("string-set!: expected char"))),
    };
    // Look up the string, mutate it, write it back
    let current = env_get(env, &name).ok_or_else(|| EvalError::UnboundVariable(span.fmt(&name)))?;
    match current {
        Value::String(mut s) => {
            let mut chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type(span.fmt("string-set!: index out of bounds")));
            }
            chars[idx] = ch;
            s = chars.into_iter().collect();
            // Update in env
            fn env_update(env: &Env, name: &str, val: Value) -> Result<(), ()> {
                let mut inner = env.borrow_mut();
                if inner.bindings.contains_key(name) {
                    inner.bindings.insert(name.to_string(), val);
                    Ok(())
                } else if let Some(ref parent) = inner.parent {
                    env_update(parent, name, val)
                } else {
                    Err(())
                }
            }
            env_update(env, &name, Value::String(s))
                .map_err(|_| EvalError::UnboundVariable(span.fmt(&name)))?;
            Ok(Value::Boolean(false))
        }
        _ => Err(EvalError::Type(span.fmt("string-set!: expected string"))),
    }
}


fn as_int(v: &Value, span: Span) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(span.fmt("expected integer"))),
    }
}

fn eval_builtin(op: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match op {
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(span.fmt("cons requires 2 arguments")));
            }
            match &args[1] {
                Value::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => Err(EvalError::Type(span.fmt("cons: second argument must be a list"))),
            }
        }
        "car" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("car requires 1 argument"))); }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                _ => Err(EvalError::Type(span.fmt("car: expected non-empty list"))),
            }
        }
        "cdr" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("cdr requires 1 argument"))); }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
                _ => Err(EvalError::Type(span.fmt("cdr: expected non-empty list"))),
            }
        }
        "null?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("null? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("length requires 1 argument"))); }
            match &args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(EvalError::Type(span.fmt("length: expected list"))),
            }
        }
        "string?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("string? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::String(_))))
        }
        "number?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("number? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("boolean? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("pair? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("symbol? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "+" => {
            let mut sum: i64 = 0;
            for a in args { sum += as_int(a, span)?; }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(span.fmt("- requires at least 1 argument")));
            }
            if args.len() == 1 {
                Ok(Value::Integer(-as_int(&args[0], span)?))
            } else {
                let mut result = as_int(&args[0], span)?;
                for a in &args[1..] { result -= as_int(a, span)?; }
                Ok(Value::Integer(result))
            }
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args { product *= as_int(a, span)?; }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(span.fmt("/ requires at least 2 arguments")));
            }
            let mut result = as_int(&args[0], span)?;
            for a in &args[1..] {
                let d = as_int(a, span)?;
                if d == 0 { return Err(EvalError::Type(span.fmt("division by zero"))); }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => cmp_vals(args, |a, b| a < b, span),
        ">" => cmp_vals(args, |a, b| a > b, span),
        "=" => cmp_vals(args, |a, b| a == b, span),
        "<=" => cmp_vals(args, |a, b| a <= b, span),
        ">=" => cmp_vals(args, |a, b| a >= b, span),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(span.fmt("not requires 1 argument")));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "char?" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("char? requires 1 argument"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "string-append" => {
            let mut result = std::string::String::new();
            for a in args {
                match a {
                    Value::String(s) => result.push_str(s),
                    _ => return Err(EvalError::Type(span.fmt("string-append: expected string"))),
                }
            }
            Ok(Value::String(result))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("string-length requires 1 argument"))); }
            match &args[0] {
                Value::String(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type(span.fmt("string-length: expected string"))),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity(span.fmt("substring requires 3 arguments"))); }
            let s = match &args[0] {
                Value::String(s) => s,
                _ => return Err(EvalError::Type(span.fmt("substring: expected string"))),
            };
            let start = as_int(&args[1], span)? as usize;
            let end = as_int(&args[2], span)? as usize;
            Ok(Value::String(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("string->number requires 1 argument"))); }
            match &args[0] {
                Value::String(s) => {
                    match s.parse::<i64>() {
                        Ok(n) => Ok(Value::Integer(n)),
                        Err(_) => Ok(Value::Boolean(false)),
                    }
                }
                _ => Err(EvalError::Type(span.fmt("string->number: expected string"))),
            }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("number->string requires 1 argument"))); }
            let n = as_int(&args[0], span)?;
            Ok(Value::String(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("symbol->string requires 1 argument"))); }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::String(s.clone())),
                _ => Err(EvalError::Type(span.fmt("symbol->string: expected symbol"))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("string->symbol requires 1 argument"))); }
            match &args[0] {
                Value::String(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type(span.fmt("string->symbol: expected string"))),
            }
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity(span.fmt("string-copy requires 1 argument"))); }
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.clone())),
                _ => Err(EvalError::Type(span.fmt("string-copy: expected string"))),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity(span.fmt("string-ref requires 2 arguments"))); }
            let s = match &args[0] {
                Value::String(s) => s,
                _ => return Err(EvalError::Type(span.fmt("string-ref: expected string"))),
            };
            let idx = as_int(&args[1], span)? as usize;
            Ok(Value::Char(s.chars().nth(idx).ok_or_else(|| EvalError::Type(span.fmt("string-ref: index out of bounds")))?))
        }
        _ => Err(EvalError::UnboundVariable(span.fmt(op))),
    }
}

fn cmp_vals(args: &[Value], f: fn(i64, i64) -> bool, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(span.fmt("comparison requires at least 2 arguments")));
    }
    let mut prev = as_int(&args[0], span)?;
    for a in &args[1..] {
        let curr = as_int(a, span)?;
        if !f(prev, curr) { return Ok(Value::Boolean(false)); }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse(input)?;
    let env = new_env(None);
    let out: Output = Rc::new(RefCell::new(std::string::String::new()));
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &env, &out)?;
    }
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse(input)?;
    let env = new_env(None);
    let out: Output = Rc::new(RefCell::new(std::string::String::new()));
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &env, &out)?;
    }
    let output = out.borrow().clone();
    Ok((result.display(), output))
}

#[cfg(test)]
mod tests;
