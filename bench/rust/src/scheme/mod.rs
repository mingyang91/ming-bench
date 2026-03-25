pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

// ── Source positions ──

#[derive(Debug, Clone, Copy)]
struct Span {
    line: usize,
    col: usize,
}

impl Span {
    fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// ── Values ──

#[derive(Debug, Clone)]
enum Val {
    Int(i64),
    Bool(bool),
    Char(char),
    Str(String),
    Symbol(String),
    Pair(Box<Val>, Box<Val>),
    Nil,
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
    Void,
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{n}"),
            Val::Bool(true) => write!(f, "#t"),
            Val::Bool(false) => write!(f, "#f"),
            Val::Char(c) => write!(f, "#\\{c}"),
            Val::Str(s) => write!(f, "\"{}\"", s),
            Val::Symbol(s) => write!(f, "{s}"),
            Val::Nil => write!(f, "()"),
            Val::Pair(_, _) => {
                write!(f, "(")?;
                let mut cur = self;
                let mut first = true;
                loop {
                    match cur {
                        Val::Pair(car, cdr) => {
                            if !first { write!(f, " ")?; }
                            first = false;
                            write!(f, "{car}")?;
                            cur = cdr;
                        }
                        Val::Nil => break,
                        other => {
                            write!(f, " . {other}")?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Val::Lambda { .. } => write!(f, "#<procedure>"),
            Val::Builtin(_) => write!(f, "#<procedure>"),
            Val::Void => write!(f, "#<void>"),
        }
    }
}

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Int(i64),
    Bool(bool),
    Char(char),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

// ── Tokenizer ──

#[derive(Debug, Clone)]
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
            '\n' => { line += 1; col = 1; i += 1; }
            ' ' | '\t' | '\r' => { col += 1; i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => { tokens.push(Token { text: "(".into(), span: Span::new(line, col) }); i += 1; col += 1; }
            ')' => { tokens.push(Token { text: ")".into(), span: Span::new(line, col) }); i += 1; col += 1; }
            '\'' => { tokens.push(Token { text: "'".into(), span: Span::new(line, col) }); i += 1; col += 1; }
            '#' => {
                let start_col = col;
                if i + 1 < chars.len() && (chars[i + 1] == 't' || chars[i + 1] == 'f') {
                    let tok: String = chars[i..i+2].iter().collect();
                    tokens.push(Token { text: tok, span: Span::new(line, start_col) });
                    i += 2;
                    col += 2;
                } else {
                    let mut tok = String::new();
                    while i < chars.len() && !chars[i].is_whitespace() && chars[i] != '(' && chars[i] != ')' {
                        tok.push(chars[i]);
                        i += 1;
                        col += 1;
                    }
                    tokens.push(Token { text: tok, span: Span::new(line, start_col) });
                }
            }
            '"' => {
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
                        if chars[i] == '\n' { line += 1; col = 1; } else { col += 1; }
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: s, span: Span::new(line, start_col) });
            }
            _ => {
                let start_col = col;
                let mut tok = String::new();
                while i < chars.len() && !chars[i].is_whitespace() && chars[i] != '(' && chars[i] != ')' {
                    tok.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: tok, span: Span::new(line, start_col) });
            }
        }
    }
    tokens
}

// ── Parser ──

fn parse(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let tok = &tokens[*pos];
    let span = tok.span;
    if tok.text == "'" {
        *pos += 1;
        let inner = parse(tokens, pos)?;
        return Ok(Expr { kind: ExprKind::List(vec![
            Expr { kind: ExprKind::Symbol("quote".into()), span },
            inner,
        ]), span });
    }
    if tok.text == "(" {
        *pos += 1;
        let mut list = Vec::new();
        while *pos < tokens.len() && tokens[*pos].text != ")" {
            list.push(parse(tokens, pos)?);
        }
        if *pos >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".into()));
        }
        *pos += 1; // skip ')'
        Ok(Expr { kind: ExprKind::List(list), span })
    } else if tok.text == ")" {
        Err(EvalError::Parse("unexpected ')'".into()))
    } else if tok.text == "#t" {
        *pos += 1;
        Ok(Expr { kind: ExprKind::Bool(true), span })
    } else if tok.text == "#f" {
        *pos += 1;
        Ok(Expr { kind: ExprKind::Bool(false), span })
    } else if tok.text.starts_with('"') {
        *pos += 1;
        let inner = &tok.text[1..tok.text.len()-1];
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
                    c => { s.push('\\'); s.push(c); }
                }
                j += 2;
            } else {
                s.push(cs[j]);
                j += 1;
            }
        }
        Ok(Expr { kind: ExprKind::Str(s), span })
    } else if tok.text.starts_with("#\\") {
        *pos += 1;
        let rest = &tok.text[2..];
        let ch = match rest {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.chars().count() == 1 => s.chars().next().unwrap(),
            _ => return Err(EvalError::Parse(format!("invalid character literal: {}", tok.text))),
        };
        Ok(Expr { kind: ExprKind::Char(ch), span })
    } else if let Ok(n) = tok.text.parse::<i64>() {
        *pos += 1;
        Ok(Expr { kind: ExprKind::Int(n), span })
    } else {
        *pos += 1;
        Ok(Expr { kind: ExprKind::Symbol(tok.text.clone()), span })
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ── Environment ──

#[derive(Debug, Clone)]
struct EnvInner {
    bindings: HashMap<String, Val>,
    parent: Option<Env>,
}

type Env = Rc<RefCell<EnvInner>>;

fn new_env(parent: Option<Env>) -> Env {
    Rc::new(RefCell::new(EnvInner {
        bindings: HashMap::new(),
        parent,
    }))
}

fn env_get(env: &Env, name: &str) -> Option<Val> {
    let inner = env.borrow();
    if let Some(val) = inner.bindings.get(name) {
        Some(val.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        None
    }
}

fn env_set(env: &Env, name: String, val: Val) {
    env.borrow_mut().bindings.insert(name, val);
}

/// Mutate an existing binding in the environment chain. Returns false if not found.
fn env_set_existing(env: &Env, name: &str, val: Val) -> bool {
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

/// Format a value using `display` semantics (no quotes on strings).
fn display_val(v: &Val, f: &mut String) {
    match v {
        Val::Str(s) => f.push_str(s),
        Val::Char(c) => f.push(*c),
        Val::Pair(_, _) => {
            f.push('(');
            let mut cur: &Val = v;
            let mut first = true;
            loop {
                match cur {
                    Val::Pair(car, cdr) => {
                        if !first { f.push(' '); }
                        first = false;
                        display_val(car, f);
                        cur = cdr;
                    }
                    Val::Nil => break,
                    other => {
                        f.push_str(" . ");
                        display_val(other, f);
                        break;
                    }
                }
            }
            f.push(')');
        }
        other => f.push_str(&other.to_string()),
    }
}

// ── Evaluator ──

fn is_truthy(v: &Val) -> bool {
    !matches!(v, Val::Bool(false))
}

fn as_int(v: &Val, ctx: &str) -> Result<i64, EvalError> {
    match v {
        Val::Int(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{ctx}: expected integer, got {v}"))),
    }
}

fn quote_expr(expr: &Expr) -> Val {
    match &expr.kind {
        ExprKind::Int(n) => Val::Int(*n),
        ExprKind::Bool(b) => Val::Bool(*b),
        ExprKind::Char(c) => Val::Char(*c),
        ExprKind::Str(s) => Val::Str(s.clone()),
        ExprKind::Symbol(s) => Val::Symbol(s.clone()),
        ExprKind::List(items) => {
            let mut result = Val::Nil;
            for item in items.iter().rev() {
                result = Val::Pair(Box::new(quote_expr(item)), Box::new(result));
            }
            result
        }
    }
}

/// Create an error with position info
fn err_at(span: Span, err: EvalError) -> EvalError {
    match err {
        EvalError::Parse(msg) => EvalError::Parse(format!("{msg} at {span}")),
        EvalError::UnboundVariable(name) => EvalError::UnboundVariable(format!("{name} at {span}")),
        EvalError::Type(msg) => EvalError::Type(format!("{msg} at {span}")),
        EvalError::Arity(msg) => EvalError::Arity(format!("{msg} at {span}")),
    }
}

fn eval(expr: &Expr, env: &Env, out: &mut String) -> Result<Val, EvalError> {
    let span = expr.span;
    match &expr.kind {
        ExprKind::Int(n) => Ok(Val::Int(*n)),
        ExprKind::Bool(b) => Ok(Val::Bool(*b)),
        ExprKind::Char(c) => Ok(Val::Char(*c)),
        ExprKind::Str(s) => Ok(Val::Str(s.clone())),
        ExprKind::Symbol(name) => {
            env_get(env, name)
                .or_else(|| if is_builtin(name) { Some(Val::Builtin(name.clone())) } else { None })
                .ok_or_else(|| err_at(span, EvalError::UnboundVariable(name.clone())))
        }
        ExprKind::List(list) => {
            if list.is_empty() {
                return Err(err_at(span, EvalError::Parse("empty application".into())));
            }
            // Check for special forms
            if let ExprKind::Symbol(op) = &list[0].kind {
                match op.as_str() {
                    "define" => {
                        if list.len() < 3 {
                            return Err(err_at(span, EvalError::Parse("define: bad syntax".into())));
                        }
                        match &list[1].kind {
                            // (define x expr)
                            ExprKind::Symbol(name) => {
                                let val = eval(&list[2], env, out)?;
                                env_set(env, name.clone(), val);
                                return Ok(Val::Void);
                            }
                            // (define (f params...) body...)
                            ExprKind::List(sig) => {
                                if sig.is_empty() {
                                    return Err(err_at(span, EvalError::Parse("define: empty signature".into())));
                                }
                                let name = match &sig[0].kind {
                                    ExprKind::Symbol(n) => n.clone(),
                                    _ => return Err(err_at(span, EvalError::Parse("define: expected symbol".into()))),
                                };
                                let (params, rest_param) = parse_params(&sig[1..], span)?;
                                let body = list[2..].to_vec();
                                let lambda = Val::Lambda {
                                    params,
                                    rest_param,
                                    body,
                                    env: env.clone(),
                                };
                                env_set(env, name, lambda);
                                return Ok(Val::Void);
                            }
                            _ => return Err(err_at(span, EvalError::Parse("define: bad syntax".into()))),
                        }
                    }
                    "if" => {
                        if list.len() < 3 || list.len() > 4 {
                            return Err(err_at(span, EvalError::Parse("if: bad syntax".into())));
                        }
                        let cond = eval(&list[1], env, out)?;
                        if is_truthy(&cond) {
                            return eval(&list[2], env, out);
                        } else if list.len() == 4 {
                            return eval(&list[3], env, out);
                        } else {
                            return Ok(Val::Void);
                        }
                    }
                    "quote" => {
                        if list.len() != 2 {
                            return Err(err_at(span, EvalError::Parse("quote: need exactly 1 argument".into())));
                        }
                        return Ok(quote_expr(&list[1]));
                    }
                    "lambda" => {
                        if list.len() < 3 {
                            return Err(err_at(span, EvalError::Parse("lambda: bad syntax".into())));
                        }
                        let (params, rest_param) = match &list[1].kind {
                            ExprKind::List(param_list) => parse_params(param_list, span)?,
                            ExprKind::Symbol(s) => {
                                // (lambda rest body) - single rest param
                                (vec![], Some(s.clone()))
                            }
                            _ => return Err(err_at(span, EvalError::Parse("lambda: expected parameter list".into()))),
                        };
                        let body = list[2..].to_vec();
                        return Ok(Val::Lambda {
                            params,
                            rest_param,
                            body,
                            env: env.clone(),
                        });
                    }
                    "let" => {
                        if list.len() < 3 {
                            return Err(err_at(span, EvalError::Parse("let: bad syntax".into())));
                        }
                        // Named let: (let name ((var init) ...) body ...)
                        if let ExprKind::Symbol(name) = &list[1].kind {
                            if list.len() < 4 {
                                return Err(err_at(span, EvalError::Parse("let: bad syntax".into())));
                            }
                            let bindings = match &list[2].kind {
                                ExprKind::List(b) => b,
                                _ => return Err(err_at(span, EvalError::Parse("let: expected bindings list".into()))),
                            };
                            let mut params = Vec::new();
                            let mut init_vals = Vec::new();
                            for binding in bindings {
                                match &binding.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let pname = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(err_at(span, EvalError::Parse("let: expected symbol".into()))),
                                        };
                                        let val = eval(&pair[1], env, out)?;
                                        params.push(pname);
                                        init_vals.push(val);
                                    }
                                    _ => return Err(err_at(span, EvalError::Parse("let: bad binding".into()))),
                                }
                            }
                            let let_env = new_env(Some(env.clone()));
                            let body = list[3..].to_vec();
                            let lambda = Val::Lambda {
                                params: params.clone(),
                                rest_param: None,
                                body,
                                env: let_env.clone(),
                            };
                            env_set(&let_env, name.clone(), lambda);
                            let call_env = new_env(Some(let_env.clone()));
                            for (p, v) in params.iter().zip(init_vals) {
                                env_set(&call_env, p.clone(), v);
                            }
                            let mut result = Val::Void;
                            for expr in &list[3..] {
                                result = eval(expr, &call_env, out)?;
                            }
                            return Ok(result);
                        }
                        let bindings = match &list[1].kind {
                            ExprKind::List(b) => b,
                            _ => return Err(err_at(span, EvalError::Parse("let: expected bindings list".into()))),
                        };
                        let let_env = new_env(Some(env.clone()));
                        for binding in bindings {
                            match &binding.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    let name = match &pair[0].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(err_at(span, EvalError::Parse("let: expected symbol".into()))),
                                    };
                                    let val = eval(&pair[1], env, out)?;
                                    env_set(&let_env, name, val);
                                }
                                _ => return Err(err_at(span, EvalError::Parse("let: bad binding".into()))),
                            }
                        }
                        let mut result = Val::Void;
                        for expr in &list[2..] {
                            result = eval(expr, &let_env, out)?;
                        }
                        return Ok(result);
                    }
                    "set!" => {
                        if list.len() != 3 {
                            return Err(err_at(span, EvalError::Parse("set!: bad syntax".into())));
                        }
                        let name = match &list[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(err_at(span, EvalError::Parse("set!: expected symbol".into()))),
                        };
                        let val = eval(&list[2], env, out)?;
                        if !env_set_existing(env, &name, val) {
                            return Err(err_at(span, EvalError::UnboundVariable(name)));
                        }
                        return Ok(Val::Void);
                    }
                    "begin" => {
                        let mut result = Val::Void;
                        for expr in &list[1..] {
                            result = eval(expr, env, out)?;
                        }
                        return Ok(result);
                    }
                    "cond" => {
                        for clause in &list[1..] {
                            match &clause.kind {
                                ExprKind::List(parts) if !parts.is_empty() => {
                                    if let ExprKind::Symbol(s) = &parts[0].kind {
                                        if s == "else" {
                                            let mut result = Val::Void;
                                            for expr in &parts[1..] {
                                                result = eval(expr, env, out)?;
                                            }
                                            return Ok(result);
                                        }
                                    }
                                    let cond_val = eval(&parts[0], env, out)?;
                                    if is_truthy(&cond_val) {
                                        let mut result = cond_val;
                                        for expr in &parts[1..] {
                                            result = eval(expr, env, out)?;
                                        }
                                        return Ok(result);
                                    }
                                }
                                _ => return Err(err_at(span, EvalError::Parse("cond: bad clause".into()))),
                            }
                        }
                        return Ok(Val::Void);
                    }
                    "and" => {
                        let mut result = Val::Bool(true);
                        for arg in &list[1..] {
                            result = eval(arg, env, out)?;
                            if !is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        let mut result = Val::Bool(false);
                        for arg in &list[1..] {
                            result = eval(arg, env, out)?;
                            if is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    _ => {}
                }
            }

            // Output builtins need access to `out`
            if let ExprKind::Symbol(op) = &list[0].kind {
                match op.as_str() {
                    "display" => {
                        if list.len() != 2 {
                            return Err(err_at(span, EvalError::Arity("display: need 1 argument".into())));
                        }
                        let val = eval(&list[1], env, out)?;
                        display_val(&val, out);
                        return Ok(Val::Void);
                    }
                    "write" => {
                        if list.len() != 2 {
                            return Err(err_at(span, EvalError::Arity("write: need 1 argument".into())));
                        }
                        let val = eval(&list[1], env, out)?;
                        out.push_str(&val.to_string());
                        return Ok(Val::Void);
                    }
                    "newline" => {
                        if list.len() != 1 {
                            return Err(err_at(span, EvalError::Arity("newline: need 0 arguments".into())));
                        }
                        out.push('\n');
                        return Ok(Val::Void);
                    }
                    "string-set!" => {
                        if list.len() != 4 {
                            return Err(err_at(span, EvalError::Arity("string-set!: need 3 arguments".into())));
                        }
                        let var_name = match &list[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(err_at(span, EvalError::Type("string-set!: first argument must be a variable".into()))),
                        };
                        let idx_val = eval(&list[2], env, out)?;
                        let idx = match &idx_val {
                            Val::Int(n) => *n as usize,
                            _ => return Err(err_at(span, EvalError::Type("string-set!: expected integer index".into()))),
                        };
                        let char_val = eval(&list[3], env, out)?;
                        let ch = match &char_val {
                            Val::Char(c) => *c,
                            _ => return Err(err_at(span, EvalError::Type("string-set!: expected char".into()))),
                        };
                        let mut s = match env_get(env, &var_name) {
                            Some(Val::Str(s)) => s,
                            Some(_) => return Err(err_at(span, EvalError::Type("string-set!: expected string".into()))),
                            None => return Err(err_at(span, EvalError::UnboundVariable(var_name.clone()))),
                        };
                        let mut chars: Vec<char> = s.chars().collect();
                        if idx >= chars.len() {
                            return Err(err_at(span, EvalError::Type("string-set!: index out of range".into())));
                        }
                        chars[idx] = ch;
                        s = chars.into_iter().collect();
                        env_set(env, var_name, Val::Str(s));
                        return Ok(Val::Void);
                    }
                    _ => {}
                }
            }

            // Function application: handle apply specially (needs out), then builtins, then lambdas
            if let ExprKind::Symbol(op) = &list[0].kind {
                if op == "apply" {
                    let mut args = Vec::new();
                    for a in &list[1..] {
                        args.push(eval(a, env, out)?);
                    }
                    return do_apply(&args, span, out);
                }
                if is_builtin(op) {
                    let mut args = Vec::new();
                    for a in &list[1..] {
                        args.push(eval(a, env, out)?);
                    }
                    return apply_builtin(op, &args).map_err(|e| err_at(span, e));
                }
            }

            let func = eval(&list[0], env, out)?;
            let mut args = Vec::new();
            for a in &list[1..] {
                args.push(eval(a, env, out)?);
            }

            call_function(&func, args, span, out)
        }
    }
}

fn is_builtin(op: &str) -> bool {
    matches!(op, "+" | "-" | "*" | "/" | "=" | "<" | ">" | "<=" | ">=" | "not"
        | "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
        | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
        | "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref"
        | "string-copy" | "apply")
}

/// Parse a parameter list that may contain dot notation for rest params.
/// Returns (fixed_params, rest_param).
fn parse_params(exprs: &[Expr], span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest = None;
    let mut i = 0;
    while i < exprs.len() {
        match &exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 != exprs.len() - 1 {
                    return Err(err_at(span, EvalError::Parse("bad dot in parameter list".into())));
                }
                match &exprs[i + 1].kind {
                    ExprKind::Symbol(r) => rest = Some(r.clone()),
                    _ => return Err(err_at(span, EvalError::Parse("expected symbol after dot".into()))),
                }
                break;
            }
            ExprKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(err_at(span, EvalError::Parse("expected parameter name".into()))),
        }
        i += 1;
    }
    Ok((params, rest))
}

/// Convert a Val list to a Vec<Val>.
fn val_list_to_vec(v: &Val) -> Result<Vec<Val>, EvalError> {
    let mut result = Vec::new();
    let mut cur = v;
    loop {
        match cur {
            Val::Nil => break,
            Val::Pair(car, cdr) => {
                result.push(*car.clone());
                cur = cdr;
            }
            _ => return Err(EvalError::Type("apply: last argument must be a list".into())),
        }
    }
    Ok(result)
}

/// Call a function value with given args.
fn call_function(func: &Val, args: Vec<Val>, span: Span, out: &mut String) -> Result<Val, EvalError> {
    match func {
        Val::Lambda { params, rest_param, body, env: closure_env } => {
            if let Some(ref rp) = rest_param {
                if args.len() < params.len() {
                    return Err(err_at(span, EvalError::Arity(format!(
                        "expected at least {} arguments, got {}", params.len(), args.len()
                    ))));
                }
            } else if args.len() != params.len() {
                return Err(err_at(span, EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                ))));
            }
            let call_env = new_env(Some(closure_env.clone()));
            for (p, a) in params.iter().zip(args.iter()) {
                env_set(&call_env, p.clone(), a.clone());
            }
            if let Some(ref rp) = rest_param {
                let rest_args = &args[params.len()..];
                let mut rest_list = Val::Nil;
                for a in rest_args.iter().rev() {
                    rest_list = Val::Pair(Box::new(a.clone()), Box::new(rest_list));
                }
                env_set(&call_env, rp.clone(), rest_list);
            }
            let mut result = Val::Void;
            for expr in body {
                result = eval(expr, &call_env, out)?;
            }
            Ok(result)
        }
        Val::Builtin(name) => {
            if name == "apply" {
                do_apply(&args, span, out)
            } else {
                apply_builtin(name, &args).map_err(|e| err_at(span, e))
            }
        }
        _ => Err(err_at(span, EvalError::Type("not a procedure".into()))),
    }
}

/// Implement apply: (apply fn arg1 ... args-list)
fn do_apply(args: &[Val], span: Span, out: &mut String) -> Result<Val, EvalError> {
    if args.len() < 2 {
        return Err(err_at(span, EvalError::Arity("apply: need at least 2 arguments".into())));
    }
    let func = &args[0];
    let last = &args[args.len() - 1];
    let mut call_args: Vec<Val> = args[1..args.len()-1].to_vec();
    let tail = val_list_to_vec(last)?;
    call_args.extend(tail);
    call_function(func, call_args, span, out)
}

fn apply_builtin(op: &str, args: &[Val]) -> Result<Val, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_int(a, "+")?;
            }
            Ok(Val::Int(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("-: need at least 1 argument".into()));
            }
            if args.len() == 1 {
                Ok(Val::Int(-as_int(&args[0], "-")?))
            } else {
                let mut result = as_int(&args[0], "-")?;
                for a in &args[1..] {
                    result -= as_int(a, "-")?;
                }
                Ok(Val::Int(result))
            }
        }
        "*" => {
            let mut prod: i64 = 1;
            for a in args {
                prod *= as_int(a, "*")?;
            }
            Ok(Val::Int(prod))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("/: need at least 2 arguments".into()));
            }
            let mut result = as_int(&args[0], "/")?;
            for a in &args[1..] {
                let d = as_int(a, "/")?;
                if d == 0 {
                    return Err(EvalError::Type("division by zero".into()));
                }
                result /= d;
            }
            Ok(Val::Int(result))
        }
        "=" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("=: need at least 2 arguments".into()));
            }
            let first = as_int(&args[0], "=")?;
            Ok(Val::Bool(args[1..].iter().all(|a| as_int(a, "=").map_or(false, |n| n == first))))
        }
        "<" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("<: need at least 2 arguments".into()));
            }
            let vals: Result<Vec<i64>, _> = args.iter().map(|a| as_int(a, "<")).collect();
            let vals = vals?;
            Ok(Val::Bool(vals.windows(2).all(|w| w[0] < w[1])))
        }
        ">" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(">: need at least 2 arguments".into()));
            }
            let vals: Result<Vec<i64>, _> = args.iter().map(|a| as_int(a, ">")).collect();
            let vals = vals?;
            Ok(Val::Bool(vals.windows(2).all(|w| w[0] > w[1])))
        }
        "<=" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("<=: need at least 2 arguments".into()));
            }
            let vals: Result<Vec<i64>, _> = args.iter().map(|a| as_int(a, "<=")).collect();
            let vals = vals?;
            Ok(Val::Bool(vals.windows(2).all(|w| w[0] <= w[1])))
        }
        ">=" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(">=: need at least 2 arguments".into()));
            }
            let vals: Result<Vec<i64>, _> = args.iter().map(|a| as_int(a, ">=")).collect();
            let vals = vals?;
            Ok(Val::Bool(vals.windows(2).all(|w| w[0] >= w[1])))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not: need exactly 1 argument".into()));
            }
            Ok(Val::Bool(!is_truthy(&args[0])))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("cons: need exactly 2 arguments".into()));
            }
            Ok(Val::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car: need exactly 1 argument".into()));
            }
            match &args[0] {
                Val::Pair(car, _) => Ok(*car.clone()),
                _ => Err(EvalError::Type("car: expected pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr: need exactly 1 argument".into()));
            }
            match &args[0] {
                Val::Pair(_, cdr) => Ok(*cdr.clone()),
                _ => Err(EvalError::Type("cdr: expected pair".into())),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("null?: need exactly 1 argument".into()));
            }
            Ok(Val::Bool(matches!(args[0], Val::Nil)))
        }
        "list" => {
            let mut result = Val::Nil;
            for item in args.iter().rev() {
                result = Val::Pair(Box::new(item.clone()), Box::new(result));
            }
            Ok(result)
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length: need exactly 1 argument".into()));
            }
            let mut count = 0i64;
            let mut cur = &args[0];
            loop {
                match cur {
                    Val::Nil => break,
                    Val::Pair(_, cdr) => {
                        count += 1;
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type("length: expected list".into())),
                }
            }
            Ok(Val::Int(count))
        }
        "append" => {
            if args.is_empty() {
                return Ok(Val::Nil);
            }
            let mut result = args.last().unwrap().clone();
            for arg in args[..args.len()-1].iter().rev() {
                let mut items = Vec::new();
                let mut cur = arg;
                loop {
                    match cur {
                        Val::Nil => break,
                        Val::Pair(car, cdr) => {
                            items.push(*car.clone());
                            cur = cdr;
                        }
                        _ => return Err(EvalError::Type("append: expected list".into())),
                    }
                }
                for item in items.into_iter().rev() {
                    result = Val::Pair(Box::new(item), Box::new(result));
                }
            }
            Ok(result)
        }
        "string?" => {
            if args.len() != 1 { return Err(EvalError::Arity("string?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Str(_))))
        }
        "number?" => {
            if args.len() != 1 { return Err(EvalError::Arity("number?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Int(_))))
        }
        "boolean?" => {
            if args.len() != 1 { return Err(EvalError::Arity("boolean?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Bool(_))))
        }
        "pair?" => {
            if args.len() != 1 { return Err(EvalError::Arity("pair?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Pair(_, _))))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Symbol(_))))
        }
        "char?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Char(_))))
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Val::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type("string-append: expected string".into())),
                }
            }
            Ok(Val::Str(result))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-length: need 1 argument".into())); }
            match &args[0] {
                Val::Str(s) => Ok(Val::Int(s.chars().count() as i64)),
                _ => Err(EvalError::Type("string-length: expected string".into())),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity("substring: need 3 arguments".into())); }
            let s = match &args[0] {
                Val::Str(s) => s,
                _ => return Err(EvalError::Type("substring: expected string".into())),
            };
            let start = as_int(&args[1], "substring")? as usize;
            let end = as_int(&args[2], "substring")? as usize;
            let chars: Vec<char> = s.chars().collect();
            if end > chars.len() || start > end {
                return Err(EvalError::Type("substring: index out of range".into()));
            }
            Ok(Val::Str(chars[start..end].iter().collect()))
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->number: need 1 argument".into())); }
            match &args[0] {
                Val::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Val::Int(n)),
                    Err(_) => Ok(Val::Bool(false)),
                },
                _ => Err(EvalError::Type("string->number: expected string".into())),
            }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("number->string: need 1 argument".into())); }
            let n = as_int(&args[0], "number->string")?;
            Ok(Val::Str(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol->string: need 1 argument".into())); }
            match &args[0] {
                Val::Symbol(s) => Ok(Val::Str(s.clone())),
                _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
            }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->symbol: need 1 argument".into())); }
            match &args[0] {
                Val::Str(s) => Ok(Val::Symbol(s.clone())),
                _ => Err(EvalError::Type("string->symbol: expected string".into())),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ref: need 2 arguments".into())); }
            let s = match &args[0] {
                Val::Str(s) => s,
                _ => return Err(EvalError::Type("string-ref: expected string".into())),
            };
            let idx = as_int(&args[1], "string-ref")? as usize;
            let chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type("string-ref: index out of range".into()));
            }
            Ok(Val::Char(chars[idx]))
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-copy: need 1 argument".into())); }
            match &args[0] {
                Val::Str(s) => Ok(Val::Str(s.clone())),
                _ => Err(EvalError::Type("string-copy: expected string".into())),
            }
        }
        _ => Err(EvalError::UnboundVariable(op.into())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let env = new_env(None);
    let mut result = Val::Void;
    let mut out = String::new();
    for expr in &exprs {
        result = eval(&expr, &env, &mut out)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    let env = new_env(None);
    let mut result = Val::Void;
    let mut out = String::new();
    for expr in &exprs {
        result = eval(&expr, &env, &mut out)?;
    }
    let result_str = match result {
        Val::Void => String::new(),
        other => other.to_string(),
    };
    Ok((result_str, out))
}

#[cfg(test)]
mod tests;
