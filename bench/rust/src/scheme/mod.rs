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
    Str(String),
    Symbol(String),
    Pair(Box<Val>, Box<Val>),
    Nil,
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Void,
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{n}"),
            Val::Bool(true) => write!(f, "#t"),
            Val::Bool(false) => write!(f, "#f"),
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

fn eval(expr: &Expr, env: &Env) -> Result<Val, EvalError> {
    let span = expr.span;
    match &expr.kind {
        ExprKind::Int(n) => Ok(Val::Int(*n)),
        ExprKind::Bool(b) => Ok(Val::Bool(*b)),
        ExprKind::Str(s) => Ok(Val::Str(s.clone())),
        ExprKind::Symbol(name) => {
            env_get(env, name)
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
                                let val = eval(&list[2], env)?;
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
                                let params: Result<Vec<String>, _> = sig[1..].iter().map(|p| {
                                    match &p.kind {
                                        ExprKind::Symbol(s) => Ok(s.clone()),
                                        _ => Err(err_at(span, EvalError::Parse("define: expected parameter name".into()))),
                                    }
                                }).collect();
                                let params = params?;
                                let body = list[2..].to_vec();
                                let lambda = Val::Lambda {
                                    params,
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
                        let cond = eval(&list[1], env)?;
                        if is_truthy(&cond) {
                            return eval(&list[2], env);
                        } else if list.len() == 4 {
                            return eval(&list[3], env);
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
                        let params = match &list[1].kind {
                            ExprKind::List(param_list) => {
                                let mut ps = Vec::new();
                                for p in param_list {
                                    match &p.kind {
                                        ExprKind::Symbol(s) => ps.push(s.clone()),
                                        _ => return Err(err_at(span, EvalError::Parse("lambda: expected parameter name".into()))),
                                    }
                                }
                                ps
                            }
                            _ => return Err(err_at(span, EvalError::Parse("lambda: expected parameter list".into()))),
                        };
                        let body = list[2..].to_vec();
                        return Ok(Val::Lambda {
                            params,
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
                                        let val = eval(&pair[1], env)?;
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
                                result = eval(expr, &call_env)?;
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
                                    let val = eval(&pair[1], env)?;
                                    env_set(&let_env, name, val);
                                }
                                _ => return Err(err_at(span, EvalError::Parse("let: bad binding".into()))),
                            }
                        }
                        let mut result = Val::Void;
                        for expr in &list[2..] {
                            result = eval(expr, &let_env)?;
                        }
                        return Ok(result);
                    }
                    "begin" => {
                        let mut result = Val::Void;
                        for expr in &list[1..] {
                            result = eval(expr, env)?;
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
                                                result = eval(expr, env)?;
                                            }
                                            return Ok(result);
                                        }
                                    }
                                    let cond_val = eval(&parts[0], env)?;
                                    if is_truthy(&cond_val) {
                                        let mut result = cond_val;
                                        for expr in &parts[1..] {
                                            result = eval(expr, env)?;
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
                            result = eval(arg, env)?;
                            if !is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        let mut result = Val::Bool(false);
                        for arg in &list[1..] {
                            result = eval(arg, env)?;
                            if is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    _ => {}
                }
            }

            // Function application: try builtins first for known symbols
            if let ExprKind::Symbol(op) = &list[0].kind {
                if is_builtin(op) {
                    let args: Result<Vec<Val>, _> = list[1..].iter().map(|a| eval(a, env)).collect();
                    let args = args?;
                    return apply_builtin(op, &args).map_err(|e| err_at(span, e));
                }
            }

            let func = eval(&list[0], env)?;
            let args: Result<Vec<Val>, _> = list[1..].iter().map(|a| eval(a, env)).collect();
            let args = args?;

            match func {
                Val::Lambda { params, body, env: closure_env } => {
                    if args.len() != params.len() {
                        return Err(err_at(span, EvalError::Arity(format!(
                            "expected {} arguments, got {}", params.len(), args.len()
                        ))));
                    }
                    let call_env = new_env(Some(closure_env));
                    for (p, a) in params.iter().zip(args) {
                        env_set(&call_env, p.clone(), a);
                    }
                    let mut result = Val::Void;
                    for expr in &body {
                        result = eval(expr, &call_env)?;
                    }
                    Ok(result)
                }
                _ => Err(err_at(span, EvalError::Type("not a procedure".into()))),
            }
        }
    }
}

fn is_builtin(op: &str) -> bool {
    matches!(op, "+" | "-" | "*" | "/" | "=" | "<" | ">" | "<=" | ">=" | "not"
        | "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
        | "string?" | "number?" | "boolean?" | "pair?" | "symbol?")
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
        _ => Err(EvalError::UnboundVariable(op.into())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let env = new_env(None);
    let mut result = Val::Void;
    for expr in &exprs {
        result = eval(&expr, &env)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
