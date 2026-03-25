pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

type BuiltinFn = fn(&[Val], &Env) -> Result<Val, EvalError>;

#[derive(Clone)]
enum Val {
    Int(i64),
    Bool(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Val>),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(BuiltinFn),
    Void,
}

impl Val {
    fn is_truthy(&self) -> bool {
        !matches!(self, Val::Bool(false))
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{n}"),
            Val::Bool(true) => write!(f, "#t"),
            Val::Bool(false) => write!(f, "#f"),
            Val::Str(s) => write!(f, "\"{}\"", s),
            Val::Char(c) => write!(f, "#\\{c}"),
            Val::Symbol(s) => write!(f, "{s}"),
            Val::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Val::Lambda { .. } | Val::Builtin(..) => write!(f, "#<procedure>"),
            Val::Void => write!(f, "#<void>"),
        }
    }
}

// --- Environment ---

type Frame = Rc<RefCell<HashMap<String, Val>>>;

#[derive(Clone)]
struct Env {
    frames: Vec<Frame>,
    output: Rc<RefCell<String>>,
}

impl Env {
    fn new() -> Self {
        let frame = Rc::new(RefCell::new(HashMap::new()));
        let builtins: &[(&str, BuiltinFn)] = &[
            ("+", builtin_add as BuiltinFn),
            ("-", builtin_sub),
            ("*", builtin_mul),
            ("/", builtin_div),
            ("<", builtin_lt),
            (">", builtin_gt),
            ("=", builtin_eq),
            ("<=", builtin_le),
            (">=", builtin_ge),
            ("not", builtin_not),
            ("cons", builtin_cons as BuiltinFn),
            ("car", builtin_car),
            ("cdr", builtin_cdr),
            ("list", builtin_list),
            ("null?", builtin_null),
            ("length", builtin_length),
            ("append", builtin_append),
            ("boolean?", builtin_is_boolean),
            ("number?", builtin_is_number),
            ("string?", builtin_is_string),
            ("symbol?", builtin_is_symbol),
            ("pair?", builtin_is_pair),
            ("char?", builtin_is_char),
            ("display", builtin_display),
            ("write", builtin_write),
            ("newline", builtin_newline),
            ("string-append", builtin_string_append),
            ("string-length", builtin_string_length),
            ("substring", builtin_substring),
            ("string->number", builtin_string_to_number),
            ("number->string", builtin_number_to_string),
            ("symbol->string", builtin_symbol_to_string),
            ("string->symbol", builtin_string_to_symbol),
            ("string-ref", builtin_string_ref),
            ("string-copy", builtin_string_copy),
        ];
        for &(name, f) in builtins {
            frame.borrow_mut().insert(name.to_string(), Val::Builtin(f));
        }
        Env { frames: vec![frame], output: Rc::new(RefCell::new(String::new())) }
    }

    fn get(&self, name: &str) -> Option<Val> {
        for frame in self.frames.iter().rev() {
            if let Some(v) = frame.borrow().get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    fn define(&self, name: String, val: Val) {
        self.frames.last().expect("env has no frames").borrow_mut().insert(name, val);
    }

    fn set(&self, name: &str, val: Val) -> Result<(), EvalError> {
        for frame in self.frames.iter().rev() {
            let mut f = frame.borrow_mut();
            if f.contains_key(name) {
                f.insert(name.to_string(), val);
                return Ok(());
            }
        }
        Err(EvalError::UnboundVariable(name.to_string()))
    }

    fn push(&self) -> Env {
        let mut frames = self.frames.clone();
        frames.push(Rc::new(RefCell::new(HashMap::new())));
        Env { frames, output: Rc::clone(&self.output) }
    }
}

// --- Source Positions ---

#[derive(Debug, Clone, Copy)]
struct Span {
    line: usize,
    col: usize,
}

impl Span {
    fn new(line: usize, col: usize) -> Self {
        Span { line, col }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// --- Parser ---

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
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

impl Expr {
    fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }
}

struct Token {
    text: String,
    span: Span,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    let mut line = 1usize;
    let mut col = 1usize;
    while let Some(&c) = chars.peek() {
        match c {
            '\n' => { chars.next(); line += 1; col = 1; }
            ' ' | '\t' | '\r' => { chars.next(); col += 1; }
            ';' => {
                while let Some(&c2) = chars.peek() {
                    chars.next();
                    col += 1;
                    if c2 == '\n' { line += 1; col = 1; break; }
                }
            }
            '(' => { tokens.push(Token { text: "(".into(), span: Span::new(line, col) }); chars.next(); col += 1; }
            ')' => { tokens.push(Token { text: ")".into(), span: Span::new(line, col) }); chars.next(); col += 1; }
            '\'' => { tokens.push(Token { text: "'".into(), span: Span::new(line, col) }); chars.next(); col += 1; }
            '"' => {
                let start_span = Span::new(line, col);
                chars.next();
                col += 1;
                let mut s = String::new();
                loop {
                    match chars.next() {
                        Some('\\') => {
                            col += 1;
                            match chars.next() {
                                Some('n') => { s.push('\n'); col += 1; }
                                Some('t') => { s.push('\t'); col += 1; }
                                Some('"') => { s.push('"'); col += 1; }
                                Some('\\') => { s.push('\\'); col += 1; }
                                Some(other) => { s.push('\\'); s.push(other); col += 1; }
                                None => break,
                            }
                        }
                        Some('"') => { col += 1; break; }
                        Some('\n') => { s.push('\n'); line += 1; col = 1; }
                        Some(c2) => { s.push(c2); col += 1; }
                        None => break,
                    }
                }
                tokens.push(Token { text: format!("\"{}\"", s), span: start_span });
            }
            _ => {
                let start_span = Span::new(line, col);
                let mut tok = String::new();
                while let Some(&c2) = chars.peek() {
                    if c2 == '(' || c2 == ')' || c2 == ' ' || c2 == '\t' || c2 == '\n' || c2 == '\r' || c2 == ';' || c2 == '\'' {
                        break;
                    }
                    tok.push(c2);
                    chars.next();
                    col += 1;
                }
                tokens.push(Token { text: tok, span: start_span });
            }
        }
    }
    tokens
}

fn parse(tokens: &[Token]) -> Result<(Expr, usize), EvalError> {
    if tokens.is_empty() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let tok = &tokens[0];
    let span = tok.span;
    if tok.text == "'" {
        let (inner, consumed) = parse(&tokens[1..])?;
        Ok((Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("quote".into()), span),
            inner,
        ]), span), 1 + consumed))
    } else if tok.text == "(" {
        let mut elems = Vec::new();
        let mut i = 1;
        while i < tokens.len() && tokens[i].text != ")" {
            let (expr, consumed) = parse(&tokens[i..])?;
            elems.push(expr);
            i += consumed;
        }
        if i >= tokens.len() {
            return Err(EvalError::Parse(format!("missing closing paren at {span}")));
        }
        Ok((Expr::new(ExprKind::List(elems), span), i + 1))
    } else if tok.text == ")" {
        Err(EvalError::Parse(format!("unexpected ) at {span}")))
    } else if tok.text.starts_with('"') {
        let s = tok.text[1..tok.text.len()-1].to_string();
        Ok((Expr::new(ExprKind::Str(s), span), 1))
    } else if tok.text == "#t" {
        Ok((Expr::new(ExprKind::Bool(true), span), 1))
    } else if tok.text == "#f" {
        Ok((Expr::new(ExprKind::Bool(false), span), 1))
    } else if tok.text.starts_with("#\\") {
        let rest = &tok.text[2..];
        let ch = match rest {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().expect("single-char string is non-empty"),
            _ => return Err(EvalError::Parse(format!("unknown character literal: {} at {span}", tok.text))),
        };
        Ok((Expr::new(ExprKind::Char(ch), span), 1))
    } else if let Ok(n) = tok.text.parse::<i64>() {
        Ok((Expr::new(ExprKind::Int(n), span), 1))
    } else {
        Ok((Expr::new(ExprKind::Symbol(tok.text.clone()), span), 1))
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut exprs = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        let (expr, consumed) = parse(&tokens[i..])?;
        exprs.push(expr);
        i += consumed;
    }
    Ok(exprs)
}

// --- Evaluator ---

fn span_err(span: Span, err: EvalError) -> EvalError {
    // If the error message already contains position info, return as-is
    let msg = err.to_string();
    if msg.contains(':') && msg.bytes().any(|b| b.is_ascii_digit()) {
        // Check more carefully: look for digit:digit pattern
        let bytes = msg.as_bytes();
        let has_pos = bytes.windows(3).any(|w| {
            w[0].is_ascii_digit() && w[1] == b':' && w[2].is_ascii_digit()
        });
        if has_pos {
            return err;
        }
    }
    match err {
        EvalError::Parse(m) => EvalError::Parse(format!("{m} at {span}")),
        EvalError::Type(m) => EvalError::Type(format!("{m} at {span}")),
        EvalError::UnboundVariable(m) => EvalError::UnboundVariable(format!("{m} at {span}")),
        EvalError::Arity(m) => EvalError::Arity(format!("{m} at {span}")),
        EvalError::Runtime(m) => EvalError::Runtime(format!("{m} at {span}")),
    }
}

fn eval(expr: &Expr, env: &Env) -> Result<Val, EvalError> {
    let span = expr.span;
    match &expr.kind {
        ExprKind::Int(n) => Ok(Val::Int(*n)),
        ExprKind::Bool(b) => Ok(Val::Bool(*b)),
        ExprKind::Str(s) => Ok(Val::Str(s.clone())),
        ExprKind::Char(c) => Ok(Val::Char(*c)),
        ExprKind::Symbol(name) => {
            env.get(name).ok_or_else(|| EvalError::UnboundVariable(format!("{name} at {span}")))
        }
        ExprKind::List(elems) => {
            if elems.is_empty() {
                return Ok(Val::List(vec![]));
            }
            // Check for special forms
            if let ExprKind::Symbol(op) = &elems[0].kind {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], env, span),
                    "if" => return eval_if(&elems[1..], env, span),
                    "quote" => return eval_quote(&elems[1..], span),
                    "lambda" => return eval_lambda(&elems[1..], env, span),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "begin" => return eval_begin(&elems[1..], env),
                    "let" => return eval_let(&elems[1..], env, span),
                    "cond" => return eval_cond(&elems[1..], env),
                    "set!" => return eval_set_bang(&elems[1..], env, span),
                    "string-set!" => return eval_string_set(&elems[1..], env, span),
                    _ => {}
                }
            }
            // Evaluate function position
            let func = eval(&elems[0], env)?;
            let args: Vec<Val> = elems[1..].iter().map(|e| eval(e, env)).collect::<Result<_, _>>()?;
            apply_val(&func, &args, env).map_err(|e| span_err(span, e))
        }
    }
}

fn apply_val(func: &Val, args: &[Val], caller_env: &Env) -> Result<Val, EvalError> {
    match func {
        Val::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let new_env = env.push();
            for (p, a) in params.iter().zip(args.iter()) {
                new_env.define(p.clone(), a.clone());
            }
            let mut result = Val::Void;
            for expr in body {
                result = eval(expr, &new_env)?;
            }
            Ok(result)
        }
        Val::Builtin(f) => f(args, caller_env),
        _ => Err(EvalError::Type("not a procedure".into())),
    }
}

fn eval_define(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("define: missing arguments at {span}")));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("define: expected 2 arguments at {span}")));
            }
            let val = eval(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Val::Void)
        }
        ExprKind::List(sig) => {
            // (define (f params...) body...)
            if sig.is_empty() {
                return Err(EvalError::Parse(format!("define: empty signature at {span}")));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse(format!("define: expected symbol at {span}"))),
            };
            let params: Vec<String> = sig[1..].iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse(format!("define: expected parameter name at {span}"))),
            }).collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Val::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env.define(name, lambda);
            Ok(Val::Void)
        }
        _ => Err(EvalError::Parse(format!("define: expected symbol or list at {span}"))),
    }
}

fn eval_set_bang(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("set!: expected 2 arguments at {span}")));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s,
        _ => return Err(EvalError::Parse(format!("set!: expected symbol at {span}"))),
    };
    let val = eval(&args[1], env)?;
    env.set(name, val).map_err(|e| span_err(span, e))?;
    Ok(Val::Void)
}

fn eval_if(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity(format!("if: expected 2 or 3 arguments at {span}")));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Val::Void)
    }
}

fn eval_quote(args: &[Expr], span: Span) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("quote: expected 1 argument at {span}")));
    }
    expr_to_val(&args[0])
}

fn expr_to_val(expr: &Expr) -> Result<Val, EvalError> {
    match &expr.kind {
        ExprKind::Int(n) => Ok(Val::Int(*n)),
        ExprKind::Bool(b) => Ok(Val::Bool(*b)),
        ExprKind::Str(s) => Ok(Val::Str(s.clone())),
        ExprKind::Char(c) => Ok(Val::Char(*c)),
        ExprKind::Symbol(s) => Ok(Val::Symbol(s.clone())),
        ExprKind::List(elems) => {
            let vals: Vec<Val> = elems.iter().map(expr_to_val).collect::<Result<_, _>>()?;
            Ok(Val::List(vals))
        }
    }
}

fn eval_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("lambda: missing parameters at {span}")));
    }
    let params = match &args[0].kind {
        ExprKind::List(param_exprs) => {
            param_exprs.iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse(format!("lambda: expected parameter name at {span}"))),
            }).collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Parse(format!("lambda: expected parameter list at {span}"))),
    };
    let body = args[1..].to_vec();
    Ok(Val::Lambda {
        params,
        body,
        env: env.clone(),
    })
}

fn eval_and(exprs: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if exprs.is_empty() {
        return Ok(Val::Bool(true));
    }
    let mut result = Val::Bool(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if exprs.is_empty() {
        return Ok(Val::Bool(false));
    }
    for expr in exprs {
        let result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Val::Bool(false))
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Val, EvalError> {
    let mut result = Val::Void;
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_let(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("let: missing arguments at {span}")));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 2 {
            return Err(EvalError::Parse(format!("let: missing bindings at {span}")));
        }
        let bindings = match &args[1].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("let: expected bindings list at {span}"))),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env)?);
                    } else {
                        return Err(EvalError::Parse(format!("let: expected variable name at {span}")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("let: invalid binding at {span}"))),
            }
        }
        let body = args[2..].to_vec();
        let new_env = env.push();
        let lambda = Val::Lambda {
            params: params.clone(),
            body,
            env: new_env.clone(),
        };
        new_env.define(name.clone(), lambda.clone());
        apply_val(&lambda, &inits, &new_env)
    } else {
        // Regular let: (let ((var init) ...) body ...)
        let bindings = match &args[0].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("let: expected bindings list at {span}"))),
        };
        let new_env = env.push();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        let val = eval(&pair[1], env)?;
                        new_env.define(s.clone(), val);
                    } else {
                        return Err(EvalError::Parse(format!("let: expected variable name at {span}")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("let: invalid binding at {span}"))),
            }
        }
        let mut result = Val::Void;
        for expr in &args[1..] {
            result = eval(expr, &new_env)?;
        }
        Ok(result)
    }
}

fn eval_cond(clauses: &[Expr], env: &Env) -> Result<Val, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => {
                // Check for else clause
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        let mut result = Val::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env)?;
                if test.is_truthy() {
                    let mut result = test;
                    for expr in &parts[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Parse(format!("cond: invalid clause at {}", clause.span))),
        }
    }
    Ok(Val::Void)
}

fn require_ints(args: &[Val], op: &str) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|a| match a {
        Val::Int(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{op}: expected number"))),
    }).collect()
}

fn builtin_add(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums = require_ints(args, "+")?;
    Ok(Val::Int(nums.iter().sum()))
}

fn builtin_sub(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("-: need at least 1 argument".into()));
    }
    let nums = require_ints(args, "-")?;
    if nums.len() == 1 {
        Ok(Val::Int(-nums[0]))
    } else {
        Ok(Val::Int(nums[0] - nums[1..].iter().sum::<i64>()))
    }
}

fn builtin_mul(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums = require_ints(args, "*")?;
    Ok(Val::Int(nums.iter().product()))
}

fn builtin_div(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("/: need at least 2 arguments".into()));
    }
    let nums = require_ints(args, "/")?;
    if nums[1..].contains(&0) {
        return Err(EvalError::Runtime("division by zero".into()));
    }
    let mut result = nums[0];
    for &n in &nums[1..] {
        result /= n;
    }
    Ok(Val::Int(result))
}

fn builtin_lt(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums = require_ints(args, "<")?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] < w[1])))
}

fn builtin_gt(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums = require_ints(args, ">")?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] > w[1])))
}

fn builtin_eq(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums = require_ints(args, "=")?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] == w[1])))
}

fn builtin_le(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums = require_ints(args, "<=")?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] <= w[1])))
}

fn builtin_ge(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums = require_ints(args, ">=")?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] >= w[1])))
}

fn builtin_not(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("not: expected 1 argument".into()));
    }
    Ok(Val::Bool(!args[0].is_truthy()))
}

fn builtin_cons(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("cons: expected 2 arguments".into()));
    }
    match &args[1] {
        Val::List(elems) => {
            let mut new = vec![args[0].clone()];
            new.extend(elems.iter().cloned());
            Ok(Val::List(new))
        }
        _ => {
            // cons pair (improper list) - for now treat as 2-element list
            Ok(Val::List(vec![args[0].clone(), args[1].clone()]))
        }
    }
}

fn builtin_car(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car: expected 1 argument".into()));
    }
    match &args[0] {
        Val::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
        _ => Err(EvalError::Type("car: expected non-empty list".into())),
    }
}

fn builtin_cdr(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr: expected 1 argument".into()));
    }
    match &args[0] {
        Val::List(elems) if !elems.is_empty() => Ok(Val::List(elems[1..].to_vec())),
        _ => Err(EvalError::Type("cdr: expected non-empty list".into())),
    }
}

fn builtin_list(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    Ok(Val::List(args.to_vec()))
}

fn builtin_null(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("null?: expected 1 argument".into()));
    }
    Ok(Val::Bool(matches!(&args[0], Val::List(v) if v.is_empty())))
}

fn builtin_length(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("length: expected 1 argument".into()));
    }
    match &args[0] {
        Val::List(elems) => Ok(Val::Int(elems.len() as i64)),
        _ => Err(EvalError::Type("length: expected list".into())),
    }
}

fn builtin_append(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let mut result = Vec::new();
    for arg in args {
        match arg {
            Val::List(elems) => result.extend(elems.iter().cloned()),
            _ => return Err(EvalError::Type("append: expected list".into())),
        }
    }
    Ok(Val::List(result))
}

fn builtin_is_boolean(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("boolean?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Bool(_))))
}

fn builtin_is_number(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("number?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Int(_))))
}

fn builtin_is_string(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Str(_))))
}

fn builtin_is_symbol(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("symbol?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Symbol(_))))
}

fn builtin_is_pair(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("pair?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(&args[0], Val::List(v) if !v.is_empty())))
}

fn builtin_is_char(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Char(_))))
}

/// Format a value for `display` (no quotes on strings).
fn display_format(val: &Val) -> String {
    match val {
        Val::Str(s) => s.clone(),
        Val::Char(c) => c.to_string(),
        other => other.to_string(),
    }
}

fn builtin_display(args: &[Val], env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("display: expected 1 argument".into())); }
    let text = display_format(&args[0]);
    env.output.borrow_mut().push_str(&text);
    Ok(Val::Void)
}

fn builtin_write(args: &[Val], env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("write: expected 1 argument".into())); }
    let text = args[0].to_string();
    env.output.borrow_mut().push_str(&text);
    Ok(Val::Void)
}

fn builtin_newline(args: &[Val], env: &Env) -> Result<Val, EvalError> {
    if !args.is_empty() { return Err(EvalError::Arity("newline: expected 0 arguments".into())); }
    env.output.borrow_mut().push('\n');
    Ok(Val::Void)
}

fn builtin_string_append(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let mut result = String::new();
    for arg in args {
        match arg {
            Val::Str(s) => result.push_str(s),
            _ => return Err(EvalError::Type("string-append: expected string".into())),
        }
    }
    Ok(Val::Str(result))
}

fn builtin_string_length(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-length: expected 1 argument".into())); }
    match &args[0] {
        Val::Str(s) => Ok(Val::Int(s.chars().count() as i64)),
        _ => Err(EvalError::Type("string-length: expected string".into())),
    }
}

fn builtin_substring(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 3 { return Err(EvalError::Arity("substring: expected 3 arguments".into())); }
    let s = match &args[0] { Val::Str(s) => s, _ => return Err(EvalError::Type("substring: expected string".into())) };
    let start = match &args[1] { Val::Int(n) => *n as usize, _ => return Err(EvalError::Type("substring: expected integer".into())) };
    let end = match &args[2] { Val::Int(n) => *n as usize, _ => return Err(EvalError::Type("substring: expected integer".into())) };
    let chars: Vec<char> = s.chars().collect();
    if start > end || end > chars.len() {
        return Err(EvalError::Runtime("substring: index out of range".into()));
    }
    Ok(Val::Str(chars[start..end].iter().collect()))
}

fn builtin_string_to_number(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string->number: expected 1 argument".into())); }
    match &args[0] {
        Val::Str(s) => match s.parse::<i64>() {
            Ok(n) => Ok(Val::Int(n)),
            Err(_) => Ok(Val::Bool(false)),
        },
        _ => Err(EvalError::Type("string->number: expected string".into())),
    }
}

fn builtin_number_to_string(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("number->string: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(n) => Ok(Val::Str(n.to_string())),
        _ => Err(EvalError::Type("number->string: expected number".into())),
    }
}

fn builtin_symbol_to_string(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("symbol->string: expected 1 argument".into())); }
    match &args[0] {
        Val::Symbol(s) => Ok(Val::Str(s.clone())),
        _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
    }
}

fn builtin_string_to_symbol(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string->symbol: expected 1 argument".into())); }
    match &args[0] {
        Val::Str(s) => Ok(Val::Symbol(s.clone())),
        _ => Err(EvalError::Type("string->symbol: expected string".into())),
    }
}

fn builtin_string_ref(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string-ref: expected 2 arguments".into())); }
    let s = match &args[0] { Val::Str(s) => s, _ => return Err(EvalError::Type("string-ref: expected string".into())) };
    let idx = match &args[1] { Val::Int(n) => *n as usize, _ => return Err(EvalError::Type("string-ref: expected integer".into())) };
    let chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::Runtime("string-ref: index out of range".into()));
    }
    Ok(Val::Char(chars[idx]))
}

fn eval_string_set(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!("string-set!: expected 3 arguments at {span}")));
    }
    let target = eval(&args[0], env)?;
    let idx = match eval(&args[1], env)? {
        Val::Int(n) => n as usize,
        _ => return Err(EvalError::Type("string-set!: expected integer index".into())),
    };
    let ch = match eval(&args[2], env)? {
        Val::Char(c) => c,
        _ => return Err(EvalError::Type("string-set!: expected char".into())),
    };
    let mut s = match target {
        Val::Str(s) => s,
        _ => return Err(EvalError::Type("string-set!: expected string".into())),
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::Runtime("string-set!: index out of range".into()));
    }
    chars[idx] = ch;
    s = chars.into_iter().collect();
    // If the first argument is a variable, update it in the environment
    if let ExprKind::Symbol(name) = &args[0].kind {
        env.set(name, Val::Str(s))?;
    }
    Ok(Val::Void)
}

fn builtin_string_copy(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-copy: expected 1 argument".into()));
    }
    match &args[0] {
        Val::Str(s) => Ok(Val::Str(s.clone())),
        _ => Err(EvalError::Type("string-copy: expected string".into())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = Env::new();
    let mut last = Val::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    Ok(last.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = Env::new();
    let mut last = Val::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    let output = env.output.borrow().clone();
    Ok((last.to_string(), output))
}

#[cfg(test)]
mod tests;
