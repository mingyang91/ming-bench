pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

thread_local! {
    static OUTPUT_BUF: RefCell<String> = RefCell::new(String::new());
}

fn output_write(s: &str) {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().push_str(s));
}

fn output_take() -> String {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().split_off(0))
}

fn make_string(s: std::string::String) -> Value {
    Value::String(Rc::new(RefCell::new(s)))
}

/// A Scheme runtime value.
#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(Rc<RefCell<std::string::String>>),
    Char(char),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Symbol(String),
    Void,
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String, fn(&[Value]) -> Result<Value, EvalError>),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "Integer({n})"),
            Value::Boolean(b) => write!(f, "Boolean({b})"),
            Value::String(s) => write!(f, "String({:?})", s.borrow()),
            Value::Char(c) => write!(f, "Char({c:?})"),
            Value::List(l) => write!(f, "List({l:?})"),
            Value::Pair(a, b) => write!(f, "Pair({a:?}, {b:?})"),
            Value::Symbol(s) => write!(f, "Symbol({s})"),
            Value::Void => write!(f, "Void"),
            Value::Lambda { params, .. } => write!(f, "Lambda({params:?})"),
            Value::Builtin(name, _) => write!(f, "Builtin({name})"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => *a.borrow() == *b.borrow(),
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Pair(a1, b1), Value::Pair(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

impl Value {
    /// Format for `display` — strings without quotes, chars as raw character.
    fn display_fmt(&self, out: &mut String) {
        match self {
            Value::String(s) => out.push_str(&s.borrow()),
            Value::Char(c) => out.push(*c),
            _ => out.push_str(&self.to_string()),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{}\"", s.borrow()),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Value::Pair(a, b) => {
                write!(f, "({a}")?;
                let mut cur = b.as_ref();
                loop {
                    match cur {
                        Value::Pair(ca, cb) => {
                            write!(f, " {ca}")?;
                            cur = cb.as_ref();
                        }
                        Value::List(l) if l.is_empty() => break,
                        other => {
                            write!(f, " . {other}")?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Void => write!(f, "#<void>"),
            Value::Lambda { .. } | Value::Builtin(..) => write!(f, "#<procedure>"),
        }
    }
}

// ── AST with source positions ──

#[derive(Clone, Debug, PartialEq)]
struct Expr {
    kind: ExprKind,
    line: usize,
    col: usize,
}

#[derive(Clone, Debug, PartialEq)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => make_string(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(elems) => Value::List(elems.iter().map(expr_to_value).collect()),
    }
}

// ── Environment ──

type Env = Rc<RefCell<EnvInner>>;

struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

fn new_env(parent: Option<Env>) -> Env {
    Rc::new(RefCell::new(EnvInner {
        bindings: HashMap::new(),
        parent,
    }))
}

fn env_get(env: &Env, name: &str) -> Option<Value> {
    let inner = env.borrow();
    if let Some(val) = inner.bindings.get(name) {
        Some(val.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        None
    }
}

fn env_set(env: &Env, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

fn default_env() -> Env {
    let env = new_env(None);
    let builtins: &[(&str, fn(&[Value]) -> Result<Value, EvalError>)] = &[
        ("+", arith_add),
        ("-", arith_sub),
        ("*", arith_mul),
        ("/", arith_div),
        ("<", |a| cmp_op(a, |x, y| x < y)),
        (">", |a| cmp_op(a, |x, y| x > y)),
        ("=", |a| cmp_op(a, |x, y| x == y)),
        ("<=", |a| cmp_op(a, |x, y| x <= y)),
        (">=", |a| cmp_op(a, |x, y| x >= y)),
        ("not", builtin_not),
        ("cons", builtin_cons),
        ("car", builtin_car),
        ("cdr", builtin_cdr),
        ("null?", builtin_null),
        ("list", builtin_list),
        ("length", builtin_length),
        ("append", builtin_append),
        ("string?", |a| builtin_type_pred(a, "string?")),
        ("number?", |a| builtin_type_pred(a, "number?")),
        ("boolean?", |a| builtin_type_pred(a, "boolean?")),
        ("pair?", |a| builtin_type_pred(a, "pair?")),
        ("symbol?", |a| builtin_type_pred(a, "symbol?")),
        ("char?", |a| builtin_type_pred(a, "char?")),
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
        ("string-set!", builtin_string_set),
    ];
    for &(name, func) in builtins {
        env_set(&env, name.to_string(), Value::Builtin(name.to_string(), func));
    }
    env
}

// ── Parser ──

struct Token {
    text: String,
    line: usize,
    col: usize,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line = 1usize;
    let mut col = 1usize;
    while i < chars.len() {
        match chars[i] {
            '\n' => {
                i += 1;
                line += 1;
                col = 1;
            }
            ' ' | '\t' | '\r' => {
                i += 1;
                col += 1;
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' | ')' => {
                tokens.push(Token { text: chars[i].to_string(), line, col });
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push(Token { text: "'".to_string(), line, col });
                i += 1;
                col += 1;
            }
            '"' => {
                let tok_line = line;
                let tok_col = col;
                let mut s = String::new();
                s.push('"');
                i += 1;
                col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        i += 1;
                        col += 1;
                        s.push(chars[i]);
                        if chars[i] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                        i += 1;
                    } else {
                        s.push(chars[i]);
                        if chars[i] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: s, line: tok_line, col: tok_col });
            }
            '#' => {
                let tok_col = col;
                let mut tok = String::new();
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')') {
                    tok.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: tok, line, col: tok_col });
            }
            _ => {
                let tok_col = col;
                let mut tok = String::new();
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';') {
                    tok.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: tok, line, col: tok_col });
            }
        }
    }
    tokens
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            tokens: tokenize(input),
            pos: 0,
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        if self.pos >= self.tokens.len() {
            return Err(EvalError::Parse("unexpected end of input".into()));
        }
        let line = self.tokens[self.pos].line;
        let col = self.tokens[self.pos].col;
        let text = self.tokens[self.pos].text.clone();

        if text == "(" {
            self.pos += 1;
            let mut elems = Vec::new();
            while self.pos < self.tokens.len() && self.tokens[self.pos].text != ")" {
                elems.push(self.parse_expr()?);
            }
            if self.pos >= self.tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into()));
            }
            self.pos += 1;
            Ok(Expr { kind: ExprKind::List(elems), line, col })
        } else if text == "'" {
            self.pos += 1;
            let inner = self.parse_expr()?;
            Ok(Expr {
                kind: ExprKind::List(vec![
                    Expr { kind: ExprKind::Symbol("quote".into()), line, col },
                    inner,
                ]),
                line,
                col,
            })
        } else {
            self.pos += 1;
            Ok(Expr { kind: parse_atom(&text), line, col })
        }
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        while self.pos < self.tokens.len() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }
}

fn parse_atom(token: &str) -> ExprKind {
    if token == "#t" {
        ExprKind::Boolean(true)
    } else if token == "#f" {
        ExprKind::Boolean(false)
    } else if token.starts_with("#\\") {
        let rest = &token[2..];
        let c = match rest {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().unwrap(),
            _ => rest.chars().next().unwrap_or('?'),
        };
        ExprKind::Char(c)
    } else if token.starts_with('"') && token.ends_with('"') {
        ExprKind::Str(token[1..token.len() - 1].to_string())
    } else if let Ok(n) = token.parse::<i64>() {
        ExprKind::Integer(n)
    } else {
        ExprKind::Symbol(token.to_string())
    }
}

// ── Evaluator (trampoline-based for TCO) ──

enum Trampoline {
    Done(Value),
    Bounce(Expr, Env),
}

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let mut cur_expr = expr.clone();
    let mut cur_env = env.clone();
    loop {
        match eval_tail(&cur_expr, &cur_env).map_err(|e| e.at(cur_expr.line, cur_expr.col))? {
            Trampoline::Done(v) => return Ok(v),
            Trampoline::Bounce(e, env) => {
                cur_expr = e;
                cur_env = env;
            }
        }
    }
}

fn eval_tail(expr: &Expr, env: &Env) -> Result<Trampoline, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Trampoline::Done(Value::Integer(*n))),
        ExprKind::Boolean(b) => Ok(Trampoline::Done(Value::Boolean(*b))),
        ExprKind::Str(s) => Ok(Trampoline::Done(make_string(s.clone()))),
        ExprKind::Char(c) => Ok(Trampoline::Done(Value::Char(*c))),
        ExprKind::Symbol(name) => {
            env_get(env, name)
                .map(Trampoline::Done)
                .ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        ExprKind::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            // Check for special forms
            if let ExprKind::Symbol(name) = &elems[0].kind {
                match name.as_str() {
                    "quote" => return eval_quote(&elems[1..]).map(Trampoline::Done),
                    "if" => return eval_if_tail(&elems[1..], env),
                    "define" => return eval_define(&elems[1..], env).map(Trampoline::Done),
                    "lambda" => return eval_lambda(&elems[1..], env).map(Trampoline::Done),
                    "and" => return eval_and_tail(&elems[1..], env),
                    "or" => return eval_or_tail(&elems[1..], env),
                    "let" => return eval_let_tail(&elems[1..], env),
                    "begin" => return eval_begin_tail(&elems[1..], env),
                    "cond" => return eval_cond_tail(&elems[1..], env),
                    _ => {}
                }
            }
            // Function application
            let func = eval(&elems[0], env)?;
            let args: Result<Vec<Value>, _> = elems[1..].iter().map(|e| eval(e, env)).collect();
            let args = args?;
            apply_tail(&func, &args)
        }
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("quote expects 1 argument".into()));
    }
    Ok(expr_to_value(&args[0]))
}

fn eval_if_tail(args: &[Expr], env: &Env) -> Result<Trampoline, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if expects 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env)?;
    if cond != Value::Boolean(false) {
        Ok(Trampoline::Bounce(args[1].clone(), env.clone()))
    } else if args.len() == 3 {
        Ok(Trampoline::Bounce(args[2].clone(), env.clone()))
    } else {
        Ok(Trampoline::Done(Value::Void))
    }
}

fn eval_define(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires arguments".into()));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define expects 2 arguments".into()));
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: expected symbol as function name".into())),
            };
            let params: Result<Vec<String>, _> = sig[1..].iter().map(|v| match &v.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type("define: expected symbol as parameter".into())),
            }).collect();
            let params = params?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("define: expected symbol or list".into())),
    }
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires params and body".into()));
    }
    let params = match &args[0].kind {
        ExprKind::List(elems) => {
            let mut params = Vec::new();
            for e in elems {
                match &e.kind {
                    ExprKind::Symbol(s) => params.push(s.clone()),
                    _ => return Err(EvalError::Type("lambda: expected symbol in params".into())),
                }
            }
            params
        }
        _ => return Err(EvalError::Type("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        env: env.clone(),
    })
}

fn eval_and_tail(exprs: &[Expr], env: &Env) -> Result<Trampoline, EvalError> {
    if exprs.is_empty() {
        return Ok(Trampoline::Done(Value::Boolean(true)));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval(expr, env)?;
        if val == Value::Boolean(false) {
            return Ok(Trampoline::Done(Value::Boolean(false)));
        }
    }
    Ok(Trampoline::Bounce(exprs.last().unwrap().clone(), env.clone()))
}

fn eval_or_tail(exprs: &[Expr], env: &Env) -> Result<Trampoline, EvalError> {
    if exprs.is_empty() {
        return Ok(Trampoline::Done(Value::Boolean(false)));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval(expr, env)?;
        if val != Value::Boolean(false) {
            return Ok(Trampoline::Done(val));
        }
    }
    Ok(Trampoline::Bounce(exprs.last().unwrap().clone(), env.clone()))
}

fn eval_let_tail(args: &[Expr], env: &Env) -> Result<Trampoline, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("let requires bindings and body".into()));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 3 {
            return Err(EvalError::Arity("named let requires bindings and body".into()));
        }
        let bindings = match &args[1].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Type("let: expected bindings list".into())),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for binding in bindings {
            match &binding.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env)?);
                    } else {
                        return Err(EvalError::Type("let: expected symbol in binding".into()));
                    }
                }
                _ => return Err(EvalError::Type("let: invalid binding".into())),
            }
        }
        let body = args[2..].to_vec();
        let local_env = new_env(Some(env.clone()));
        let lambda = Value::Lambda {
            params: params.clone(),
            body: body.clone(),
            env: local_env.clone(),
        };
        env_set(&local_env, name.clone(), lambda);
        for (param, init) in params.iter().zip(&inits) {
            env_set(&local_env, param.clone(), init.clone());
        }
        return eval_body_tail(&body, &local_env);
    }
    // Regular let: (let ((var init) ...) body ...)
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Type("let: expected bindings list".into())),
    };
    let local_env = new_env(Some(env.clone()));
    for binding in bindings {
        match &binding.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], env)?;
                    env_set(&local_env, s.clone(), val);
                } else {
                    return Err(EvalError::Type("let: expected symbol in binding".into()));
                }
            }
            _ => return Err(EvalError::Type("let: invalid binding".into())),
        }
    }
    eval_body_tail(&args[1..], &local_env)
}

/// Evaluate a body (sequence of exprs) with TCO on the last one.
fn eval_body_tail(body: &[Expr], env: &Env) -> Result<Trampoline, EvalError> {
    if body.is_empty() {
        return Ok(Trampoline::Done(Value::Void));
    }
    for expr in &body[..body.len() - 1] {
        eval(expr, env)?;
    }
    Ok(Trampoline::Bounce(body.last().unwrap().clone(), env.clone()))
}

fn eval_begin_tail(args: &[Expr], env: &Env) -> Result<Trampoline, EvalError> {
    eval_body_tail(args, env)
}

fn eval_cond_tail(clauses: &[Expr], env: &Env) -> Result<Trampoline, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => {
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        return eval_body_tail(&parts[1..], env);
                    }
                }
                let test = eval(&parts[0], env)?;
                if test != Value::Boolean(false) {
                    if parts.len() == 1 {
                        return Ok(Trampoline::Done(test));
                    }
                    return eval_body_tail(&parts[1..], env);
                }
            }
            _ => return Err(EvalError::Type("cond: invalid clause".into())),
        }
    }
    Ok(Trampoline::Done(Value::Void))
}

fn apply_tail(func: &Value, args: &[Value]) -> Result<Trampoline, EvalError> {
    match func {
        Value::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let local_env = new_env(Some(env.clone()));
            for (param, arg) in params.iter().zip(args) {
                env_set(&local_env, param.clone(), arg.clone());
            }
            eval_body_tail(body, &local_env)
        }
        Value::Builtin(_, func) => func(args).map(Trampoline::Done),
        _ => Err(EvalError::Type(format!("not a procedure: {func}"))),
    }
}

// ── Builtins ──

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("not expects 1 argument".into()));
    }
    Ok(Value::Boolean(args[0] == Value::Boolean(false)))
}

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("cons expects 2 arguments".into()));
    }
    match &args[1] {
        Value::List(elems) => {
            let mut new = vec![args[0].clone()];
            new.extend(elems.iter().cloned());
            Ok(Value::List(new))
        }
        Value::Pair(..) => Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone()))),
        _ => Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone()))),
    }
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car expects 1 argument".into()));
    }
    match &args[0] {
        Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
        Value::Pair(a, _) => Ok(*a.clone()),
        _ => Err(EvalError::Type("car: expected pair".into())),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr expects 1 argument".into()));
    }
    match &args[0] {
        Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
        Value::Pair(_, b) => Ok(*b.clone()),
        _ => Err(EvalError::Type("cdr: expected pair".into())),
    }
}

fn builtin_null(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("null? expects 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(&args[0], Value::List(l) if l.is_empty())))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("length expects 1 argument".into()));
    }
    match &args[0] {
        Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
        _ => Err(EvalError::Type("length: expected list".into())),
    }
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for arg in args {
        match arg {
            Value::List(elems) => result.extend(elems.iter().cloned()),
            _ => return Err(EvalError::Type("append: expected list".into())),
        }
    }
    Ok(Value::List(result))
}

fn builtin_type_pred(args: &[Value], name: &str) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("{name} expects 1 argument")));
    }
    let result = match name {
        "string?" => matches!(&args[0], Value::String(_)),
        "number?" => matches!(&args[0], Value::Integer(_)),
        "boolean?" => matches!(&args[0], Value::Boolean(_)),
        "pair?" => matches!(&args[0], Value::List(l) if !l.is_empty()) || matches!(&args[0], Value::Pair(..)),
        "symbol?" => matches!(&args[0], Value::Symbol(_)),
        "char?" => matches!(&args[0], Value::Char(_)),
        _ => false,
    };
    Ok(Value::Boolean(result))
}

fn require_ints(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|v| match v {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!("expected integer, got {v}"))),
        })
        .collect()
}

fn arith_add(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    Ok(Value::Integer(nums.iter().sum()))
}

fn arith_sub(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    if nums.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    if nums.len() == 1 {
        Ok(Value::Integer(-nums[0]))
    } else {
        Ok(Value::Integer(nums[0] - nums[1..].iter().sum::<i64>()))
    }
}

fn arith_mul(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    Ok(Value::Integer(nums.iter().product()))
}

fn arith_div(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    if nums.len() < 2 {
        return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
    }
    let mut result = nums[0];
    for &d in &nums[1..] {
        if d == 0 {
            return Err(EvalError::Type("division by zero".into()));
        }
        result /= d;
    }
    Ok(Value::Integer(result))
}

fn cmp_op(args: &[Value], op: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    if nums.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let result = nums.windows(2).all(|w| op(w[0], w[1]));
    Ok(Value::Boolean(result))
}

fn builtin_display(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("display expects 1 argument".into()));
    }
    let mut s = String::new();
    args[0].display_fmt(&mut s);
    output_write(&s);
    Ok(Value::Void)
}

fn builtin_write(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("write expects 1 argument".into()));
    }
    output_write(&args[0].to_string());
    Ok(Value::Void)
}

fn builtin_newline(args: &[Value]) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::Arity("newline expects 0 arguments".into()));
    }
    output_write("\n");
    Ok(Value::Void)
}

fn builtin_string_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = std::string::String::new();
    for arg in args {
        match arg {
            Value::String(s) => result.push_str(&s.borrow()),
            _ => return Err(EvalError::Type("string-append: expected string".into())),
        }
    }
    Ok(make_string(result))
}

fn builtin_string_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-length expects 1 argument".into()));
    }
    match &args[0] {
        Value::String(s) => Ok(Value::Integer(s.borrow().len() as i64)),
        _ => Err(EvalError::Type("string-length: expected string".into())),
    }
}

fn builtin_substring(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("substring expects 3 arguments".into()));
    }
    match (&args[0], &args[1], &args[2]) {
        (Value::String(s), Value::Integer(start), Value::Integer(end)) => {
            let s = s.borrow();
            let start = *start as usize;
            let end = *end as usize;
            if start > end || end > s.len() {
                return Err(EvalError::Type("substring: index out of range".into()));
            }
            Ok(make_string(s[start..end].to_string()))
        }
        _ => Err(EvalError::Type("substring: expected string and two integers".into())),
    }
}

fn builtin_string_to_number(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->number expects 1 argument".into()));
    }
    match &args[0] {
        Value::String(s) => match s.borrow().parse::<i64>() {
            Ok(n) => Ok(Value::Integer(n)),
            Err(_) => Ok(Value::Boolean(false)),
        },
        _ => Err(EvalError::Type("string->number: expected string".into())),
    }
}

fn builtin_number_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("number->string expects 1 argument".into()));
    }
    match &args[0] {
        Value::Integer(n) => Ok(make_string(n.to_string())),
        _ => Err(EvalError::Type("number->string: expected number".into())),
    }
}

fn builtin_symbol_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("symbol->string expects 1 argument".into()));
    }
    match &args[0] {
        Value::Symbol(s) => Ok(make_string(s.clone())),
        _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
    }
}

fn builtin_string_to_symbol(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->symbol expects 1 argument".into()));
    }
    match &args[0] {
        Value::String(s) => Ok(Value::Symbol(s.borrow().clone())),
        _ => Err(EvalError::Type("string->symbol: expected string".into())),
    }
}

fn builtin_string_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("string-ref expects 2 arguments".into()));
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::Integer(idx)) => {
            let idx = *idx as usize;
            s.borrow().chars().nth(idx)
                .map(Value::Char)
                .ok_or_else(|| EvalError::Type("string-ref: index out of range".into()))
        }
        _ => Err(EvalError::Type("string-ref: expected string and integer".into())),
    }
}

fn builtin_string_copy(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-copy expects 1 argument".into()));
    }
    match &args[0] {
        Value::String(s) => Ok(make_string(s.borrow().clone())),
        _ => Err(EvalError::Type("string-copy: expected string".into())),
    }
}

fn builtin_string_set(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("string-set! expects 3 arguments".into()));
    }
    match (&args[0], &args[1], &args[2]) {
        (Value::String(s), Value::Integer(idx), Value::Char(c)) => {
            let idx = *idx as usize;
            let mut borrowed = s.borrow_mut();
            let len = borrowed.len();
            if idx >= len {
                return Err(EvalError::Type("string-set!: index out of range".into()));
            }
            // Safe for ASCII; for full Unicode we work char-by-char
            let mut chars: Vec<char> = borrowed.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type("string-set!: index out of range".into()));
            }
            chars[idx] = *c;
            *borrowed = chars.into_iter().collect();
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("string-set!: expected string, integer, and char".into())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = default_env();
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &env)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    // Clear any stale output
    output_take();
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = default_env();
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &env)?;
    }
    let output = output_take();
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
