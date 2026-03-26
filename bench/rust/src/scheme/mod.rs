pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}##{}", base, n)
}

#[derive(Debug, Clone, Copy, Default)]
struct Pos {
    line: usize,
    col: usize,
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    Nil,
    Pair(Box<Value>, Box<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
}

thread_local! {
    static OUTPUT: RefCell<String> = RefCell::new(String::new());
}

fn write_output(s: &str) {
    OUTPUT.with(|out| out.borrow_mut().push_str(s));
}

fn display_value(val: &Value) -> String {
    match val {
        Value::Str(s) => s.clone(),
        Value::Char(c) => c.to_string(),
        other => other.to_string(),
    }
}

type Env = Rc<RefCell<EnvInner>>;

#[derive(Debug)]
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
    {
        let inner = env.borrow();
        if !inner.bindings.contains_key(name) {
            if let Some(ref parent) = inner.parent {
                let parent = parent.clone();
                drop(inner);
                env_set_existing(&parent, name, val);
                return;
            }
            return;
        }
    }
    env.borrow_mut().bindings.insert(name.to_string(), val);
}

fn default_env() -> Env {
    let env = new_env(None);
    env
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Char(c) => match c {
                ' ' => write!(f, "#\\space"),
                '\n' => write!(f, "#\\newline"),
                '\t' => write!(f, "#\\tab"),
                c => write!(f, "#\\{}", c),
            },
            Value::Symbol(s) => write!(f, "{}", s),
            Value::Nil => write!(f, "()"),
            Value::Pair(_, _) => {
                write!(f, "(")?;
                let mut cur = self;
                let mut first = true;
                loop {
                    match cur {
                        Value::Pair(car, cdr) => {
                            if !first {
                                write!(f, " ")?;
                            }
                            first = false;
                            write!(f, "{}", car)?;
                            cur = cdr;
                        }
                        Value::Nil => break,
                        other => {
                            write!(f, " . {}", other)?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
        }
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_string_at(&self, pos: Pos) -> Result<&str, EvalError> {
        match self {
            Value::Str(s) => Ok(s.as_str()),
            _ => Err(EvalError::Type(format!(
                "expected string, got {} at {}",
                self, pos
            ))),
        }
    }

    fn as_integer_at(&self, pos: Pos) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!(
                "expected number, got {} at {}",
                self, pos
            ))),
        }
    }
}

/// Convert a Rust Vec of Values into a proper Scheme list (cons chain ending in Nil).
fn vec_to_list(vals: Vec<Value>) -> Value {
    let mut result = Value::Nil;
    for v in vals.into_iter().rev() {
        result = Value::Pair(Box::new(v), Box::new(result));
    }
    result
}

// --- Parser ---

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64, Pos),
    Boolean(bool, Pos),
    Str(String, Pos),
    Char(char, Pos),
    Symbol(String, Pos),
    List(Vec<Expr>, Pos),
}

impl Expr {
    fn pos(&self) -> Pos {
        match self {
            Expr::Integer(_, p) => *p,
            Expr::Boolean(_, p) => *p,
            Expr::Str(_, p) => *p,
            Expr::Char(_, p) => *p,
            Expr::Symbol(_, p) => *p,
            Expr::List(_, p) => *p,
        }
    }
}

struct Parser {
    tokens: Vec<(String, Pos)>,
    pos: usize,
}

fn tokenize(input: &str) -> Vec<(String, Pos)> {
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
            '(' => {
                tokens.push(("(".into(), Pos { line, col }));
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push((")".into(), Pos { line, col }));
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push(("'".into(), Pos { line, col }));
                i += 1;
                col += 1;
            }
            '"' => {
                let start_pos = Pos { line, col };
                let mut s = String::from("\"");
                i += 1;
                col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        if chars[i + 1] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 2;
                        }
                        i += 2;
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
                tokens.push((s, start_pos));
            }
            _ => {
                let start_pos = Pos { line, col };
                let mut tok = String::new();
                while i < chars.len()
                    && !matches!(
                        chars[i],
                        ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';' | '\''
                    )
                {
                    tok.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push((tok, start_pos));
            }
        }
    }
    tokens
}

impl Parser {
    fn new(input: &str) -> Self {
        Parser {
            tokens: tokenize(input),
            pos: 0,
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        if self.pos >= self.tokens.len() {
            return Err(EvalError::Parse("unexpected end of input".into()));
        }
        let (tok, tpos) = self.tokens[self.pos].clone();
        self.pos += 1;

        if tok == "'" {
            let inner = self.parse_expr()?;
            Ok(Expr::List(
                vec![Expr::Symbol("quote".into(), tpos), inner],
                tpos,
            ))
        } else if tok == "(" {
            let list_pos = tpos;
            let mut elems = Vec::new();
            while self.pos < self.tokens.len() && self.tokens[self.pos].0 != ")" {
                elems.push(self.parse_expr()?);
            }
            if self.pos >= self.tokens.len() {
                return Err(EvalError::Parse(format!(
                    "missing closing paren at {}",
                    list_pos
                )));
            }
            self.pos += 1; // skip ')'
            Ok(Expr::List(elems, list_pos))
        } else if tok == ")" {
            Err(EvalError::Parse(format!("unexpected ')' at {}", tpos)))
        } else if tok == "#t" {
            Ok(Expr::Boolean(true, tpos))
        } else if tok == "#f" {
            Ok(Expr::Boolean(false, tpos))
        } else if tok.starts_with('"') && tok.ends_with('"') {
            let inner = &tok[1..tok.len() - 1];
            let mut result = String::new();
            let chars: Vec<char> = inner.chars().collect();
            let mut j = 0;
            while j < chars.len() {
                if chars[j] == '\\' && j + 1 < chars.len() {
                    match chars[j + 1] {
                        'n' => result.push('\n'),
                        't' => result.push('\t'),
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        c => {
                            result.push('\\');
                            result.push(c);
                        }
                    }
                    j += 2;
                } else {
                    result.push(chars[j]);
                    j += 1;
                }
            }
            Ok(Expr::Str(result, tpos))
        } else if tok.starts_with("#\\") {
            let ch = match &tok[2..] {
                "space" => ' ',
                "newline" => '\n',
                "tab" => '\t',
                s if s.len() == 1 => s.chars().next().unwrap(),
                _ => return Err(EvalError::Parse(format!("unknown character literal: {} at {}", tok, tpos))),
            };
            Ok(Expr::Char(ch, tpos))
        } else if let Ok(n) = tok.parse::<i64>() {
            Ok(Expr::Integer(n, tpos))
        } else {
            Ok(Expr::Symbol(tok, tpos))
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

// --- Evaluator ---

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
            | "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
            | "number?" | "string?" | "boolean?" | "pair?" | "symbol?" | "char?"
            | "display" | "write" | "newline"
            | "string-append" | "string-length" | "substring"
            | "string->number" | "number->string"
            | "symbol->string" | "string->symbol"
            | "string-ref"
            | "string-copy"
            | "apply"
            | "abs" | "modulo" | "remainder" | "quotient"
            | "min" | "max" | "expt"
            | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
            | "list-ref" | "list-tail" | "list?" | "assoc"
            | "map" | "eq?" | "equal?"
            | "char-alphabetic?" | "char-numeric?"
            | "char-upcase" | "char-downcase"
            | "char=?" | "char<?"
            | "string=?" | "string<?" | "string-ci=?"
            | "string-upcase" | "string-downcase"
    )
}

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let p = expr.pos();
    match expr {
        Expr::Integer(n, _) => Ok(Value::Integer(*n)),
        Expr::Boolean(b, _) => Ok(Value::Boolean(*b)),
        Expr::Str(s, _) => Ok(Value::Str(s.clone())),
        Expr::Char(c, _) => Ok(Value::Char(*c)),
        Expr::Symbol(name, _) => {
            if let Some(val) = env_get(env, name) {
                Ok(val)
            } else if is_builtin(name) {
                Ok(Value::Symbol(name.clone()))
            } else {
                Err(EvalError::UnboundVariable(format!("{} at {}", name, p)))
            }
        }
        Expr::List(elems, _) => {
            if elems.is_empty() {
                return Ok(Value::Nil);
            }
            if let Expr::Symbol(op, _) = &elems[0] {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], env, p),
                    "if" => return eval_if(&elems[1..], env, p),
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity(format!(
                                "quote expects 1 argument at {}",
                                p
                            )));
                        }
                        return Ok(expr_to_value(&elems[1]));
                    }
                    "lambda" => return eval_lambda(&elems[1..], env, p),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "let" => return eval_let(&elems[1..], env, p),
                    "begin" => return eval_begin(&elems[1..], env),
                    "cond" => return eval_cond(&elems[1..], env),
                    "string-set!" => return eval_string_set(&elems[1..], env, p),
                    "set!" => return eval_set(&elems[1..], env, p),
                    "define-syntax" => return eval_define_syntax(&elems[1..], env, p),
                    _ => {
                        if let Some(Value::Macro { literals, rules, def_env }) = env_get(env, op) {
                            return eval_macro(&literals, &rules, &def_env, elems, env, p);
                        }
                    }
                }
            }
            // Function call
            let func = eval(&elems[0], env)?;
            let args: Vec<Value> = elems[1..]
                .iter()
                .map(|e| eval(e, env))
                .collect::<Result<_, _>>()?;
            apply_value(&func, &args, p)
        }
    }
}

fn apply_value(func: &Value, args: &[Value], call_pos: Pos) -> Result<Value, EvalError> {
    match func {
        Value::Symbol(op) => apply_builtin(op, args, call_pos),
        Value::Lambda { params, rest_param, body, env } => {
            if let Some(ref rest) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {} at {}",
                        params.len(),
                        args.len(),
                        call_pos
                    )));
                }
                let local_env = new_env(Some(env.clone()));
                for (p, a) in params.iter().zip(args.iter()) {
                    env_set(&local_env, p.clone(), a.clone());
                }
                let rest_list = vec_to_list(args[params.len()..].to_vec());
                env_set(&local_env, rest.clone(), rest_list);
                let mut result = Value::Boolean(false);
                for expr in body {
                    result = eval(expr, &local_env)?;
                }
                return Ok(result);
            }
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {} at {}",
                    params.len(),
                    args.len(),
                    call_pos
                )));
            }
            let local_env = new_env(Some(env.clone()));
            for (p, a) in params.iter().zip(args.iter()) {
                env_set(&local_env, p.clone(), a.clone());
            }
            let mut result = Value::Boolean(false);
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type(format!(
            "not a procedure: {} at {}",
            func, call_pos
        ))),
    }
}

/// Parse a parameter list, detecting dot notation for rest params.
/// e.g. `(x y . rest)` → (["x", "y"], Some("rest"))
fn parse_params(param_exprs: &[Expr], p: Pos) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i] {
            Expr::Symbol(s, _) if s == "." => {
                if i + 1 >= param_exprs.len() || i + 2 != param_exprs.len() {
                    return Err(EvalError::Parse(format!(
                        "invalid dot notation in parameter list at {}", p
                    )));
                }
                match &param_exprs[i + 1] {
                    Expr::Symbol(rest, _) => rest_param = Some(rest.clone()),
                    _ => return Err(EvalError::Type(format!(
                        "expected symbol after dot in parameter list at {}", p
                    ))),
                }
                break;
            }
            Expr::Symbol(s, _) => params.push(s.clone()),
            _ => return Err(EvalError::Type(format!(
                "expected symbol as parameter at {}", p
            ))),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_define(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!(
            "define requires at least 2 arguments at {}",
            p
        )));
    }
    match &args[0] {
        Expr::Symbol(name, _) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "define requires exactly 2 arguments at {}",
                    p
                )));
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Value::Symbol(name.clone()))
        }
        Expr::List(name_and_params, _) => {
            if name_and_params.is_empty() {
                return Err(EvalError::Parse(format!("define: empty name list at {}", p)));
            }
            let name = match &name_and_params[0] {
                Expr::Symbol(s, _) => s.clone(),
                _ => {
                    return Err(EvalError::Type(format!(
                        "define: expected symbol as function name at {}",
                        p
                    )))
                }
            };
            let (params, rest_param) = parse_params(&name_and_params[1..], p)?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                env: env.clone(),
            };
            env_set(env, name.clone(), lambda);
            Ok(Value::Symbol(name))
        }
        _ => Err(EvalError::Type(format!(
            "define: expected symbol or list at {}",
            p
        ))),
    }
}

fn eval_if(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity(format!(
            "if requires 2 or 3 arguments at {}",
            p
        )));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Boolean(false))
    }
}

fn eval_lambda(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "lambda requires params and body at {}",
            p
        )));
    }
    let (params, rest_param) = match &args[0] {
        Expr::List(param_exprs, _) => parse_params(param_exprs, p)?,
        Expr::Symbol(s, _) => {
            // (lambda args body) — single symbol captures all args
            (vec![], Some(s.clone()))
        }
        _ => {
            return Err(EvalError::Type(format!(
                "lambda: expected parameter list at {}",
                p
            )))
        }
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        env: env.clone(),
    })
}

fn expr_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(n, _) => Value::Integer(*n),
        Expr::Boolean(b, _) => Value::Boolean(*b),
        Expr::Str(s, _) => Value::Str(s.clone()),
        Expr::Char(c, _) => Value::Char(*c),
        Expr::Symbol(s, _) => Value::Symbol(s.clone()),
        Expr::List(elems, _) => {
            if elems.is_empty() {
                Value::Nil
            } else {
                vec_to_list(elems.iter().map(expr_to_value).collect())
            }
        }
    }
}

fn eval_and(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in exprs {
        let result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_let(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "let requires bindings and body at {}",
            p
        )));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let Expr::Symbol(name, _) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::Arity(format!(
                "named let requires bindings and body at {}",
                p
            )));
        }
        let bindings_expr = match &args[1] {
            Expr::List(b, _) => b,
            _ => {
                return Err(EvalError::Type(format!(
                    "let: expected bindings list at {}",
                    p
                )))
            }
        };
        let mut param_names = Vec::new();
        let mut init_exprs = Vec::new();
        for b in bindings_expr {
            match b {
                Expr::List(pair, _) if pair.len() == 2 => {
                    match &pair[0] {
                        Expr::Symbol(s, _) => param_names.push(s.clone()),
                        _ => {
                            return Err(EvalError::Type(format!(
                                "let: expected symbol in binding at {}",
                                p
                            )))
                        }
                    }
                    init_exprs.push(pair[1].clone());
                }
                _ => {
                    return Err(EvalError::Type(format!(
                        "let: invalid binding at {}",
                        p
                    )))
                }
            }
        }
        let body = args[2..].to_vec();
        // Create a new env with the loop function bound
        let loop_env = new_env(Some(env.clone()));
        let lambda = Value::Lambda {
            params: param_names,
            rest_param: None,
            body,
            env: loop_env.clone(),
        };
        env_set(&loop_env, name.clone(), lambda);
        // Evaluate initial values in the outer env
        let init_vals: Vec<Value> = init_exprs
            .iter()
            .map(|e| eval(e, env))
            .collect::<Result<_, _>>()?;
        // Call the loop function
        let func = env_get(&loop_env, name).unwrap();
        return apply_value(&func, &init_vals, p);
    }
    // Regular let: (let ((var init) ...) body ...)
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => {
            return Err(EvalError::Type(format!(
                "let: expected bindings list at {}",
                p
            )))
        }
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                let name = match &pair[0] {
                    Expr::Symbol(s, _) => s.clone(),
                    _ => {
                        return Err(EvalError::Type(format!(
                            "let: expected symbol in binding at {}",
                            p
                        )))
                    }
                };
                let val = eval(&pair[1], env)?;
                env_set(&local_env, name, val);
            }
            _ => {
                return Err(EvalError::Type(format!(
                    "let: invalid binding at {}",
                    p
                )))
            }
        }
    }
    let mut result = Value::Boolean(false);
    for expr in &args[1..] {
        result = eval(expr, &local_env)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &Env) -> Result<Value, EvalError> {
    for clause in clauses {
        match clause {
            Expr::List(parts, _) if !parts.is_empty() => {
                // Check for else clause
                if let Expr::Symbol(s, _) = &parts[0] {
                    if s == "else" {
                        let mut result = Value::Boolean(false);
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
            _ => {
                return Err(EvalError::Type(format!(
                    "cond: invalid clause at {}",
                    clause.pos()
                )))
            }
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_string_set(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!(
            "string-set! requires 3 arguments at {}", p
        )));
    }
    let var_name = match &args[0] {
        Expr::Symbol(name, _) => name.clone(),
        _ => return Err(EvalError::Type(format!(
            "string-set!: first argument must be a variable at {}", p
        ))),
    };
    let idx_val = eval(&args[1], env)?;
    let idx = idx_val.as_integer_at(p)? as usize;
    let char_val = eval(&args[2], env)?;
    let ch = match &char_val {
        Value::Char(c) => *c,
        _ => return Err(EvalError::Type(format!(
            "string-set!: expected char, got {} at {}", char_val, p
        ))),
    };
    // Look up the string, modify it, store back
    let current = env_get(env, &var_name).ok_or_else(|| {
        EvalError::UnboundVariable(format!("{} at {}", var_name, p))
    })?;
    let mut s = match current {
        Value::Str(s) => s,
        _ => return Err(EvalError::Type(format!(
            "string-set!: expected string, got {} at {}", current, p
        ))),
    };
    let chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::Type(format!(
            "string-set!: index out of range at {}", p
        )));
    }
    let mut new_chars = chars;
    new_chars[idx] = ch;
    s = new_chars.into_iter().collect();
    env_set_existing(env, &var_name, Value::Str(s));
    Ok(Value::Nil)
}

fn eval_set(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!(
            "set! requires 2 arguments at {}", p
        )));
    }
    let name = match &args[0] {
        Expr::Symbol(s, _) => s.clone(),
        _ => return Err(EvalError::Type(format!(
            "set!: expected symbol at {}", p
        ))),
    };
    // Check that the variable exists
    if env_get(env, &name).is_none() {
        return Err(EvalError::UnboundVariable(format!("{} at {}", name, p)));
    }
    let val = eval(&args[1], env)?;
    env_set_existing(env, &name, val);
    Ok(Value::Nil)
}

// --- Macros (syntax-rules) ---

fn is_special_form(name: &str) -> bool {
    matches!(
        name,
        "define" | "if" | "quote" | "lambda" | "and" | "or" | "let" | "begin"
            | "cond" | "string-set!" | "set!" | "define-syntax"
    )
}

fn eval_define_syntax(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!(
            "define-syntax requires 2 arguments at {}", p
        )));
    }
    let name = match &args[0] {
        Expr::Symbol(s, _) => s.clone(),
        _ => return Err(EvalError::Type(format!(
            "define-syntax: expected symbol at {}", p
        ))),
    };
    let sr = match &args[1] {
        Expr::List(elems, _) => elems,
        _ => return Err(EvalError::Type(format!(
            "define-syntax: expected syntax-rules at {}", p
        ))),
    };
    if sr.len() < 2 {
        return Err(EvalError::Parse(format!(
            "define-syntax: invalid syntax-rules at {}", p
        )));
    }
    match &sr[0] {
        Expr::Symbol(s, _) if s == "syntax-rules" => {}
        _ => return Err(EvalError::Type(format!(
            "define-syntax: expected syntax-rules at {}", p
        ))),
    }
    let literals = match &sr[1] {
        Expr::List(lits, _) => lits
            .iter()
            .map(|l| match l {
                Expr::Symbol(s, _) => Ok(s.clone()),
                _ => Err(EvalError::Type(format!(
                    "define-syntax: expected symbol in literals at {}", p
                ))),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => return Err(EvalError::Type(format!(
            "define-syntax: expected literals list at {}", p
        ))),
    };
    let mut rules = Vec::new();
    for rule in &sr[2..] {
        match rule {
            Expr::List(parts, _) if parts.len() == 2 => {
                rules.push((parts[0].clone(), parts[1].clone()));
            }
            _ => return Err(EvalError::Type(format!(
                "define-syntax: invalid rule at {}", p
            ))),
        }
    }
    let mac = Value::Macro {
        literals,
        rules,
        def_env: env.clone(),
    };
    env_set(env, name.clone(), mac);
    Ok(Value::Symbol(name))
}

#[derive(Debug, Clone)]
enum PatBinding {
    Single(Expr),
    List(Vec<Expr>),
}

fn match_one(
    pattern: &Expr,
    input: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, PatBinding>,
) -> bool {
    match pattern {
        Expr::Symbol(name, _) if name == "_" => true,
        Expr::Symbol(name, _) if name == "..." => false,
        Expr::Symbol(name, _) if literals.contains(name) => {
            matches!(input, Expr::Symbol(s, _) if s == name)
        }
        Expr::Symbol(name, _) => {
            bindings.insert(name.clone(), PatBinding::Single(input.clone()));
            true
        }
        Expr::List(pelems, _) => match input {
            Expr::List(ielems, _) => match_list(pelems, ielems, literals, bindings),
            _ => false,
        },
        Expr::Integer(n, _) => matches!(input, Expr::Integer(m, _) if m == n),
        Expr::Boolean(b, _) => matches!(input, Expr::Boolean(c, _) if c == b),
        Expr::Str(s, _) => matches!(input, Expr::Str(t, _) if t == s),
        Expr::Char(c, _) => matches!(input, Expr::Char(d, _) if d == c),
    }
}

fn match_list(
    pattern: &[Expr],
    input: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, PatBinding>,
) -> bool {
    let mut pi = 0;
    let mut ii = 0;
    while pi < pattern.len() {
        // Check if current element is followed by ...
        if pi + 1 < pattern.len() {
            if let Expr::Symbol(s, _) = &pattern[pi + 1] {
                if s == "..." {
                    let sub_pattern = &pattern[pi];
                    let var_names = collect_pattern_vars(sub_pattern, literals);
                    let remaining_pat = pattern.len() - pi - 2;
                    let max_match = if input.len() >= ii + remaining_pat {
                        input.len() - ii - remaining_pat
                    } else {
                        return false;
                    };
                    let mut list_bindings: HashMap<String, Vec<Expr>> = HashMap::new();
                    for v in &var_names {
                        list_bindings.insert(v.clone(), Vec::new());
                    }
                    for j in 0..max_match {
                        let mut sub_bindings = HashMap::new();
                        if !match_one(sub_pattern, &input[ii + j], literals, &mut sub_bindings) {
                            return false;
                        }
                        for v in &var_names {
                            if let Some(PatBinding::Single(e)) = sub_bindings.get(v) {
                                list_bindings.get_mut(v).unwrap().push(e.clone());
                            }
                        }
                    }
                    for (v, exprs) in list_bindings {
                        bindings.insert(v, PatBinding::List(exprs));
                    }
                    ii += max_match;
                    pi += 2;
                    continue;
                }
            }
        }
        if ii >= input.len() {
            return false;
        }
        if !match_one(&pattern[pi], &input[ii], literals, bindings) {
            return false;
        }
        pi += 1;
        ii += 1;
    }
    pi == pattern.len() && ii == input.len()
}

fn collect_pattern_vars(pattern: &Expr, literals: &[String]) -> Vec<String> {
    let mut vars = Vec::new();
    collect_pvars_inner(pattern, literals, &mut vars);
    vars
}

fn collect_pvars_inner(pattern: &Expr, literals: &[String], vars: &mut Vec<String>) {
    match pattern {
        Expr::Symbol(name, _)
            if name != "..." && name != "_" && !literals.contains(name) =>
        {
            vars.push(name.clone());
        }
        Expr::List(elems, _) => {
            for e in elems {
                collect_pvars_inner(e, literals, vars);
            }
        }
        _ => {}
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, PatBinding>,
    gensym_map: &HashMap<String, String>,
) -> Expr {
    match template {
        Expr::Symbol(name, pos) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    PatBinding::Single(e) => e.clone(),
                    PatBinding::List(_) => template.clone(),
                }
            } else if let Some(gname) = gensym_map.get(name) {
                Expr::Symbol(gname.clone(), *pos)
            } else {
                template.clone()
            }
        }
        Expr::List(elems, pos) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() {
                    if let Expr::Symbol(s, _) = &elems[i + 1] {
                        if s == "..." {
                            let sub = &elems[i];
                            let evars = find_ellipsis_vars(sub, bindings);
                            if let Some(first_var) = evars.first() {
                                if let Some(PatBinding::List(items)) = bindings.get(first_var) {
                                    let count = items.len();
                                    for j in 0..count {
                                        let mut sub_bindings = bindings.clone();
                                        for v in &evars {
                                            if let Some(PatBinding::List(vitems)) = bindings.get(v)
                                            {
                                                sub_bindings.insert(
                                                    v.clone(),
                                                    PatBinding::Single(vitems[j].clone()),
                                                );
                                            }
                                        }
                                        result.push(expand_template(sub, &sub_bindings, gensym_map));
                                    }
                                }
                            }
                            i += 2;
                            continue;
                        }
                    }
                }
                result.push(expand_template(&elems[i], bindings, gensym_map));
                i += 1;
            }
            Expr::List(result, *pos)
        }
        _ => template.clone(),
    }
}

fn find_ellipsis_vars(template: &Expr, bindings: &HashMap<String, PatBinding>) -> Vec<String> {
    let mut vars = Vec::new();
    find_evars_inner(template, bindings, &mut vars);
    vars
}

fn find_evars_inner(
    template: &Expr,
    bindings: &HashMap<String, PatBinding>,
    vars: &mut Vec<String>,
) {
    match template {
        Expr::Symbol(name, _) => {
            if matches!(bindings.get(name), Some(PatBinding::List(_))) && !vars.contains(name) {
                vars.push(name.clone());
            }
        }
        Expr::List(elems, _) => {
            for e in elems {
                find_evars_inner(e, bindings, vars);
            }
        }
        _ => {}
    }
}

fn collect_template_symbols(
    template: &Expr,
    pat_vars: &[String],
    gensym_map: &mut HashMap<String, String>,
) {
    match template {
        Expr::Symbol(name, _) => {
            if !pat_vars.contains(name)
                && !is_special_form(name)
                && !is_builtin(name)
                && name != "..."
                && !gensym_map.contains_key(name)
            {
                gensym_map.insert(name.clone(), gensym(name));
            }
        }
        Expr::List(elems, _) => {
            for e in elems {
                collect_template_symbols(e, pat_vars, gensym_map);
            }
        }
        _ => {}
    }
}

fn eval_macro(
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &Env,
    input: &[Expr],
    env: &Env,
    p: Pos,
) -> Result<Value, EvalError> {
    for (pattern, template) in rules {
        let mut bindings = HashMap::new();
        if let Expr::List(pat_elems, _) = pattern {
            if match_list(&pat_elems[1..], &input[1..], literals, &mut bindings) {
                let pat_vars: Vec<String> = bindings.keys().cloned().collect();
                let mut gensym_map = HashMap::new();
                collect_template_symbols(template, &pat_vars, &mut gensym_map);

                let expanded = expand_template(template, &bindings, &gensym_map);

                let wrapper_env = new_env(Some(env.clone()));
                for (orig, gsym) in &gensym_map {
                    if let Some(val) = env_get(def_env, orig) {
                        env_set(&wrapper_env, gsym.clone(), val);
                    }
                }

                return eval(&expanded, &wrapper_env);
            }
        }
    }
    Err(EvalError::Type(format!(
        "no matching macro pattern at {}", p
    )))
}

fn apply_builtin(op: &str, args: &[Value], p: Pos) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += a.as_integer_at(p)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "- requires at least 1 argument at {}",
                    p
                )));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-args[0].as_integer_at(p)?));
            }
            let mut result = args[0].as_integer_at(p)?;
            for a in &args[1..] {
                result -= a.as_integer_at(p)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= a.as_integer_at(p)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "/ requires at least 1 argument at {}",
                    p
                )));
            }
            if args.len() == 1 {
                let d = args[0].as_integer_at(p)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero(format!("{}", p)));
                }
                return Ok(Value::Integer(1 / d));
            }
            let mut result = args[0].as_integer_at(p)?;
            for a in &args[1..] {
                let d = a.as_integer_at(p)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero(format!("{}", p)));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(
                args[0].as_integer_at(p)? < args[1].as_integer_at(p)?,
            ))
        }
        ">" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(
                args[0].as_integer_at(p)? > args[1].as_integer_at(p)?,
            ))
        }
        "=" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(
                args[0].as_integer_at(p)? == args[1].as_integer_at(p)?,
            ))
        }
        "<=" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(
                args[0].as_integer_at(p)? <= args[1].as_integer_at(p)?,
            ))
        }
        ">=" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(
                args[0].as_integer_at(p)? >= args[1].as_integer_at(p)?,
            ))
        }
        "not" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "cons" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Pair(
                Box::new(args[0].clone()),
                Box::new(args[1].clone()),
            ))
        }
        "car" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Pair(car, _) => Ok(*car.clone()),
                _ => Err(EvalError::Type(format!(
                    "car: expected pair, got {} at {}",
                    args[0], p
                ))),
            }
        }
        "cdr" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Pair(_, cdr) => Ok(*cdr.clone()),
                _ => Err(EvalError::Type(format!(
                    "cdr: expected pair, got {} at {}",
                    args[0], p
                ))),
            }
        }
        "null?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Nil)))
        }
        "list" => Ok(vec_to_list(args.to_vec())),
        "length" => {
            ensure_args(op, args, 1, p)?;
            let mut count = 0i64;
            let mut cur = &args[0];
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(_, cdr) => {
                        count += 1;
                        cur = cdr;
                    }
                    _ => {
                        return Err(EvalError::Type(format!(
                            "length: expected list at {}",
                            p
                        )))
                    }
                }
            }
            Ok(Value::Integer(count))
        }
        "append" => {
            if args.is_empty() {
                return Ok(Value::Nil);
            }
            // Append all lists together
            let mut result = args.last().unwrap().clone();
            for arg in args[..args.len() - 1].iter().rev() {
                let mut elems = Vec::new();
                let mut cur = arg;
                loop {
                    match cur {
                        Value::Nil => break,
                        Value::Pair(car, cdr) => {
                            elems.push(*car.clone());
                            cur = cdr;
                        }
                        _ => {
                            return Err(EvalError::Type(format!(
                                "append: expected list at {}",
                                p
                            )))
                        }
                    }
                }
                for e in elems.into_iter().rev() {
                    result = Value::Pair(Box::new(e), Box::new(result));
                }
            }
            Ok(result)
        }
        "number?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
        }
        "string?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Str(_))))
        }
        "boolean?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
        }
        "pair?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Pair(_, _))))
        }
        "symbol?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
        }
        "char?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Char(_))))
        }
        "display" => {
            ensure_args(op, args, 1, p)?;
            write_output(&display_value(&args[0]));
            Ok(Value::Nil)
        }
        "write" => {
            ensure_args(op, args, 1, p)?;
            write_output(&args[0].to_string());
            Ok(Value::Nil)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "newline expects 0 arguments, got {} at {}",
                    args.len(), p
                )));
            }
            write_output("\n");
            Ok(Value::Nil)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                result.push_str(a.as_string_at(p)?);
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            ensure_args(op, args, 1, p)?;
            let s = args[0].as_string_at(p)?;
            Ok(Value::Integer(s.len() as i64))
        }
        "substring" => {
            ensure_args(op, args, 3, p)?;
            let s = args[0].as_string_at(p)?;
            let start = args[1].as_integer_at(p)? as usize;
            let end = args[2].as_integer_at(p)? as usize;
            if start > end || end > s.len() {
                return Err(EvalError::Type(format!(
                    "substring: index out of range at {}", p
                )));
            }
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            ensure_args(op, args, 1, p)?;
            let s = args[0].as_string_at(p)?;
            match s.parse::<i64>() {
                Ok(n) => Ok(Value::Integer(n)),
                Err(_) => Ok(Value::Boolean(false)),
            }
        }
        "number->string" => {
            ensure_args(op, args, 1, p)?;
            let n = args[0].as_integer_at(p)?;
            Ok(Value::Str(n.to_string()))
        }
        "symbol->string" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!(
                    "symbol->string: expected symbol, got {} at {}", args[0], p
                ))),
            }
        }
        "string->symbol" => {
            ensure_args(op, args, 1, p)?;
            let s = args[0].as_string_at(p)?;
            Ok(Value::Symbol(s.to_string()))
        }
        "string-ref" => {
            ensure_args(op, args, 2, p)?;
            let s = args[0].as_string_at(p)?;
            let idx = args[1].as_integer_at(p)? as usize;
            if idx >= s.len() {
                return Err(EvalError::Type(format!(
                    "string-ref: index out of range at {}", p
                )));
            }
            Ok(Value::Char(s.chars().nth(idx).unwrap()))
        }
        "string-copy" => {
            ensure_args(op, args, 1, p)?;
            let s = args[0].as_string_at(p)?;
            Ok(Value::Str(s.to_string()))
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!(
                    "apply requires at least 2 arguments at {}", p
                )));
            }
            let func = &args[0];
            // Last argument must be a list; prefix args come before it
            let last = &args[args.len() - 1];
            let mut call_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            // Flatten the last argument (a list) into call_args
            let mut cur = last;
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(car, cdr) => {
                        call_args.push(*car.clone());
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type(format!(
                        "apply: last argument must be a list at {}", p
                    ))),
                }
            }
            apply_value(func, &call_args, p)
        }
        "abs" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Integer(args[0].as_integer_at(p)?.abs()))
        }
        "modulo" => {
            ensure_args(op, args, 2, p)?;
            let a = args[0].as_integer_at(p)?;
            let b = args[1].as_integer_at(p)?;
            if b == 0 { return Err(EvalError::DivisionByZero(format!("{}", p))); }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            ensure_args(op, args, 2, p)?;
            let a = args[0].as_integer_at(p)?;
            let b = args[1].as_integer_at(p)?;
            if b == 0 { return Err(EvalError::DivisionByZero(format!("{}", p))); }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            ensure_args(op, args, 2, p)?;
            let a = args[0].as_integer_at(p)?;
            let b = args[1].as_integer_at(p)?;
            if b == 0 { return Err(EvalError::DivisionByZero(format!("{}", p))); }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("min requires at least 1 argument at {}", p)));
            }
            let mut m = args[0].as_integer_at(p)?;
            for a in &args[1..] { m = m.min(a.as_integer_at(p)?); }
            Ok(Value::Integer(m))
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("max requires at least 1 argument at {}", p)));
            }
            let mut m = args[0].as_integer_at(p)?;
            for a in &args[1..] { m = m.max(a.as_integer_at(p)?); }
            Ok(Value::Integer(m))
        }
        "expt" => {
            ensure_args(op, args, 2, p)?;
            let base = args[0].as_integer_at(p)?;
            let exp = args[1].as_integer_at(p)?;
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(args[0].as_integer_at(p)? == 0))
        }
        "positive?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(args[0].as_integer_at(p)? > 0))
        }
        "negative?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(args[0].as_integer_at(p)? < 0))
        }
        "odd?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(args[0].as_integer_at(p)? % 2 != 0))
        }
        "even?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(args[0].as_integer_at(p)? % 2 == 0))
        }
        "list-ref" => {
            ensure_args(op, args, 2, p)?;
            let idx = args[1].as_integer_at(p)? as usize;
            let mut cur = &args[0];
            for _ in 0..idx {
                match cur {
                    Value::Pair(_, cdr) => cur = cdr,
                    _ => return Err(EvalError::Type(format!("list-ref: index out of range at {}", p))),
                }
            }
            match cur {
                Value::Pair(car, _) => Ok(*car.clone()),
                _ => Err(EvalError::Type(format!("list-ref: index out of range at {}", p))),
            }
        }
        "list-tail" => {
            ensure_args(op, args, 2, p)?;
            let idx = args[1].as_integer_at(p)? as usize;
            let mut cur = args[0].clone();
            for _ in 0..idx {
                match cur {
                    Value::Pair(_, cdr) => cur = *cdr,
                    _ => return Err(EvalError::Type(format!("list-tail: index out of range at {}", p))),
                }
            }
            Ok(cur)
        }
        "list?" => {
            ensure_args(op, args, 1, p)?;
            let mut cur = &args[0];
            let result = loop {
                match cur {
                    Value::Nil => break true,
                    Value::Pair(_, cdr) => cur = cdr,
                    _ => break false,
                }
            };
            Ok(Value::Boolean(result))
        }
        "assoc" => {
            ensure_args(op, args, 2, p)?;
            let key = &args[0];
            let mut cur = &args[1];
            loop {
                match cur {
                    Value::Nil => return Ok(Value::Boolean(false)),
                    Value::Pair(car, cdr) => {
                        if let Value::Pair(k, _) = car.as_ref() {
                            if values_equal(k, key) {
                                return Ok(*car.clone());
                            }
                        }
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type(format!("assoc: expected list at {}", p))),
                }
            }
        }
        "eq?" => {
            ensure_args(op, args, 2, p)?;
            let result = match (&args[0], &args[1]) {
                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                (Value::Integer(a), Value::Integer(b)) => a == b,
                (Value::Boolean(a), Value::Boolean(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::Nil, Value::Nil) => true,
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "equal?" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        "map" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("map requires at least 2 arguments at {}", p)));
            }
            let func = &args[0];
            let mut current_lists: Vec<Value> = args[1..].to_vec();
            let mut results = Vec::new();
            loop {
                let all_pairs = current_lists.iter().all(|l| matches!(l, Value::Pair(_, _)));
                if !all_pairs { break; }
                let mut call_args = Vec::new();
                let mut next_lists = Vec::new();
                for list in &current_lists {
                    match list {
                        Value::Pair(car, cdr) => {
                            call_args.push(*car.clone());
                            next_lists.push(*cdr.clone());
                        }
                        _ => unreachable!(),
                    }
                }
                results.push(apply_value(func, &call_args, p)?);
                current_lists = next_lists;
            }
            Ok(vec_to_list(results))
        }
        "char-alphabetic?" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(EvalError::Type(format!("char-alphabetic?: expected char at {}", p))),
            }
        }
        "char-numeric?" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(EvalError::Type(format!("char-numeric?: expected char at {}", p))),
            }
        }
        "char-upcase" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => Err(EvalError::Type(format!("char-upcase: expected char at {}", p))),
            }
        }
        "char-downcase" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                _ => Err(EvalError::Type(format!("char-downcase: expected char at {}", p))),
            }
        }
        "char=?" => {
            ensure_args(op, args, 2, p)?;
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(format!("char=?: expected chars at {}", p))),
            }
        }
        "char<?" => {
            ensure_args(op, args, 2, p)?;
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(format!("char<?: expected chars at {}", p))),
            }
        }
        "string=?" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(args[0].as_string_at(p)? == args[1].as_string_at(p)?))
        }
        "string<?" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(args[0].as_string_at(p)? < args[1].as_string_at(p)?))
        }
        "string-ci=?" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(
                args[0].as_string_at(p)?.to_lowercase() == args[1].as_string_at(p)?.to_lowercase()
            ))
        }
        "string-upcase" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Str(args[0].as_string_at(p)?.to_uppercase()))
        }
        "string-downcase" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Str(args[0].as_string_at(p)?.to_lowercase()))
        }
        _ => Err(EvalError::UnboundVariable(format!("{} at {}", op, p))),
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => {
            values_equal(a1, b1) && values_equal(a2, b2)
        }
        _ => false,
    }
}

fn ensure_args(op: &str, args: &[Value], expected: usize, p: Pos) -> Result<(), EvalError> {
    if args.len() != expected {
        return Err(EvalError::Arity(format!(
            "{} expects {} arguments, got {} at {}",
            op, expected,
            args.len(),
            p
        )));
    }
    Ok(())
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = default_env();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &env)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    OUTPUT.with(|out| out.borrow_mut().clear());
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = default_env();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &env)?;
    }
    let output = OUTPUT.with(|out| out.borrow().clone());
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
