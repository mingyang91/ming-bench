pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Char(char),
    Builtin(String),
    Void,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
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
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::Builtin(name) => write!(f, "#<builtin:{name}>"),
            Value::Void => write!(f, ""),
        }
    }
}

impl Value {
    /// Format for `display` — strings without quotes.
    fn display_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Str(s) => write!(f, "{s}"),
            Value::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    e.display_fmt(f)?;
                }
                write!(f, ")")
            }
            other => fmt::Display::fmt(other, f),
        }
    }
}

struct DisplayValue<'a>(&'a Value);
impl<'a> fmt::Display for DisplayValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.display_fmt(f)
    }
}

// ---------- Environment ----------

type Env = Rc<RefCell<EnvInner>>;

struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl fmt::Debug for EnvInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Env")
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

fn default_env() -> Env {
    let env = new_env(None);
    for &name in BUILTINS {
        env_set(&env, name.to_string(), Value::Builtin(name.to_string()));
    }
    env
}

// ---------- Source Position ----------

#[derive(Debug, Clone, Copy, PartialEq)]
struct Pos {
    line: usize,
    col: usize,
}

impl Pos {
    fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// ---------- Tokenizer ----------

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Quote,
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    Str(String),
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
                tokens.push(SpannedToken { token: Token::LParen, pos: Pos::new(line, col) });
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push(SpannedToken { token: Token::RParen, pos: Pos::new(line, col) });
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push(SpannedToken { token: Token::Quote, pos: Pos::new(line, col) });
                i += 1;
                col += 1;
            }
            '"' => {
                let start_pos = Pos::new(line, col);
                i += 1;
                col += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1;
                        col += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            c => {
                                s.push('\\');
                                s.push(c);
                            }
                        }
                    } else {
                        if chars[i] == '\n' {
                            line += 1;
                            col = 0;
                        }
                        s.push(chars[i]);
                    }
                    i += 1;
                    col += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse(format!("{start_pos}: unterminated string")));
                }
                i += 1;
                col += 1;
                tokens.push(SpannedToken { token: Token::Str(s), pos: start_pos });
            }
            '#' => {
                let start_pos = Pos::new(line, col);
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push(SpannedToken { token: Token::Boolean(true), pos: start_pos });
                            i += 2;
                            col += 2;
                        }
                        'f' => {
                            tokens.push(SpannedToken { token: Token::Boolean(false), pos: start_pos });
                            i += 2;
                            col += 2;
                        }
                        _ => {
                            return Err(EvalError::Parse(format!(
                                "{start_pos}: unexpected character after #: {}",
                                chars[i + 1]
                            )));
                        }
                    }
                } else {
                    return Err(EvalError::Parse(format!("{start_pos}: unexpected #")));
                }
            }
            _ => {
                let start_pos = Pos::new(line, col);
                let start = i;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"' | '\'')
                {
                    i += 1;
                    col += 1;
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

// ---------- Parser ----------

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64, Pos),
    Boolean(bool, Pos),
    Str(String, Pos),
    Symbol(String, Pos),
    List(Vec<Expr>, Pos),
}

impl Expr {
    fn pos(&self) -> Pos {
        match self {
            Expr::Integer(_, p)
            | Expr::Boolean(_, p)
            | Expr::Str(_, p)
            | Expr::Symbol(_, p)
            | Expr::List(_, p) => *p,
        }
    }
}

fn parse(tokens: &[SpannedToken], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let st = &tokens[*pos];
    let src_pos = st.pos;
    match &st.token {
        Token::Integer(n) => {
            let n = *n;
            *pos += 1;
            Ok(Expr::Integer(n, src_pos))
        }
        Token::Boolean(b) => {
            let b = *b;
            *pos += 1;
            Ok(Expr::Boolean(b, src_pos))
        }
        Token::Str(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::Str(s, src_pos))
        }
        Token::Symbol(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::Symbol(s, src_pos))
        }
        Token::Quote => {
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Expr::List(vec![Expr::Symbol("quote".into(), src_pos), inner], src_pos))
        }
        Token::LParen => {
            *pos += 1;
            let mut elems = Vec::new();
            while *pos < tokens.len() && tokens[*pos].token != Token::RParen {
                elems.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse(format!("{src_pos}: missing closing paren")));
            }
            *pos += 1;
            Ok(Expr::List(elems, src_pos))
        }
        Token::RParen => Err(EvalError::Parse(format!("{src_pos}: unexpected )"))),
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ---------- Evaluator ----------

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

const BUILTINS: &[&str] = &[
    "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
    "cons", "car", "cdr", "null?", "list", "length", "append",
    "number?", "boolean?", "string?", "pair?", "symbol?",
    "display", "write", "newline",
    "string-append", "string-length", "substring",
    "string->number", "number->string",
    "symbol->string", "string->symbol",
    "string-ref", "char?",
];

fn eval(expr: &Expr, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let p = expr.pos();
    match expr {
        Expr::Integer(n, _) => Ok(Value::Integer(*n)),
        Expr::Boolean(b, _) => Ok(Value::Boolean(*b)),
        Expr::Str(s, _) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name, _) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable(format!("{p}: {name}")))
        }
        Expr::List(elems, _) => {
            if elems.is_empty() {
                return Ok(Value::List(vec![]));
            }
            // Check for special forms
            if let Expr::Symbol(op, _) = &elems[0] {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], p, env, output),
                    "if" => return eval_if(&elems[1..], p, env, output),
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity(format!("{p}: quote expects 1 argument")));
                        }
                        return Ok(expr_to_value(&elems[1]));
                    }
                    "lambda" => return eval_lambda(&elems[1..], p, env),
                    "let" => return eval_let(&elems[1..], p, env, output),
                    "begin" => return eval_begin(&elems[1..], env, output),
                    "cond" => return eval_cond(&elems[1..], env, output),
                    "and" => return eval_and(&elems[1..], env, output),
                    "or" => return eval_or(&elems[1..], env, output),
                    "not" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity(format!("{p}: not expects 1 argument")));
                        }
                        let val = eval(&elems[1], env, output)?;
                        return Ok(Value::Boolean(!is_truthy(&val)));
                    }
                    _ => {}
                }
            }
            // Function call
            let func = eval(&elems[0], env, output)?;
            let args: Vec<Value> = elems[1..]
                .iter()
                .map(|e| eval(e, env, output))
                .collect::<Result<_, _>>()?;
            apply_func(&func, &args, p, output)
        }
    }
}

fn expr_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(n, _) => Value::Integer(*n),
        Expr::Boolean(b, _) => Value::Boolean(*b),
        Expr::Str(s, _) => Value::Str(s.clone()),
        Expr::Symbol(s, _) => Value::Symbol(s.clone()),
        Expr::List(elems, _) => Value::List(elems.iter().map(expr_to_value).collect()),
    }
}

fn eval_define(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!("{call_pos}: define requires at least 2 arguments")));
    }
    match &args[0] {
        // (define x expr)
        Expr::Symbol(name, _) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: define requires exactly 2 arguments")));
            }
            let val = eval(&args[1], env, output)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...)
        Expr::List(sig, _) => {
            if sig.is_empty() {
                return Err(EvalError::Parse(format!("{call_pos}: define: empty signature")));
            }
            let name = match &sig[0] {
                Expr::Symbol(s, _) => s.clone(),
                _ => return Err(EvalError::Parse(format!("{call_pos}: define: expected function name"))),
            };
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|e| match e {
                    Expr::Symbol(s, _) => Ok(s.clone()),
                    _ => Err(EvalError::Parse(format!("{call_pos}: define: expected parameter name"))),
                })
                .collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            if body.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: define: empty body")));
            }
            let lambda = Value::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse(format!("{call_pos}: define: expected symbol or list"))),
    }
}

fn eval_if(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity(format!("{call_pos}: if requires 2 or 3 arguments")));
    }
    let cond = eval(&args[0], env, output)?;
    if is_truthy(&cond) {
        eval(&args[1], env, output)
    } else if args.len() == 3 {
        eval(&args[2], env, output)
    } else {
        Ok(Value::Void)
    }
}

fn eval_lambda(args: &[Expr], call_pos: Pos, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{call_pos}: lambda requires params and body")));
    }
    let params = match &args[0] {
        Expr::List(elems, _) => elems
            .iter()
            .map(|e| match e {
                Expr::Symbol(s, _) => Ok(s.clone()),
                _ => Err(EvalError::Parse(format!("{call_pos}: lambda: expected parameter name"))),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => return Err(EvalError::Parse(format!("{call_pos}: lambda: expected parameter list"))),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        env: env.clone(),
    })
}

fn eval_and(exprs: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for e in exprs {
        result = eval(e, env, output)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for e in exprs {
        let result = eval(e, env, output)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_let(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{call_pos}: let requires bindings and body")));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let Expr::Symbol(name, _) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::Arity(format!("{call_pos}: named let requires bindings and body")));
        }
        let bindings_expr = match &args[1] {
            Expr::List(b, _) => b,
            _ => return Err(EvalError::Parse(format!("{call_pos}: let: expected bindings list"))),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings_expr {
            match b {
                Expr::List(pair, _) if pair.len() == 2 => {
                    if let Expr::Symbol(s, _) = &pair[0] {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env, output)?);
                    } else {
                        return Err(EvalError::Parse(format!("{call_pos}: let: expected symbol in binding")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("{call_pos}: let: invalid binding"))),
            }
        }
        let body = args[2..].to_vec();
        let local_env = new_env(Some(env.clone()));
        let lambda = Value::Lambda {
            params: params.clone(),
            body,
            env: local_env.clone(),
        };
        env_set(&local_env, name.clone(), lambda);
        for (param, init) in params.iter().zip(inits.iter()) {
            env_set(&local_env, param.clone(), init.clone());
        }
        let mut result = Value::Void;
        for expr in &args[2..] {
            result = eval(expr, &local_env, output)?;
        }
        return Ok(result);
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => return Err(EvalError::Parse(format!("{call_pos}: let: expected bindings list"))),
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                if let Expr::Symbol(s, _) = &pair[0] {
                    let val = eval(&pair[1], env, output)?;
                    env_set(&local_env, s.clone(), val);
                } else {
                    return Err(EvalError::Parse(format!("{call_pos}: let: expected symbol in binding")));
                }
            }
            _ => return Err(EvalError::Parse(format!("{call_pos}: let: invalid binding"))),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, output)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval(expr, env, output)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    for clause in clauses {
        match clause {
            Expr::List(parts, _) if !parts.is_empty() => {
                if let Expr::Symbol(s, _) = &parts[0] {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env, output)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env, output)?;
                if is_truthy(&test) {
                    let mut result = test;
                    for expr in &parts[1..] {
                        result = eval(expr, env, output)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Parse("cond: invalid clause".into())),
        }
    }
    Ok(Value::Void)
}

fn as_integer(v: &Value, call_pos: Pos) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{call_pos}: expected integer, got {v}"))),
    }
}

fn apply_func(func: &Value, args: &[Value], call_pos: Pos, output: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(name) => apply_builtin(name, args, call_pos, output),
        Value::Lambda {
            params, body, env, ..
        } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: expected {} arguments, got {}",
                    params.len(),
                    args.len()
                )));
            }
            let local_env = new_env(Some(env.clone()));
            for (param, arg) in params.iter().zip(args.iter()) {
                env_set(&local_env, param.clone(), arg.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env, output)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type(format!("{call_pos}: not a procedure: {func}"))),
    }
}

fn apply_builtin(name: &str, args: &[Value], call_pos: Pos, output: &mut String) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_integer(a, call_pos)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: - requires at least 1 argument")));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-as_integer(&args[0], call_pos)?));
            }
            let mut result = as_integer(&args[0], call_pos)?;
            for a in &args[1..] {
                result -= as_integer(a, call_pos)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_integer(a, call_pos)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("{call_pos}: / requires at least 2 arguments")));
            }
            let mut result = as_integer(&args[0], call_pos)?;
            for a in &args[1..] {
                let divisor = as_integer(a, call_pos)?;
                if divisor == 0 {
                    return Err(EvalError::DivisionByZero(format!("{call_pos}: division by zero")));
                }
                result /= divisor;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: < requires 2 arguments")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? < as_integer(&args[1], call_pos)?))
        }
        ">" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: > requires 2 arguments")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? > as_integer(&args[1], call_pos)?))
        }
        "=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: = requires 2 arguments")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? == as_integer(&args[1], call_pos)?))
        }
        "<=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: <= requires 2 arguments")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? <= as_integer(&args[1], call_pos)?))
        }
        ">=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: >= requires 2 arguments")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? >= as_integer(&args[1], call_pos)?))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: cons requires 2 arguments")));
            }
            match &args[1] {
                Value::List(elems) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(elems.iter().cloned());
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
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(EvalError::Type(format!("{call_pos}: car: not a pair"))),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: cdr requires 1 argument")));
            }
            match &args[0] {
                Value::List(elems) if !elems.is_empty() => {
                    Ok(Value::List(elems[1..].to_vec()))
                }
                _ => Err(EvalError::Type(format!("{call_pos}: cdr: not a pair"))),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: null? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(e) if e.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: length requires 1 argument")));
            }
            match &args[0] {
                Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                _ => Err(EvalError::Type(format!("{call_pos}: length: not a list"))),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for arg in args {
                match arg {
                    Value::List(elems) => result.extend(elems.iter().cloned()),
                    _ => return Err(EvalError::Type(format!("{call_pos}: append: not a list"))),
                }
            }
            Ok(Value::List(result))
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
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: pair? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(e) if !e.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: symbol? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: display requires 1 argument")));
            }
            let s = format!("{}", DisplayValue(&args[0]));
            output.push_str(&s);
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: write requires 1 argument")));
            }
            let s = format!("{}", &args[0]);
            output.push_str(&s);
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: newline takes 0 arguments")));
            }
            output.push('\n');
            Ok(Value::Void)
        }
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
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type(format!("{call_pos}: substring: expected string"))),
            };
            let start = as_integer(&args[1], call_pos)? as usize;
            let end = as_integer(&args[2], call_pos)? as usize;
            Ok(Value::Str(s[start..end].to_string()))
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
            let n = as_integer(&args[0], call_pos)?;
            Ok(Value::Str(n.to_string()))
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
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type(format!("{call_pos}: string-ref: expected string"))),
            };
            let idx = as_integer(&args[1], call_pos)? as usize;
            Ok(Value::Char(s.chars().nth(idx).ok_or_else(|| {
                EvalError::Type(format!("{call_pos}: string-ref: index out of range"))
            })?))
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: char? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = default_env();
    let mut output = String::new();
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &env, &mut output)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = default_env();
    let mut output = String::new();
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &env, &mut output)?;
    }
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
