pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// ---------------------------------------------------------------------------
// Value representation
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        body: Vec<Value>,
        env: Env,
    },
    Void,
}

impl Value {
    fn display_scheme(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.display_scheme()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda { .. } => "#<procedure>".to_string(),
            Value::Void => "".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// ---------------------------------------------------------------------------
// Environment
// ---------------------------------------------------------------------------

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

fn default_env() -> Env {
    new_env(None)
}

// ---------------------------------------------------------------------------
// Tokenizer
// ---------------------------------------------------------------------------

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

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => { tokens.push(Token::LParen); i += 1; }
            ')' => { tokens.push(Token::RParen); i += 1; }
            '\'' => { tokens.push(Token::Quote); i += 1; }
            '"' => {
                i += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            c => { s.push('\\'); s.push(c); }
                        }
                    } else {
                        s.push(chars[i]);
                    }
                    i += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse("unterminated string".into()));
                }
                i += 1;
                tokens.push(Token::Str(s));
            }
            '#' => {
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => { tokens.push(Token::Boolean(true)); i += 2; }
                        'f' => { tokens.push(Token::Boolean(false)); i += 2; }
                        _ => return Err(EvalError::Parse(format!("unexpected #{}", chars[i+1]))),
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into()));
                }
            }
            _ => {
                let start = i;
                while i < chars.len() && !matches!(chars[i], ' '|'\t'|'\n'|'\r'|'('|')'|'"'|';'|'\'') {
                    i += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push(Token::Integer(n));
                } else {
                    tokens.push(Token::Symbol(word));
                }
            }
        }
    }
    Ok(tokens)
}

// ---------------------------------------------------------------------------
// Parser — tokens → Value (s-expression)
// ---------------------------------------------------------------------------

fn parse(tokens: &[Token], pos: &mut usize) -> Result<Value, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    match &tokens[*pos] {
        Token::Integer(n) => { let v = Value::Integer(*n); *pos += 1; Ok(v) }
        Token::Boolean(b) => { let v = Value::Boolean(*b); *pos += 1; Ok(v) }
        Token::Str(s) => { let v = Value::Str(s.clone()); *pos += 1; Ok(v) }
        Token::Symbol(s) => { let v = Value::Symbol(s.clone()); *pos += 1; Ok(v) }
        Token::Quote => {
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Value::List(vec![Value::Symbol("quote".into()), inner]))
        }
        Token::LParen => {
            *pos += 1;
            let mut elems = Vec::new();
            while *pos < tokens.len() && tokens[*pos] != Token::RParen {
                elems.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into()));
            }
            *pos += 1;
            Ok(Value::List(elems))
        }
        Token::RParen => Err(EvalError::Parse("unexpected )".into())),
    }
}

fn parse_all(input: &str) -> Result<Vec<Value>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ---------------------------------------------------------------------------
// Evaluator
// ---------------------------------------------------------------------------

fn eval(expr: &Value, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) => Ok(expr.clone()),
        Value::Symbol(name) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        Value::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            if let Value::Symbol(op) = &elems[0] {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], env),
                    "if" => return eval_if(&elems[1..], env),
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::WrongArgCount { expected: "1".into(), got: elems.len() - 1 });
                        }
                        return Ok(elems[1].clone());
                    }
                    "lambda" => return eval_lambda(&elems[1..], env),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "let" => return eval_let(&elems[1..], env),
                    "begin" => return eval_begin(&elems[1..], env),
                    "cond" => return eval_cond(&elems[1..], env),
                    "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
                    | "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length"
                    | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" => {
                        return eval_builtin(op, &elems[1..], env);
                    }
                    _ => {}
                }
            }
            // General function application
            let func = eval(&elems[0], env)?;
            let args: Vec<Value> = elems[1..].iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
            apply_function(&func, &args)
        }
        Value::Void => Ok(Value::Void),
        Value::Lambda { .. } => Ok(expr.clone()),
    }
}

fn apply_function(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len().to_string(),
                    got: args.len(),
                });
            }
            let local_env = new_env(Some(env.clone()));
            for (p, a) in params.iter().zip(args.iter()) {
                env_set(&local_env, p.clone(), a.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::NotAProcedure(func.display_scheme())),
    }
}

fn eval_define(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse("define: missing arguments".into()));
    }
    match &args[0] {
        Value::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Parse("define: expected 2 arguments".into()));
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        Value::List(sig) => {
            // (define (f params...) body...)
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0] {
                Value::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse("define: expected symbol".into())),
            };
            let params: Vec<String> = sig[1..].iter().map(|p| {
                match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Parse("define: expected symbol in params".into())),
                }
            }).collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda { params, body, env: env.clone() };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse("define: invalid syntax".into())),
    }
}

fn eval_if(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Parse("if: expected 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_lambda(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse("lambda: expected params and body".into()));
    }
    let params = match &args[0] {
        Value::List(elems) => {
            elems.iter().map(|p| match p {
                Value::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse("lambda: expected symbol in params".into())),
            }).collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Parse("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda { params, body, env: env.clone() })
}

fn eval_builtin(op: &str, args: &[Value], env: &Env) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_integer(&eval(a, env)?)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: "at least 1".into(), got: 0 });
            }
            let first = expect_integer(&eval(&args[0], env)?)?;
            if args.len() == 1 {
                return Ok(Value::Integer(-first));
            }
            let mut result = first;
            for a in &args[1..] {
                result -= expect_integer(&eval(a, env)?)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut prod: i64 = 1;
            for a in args {
                prod *= expect_integer(&eval(a, env)?)?;
            }
            Ok(Value::Integer(prod))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: "at least 1".into(), got: 0 });
            }
            let first = expect_integer(&eval(&args[0], env)?)?;
            if args.len() == 1 {
                if first == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                return Ok(Value::Integer(1 / first));
            }
            let mut result = first;
            for a in &args[1..] {
                let d = expect_integer(&eval(a, env)?)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => compare_op(args, env, |a, b| a < b),
        ">" => compare_op(args, env, |a, b| a > b),
        "=" => compare_op(args, env, |a, b| a == b),
        "<=" => compare_op(args, env, |a, b| a <= b),
        ">=" => compare_op(args, env, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len() });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(!val.is_truthy()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount { expected: "2".into(), got: args.len() });
            }
            let head = eval(&args[0], env)?;
            let tail = eval(&args[1], env)?;
            match tail {
                Value::List(mut elems) => {
                    elems.insert(0, head);
                    Ok(Value::List(elems))
                }
                _ => Err(EvalError::TypeError("cons: second argument must be a list".into())),
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len() });
            }
            let val = eval(&args[0], env)?;
            match val {
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(EvalError::TypeError("car: expected non-empty list".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len() });
            }
            let val = eval(&args[0], env)?;
            match val {
                Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
                _ => Err(EvalError::TypeError("cdr: expected non-empty list".into())),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len() });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(matches!(val, Value::List(ref e) if e.is_empty())))
        }
        "list" => {
            let vals: Vec<Value> = args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
            Ok(Value::List(vals))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len() });
            }
            let val = eval(&args[0], env)?;
            match val {
                Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                _ => Err(EvalError::TypeError("length: expected list".into())),
            }
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len() });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(matches!(val, Value::Str(_))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len() });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(matches!(val, Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len() });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(matches!(val, Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len() });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(matches!(val, Value::List(ref e) if !e.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len() });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(matches!(val, Value::Symbol(_))))
        }
        _ => Err(EvalError::UnboundVariable(op.to_string())),
    }
}

fn eval_and(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for a in args {
        result = eval(a, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(false));
    }
    let mut result = Value::Boolean(false);
    for a in args {
        result = eval(a, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_let(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse("let: expected bindings and body".into()));
    }
    let bindings = match &args[0] {
        Value::List(b) => b,
        _ => return Err(EvalError::Parse("let: expected bindings list".into())),
    };
    let local_env = new_env(Some(env.clone()));
    for binding in bindings {
        match binding {
            Value::List(pair) if pair.len() == 2 => {
                let name = match &pair[0] {
                    Value::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("let: expected symbol in binding".into())),
                };
                let val = eval(&pair[1], env)?;
                env_set(&local_env, name, val);
            }
            _ => return Err(EvalError::Parse("let: invalid binding".into())),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    for clause in args {
        match clause {
            Value::List(elems) if elems.len() >= 2 => {
                if matches!(&elems[0], Value::Symbol(s) if s == "else") {
                    let mut result = Value::Void;
                    for expr in &elems[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
                let test = eval(&elems[0], env)?;
                if test.is_truthy() {
                    let mut result = Value::Void;
                    for expr in &elems[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Parse("cond: invalid clause".into())),
        }
    }
    Ok(Value::Void)
}

fn compare_op(args: &[Value], env: &Env, cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount { expected: "at least 2".into(), got: args.len() });
    }
    let mut prev = expect_integer(&eval(&args[0], env)?)?;
    for a in &args[1..] {
        let cur = expect_integer(&eval(a, env)?)?;
        if !cmp(prev, cur) {
            return Ok(Value::Boolean(false));
        }
        prev = cur;
    }
    Ok(Value::Boolean(true))
}

fn expect_integer(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::TypeError(format!("expected integer, got {}", v.display_scheme()))),
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let env = default_env();
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    Ok(last.display_scheme())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
