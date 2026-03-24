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
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::Builtin(name) => write!(f, "#<builtin:{name}>"),
            Value::Void => write!(f, ""),
        }
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
            '(' => {
                tokens.push(Token::LParen);
                i += 1;
            }
            ')' => {
                tokens.push(Token::RParen);
                i += 1;
            }
            '\'' => {
                tokens.push(Token::Quote);
                i += 1;
            }
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
                            c => {
                                s.push('\\');
                                s.push(c);
                            }
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
                        't' => {
                            tokens.push(Token::Boolean(true));
                            i += 2;
                        }
                        'f' => {
                            tokens.push(Token::Boolean(false));
                            i += 2;
                        }
                        _ => {
                            return Err(EvalError::Parse(format!(
                                "unexpected character after #: {}",
                                chars[i + 1]
                            )));
                        }
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into()));
                }
            }
            _ => {
                let start = i;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"' | '\'')
                {
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

// ---------- Parser ----------

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

fn parse(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    match &tokens[*pos] {
        Token::Integer(n) => {
            let n = *n;
            *pos += 1;
            Ok(Expr::Integer(n))
        }
        Token::Boolean(b) => {
            let b = *b;
            *pos += 1;
            Ok(Expr::Boolean(b))
        }
        Token::Str(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::Str(s))
        }
        Token::Symbol(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::Symbol(s))
        }
        Token::Quote => {
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Expr::List(vec![Expr::Symbol("quote".into()), inner]))
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
            Ok(Expr::List(elems))
        }
        Token::RParen => Err(EvalError::Parse("unexpected )".into())),
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

const BUILTINS: &[&str] = &["+", "-", "*", "/", "<", ">", "=", "<=", ">="];

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        Expr::List(elems) => {
            if elems.is_empty() {
                return Ok(Value::List(vec![]));
            }
            // Check for special forms
            if let Expr::Symbol(op) = &elems[0] {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], env),
                    "if" => return eval_if(&elems[1..], env),
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity("quote expects 1 argument".into()));
                        }
                        return Ok(expr_to_value(&elems[1]));
                    }
                    "lambda" => return eval_lambda(&elems[1..], env),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "not" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity("not expects 1 argument".into()));
                        }
                        let val = eval(&elems[1], env)?;
                        return Ok(Value::Boolean(!is_truthy(&val)));
                    }
                    _ => {}
                }
            }
            // Function call
            let func = eval(&elems[0], env)?;
            let args: Vec<Value> = elems[1..]
                .iter()
                .map(|e| eval(e, env))
                .collect::<Result<_, _>>()?;
            apply_func(&func, &args)
        }
    }
}

fn expr_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(n) => Value::Integer(*n),
        Expr::Boolean(b) => Value::Boolean(*b),
        Expr::Str(s) => Value::Str(s.clone()),
        Expr::Symbol(s) => Value::Symbol(s.clone()),
        Expr::List(elems) => Value::List(elems.iter().map(expr_to_value).collect()),
    }
}

fn eval_define(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires at least 2 arguments".into()));
    }
    match &args[0] {
        // (define x expr)
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires exactly 2 arguments".into()));
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...)
        Expr::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0] {
                Expr::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse("define: expected function name".into())),
            };
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|e| match e {
                    Expr::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Parse("define: expected parameter name".into())),
                })
                .collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            if body.is_empty() {
                return Err(EvalError::Arity("define: empty body".into()));
            }
            let lambda = Value::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse("define: expected symbol or list".into())),
    }
}

fn eval_if(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env)?;
    if is_truthy(&cond) {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires params and body".into()));
    }
    let params = match &args[0] {
        Expr::List(elems) => elems
            .iter()
            .map(|e| match e {
                Expr::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse("lambda: expected parameter name".into())),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => return Err(EvalError::Parse("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        env: env.clone(),
    })
}

fn eval_and(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for e in exprs {
        result = eval(e, env)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for e in exprs {
        let result = eval(e, env)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(Value::Boolean(false))
}

fn as_integer(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected integer, got {v}"))),
    }
}

fn apply_func(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(name) => apply_builtin(name, args),
        Value::Lambda {
            params, body, env, ..
        } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}",
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
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type(format!("not a procedure: {func}"))),
    }
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_integer(a)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-as_integer(&args[0])?));
            }
            let mut result = as_integer(&args[0])?;
            for a in &args[1..] {
                result -= as_integer(a)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_integer(a)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
            }
            let mut result = as_integer(&args[0])?;
            for a in &args[1..] {
                let divisor = as_integer(a)?;
                if divisor == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= divisor;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("< requires 2 arguments".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? < as_integer(&args[1])?))
        }
        ">" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("> requires 2 arguments".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? > as_integer(&args[1])?))
        }
        "=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("= requires 2 arguments".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? == as_integer(&args[1])?))
        }
        "<=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("<= requires 2 arguments".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? <= as_integer(&args[1])?))
        }
        ">=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(">= requires 2 arguments".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? >= as_integer(&args[1])?))
        }
        _ => Err(EvalError::UnboundVariable(name.to_string())),
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
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &env)?;
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
