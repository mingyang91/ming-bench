pub mod error;

pub use error::EvalError;

use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        name: Option<String>,
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
            Value::Str(s) => write!(f, "\"{}\"", s),
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
            Value::Builtin(name) => write!(f, "#<procedure:{name}>"),
            Value::Void => write!(f, "#<void>"),
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

// --- Tokenizer ---

#[derive(Debug, Clone)]
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
                        't' => {
                            tokens.push(Token::Boolean(true));
                            i += 2;
                        }
                        'f' => {
                            tokens.push(Token::Boolean(false));
                            i += 2;
                        }
                        _ => return Err(EvalError::Parse(format!("unexpected #{}", chars[i + 1]))),
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into()));
                }
            }
            c if c == '-' || c == '+' => {
                if i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                    let is_number = i == 0
                        || matches!(tokens.last(), Some(Token::LParen) | None);
                    if is_number {
                        let start = i;
                        i += 1;
                        while i < chars.len() && chars[i].is_ascii_digit() {
                            i += 1;
                        }
                        let num_str: String = chars[start..i].iter().collect();
                        tokens.push(Token::Integer(num_str.parse().map_err(|_| {
                            EvalError::Parse(format!("invalid number: {num_str}"))
                        })?));
                    } else {
                        let start = i;
                        i += 1;
                        while i < chars.len() && is_symbol_char(chars[i]) {
                            i += 1;
                        }
                        let sym: String = chars[start..i].iter().collect();
                        tokens.push(Token::Symbol(sym));
                    }
                } else {
                    let start = i;
                    i += 1;
                    while i < chars.len() && is_symbol_char(chars[i]) {
                        i += 1;
                    }
                    let sym: String = chars[start..i].iter().collect();
                    tokens.push(Token::Symbol(sym));
                }
            }
            c if c.is_ascii_digit() => {
                let start = i;
                while i < chars.len() && chars[i].is_ascii_digit() {
                    i += 1;
                }
                let num_str: String = chars[start..i].iter().collect();
                tokens.push(Token::Integer(num_str.parse().map_err(|_| {
                    EvalError::Parse(format!("invalid number: {num_str}"))
                })?));
            }
            c if is_symbol_start(c) => {
                let start = i;
                while i < chars.len() && is_symbol_char(chars[i]) {
                    i += 1;
                }
                let sym: String = chars[start..i].iter().collect();
                tokens.push(Token::Symbol(sym));
            }
            c => return Err(EvalError::Parse(format!("unexpected character: {c}"))),
        }
    }
    Ok(tokens)
}

fn is_symbol_start(c: char) -> bool {
    c.is_alphabetic() || "!$%&*/<=>?^_~".contains(c)
}

fn is_symbol_char(c: char) -> bool {
    is_symbol_start(c) || c.is_ascii_digit() || "+-.:@#".contains(c)
}

// --- Parser ---

fn parse(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    match &tokens[*pos] {
        Token::Integer(n) => { let n = *n; *pos += 1; Ok(Expr::Integer(n)) }
        Token::Boolean(b) => { let b = *b; *pos += 1; Ok(Expr::Boolean(b)) }
        Token::Str(s) => { let s = s.clone(); *pos += 1; Ok(Expr::Str(s)) }
        Token::Symbol(s) => { let s = s.clone(); *pos += 1; Ok(Expr::Symbol(s)) }
        Token::Quote => {
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Expr::List(vec![Expr::Symbol("quote".into()), inner]))
        }
        Token::LParen => {
            *pos += 1;
            let mut list = Vec::new();
            while *pos < tokens.len() && !matches!(tokens[*pos], Token::RParen) {
                list.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into()));
            }
            *pos += 1;
            Ok(Expr::List(list))
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

// --- Environment ---

#[derive(Debug, Clone)]
struct Env {
    bindings: HashMap<String, Value>,
    parent: Option<Box<Env>>,
}

impl Env {
    fn new() -> Self {
        Env { bindings: HashMap::new(), parent: None }
    }

    fn with_parent(parent: Env) -> Self {
        Env { bindings: HashMap::new(), parent: Some(Box::new(parent)) }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.bindings.get(name) {
            Some(v.clone())
        } else if let Some(ref parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    fn set(&mut self, name: String, val: Value) {
        self.bindings.insert(name, val);
    }
}

fn default_env() -> Env {
    let mut env = Env::new();
    for name in ["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not", "and", "or"] {
        env.set(name.into(), Value::Builtin(name.into()));
    }
    env
}

// --- Evaluator ---

fn eval(expr: &Expr, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(s) => {
            env.get(s).ok_or_else(|| EvalError::Unbound(s.clone()))
        }
        Expr::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Syntax("empty application".into()));
            }

            // Check for special forms
            if let Expr::Symbol(head) = &elems[0] {
                match head.as_str() {
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Syntax("quote requires 1 argument".into()));
                        }
                        return Ok(expr_to_value(&elems[1]));
                    }
                    "if" => {
                        if elems.len() < 3 || elems.len() > 4 {
                            return Err(EvalError::Syntax("if requires 2 or 3 arguments".into()));
                        }
                        let cond = eval(&elems[1], env)?;
                        if is_truthy(&cond) {
                            return eval(&elems[2], env);
                        } else if elems.len() == 4 {
                            return eval(&elems[3], env);
                        } else {
                            return Ok(Value::Void);
                        }
                    }
                    "define" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax("define requires at least 2 arguments".into()));
                        }
                        match &elems[1] {
                            Expr::Symbol(name) => {
                                let val = eval(&elems[2], env)?;
                                env.set(name.clone(), val);
                                return Ok(Value::Void);
                            }
                            Expr::List(sig) => {
                                // (define (f x y) body...)
                                if sig.is_empty() {
                                    return Err(EvalError::Syntax("define: empty signature".into()));
                                }
                                let name = match &sig[0] {
                                    Expr::Symbol(s) => s.clone(),
                                    _ => return Err(EvalError::Syntax("define: expected function name".into())),
                                };
                                let params: Vec<String> = sig[1..].iter().map(|e| match e {
                                    Expr::Symbol(s) => Ok(s.clone()),
                                    _ => Err(EvalError::Syntax("define: expected parameter name".into())),
                                }).collect::<Result<_, _>>()?;
                                let body = elems[2..].to_vec();
                                let lambda = Value::Lambda {
                                    name: Some(name.clone()),
                                    params,
                                    body,
                                    env: env.clone(),
                                };
                                env.set(name, lambda);
                                return Ok(Value::Void);
                            }
                            _ => return Err(EvalError::Syntax("define: expected symbol or list".into())),
                        }
                    }
                    "lambda" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax("lambda requires params and body".into()));
                        }
                        let params = match &elems[1] {
                            Expr::List(ps) => {
                                ps.iter().map(|e| match e {
                                    Expr::Symbol(s) => Ok(s.clone()),
                                    _ => Err(EvalError::Syntax("lambda: expected parameter name".into())),
                                }).collect::<Result<Vec<_>, _>>()?
                            }
                            _ => return Err(EvalError::Syntax("lambda: expected parameter list".into())),
                        };
                        let body = elems[2..].to_vec();
                        return Ok(Value::Lambda {
                            name: None,
                            params,
                            body,
                            env: env.clone(),
                        });
                    }
                    "and" => {
                        if elems.len() == 1 {
                            return Ok(Value::Boolean(true));
                        }
                        let args = &elems[1..];
                        let mut result = Value::Boolean(true);
                        for a in args {
                            result = eval(a, env)?;
                            if !is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        if elems.len() == 1 {
                            return Ok(Value::Boolean(false));
                        }
                        let args = &elems[1..];
                        let mut result = Value::Boolean(false);
                        for a in args {
                            result = eval(a, env)?;
                            if is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    _ => {}
                }
            }

            // Function application
            let func = eval(&elems[0], env)?;
            let args: Vec<Value> = elems[1..].iter()
                .map(|a| eval(a, env))
                .collect::<Result<_, _>>()?;

            apply_func(&func, &args)
        }
    }
}

fn apply_func(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(name) => apply_builtin(name, args),
        Value::Lambda { name, params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let mut local_env = Env::with_parent(env.clone());
            // For named functions, bind self for recursion
            if let Some(n) = name {
                local_env.set(n.clone(), func.clone());
            }
            for (p, a) in params.iter().zip(args.iter()) {
                local_env.set(p.clone(), a.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &mut local_env)?;
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
                sum += as_int(a)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                Ok(Value::Integer(-as_int(&args[0])?))
            } else {
                let mut result = as_int(&args[0])?;
                for a in &args[1..] {
                    result -= as_int(a)?;
                }
                Ok(Value::Integer(result))
            }
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_int(a)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let mut result = as_int(&args[0])?;
            for a in &args[1..] {
                let divisor = as_int(a)?;
                if divisor == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= divisor;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            let vals = args_to_ints(args)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] < w[1])))
        }
        ">" => {
            let vals = args_to_ints(args)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] > w[1])))
        }
        "=" => {
            let vals = args_to_ints(args)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] == w[1])))
        }
        "<=" => {
            let vals = args_to_ints(args)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] <= w[1])))
        }
        ">=" => {
            let vals = args_to_ints(args)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] >= w[1])))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            Ok(Value::Boolean(!is_truthy(&args[0])))
        }
        _ => Err(EvalError::Unbound(name.to_string())),
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

fn as_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected integer, got {v}"))),
    }
}

fn args_to_ints(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|a| as_int(a)).collect()
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let mut env = default_env();
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &mut env)?;
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
