pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type EnvRef = Rc<RefCell<EnvFrame>>;

#[derive(Debug, Clone, PartialEq)]
struct EnvFrame {
    bindings: HashMap<String, Value>,
    parent: Option<EnvRef>,
}

impl EnvFrame {
    fn new() -> EnvRef {
        Rc::new(RefCell::new(EnvFrame {
            bindings: HashMap::new(),
            parent: None,
        }))
    }

    fn child(parent: &EnvRef) -> EnvRef {
        Rc::new(RefCell::new(EnvFrame {
            bindings: HashMap::new(),
            parent: Some(Rc::clone(parent)),
        }))
    }

    fn get(env: &EnvRef, name: &str) -> Option<Value> {
        let frame = env.borrow();
        if let Some(val) = frame.bindings.get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = frame.parent {
            Self::get(parent, name)
        } else {
            None
        }
    }

    fn set(env: &EnvRef, name: String, val: Value) {
        env.borrow_mut().bindings.insert(name, val);
    }
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda(Vec<String>, Vec<Expr>, EnvRef),
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Void, Value::Void) => true,
            (Value::Lambda(..), Value::Lambda(..)) => false,
            _ => false,
        }
    }
}

impl Value {
    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_scheme_string()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda(..) => "#<procedure>".to_string(),
            Value::Void => String::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            '(' => { tokens.push("(".to_string()); i += 1; }
            ')' => { tokens.push(")".to_string()); i += 1; }
            '"' => {
                let mut s = String::from('"');
                i += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        i += 2;
                    } else {
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                }
                tokens.push(s);
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '\'' => { tokens.push("'".to_string()); i += 1; }
            _ => {
                let mut s = String::new();
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '\'') {
                    s.push(chars[i]);
                    i += 1;
                }
                tokens.push(s);
            }
        }
    }
    tokens
}

fn parse(tokens: &[String], pos: usize) -> Result<(Expr, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".to_string()));
    }
    let token = &tokens[pos];
    if token == "(" {
        let mut elems = Vec::new();
        let mut i = pos + 1;
        while i < tokens.len() && tokens[i] != ")" {
            let (expr, next) = parse(tokens, i)?;
            elems.push(expr);
            i = next;
        }
        if i >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".to_string()));
        }
        Ok((Expr::List(elems), i + 1))
    } else if token == ")" {
        Err(EvalError::Parse("unexpected )".to_string()))
    } else if token == "'" {
        let (inner, next) = parse(tokens, pos + 1)?;
        Ok((Expr::List(vec![Expr::Symbol("quote".to_string()), inner]), next))
    } else {
        Ok((parse_atom(token)?, pos + 1))
    }
}

fn parse_atom(token: &str) -> Result<Expr, EvalError> {
    if token == "#t" {
        return Ok(Expr::Boolean(true));
    }
    if token == "#f" {
        return Ok(Expr::Boolean(false));
    }
    if let Ok(n) = token.parse::<i64>() {
        return Ok(Expr::Integer(n));
    }
    if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
        let inner = &token[1..token.len() - 1];
        return Ok(Expr::Str(inner.to_string()));
    }
    Ok(Expr::Symbol(token.to_string()))
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut exprs = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let (expr, next) = parse(&tokens, pos)?;
        exprs.push(expr);
        pos = next;
    }
    Ok(exprs)
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

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(s) => EnvFrame::get(env, s)
            .ok_or_else(|| EvalError::Parse(format!("unbound variable: {}", s))),
        Expr::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".to_string()));
            }
            // Check for special forms
            if let Expr::Symbol(op) = &elems[0] {
                match op.as_str() {
                    "define" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Parse("define requires exactly 2 arguments".to_string()));
                        }
                        match &elems[1] {
                            Expr::Symbol(name) => {
                                if elems.len() != 3 {
                                    return Err(EvalError::Parse("define requires exactly 2 arguments".to_string()));
                                }
                                let val = eval_expr(&elems[2], env)?;
                                // For recursive lambdas: patch the closure env
                                if let Value::Lambda(params, body, closure_env) = &val {
                                    let val = Value::Lambda(params.clone(), body.clone(), Rc::clone(closure_env));
                                    EnvFrame::set(env, name.clone(), val.clone());
                                    // Make the lambda visible in its own closure for recursion
                                    EnvFrame::set(closure_env, name.clone(), val);
                                } else {
                                    EnvFrame::set(env, name.clone(), val);
                                }
                                Ok(Value::Void)
                            }
                            // Shorthand: (define (f x y) body...) => (define f (lambda (x y) body...))
                            Expr::List(name_and_params) => {
                                if name_and_params.is_empty() {
                                    return Err(EvalError::Parse("define shorthand requires a name".to_string()));
                                }
                                if let Expr::Symbol(name) = &name_and_params[0] {
                                    let params: Result<Vec<String>, _> = name_and_params[1..]
                                        .iter()
                                        .map(|p| match p {
                                            Expr::Symbol(s) => Ok(s.clone()),
                                            _ => Err(EvalError::Parse("parameter must be a symbol".to_string())),
                                        })
                                        .collect();
                                    let params = params?;
                                    let body: Vec<Expr> = elems[2..].to_vec();
                                    let closure_env = Rc::clone(env);
                                    let val = Value::Lambda(params, body, closure_env.clone());
                                    EnvFrame::set(env, name.clone(), val.clone());
                                    // Recursion support
                                    EnvFrame::set(&closure_env, name.clone(), val);
                                    Ok(Value::Void)
                                } else {
                                    Err(EvalError::Parse("define requires a symbol as name".to_string()))
                                }
                            }
                            _ => Err(EvalError::Parse("define requires a symbol or list".to_string())),
                        }
                    }
                    "lambda" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Parse("lambda requires params and body".to_string()));
                        }
                        if let Expr::List(param_exprs) = &elems[1] {
                            let params: Result<Vec<String>, _> = param_exprs
                                .iter()
                                .map(|p| match p {
                                    Expr::Symbol(s) => Ok(s.clone()),
                                    _ => Err(EvalError::Parse("parameter must be a symbol".to_string())),
                                })
                                .collect();
                            let params = params?;
                            let body: Vec<Expr> = elems[2..].to_vec();
                            Ok(Value::Lambda(params, body, Rc::clone(env)))
                        } else {
                            Err(EvalError::Parse("lambda params must be a list".to_string()))
                        }
                    }
                    "if" => {
                        if elems.len() < 3 || elems.len() > 4 {
                            return Err(EvalError::Parse("if requires 2 or 3 arguments".to_string()));
                        }
                        let cond = eval_expr(&elems[1], env)?;
                        if !is_false(&cond) {
                            eval_expr(&elems[2], env)
                        } else if elems.len() == 4 {
                            eval_expr(&elems[3], env)
                        } else {
                            Ok(Value::Void)
                        }
                    }
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Parse("quote requires exactly 1 argument".to_string()));
                        }
                        Ok(expr_to_value(&elems[1]))
                    }
                    "+" => {
                        let mut sum: i64 = 0;
                        for arg in &elems[1..] {
                            sum += require_int(&eval_expr(arg, env)?)?;
                        }
                        Ok(Value::Integer(sum))
                    }
                    "-" => {
                        if elems.len() < 2 {
                            return Err(EvalError::Parse("- requires at least one argument".to_string()));
                        }
                        let first = require_int(&eval_expr(&elems[1], env)?)?;
                        if elems.len() == 2 {
                            Ok(Value::Integer(-first))
                        } else {
                            let mut result = first;
                            for arg in &elems[2..] {
                                result -= require_int(&eval_expr(arg, env)?)?;
                            }
                            Ok(Value::Integer(result))
                        }
                    }
                    "*" => {
                        let mut product: i64 = 1;
                        for arg in &elems[1..] {
                            product *= require_int(&eval_expr(arg, env)?)?;
                        }
                        Ok(Value::Integer(product))
                    }
                    "/" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Parse("/ requires at least two arguments".to_string()));
                        }
                        let mut result = require_int(&eval_expr(&elems[1], env)?)?;
                        for arg in &elems[2..] {
                            let divisor = require_int(&eval_expr(arg, env)?)?;
                            if divisor == 0 {
                                return Err(EvalError::Parse("division by zero".to_string()));
                            }
                            result /= divisor;
                        }
                        Ok(Value::Integer(result))
                    }
                    "<" => {
                        let (a, b) = require_two_ints(&elems[1..], "<", env)?;
                        Ok(Value::Boolean(a < b))
                    }
                    ">" => {
                        let (a, b) = require_two_ints(&elems[1..], ">", env)?;
                        Ok(Value::Boolean(a > b))
                    }
                    "=" => {
                        let (a, b) = require_two_ints(&elems[1..], "=", env)?;
                        Ok(Value::Boolean(a == b))
                    }
                    "<=" => {
                        let (a, b) = require_two_ints(&elems[1..], "<=", env)?;
                        Ok(Value::Boolean(a <= b))
                    }
                    ">=" => {
                        let (a, b) = require_two_ints(&elems[1..], ">=", env)?;
                        Ok(Value::Boolean(a >= b))
                    }
                    "not" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Parse("not requires exactly one argument".to_string()));
                        }
                        let val = eval_expr(&elems[1], env)?;
                        Ok(Value::Boolean(is_false(&val)))
                    }
                    "and" => {
                        let mut result = Value::Boolean(true);
                        for arg in &elems[1..] {
                            result = eval_expr(arg, env)?;
                            if is_false(&result) {
                                return Ok(result);
                            }
                        }
                        Ok(result)
                    }
                    "or" => {
                        let mut result = Value::Boolean(false);
                        for arg in &elems[1..] {
                            result = eval_expr(arg, env)?;
                            if !is_false(&result) {
                                return Ok(result);
                            }
                        }
                        Ok(result)
                    }
                    "cons" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Parse("cons requires exactly 2 arguments".to_string()));
                        }
                        let head = eval_expr(&elems[1], env)?;
                        let tail = eval_expr(&elems[2], env)?;
                        match tail {
                            Value::List(mut v) => {
                                v.insert(0, head);
                                Ok(Value::List(v))
                            }
                            _ => Err(EvalError::Parse("cons: second argument must be a list".to_string())),
                        }
                    }
                    "car" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Parse("car requires exactly 1 argument".to_string()));
                        }
                        let val = eval_expr(&elems[1], env)?;
                        match val {
                            Value::List(v) if !v.is_empty() => Ok(v[0].clone()),
                            _ => Err(EvalError::Parse("car: argument must be a non-empty list".to_string())),
                        }
                    }
                    "cdr" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Parse("cdr requires exactly 1 argument".to_string()));
                        }
                        let val = eval_expr(&elems[1], env)?;
                        match val {
                            Value::List(v) if !v.is_empty() => Ok(Value::List(v[1..].to_vec())),
                            _ => Err(EvalError::Parse("cdr: argument must be a non-empty list".to_string())),
                        }
                    }
                    "null?" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Parse("null? requires exactly 1 argument".to_string()));
                        }
                        let val = eval_expr(&elems[1], env)?;
                        Ok(Value::Boolean(matches!(val, Value::List(ref v) if v.is_empty())))
                    }
                    "list" => {
                        let mut items = Vec::new();
                        for arg in &elems[1..] {
                            items.push(eval_expr(arg, env)?);
                        }
                        Ok(Value::List(items))
                    }
                    "string?" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Parse("string? requires exactly 1 argument".to_string()));
                        }
                        let val = eval_expr(&elems[1], env)?;
                        Ok(Value::Boolean(matches!(val, Value::Str(_))))
                    }
                    "number?" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Parse("number? requires exactly 1 argument".to_string()));
                        }
                        let val = eval_expr(&elems[1], env)?;
                        Ok(Value::Boolean(matches!(val, Value::Integer(_))))
                    }
                    "boolean?" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Parse("boolean? requires exactly 1 argument".to_string()));
                        }
                        let val = eval_expr(&elems[1], env)?;
                        Ok(Value::Boolean(matches!(val, Value::Boolean(_))))
                    }
                    "pair?" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Parse("pair? requires exactly 1 argument".to_string()));
                        }
                        let val = eval_expr(&elems[1], env)?;
                        Ok(Value::Boolean(matches!(val, Value::List(ref v) if !v.is_empty())))
                    }
                    "symbol?" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Parse("symbol? requires exactly 1 argument".to_string()));
                        }
                        let val = eval_expr(&elems[1], env)?;
                        Ok(Value::Boolean(matches!(val, Value::Symbol(_))))
                    }
                    "length" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Parse("length requires exactly 1 argument".to_string()));
                        }
                        let val = eval_expr(&elems[1], env)?;
                        match val {
                            Value::List(v) => Ok(Value::Integer(v.len() as i64)),
                            _ => Err(EvalError::Parse("length: argument must be a list".to_string())),
                        }
                    }
                    "begin" => {
                        let mut result = Value::Void;
                        for arg in &elems[1..] {
                            result = eval_expr(arg, env)?;
                        }
                        Ok(result)
                    }
                    "let" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Parse("let requires bindings and body".to_string()));
                        }
                        let bindings = match &elems[1] {
                            Expr::List(b) => b,
                            _ => return Err(EvalError::Parse("let bindings must be a list".to_string())),
                        };
                        let let_env = EnvFrame::child(env);
                        for binding in bindings {
                            match binding {
                                Expr::List(pair) if pair.len() == 2 => {
                                    if let Expr::Symbol(name) = &pair[0] {
                                        let val = eval_expr(&pair[1], env)?;
                                        EnvFrame::set(&let_env, name.clone(), val);
                                    } else {
                                        return Err(EvalError::Parse("let binding name must be a symbol".to_string()));
                                    }
                                }
                                _ => return Err(EvalError::Parse("let binding must be a pair".to_string())),
                            }
                        }
                        let mut result = Value::Void;
                        for expr in &elems[2..] {
                            result = eval_expr(expr, &let_env)?;
                        }
                        Ok(result)
                    }
                    "cond" => {
                        for clause in &elems[1..] {
                            match clause {
                                Expr::List(parts) if !parts.is_empty() => {
                                    if let Expr::Symbol(s) = &parts[0] {
                                        if s == "else" {
                                            let mut result = Value::Void;
                                            for expr in &parts[1..] {
                                                result = eval_expr(expr, env)?;
                                            }
                                            return Ok(result);
                                        }
                                    }
                                    let test = eval_expr(&parts[0], env)?;
                                    if !is_false(&test) {
                                        let mut result = test;
                                        for expr in &parts[1..] {
                                            result = eval_expr(expr, env)?;
                                        }
                                        return Ok(result);
                                    }
                                }
                                _ => return Err(EvalError::Parse("cond clause must be a list".to_string())),
                            }
                        }
                        Ok(Value::Void)
                    }
                    _ => {
                        // Not a special form, try as procedure call
                        apply_proc(elems, env)
                    }
                }
            } else {
                // Operator is not a symbol — evaluate it (e.g., ((lambda ...) args))
                apply_proc(elems, env)
            }
        }
    }
}

fn apply_proc(elems: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let func = eval_expr(&elems[0], env)?;
    let args: Result<Vec<Value>, _> = elems[1..].iter().map(|a| eval_expr(a, env)).collect();
    let args = args?;
    match func {
        Value::Lambda(params, body, closure_env) => {
            if params.len() != args.len() {
                return Err(EvalError::Parse(format!(
                    "expected {} arguments, got {}",
                    params.len(),
                    args.len()
                )));
            }
            let call_env = EnvFrame::child(&closure_env);
            for (param, arg) in params.iter().zip(args) {
                EnvFrame::set(&call_env, param.clone(), arg);
            }
            let mut result = Value::Void;
            for expr in &body {
                result = eval_expr(expr, &call_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Parse("not a procedure".to_string())),
    }
}

fn is_false(val: &Value) -> bool {
    matches!(val, Value::Boolean(false))
}

fn require_two_ints(args: &[Expr], op: &str, env: &EnvRef) -> Result<(i64, i64), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Parse(format!("{} requires exactly two arguments", op)));
    }
    let a = require_int(&eval_expr(&args[0], env)?)?;
    let b = require_int(&eval_expr(&args[1], env)?)?;
    Ok((a, b))
}

fn require_int(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Parse("expected integer".to_string())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("42"), Ok("42".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".to_string()));
    }
    let env = EnvFrame::new();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval_expr(expr, &env)?;
    }
    Ok(result.to_scheme_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
