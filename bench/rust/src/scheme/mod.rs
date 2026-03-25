pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    List(Vec<Value>),
    Symbol(String),
    Lambda {
        params: Vec<String>,
        body: Vec<Value>,
        env: Env,
    },
    Builtin(fn(&[Value]) -> Result<Value, EvalError>),
    Void,
}

type Env = Rc<RefCell<Environment>>;

#[derive(Debug)]
struct Environment {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl Environment {
    fn new() -> Env {
        Rc::new(RefCell::new(Environment {
            bindings: HashMap::new(),
            parent: None,
        }))
    }

    fn with_parent(parent: &Env) -> Env {
        Rc::new(RefCell::new(Environment {
            bindings: HashMap::new(),
            parent: Some(Rc::clone(parent)),
        }))
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    fn set(&mut self, name: String, val: Value) {
        self.bindings.insert(name, val);
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!("expected number, got {}", self.display_value()))),
        }
    }

    fn display_value(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_value()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda { .. } | Value::Builtin(_) => "#<procedure>".into(),
            Value::Void => "".into(),
        }
    }
}

struct Parser {
    chars: Vec<char>,
    pos: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.chars.len() {
            if self.chars[self.pos].is_whitespace() {
                self.pos += 1;
            } else if self.chars[self.pos] == ';' {
                while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.chars.get(self.pos).copied();
        if ch.is_some() {
            self.pos += 1;
        }
        ch
    }

    fn parse_expr(&mut self) -> Result<Value, EvalError> {
        self.skip_whitespace();
        match self.peek() {
            None => Err(EvalError::Parse("unexpected end of input".into())),
            Some('\'') => {
                self.next_char(); // consume quote
                let expr = self.parse_expr()?;
                Ok(Value::List(vec![Value::Symbol("quote".into()), expr]))
            }
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string(),
            Some('#') => self.parse_hash(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Value, EvalError> {
        self.next_char(); // consume '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace();
            match self.peek() {
                None => return Err(EvalError::Parse("unclosed parenthesis".into())),
                Some(')') => {
                    self.next_char();
                    return Ok(Value::List(items));
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Value, EvalError> {
        self.next_char(); // consume opening '"'
        let mut s = String::new();
        loop {
            match self.next_char() {
                None => return Err(EvalError::Parse("unclosed string".into())),
                Some('"') => return Ok(Value::Str(s)),
                Some('\\') => match self.next_char() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('\\') => s.push('\\'),
                    Some('"') => s.push('"'),
                    Some(c) => s.push(c),
                    None => return Err(EvalError::Parse("unclosed string escape".into())),
                },
                Some(c) => s.push(c),
            }
        }
    }

    fn parse_hash(&mut self) -> Result<Value, EvalError> {
        self.next_char(); // consume '#'
        match self.next_char() {
            Some('t') => {
                if self.peek().is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-' && c != '!' && c != '?') {
                    Ok(Value::Boolean(true))
                } else {
                    Err(EvalError::Parse("invalid boolean literal".into()))
                }
            }
            Some('f') => {
                if self.peek().is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-' && c != '!' && c != '?') {
                    Ok(Value::Boolean(false))
                } else {
                    Err(EvalError::Parse("invalid boolean literal".into()))
                }
            }
            _ => Err(EvalError::Parse("invalid hash literal".into())),
        }
    }

    fn parse_atom(&mut self) -> Result<Value, EvalError> {
        let mut token = String::new();
        while let Some(c) = self.peek() {
            if c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == ';' {
                break;
            }
            token.push(c);
            self.next_char();
        }
        if token.is_empty() {
            return Err(EvalError::Parse("unexpected character".into()));
        }
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Value::Integer(n));
        }
        Ok(Value::Symbol(token))
    }

    fn parse_all(&mut self) -> Result<Vec<Value>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace();
            if self.pos >= self.chars.len() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        if exprs.is_empty() {
            return Err(EvalError::Parse("empty input".into()));
        }
        Ok(exprs)
    }
}

fn eval(expr: &Value, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Void
        | Value::Lambda { .. } | Value::Builtin(_) => Ok(expr.clone()),
        Value::Symbol(name) => {
            env.borrow().get(name).ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        Value::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            // Check for special forms
            if let Value::Symbol(op) = &items[0] {
                match op.as_str() {
                    "define" => return eval_define(&items[1..], env),
                    "if" => return eval_if(&items[1..], env),
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("quote requires exactly 1 argument".into()));
                        }
                        return Ok(items[1].clone());
                    }
                    "lambda" => return eval_lambda(&items[1..], env),
                    "and" => {
                        if items.len() == 1 {
                            return Ok(Value::Boolean(true));
                        }
                        let mut result = Value::Boolean(true);
                        for a in &items[1..] {
                            result = eval(a, env)?;
                            if !result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        if items.len() == 1 {
                            return Ok(Value::Boolean(false));
                        }
                        let mut result = Value::Boolean(false);
                        for a in &items[1..] {
                            result = eval(a, env)?;
                            if result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    _ => {}
                }
            }
            // Function application
            let func = eval(&items[0], env)?;
            let args: Vec<Value> = items[1..].iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
            apply(&func, &args)
        }
    }
}

fn eval_define(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires at least 2 arguments".into()));
    }
    match &args[0] {
        // (define x expr)
        Value::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires exactly 2 arguments".into()));
            }
            let val = eval(&args[1], env)?;
            env.borrow_mut().set(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...)
        Value::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0] {
                Value::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: expected symbol for function name".into())),
            };
            let params: Vec<String> = sig[1..].iter().map(|p| match p {
                Value::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type("define: expected symbol for parameter".into())),
            }).collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                body,
                env: Rc::clone(env),
            };
            env.borrow_mut().set(name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("define: expected symbol or list".into())),
    }
}

fn eval_if(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
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
        return Err(EvalError::Arity("lambda requires at least 2 arguments".into()));
    }
    let params = match &args[0] {
        Value::List(ps) => {
            ps.iter().map(|p| match p {
                Value::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type("lambda: expected symbol for parameter".into())),
            }).collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Type("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        env: Rc::clone(env),
    })
}

fn apply(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(f) => f(args),
        Value::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let local_env = Environment::with_parent(env);
            for (p, a) in params.iter().zip(args.iter()) {
                local_env.borrow_mut().set(p.clone(), a.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type(format!("not a procedure: {}", func.display_value()))),
    }
}

fn args_to_ints(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|a| a.as_integer()).collect()
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum: i64 = 0;
    for a in args { sum += a.as_integer()?; }
    Ok(Value::Integer(sum))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("- requires at least 1 argument".into())); }
    if args.len() == 1 { return Ok(Value::Integer(-args[0].as_integer()?)); }
    let mut r = args[0].as_integer()?;
    for a in &args[1..] { r -= a.as_integer()?; }
    Ok(Value::Integer(r))
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut p: i64 = 1;
    for a in args { p *= a.as_integer()?; }
    Ok(Value::Integer(p))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("/ requires at least 1 argument".into())); }
    let mut r = args[0].as_integer()?;
    for a in &args[1..] {
        let d = a.as_integer()?;
        if d == 0 { return Err(EvalError::DivisionByZero); }
        r /= d;
    }
    Ok(Value::Integer(r))
}

fn builtin_lt(args: &[Value]) -> Result<Value, EvalError> {
    let v = args_to_ints(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] < w[1])))
}
fn builtin_gt(args: &[Value]) -> Result<Value, EvalError> {
    let v = args_to_ints(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] > w[1])))
}
fn builtin_eq(args: &[Value]) -> Result<Value, EvalError> {
    let v = args_to_ints(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] == w[1])))
}
fn builtin_le(args: &[Value]) -> Result<Value, EvalError> {
    let v = args_to_ints(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] <= w[1])))
}
fn builtin_ge(args: &[Value]) -> Result<Value, EvalError> {
    let v = args_to_ints(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] >= w[1])))
}
fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("not requires exactly 1 argument".into())); }
    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn make_global_env() -> Env {
    let env = Environment::new();
    {
        let mut e = env.borrow_mut();
        e.set("+".into(), Value::Builtin(builtin_add));
        e.set("-".into(), Value::Builtin(builtin_sub));
        e.set("*".into(), Value::Builtin(builtin_mul));
        e.set("/".into(), Value::Builtin(builtin_div));
        e.set("<".into(), Value::Builtin(builtin_lt));
        e.set(">".into(), Value::Builtin(builtin_gt));
        e.set("=".into(), Value::Builtin(builtin_eq));
        e.set("<=".into(), Value::Builtin(builtin_le));
        e.set(">=".into(), Value::Builtin(builtin_ge));
        e.set("not".into(), Value::Builtin(builtin_not));
    }
    env
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let env = make_global_env();
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    Ok(last.display_value())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
