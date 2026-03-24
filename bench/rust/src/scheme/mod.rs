pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda(Vec<String>, Vec<Value>, Env), // params, body, closure env
    Void,
}

impl Value {
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
            Value::Lambda(..) => "#<procedure>".into(),
            Value::Void => "".into(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// --- Environment ---

type Env = Rc<RefCell<EnvInner>>;

#[derive(Debug, PartialEq)]
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

// --- Parser ---

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

    fn parse_expr(&mut self) -> Result<Value, EvalError> {
        self.skip_whitespace();
        match self.peek() {
            None => Err(EvalError::Parse("unexpected end of input".into())),
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string(),
            Some('#') => self.parse_hash(),
            Some('\'') => {
                self.pos += 1;
                let inner = self.parse_expr()?;
                Ok(Value::List(vec![Value::Symbol("quote".into()), inner]))
            }
            Some(_) => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Value, EvalError> {
        self.pos += 1; // skip '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace();
            match self.peek() {
                None => return Err(EvalError::Parse("unterminated list".into())),
                Some(')') => {
                    self.pos += 1;
                    return Ok(Value::List(items));
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Value, EvalError> {
        self.pos += 1;
        let mut s = String::new();
        loop {
            match self.chars.get(self.pos) {
                None => return Err(EvalError::Parse("unterminated string".into())),
                Some('\\') => {
                    self.pos += 1;
                    match self.chars.get(self.pos) {
                        Some('n') => { s.push('\n'); self.pos += 1; }
                        Some('t') => { s.push('\t'); self.pos += 1; }
                        Some('\\') => { s.push('\\'); self.pos += 1; }
                        Some('"') => { s.push('"'); self.pos += 1; }
                        Some(c) => { s.push(*c); self.pos += 1; }
                        None => return Err(EvalError::Parse("unterminated escape".into())),
                    }
                }
                Some('"') => {
                    self.pos += 1;
                    return Ok(Value::Str(s));
                }
                Some(c) => {
                    s.push(*c);
                    self.pos += 1;
                }
            }
        }
    }

    fn parse_hash(&mut self) -> Result<Value, EvalError> {
        self.pos += 1;
        match self.peek() {
            Some('t') => {
                self.pos += 1;
                if self.peek().is_none_or(is_delimiter) {
                    Ok(Value::Boolean(true))
                } else {
                    Err(EvalError::Parse("invalid # literal".into()))
                }
            }
            Some('f') => {
                self.pos += 1;
                if self.peek().is_none_or(is_delimiter) {
                    Ok(Value::Boolean(false))
                } else {
                    Err(EvalError::Parse("invalid # literal".into()))
                }
            }
            _ => Err(EvalError::Parse("invalid # literal".into())),
        }
    }

    fn parse_atom(&mut self) -> Result<Value, EvalError> {
        let start = self.pos;
        while self.pos < self.chars.len() && !is_delimiter(self.chars[self.pos]) {
            self.pos += 1;
        }
        let token: String = self.chars[start..self.pos].iter().collect();
        if let Ok(n) = token.parse::<i64>() {
            Ok(Value::Integer(n))
        } else {
            Ok(Value::Symbol(token))
        }
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
        Ok(exprs)
    }
}

fn is_delimiter(c: char) -> bool {
    c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == ';'
}

fn extract_symbol_list(values: &[Value], context: &str) -> Result<Vec<String>, EvalError> {
    values
        .iter()
        .map(|v| match v {
            Value::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Type(format!("{context}: expected symbol in parameter list"))),
        })
        .collect()
}

// --- Evaluator ---

fn eval(val: &Value, env: &Env) -> Result<Value, EvalError> {
    match val {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Lambda(..) => Ok(val.clone()),
        Value::Symbol(name) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        Value::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            let head = &items[0];
            if let Value::Symbol(name) = head {
                match name.as_str() {
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("quote requires 1 argument".into()));
                        }
                        return Ok(items[1].clone());
                    }
                    "if" => {
                        if items.len() < 3 || items.len() > 4 {
                            return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
                        }
                        let cond = eval(&items[1], env)?;
                        if cond.is_truthy() {
                            return eval(&items[2], env);
                        } else if items.len() == 4 {
                            return eval(&items[3], env);
                        } else {
                            return Ok(Value::Void);
                        }
                    }
                    "define" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("define requires at least 2 arguments".into()));
                        }
                        match &items[1] {
                            Value::Symbol(var_name) => {
                                let val = eval(&items[2], env)?;
                                env_set(env, var_name.clone(), val);
                                return Ok(Value::Void);
                            }
                            Value::List(sig) => {
                                // (define (f params...) body...)
                                if sig.is_empty() {
                                    return Err(EvalError::Parse("define: empty signature".into()));
                                }
                                let func_name = match &sig[0] {
                                    Value::Symbol(s) => s.clone(),
                                    _ => return Err(EvalError::Type("define: expected symbol for function name".into())),
                                };
                                let params = extract_symbol_list(&sig[1..], "define")?;
                                let body = items[2..].to_vec();
                                let lambda = Value::Lambda(params, body, env.clone());
                                env_set(env, func_name, lambda);
                                return Ok(Value::Void);
                            }
                            _ => return Err(EvalError::Type("define: expected symbol or list".into())),
                        }
                    }
                    "lambda" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("lambda requires at least 2 arguments".into()));
                        }
                        let Value::List(param_list) = &items[1] else {
                            return Err(EvalError::Type("lambda: expected parameter list".into()));
                        };
                        let params = extract_symbol_list(param_list, "lambda")?;
                        let body = items[2..].to_vec();
                        return Ok(Value::Lambda(params, body, env.clone()));
                    }
                    "and" => return eval_and(&items[1..], env),
                    "or" => return eval_or(&items[1..], env),
                    "not" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("not requires 1 argument".into()));
                        }
                        let v = eval(&items[1], env)?;
                        return Ok(Value::Boolean(!v.is_truthy()));
                    }
                    _ => {}
                }
            }
            // Function application
            let func = eval(head, env)?;
            let args: Result<Vec<Value>, _> = items[1..].iter().map(|a| eval(a, env)).collect();
            let args = args?;
            apply(&func, &args)
        }
        Value::Void => Ok(Value::Void),
    }
}

fn apply(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Lambda(params, body, closure_env) => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let local_env = new_env(Some(closure_env.clone()));
            for (param, arg) in params.iter().zip(args.iter()) {
                env_set(&local_env, param.clone(), arg.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        Value::Symbol(name) => apply_builtin(name, args),
        _ => Err(EvalError::Type("not a procedure".into())),
    }
}

fn eval_and(exprs: &[Value], env: &Env) -> Result<Value, EvalError> {
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

fn eval_or(exprs: &[Value], env: &Env) -> Result<Value, EvalError> {
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
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let mut result = as_integer(&args[0])?;
            for a in &args[1..] {
                let d = as_integer(a)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => compare_nums(args, |a, b| a < b),
        ">" => compare_nums(args, |a, b| a > b),
        "=" => compare_nums(args, |a, b| a == b),
        "<=" => compare_nums(args, |a, b| a <= b),
        ">=" => compare_nums(args, |a, b| a >= b),
        _ => Err(EvalError::UnboundVariable(name.into())),
    }
}

fn as_integer(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected integer, got {:?}", val))),
    }
}

fn compare_nums(args: &[Value], cmp: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = as_integer(&args[0])?;
    for a in &args[1..] {
        let curr = as_integer(a)?;
        if !cmp(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

fn make_global_env() -> Env {
    let env = new_env(None);
    // Register builtins as symbols that apply_builtin knows
    for name in &["+", "-", "*", "/", "<", ">", "=", "<=", ">="] {
        env_set(&env, name.to_string(), Value::Symbol(name.to_string()));
    }
    env
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
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
