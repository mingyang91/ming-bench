pub mod error;

pub use error::EvalError;

use std::collections::HashMap;

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Pair(Box<Value>, Box<Value>),
    Nil,
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Nil => "()".to_string(),
            Value::Pair(_, _) => {
                let mut out = String::from("(");
                let mut cur = self;
                let mut first = true;
                loop {
                    match cur {
                        Value::Pair(car, cdr) => {
                            if !first {
                                out.push(' ');
                            }
                            first = false;
                            out.push_str(&car.display());
                            cur = cdr;
                        }
                        Value::Nil => break,
                        other => {
                            out.push_str(" . ");
                            out.push_str(&other.display());
                            break;
                        }
                    }
                }
                out.push(')');
                out
            }
            Value::Lambda { .. } => "<procedure>".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::Type(format!("expected integer, got {}", other.display()))),
        }
    }
}

/// A parsed S-expression.
#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

// ── Environment ────────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct Env {
    bindings: HashMap<String, Value>,
    parent: Option<Box<Env>>,
}

impl Env {
    fn new() -> Self {
        Env {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    fn with_parent(parent: &Env) -> Self {
        Env {
            bindings: HashMap::new(),
            parent: Some(Box::new(parent.clone())),
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.get(name) {
            Some(val.clone())
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

// ── Parser ──────────────────────────────────────────────────────────

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
                // skip line comments
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
        let c = self.chars.get(self.pos).copied();
        if c.is_some() {
            self.pos += 1;
        }
        c
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace();
        match self.peek() {
            None => Err(EvalError::Parse("unexpected end of input".into())),
            Some('(') => self.parse_list(),
            Some('\'') => {
                self.next_char(); // consume quote
                let expr = self.parse_expr()?;
                Ok(Expr::List(vec![Expr::Symbol("quote".into()), expr]))
            }
            Some('"') => self.parse_string(),
            Some('#') => self.parse_hash(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.next_char(); // consume '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace();
            match self.peek() {
                None => return Err(EvalError::Parse("unterminated list".into())),
                Some(')') => {
                    self.next_char();
                    return Ok(Expr::List(items));
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.next_char(); // consume opening "
        let mut s = String::new();
        loop {
            match self.next_char() {
                None => return Err(EvalError::Parse("unterminated string".into())),
                Some('\\') => match self.next_char() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('\\') => s.push('\\'),
                    Some('"') => s.push('"'),
                    Some(c) => {
                        s.push('\\');
                        s.push(c);
                    }
                    None => return Err(EvalError::Parse("unterminated escape".into())),
                },
                Some('"') => return Ok(Expr::Str(s)),
                Some(c) => s.push(c),
            }
        }
    }

    fn parse_hash(&mut self) -> Result<Expr, EvalError> {
        self.next_char(); // consume '#'
        match self.next_char() {
            Some('t') => Ok(Expr::Boolean(true)),
            Some('f') => Ok(Expr::Boolean(false)),
            _ => Err(EvalError::Parse("invalid # literal".into())),
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;
        while self.pos < self.chars.len() {
            let c = self.chars[self.pos];
            if c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == ';' {
                break;
            }
            self.pos += 1;
        }
        let token: String = self.chars[start..self.pos].iter().collect();
        if token.is_empty() {
            return Err(EvalError::Parse("empty token".into()));
        }
        // Try parsing as integer
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Expr::Integer(n));
        }
        Ok(Expr::Symbol(token))
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
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

// ── Evaluator ───────────────────────────────────────────────────────

fn eval(expr: &Expr, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => {
            env.get(name).ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        Expr::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            // Check for special forms
            if let Expr::Symbol(op) = &items[0] {
                match op.as_str() {
                    "if" => return eval_if(&items[1..], env),
                    "define" => return eval_define(&items[1..], env),
                    "quote" => return eval_quote(&items[1..]),
                    "lambda" => return eval_lambda(&items[1..], env),
                    "and" => return eval_and(&items[1..], env),
                    "or" => return eval_or(&items[1..], env),
                    _ => {}
                }
            }
            // Check for builtins by name before general eval
            if let Expr::Symbol(name) = &items[0] {
                let args: Result<Vec<Value>, _> = items[1..].iter().map(|a| eval(a, env)).collect();
                let args = args?;
                if let Some(result) = apply_builtin(name, &args)? {
                    return Ok(result);
                }
                // Not a builtin, look up in env
                let func = env.get(name).ok_or_else(|| EvalError::UnboundVariable(name.clone()))?;
                return apply_named(&func, &args, name, env);
            }
            // General application (e.g., lambda expression in head position)
            let func = eval(&items[0], env)?;
            let args: Result<Vec<Value>, _> = items[1..].iter().map(|a| eval(a, env)).collect();
            let args = args?;
            apply(&func, &args)
        }
    }
}

fn apply(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    apply_named(func, args, "", &Env::new())
}

fn apply_named(func: &Value, args: &[Value], name: &str, calling_env: &Env) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { params, body, env } => {
            if params.len() != args.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}",
                    params.len(),
                    args.len()
                )));
            }
            let mut local_env = Env::with_parent(env);
            // For recursion: inject the function itself into the local env
            if !name.is_empty() {
                if let Some(f) = calling_env.get(name) {
                    local_env.set(name.to_string(), f);
                }
            }
            for (p, a) in params.iter().zip(args.iter()) {
                local_env.set(p.clone(), a.clone());
            }
            let mut result = Value::Boolean(false);
            for expr in body {
                result = eval(expr, &mut local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type("not a procedure".into())),
    }
}

fn eval_if(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Nil)
    }
}

fn eval_define(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires arguments".into()));
    }
    match &args[0] {
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires a value".into()));
            }
            let val = eval(&args[1], env)?;
            env.set(name.clone(), val);
            Ok(Value::Nil)
        }
        Expr::List(parts) => {
            // (define (name params...) body...)
            if parts.is_empty() {
                return Err(EvalError::Parse("define: empty name list".into()));
            }
            let name = match &parts[0] {
                Expr::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: name must be symbol".into())),
            };
            let params: Result<Vec<String>, _> = parts[1..].iter().map(|p| {
                match p {
                    Expr::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Type("parameter must be symbol".into())),
                }
            }).collect();
            let params = params?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env.set(name.clone(), lambda);
            Ok(Value::Nil)
        }
        _ => Err(EvalError::Type("define: first argument must be symbol or list".into())),
    }
}

fn expr_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(n) => Value::Integer(*n),
        Expr::Boolean(b) => Value::Boolean(*b),
        Expr::Str(s) => Value::Str(s.clone()),
        Expr::Symbol(s) => Value::Str(s.clone()), // symbols become strings in quoted context? No, they should be symbols. But we don't have a Symbol value... let's use Str for now and see.
        Expr::List(items) => {
            let mut result = Value::Nil;
            for item in items.iter().rev() {
                result = Value::Pair(Box::new(expr_to_value(item)), Box::new(result));
            }
            result
        }
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("quote requires 1 argument".into()));
    }
    Ok(expr_to_value(&args[0]))
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires params and body".into()));
    }
    let params = match &args[0] {
        Expr::List(parts) => {
            let mut ps = Vec::new();
            for p in parts {
                match p {
                    Expr::Symbol(s) => ps.push(s.clone()),
                    _ => return Err(EvalError::Type("parameter must be symbol".into())),
                }
            }
            ps
        }
        _ => return Err(EvalError::Type("lambda: params must be a list".into())),
    };
    Ok(Value::Lambda {
        params,
        body: args[1..].to_vec(),
        env: env.clone(),
    })
}

fn eval_and(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for a in args {
        result = eval(a, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for a in args {
        result = eval(a, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn builtin_env() -> Env {
    let mut env = Env::new();

    // Wrap arithmetic/comparison builtins as lambdas isn't practical.
    // Instead, we'll handle them as named builtins during application.
    // We store them as special lambda values with empty bodies.
    // Actually, let's just keep them as symbol lookups handled in eval.
    // We need a different approach: store builtins in the env.

    // We'll use a special marker. Let's not — instead, check builtins in apply.
    // Actually the simplest: just don't put them in env, handle in eval's application path.
    env
}

fn apply_builtin(op: &str, args: &[Value]) -> Result<Option<Value>, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += a.as_integer()?;
            }
            Ok(Some(Value::Integer(sum)))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return Ok(Some(Value::Integer(-args[0].as_integer()?)));
            }
            let mut result = args[0].as_integer()?;
            for a in &args[1..] {
                result -= a.as_integer()?;
            }
            Ok(Some(Value::Integer(result)))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= a.as_integer()?;
            }
            Ok(Some(Value::Integer(product)))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let mut result = args[0].as_integer()?;
            for a in &args[1..] {
                let divisor = a.as_integer()?;
                if divisor == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= divisor;
            }
            Ok(Some(Value::Integer(result)))
        }
        "<" => compare_op_val(args, |a, b| a < b),
        ">" => compare_op_val(args, |a, b| a > b),
        "=" => compare_op_val(args, |a, b| a == b),
        "<=" => compare_op_val(args, |a, b| a <= b),
        ">=" => compare_op_val(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            Ok(Some(Value::Boolean(!args[0].is_truthy())))
        }
        _ => Ok(None),
    }
}

fn compare_op_val(args: &[Value], cmp: impl Fn(i64, i64) -> bool) -> Result<Option<Value>, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = args[0].as_integer()?;
    for a in &args[1..] {
        let cur = a.as_integer()?;
        if !cmp(prev, cur) {
            return Ok(Some(Value::Boolean(false)));
        }
        prev = cur;
    }
    Ok(Some(Value::Boolean(true)))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let mut env = builtin_env();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &mut env)?;
    }
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
