pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

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
    Nil,
    Pair(Box<Value>, Box<Value>),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
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
        }
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
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
    Symbol(String, Pos),
    List(Vec<Expr>, Pos),
}

impl Expr {
    fn pos(&self) -> Pos {
        match self {
            Expr::Integer(_, p) => *p,
            Expr::Boolean(_, p) => *p,
            Expr::Str(_, p) => *p,
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
            | "number?" | "string?" | "boolean?" | "pair?" | "symbol?"
    )
}

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let p = expr.pos();
    match expr {
        Expr::Integer(n, _) => Ok(Value::Integer(*n)),
        Expr::Boolean(b, _) => Ok(Value::Boolean(*b)),
        Expr::Str(s, _) => Ok(Value::Str(s.clone())),
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
                    _ => {}
                }
            }
            // Function call
            let func = eval(&elems[0], env)?;
            let args: Vec<Value> = elems[1..]
                .iter()
                .map(|e| eval(e, env))
                .collect::<Result<_, _>>()?;
            apply(&func, &args, p)
        }
    }
}

fn apply(func: &Value, args: &[Value], call_pos: Pos) -> Result<Value, EvalError> {
    match func {
        Value::Symbol(op) => apply_builtin(op, args, call_pos),
        Value::Lambda { params, body, env } => {
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
            let params: Vec<String> = name_and_params[1..]
                .iter()
                .map(|e| match e {
                    Expr::Symbol(s, _) => Ok(s.clone()),
                    _ => Err(EvalError::Type(format!(
                        "define: expected symbol as parameter at {}",
                        p
                    ))),
                })
                .collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
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
    let params = match &args[0] {
        Expr::List(param_exprs, _) => param_exprs
            .iter()
            .map(|e| match e {
                Expr::Symbol(s, _) => Ok(s.clone()),
                _ => Err(EvalError::Type(format!(
                    "lambda: expected symbol as parameter at {}",
                    p
                ))),
            })
            .collect::<Result<Vec<_>, _>>()?,
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
        body,
        env: env.clone(),
    })
}

fn expr_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(n, _) => Value::Integer(*n),
        Expr::Boolean(b, _) => Value::Boolean(*b),
        Expr::Str(s, _) => Value::Str(s.clone()),
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
        return apply(&func, &init_vals, p);
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
        _ => Err(EvalError::UnboundVariable(format!("{} at {}", op, p))),
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
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
