pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

type BuiltinFn = fn(&[Val]) -> Result<Val, EvalError>;

#[derive(Clone)]
enum Val {
    Int(i64),
    Bool(bool),
    Str(String),
    Symbol(String),
    List(Vec<Val>),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String, BuiltinFn),
    Void,
}

impl Val {
    fn is_truthy(&self) -> bool {
        !matches!(self, Val::Bool(false))
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{n}"),
            Val::Bool(true) => write!(f, "#t"),
            Val::Bool(false) => write!(f, "#f"),
            Val::Str(s) => write!(f, "\"{}\"", s),
            Val::Symbol(s) => write!(f, "{s}"),
            Val::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Val::Lambda { .. } | Val::Builtin(..) => write!(f, "#<procedure>"),
            Val::Void => write!(f, "#<void>"),
        }
    }
}

// --- Environment ---

type Frame = Rc<RefCell<HashMap<String, Val>>>;

#[derive(Clone)]
struct Env {
    frames: Vec<Frame>,
}

impl Env {
    fn new() -> Self {
        let frame = Rc::new(RefCell::new(HashMap::new()));
        let builtins: &[(&str, BuiltinFn)] = &[
            ("+", builtin_add as BuiltinFn),
            ("-", builtin_sub),
            ("*", builtin_mul),
            ("/", builtin_div),
            ("<", builtin_lt),
            (">", builtin_gt),
            ("=", builtin_eq),
            ("<=", builtin_le),
            (">=", builtin_ge),
            ("not", builtin_not),
            ("cons", builtin_cons as BuiltinFn),
            ("car", builtin_car),
            ("cdr", builtin_cdr),
            ("list", builtin_list),
            ("null?", builtin_null),
            ("length", builtin_length),
            ("append", builtin_append),
            ("boolean?", builtin_is_boolean),
            ("number?", builtin_is_number),
            ("string?", builtin_is_string),
            ("symbol?", builtin_is_symbol),
            ("pair?", builtin_is_pair),
        ];
        for &(name, f) in builtins {
            frame.borrow_mut().insert(name.to_string(), Val::Builtin(name.to_string(), f));
        }
        Env { frames: vec![frame] }
    }

    fn get(&self, name: &str) -> Option<Val> {
        for frame in self.frames.iter().rev() {
            if let Some(v) = frame.borrow().get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    fn define(&self, name: String, val: Val) {
        self.frames.last().expect("env has no frames").borrow_mut().insert(name, val);
    }

    fn push(&self) -> Env {
        let mut frames = self.frames.clone();
        frames.push(Rc::new(RefCell::new(HashMap::new())));
        Env { frames }
    }
}

// --- Parser ---

#[derive(Debug, Clone)]
enum Expr {
    Int(i64),
    Bool(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    while let Some(&c) = chars.peek() {
        match c {
            ' ' | '\t' | '\n' | '\r' => { chars.next(); }
            ';' => {
                while let Some(&c2) = chars.peek() {
                    chars.next();
                    if c2 == '\n' { break; }
                }
            }
            '(' => { tokens.push("(".into()); chars.next(); }
            ')' => { tokens.push(")".into()); chars.next(); }
            '\'' => { tokens.push("'".into()); chars.next(); }
            '"' => {
                chars.next();
                let mut s = String::new();
                loop {
                    match chars.next() {
                        Some('\\') => {
                            match chars.next() {
                                Some('n') => s.push('\n'),
                                Some('t') => s.push('\t'),
                                Some('"') => s.push('"'),
                                Some('\\') => s.push('\\'),
                                Some(other) => { s.push('\\'); s.push(other); }
                                None => break,
                            }
                        }
                        Some('"') => break,
                        Some(c2) => s.push(c2),
                        None => break,
                    }
                }
                tokens.push(format!("\"{}\"", s));
            }
            _ => {
                let mut tok = String::new();
                while let Some(&c2) = chars.peek() {
                    if c2 == '(' || c2 == ')' || c2 == ' ' || c2 == '\t' || c2 == '\n' || c2 == '\r' || c2 == ';' || c2 == '\'' {
                        break;
                    }
                    tok.push(c2);
                    chars.next();
                }
                tokens.push(tok);
            }
        }
    }
    tokens
}

fn parse(tokens: &[String]) -> Result<(Expr, usize), EvalError> {
    if tokens.is_empty() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let tok = &tokens[0];
    if tok == "'" {
        let (inner, consumed) = parse(&tokens[1..])?;
        Ok((Expr::List(vec![Expr::Symbol("quote".into()), inner]), 1 + consumed))
    } else if tok == "(" {
        let mut elems = Vec::new();
        let mut i = 1;
        while i < tokens.len() && tokens[i] != ")" {
            let (expr, consumed) = parse(&tokens[i..])?;
            elems.push(expr);
            i += consumed;
        }
        if i >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".into()));
        }
        Ok((Expr::List(elems), i + 1))
    } else if tok == ")" {
        Err(EvalError::Parse("unexpected )".into()))
    } else if tok.starts_with('"') {
        let s = tok[1..tok.len()-1].to_string();
        Ok((Expr::Str(s), 1))
    } else if tok == "#t" {
        Ok((Expr::Bool(true), 1))
    } else if tok == "#f" {
        Ok((Expr::Bool(false), 1))
    } else if let Ok(n) = tok.parse::<i64>() {
        Ok((Expr::Int(n), 1))
    } else {
        Ok((Expr::Symbol(tok.clone()), 1))
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut exprs = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        let (expr, consumed) = parse(&tokens[i..])?;
        exprs.push(expr);
        i += consumed;
    }
    Ok(exprs)
}

// --- Evaluator ---

fn eval(expr: &Expr, env: &Env) -> Result<Val, EvalError> {
    match expr {
        Expr::Int(n) => Ok(Val::Int(*n)),
        Expr::Bool(b) => Ok(Val::Bool(*b)),
        Expr::Str(s) => Ok(Val::Str(s.clone())),
        Expr::Symbol(name) => {
            env.get(name).ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        Expr::List(elems) => {
            if elems.is_empty() {
                return Ok(Val::List(vec![]));
            }
            // Check for special forms
            if let Expr::Symbol(op) = &elems[0] {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], env),
                    "if" => return eval_if(&elems[1..], env),
                    "quote" => return eval_quote(&elems[1..]),
                    "lambda" => return eval_lambda(&elems[1..], env),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "begin" => return eval_begin(&elems[1..], env),
                    "let" => return eval_let(&elems[1..], env),
                    "cond" => return eval_cond(&elems[1..], env),
                    _ => {}
                }
            }
            // Evaluate function position
            let func = eval(&elems[0], env)?;
            let args: Vec<Val> = elems[1..].iter().map(|e| eval(e, env)).collect::<Result<_, _>>()?;
            apply_val(&func, &args)
        }
    }
}

fn apply_val(func: &Val, args: &[Val]) -> Result<Val, EvalError> {
    match func {
        Val::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let new_env = env.push();
            for (p, a) in params.iter().zip(args.iter()) {
                new_env.define(p.clone(), a.clone());
            }
            let mut result = Val::Void;
            for expr in body {
                result = eval(expr, &new_env)?;
            }
            Ok(result)
        }
        Val::Builtin(_, f) => f(args),
        _ => Err(EvalError::Type("not a procedure".into())),
    }
}

fn eval_define(args: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse("define: missing arguments".into()));
    }
    match &args[0] {
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define: expected 2 arguments".into()));
            }
            let val = eval(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Val::Void)
        }
        Expr::List(sig) => {
            // (define (f params...) body...)
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0] {
                Expr::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse("define: expected symbol".into())),
            };
            let params: Vec<String> = sig[1..].iter().map(|e| match e {
                Expr::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse("define: expected parameter name".into())),
            }).collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Val::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env.define(name, lambda);
            Ok(Val::Void)
        }
        _ => Err(EvalError::Parse("define: expected symbol or list".into())),
    }
}

fn eval_if(args: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if: expected 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Val::Void)
    }
}

fn eval_quote(args: &[Expr]) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("quote: expected 1 argument".into()));
    }
    expr_to_val(&args[0])
}

fn expr_to_val(expr: &Expr) -> Result<Val, EvalError> {
    match expr {
        Expr::Int(n) => Ok(Val::Int(*n)),
        Expr::Bool(b) => Ok(Val::Bool(*b)),
        Expr::Str(s) => Ok(Val::Str(s.clone())),
        Expr::Symbol(s) => Ok(Val::Symbol(s.clone())),
        Expr::List(elems) => {
            let vals: Vec<Val> = elems.iter().map(expr_to_val).collect::<Result<_, _>>()?;
            Ok(Val::List(vals))
        }
    }
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse("lambda: missing parameters".into()));
    }
    let params = match &args[0] {
        Expr::List(param_exprs) => {
            param_exprs.iter().map(|e| match e {
                Expr::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse("lambda: expected parameter name".into())),
            }).collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Parse("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Val::Lambda {
        params,
        body,
        env: env.clone(),
    })
}

fn eval_and(exprs: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if exprs.is_empty() {
        return Ok(Val::Bool(true));
    }
    let mut result = Val::Bool(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if exprs.is_empty() {
        return Ok(Val::Bool(false));
    }
    for expr in exprs {
        let result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Val::Bool(false))
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Val, EvalError> {
    let mut result = Val::Void;
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_let(args: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse("let: missing arguments".into()));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let Expr::Symbol(name) = &args[0] {
        if args.len() < 2 {
            return Err(EvalError::Parse("let: missing bindings".into()));
        }
        let bindings = match &args[1] {
            Expr::List(b) => b,
            _ => return Err(EvalError::Parse("let: expected bindings list".into())),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings {
            match b {
                Expr::List(pair) if pair.len() == 2 => {
                    if let Expr::Symbol(s) = &pair[0] {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env)?);
                    } else {
                        return Err(EvalError::Parse("let: expected variable name".into()));
                    }
                }
                _ => return Err(EvalError::Parse("let: invalid binding".into())),
            }
        }
        let body = args[2..].to_vec();
        let new_env = env.push();
        let lambda = Val::Lambda {
            params: params.clone(),
            body,
            env: new_env.clone(),
        };
        new_env.define(name.clone(), lambda.clone());
        apply_val(&lambda, &inits)
    } else {
        // Regular let: (let ((var init) ...) body ...)
        let bindings = match &args[0] {
            Expr::List(b) => b,
            _ => return Err(EvalError::Parse("let: expected bindings list".into())),
        };
        let new_env = env.push();
        for b in bindings {
            match b {
                Expr::List(pair) if pair.len() == 2 => {
                    if let Expr::Symbol(s) = &pair[0] {
                        let val = eval(&pair[1], env)?;
                        new_env.define(s.clone(), val);
                    } else {
                        return Err(EvalError::Parse("let: expected variable name".into()));
                    }
                }
                _ => return Err(EvalError::Parse("let: invalid binding".into())),
            }
        }
        let mut result = Val::Void;
        for expr in &args[1..] {
            result = eval(expr, &new_env)?;
        }
        Ok(result)
    }
}

fn eval_cond(clauses: &[Expr], env: &Env) -> Result<Val, EvalError> {
    for clause in clauses {
        match clause {
            Expr::List(parts) if !parts.is_empty() => {
                // Check for else clause
                if let Expr::Symbol(s) = &parts[0] {
                    if s == "else" {
                        let mut result = Val::Void;
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
            _ => return Err(EvalError::Parse("cond: invalid clause".into())),
        }
    }
    Ok(Val::Void)
}

fn require_ints(args: &[Val], op: &str) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|a| match a {
        Val::Int(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{op}: expected number"))),
    }).collect()
}

fn builtin_add(args: &[Val]) -> Result<Val, EvalError> {
    let nums = require_ints(args, "+")?;
    Ok(Val::Int(nums.iter().sum()))
}

fn builtin_sub(args: &[Val]) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("-: need at least 1 argument".into()));
    }
    let nums = require_ints(args, "-")?;
    if nums.len() == 1 {
        Ok(Val::Int(-nums[0]))
    } else {
        Ok(Val::Int(nums[0] - nums[1..].iter().sum::<i64>()))
    }
}

fn builtin_mul(args: &[Val]) -> Result<Val, EvalError> {
    let nums = require_ints(args, "*")?;
    Ok(Val::Int(nums.iter().product()))
}

fn builtin_div(args: &[Val]) -> Result<Val, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("/: need at least 2 arguments".into()));
    }
    let nums = require_ints(args, "/")?;
    if nums[1..].contains(&0) {
        return Err(EvalError::Runtime("division by zero".into()));
    }
    let mut result = nums[0];
    for &n in &nums[1..] {
        result /= n;
    }
    Ok(Val::Int(result))
}

fn builtin_lt(args: &[Val]) -> Result<Val, EvalError> {
    let nums = require_ints(args, "<")?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] < w[1])))
}

fn builtin_gt(args: &[Val]) -> Result<Val, EvalError> {
    let nums = require_ints(args, ">")?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] > w[1])))
}

fn builtin_eq(args: &[Val]) -> Result<Val, EvalError> {
    let nums = require_ints(args, "=")?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] == w[1])))
}

fn builtin_le(args: &[Val]) -> Result<Val, EvalError> {
    let nums = require_ints(args, "<=")?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] <= w[1])))
}

fn builtin_ge(args: &[Val]) -> Result<Val, EvalError> {
    let nums = require_ints(args, ">=")?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] >= w[1])))
}

fn builtin_not(args: &[Val]) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("not: expected 1 argument".into()));
    }
    Ok(Val::Bool(!args[0].is_truthy()))
}

fn builtin_cons(args: &[Val]) -> Result<Val, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("cons: expected 2 arguments".into()));
    }
    match &args[1] {
        Val::List(elems) => {
            let mut new = vec![args[0].clone()];
            new.extend(elems.iter().cloned());
            Ok(Val::List(new))
        }
        _ => {
            // cons pair (improper list) - for now treat as 2-element list
            Ok(Val::List(vec![args[0].clone(), args[1].clone()]))
        }
    }
}

fn builtin_car(args: &[Val]) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car: expected 1 argument".into()));
    }
    match &args[0] {
        Val::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
        _ => Err(EvalError::Type("car: expected non-empty list".into())),
    }
}

fn builtin_cdr(args: &[Val]) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr: expected 1 argument".into()));
    }
    match &args[0] {
        Val::List(elems) if !elems.is_empty() => Ok(Val::List(elems[1..].to_vec())),
        _ => Err(EvalError::Type("cdr: expected non-empty list".into())),
    }
}

fn builtin_list(args: &[Val]) -> Result<Val, EvalError> {
    Ok(Val::List(args.to_vec()))
}

fn builtin_null(args: &[Val]) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("null?: expected 1 argument".into()));
    }
    Ok(Val::Bool(matches!(&args[0], Val::List(v) if v.is_empty())))
}

fn builtin_length(args: &[Val]) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("length: expected 1 argument".into()));
    }
    match &args[0] {
        Val::List(elems) => Ok(Val::Int(elems.len() as i64)),
        _ => Err(EvalError::Type("length: expected list".into())),
    }
}

fn builtin_append(args: &[Val]) -> Result<Val, EvalError> {
    let mut result = Vec::new();
    for arg in args {
        match arg {
            Val::List(elems) => result.extend(elems.iter().cloned()),
            _ => return Err(EvalError::Type("append: expected list".into())),
        }
    }
    Ok(Val::List(result))
}

fn builtin_is_boolean(args: &[Val]) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("boolean?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Bool(_))))
}

fn builtin_is_number(args: &[Val]) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("number?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Int(_))))
}

fn builtin_is_string(args: &[Val]) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Str(_))))
}

fn builtin_is_symbol(args: &[Val]) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("symbol?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Symbol(_))))
}

fn builtin_is_pair(args: &[Val]) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("pair?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(&args[0], Val::List(v) if !v.is_empty())))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = Env::new();
    let mut last = Val::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    Ok(last.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
