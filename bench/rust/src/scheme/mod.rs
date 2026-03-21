pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

/// A Scheme value.
#[derive(Debug, Clone)]
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
    Builtin(String),
    Void,
}

impl Val {
    fn is_truthy(&self) -> bool {
        !matches!(self, Val::Bool(false))
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{}", n),
            Val::Bool(true) => write!(f, "#t"),
            Val::Bool(false) => write!(f, "#f"),
            Val::Str(s) => write!(f, "\"{}\"", s),
            Val::Symbol(s) => write!(f, "{}", s),
            Val::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")
            }
            Val::Lambda { .. } => write!(f, "#<procedure>"),
            Val::Builtin(name) => write!(f, "#<builtin:{}>", name),
            Val::Void => write!(f, ""),
        }
    }
}

// ---------- Environment ----------

type Env = Rc<RefCell<EnvInner>>;

struct EnvInner {
    bindings: HashMap<String, Val>,
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

fn env_get(env: &Env, name: &str) -> Option<Val> {
    let inner = env.borrow();
    if let Some(val) = inner.bindings.get(name) {
        Some(val.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        None
    }
}

fn env_set(env: &Env, name: String, val: Val) {
    env.borrow_mut().bindings.insert(name, val);
}

fn default_env() -> Env {
    let env = new_env(None);
    for name in &[
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
        "cons", "car", "cdr", "null?", "list", "length",
        "string?", "number?", "boolean?", "pair?", "symbol?",
    ] {
        env_set(&env, name.to_string(), Val::Builtin(name.to_string()));
    }
    env
}

// ---------- Tokenizer ----------

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Int(i64),
    Bool(bool),
    Str(String),
    Quote,
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
                            '"' => s.push('"'),
                            '\\' => s.push('\\'),
                            c => s.push(c),
                        }
                    } else {
                        s.push(chars[i]);
                    }
                    i += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse("unterminated string".into()));
                }
                i += 1; // skip closing "
                tokens.push(Token::Str(s));
            }
            '#' => {
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push(Token::Bool(true));
                            i += 2;
                        }
                        'f' => {
                            tokens.push(Token::Bool(false));
                            i += 2;
                        }
                        _ => return Err(EvalError::Parse(format!("unexpected #{}", chars[i + 1]))),
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into()));
                }
            }
            _ => {
                // number or symbol
                let start = i;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"' | '\'')
                {
                    i += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push(Token::Int(n));
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
    Int(i64),
    Bool(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

fn parse(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    match &tokens[*pos] {
        Token::Int(n) => {
            let n = *n;
            *pos += 1;
            Ok(Expr::Int(n))
        }
        Token::Bool(b) => {
            let b = *b;
            *pos += 1;
            Ok(Expr::Bool(b))
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
                return Err(EvalError::Parse("unmatched (".into()));
            }
            *pos += 1; // skip )
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

fn eval(expr: &Expr, env: &Env) -> Result<Val, EvalError> {
    match expr {
        Expr::Int(n) => Ok(Val::Int(*n)),
        Expr::Bool(b) => Ok(Val::Bool(*b)),
        Expr::Str(s) => Ok(Val::Str(s.clone())),
        Expr::Symbol(name) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable {
                name: name.clone(),
            })
        }
        Expr::List(elems) => {
            if elems.is_empty() {
                return Ok(Val::List(vec![]));
            }

            // Check for special forms
            if let Expr::Symbol(op) = &elems[0] {
                match op.as_str() {
                    "if" => return eval_if(&elems[1..], env),
                    "define" => return eval_define(&elems[1..], env),
                    "quote" => return eval_quote(&elems[1..]),
                    "lambda" => return eval_lambda(&elems[1..], env),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "let" => return eval_let(&elems[1..], env),
                    "begin" => return eval_begin(&elems[1..], env),
                    "cond" => return eval_cond(&elems[1..], env),
                    _ => {}
                }
            }

            // Function call
            let func = eval(&elems[0], env)?;
            let args: Vec<Val> = elems[1..]
                .iter()
                .map(|e| eval(e, env))
                .collect::<Result<Vec<_>, _>>()?;

            apply(&func, &args)
        }
    }
}

fn apply(func: &Val, args: &[Val]) -> Result<Val, EvalError> {
    match func {
        Val::Builtin(name) => apply_builtin(name, args),
        Val::Lambda {
            params,
            body,
            env: closure_env,
        } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}",
                    params.len(),
                    args.len()
                )));
            }
            let local_env = new_env(Some(closure_env.clone()));
            for (param, arg) in params.iter().zip(args.iter()) {
                env_set(&local_env, param.clone(), arg.clone());
            }
            let mut result = Val::Void;
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type(format!("not a procedure: {}", func))),
    }
}

fn eval_if(args: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
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

fn eval_define(args: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires at least 2 arguments".into()));
    }
    match &args[0] {
        // (define x expr)
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires 2 arguments".into()));
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Val::Void)
        }
        // (define (f params...) body...)
        Expr::List(name_and_params) => {
            if name_and_params.is_empty() {
                return Err(EvalError::Parse("define: empty name list".into()));
            }
            let name = match &name_and_params[0] {
                Expr::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse("define: expected symbol for name".into())),
            };
            let params: Vec<String> = name_and_params[1..]
                .iter()
                .map(|e| match e {
                    Expr::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Parse("define: expected symbol for parameter".into())),
                })
                .collect::<Result<Vec<_>, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Val::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env_set(env, name, lambda);
            Ok(Val::Void)
        }
        _ => Err(EvalError::Parse("define: expected symbol or list".into())),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("quote requires 1 argument".into()));
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
            let vals: Vec<Val> = elems
                .iter()
                .map(expr_to_val)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Val::List(vals))
        }
    }
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires at least 2 arguments".into()));
    }
    let params = match &args[0] {
        Expr::List(param_exprs) => {
            param_exprs
                .iter()
                .map(|e| match e {
                    Expr::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Parse("lambda: expected symbol for parameter".into())),
                })
                .collect::<Result<Vec<_>, _>>()?
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

fn eval_let(args: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into()));
    }
    let bindings = match &args[0] {
        Expr::List(b) => b,
        _ => return Err(EvalError::Parse("let: expected bindings list".into())),
    };
    let local_env = new_env(Some(env.clone()));
    for binding in bindings {
        match binding {
            Expr::List(pair) if pair.len() == 2 => {
                let name = match &pair[0] {
                    Expr::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("let: expected symbol".into())),
                };
                let val = eval(&pair[1], env)?;
                env_set(&local_env, name, val);
            }
            _ => return Err(EvalError::Parse("let: invalid binding".into())),
        }
    }
    let mut result = Val::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Val, EvalError> {
    let mut result = Val::Void;
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &Env) -> Result<Val, EvalError> {
    for clause in clauses {
        let parts = match clause {
            Expr::List(p) => p,
            _ => return Err(EvalError::Parse("cond: expected clause list".into())),
        };
        if parts.is_empty() {
            return Err(EvalError::Parse("cond: empty clause".into()));
        }
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
    Ok(Val::Void)
}

fn apply_builtin(name: &str, args: &[Val]) -> Result<Val, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_int(a)?;
            }
            Ok(Val::Int(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return Ok(Val::Int(-as_int(&args[0])?));
            }
            let mut result = as_int(&args[0])?;
            for a in &args[1..] {
                result -= as_int(a)?;
            }
            Ok(Val::Int(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_int(a)?;
            }
            Ok(Val::Int(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let mut result = as_int(&args[0])?;
            for a in &args[1..] {
                let d = as_int(a)?;
                if d == 0 {
                    return Err(EvalError::Runtime("division by zero".into()));
                }
                result /= d;
            }
            Ok(Val::Int(result))
        }
        "<" => {
            let (a, b) = two_ints(args, "<")?;
            Ok(Val::Bool(a < b))
        }
        ">" => {
            let (a, b) = two_ints(args, ">")?;
            Ok(Val::Bool(a > b))
        }
        "=" => {
            let (a, b) = two_ints(args, "=")?;
            Ok(Val::Bool(a == b))
        }
        "<=" => {
            let (a, b) = two_ints(args, "<=")?;
            Ok(Val::Bool(a <= b))
        }
        ">=" => {
            let (a, b) = two_ints(args, ">=")?;
            Ok(Val::Bool(a >= b))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            Ok(Val::Bool(!args[0].is_truthy()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("cons requires 2 arguments".into()));
            }
            match &args[1] {
                Val::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Val::List(new_list))
                }
                _ => {
                    // Improper pair — represent as 2-element "pair" for now
                    Ok(Val::List(vec![args[0].clone(), args[1].clone()]))
                }
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car requires 1 argument".into()));
            }
            match &args[0] {
                Val::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(EvalError::Type("car: not a pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr requires 1 argument".into()));
            }
            match &args[0] {
                Val::List(elems) if !elems.is_empty() => Ok(Val::List(elems[1..].to_vec())),
                _ => Err(EvalError::Type("cdr: not a pair".into())),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("null? requires 1 argument".into()));
            }
            Ok(Val::Bool(matches!(&args[0], Val::List(e) if e.is_empty())))
        }
        "list" => Ok(Val::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length requires 1 argument".into()));
            }
            match &args[0] {
                Val::List(elems) => Ok(Val::Int(elems.len() as i64)),
                _ => Err(EvalError::Type("length: not a list".into())),
            }
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string? requires 1 argument".into()));
            }
            Ok(Val::Bool(matches!(&args[0], Val::Str(_))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("number? requires 1 argument".into()));
            }
            Ok(Val::Bool(matches!(&args[0], Val::Int(_))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("boolean? requires 1 argument".into()));
            }
            Ok(Val::Bool(matches!(&args[0], Val::Bool(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("pair? requires 1 argument".into()));
            }
            Ok(Val::Bool(matches!(&args[0], Val::List(e) if !e.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("symbol? requires 1 argument".into()));
            }
            Ok(Val::Bool(matches!(&args[0], Val::Symbol(_))))
        }
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

fn as_int(v: &Val) -> Result<i64, EvalError> {
    match v {
        Val::Int(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected number, got {}", v))),
    }
}

fn two_ints(args: &[Val], op: &str) -> Result<(i64, i64), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("{} requires 2 arguments", op)));
    }
    Ok((as_int(&args[0])?, as_int(&args[1])?))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let env = default_env();
    let mut result = Val::Void;
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
