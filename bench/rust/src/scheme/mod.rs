pub mod error;

pub use error::{EvalError, Pos};

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

fn tokenize(input: &str) -> Result<Vec<(Token, Pos)>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;

    while i < chars.len() {
        match chars[i] {
            '\n' => {
                line += 1;
                col = 1;
                i += 1;
            }
            ' ' | '\t' | '\r' => {
                col += 1;
                i += 1;
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => {
                tokens.push((Token::LParen, Pos::new(line, col)));
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push((Token::RParen, Pos::new(line, col)));
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push((Token::Quote, Pos::new(line, col)));
                i += 1;
                col += 1;
            }
            '"' => {
                let start_pos = Pos::new(line, col);
                i += 1;
                col += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\n' {
                        line += 1;
                        col = 1;
                    } else {
                        col += 1;
                    }
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1;
                        col += 1;
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
                    return Err(EvalError::Parse {
                        msg: "unterminated string".into(),
                        pos: start_pos,
                    });
                }
                i += 1; // skip closing "
                col += 1;
                tokens.push((Token::Str(s), start_pos));
            }
            '#' => {
                let p = Pos::new(line, col);
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push((Token::Bool(true), p));
                            i += 2;
                            col += 2;
                        }
                        'f' => {
                            tokens.push((Token::Bool(false), p));
                            i += 2;
                            col += 2;
                        }
                        _ => {
                            return Err(EvalError::Parse {
                                msg: format!("unexpected #{}", chars[i + 1]),
                                pos: p,
                            })
                        }
                    }
                } else {
                    return Err(EvalError::Parse {
                        msg: "unexpected #".into(),
                        pos: p,
                    });
                }
            }
            _ => {
                // number or symbol
                let start = i;
                let p = Pos::new(line, col);
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"' | '\'')
                {
                    i += 1;
                    col += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push((Token::Int(n), p));
                } else {
                    tokens.push((Token::Symbol(word), p));
                }
            }
        }
    }
    Ok(tokens)
}

// ---------- Parser ----------

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    pos: Pos,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Int(i64),
    Bool(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

impl Expr {
    fn new(kind: ExprKind, pos: Pos) -> Self {
        Self { kind, pos }
    }
}

fn parse(tokens: &[(Token, Pos)], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        let p = if tokens.is_empty() {
            Pos::new(1, 1)
        } else {
            tokens[tokens.len() - 1].1
        };
        return Err(EvalError::Parse {
            msg: "unexpected end of input".into(),
            pos: p,
        });
    }
    let (ref tok, tpos) = tokens[*pos];
    match tok {
        Token::Int(n) => {
            let n = *n;
            *pos += 1;
            Ok(Expr::new(ExprKind::Int(n), tpos))
        }
        Token::Bool(b) => {
            let b = *b;
            *pos += 1;
            Ok(Expr::new(ExprKind::Bool(b), tpos))
        }
        Token::Str(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::new(ExprKind::Str(s), tpos))
        }
        Token::Symbol(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::new(ExprKind::Symbol(s), tpos))
        }
        Token::Quote => {
            let qpos = tpos;
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Expr::new(
                ExprKind::List(vec![
                    Expr::new(ExprKind::Symbol("quote".into()), qpos),
                    inner,
                ]),
                qpos,
            ))
        }
        Token::LParen => {
            let lpos = tpos;
            *pos += 1;
            let mut elems = Vec::new();
            while *pos < tokens.len() && !matches!(tokens[*pos].0, Token::RParen) {
                elems.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse {
                    msg: "unmatched (".into(),
                    pos: lpos,
                });
            }
            *pos += 1; // skip )
            Ok(Expr::new(ExprKind::List(elems), lpos))
        }
        Token::RParen => Err(EvalError::Parse {
            msg: "unexpected )".into(),
            pos: tpos,
        }),
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
    let p = expr.pos;
    match &expr.kind {
        ExprKind::Int(n) => Ok(Val::Int(*n)),
        ExprKind::Bool(b) => Ok(Val::Bool(*b)),
        ExprKind::Str(s) => Ok(Val::Str(s.clone())),
        ExprKind::Symbol(name) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable {
                name: name.clone(),
                pos: p,
            })
        }
        ExprKind::List(elems) => {
            if elems.is_empty() {
                return Ok(Val::List(vec![]));
            }

            // Check for special forms
            if let ExprKind::Symbol(op) = &elems[0].kind {
                match op.as_str() {
                    "if" => return eval_if(&elems[1..], env, p),
                    "define" => return eval_define(&elems[1..], env, p),
                    "quote" => return eval_quote(&elems[1..], p),
                    "lambda" => return eval_lambda(&elems[1..], env, p),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "let" => return eval_let(&elems[1..], env, p),
                    "begin" => return eval_begin(&elems[1..], env),
                    "cond" => return eval_cond(&elems[1..], env, p),
                    _ => {}
                }
            }

            // Function call
            let func = eval(&elems[0], env)?;
            let args: Vec<Val> = elems[1..]
                .iter()
                .map(|e| eval(e, env))
                .collect::<Result<Vec<_>, _>>()?;

            apply(&func, &args, p)
        }
    }
}

fn apply(func: &Val, args: &[Val], pos: Pos) -> Result<Val, EvalError> {
    match func {
        Val::Builtin(name) => apply_builtin(name, args, pos),
        Val::Lambda {
            params,
            body,
            env: closure_env,
        } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity {
                    msg: format!("expected {} arguments, got {}", params.len(), args.len()),
                    pos,
                });
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
        _ => Err(EvalError::Type {
            msg: format!("not a procedure: {}", func),
            pos,
        }),
    }
}

fn eval_if(args: &[Expr], env: &Env, pos: Pos) -> Result<Val, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity {
            msg: "if requires 2 or 3 arguments".into(),
            pos,
        });
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

fn eval_define(args: &[Expr], env: &Env, pos: Pos) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity {
            msg: "define requires at least 2 arguments".into(),
            pos,
        });
    }
    match &args[0].kind {
        // (define x expr)
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity {
                    msg: "define requires 2 arguments".into(),
                    pos,
                });
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Val::Void)
        }
        // (define (f params...) body...)
        ExprKind::List(name_and_params) => {
            if name_and_params.is_empty() {
                return Err(EvalError::Parse {
                    msg: "define: empty name list".into(),
                    pos,
                });
            }
            let name = match &name_and_params[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => {
                    return Err(EvalError::Parse {
                        msg: "define: expected symbol for name".into(),
                        pos,
                    })
                }
            };
            let params: Vec<String> = name_and_params[1..]
                .iter()
                .map(|e| match &e.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Parse {
                        msg: "define: expected symbol for parameter".into(),
                        pos: e.pos,
                    }),
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
        _ => Err(EvalError::Parse {
            msg: "define: expected symbol or list".into(),
            pos,
        }),
    }
}

fn eval_quote(args: &[Expr], pos: Pos) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            msg: "quote requires 1 argument".into(),
            pos,
        });
    }
    expr_to_val(&args[0])
}

fn expr_to_val(expr: &Expr) -> Result<Val, EvalError> {
    match &expr.kind {
        ExprKind::Int(n) => Ok(Val::Int(*n)),
        ExprKind::Bool(b) => Ok(Val::Bool(*b)),
        ExprKind::Str(s) => Ok(Val::Str(s.clone())),
        ExprKind::Symbol(s) => Ok(Val::Symbol(s.clone())),
        ExprKind::List(elems) => {
            let vals: Vec<Val> = elems
                .iter()
                .map(expr_to_val)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Val::List(vals))
        }
    }
}

fn eval_lambda(args: &[Expr], env: &Env, pos: Pos) -> Result<Val, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity {
            msg: "lambda requires at least 2 arguments".into(),
            pos,
        });
    }
    let params = match &args[0].kind {
        ExprKind::List(param_exprs) => {
            param_exprs
                .iter()
                .map(|e| match &e.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Parse {
                        msg: "lambda: expected symbol for parameter".into(),
                        pos: e.pos,
                    }),
                })
                .collect::<Result<Vec<_>, _>>()?
        }
        _ => {
            return Err(EvalError::Parse {
                msg: "lambda: expected parameter list".into(),
                pos,
            })
        }
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

fn eval_let(args: &[Expr], env: &Env, pos: Pos) -> Result<Val, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity {
            msg: "let requires bindings and body".into(),
            pos,
        });
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => {
            return Err(EvalError::Parse {
                msg: "let: expected bindings list".into(),
                pos,
            })
        }
    };
    let local_env = new_env(Some(env.clone()));
    for binding in bindings {
        match &binding.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => {
                        return Err(EvalError::Parse {
                            msg: "let: expected symbol".into(),
                            pos: pair[0].pos,
                        })
                    }
                };
                let val = eval(&pair[1], env)?;
                env_set(&local_env, name, val);
            }
            _ => {
                return Err(EvalError::Parse {
                    msg: "let: invalid binding".into(),
                    pos: binding.pos,
                })
            }
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

fn eval_cond(clauses: &[Expr], env: &Env, pos: Pos) -> Result<Val, EvalError> {
    for clause in clauses {
        let parts = match &clause.kind {
            ExprKind::List(p) => p,
            _ => {
                return Err(EvalError::Parse {
                    msg: "cond: expected clause list".into(),
                    pos: clause.pos,
                })
            }
        };
        if parts.is_empty() {
            return Err(EvalError::Parse {
                msg: "cond: empty clause".into(),
                pos: clause.pos,
            });
        }
        // Check for else clause
        if let ExprKind::Symbol(s) = &parts[0].kind {
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

fn apply_builtin(name: &str, args: &[Val], pos: Pos) -> Result<Val, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_int(a, pos)?;
            }
            Ok(Val::Int(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity {
                    msg: "- requires at least 1 argument".into(),
                    pos,
                });
            }
            if args.len() == 1 {
                return Ok(Val::Int(-as_int(&args[0], pos)?));
            }
            let mut result = as_int(&args[0], pos)?;
            for a in &args[1..] {
                result -= as_int(a, pos)?;
            }
            Ok(Val::Int(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_int(a, pos)?;
            }
            Ok(Val::Int(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity {
                    msg: "/ requires at least 1 argument".into(),
                    pos,
                });
            }
            let mut result = as_int(&args[0], pos)?;
            for a in &args[1..] {
                let d = as_int(a, pos)?;
                if d == 0 {
                    return Err(EvalError::Runtime {
                        msg: "division by zero".into(),
                        pos,
                    });
                }
                result /= d;
            }
            Ok(Val::Int(result))
        }
        "<" => {
            let (a, b) = two_ints(args, "<", pos)?;
            Ok(Val::Bool(a < b))
        }
        ">" => {
            let (a, b) = two_ints(args, ">", pos)?;
            Ok(Val::Bool(a > b))
        }
        "=" => {
            let (a, b) = two_ints(args, "=", pos)?;
            Ok(Val::Bool(a == b))
        }
        "<=" => {
            let (a, b) = two_ints(args, "<=", pos)?;
            Ok(Val::Bool(a <= b))
        }
        ">=" => {
            let (a, b) = two_ints(args, ">=", pos)?;
            Ok(Val::Bool(a >= b))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "not requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(!args[0].is_truthy()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity {
                    msg: "cons requires 2 arguments".into(),
                    pos,
                });
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
                return Err(EvalError::Arity {
                    msg: "car requires 1 argument".into(),
                    pos,
                });
            }
            match &args[0] {
                Val::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(EvalError::Type {
                    msg: "car: not a pair".into(),
                    pos,
                }),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "cdr requires 1 argument".into(),
                    pos,
                });
            }
            match &args[0] {
                Val::List(elems) if !elems.is_empty() => Ok(Val::List(elems[1..].to_vec())),
                _ => Err(EvalError::Type {
                    msg: "cdr: not a pair".into(),
                    pos,
                }),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "null? requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(matches!(&args[0], Val::List(e) if e.is_empty())))
        }
        "list" => Ok(Val::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "length requires 1 argument".into(),
                    pos,
                });
            }
            match &args[0] {
                Val::List(elems) => Ok(Val::Int(elems.len() as i64)),
                _ => Err(EvalError::Type {
                    msg: "length: not a list".into(),
                    pos,
                }),
            }
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "string? requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(matches!(&args[0], Val::Str(_))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "number? requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(matches!(&args[0], Val::Int(_))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "boolean? requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(matches!(&args[0], Val::Bool(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "pair? requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(matches!(&args[0], Val::List(e) if !e.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "symbol? requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(matches!(&args[0], Val::Symbol(_))))
        }
        _ => Err(EvalError::UnboundVariable {
            name: name.into(),
            pos,
        }),
    }
}

fn as_int(v: &Val, pos: Pos) -> Result<i64, EvalError> {
    match v {
        Val::Int(n) => Ok(*n),
        _ => Err(EvalError::Type {
            msg: format!("expected number, got {}", v),
            pos,
        }),
    }
}

fn two_ints(args: &[Val], op: &str, pos: Pos) -> Result<(i64, i64), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity {
            msg: format!("{} requires 2 arguments", op),
            pos,
        });
    }
    Ok((as_int(&args[0], pos)?, as_int(&args[1], pos)?))
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
