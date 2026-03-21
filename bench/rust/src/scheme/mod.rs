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
    Char(char),
    List(Vec<Val>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
    Continuation(u64),
    Void,
}

type Output = Rc<RefCell<String>>;

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
            Val::Char(c) => write!(f, "#\\{}", c),
            Val::Lambda { .. } => write!(f, "#<procedure>"),
            Val::Builtin(name) => write!(f, "#<builtin:{}>", name),
            Val::Continuation(_) => write!(f, "#<continuation>"),
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

fn env_update(env: &Env, name: &str, val: Val) -> bool {
    let mut inner = env.borrow_mut();
    if inner.bindings.contains_key(name) {
        inner.bindings.insert(name.to_string(), val);
        true
    } else if let Some(ref parent) = inner.parent {
        env_update(parent, name, val)
    } else {
        false
    }
}

fn default_env() -> Env {
    let env = new_env(None);
    for name in &[
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
        "cons", "car", "cdr", "null?", "list", "length",
        "string?", "number?", "boolean?", "pair?", "symbol?",
        "display", "write", "newline",
        "string-append", "string-length", "substring",
        "string->number", "number->string",
        "symbol->string", "string->symbol",
        "string-ref", "char?", "string-copy",
        "string->list", "list->string", "char->integer", "integer->char",
        "map", "apply", "call/cc",
    ] {
        env_set(&env, name.to_string(), Val::Builtin(name.to_string()));
    }
    env
}

/// Display a value without quotes (for `display`).
fn display_val(v: &Val) -> String {
    match v {
        Val::Str(s) => s.clone(),
        Val::List(elems) => {
            let mut s = String::from("(");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    s.push(' ');
                }
                s.push_str(&display_val(e));
            }
            s.push(')');
            s
        }
        other => other.to_string(),
    }
}

// ---------- Continuation State ----------

struct ContState {
    next_id: u64,
    current_expr_index: usize,
    expr_start_ids: Vec<u64>,
    cont_expr_index: HashMap<u64, usize>,
    signal: Option<(u64, Val)>,
    resume: Option<(u64, Val)>,
}

impl ContState {
    fn new() -> Self {
        ContState {
            next_id: 0,
            current_expr_index: 0,
            expr_start_ids: Vec::new(),
            cont_expr_index: HashMap::new(),
            signal: None,
            resume: None,
        }
    }
}

thread_local! {
    static CONT_STATE: RefCell<ContState> = RefCell::new(ContState::new());
}

fn callcc_exec(proc: &Val, pos: Pos, out: &Output) -> Result<Val, EvalError> {
    let (id, resume_val) = CONT_STATE.with(|cs| {
        let mut state = cs.borrow_mut();
        let id = state.next_id;
        state.next_id += 1;

        if let Some((target_id, _)) = &state.resume {
            if id == *target_id {
                let (_, val) = state.resume.take().unwrap();
                return (id, Some(val));
            }
        }

        let expr_idx = state.current_expr_index;
        state.cont_expr_index.insert(id, expr_idx);
        (id, None)
    });

    if let Some(val) = resume_val {
        return Ok(val);
    }

    let cont_val = Val::Continuation(id);
    apply_func(proc, &[cont_val], pos, out)
}

fn invoke_continuation(id: u64, val: Val) -> EvalError {
    CONT_STATE.with(|cs| {
        cs.borrow_mut().signal = Some((id, val));
    });
    EvalError::ContinuationReturn
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
    Char(char),
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
                        '\\' => {
                            // Character literal #\<char>
                            if i + 2 >= chars.len() {
                                return Err(EvalError::Parse {
                                    msg: "unexpected end of character literal".into(),
                                    pos: p,
                                });
                            }
                            let ch = chars[i + 2];
                            tokens.push((Token::Char(ch), p));
                            i += 3;
                            col += 3;
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
    Char(char),
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
        Token::Char(c) => {
            let c = *c;
            *pos += 1;
            Ok(Expr::new(ExprKind::Char(c), tpos))
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

fn eval(expr: &Expr, env: &Env, out: &Output) -> Result<Val, EvalError> {
    let mut cur_expr = expr.clone();
    let mut cur_env = env.clone();

    loop {
        let p = cur_expr.pos;
        match &cur_expr.kind {
            ExprKind::Int(n) => return Ok(Val::Int(*n)),
            ExprKind::Bool(b) => return Ok(Val::Bool(*b)),
            ExprKind::Str(s) => return Ok(Val::Str(s.clone())),
            ExprKind::Char(c) => return Ok(Val::Char(*c)),
            ExprKind::Symbol(name) => {
                return env_get(&cur_env, name).ok_or_else(|| EvalError::UnboundVariable {
                    name: name.clone(),
                    pos: p,
                });
            }
            ExprKind::List(elems) => {
                if elems.is_empty() {
                    return Ok(Val::List(vec![]));
                }

                // Check for special forms
                if let ExprKind::Symbol(op) = &elems[0].kind {
                    match op.as_str() {
                        "if" => {
                            let args = &elems[1..];
                            if args.len() < 2 || args.len() > 3 {
                                return Err(EvalError::Arity {
                                    msg: "if requires 2 or 3 arguments".into(),
                                    pos: p,
                                });
                            }
                            let cond = eval(&args[0], &cur_env, out)?;
                            if cond.is_truthy() {
                                cur_expr = args[1].clone();
                                continue;
                            } else if args.len() == 3 {
                                cur_expr = args[2].clone();
                                continue;
                            } else {
                                return Ok(Val::Void);
                            }
                        }
                        "define" => return eval_define(&elems[1..], &cur_env, p, out),
                        "set!" => {
                            if elems.len() != 3 {
                                return Err(EvalError::Arity {
                                    msg: "set! requires exactly 2 arguments".to_string(),
                                    pos: p,
                                });
                            }
                            let name = match &elems[1].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => return Err(EvalError::Type {
                                    msg: "set! requires a symbol as first argument".to_string(),
                                    pos: p,
                                }),
                            };
                            let val = eval(&elems[2], &cur_env, out)?;
                            if !env_update(&cur_env, &name, val) {
                                return Err(EvalError::UnboundVariable { name, pos: p });
                            }
                            return Ok(Val::Void);
                        }
                        "quote" => return eval_quote(&elems[1..], p),
                        "lambda" => return eval_lambda(&elems[1..], &cur_env, p),
                        "and" => {
                            let args = &elems[1..];
                            if args.is_empty() {
                                return Ok(Val::Bool(true));
                            }
                            for expr in &args[..args.len() - 1] {
                                let result = eval(expr, &cur_env, out)?;
                                if !result.is_truthy() {
                                    return Ok(result);
                                }
                            }
                            cur_expr = args[args.len() - 1].clone();
                            continue;
                        }
                        "or" => {
                            let args = &elems[1..];
                            if args.is_empty() {
                                return Ok(Val::Bool(false));
                            }
                            for expr in &args[..args.len() - 1] {
                                let result = eval(expr, &cur_env, out)?;
                                if result.is_truthy() {
                                    return Ok(result);
                                }
                            }
                            cur_expr = args[args.len() - 1].clone();
                            continue;
                        }
                        "let" => {
                            let args = &elems[1..];
                            if args.len() < 2 {
                                return Err(EvalError::Arity {
                                    msg: "let requires bindings and body".into(),
                                    pos: p,
                                });
                            }
                            // Named let: (let name ((var init) ...) body...)
                            if let ExprKind::Symbol(name) = &args[0].kind {
                                let name = name.clone();
                                if args.len() < 3 {
                                    return Err(EvalError::Arity {
                                        msg: "named let requires bindings and body".into(),
                                        pos: p,
                                    });
                                }
                                let bindings_expr = match &args[1].kind {
                                    ExprKind::List(b) => b,
                                    _ => return Err(EvalError::Parse {
                                        msg: "let: expected bindings list".into(),
                                        pos: p,
                                    }),
                                };
                                let mut param_names = Vec::new();
                                let mut init_vals = Vec::new();
                                for binding in bindings_expr {
                                    match &binding.kind {
                                        ExprKind::List(pair) if pair.len() == 2 => {
                                            let pname = match &pair[0].kind {
                                                ExprKind::Symbol(s) => s.clone(),
                                                _ => return Err(EvalError::Parse {
                                                    msg: "let: expected symbol".into(),
                                                    pos: pair[0].pos,
                                                }),
                                            };
                                            let val = eval(&pair[1], &cur_env, out)?;
                                            param_names.push(pname);
                                            init_vals.push(val);
                                        }
                                        _ => return Err(EvalError::Parse {
                                            msg: "let: invalid binding".into(),
                                            pos: binding.pos,
                                        }),
                                    }
                                }
                                let body = args[2..].to_vec();
                                // Create env where the named function closes over itself
                                let fn_env = new_env(Some(cur_env.clone()));
                                let lambda = Val::Lambda {
                                    params: param_names.clone(),
                                    rest_param: None,
                                    body: body.clone(),
                                    env: fn_env.clone(),
                                };
                                env_set(&fn_env, name, lambda);

                                // Set up call env with initial bindings
                                let call_env = new_env(Some(fn_env));
                                for (pn, val) in param_names.iter().zip(init_vals.iter()) {
                                    env_set(&call_env, pn.clone(), val.clone());
                                }

                                for expr in &body[..body.len() - 1] {
                                    eval(expr, &call_env, out)?;
                                }
                                cur_expr = body[body.len() - 1].clone();
                                cur_env = call_env;
                                continue;
                            }
                            let bindings = match &args[0].kind {
                                ExprKind::List(b) => b,
                                _ => {
                                    return Err(EvalError::Parse {
                                        msg: "let: expected bindings list".into(),
                                        pos: p,
                                    })
                                }
                            };
                            let local_env = new_env(Some(cur_env.clone()));
                            for binding in bindings {
                                match &binding.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let bname = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => {
                                                return Err(EvalError::Parse {
                                                    msg: "let: expected symbol".into(),
                                                    pos: pair[0].pos,
                                                })
                                            }
                                        };
                                        let val = eval(&pair[1], &cur_env, out)?;
                                        env_set(&local_env, bname, val);
                                    }
                                    _ => {
                                        return Err(EvalError::Parse {
                                            msg: "let: invalid binding".into(),
                                            pos: binding.pos,
                                        })
                                    }
                                }
                            }
                            let body = &args[1..];
                            for expr in &body[..body.len() - 1] {
                                eval(expr, &local_env, out)?;
                            }
                            cur_expr = body[body.len() - 1].clone();
                            cur_env = local_env;
                            continue;
                        }
                        "begin" => {
                            let args = &elems[1..];
                            if args.is_empty() {
                                return Ok(Val::Void);
                            }
                            for expr in &args[..args.len() - 1] {
                                eval(expr, &cur_env, out)?;
                            }
                            cur_expr = args[args.len() - 1].clone();
                            continue;
                        }
                        "cond" => {
                            let clauses = &elems[1..];
                            let mut found = false;
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
                                if let ExprKind::Symbol(s) = &parts[0].kind {
                                    if s == "else" {
                                        for expr in &parts[1..parts.len() - 1] {
                                            eval(expr, &cur_env, out)?;
                                        }
                                        if parts.len() > 1 {
                                            cur_expr = parts[parts.len() - 1].clone();
                                        } else {
                                            return Ok(Val::Void);
                                        }
                                        found = true;
                                        break;
                                    }
                                }
                                let test = eval(&parts[0], &cur_env, out)?;
                                if test.is_truthy() {
                                    if parts.len() == 1 {
                                        return Ok(test);
                                    }
                                    for expr in &parts[1..parts.len() - 1] {
                                        eval(expr, &cur_env, out)?;
                                    }
                                    cur_expr = parts[parts.len() - 1].clone();
                                    found = true;
                                    break;
                                }
                            }
                            if found {
                                continue;
                            }
                            return Ok(Val::Void);
                        }
                        "string-set!" => return eval_string_set(&elems[1..], &cur_env, p, out),
                        _ => {}
                    }
                }

                // Function call
                let func = eval(&elems[0], &cur_env, out)?;
                let args: Vec<Val> = elems[1..]
                    .iter()
                    .map(|e| eval(e, &cur_env, out))
                    .collect::<Result<Vec<_>, _>>()?;

                match func {
                    Val::Builtin(ref name) if name == "call/cc" => {
                        if args.len() != 1 {
                            return Err(EvalError::Arity {
                                msg: "call/cc requires 1 argument".into(),
                                pos: p,
                            });
                        }
                        return callcc_exec(&args[0], p, out);
                    }
                    Val::Continuation(id) => {
                        if args.len() != 1 {
                            return Err(EvalError::Arity {
                                msg: "continuation requires 1 argument".into(),
                                pos: p,
                            });
                        }
                        return Err(invoke_continuation(id, args[0].clone()));
                    }
                    Val::Builtin(name) => return apply_builtin(&name, &args, p, out),
                    Val::Lambda {
                        params,
                        rest_param,
                        body,
                        env: closure_env,
                    } => {
                        if rest_param.is_some() {
                            if args.len() < params.len() {
                                return Err(EvalError::Arity {
                                    msg: format!("expected at least {} arguments, got {}", params.len(), args.len()),
                                    pos: p,
                                });
                            }
                        } else if args.len() != params.len() {
                            return Err(EvalError::Arity {
                                msg: format!("expected {} arguments, got {}", params.len(), args.len()),
                                pos: p,
                            });
                        }
                        let local_env = new_env(Some(closure_env.clone()));
                        for (param, arg) in params.iter().zip(args.iter()) {
                            env_set(&local_env, param.clone(), arg.clone());
                        }
                        if let Some(ref rp) = rest_param {
                            let rest = args[params.len()..].to_vec();
                            env_set(&local_env, rp.clone(), Val::List(rest));
                        }
                        for expr in &body[..body.len().saturating_sub(1)] {
                            eval(expr, &local_env, out)?;
                        }
                        if body.is_empty() {
                            return Ok(Val::Void);
                        }
                        cur_expr = body[body.len() - 1].clone();
                        cur_env = local_env;
                        continue;
                    }
                    _ => {
                        return Err(EvalError::Type {
                            msg: format!("not a procedure: {}", func),
                            pos: p,
                        });
                    }
                }
            }
        }
    }
}

fn apply_func(func: &Val, args: &[Val], pos: Pos, out: &Output) -> Result<Val, EvalError> {
    match func {
        Val::Builtin(name) if name == "call/cc" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "call/cc requires 1 argument".into(),
                    pos,
                });
            }
            callcc_exec(&args[0], pos, out)
        }
        Val::Continuation(id) => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "continuation requires 1 argument".into(),
                    pos,
                });
            }
            Err(invoke_continuation(*id, args[0].clone()))
        }
        Val::Builtin(name) => apply_builtin(name, args, pos, out),
        Val::Lambda {
            params,
            rest_param,
            body,
            env: closure_env,
        } => {
            if rest_param.is_some() {
                if args.len() < params.len() {
                    return Err(EvalError::Arity {
                        msg: format!("expected at least {} arguments, got {}", params.len(), args.len()),
                        pos,
                    });
                }
            } else if args.len() != params.len() {
                return Err(EvalError::Arity {
                    msg: format!("expected {} arguments, got {}", params.len(), args.len()),
                    pos,
                });
            }
            let local_env = new_env(Some(closure_env.clone()));
            for (param, arg) in params.iter().zip(args.iter()) {
                env_set(&local_env, param.clone(), arg.clone());
            }
            if let Some(rp) = rest_param {
                let rest = args[params.len()..].to_vec();
                env_set(&local_env, rp.clone(), Val::List(rest));
            }
            let mut result = Val::Void;
            for expr in body {
                result = eval(expr, &local_env, out)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type {
            msg: format!("not a procedure: {}", func),
            pos,
        }),
    }
}

fn eval_define(args: &[Expr], env: &Env, pos: Pos, out: &Output) -> Result<Val, EvalError> {
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
            let val = eval(&args[1], env, out)?;
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
            let (params, rest_param) = parse_params(&name_and_params[1..], pos)?;
            let body = args[1..].to_vec();
            let lambda = Val::Lambda {
                params,
                rest_param,
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
        ExprKind::Char(c) => Ok(Val::Char(*c)),
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

fn parse_params(param_exprs: &[Expr], pos: Pos) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= param_exprs.len() {
                    return Err(EvalError::Parse {
                        msg: "expected rest parameter after dot".into(),
                        pos,
                    });
                }
                rest_param = Some(match &param_exprs[i + 1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse {
                        msg: "expected symbol for rest parameter".into(),
                        pos: param_exprs[i + 1].pos,
                    }),
                });
                break;
            }
            ExprKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(EvalError::Parse {
                msg: "expected symbol for parameter".into(),
                pos: param_exprs[i].pos,
            }),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_lambda(args: &[Expr], env: &Env, pos: Pos) -> Result<Val, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity {
            msg: "lambda requires at least 2 arguments".into(),
            pos,
        });
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(param_exprs) => parse_params(param_exprs, pos)?,
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
        rest_param,
        body,
        env: env.clone(),
    })
}

fn eval_string_set(_args: &[Expr], _env: &Env, pos: Pos, _out: &Output) -> Result<Val, EvalError> {
    Err(EvalError::Runtime {
        msg: "string-set!: strings are immutable".into(),
        pos,
    })
}

fn apply_builtin(name: &str, args: &[Val], pos: Pos, out: &Output) -> Result<Val, EvalError> {
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
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "display requires 1 argument".into(), pos });
            }
            out.borrow_mut().push_str(&display_val(&args[0]));
            Ok(Val::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "write requires 1 argument".into(), pos });
            }
            out.borrow_mut().push_str(&args[0].to_string());
            Ok(Val::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity { msg: "newline takes 0 arguments".into(), pos });
            }
            out.borrow_mut().push('\n');
            Ok(Val::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Val::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type { msg: "string-append: expected string".into(), pos }),
                }
            }
            Ok(Val::Str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "string-length requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Str(s) => Ok(Val::Int(s.len() as i64)),
                _ => Err(EvalError::Type { msg: "string-length: expected string".into(), pos }),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity { msg: "substring requires 3 arguments".into(), pos });
            }
            let s = match &args[0] {
                Val::Str(s) => s,
                _ => return Err(EvalError::Type { msg: "substring: expected string".into(), pos }),
            };
            let start = as_int(&args[1], pos)? as usize;
            let end = as_int(&args[2], pos)? as usize;
            Ok(Val::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "string->number requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Val::Int(n)),
                    Err(_) => Ok(Val::Bool(false)),
                },
                _ => Err(EvalError::Type { msg: "string->number: expected string".into(), pos }),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "number->string requires 1 argument".into(), pos });
            }
            let n = as_int(&args[0], pos)?;
            Ok(Val::Str(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "symbol->string requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Symbol(s) => Ok(Val::Str(s.clone())),
                _ => Err(EvalError::Type { msg: "symbol->string: expected symbol".into(), pos }),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "string->symbol requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Str(s) => Ok(Val::Symbol(s.clone())),
                _ => Err(EvalError::Type { msg: "string->symbol: expected string".into(), pos }),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "string-ref requires 2 arguments".into(), pos });
            }
            let s = match &args[0] {
                Val::Str(s) => s,
                _ => return Err(EvalError::Type { msg: "string-ref: expected string".into(), pos }),
            };
            let idx = as_int(&args[1], pos)? as usize;
            Ok(Val::Char(s.chars().nth(idx).ok_or_else(|| EvalError::Runtime {
                msg: "string-ref: index out of bounds".into(), pos,
            })?))
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "char? requires 1 argument".into(), pos });
            }
            Ok(Val::Bool(matches!(&args[0], Val::Char(_))))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "string-copy requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Str(s) => Ok(Val::Str(s.clone())),
                _ => Err(EvalError::Type { msg: "string-copy: expected string".into(), pos }),
            }
        }
        "string->list" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "string->list requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Str(s) => Ok(Val::List(s.chars().map(Val::Char).collect())),
                _ => Err(EvalError::Type { msg: "string->list: expected string".into(), pos }),
            }
        }
        "list->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "list->string requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::List(elems) => {
                    let mut s = String::new();
                    for e in elems {
                        match e {
                            Val::Char(c) => s.push(*c),
                            _ => return Err(EvalError::Type { msg: "list->string: expected list of characters".into(), pos }),
                        }
                    }
                    Ok(Val::Str(s))
                }
                _ => Err(EvalError::Type { msg: "list->string: expected list".into(), pos }),
            }
        }
        "char->integer" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "char->integer requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Char(c) => Ok(Val::Int(*c as i64)),
                _ => Err(EvalError::Type { msg: "char->integer: expected character".into(), pos }),
            }
        }
        "integer->char" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "integer->char requires 1 argument".into(), pos });
            }
            let n = as_int(&args[0], pos)?;
            Ok(Val::Char(char::from_u32(n as u32).ok_or_else(|| EvalError::Runtime {
                msg: format!("integer->char: invalid code point {}", n), pos,
            })?))
        }
        "map" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "map requires 2 arguments".into(), pos });
            }
            let func = &args[0];
            match &args[1] {
                Val::List(elems) => {
                    let results: Vec<Val> = elems
                        .iter()
                        .map(|e| apply_func(func, &[e.clone()], pos, out))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Val::List(results))
                }
                _ => Err(EvalError::Type { msg: "map: expected list as second argument".into(), pos }),
            }
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity { msg: "apply requires at least 2 arguments".into(), pos });
            }
            let func = &args[0];
            let last = &args[args.len() - 1];
            let tail = match last {
                Val::List(elems) => elems.clone(),
                _ => return Err(EvalError::Type { msg: "apply: last argument must be a list".into(), pos }),
            };
            let mut all_args: Vec<Val> = args[1..args.len() - 1].to_vec();
            all_args.extend(tail);
            apply_func(func, &all_args, pos, out)
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

fn eval_program(exprs: &[Expr], env: &Env, out: &Output) -> Result<Val, EvalError> {
    CONT_STATE.with(|cs| *cs.borrow_mut() = ContState::new());

    let mut i = 0;
    let mut result = Val::Void;

    while i < exprs.len() {
        CONT_STATE.with(|cs| {
            let mut state = cs.borrow_mut();
            state.current_expr_index = i;
            if state.expr_start_ids.len() <= i {
                let id = state.next_id;
                state.expr_start_ids.push(id);
            }
        });

        match eval(&exprs[i], env, out) {
            Ok(val) => {
                result = val;
                i += 1;
            }
            Err(EvalError::ContinuationReturn) => {
                let (cont_id, cont_val) = CONT_STATE.with(|cs| {
                    cs.borrow_mut().signal.take().unwrap()
                });
                let restart_idx = CONT_STATE.with(|cs| {
                    cs.borrow().cont_expr_index[&cont_id]
                });
                CONT_STATE.with(|cs| {
                    let mut state = cs.borrow_mut();
                    state.next_id = state.expr_start_ids[restart_idx];
                    state.resume = Some((cont_id, cont_val));
                });
                i = restart_idx;
            }
            Err(e) => return Err(e),
        }
    }

    Ok(result)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let env = default_env();
    let out: Output = Rc::new(RefCell::new(String::new()));
    let result = eval_program(&exprs, &env, &out)?;
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    let env = default_env();
    let out: Output = Rc::new(RefCell::new(String::new()));
    let result = eval_program(&exprs, &env, &out)?;
    let output = out.borrow().clone();
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
