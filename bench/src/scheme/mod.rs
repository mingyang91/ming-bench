pub mod error;

pub use error::EvalError;
use error::Span;

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

struct ContinuationData {
    id: u64,
    value: Value,
    remaining_forms: Vec<Value>,
    body_frames: Vec<BodyFrame>,
    callcc_span: Span,
}

#[derive(Debug, Clone)]
struct BodyFrame {
    remaining: Vec<Value>,
    env: Env,
}

thread_local! {
    static OUTPUT: RefCell<String> = RefCell::new(String::new());
    static REMAINING_FORMS: RefCell<Vec<Value>> = RefCell::new(Vec::new());
    static CONT_DATA: RefCell<Option<ContinuationData>> = RefCell::new(None);
    static REPLAY_TARGET: RefCell<Option<(u64, Span, Value)>> = RefCell::new(None);
    static CONT_ID_COUNTER: Cell<u64> = Cell::new(0);
    static GENSYM_COUNTER: Cell<u64> = Cell::new(0);
    static CONT_FRAMES: RefCell<Vec<BodyFrame>> = RefCell::new(Vec::new());
}

fn next_cont_id() -> u64 {
    CONT_ID_COUNTER.with(|c| {
        let id = c.get();
        c.set(id + 1);
        id
    })
}

fn gensym(base: &str) -> String {
    GENSYM_COUNTER.with(|c| {
        let id = c.get();
        c.set(id + 1);
        format!("{}__hyg_{}", base, id)
    })
}

// ---------------------------------------------------------------------------
// Value representation
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum Value {
    Integer(i64, Span),
    Boolean(bool, Span),
    Str(String, Span),
    Symbol(String, Span),
    Char(char, Span),
    List(Vec<Value>, Span),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Value>,
        env: Env,
        span: Span,
    },
    Builtin(String, Span),
    Continuation {
        id: u64,
        remaining_forms: Vec<Value>,
        body_frames: Vec<BodyFrame>,
        span: Span,
    },
    Macro {
        literals: Vec<String>,
        rules: Vec<(Value, Value)>,
        def_env: Env,
        span: Span,
    },
    Void,
}

impl Value {
    fn span(&self) -> Span {
        match self {
            Value::Integer(_, s)
            | Value::Boolean(_, s)
            | Value::Str(_, s)
            | Value::Symbol(_, s)
            | Value::Char(_, s)
            | Value::List(_, s)
            | Value::Builtin(_, s) => *s,
            Value::Lambda { span, .. } | Value::Continuation { span, .. } | Value::Macro { span, .. } => *span,
            Value::Void => Span::default(),
        }
    }

    fn display_scheme(&self) -> String {
        match self {
            Value::Integer(n, _) => n.to_string(),
            Value::Boolean(true, _) => "#t".to_string(),
            Value::Boolean(false, _) => "#f".to_string(),
            Value::Char(c, _) => format!("#\\{}", c),
            Value::Str(s, _) => format!("\"{}\"", s),
            Value::Symbol(s, _) => s.clone(),
            Value::List(elems, _) => {
                let inner: Vec<String> = elems.iter().map(|v| v.display_scheme()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda { .. } | Value::Builtin(..) | Value::Continuation { .. } | Value::Macro { .. } => "#<procedure>".to_string(),
            Value::Void => "".to_string(),
        }
    }

    fn display_output(&self) -> String {
        match self {
            Value::Str(s, _) => s.clone(),
            _ => self.display_scheme(),
        }
    }

    fn write_output(&self) -> String {
        self.display_scheme()
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false, _))
    }
}

// ---------------------------------------------------------------------------
// Environment
// ---------------------------------------------------------------------------

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

fn env_set_existing(env: &Env, name: &str, val: Value) -> bool {
    let mut inner = env.borrow_mut();
    if inner.bindings.contains_key(name) {
        inner.bindings.insert(name.to_string(), val);
        true
    } else if let Some(ref parent) = inner.parent {
        env_set_existing(parent, name, val)
    } else {
        false
    }
}

fn default_env() -> Env {
    let env = new_env(None);
    let sp = Span::default();
    let builtins = [
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
        "cons", "car", "cdr", "null?", "list", "length",
        "string?", "number?", "boolean?", "pair?", "symbol?", "char?",
        "display", "write", "newline", "string-append", "string-length",
        "substring", "string->number", "number->string", "symbol->string",
        "string->symbol", "string-ref", "string-copy", "string->list",
        "list->string", "char->integer", "integer->char", "map", "apply",
        "call/cc", "call-with-current-continuation",
    ];
    for name in &builtins {
        env_set(&env, name.to_string(), Value::Builtin(name.to_string(), sp));
    }
    env
}

// ---------------------------------------------------------------------------
// Tokenizer
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Quote,
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
}

fn tokenize(input: &str) -> Result<Vec<(Token, Span)>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;
    while i < chars.len() {
        let span = Span { line, col };
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
                tokens.push((Token::LParen, span));
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push((Token::RParen, span));
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push((Token::Quote, span));
                i += 1;
                col += 1;
            }
            '"' => {
                i += 1;
                col += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1;
                        col += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            c => {
                                s.push('\\');
                                s.push(c);
                            }
                        }
                    } else {
                        if chars[i] == '\n' {
                            line += 1;
                            col = 0;
                        }
                        s.push(chars[i]);
                    }
                    i += 1;
                    col += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse("unterminated string".into(), span));
                }
                i += 1;
                col += 1;
                tokens.push((Token::Str(s), span));
            }
            '#' => {
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push((Token::Boolean(true), span));
                            i += 2;
                            col += 2;
                        }
                        'f' => {
                            tokens.push((Token::Boolean(false), span));
                            i += 2;
                            col += 2;
                        }
                        '\\' => {
                            // Character literal: #\x or #\space etc.
                            i += 2;
                            col += 2;
                            if i >= chars.len() {
                                return Err(EvalError::Parse("unexpected end of character literal".into(), span));
                            }
                            // Check for named characters
                            let start = i;
                            while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';' | '\'') {
                                i += 1;
                                col += 1;
                            }
                            let name: String = chars[start..i].iter().collect();
                            let ch = match name.as_str() {
                                "space" => ' ',
                                "newline" => '\n',
                                "tab" => '\t',
                                s if s.chars().count() == 1 => s.chars().next().unwrap(),
                                _ => return Err(EvalError::Parse(format!("unknown character name: {}", name), span)),
                            };
                            tokens.push((Token::Char(ch), span));
                        }
                        _ => {
                            return Err(EvalError::Parse(
                                format!("unexpected #{}", chars[i + 1]),
                                span,
                            ))
                        }
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into(), span));
                }
            }
            _ => {
                let start = i;
                while i < chars.len()
                    && !matches!(
                        chars[i],
                        ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';' | '\''
                    )
                {
                    i += 1;
                    col += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push((Token::Integer(n), span));
                } else {
                    tokens.push((Token::Symbol(word), span));
                }
            }
        }
    }
    Ok(tokens)
}

// ---------------------------------------------------------------------------
// Parser — tokens → Value (s-expression)
// ---------------------------------------------------------------------------

fn parse(tokens: &[(Token, Span)], pos: &mut usize) -> Result<Value, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse(
            "unexpected end of input".into(),
            Span::default(),
        ));
    }
    let (token, span) = &tokens[*pos];
    let span = *span;
    match token {
        Token::Integer(n) => {
            let v = Value::Integer(*n, span);
            *pos += 1;
            Ok(v)
        }
        Token::Boolean(b) => {
            let v = Value::Boolean(*b, span);
            *pos += 1;
            Ok(v)
        }
        Token::Str(s) => {
            let v = Value::Str(s.clone(), span);
            *pos += 1;
            Ok(v)
        }
        Token::Symbol(s) => {
            let v = Value::Symbol(s.clone(), span);
            *pos += 1;
            Ok(v)
        }
        Token::Char(c) => {
            let v = Value::Char(*c, span);
            *pos += 1;
            Ok(v)
        }
        Token::Quote => {
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Value::List(
                vec![Value::Symbol("quote".into(), span), inner],
                span,
            ))
        }
        Token::LParen => {
            *pos += 1;
            let mut elems = Vec::new();
            while *pos < tokens.len() && !matches!(tokens[*pos].0, Token::RParen) {
                elems.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into(), span));
            }
            *pos += 1;
            Ok(Value::List(elems, span))
        }
        Token::RParen => Err(EvalError::Parse("unexpected )".into(), span)),
    }
}

fn parse_all(input: &str) -> Result<Vec<Value>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ---------------------------------------------------------------------------
// Evaluator
// ---------------------------------------------------------------------------

fn eval(expr: &Value, env: &Env) -> Result<Value, EvalError> {
    let mut current_expr = expr.clone();
    let mut current_env = env.clone();

    loop {
        match &current_expr {
            Value::Integer(..) | Value::Boolean(..) | Value::Str(..) | Value::Char(..) => {
                return Ok(current_expr);
            }
            Value::Symbol(name, span) => {
                return env_get(&current_env, name)
                    .ok_or_else(|| EvalError::UnboundVariable(name.clone(), *span));
            }
            Value::Void => return Ok(Value::Void),
            Value::Lambda { .. } | Value::Builtin(..) | Value::Continuation { .. } | Value::Macro { .. } => return Ok(current_expr),
            Value::List(elems, span) => {
                let form_span = *span;
                if elems.is_empty() {
                    return Err(EvalError::Parse("empty application".into(), form_span));
                }
                let elems = elems.clone();
                if let Value::Symbol(op, _) = &elems[0] {
                    match op.as_str() {
                        "define" => {
                            return eval_define(&elems[1..], &current_env, form_span);
                        }
                        "set!" => {
                            if elems.len() != 3 {
                                return Err(EvalError::WrongArgCount {
                                    expected: "2".into(),
                                    got: elems.len() - 1,
                                    at: form_span,
                                });
                            }
                            let name = match &elems[1] {
                                Value::Symbol(s, _) => s.clone(),
                                _ => {
                                    return Err(EvalError::Parse(
                                        "set!: expected symbol".into(),
                                        form_span,
                                    ))
                                }
                            };
                            let val = eval(&elems[2], &current_env)?;
                            if !env_set_existing(&current_env, &name, val) {
                                return Err(EvalError::UnboundVariable(name, form_span));
                            }
                            return Ok(Value::Void);
                        }
                        "quote" => {
                            if elems.len() != 2 {
                                return Err(EvalError::WrongArgCount {
                                    expected: "1".into(),
                                    got: elems.len() - 1,
                                    at: form_span,
                                });
                            }
                            return Ok(elems[1].clone());
                        }
                        "lambda" => {
                            return eval_lambda(&elems[1..], &current_env, form_span);
                        }
                        "string-set!" => {
                            return eval_string_set(&elems[1..], &current_env, form_span);
                        }
                        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
                        | "cons" | "car" | "cdr" | "null?" | "list" | "length"
                        | "string?" | "number?" | "boolean?" | "pair?" | "symbol?"
                        | "char?" | "display" | "write" | "newline" | "string-append"
                        | "string-length" | "substring" | "string->number"
                        | "number->string" | "symbol->string" | "string->symbol"
                        | "string-ref" | "string-copy" | "string->list" | "list->string"
                        | "char->integer" | "integer->char" | "map" | "apply" => {
                            return eval_builtin(op, &elems[1..], &current_env, form_span);
                        }
                        // --- TCO forms: update current_expr/current_env and continue ---
                        "if" => {
                            let args = &elems[1..];
                            if args.len() < 2 || args.len() > 3 {
                                return Err(EvalError::Parse(
                                    "if: expected 2 or 3 arguments".into(),
                                    form_span,
                                ));
                            }
                            let cond = eval(&args[0], &current_env)?;
                            if cond.is_truthy() {
                                current_expr = args[1].clone();
                            } else if args.len() == 3 {
                                current_expr = args[2].clone();
                            } else {
                                return Ok(Value::Void);
                            }
                            continue;
                        }
                        "begin" => {
                            let args = &elems[1..];
                            if args.is_empty() {
                                return Ok(Value::Void);
                            }
                            if args.len() > 1 {
                                CONT_FRAMES.with(|cf| cf.borrow_mut().push(BodyFrame {
                                    remaining: args[1..].to_vec(),
                                    env: current_env.clone(),
                                }));
                                for (idx, a) in args[..args.len() - 1].iter().enumerate() {
                                    if idx > 0 {
                                        CONT_FRAMES.with(|cf| {
                                            if let Some(frame) = cf.borrow_mut().last_mut() {
                                                frame.remaining = args[idx + 1..].to_vec();
                                            }
                                        });
                                    }
                                    match eval(a, &current_env) {
                                        Ok(_) => {}
                                        Err(e) => {
                                            CONT_FRAMES.with(|cf| cf.borrow_mut().pop());
                                            return Err(e);
                                        }
                                    }
                                }
                                CONT_FRAMES.with(|cf| cf.borrow_mut().pop());
                            }
                            current_expr = args[args.len() - 1].clone();
                            continue;
                        }
                        "and" => {
                            let args = &elems[1..];
                            if args.is_empty() {
                                return Ok(Value::Boolean(true, Span::default()));
                            }
                            for a in &args[..args.len() - 1] {
                                let val = eval(a, &current_env)?;
                                if !val.is_truthy() {
                                    return Ok(val);
                                }
                            }
                            current_expr = args[args.len() - 1].clone();
                            continue;
                        }
                        "or" => {
                            let args = &elems[1..];
                            if args.is_empty() {
                                return Ok(Value::Boolean(false, Span::default()));
                            }
                            for a in &args[..args.len() - 1] {
                                let val = eval(a, &current_env)?;
                                if val.is_truthy() {
                                    return Ok(val);
                                }
                            }
                            current_expr = args[args.len() - 1].clone();
                            continue;
                        }
                        "cond" => {
                            let args = &elems[1..];
                            let mut found = false;
                            for clause in args {
                                match clause {
                                    Value::List(celems, _) if celems.len() >= 2 => {
                                        let is_else = matches!(&celems[0], Value::Symbol(s, _) if s == "else");
                                        if is_else || eval(&celems[0], &current_env)?.is_truthy()
                                        {
                                            for e in &celems[1..celems.len() - 1] {
                                                eval(e, &current_env)?;
                                            }
                                            current_expr =
                                                celems[celems.len() - 1].clone();
                                            found = true;
                                            break;
                                        }
                                    }
                                    _ => {
                                        return Err(EvalError::Parse(
                                            "cond: invalid clause".into(),
                                            clause.span(),
                                        ));
                                    }
                                }
                            }
                            if found {
                                continue;
                            }
                            return Ok(Value::Void);
                        }
                        "let" => {
                            let args = &elems[1..];
                            if args.len() < 2 {
                                return Err(EvalError::Parse(
                                    "let: expected bindings and body".into(),
                                    form_span,
                                ));
                            }
                            // Named let: (let name ((var init) ...) body ...)
                            if let Value::Symbol(name, _) = &args[0] {
                                if args.len() < 3 {
                                    return Err(EvalError::Parse(
                                        "let: expected bindings and body".into(),
                                        form_span,
                                    ));
                                }
                                let bindings = match &args[1] {
                                    Value::List(b, _) => b,
                                    _ => {
                                        return Err(EvalError::Parse(
                                            "let: expected bindings list".into(),
                                            form_span,
                                        ));
                                    }
                                };
                                let mut params = Vec::new();
                                let mut init_vals = Vec::new();
                                for binding in bindings {
                                    match binding {
                                        Value::List(pair, _) if pair.len() == 2 => {
                                            let pname = match &pair[0] {
                                                Value::Symbol(s, _) => s.clone(),
                                                _ => {
                                                    return Err(EvalError::Parse(
                                                        "let: expected symbol in binding".into(),
                                                        form_span,
                                                    ));
                                                }
                                            };
                                            let val = eval(&pair[1], &current_env)?;
                                            params.push(pname);
                                            init_vals.push(val);
                                        }
                                        _ => {
                                            return Err(EvalError::Parse(
                                                "let: invalid binding".into(),
                                                form_span,
                                            ));
                                        }
                                    }
                                }
                                let body = args[2..].to_vec();
                                let local_env = new_env(Some(current_env.clone()));
                                let lambda = Value::Lambda {
                                    params: params.clone(),
                                    rest_param: None,
                                    body,
                                    env: local_env.clone(),
                                    span: form_span,
                                };
                                env_set(&local_env, name.clone(), lambda);
                                for (p, v) in params.iter().zip(init_vals.iter()) {
                                    env_set(&local_env, p.clone(), v.clone());
                                }
                                let body_exprs = &args[2..];
                                if body_exprs.len() > 1 {
                                    CONT_FRAMES.with(|cf| cf.borrow_mut().push(BodyFrame {
                                        remaining: body_exprs[1..].to_vec(),
                                        env: local_env.clone(),
                                    }));
                                    for (idx, e) in body_exprs[..body_exprs.len() - 1].iter().enumerate() {
                                        if idx > 0 {
                                            CONT_FRAMES.with(|cf| {
                                                if let Some(frame) = cf.borrow_mut().last_mut() {
                                                    frame.remaining = body_exprs[idx + 1..].to_vec();
                                                }
                                            });
                                        }
                                        match eval(e, &local_env) {
                                            Ok(_) => {}
                                            Err(e) => {
                                                CONT_FRAMES.with(|cf| cf.borrow_mut().pop());
                                                return Err(e);
                                            }
                                        }
                                    }
                                    CONT_FRAMES.with(|cf| cf.borrow_mut().pop());
                                }
                                current_expr = body_exprs[body_exprs.len() - 1].clone();
                                current_env = local_env;
                                continue;
                            }
                            // Regular let
                            let bindings = match &args[0] {
                                Value::List(b, _) => b,
                                _ => {
                                    return Err(EvalError::Parse(
                                        "let: expected bindings list".into(),
                                        form_span,
                                    ));
                                }
                            };
                            let local_env = new_env(Some(current_env.clone()));
                            for binding in bindings {
                                match binding {
                                    Value::List(pair, _) if pair.len() == 2 => {
                                        let bname = match &pair[0] {
                                            Value::Symbol(s, _) => s.clone(),
                                            _ => {
                                                return Err(EvalError::Parse(
                                                    "let: expected symbol in binding"
                                                        .into(),
                                                    form_span,
                                                ));
                                            }
                                        };
                                        let val = eval(&pair[1], &current_env)?;
                                        env_set(&local_env, bname, val);
                                    }
                                    _ => {
                                        return Err(EvalError::Parse(
                                            "let: invalid binding".into(),
                                            form_span,
                                        ));
                                    }
                                }
                            }
                            let body = &args[1..];
                            if body.len() > 1 {
                                CONT_FRAMES.with(|cf| cf.borrow_mut().push(BodyFrame {
                                    remaining: body[1..].to_vec(),
                                    env: local_env.clone(),
                                }));
                                for (idx, e) in body[..body.len() - 1].iter().enumerate() {
                                    if idx > 0 {
                                        CONT_FRAMES.with(|cf| {
                                            if let Some(frame) = cf.borrow_mut().last_mut() {
                                                frame.remaining = body[idx + 1..].to_vec();
                                            }
                                        });
                                    }
                                    match eval(e, &local_env) {
                                        Ok(_) => {}
                                        Err(e) => {
                                            CONT_FRAMES.with(|cf| cf.borrow_mut().pop());
                                            return Err(e);
                                        }
                                    }
                                }
                                CONT_FRAMES.with(|cf| cf.borrow_mut().pop());
                            }
                            current_expr = body[body.len() - 1].clone();
                            current_env = local_env;
                            continue;
                        }
                        "define-syntax" => {
                            if elems.len() != 3 {
                                return Err(EvalError::Parse("define-syntax: expected name and transformer".into(), form_span));
                            }
                            let name = match &elems[1] {
                                Value::Symbol(s, _) => s.clone(),
                                _ => return Err(EvalError::Parse("define-syntax: expected symbol".into(), form_span)),
                            };
                            let transformer = &elems[2];
                            let telems = match transformer {
                                Value::List(te, _) => te,
                                _ => return Err(EvalError::Parse("define-syntax: expected syntax-rules".into(), form_span)),
                            };
                            if telems.is_empty() || !matches!(&telems[0], Value::Symbol(s, _) if s == "syntax-rules") {
                                return Err(EvalError::Parse("define-syntax: expected syntax-rules".into(), form_span));
                            }
                            let literals = match &telems[1] {
                                Value::List(lits, _) => lits.iter().filter_map(|l| match l {
                                    Value::Symbol(s, _) => Some(s.clone()),
                                    _ => None,
                                }).collect::<Vec<_>>(),
                                _ => return Err(EvalError::Parse("syntax-rules: expected literal list".into(), form_span)),
                            };
                            let rules: Vec<(Value, Value)> = telems[2..].iter().map(|rule| {
                                match rule {
                                    Value::List(relems, _) if relems.len() == 2 => Ok((relems[0].clone(), relems[1].clone())),
                                    _ => Err(EvalError::Parse("syntax-rules: invalid rule".into(), form_span)),
                                }
                            }).collect::<Result<_, _>>()?;
                            let macro_val = Value::Macro {
                                literals,
                                rules,
                                def_env: current_env.clone(),
                                span: form_span,
                            };
                            env_set(&current_env, name, macro_val);
                            return Ok(Value::Void);
                        }
                        _ => {
                            // Check if this is a macro application
                            if let Some(macro_val) = env_get(&current_env, op) {
                                if let Value::Macro { ref literals, ref rules, ref def_env, .. } = macro_val {
                                    let expanded = expand_macro(&elems, literals, rules, def_env, &current_env, form_span)?;
                                    current_expr = expanded;
                                    continue;
                                }
                            }
                        }
                    }
                }
                // General function application
                let func = eval(&elems[0], &current_env)?;
                let args: Vec<Value> = elems[1..]
                    .iter()
                    .map(|a| eval(a, &current_env))
                    .collect::<Result<_, _>>()?;
                match func {
                    Value::Builtin(ref name, _) if name == "call/cc" || name == "call-with-current-continuation" => {
                        if args.len() != 1 {
                            return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len(), at: form_span });
                        }
                        return eval_callcc(&args[0], form_span);
                    }
                    Value::Continuation { id, ref remaining_forms, ref body_frames, span, .. } => {
                        if args.len() != 1 {
                            return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len(), at: form_span });
                        }
                        return invoke_continuation(id, remaining_forms, body_frames, span, args[0].clone());
                    }
                    Value::Lambda {
                        ref params,
                        ref rest_param,
                        ref body,
                        env: ref lambda_env,
                        ..
                    } => {
                        if rest_param.is_some() {
                            if args.len() < params.len() {
                                return Err(EvalError::WrongArgCount {
                                    expected: format!("at least {}", params.len()),
                                    got: args.len(),
                                    at: form_span,
                                });
                            }
                        } else if args.len() != params.len() {
                            return Err(EvalError::WrongArgCount {
                                expected: params.len().to_string(),
                                got: args.len(),
                                at: form_span,
                            });
                        }
                        let local_env = new_env(Some(lambda_env.clone()));
                        for (p, a) in params.iter().zip(args.iter()) {
                            env_set(&local_env, p.clone(), a.clone());
                        }
                        if let Some(rp) = rest_param {
                            let rest = args[params.len()..].to_vec();
                            env_set(&local_env, rp.clone(), Value::List(rest, form_span));
                        }
                        if body.is_empty() {
                            return Ok(Value::Void);
                        }
                        if body.len() > 1 {
                            CONT_FRAMES.with(|cf| cf.borrow_mut().push(BodyFrame {
                                remaining: body[1..].to_vec(),
                                env: local_env.clone(),
                            }));
                            for (idx, e) in body[..body.len() - 1].iter().enumerate() {
                                if idx > 0 {
                                    CONT_FRAMES.with(|cf| {
                                        if let Some(frame) = cf.borrow_mut().last_mut() {
                                            frame.remaining = body[idx + 1..].to_vec();
                                        }
                                    });
                                }
                                match eval(e, &local_env) {
                                    Ok(_) => {}
                                    Err(e) => {
                                        CONT_FRAMES.with(|cf| cf.borrow_mut().pop());
                                        return Err(e);
                                    }
                                }
                            }
                            CONT_FRAMES.with(|cf| cf.borrow_mut().pop());
                        }
                        current_expr = body[body.len() - 1].clone();
                        current_env = local_env;
                        continue;
                    }
                    Value::Builtin(name, _) => {
                        return eval_builtin_with_values(&name, &args, form_span);
                    }
                    _ => {
                        return Err(EvalError::NotAProcedure(
                            func.display_scheme(),
                            form_span,
                        ));
                    }
                }
            }
        }
    }
}

fn eval_callcc(proc: &Value, form_span: Span) -> Result<Value, EvalError> {
    let id = next_cont_id();
    // Check if this call/cc is the target of a replay (match by id AND span)
    let replay = REPLAY_TARGET.with(|rt| {
        let should_take = rt.borrow().as_ref()
            .map(|(tid, tspan, _)| *tid == id && *tspan == form_span)
            .unwrap_or(false);
        if should_take {
            rt.borrow_mut().take().map(|(_, _, v)| v)
        } else {
            None
        }
    });
    if let Some(val) = replay {
        return Ok(val);
    }
    let remaining = REMAINING_FORMS.with(|rf| rf.borrow().clone());
    let body_frames = CONT_FRAMES.with(|cf| cf.borrow().clone());
    let cont = Value::Continuation {
        id,
        remaining_forms: remaining,
        body_frames,
        span: form_span,
    };
    let saved_depth = CONT_FRAMES.with(|cf| cf.borrow().len());
    let result = apply_function(proc, &[cont], form_span);
    CONT_FRAMES.with(|cf| cf.borrow_mut().truncate(saved_depth));
    match result {
        Ok(val) => Ok(val),
        Err(EvalError::ContinuationReturn) => {
            let matches = CONT_DATA.with(|cd| {
                cd.borrow().as_ref().map(|d| d.id) == Some(id)
            });
            if matches {
                let data = CONT_DATA.with(|cd| cd.borrow_mut().take()).unwrap();
                Ok(data.value)
            } else {
                Err(EvalError::ContinuationReturn)
            }
        }
        Err(e) => Err(e),
    }
}

fn invoke_continuation(id: u64, remaining_forms: &[Value], body_frames: &[BodyFrame], callcc_span: Span, value: Value) -> Result<Value, EvalError> {
    CONT_DATA.with(|cd| {
        *cd.borrow_mut() = Some(ContinuationData {
            id,
            value,
            remaining_forms: remaining_forms.to_vec(),
            body_frames: body_frames.to_vec(),
            callcc_span,
        })
    });
    Err(EvalError::ContinuationReturn)
}

fn apply_function(func: &Value, args: &[Value], call_span: Span) -> Result<Value, EvalError> {
    match func {
        Value::Lambda {
            params, rest_param, body, env, ..
        } => {
            if rest_param.is_some() {
                if args.len() < params.len() {
                    return Err(EvalError::WrongArgCount {
                        expected: format!("at least {}", params.len()),
                        got: args.len(),
                        at: call_span,
                    });
                }
            } else if args.len() != params.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len().to_string(),
                    got: args.len(),
                    at: call_span,
                });
            }
            let local_env = new_env(Some(env.clone()));
            for (p, a) in params.iter().zip(args.iter()) {
                env_set(&local_env, p.clone(), a.clone());
            }
            if let Some(rp) = rest_param {
                let rest = args[params.len()..].to_vec();
                env_set(&local_env, rp.clone(), Value::List(rest, call_span));
            }
            let mut result = Value::Void;
            if body.len() > 1 {
                CONT_FRAMES.with(|cf| cf.borrow_mut().push(BodyFrame {
                    remaining: body[1..].to_vec(),
                    env: local_env.clone(),
                }));
                for (idx, expr) in body.iter().enumerate() {
                    if idx > 0 {
                        CONT_FRAMES.with(|cf| {
                            if let Some(frame) = cf.borrow_mut().last_mut() {
                                frame.remaining = body[idx + 1..].to_vec();
                            }
                        });
                    }
                    match eval(expr, &local_env) {
                        Ok(val) => result = val,
                        Err(e) => {
                            CONT_FRAMES.with(|cf| cf.borrow_mut().pop());
                            return Err(e);
                        }
                    }
                }
                CONT_FRAMES.with(|cf| cf.borrow_mut().pop());
            } else {
                for expr in body {
                    result = eval(expr, &local_env)?;
                }
            }
            Ok(result)
        }
        Value::Builtin(ref name, _) if name == "call/cc" || name == "call-with-current-continuation" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len(), at: call_span });
            }
            eval_callcc(&args[0], call_span)
        }
        Value::Continuation { id, ref remaining_forms, ref body_frames, span, .. } => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len(), at: call_span });
            }
            invoke_continuation(*id, remaining_forms, body_frames, *span, args[0].clone())
        }
        Value::Builtin(name, _) => eval_builtin_with_values(name, args, call_span),
        _ => Err(EvalError::NotAProcedure(
            func.display_scheme(),
            call_span,
        )),
    }
}

fn parse_params(elems: &[Value], span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < elems.len() {
        match &elems[i] {
            Value::Symbol(s, _) if s == "." => {
                if i + 1 >= elems.len() {
                    return Err(EvalError::Parse("expected symbol after dot".into(), span));
                }
                match &elems[i + 1] {
                    Value::Symbol(r, _) => rest_param = Some(r.clone()),
                    _ => return Err(EvalError::Parse("expected symbol after dot".into(), span)),
                }
                break;
            }
            Value::Symbol(s, _) => params.push(s.clone()),
            _ => return Err(EvalError::Parse("expected symbol in params".into(), span)),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_define(args: &[Value], env: &Env, form_span: Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(
            "define: missing arguments".into(),
            form_span,
        ));
    }
    match &args[0] {
        Value::Symbol(name, _) => {
            if args.len() != 2 {
                return Err(EvalError::Parse(
                    "define: expected 2 arguments".into(),
                    form_span,
                ));
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        Value::List(sig, _) => {
            if sig.is_empty() {
                return Err(EvalError::Parse(
                    "define: empty signature".into(),
                    form_span,
                ));
            }
            let name = match &sig[0] {
                Value::Symbol(s, _) => s.clone(),
                _ => {
                    return Err(EvalError::Parse(
                        "define: expected symbol".into(),
                        form_span,
                    ))
                }
            };
            let (params, rest_param) = parse_params(&sig[1..], form_span)?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                env: env.clone(),
                span: form_span,
            };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse(
            "define: invalid syntax".into(),
            form_span,
        )),
    }
}

fn eval_lambda(args: &[Value], env: &Env, form_span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse(
            "lambda: expected params and body".into(),
            form_span,
        ));
    }
    let (params, rest_param) = match &args[0] {
        Value::List(elems, _) => parse_params(elems, form_span)?,
        Value::Symbol(s, _) => {
            // (lambda args body) — single symbol captures all args
            (vec![], Some(s.clone()))
        }
        _ => {
            return Err(EvalError::Parse(
                "lambda: expected parameter list".into(),
                form_span,
            ))
        }
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        env: env.clone(),
        span: form_span,
    })
}

// ---------------------------------------------------------------------------
// Hygienic Macros (define-syntax / syntax-rules)
// ---------------------------------------------------------------------------

fn is_special_form(s: &str) -> bool {
    matches!(s, "define" | "set!" | "quote" | "lambda" | "if" | "begin"
        | "and" | "or" | "cond" | "let" | "define-syntax" | "syntax-rules"
        | "string-set!" | "else")
}

#[derive(Clone)]
enum PatternBinding {
    Single(Value),
    Ellipsis(Vec<Value>),
}

fn collect_pattern_vars(pattern: &Value, literals: &[String], vars: &mut HashSet<String>) {
    match pattern {
        Value::Symbol(s, _) if s != "..." && s != "_" && !literals.contains(s) => {
            vars.insert(s.clone());
        }
        Value::List(elems, _) => {
            for e in elems {
                collect_pattern_vars(e, literals, vars);
            }
        }
        _ => {}
    }
}

fn match_pattern_elems(
    pat: &[Value], input: &[Value], literals: &[String],
) -> Option<HashMap<String, PatternBinding>> {
    let mut bindings = HashMap::new();

    // Check for trailing ellipsis: ... as the last element
    let has_ellipsis = pat.len() >= 2
        && matches!(&pat[pat.len() - 1], Value::Symbol(s, _) if s == "...");

    if has_ellipsis {
        let fixed = &pat[..pat.len() - 2];
        let var_pat = &pat[pat.len() - 2];
        if input.len() < fixed.len() {
            return None;
        }
        for (p, inp) in fixed.iter().zip(input.iter()) {
            match_single(p, inp, literals, &mut bindings)?;
        }
        let var_name = match var_pat {
            Value::Symbol(s, _) => s.clone(),
            _ => return None,
        };
        bindings.insert(var_name, PatternBinding::Ellipsis(input[fixed.len()..].to_vec()));
    } else {
        if pat.len() != input.len() {
            return None;
        }
        for (p, inp) in pat.iter().zip(input.iter()) {
            match_single(p, inp, literals, &mut bindings)?;
        }
    }
    Some(bindings)
}

fn match_single(
    pat: &Value, input: &Value, literals: &[String],
    bindings: &mut HashMap<String, PatternBinding>,
) -> Option<()> {
    match pat {
        Value::Symbol(s, _) if literals.contains(s) => {
            match input {
                Value::Symbol(is, _) if is == s => Some(()),
                _ => None,
            }
        }
        Value::Symbol(s, _) if s == "_" => Some(()),
        Value::Symbol(s, _) => {
            bindings.insert(s.clone(), PatternBinding::Single(input.clone()));
            Some(())
        }
        _ => None,
    }
}

fn collect_template_free_vars(
    template: &Value, pattern_vars: &HashSet<String>, free_vars: &mut HashSet<String>,
) {
    match template {
        Value::Symbol(s, _) if s != "..." && !pattern_vars.contains(s) && !is_special_form(s) => {
            free_vars.insert(s.clone());
        }
        Value::List(elems, _) => {
            for e in elems {
                collect_template_free_vars(e, pattern_vars, free_vars);
            }
        }
        _ => {}
    }
}

fn find_ellipsis_var(tmpl: &Value, bindings: &HashMap<String, PatternBinding>) -> Option<String> {
    match tmpl {
        Value::Symbol(s, _) => {
            if matches!(bindings.get(s.as_str()), Some(PatternBinding::Ellipsis(_))) {
                Some(s.clone())
            } else {
                None
            }
        }
        Value::List(elems, _) => {
            for e in elems {
                if let Some(v) = find_ellipsis_var(e, bindings) {
                    return Some(v);
                }
            }
            None
        }
        _ => None,
    }
}

fn expand_template(
    template: &Value,
    bindings: &HashMap<String, PatternBinding>,
    gensyms: &HashMap<String, String>,
) -> Value {
    match template {
        Value::Symbol(s, span) => {
            if let Some(PatternBinding::Single(val)) = bindings.get(s.as_str()) {
                val.clone()
            } else if let Some(gs) = gensyms.get(s.as_str()) {
                Value::Symbol(gs.clone(), *span)
            } else {
                template.clone()
            }
        }
        Value::List(elems, span) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len()
                    && matches!(&elems[i + 1], Value::Symbol(s, _) if s == "...")
                {
                    let tmpl_elem = &elems[i];
                    if let Some(var_name) = find_ellipsis_var(tmpl_elem, bindings) {
                        if let Some(PatternBinding::Ellipsis(vals)) = bindings.get(&var_name) {
                            for val in vals {
                                let mut sub = bindings.clone();
                                sub.insert(var_name.clone(), PatternBinding::Single(val.clone()));
                                result.push(expand_template(tmpl_elem, &sub, gensyms));
                            }
                        }
                    }
                    i += 2;
                } else {
                    result.push(expand_template(&elems[i], bindings, gensyms));
                    i += 1;
                }
            }
            Value::List(result, *span)
        }
        _ => template.clone(),
    }
}

fn expand_macro(
    input: &[Value],
    literals: &[String],
    rules: &[(Value, Value)],
    def_env: &Env,
    call_env: &Env,
    span: Span,
) -> Result<Value, EvalError> {
    for (pattern, template) in rules {
        let pat_elems = match pattern {
            Value::List(elems, _) => elems,
            _ => continue,
        };
        // Skip first element (macro name) in both pattern and input
        if let Some(bindings) = match_pattern_elems(&pat_elems[1..], &input[1..], literals) {
            let mut pattern_vars = HashSet::new();
            for e in &pat_elems[1..] {
                collect_pattern_vars(e, literals, &mut pattern_vars);
            }
            let mut free_vars = HashSet::new();
            collect_template_free_vars(template, &pattern_vars, &mut free_vars);

            let mut gensym_map = HashMap::new();
            for fv in &free_vars {
                gensym_map.insert(fv.clone(), gensym(fv));
            }

            let expanded = expand_template(template, &bindings, &gensym_map);

            // Bind gensyms to definition-site values
            for (original, gs) in &gensym_map {
                if let Some(val) = env_get(def_env, original) {
                    env_set(call_env, gs.clone(), val);
                }
            }

            return Ok(expanded);
        }
    }
    Err(EvalError::Parse("no matching pattern for macro".into(), span))
}

fn eval_builtin(
    op: &str,
    args: &[Value],
    env: &Env,
    form_span: Span,
) -> Result<Value, EvalError> {
    let sp = form_span;
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_integer(&eval(a, env)?)?;
            }
            Ok(Value::Integer(sum, sp))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount {
                    expected: "at least 1".into(),
                    got: 0,
                    at: sp,
                });
            }
            let first = expect_integer(&eval(&args[0], env)?)?;
            if args.len() == 1 {
                return Ok(Value::Integer(-first, sp));
            }
            let mut result = first;
            for a in &args[1..] {
                result -= expect_integer(&eval(a, env)?)?;
            }
            Ok(Value::Integer(result, sp))
        }
        "*" => {
            let mut prod: i64 = 1;
            for a in args {
                prod *= expect_integer(&eval(a, env)?)?;
            }
            Ok(Value::Integer(prod, sp))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount {
                    expected: "at least 1".into(),
                    got: 0,
                    at: sp,
                });
            }
            let first = expect_integer(&eval(&args[0], env)?)?;
            if args.len() == 1 {
                if first == 0 {
                    return Err(EvalError::DivisionByZero(sp));
                }
                return Ok(Value::Integer(1 / first, sp));
            }
            let mut result = first;
            for a in &args[1..] {
                let d = expect_integer(&eval(a, env)?)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero(sp));
                }
                result /= d;
            }
            Ok(Value::Integer(result, sp))
        }
        "<" => compare_op(args, env, sp, |a, b| a < b),
        ">" => compare_op(args, env, sp, |a, b| a > b),
        "=" => compare_op(args, env, sp, |a, b| a == b),
        "<=" => compare_op(args, env, sp, |a, b| a <= b),
        ">=" => compare_op(args, env, sp, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(!val.is_truthy(), sp))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    expected: "2".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let head = eval(&args[0], env)?;
            let tail = eval(&args[1], env)?;
            match tail {
                Value::List(mut elems, _) => {
                    elems.insert(0, head);
                    Ok(Value::List(elems, sp))
                }
                _ => Err(EvalError::TypeError(
                    "cons: second argument must be a list".into(),
                    sp,
                )),
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            match val {
                Value::List(elems, _) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(EvalError::TypeError(
                    "car: expected non-empty list".into(),
                    sp,
                )),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            match val {
                Value::List(elems, _) if !elems.is_empty() => {
                    Ok(Value::List(elems[1..].to_vec(), sp))
                }
                _ => Err(EvalError::TypeError(
                    "cdr: expected non-empty list".into(),
                    sp,
                )),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(
                matches!(val, Value::List(ref e, _) if e.is_empty()),
                sp,
            ))
        }
        "list" => {
            let vals: Vec<Value> = args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
            Ok(Value::List(vals, sp))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            match val {
                Value::List(elems, _) => Ok(Value::Integer(elems.len() as i64, sp)),
                _ => Err(EvalError::TypeError("length: expected list".into(), sp)),
            }
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(matches!(val, Value::Str(..)), sp))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(matches!(val, Value::Integer(..)), sp))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(matches!(val, Value::Boolean(..)), sp))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(
                matches!(val, Value::List(ref e, _) if !e.is_empty()),
                sp,
            ))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(matches!(val, Value::Symbol(..)), sp))
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            Ok(Value::Boolean(matches!(val, Value::Char(..)), sp))
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            let text = val.display_output();
            OUTPUT.with(|o| o.borrow_mut().push_str(&text));
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            let text = val.write_output();
            OUTPUT.with(|o| o.borrow_mut().push_str(&text));
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::WrongArgCount {
                    expected: "0".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            OUTPUT.with(|o| o.borrow_mut().push('\n'));
            Ok(Value::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                let val = eval(a, env)?;
                match val {
                    Value::Str(s, _) => result.push_str(&s),
                    _ => return Err(EvalError::TypeError("string-append: expected string".into(), sp)),
                }
            }
            Ok(Value::Str(result, sp))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            match val {
                Value::Str(s, _) => Ok(Value::Integer(s.len() as i64, sp)),
                _ => Err(EvalError::TypeError("string-length: expected string".into(), sp)),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::WrongArgCount {
                    expected: "3".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            let start = expect_integer(&eval(&args[1], env)?)? as usize;
            let end = expect_integer(&eval(&args[2], env)?)? as usize;
            match val {
                Value::Str(s, _) => {
                    if end > s.len() || start > end {
                        return Err(EvalError::TypeError("substring: index out of range".into(), sp));
                    }
                    Ok(Value::Str(s[start..end].to_string(), sp))
                }
                _ => Err(EvalError::TypeError("substring: expected string".into(), sp)),
            }
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            match val {
                Value::Str(s, _) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n, sp)),
                    Err(_) => Ok(Value::Boolean(false, sp)),
                },
                _ => Err(EvalError::TypeError("string->number: expected string".into(), sp)),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            let n = expect_integer(&val)?;
            Ok(Value::Str(n.to_string(), sp))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            match val {
                Value::Symbol(s, _) => Ok(Value::Str(s, sp)),
                _ => Err(EvalError::TypeError("symbol->string: expected symbol".into(), sp)),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            match val {
                Value::Str(s, _) => Ok(Value::Symbol(s, sp)),
                _ => Err(EvalError::TypeError("string->symbol: expected string".into(), sp)),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    expected: "2".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            let idx = expect_integer(&eval(&args[1], env)?)? as usize;
            match val {
                Value::Str(s, _) => {
                    if idx >= s.len() {
                        return Err(EvalError::TypeError("string-ref: index out of range".into(), sp));
                    }
                    Ok(Value::Char(s.chars().nth(idx).unwrap(), sp))
                }
                _ => Err(EvalError::TypeError("string-ref: expected string".into(), sp)),
            }
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            match val {
                Value::Str(s, _) => Ok(Value::Str(s, sp)),
                _ => Err(EvalError::TypeError("string-copy: expected string".into(), sp)),
            }
        }
        "string->list" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            match val {
                Value::Str(s, _) => {
                    let chars: Vec<Value> = s.chars().map(|c| Value::Char(c, sp)).collect();
                    Ok(Value::List(chars, sp))
                }
                _ => Err(EvalError::TypeError("string->list: expected string".into(), sp)),
            }
        }
        "list->string" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            match &val {
                Value::List(elems, _) => {
                    let mut s = String::new();
                    for e in elems {
                        match e {
                            Value::Char(c, _) => s.push(*c),
                            _ => return Err(EvalError::TypeError("list->string: expected list of characters".into(), sp)),
                        }
                    }
                    Ok(Value::Str(s, sp))
                }
                _ => Err(EvalError::TypeError("list->string: expected list".into(), sp)),
            }
        }
        "char->integer" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            match val {
                Value::Char(c, _) => Ok(Value::Integer(c as i64, sp)),
                _ => Err(EvalError::TypeError("char->integer: expected character".into(), sp)),
            }
        }
        "integer->char" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    expected: "1".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let val = eval(&args[0], env)?;
            let n = expect_integer(&val)?;
            Ok(Value::Char(char::from_u32(n as u32).unwrap_or('\0'), sp))
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::WrongArgCount {
                    expected: "at least 2".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let func = eval(&args[0], env)?;
            // Evaluate prefix args and final list arg
            let mut all_args = Vec::new();
            for a in &args[1..args.len() - 1] {
                all_args.push(eval(a, env)?);
            }
            let last = eval(&args[args.len() - 1], env)?;
            match last {
                Value::List(elems, _) => all_args.extend(elems),
                _ => return Err(EvalError::TypeError("apply: last argument must be a list".into(), sp)),
            }
            match &func {
                Value::Builtin(name, _) => eval_builtin_with_values(name, &all_args, sp),
                Value::Lambda { .. } => apply_function(&func, &all_args, sp),
                _ => Err(EvalError::NotAProcedure(func.display_scheme(), sp)),
            }
        }
        "map" => {
            if args.len() < 2 {
                return Err(EvalError::WrongArgCount {
                    expected: "2+".into(),
                    got: args.len(),
                    at: sp,
                });
            }
            let func = eval(&args[0], env)?;
            let list_val = eval(&args[1], env)?;
            match list_val {
                Value::List(elems, _) => {
                    let results: Vec<Value> = elems
                        .iter()
                        .map(|e| apply_function(&func, &[e.clone()], sp))
                        .collect::<Result<_, _>>()?;
                    Ok(Value::List(results, sp))
                }
                _ => Err(EvalError::TypeError("map: expected list".into(), sp)),
            }
        }
        _ => Err(EvalError::UnboundVariable(op.to_string(), sp)),
    }
}

fn eval_builtin_with_values(op: &str, args: &[Value], sp: Span) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_integer(a)?;
            }
            Ok(Value::Integer(sum, sp))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: "at least 1".into(), got: 0, at: sp });
            }
            let first = expect_integer(&args[0])?;
            if args.len() == 1 { return Ok(Value::Integer(-first, sp)); }
            let mut result = first;
            for a in &args[1..] { result -= expect_integer(a)?; }
            Ok(Value::Integer(result, sp))
        }
        "*" => {
            let mut prod: i64 = 1;
            for a in args { prod *= expect_integer(a)?; }
            Ok(Value::Integer(prod, sp))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: "at least 1".into(), got: 0, at: sp });
            }
            let first = expect_integer(&args[0])?;
            if args.len() == 1 {
                if first == 0 { return Err(EvalError::DivisionByZero(sp)); }
                return Ok(Value::Integer(1 / first, sp));
            }
            let mut result = first;
            for a in &args[1..] {
                let d = expect_integer(a)?;
                if d == 0 { return Err(EvalError::DivisionByZero(sp)); }
                result /= d;
            }
            Ok(Value::Integer(result, sp))
        }
        "<" => compare_op_values(args, sp, |a, b| a < b),
        ">" => compare_op_values(args, sp, |a, b| a > b),
        "=" => compare_op_values(args, sp, |a, b| a == b),
        "<=" => compare_op_values(args, sp, |a, b| a <= b),
        ">=" => compare_op_values(args, sp, |a, b| a >= b),
        "not" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len(), at: sp }); }
            Ok(Value::Boolean(!args[0].is_truthy(), sp))
        }
        "cons" => {
            if args.len() != 2 { return Err(EvalError::WrongArgCount { expected: "2".into(), got: args.len(), at: sp }); }
            match &args[1] {
                Value::List(elems, _) => {
                    let mut new_elems = vec![args[0].clone()];
                    new_elems.extend(elems.iter().cloned());
                    Ok(Value::List(new_elems, sp))
                }
                _ => Err(EvalError::TypeError("cons: second argument must be a list".into(), sp)),
            }
        }
        "car" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len(), at: sp }); }
            match &args[0] {
                Value::List(elems, _) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(EvalError::TypeError("car: expected non-empty list".into(), sp)),
            }
        }
        "cdr" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len(), at: sp }); }
            match &args[0] {
                Value::List(elems, _) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec(), sp)),
                _ => Err(EvalError::TypeError("cdr: expected non-empty list".into(), sp)),
            }
        }
        "null?" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len(), at: sp }); }
            Ok(Value::Boolean(matches!(args[0], Value::List(ref e, _) if e.is_empty()), sp))
        }
        "list" => Ok(Value::List(args.to_vec(), sp)),
        "length" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len(), at: sp }); }
            match &args[0] {
                Value::List(elems, _) => Ok(Value::Integer(elems.len() as i64, sp)),
                _ => Err(EvalError::TypeError("length: expected list".into(), sp)),
            }
        }
        "display" => {
            if args.len() != 1 { return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len(), at: sp }); }
            let text = args[0].display_output();
            OUTPUT.with(|o| o.borrow_mut().push_str(&text));
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() { return Err(EvalError::WrongArgCount { expected: "0".into(), got: args.len(), at: sp }); }
            OUTPUT.with(|o| o.borrow_mut().push('\n'));
            Ok(Value::Void)
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::WrongArgCount { expected: "at least 2".into(), got: args.len(), at: sp });
            }
            let func = &args[0];
            let mut all_args = Vec::new();
            for a in &args[1..args.len() - 1] {
                all_args.push(a.clone());
            }
            match &args[args.len() - 1] {
                Value::List(elems, _) => all_args.extend(elems.iter().cloned()),
                _ => return Err(EvalError::TypeError("apply: last argument must be a list".into(), sp)),
            }
            match func {
                Value::Builtin(name, _) => eval_builtin_with_values(name, &all_args, sp),
                Value::Lambda { .. } => apply_function(func, &all_args, sp),
                _ => Err(EvalError::NotAProcedure(func.display_scheme(), sp)),
            }
        }
        _ => Err(EvalError::UnboundVariable(op.to_string(), sp)),
    }
}

fn compare_op_values(args: &[Value], sp: Span, cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount { expected: "at least 2".into(), got: args.len(), at: sp });
    }
    let mut prev = expect_integer(&args[0])?;
    for a in &args[1..] {
        let cur = expect_integer(a)?;
        if !cmp(prev, cur) { return Ok(Value::Boolean(false, sp)); }
        prev = cur;
    }
    Ok(Value::Boolean(true, sp))
}

fn eval_string_set(_args: &[Value], _env: &Env, form_span: Span) -> Result<Value, EvalError> {
    Err(EvalError::TypeError("string-set!: strings are immutable".into(), form_span))
}


fn compare_op(
    args: &[Value],
    env: &Env,
    sp: Span,
    cmp: fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".into(),
            got: args.len(),
            at: sp,
        });
    }
    let mut prev = expect_integer(&eval(&args[0], env)?)?;
    for a in &args[1..] {
        let cur = expect_integer(&eval(a, env)?)?;
        if !cmp(prev, cur) {
            return Ok(Value::Boolean(false, sp));
        }
        prev = cur;
    }
    Ok(Value::Boolean(true, sp))
}

fn expect_integer(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n, _) => Ok(*n),
        _ => Err(EvalError::TypeError(
            format!("expected integer, got {}", v.display_scheme()),
            v.span(),
        )),
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    // Clear continuation state
    CONT_DATA.with(|cd| *cd.borrow_mut() = None);
    REPLAY_TARGET.with(|rt| *rt.borrow_mut() = None);
    REMAINING_FORMS.with(|rf| rf.borrow_mut().clear());
    CONT_ID_COUNTER.with(|c| c.set(0));
    GENSYM_COUNTER.with(|c| c.set(0));
    CONT_FRAMES.with(|cf| cf.borrow_mut().clear());


    let exprs = parse_all(input)?;
    let env = default_env();
    let result = eval_top_level(&exprs, &env)?;
    Ok(result.display_scheme())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    OUTPUT.with(|o| o.borrow_mut().clear());
    // Clear continuation state
    CONT_DATA.with(|cd| *cd.borrow_mut() = None);
    REPLAY_TARGET.with(|rt| *rt.borrow_mut() = None);
    REMAINING_FORMS.with(|rf| rf.borrow_mut().clear());
    CONT_ID_COUNTER.with(|c| c.set(0));
    GENSYM_COUNTER.with(|c| c.set(0));
    CONT_FRAMES.with(|cf| cf.borrow_mut().clear());


    let exprs = parse_all(input)?;
    let env = default_env();
    let result = eval_top_level(&exprs, &env)?;
    let output = OUTPUT.with(|o| o.borrow().clone());
    Ok((result.display_scheme(), output))
}

fn eval_top_level(exprs: &[Value], env: &Env) -> Result<Value, EvalError> {
    let mut forms = exprs.to_vec();
    let mut i = 0;
    let mut last = Value::Void;
    loop {
        if i >= forms.len() {
            return Ok(last);
        }
        REMAINING_FORMS.with(|rf| *rf.borrow_mut() = forms[i..].to_vec());
        match eval(&forms[i], env) {
            Ok(val) => {
                last = val;
                i += 1;
            }
            Err(EvalError::ContinuationReturn) => {
                let data = CONT_DATA.with(|cd| cd.borrow_mut().take())
                    .expect("ContinuationReturn without data");
                CONT_FRAMES.with(|cf| cf.borrow_mut().clear());
                REPLAY_TARGET.with(|rt| *rt.borrow_mut() = Some((data.id, data.callcc_span, data.value)));
                CONT_ID_COUNTER.with(|c| c.set(0));
                forms = data.remaining_forms;
                i = 0;
            }
            Err(e) => return Err(e),
        }
    }
}

#[cfg(test)]
mod tests;
