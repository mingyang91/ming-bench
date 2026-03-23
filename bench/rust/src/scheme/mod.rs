pub mod error;

pub use error::EvalError;

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

type BuiltinFn = fn(&[Value]) -> Result<Value, EvalError>;

// ---------- Continuation support ----------

#[derive(Clone)]
struct BodyFrame {
    exprs: Vec<Expr>,
    env: Env,
}

#[derive(Clone)]
struct Winder {
    in_thunk: Value,
    out_thunk: Value,
}

struct ContData {
    id: usize,
    frame: Option<BodyFrame>,
    winders: Vec<Winder>,
}

thread_local! {
    static NEXT_CONT_ID: Cell<usize> = Cell::new(0);
    static CALLCC_OVERRIDE: RefCell<Option<Value>> = RefCell::new(None);
    static BODY_FRAMES: RefCell<Vec<BodyFrame>> = RefCell::new(Vec::new());
    static CONT_REGISTRY: RefCell<HashMap<usize, Rc<ContData>>> = RefCell::new(HashMap::new());
    static CONT_RETURN_VALUE: RefCell<Option<Value>> = RefCell::new(None);
    static CONT_RESULT_VALUE: RefCell<Option<Value>> = RefCell::new(None);
    /// Set of cont_ids whose call/cc is still on the call stack.
    static ACTIVE_CALLCC: RefCell<Vec<usize>> = RefCell::new(Vec::new());
    static WINDERS: RefCell<Vec<Winder>> = RefCell::new(Vec::new());
    static EXCEPTION_HANDLERS: RefCell<Vec<Value>> = RefCell::new(Vec::new());
}

/// Handle result from a non-tail body expression.
/// Catches ContinuationReturn from escaped continuations (invoked outside
/// their original call/cc dynamic extent) and discards them, allowing
/// the body evaluation to continue with remaining expressions.
/// Continuations invoked inside their active call/cc are propagated normally.
fn catch_escaped_continuation(r: Result<Value, EvalError>) -> Result<(), EvalError> {
    match r {
        Ok(_) => Ok(()),
        Err(EvalError::ContinuationReturn { cont_id }) => {
            let is_active = ACTIVE_CALLCC.with(|ac| ac.borrow().contains(&cont_id));
            if is_active {
                // call/cc is still on the stack — propagate so it can catch
                Err(EvalError::ContinuationReturn { cont_id })
            } else {
                // Escaped continuation — discard and continue
                CONT_RETURN_VALUE.with(|v| { v.borrow_mut().take(); });
                Ok(())
            }
        }
        Err(e) => Err(e),
    }
}

fn push_body_frame(exprs: &[Expr], env: &Env) {
    BODY_FRAMES.with(|bf| {
        bf.borrow_mut().push(BodyFrame {
            exprs: exprs.to_vec(),
            env: env.clone(),
        });
    });
}

fn pop_body_frame() {
    BODY_FRAMES.with(|bf| { bf.borrow_mut().pop(); });
}

fn init_cont_state() {
    NEXT_CONT_ID.with(|c| c.set(0));
    CALLCC_OVERRIDE.with(|o| *o.borrow_mut() = None);
    BODY_FRAMES.with(|bf| bf.borrow_mut().clear());
    CONT_REGISTRY.with(|cr| cr.borrow_mut().clear());
    CONT_RETURN_VALUE.with(|v| *v.borrow_mut() = None);
    CONT_RESULT_VALUE.with(|v| *v.borrow_mut() = None);
    ACTIVE_CALLCC.with(|ac| ac.borrow_mut().clear());
    WINDERS.with(|w| w.borrow_mut().clear());
    EXCEPTION_HANDLERS.with(|h| h.borrow_mut().clear());
    SYNTAX_BINDINGS.with(|sb| sb.borrow_mut().clear());
    SYNTAX_DEF_ENV.with(|de| *de.borrow_mut() = None);
}

/// Evaluate a body (sequence of expressions) for continuation replay.
/// Does NOT use TailCall — all expressions are fully evaluated.
fn eval_body_for_replay(body: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for (i, expr) in body.iter().enumerate() {
        push_body_frame(&body[i..], env);
        match eval(expr, env) {
            Ok(v) => {
                pop_body_frame();
                last = v;
            }
            Err(e) => {
                pop_body_frame();
                return Err(e);
            }
        }
    }
    Ok(last)
}

/// Replay a continuation: set override, re-evaluate from the captured frame.
/// Loops to handle the same continuation being invoked again (reentrant).
fn replay_continuation(cont_data: &Rc<ContData>, initial_val: Value) -> Result<Value, EvalError> {
    let frame = cont_data.frame.as_ref()
        .ok_or_else(|| EvalError::Runtime("continuation has no frame".into()))?;
    let mut val = initial_val;
    loop {
        // Rewind: run in-thunks and push winders
        for winder in &cont_data.winders {
            apply_value(&winder.in_thunk, &[])?;
            WINDERS.with(|w| w.borrow_mut().push(winder.clone()));
        }

        CALLCC_OVERRIDE.with(|o| *o.borrow_mut() = Some(val));
        let result = eval_body_for_replay(&frame.exprs, &frame.env);

        // Unwind: pop winders and run out-thunks (reverse order)
        for winder in cont_data.winders.iter().rev() {
            WINDERS.with(|w| w.borrow_mut().pop());
            let _ = apply_value(&winder.out_thunk, &[]);
        }

        match result {
            Ok(v) => return Ok(v),
            Err(EvalError::ContinuationReturn { cont_id }) if cont_id == cont_data.id => {
                // Same continuation invoked again — loop with new value
                val = CONT_RETURN_VALUE.with(|v| v.borrow_mut().take().unwrap());
                continue;
            }
            Err(EvalError::ContinuationResult) => {
                return Ok(CONT_RESULT_VALUE.with(|v| v.borrow_mut().take().unwrap()));
            }
            Err(e) => return Err(e),
        }
    }
}

thread_local! {
    static OUTPUT_BUF: RefCell<String> = RefCell::new(String::new());
}

fn output_write(s: &str) {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().push_str(s));
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (den > 0, gcd(n,d) = 1)
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Char(char),
    Pair(Rc<RefCell<(Value, Value)>>),
    Vector(Rc<RefCell<Vec<Value>>>),
    Builtin(BuiltinFn),
    CallCC,
    Continuation(Rc<ContData>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    Void,
    Values(Vec<Value>),
    Record {
        type_id: u64,
        type_name: String,
        fields: Vec<Value>,
    },
    DynBuiltin(Rc<dyn Fn(&[Value]) -> Result<Value, EvalError>>),
    MacroTransformer { proc: Box<Value>, def_env: Env },
    Syntax(Box<Expr>),
    SyntaxList(Vec<Expr>),
}

static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "Integer({})", n),
            Value::Float(v) => write!(f, "Float({})", v),
            Value::Rational(n, d) => write!(f, "Rational({}/{})", n, d),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Str(s) => write!(f, "Str({})", s),
            Value::Symbol(s) => write!(f, "Symbol({})", s),
            Value::List(l) => write!(f, "List({:?})", l),
            Value::Pair(p) => { let inner = p.borrow(); write!(f, "Pair({:?} . {:?})", inner.0, inner.1) }
            Value::Vector(v) => write!(f, "Vector({:?})", v.borrow()),
            Value::Lambda { params, .. } => write!(f, "Lambda({:?})", params),
            Value::Char(c) => write!(f, "Char({})", c),
            Value::Builtin(_) => write!(f, "Builtin"),
            Value::CallCC => write!(f, "CallCC"),
            Value::Continuation(_) => write!(f, "Continuation"),
            Value::Macro { .. } => write!(f, "Macro"),
            Value::Void => write!(f, "Void"),
            Value::Values(vs) => write!(f, "Values({:?})", vs),
            Value::Record { type_name, fields, .. } => write!(f, "Record({}, {:?})", type_name, fields),
            Value::DynBuiltin(_) => write!(f, "DynBuiltin"),
            Value::MacroTransformer { .. } => write!(f, "MacroTransformer"),
            Value::Syntax(_) => write!(f, "Syntax"),
            Value::SyntaxList(_) => write!(f, "SyntaxList"),
        }
    }
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => {
                let s = format!("{}", f);
                if f.is_finite() && !s.contains('.') { format!("{}.0", s) } else { s }
            }
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::Char(c) => match c {
                ' ' => "#\\space".to_string(),
                '\n' => "#\\newline".to_string(),
                '\t' => "#\\tab".to_string(),
                _ => format!("#\\{}", c),
            },
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(_) => {
                let mut parts = Vec::new();
                let mut current = self.clone();
                let mut tail = None;
                let mut seen = std::collections::HashSet::new();
                loop {
                    match &current {
                        Value::Pair(p) => {
                            let ptr = Rc::as_ptr(p) as usize;
                            if !seen.insert(ptr) {
                                tail = Some("...".to_string());
                                break;
                            }
                            let inner = p.borrow();
                            parts.push(inner.0.display());
                            let next = inner.1.clone();
                            drop(inner);
                            current = next;
                        }
                        Value::List(items) => {
                            // Unpack remaining list items (empty list = proper end)
                            for item in items {
                                parts.push(item.display());
                            }
                            break;
                        }
                        other => {
                            tail = Some(other.display());
                            break;
                        }
                    }
                }
                if let Some(t) = tail {
                    if parts.is_empty() {
                        format!("(() . {})", t)
                    } else {
                        format!("({} . {})", parts.join(" "), t)
                    }
                } else {
                    format!("({})", parts.join(" "))
                }
            }
            Value::Vector(v) => {
                let inner: Vec<String> = v.borrow().iter().map(|v| v.display()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Lambda { .. } | Value::Builtin(_) | Value::DynBuiltin(_) | Value::CallCC | Value::Macro { .. } | Value::MacroTransformer { .. } => "#<procedure>".to_string(),
            Value::Continuation(_) => "#<continuation>".to_string(),
            Value::Void => "".to_string(),
            Value::Values(_) => "".to_string(),
            Value::Record { type_name, .. } => format!("#<{}>", type_name),
            Value::Syntax(_) => "#<syntax>".to_string(),
            Value::SyntaxList(_) => "#<syntax-list>".to_string(),
        }
    }

    /// Format for Scheme `display` — strings without quotes.
    fn display_output(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Macro { .. } | Value::MacroTransformer { .. } => "#<macro>".to_string(),
            other => other.display(),
        }
    }

    /// Format for Scheme `write` — strings with quotes.
    fn write_output(&self) -> String {
        self.display()
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

#[derive(Debug, Clone, Copy)]
struct Span {
    line: usize,
    col: usize,
}

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
    /// A literal value injected by macro expansion (bypasses env lookup).
    Literal(Value),
}

// ---------- Environment ----------

#[derive(Debug, Clone)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

type Env = Rc<RefCell<EnvInner>>;

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

fn env_update(env: &Env, name: &str, val: Value) -> Result<(), EvalError> {
    let has_key = env.borrow().bindings.contains_key(name);
    if has_key {
        env.borrow_mut().bindings.insert(name.to_string(), val);
        Ok(())
    } else {
        let parent = env.borrow().parent.clone();
        if let Some(ref p) = parent {
            env_update(p, name, val)
        } else {
            Err(EvalError::UnboundVariable(name.to_string()))
        }
    }
}

// ---------- Parser ----------

#[derive(Debug, Clone)]
struct Token {
    text: String,
    span: Span,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;
    while i < chars.len() {
        match chars[i] {
            '\n' => { line += 1; col = 1; i += 1; }
            ' ' | '\t' | '\r' => { col += 1; i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1; col += 1;
                }
            }
            '(' => { tokens.push(Token { text: "(".into(), span: Span { line, col } }); col += 1; i += 1; }
            ')' => { tokens.push(Token { text: ")".into(), span: Span { line, col } }); col += 1; i += 1; }
            '\'' => { tokens.push(Token { text: "'".into(), span: Span { line, col } }); col += 1; i += 1; }
            '"' => {
                let start_col = col;
                let start_line = line;
                let mut s = String::new();
                s.push('"');
                i += 1; col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        let next = chars[i + 1];
                        match next {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            _ => { s.push('\\'); s.push(next); }
                        }
                        i += 2; col += 2;
                    } else {
                        if chars[i] == '\n' { line += 1; col = 1; } else { col += 1; }
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                s.push('"');
                if i < chars.len() { i += 1; col += 1; }
                tokens.push(Token { text: s, span: Span { line: start_line, col: start_col } });
            }
            '#' if i + 1 < chars.len() && chars[i + 1] == '\'' => {
                tokens.push(Token { text: "#'".into(), span: Span { line, col } });
                col += 2; i += 2;
            }
            _ => {
                let start_col = col;
                let start = i;
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '\'') {
                    i += 1; col += 1;
                }
                tokens.push(Token { text: chars[start..i].iter().collect(), span: Span { line, col: start_col } });
            }
        }
    }
    tokens
}

fn parse_tokens(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let tok = &tokens[*pos];
    if tok.text == "(" {
        let span = tok.span;
        *pos += 1;
        let mut list = Vec::new();
        while *pos < tokens.len() && tokens[*pos].text != ")" {
            list.push(parse_tokens(tokens, pos)?);
        }
        if *pos >= tokens.len() {
            return Err(EvalError::Parse("missing closing parenthesis".into()));
        }
        *pos += 1;
        Ok(Expr { kind: ExprKind::List(list), span })
    } else if tok.text == "'" {
        let span = tok.span;
        *pos += 1;
        let inner = parse_tokens(tokens, pos)?;
        Ok(Expr { kind: ExprKind::List(vec![
            Expr { kind: ExprKind::Symbol("quote".into()), span },
            inner,
        ]), span })
    } else if tok.text == "#'" {
        let span = tok.span;
        *pos += 1;
        let inner = parse_tokens(tokens, pos)?;
        Ok(Expr { kind: ExprKind::List(vec![
            Expr { kind: ExprKind::Symbol("syntax".into()), span },
            inner,
        ]), span })
    } else if tok.text == ")" {
        Err(EvalError::Parse("unexpected )".into()))
    } else {
        let span = tok.span;
        *pos += 1;
        Ok(parse_atom(&tok.text, span))
    }
}

fn parse_char_literal(name: &str) -> Option<char> {
    match name {
        "space" => Some(' '),
        "newline" => Some('\n'),
        "tab" => Some('\t'),
        _ if name.len() == 1 => Some(name.chars().next().unwrap()),
        _ => None,
    }
}

fn parse_atom(token: &str, span: Span) -> Expr {
    let kind = if token == "#t" {
        ExprKind::Boolean(true)
    } else if token == "#f" {
        ExprKind::Boolean(false)
    } else if token.starts_with("#\\") {
        let name = &token[2..];
        match parse_char_literal(name) {
            Some(c) => ExprKind::Char(c),
            None => ExprKind::Symbol(token.to_string()),
        }
    } else if token.starts_with('"') && token.ends_with('"') {
        ExprKind::Str(token[1..token.len()-1].to_string())
    } else if let Ok(n) = token.parse::<i64>() {
        ExprKind::Integer(n)
    } else if token.contains('.') {
        if let Ok(f) = token.parse::<f64>() {
            ExprKind::Literal(Value::Float(f))
        } else {
            ExprKind::Symbol(token.to_string())
        }
    } else if let Some(slash) = token.find('/') {
        if slash > 0 && slash < token.len() - 1 {
            if let (Ok(n), Ok(d)) = (token[..slash].parse::<i64>(), token[slash+1..].parse::<i64>()) {
                if d != 0 {
                    match make_rational(n, d) {
                        Value::Integer(i) => ExprKind::Integer(i),
                        v => ExprKind::Literal(v),
                    }
                } else { ExprKind::Symbol(token.to_string()) }
            } else { ExprKind::Symbol(token.to_string()) }
        } else { ExprKind::Symbol(token.to_string()) }
    } else {
        ExprKind::Symbol(token.to_string())
    };
    Expr { kind, span }
}

fn parse(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_tokens(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ---------- Evaluator ----------

/// Result of evaluation: either a final value or a tail call to trampoline.
enum Trampoline {
    Done(Value),
    TailCall { expr: Expr, env: Env },
    Guard { var_name: String, clauses: Vec<Expr>, body: Vec<Expr>, env: Env, span: Span },
}

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let mut current_expr = expr.clone();
    let mut current_env = env.clone();
    loop {
        match eval_inner(&current_expr, &current_env)? {
            Trampoline::Done(v) => return Ok(v),
            Trampoline::TailCall { expr: e, env: en } => {
                current_expr = e;
                current_env = en;
            }
            Trampoline::Guard { var_name, clauses, body, env, span } => {
                return eval_guard_loop(var_name, clauses, body, env, span);
            }
        }
    }
}

fn eval_inner(expr: &Expr, env: &Env) -> Result<Trampoline, EvalError> {
    let span = expr.span;
    let result = match &expr.kind {
        ExprKind::Integer(n) => Ok(Trampoline::Done(Value::Integer(*n))),
        ExprKind::Boolean(b) => Ok(Trampoline::Done(Value::Boolean(*b))),
        ExprKind::Str(s) => Ok(Trampoline::Done(Value::Str(s.clone()))),
        ExprKind::Char(c) => Ok(Trampoline::Done(Value::Char(*c))),
        ExprKind::Literal(v) => Ok(Trampoline::Done(v.clone())),
        ExprKind::Symbol(name) => {
            env_get(env, name)
                .map(Trampoline::Done)
                .ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        ExprKind::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".into()).with_position(span.line, span.col));
            }
            // Check for special forms
            if let ExprKind::Symbol(op) = &elems[0].kind {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], env, span).map(Trampoline::Done),
                    "if" => return eval_if_tc(&elems[1..], env, span),
                    "quote" => return eval_quote(&elems[1..], span).map(Trampoline::Done),
                    "lambda" => return eval_lambda(&elems[1..], env, span).map(Trampoline::Done),
                    "and" => return eval_and_tc(&elems[1..], env),
                    "or" => return eval_or_tc(&elems[1..], env),
                    "let" => return eval_let_tc(&elems[1..], env, span),
                    "begin" => return eval_begin_tc(&elems[1..], env),
                    "cond" => return eval_cond_tc(&elems[1..], env),
                    "display" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity("display requires 1 argument".into()).with_position(span.line, span.col));
                        }
                        let val = eval(&elems[1], env)?;
                        output_write(&val.display_output());
                        return Ok(Trampoline::Done(Value::Void));
                    }
                    "write" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity("write requires 1 argument".into()).with_position(span.line, span.col));
                        }
                        let val = eval(&elems[1], env)?;
                        output_write(&val.write_output());
                        return Ok(Trampoline::Done(Value::Void));
                    }
                    "newline" => {
                        if elems.len() != 1 {
                            return Err(EvalError::Arity("newline requires 0 arguments".into()).with_position(span.line, span.col));
                        }
                        output_write("\n");
                        return Ok(Trampoline::Done(Value::Void));
                    }
                    "set!" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Arity("set! requires 2 arguments".into()).with_position(span.line, span.col));
                        }
                        let name = match &elems[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Parse("set!: expected symbol".into()).with_position(span.line, span.col)),
                        };
                        let val = eval(&elems[2], env)?;
                        env_update(env, &name, val)?;
                        return Ok(Trampoline::Done(Value::Void));
                    }
                    "string-set!" => return eval_string_set(&elems[1..], env, span).map(Trampoline::Done),
                    "letrec" => return eval_letrec_tc(&elems[1..], env, span),
                    "letrec*" => return eval_letrec_star_tc(&elems[1..], env, span),
                    "case" => return eval_case_tc(&elems[1..], env, span),
                    "do" => return eval_do(expr, &elems[1..], env, span),
                    "vector-set!" => return eval_vector_set(&elems[1..], env, span).map(Trampoline::Done),
                    "let*" => return eval_letrec_star_tc(&elems[1..], env, span),
                    "define-syntax" => return eval_define_syntax(&elems[1..], env, span).map(Trampoline::Done),
                    "define-record-type" => return eval_define_record_type(&elems[1..], env, span).map(Trampoline::Done),
                    "raise" if env_get(env, "raise").is_none() => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity("raise requires 1 argument".into()).with_position(span.line, span.col));
                        }
                        let val = eval(&elems[1], env)?;
                        // Check if there's a with-exception-handler installed
                        let handler = EXCEPTION_HANDLERS.with(|h| h.borrow().last().cloned());
                        if let Some(handler) = handler {
                            EXCEPTION_HANDLERS.with(|h| h.borrow_mut().pop());
                            let result = apply_value(&handler, &[val.clone()]);
                            EXCEPTION_HANDLERS.with(|h| h.borrow_mut().push(handler));
                            match result {
                                Ok(v) => return Ok(Trampoline::Done(v)),
                                Err(e) => return Err(e),
                            }
                        }
                        return Err(EvalError::SchemeException(val));
                    }
                    "with-exception-handler" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Arity("with-exception-handler requires 2 arguments".into()).with_position(span.line, span.col));
                        }
                        let handler = eval(&elems[1], env)?;
                        let thunk = eval(&elems[2], env)?;
                        EXCEPTION_HANDLERS.with(|h| h.borrow_mut().push(handler));
                        let result = apply_value(&thunk, &[]);
                        EXCEPTION_HANDLERS.with(|h| h.borrow_mut().pop());
                        return result.map(Trampoline::Done);
                    }
                    "guard" => return eval_guard(&elems[1..], env, span),
                    "values" => {
                        let vals: Result<Vec<Value>, _> = elems[1..].iter().map(|a| eval(a, env)).collect();
                        let vals = vals?;
                        return Ok(Trampoline::Done(match vals.len() {
                            1 => vals.into_iter().next().unwrap(),
                            _ => Value::Values(vals),
                        }));
                    }
                    "call-with-values" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Arity("call-with-values requires 2 arguments".into()).with_position(span.line, span.col));
                        }
                        let producer = eval(&elems[1], env)?;
                        let consumer = eval(&elems[2], env)?;
                        let produced = apply_value(&producer, &[])?;
                        let args = match produced {
                            Value::Values(vs) => vs,
                            other => vec![other],
                        };
                        return apply_tc(&consumer, &args);
                    }
                    "dynamic-wind" => {
                        if elems.len() != 4 {
                            return Err(EvalError::Arity("dynamic-wind requires 3 arguments".into()).with_position(span.line, span.col));
                        }
                        let in_thunk = eval(&elems[1], env)?;
                        let body_thunk = eval(&elems[2], env)?;
                        let out_thunk = eval(&elems[3], env)?;

                        apply_value(&in_thunk, &[])?;

                        WINDERS.with(|w| w.borrow_mut().push(Winder {
                            in_thunk: in_thunk.clone(),
                            out_thunk: out_thunk.clone(),
                        }));

                        let body_result = apply_value(&body_thunk, &[]);

                        WINDERS.with(|w| w.borrow_mut().pop());
                        apply_value(&out_thunk, &[])?;

                        return body_result.map(Trampoline::Done);
                    }
                    "syntax-case" => return eval_syntax_case(&elems[1..], env, span).map(Trampoline::Done),
                    "syntax" => return eval_syntax_form(&elems[1..], env, span).map(Trampoline::Done),
                    "with-syntax" => return eval_with_syntax(&elems[1..], env, span).map(Trampoline::Done),
                    _ => {
                        // Check for macro application
                        if let Some(Value::Macro { literals, rules, def_env }) = env_get(env, op) {
                            let expanded = expand_macro(&literals, &rules, &def_env, expr)?;
                            return eval_inner(&expanded, env);
                        }
                        if let Some(Value::MacroTransformer { proc, def_env }) = env_get(env, op) {
                            let stx = Value::Syntax(Box::new(expr.clone()));
                            SYNTAX_DEF_ENV.with(|de| *de.borrow_mut() = Some(def_env.clone()));
                            let result = apply_value(&proc, &[stx]);
                            SYNTAX_DEF_ENV.with(|de| *de.borrow_mut() = None);
                            SYNTAX_BINDINGS.with(|sb| sb.borrow_mut().clear());
                            match result? {
                                Value::Syntax(expanded) => return eval_inner(&expanded, env),
                                other => return Ok(Trampoline::Done(other)),
                            }
                        }
                    }
                }
            }
            // Function application
            let func = eval(&elems[0], env)?;
            let args: Result<Vec<Value>, _> = elems[1..].iter().map(|a| eval(a, env)).collect();
            apply_tc(&func, &args?)
        }
    };
    result.map_err(|e| e.with_position(span.line, span.col))
}

fn apply_tc(func: &Value, args: &[Value]) -> Result<Trampoline, EvalError> {
    match func {
        Value::Lambda { params, rest_param, body, env } => {
            if let Some(ref rp) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} args, got {}", params.len(), args.len()
                    )));
                }
                let local = new_env(Some(env.clone()));
                for (p, a) in params.iter().zip(args) {
                    env_set(&local, p.clone(), a.clone());
                }
                let rest = list_from_vec(args[params.len()..].to_vec());
                env_set(&local, rp.clone(), rest);
                for (i, expr) in body.iter().enumerate() {
                    push_body_frame(&body[i..], &local);
                    if i < body.len() - 1 {
                        let r = eval(expr, &local);
                        pop_body_frame();
                        catch_escaped_continuation(r)?;
                    } else {
                        pop_body_frame();
                        return Ok(Trampoline::TailCall {
                            expr: expr.clone(),
                            env: local,
                        });
                    }
                }
                unreachable!()
            } else {
                if args.len() != params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected {} args, got {}", params.len(), args.len()
                    )));
                }
                let local = new_env(Some(env.clone()));
                for (p, a) in params.iter().zip(args) {
                    env_set(&local, p.clone(), a.clone());
                }
                for (i, expr) in body.iter().enumerate() {
                    push_body_frame(&body[i..], &local);
                    if i < body.len() - 1 {
                        let r = eval(expr, &local);
                        pop_body_frame();
                        catch_escaped_continuation(r)?;
                    } else {
                        pop_body_frame();
                        return Ok(Trampoline::TailCall {
                            expr: expr.clone(),
                            env: local,
                        });
                    }
                }
                unreachable!()
            }
        }
        Value::Builtin(f) => f(args).map(Trampoline::Done),
        Value::DynBuiltin(f) => f(args).map(Trampoline::Done),
        Value::CallCC => {
            if args.len() != 1 {
                return Err(EvalError::Arity("call/cc requires 1 argument".into()));
            }
            // Check for override (re-entry from saved continuation)
            let override_val = CALLCC_OVERRIDE.with(|o| o.borrow_mut().take());
            if let Some(val) = override_val {
                return Ok(Trampoline::Done(val));
            }

            let id = NEXT_CONT_ID.with(|c| { let v = c.get(); c.set(v + 1); v });
            let frame = BODY_FRAMES.with(|bf| bf.borrow().last().cloned());
            let winders = WINDERS.with(|w| w.borrow().clone());
            let cont_data = Rc::new(ContData { id, frame, winders });
            CONT_REGISTRY.with(|cr| cr.borrow_mut().insert(id, cont_data.clone()));

            let cont_val = Value::Continuation(cont_data);

            ACTIVE_CALLCC.with(|ac| ac.borrow_mut().push(id));
            let result = match apply_value(&args[0], &[cont_val]) {
                Ok(v) => Ok(Trampoline::Done(v)),
                Err(EvalError::ContinuationReturn { cont_id }) if cont_id == id => {
                    let val = CONT_RETURN_VALUE.with(|v| v.borrow_mut().take().unwrap());
                    Ok(Trampoline::Done(val))
                }
                Err(e) => Err(e),
            };
            ACTIVE_CALLCC.with(|ac| ac.borrow_mut().retain(|&x| x != id));
            result
        }
        Value::Continuation(cont_data) => {
            if args.is_empty() {
                return Err(EvalError::Arity("continuation requires at least 1 argument".into()));
            }
            let val = if args.len() == 1 {
                args[0].clone()
            } else {
                Value::Values(args.to_vec())
            };
            let is_active = ACTIVE_CALLCC.with(|ac| ac.borrow().contains(&cont_data.id));
            if is_active {
                CONT_RETURN_VALUE.with(|v| *v.borrow_mut() = Some(val));
                Err(EvalError::ContinuationReturn { cont_id: cont_data.id })
            } else if !cont_data.winders.is_empty() {
                // Escaped continuation with winders — replay to re-enter dynamic-wind extents
                replay_continuation(cont_data, val).map(Trampoline::Done)
            } else {
                CONT_RETURN_VALUE.with(|v| *v.borrow_mut() = Some(val));
                Err(EvalError::ContinuationReturn { cont_id: cont_data.id })
            }
        }
        _ => Err(EvalError::Type("not a procedure".into())),
    }
}

fn apply_value(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match apply_tc(func, args)? {
        Trampoline::Done(v) => Ok(v),
        Trampoline::TailCall { expr, env } => eval(&expr, &env),
        Trampoline::Guard { var_name, clauses, body, env, span } => {
            eval_guard_loop(var_name, clauses, body, env, span)
        }
    }
}

fn eval_define(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires arguments".into()).with_position(span.line, span.col));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires a value".into()).with_position(span.line, span.col));
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()).with_position(span.line, span.col));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(n) => n.clone(),
                _ => return Err(EvalError::Parse("define: expected function name".into()).with_position(span.line, span.col)),
            };
            let (params, rest_param) = parse_param_list(&sig[1..], span)?;
            let lambda = Value::Lambda {
                params,
                rest_param,
                body: args[1..].to_vec(),
                env: env.clone(),
            };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse("define: expected symbol or list".into()).with_position(span.line, span.col)),
    }
}

fn eval_if_tc(args: &[Expr], env: &Env, span: Span) -> Result<Trampoline, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()).with_position(span.line, span.col));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        Ok(Trampoline::TailCall { expr: args[1].clone(), env: env.clone() })
    } else if args.len() == 3 {
        Ok(Trampoline::TailCall { expr: args[2].clone(), env: env.clone() })
    } else {
        Ok(Trampoline::Done(Value::Void))
    }
}

fn eval_quote(args: &[Expr], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("quote requires 1 argument".into()).with_position(span.line, span.col));
    }
    Ok(expr_to_value(&args[0]))
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
        ExprKind::Literal(v) => v.clone(),
    }
}

fn parse_param_list(param_exprs: &[Expr], span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 != param_exprs.len() - 1 {
                    return Err(EvalError::Parse("malformed dot notation in parameter list".into()).with_position(span.line, span.col));
                }
                match &param_exprs[i + 1].kind {
                    ExprKind::Symbol(rp) => rest_param = Some(rp.clone()),
                    _ => return Err(EvalError::Parse("expected symbol after dot".into()).with_position(span.line, span.col)),
                }
                break;
            }
            ExprKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(EvalError::Parse("expected parameter name".into()).with_position(span.line, span.col)),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires params and body".into()).with_position(span.line, span.col));
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(param_exprs) => parse_param_list(param_exprs, span)?,
        _ => return Err(EvalError::Parse("lambda: expected parameter list".into()).with_position(span.line, span.col)),
    };
    Ok(Value::Lambda {
        params,
        rest_param,
        body: args[1..].to_vec(),
        env: env.clone(),
    })
}

fn eval_and_tc(args: &[Expr], env: &Env) -> Result<Trampoline, EvalError> {
    if args.is_empty() {
        return Ok(Trampoline::Done(Value::Boolean(true)));
    }
    for a in &args[..args.len() - 1] {
        let result = eval(a, env)?;
        if !result.is_truthy() {
            return Ok(Trampoline::Done(result));
        }
    }
    Ok(Trampoline::TailCall { expr: args.last().unwrap().clone(), env: env.clone() })
}

fn eval_or_tc(args: &[Expr], env: &Env) -> Result<Trampoline, EvalError> {
    if args.is_empty() {
        return Ok(Trampoline::Done(Value::Boolean(false)));
    }
    for a in &args[..args.len() - 1] {
        let result = eval(a, env)?;
        if result.is_truthy() {
            return Ok(Trampoline::Done(result));
        }
    }
    Ok(Trampoline::TailCall { expr: args.last().unwrap().clone(), env: env.clone() })
}

fn eval_let_tc(args: &[Expr], env: &Env, span: Span) -> Result<Trampoline, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into()).with_position(span.line, span.col));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        let bindings_expr = match &args[1].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse("let: expected bindings list".into()).with_position(span.line, span.col)),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings_expr {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env)?);
                    } else {
                        return Err(EvalError::Parse("let: expected variable name".into()).with_position(span.line, span.col));
                    }
                }
                _ => return Err(EvalError::Parse("let: expected (var init) pair".into()).with_position(span.line, span.col)),
            }
        }
        let local = new_env(Some(env.clone()));
        let lambda = Value::Lambda {
            params: params.clone(),
            rest_param: None,
            body: args[2..].to_vec(),
            env: local.clone(),
        };
        env_set(&local, name.clone(), lambda);
        for (p, v) in params.iter().zip(&inits) {
            env_set(&local, p.clone(), v.clone());
        }
        let body = &args[2..];
        for (i, expr) in body.iter().enumerate() {
            push_body_frame(&body[i..], &local);
            if i < body.len() - 1 {
                let r = eval(expr, &local);
                pop_body_frame();
                catch_escaped_continuation(r)?;
            } else {
                pop_body_frame();
                return Ok(Trampoline::TailCall {
                    expr: expr.clone(),
                    env: local,
                });
            }
        }
        unreachable!();
    }
    let bindings_expr = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse("let: expected bindings list".into()).with_position(span.line, span.col)),
    };
    let local = new_env(Some(env.clone()));
    for b in bindings_expr {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], env)?;
                    env_set(&local, s.clone(), val);
                } else {
                    return Err(EvalError::Parse("let: expected variable name".into()).with_position(span.line, span.col));
                }
            }
            _ => return Err(EvalError::Parse("let: expected (var init) pair".into()).with_position(span.line, span.col)),
        }
    }
    let body = &args[1..];
    for (i, expr) in body.iter().enumerate() {
        push_body_frame(&body[i..], &local);
        if i < body.len() - 1 {
            let r = eval(expr, &local);
            pop_body_frame();
            catch_escaped_continuation(r)?;
        } else {
            pop_body_frame();
            return Ok(Trampoline::TailCall {
                expr: expr.clone(),
                env: local,
            });
        }
    }
    Ok(Trampoline::Done(Value::Void))
}

fn eval_begin_tc(args: &[Expr], env: &Env) -> Result<Trampoline, EvalError> {
    if args.is_empty() {
        return Ok(Trampoline::Done(Value::Void));
    }
    for (i, expr) in args.iter().enumerate() {
        push_body_frame(&args[i..], env);
        if i < args.len() - 1 {
            let r = eval(expr, env);
            pop_body_frame();
            catch_escaped_continuation(r)?;
        } else {
            pop_body_frame();
            return Ok(Trampoline::TailCall { expr: expr.clone(), env: env.clone() });
        }
    }
    unreachable!()
}

fn eval_cond_tc(args: &[Expr], env: &Env) -> Result<Trampoline, EvalError> {
    for clause in args {
        match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => {
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        if parts.len() == 1 {
                            return Ok(Trampoline::Done(Value::Void));
                        }
                        for expr in &parts[1..parts.len() - 1] {
                            eval(expr, env)?;
                        }
                        return Ok(Trampoline::TailCall {
                            expr: parts.last().unwrap().clone(),
                            env: env.clone(),
                        });
                    }
                }
                let test = eval(&parts[0], env)?;
                if test.is_truthy() {
                    if parts.len() == 1 {
                        return Ok(Trampoline::Done(test));
                    }
                    for expr in &parts[1..parts.len() - 1] {
                        eval(expr, env)?;
                    }
                    return Ok(Trampoline::TailCall {
                        expr: parts.last().unwrap().clone(),
                        env: env.clone(),
                    });
                }
            }
            _ => return Err(EvalError::Parse("cond: expected clause".into())),
        }
    }
    Ok(Trampoline::Done(Value::Void))
}

fn eval_letrec_tc(args: &[Expr], env: &Env, span: Span) -> Result<Trampoline, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec requires bindings and body".into()).with_position(span.line, span.col));
    }
    let bindings_expr = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse("letrec: expected bindings list".into()).with_position(span.line, span.col)),
    };
    let local = new_env(Some(env.clone()));
    // First pass: bind all variables to void so they're visible
    let mut names = Vec::new();
    for b in bindings_expr {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    names.push(s.clone());
                    env_set(&local, s.clone(), Value::Void);
                } else {
                    return Err(EvalError::Parse("letrec: expected variable name".into()).with_position(span.line, span.col));
                }
            }
            _ => return Err(EvalError::Parse("letrec: expected (var init) pair".into()).with_position(span.line, span.col)),
        }
    }
    // Second pass: evaluate inits in the local env and update bindings
    for (i, b) in bindings_expr.iter().enumerate() {
        if let ExprKind::List(pair) = &b.kind {
            let val = eval(&pair[1], &local)?;
            env_set(&local, names[i].clone(), val);
        }
    }
    let body = &args[1..];
    for (i, expr) in body.iter().enumerate() {
        push_body_frame(&body[i..], &local);
        if i < body.len() - 1 {
            let r = eval(expr, &local);
            pop_body_frame();
            catch_escaped_continuation(r)?;
        } else {
            pop_body_frame();
            return Ok(Trampoline::TailCall { expr: expr.clone(), env: local });
        }
    }
    Ok(Trampoline::Done(Value::Void))
}

fn eval_letrec_star_tc(args: &[Expr], env: &Env, span: Span) -> Result<Trampoline, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec* requires bindings and body".into()).with_position(span.line, span.col));
    }
    let bindings_expr = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse("letrec*: expected bindings list".into()).with_position(span.line, span.col)),
    };
    let local = new_env(Some(env.clone()));
    // Bind sequentially — each init can see previous bindings
    for b in bindings_expr {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], &local)?;
                    env_set(&local, s.clone(), val);
                } else {
                    return Err(EvalError::Parse("letrec*: expected variable name".into()).with_position(span.line, span.col));
                }
            }
            _ => return Err(EvalError::Parse("letrec*: expected (var init) pair".into()).with_position(span.line, span.col)),
        }
    }
    let body = &args[1..];
    for (i, expr) in body.iter().enumerate() {
        push_body_frame(&body[i..], &local);
        if i < body.len() - 1 {
            let r = eval(expr, &local);
            pop_body_frame();
            catch_escaped_continuation(r)?;
        } else {
            pop_body_frame();
            return Ok(Trampoline::TailCall { expr: expr.clone(), env: local });
        }
    }
    Ok(Trampoline::Done(Value::Void))
}

fn eqv_match(val: &Value, datum: &Value) -> bool {
    match (val, datum) {
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        _ => false,
    }
}

fn eval_case_tc(args: &[Expr], env: &Env, span: Span) -> Result<Trampoline, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("case requires at least a key expression".into()).with_position(span.line, span.col));
    }
    let key = eval(&args[0], env)?;
    for clause in &args[1..] {
        match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => {
                // Check for else clause
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        if parts.len() == 1 {
                            return Ok(Trampoline::Done(Value::Void));
                        }
                        for expr in &parts[1..parts.len() - 1] {
                            eval(expr, env)?;
                        }
                        return Ok(Trampoline::TailCall {
                            expr: parts.last().unwrap().clone(),
                            env: env.clone(),
                        });
                    }
                }
                // First element is list of datums
                let datums = match &parts[0].kind {
                    ExprKind::List(d) => d,
                    _ => return Err(EvalError::Parse("case: expected datum list".into()).with_position(span.line, span.col)),
                };
                let matched = datums.iter().any(|d| eqv_match(&key, &expr_to_value(d)));
                if matched {
                    if parts.len() == 1 {
                        return Ok(Trampoline::Done(Value::Void));
                    }
                    for expr in &parts[1..parts.len() - 1] {
                        eval(expr, env)?;
                    }
                    return Ok(Trampoline::TailCall {
                        expr: parts.last().unwrap().clone(),
                        env: env.clone(),
                    });
                }
            }
            _ => return Err(EvalError::Parse("case: expected clause".into()).with_position(span.line, span.col)),
        }
    }
    Ok(Trampoline::Done(Value::Void))
}

fn eval_guard(args: &[Expr], env: &Env, span: Span) -> Result<Trampoline, EvalError> {
    // (guard (var clause ...) body ...)
    if args.is_empty() {
        return Err(EvalError::Arity("guard requires at least 2 arguments".into()).with_position(span.line, span.col));
    }
    let clauses_expr = match &args[0].kind {
        ExprKind::List(elems) if !elems.is_empty() => elems,
        _ => return Err(EvalError::Parse("guard: expected (var clause ...)".into()).with_position(span.line, span.col)),
    };
    let var_name = match &clauses_expr[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse("guard: expected variable name".into()).with_position(span.line, span.col)),
    };
    let clauses = clauses_expr[1..].to_vec();
    let body = args[1..].to_vec();

    Ok(Trampoline::Guard { var_name, clauses, body, env: env.clone(), span })
}

/// Evaluate guard clauses against an exception value, returning the matched result.
fn eval_guard_clauses(var_name: &str, clauses: &[Expr], env: &Env, exn: Value, span: Span) -> Result<Value, EvalError> {
    let clause_env = new_env(Some(env.clone()));
    env_set(&clause_env, var_name.to_string(), exn.clone());
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(celems) if celems.len() >= 2 => {
                if let ExprKind::Symbol(s) = &celems[0].kind {
                    if s == "else" {
                        let mut result = Value::Void;
                        for handler_expr in &celems[1..] {
                            result = eval(handler_expr, &clause_env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&celems[0], &clause_env)?;
                if test.is_truthy() {
                    let mut result = Value::Void;
                    for handler_expr in &celems[1..] {
                        result = eval(handler_expr, &clause_env)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Parse("guard: invalid clause".into()).with_position(span.line, span.col)),
        }
    }
    // No clause matched — re-raise
    Err(EvalError::SchemeException(exn))
}

/// Iteratively evaluate guard bodies, handling tail calls without stack growth.
fn eval_guard_loop(mut var_name: String, mut clauses: Vec<Expr>, mut body: Vec<Expr>, mut env: Env, mut span: Span) -> Result<Value, EvalError> {
    'guard: loop {
        let body_env = new_env(Some(env.clone()));

        // Evaluate all body exprs except the last
        for expr in &body[..body.len().saturating_sub(1)] {
            match eval(expr, &body_env) {
                Ok(_) => {}
                Err(EvalError::SchemeException(exn)) => {
                    return eval_guard_clauses(&var_name, &clauses, &env, exn, span);
                }
                Err(e) => return Err(e),
            }
        }

        // Last body expression — resolve via inline trampoline
        let last = match body.last() {
            Some(e) => e.clone(),
            None => return Ok(Value::Void),
        };
        let mut current_expr = last;
        let mut current_env: Env = body_env;

        loop {
            match eval_inner(&current_expr, &current_env) {
                Ok(Trampoline::Done(v)) => return Ok(v),
                Ok(Trampoline::TailCall { expr: e, env: en }) => {
                    current_expr = e;
                    current_env = en;
                }
                Ok(Trampoline::Guard { var_name: v, clauses: c, body: b, env: e, span: s }) => {
                    // Nested/recursive guard — restart outer loop iteratively
                    var_name = v;
                    clauses = c;
                    body = b;
                    env = e;
                    span = s;
                    continue 'guard;
                }
                Err(EvalError::SchemeException(exn)) => {
                    return eval_guard_clauses(&var_name, &clauses, &env, exn, span);
                }
                Err(e) => return Err(e),
            }
        }
    }
}

fn eval_do(_full_expr: &Expr, args: &[Expr], env: &Env, span: Span) -> Result<Trampoline, EvalError> {
    // (do ((var init step) ...) (test expr ...) body ...)
    if args.len() < 2 {
        return Err(EvalError::Arity("do requires variable bindings and test".into()).with_position(span.line, span.col));
    }
    let var_specs = match &args[0].kind {
        ExprKind::List(v) => v,
        _ => return Err(EvalError::Parse("do: expected variable list".into()).with_position(span.line, span.col)),
    };
    let test_clause = match &args[1].kind {
        ExprKind::List(t) if !t.is_empty() => t,
        _ => return Err(EvalError::Parse("do: expected test clause".into()).with_position(span.line, span.col)),
    };
    let body = &args[2..];

    // Parse variable specs: (var init step?)
    struct VarSpec {
        name: String,
        step: Option<Expr>,
    }
    let mut specs = Vec::new();
    let local = new_env(Some(env.clone()));
    for vs in var_specs {
        match &vs.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                let name = match &parts[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("do: expected variable name".into()).with_position(span.line, span.col)),
                };
                let init = eval(&parts[1], env)?;
                let step = if parts.len() >= 3 { Some(parts[2].clone()) } else { None };
                env_set(&local, name.clone(), init);
                specs.push(VarSpec { name, step });
            }
            _ => return Err(EvalError::Parse("do: expected (var init step)".into()).with_position(span.line, span.col)),
        }
    }

    loop {
        // Test
        let test_val = eval(&test_clause[0], &local)?;
        if test_val.is_truthy() {
            // Evaluate test expressions and return last
            if test_clause.len() == 1 {
                return Ok(Trampoline::Done(Value::Void));
            }
            for expr in &test_clause[1..test_clause.len() - 1] {
                eval(expr, &local)?;
            }
            return Ok(Trampoline::TailCall {
                expr: test_clause.last().unwrap().clone(),
                env: local,
            });
        }
        // Evaluate body (for side effects)
        for expr in body {
            eval(expr, &local)?;
        }
        // Step: evaluate all steps with current values, then update in parallel
        let new_vals: Vec<Option<Value>> = specs.iter().map(|s| {
            match &s.step {
                Some(step_expr) => eval(step_expr, &local).map(Some),
                None => Ok(None),
            }
        }).collect::<Result<Vec<_>, _>>()?;
        for (spec, new_val) in specs.iter().zip(new_vals) {
            if let Some(v) = new_val {
                env_set(&local, spec.name.clone(), v);
            }
        }
    }
}

fn eval_vector_set(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("vector-set! requires 3 arguments".into()).with_position(span.line, span.col));
    }
    let vec_val = eval(&args[0], env)?;
    let idx_val = eval(&args[1], env)?;
    let new_val = eval(&args[2], env)?;
    let idx = match idx_val {
        Value::Integer(n) => n as usize,
        _ => return Err(EvalError::Type("vector-set!: expected integer index".into()).with_position(span.line, span.col)),
    };
    match vec_val {
        Value::Vector(v) => {
            let mut inner = v.borrow_mut();
            if idx >= inner.len() {
                return Err(EvalError::Runtime("vector-set!: index out of range".into()).with_position(span.line, span.col));
            }
            inner[idx] = new_val;
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("vector-set!: expected vector".into()).with_position(span.line, span.col)),
    }
}

fn eval_string_set(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("string-set! requires 3 arguments".into()));
    }
    // String literals are immutable
    if matches!(args[0].kind, ExprKind::Str(_)) {
        return Err(EvalError::Runtime("string-set!: strings are immutable".into()).with_position(span.line, span.col));
    }
    let idx_val = eval(&args[1], env)?;
    let ch_val = eval(&args[2], env)?;
    let idx = match idx_val {
        Value::Integer(n) => n as usize,
        _ => return Err(EvalError::Type("string-set!: expected integer index".into())),
    };
    let ch = match ch_val {
        Value::Char(c) => c,
        _ => return Err(EvalError::Type("string-set!: expected char".into())),
    };
    // The first arg must be a symbol referencing a mutable string variable
    if let ExprKind::Symbol(ref name) = args[0].kind {
        let s = match env_get(env, name) {
            Some(Value::Str(s)) => s,
            Some(_) => return Err(EvalError::Type("string-set!: expected string".into())),
            None => return Err(EvalError::UnboundVariable(name.clone())),
        };
        let mut chars: Vec<char> = s.chars().collect();
        if idx >= chars.len() {
            return Err(EvalError::Runtime("string-set!: index out of range".into()));
        }
        chars[idx] = ch;
        let new_s: String = chars.into_iter().collect();
        env_update(env, name, Value::Str(new_s))
            .map_err(|e| e.with_position(span.line, span.col))?;
        Ok(Value::Void)
    } else {
        // Non-symbol, non-literal expression (e.g. function call result) — can't mutate
        Err(EvalError::Runtime("string-set!: strings are immutable".into()).with_position(span.line, span.col))
    }
}

// ---------- Macros (define-syntax / syntax-rules) ----------

thread_local! {
    static GENSYM_COUNTER: Cell<usize> = Cell::new(0);
    static SYNTAX_BINDINGS: RefCell<Vec<HashMap<String, PatBinding>>> = RefCell::new(Vec::new());
    static SYNTAX_DEF_ENV: RefCell<Option<Env>> = RefCell::new(None);
}

fn gensym(prefix: &str) -> String {
    GENSYM_COUNTER.with(|c| {
        let id = c.get();
        c.set(id + 1);
        format!("{}##g{}", prefix, id)
    })
}

fn eval_define_syntax(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("define-syntax requires 2 arguments".into()).with_position(span.line, span.col));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse("define-syntax: expected name".into()).with_position(span.line, span.col)),
    };
    // Check if it's (syntax-rules ...) or a transformer expression (e.g. lambda)
    let is_syntax_rules = match &args[1].kind {
        ExprKind::List(elems) => !elems.is_empty() && matches!(&elems[0].kind, ExprKind::Symbol(s) if s == "syntax-rules"),
        _ => false,
    };
    if is_syntax_rules {
        let sr = match &args[1].kind {
            ExprKind::List(elems) => elems,
            _ => unreachable!(),
        };
        let literals = match &sr[1].kind {
            ExprKind::List(lits) => lits.iter().filter_map(|e| {
                if let ExprKind::Symbol(s) = &e.kind { Some(s.clone()) } else { None }
            }).collect::<Vec<_>>(),
            _ => return Err(EvalError::Parse("syntax-rules: expected literals list".into()).with_position(span.line, span.col)),
        };
        let mut rules = Vec::new();
        for rule_expr in &sr[2..] {
            match &rule_expr.kind {
                ExprKind::List(parts) if parts.len() == 2 => {
                    rules.push((parts[0].clone(), parts[1].clone()));
                }
                _ => return Err(EvalError::Parse("syntax-rules: expected (pattern template)".into()).with_position(span.line, span.col)),
            }
        }
        env_set(env, name, Value::Macro { literals, rules, def_env: env.clone() });
    } else {
        // Transformer expression (e.g. lambda)
        let proc = eval(&args[1], env)?;
        env_set(env, name, Value::MacroTransformer { proc: Box::new(proc), def_env: env.clone() });
    }
    Ok(Value::Void)
}

#[derive(Clone)]
enum PatBinding {
    One(Expr),
    Many(Vec<Expr>),
}

fn match_pattern(
    pattern: &Expr,
    form: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, PatBinding>,
) -> bool {
    match &pattern.kind {
        ExprKind::Symbol(name) if name == "_" => true,
        ExprKind::Symbol(name) if literals.contains(name) => {
            matches!(&form.kind, ExprKind::Symbol(s) if s == name)
        }
        ExprKind::Symbol(name) => {
            bindings.insert(name.clone(), PatBinding::One(form.clone()));
            true
        }
        ExprKind::List(pelems) => {
            if let ExprKind::List(felems) = &form.kind {
                match_list_elems(pelems, felems, literals, bindings)
            } else {
                false
            }
        }
        ExprKind::Integer(n) => matches!(&form.kind, ExprKind::Integer(m) if m == n),
        ExprKind::Boolean(b) => matches!(&form.kind, ExprKind::Boolean(b2) if b2 == b),
        ExprKind::Str(s) => matches!(&form.kind, ExprKind::Str(s2) if s2 == s),
        _ => false,
    }
}

fn match_list_elems(
    pelems: &[Expr],
    felems: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, PatBinding>,
) -> bool {
    // Find ellipsis position
    let ellipsis_pos = pelems.iter().position(|e| matches!(&e.kind, ExprKind::Symbol(s) if s == "..."));
    if let Some(ep) = ellipsis_pos {
        if ep == 0 { return false; }
        let before = &pelems[..ep - 1];
        let ellipsis_pat = &pelems[ep - 1];
        let after = &pelems[ep + 1..];
        let min_required = before.len() + after.len();
        if felems.len() < min_required { return false; }
        for (p, f) in before.iter().zip(felems.iter()) {
            if !match_pattern(p, f, literals, bindings) { return false; }
        }
        let after_start = felems.len() - after.len();
        for (p, f) in after.iter().zip(felems[after_start..].iter()) {
            if !match_pattern(p, f, literals, bindings) { return false; }
        }
        let ellipsis_forms = &felems[before.len()..after_start];
        if let ExprKind::Symbol(name) = &ellipsis_pat.kind {
            bindings.insert(name.clone(), PatBinding::Many(ellipsis_forms.to_vec()));
        }
        true
    } else {
        if pelems.len() != felems.len() { return false; }
        for (p, f) in pelems.iter().zip(felems.iter()) {
            if !match_pattern(p, f, literals, bindings) { return false; }
        }
        true
    }
}

fn expand_macro(
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &Env,
    form: &Expr,
) -> Result<Expr, EvalError> {
    let form_elems = match &form.kind {
        ExprKind::List(elems) => elems,
        _ => return Err(EvalError::Runtime("macro: expected list form".into())),
    };
    for (pattern, template) in rules {
        let pat_elems = match &pattern.kind {
            ExprKind::List(elems) => elems,
            _ => continue,
        };
        let mut bindings = HashMap::new();
        // Match: skip the first element (macro name) in both pattern and form
        if match_list_elems(&pat_elems[1..], &form_elems[1..], literals, &mut bindings) {
            let mut renames = HashMap::new();
            return Ok(expand_template(template, &bindings, &mut renames, def_env));
        }
    }
    Err(EvalError::Runtime("no matching macro pattern".into()))
}

// ---------- L21: set-car! / set-cdr! ----------

fn builtin_set_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("set-car! requires 2 arguments".into()));
    }
    match &args[0] {
        Value::Pair(p) => {
            p.borrow_mut().0 = args[1].clone();
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("set-car!: expected pair".into())),
    }
}

fn builtin_set_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("set-cdr! requires 2 arguments".into()));
    }
    match &args[0] {
        Value::Pair(p) => {
            p.borrow_mut().1 = args[1].clone();
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("set-cdr!: expected pair".into())),
    }
}

fn builtin_caar(args: &[Value]) -> Result<Value, EvalError> {
    builtin_car(&[builtin_car(args)?])
}
fn builtin_cadr(args: &[Value]) -> Result<Value, EvalError> {
    builtin_car(&[builtin_cdr(args)?])
}
fn builtin_cdar(args: &[Value]) -> Result<Value, EvalError> {
    builtin_cdr(&[builtin_car(args)?])
}
fn builtin_cddr(args: &[Value]) -> Result<Value, EvalError> {
    builtin_cdr(&[builtin_cdr(args)?])
}
fn builtin_caddr(args: &[Value]) -> Result<Value, EvalError> {
    builtin_car(&[builtin_cddr(args)?])
}
fn builtin_cdddr(args: &[Value]) -> Result<Value, EvalError> {
    builtin_cdr(&[builtin_cddr(args)?])
}
fn builtin_cadddr(args: &[Value]) -> Result<Value, EvalError> {
    builtin_car(&[builtin_cdddr(args)?])
}

// ---------- L20: define-record-type ----------

fn eval_define_record_type(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    // (define-record-type <name> (constructor field...) predicate (field accessor)...)
    if args.len() < 3 {
        return Err(EvalError::Arity("define-record-type requires at least 3 arguments".into()).with_position(span.line, span.col));
    }
    let type_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse("define-record-type: expected type name".into()).with_position(span.line, span.col)),
    };

    // Parse constructor: (constructor-name field-name ...)
    let (ctor_name, ctor_fields) = match &args[1].kind {
        ExprKind::List(elems) if !elems.is_empty() => {
            let name = match &elems[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse("define-record-type: expected constructor name".into()).with_position(span.line, span.col)),
            };
            let fields: Vec<String> = elems[1..].iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse("define-record-type: expected field name".into()).with_position(span.line, span.col)),
            }).collect::<Result<_, _>>()?;
            (name, fields)
        }
        _ => return Err(EvalError::Parse("define-record-type: expected constructor form".into()).with_position(span.line, span.col)),
    };

    // Parse predicate name
    let pred_name = match &args[2].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse("define-record-type: expected predicate name".into()).with_position(span.line, span.col)),
    };

    // Parse field specs: (field-name accessor-name)
    let mut field_accessors: Vec<(String, String)> = Vec::new();
    for arg in &args[3..] {
        match &arg.kind {
            ExprKind::List(elems) if elems.len() >= 2 => {
                let field = match &elems[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("define-record-type: expected field name".into()).with_position(span.line, span.col)),
                };
                let accessor = match &elems[1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("define-record-type: expected accessor name".into()).with_position(span.line, span.col)),
                };
                field_accessors.push((field, accessor));
            }
            _ => return Err(EvalError::Parse("define-record-type: expected field spec".into()).with_position(span.line, span.col)),
        }
    }

    // Allocate a unique type id
    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);

    // Build field-name-to-ctor-index map for accessor lookup
    let ctor_field_map: HashMap<String, usize> = ctor_fields.iter().enumerate().map(|(i, f)| (f.clone(), i)).collect();

    // Define constructor
    let ctor_field_count = ctor_fields.len();
    let ctor_type_name = type_name.clone();
    env_set(env, ctor_name, Value::DynBuiltin(Rc::new(move |args: &[Value]| {
        if args.len() != ctor_field_count {
            return Err(EvalError::Arity(format!("record constructor expects {} arguments, got {}", ctor_field_count, args.len())));
        }
        Ok(Value::Record {
            type_id,
            type_name: ctor_type_name.clone(),
            fields: args.to_vec(),
        })
    })));

    // Define predicate
    env_set(env, pred_name, Value::DynBuiltin(Rc::new(move |args: &[Value]| {
        if args.len() != 1 {
            return Err(EvalError::Arity("record predicate requires 1 argument".into()));
        }
        Ok(Value::Boolean(matches!(&args[0], Value::Record { type_id: tid, .. } if *tid == type_id)))
    })));

    // Define accessors
    for (field_name, accessor_name) in &field_accessors {
        let idx = match ctor_field_map.get(field_name) {
            Some(&i) => i,
            None => return Err(EvalError::Parse(format!("define-record-type: field {} not in constructor", field_name)).with_position(span.line, span.col)),
        };
        let acc_name = accessor_name.clone();
        env_set(env, accessor_name.clone(), Value::DynBuiltin(Rc::new(move |args: &[Value]| {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{} requires 1 argument", acc_name)));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields, .. } if *tid == type_id => {
                    Ok(fields[idx].clone())
                }
                _ => Err(EvalError::Type(format!("{}: expected record of correct type", acc_name))),
            }
        })));
    }

    Ok(Value::Void)
}

fn is_special_form(name: &str) -> bool {
    matches!(name,
        "define" | "if" | "quote" | "lambda" | "and" | "or" | "let" | "begin"
        | "cond" | "set!" | "display" | "write" | "newline" | "string-set!"
        | "define-syntax" | "syntax-rules" | "letrec" | "letrec*" | "case" | "do"
        | "vector-set!" | "let*" | "raise" | "guard" | "with-exception-handler"
        | "define-record-type" | "syntax-case" | "syntax" | "with-syntax"
    )
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, PatBinding>,
    renames: &mut HashMap<String, String>,
    def_env: &Env,
) -> Expr {
    let span = template.span;
    match &template.kind {
        ExprKind::Symbol(name) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    PatBinding::One(expr) => expr.clone(),
                    PatBinding::Many(exprs) => {
                        // Shouldn't happen outside list context; return first as fallback
                        if let Some(e) = exprs.first() { e.clone() } else {
                            Expr { kind: ExprKind::List(vec![]), span }
                        }
                    }
                }
            } else if is_special_form(name) {
                template.clone()
            } else if let Some(renamed) = renames.get(name) {
                Expr { kind: ExprKind::Symbol(renamed.clone()), span }
            } else if let Some(val) = env_get(def_env, name) {
                if matches!(val, Value::Macro { .. }) {
                    template.clone() // leave macro names as symbols for re-expansion
                } else {
                    // Definition-site binding: inject as literal for hygiene
                    Expr { kind: ExprKind::Literal(val), span }
                }
            } else {
                // Macro-introduced identifier: rename for hygiene
                let gs = gensym(name);
                renames.insert(name.clone(), gs.clone());
                Expr { kind: ExprKind::Symbol(gs), span }
            }
        }
        ExprKind::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() && matches!(&elems[i + 1].kind, ExprKind::Symbol(s) if s == "...") {
                    // Ellipsis: expand the preceding element for each match
                    let sub = &elems[i];
                    let evars = collect_ellipsis_vars(sub, bindings);
                    if let Some(first) = evars.first() {
                        if let Some(PatBinding::Many(exprs)) = bindings.get(first) {
                            let count = exprs.len();
                            for j in 0..count {
                                let mut sub_bindings = bindings.clone();
                                for var in &evars {
                                    if let Some(PatBinding::Many(var_exprs)) = bindings.get(var) {
                                        if j < var_exprs.len() {
                                            sub_bindings.insert(var.clone(), PatBinding::One(var_exprs[j].clone()));
                                        }
                                    }
                                }
                                result.push(expand_template(sub, &sub_bindings, renames, def_env));
                            }
                        }
                    }
                    i += 2;
                } else {
                    result.push(expand_template(&elems[i], bindings, renames, def_env));
                    i += 1;
                }
            }
            Expr { kind: ExprKind::List(result), span }
        }
        _ => template.clone(),
    }
}

fn collect_ellipsis_vars(template: &Expr, bindings: &HashMap<String, PatBinding>) -> Vec<String> {
    let mut vars = Vec::new();
    collect_ellipsis_vars_inner(template, bindings, &mut vars);
    vars
}

fn collect_ellipsis_vars_inner(template: &Expr, bindings: &HashMap<String, PatBinding>, vars: &mut Vec<String>) {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if matches!(bindings.get(name), Some(PatBinding::Many(_))) {
                vars.push(name.clone());
            }
        }
        ExprKind::List(elems) => {
            for e in elems {
                collect_ellipsis_vars_inner(e, bindings, vars);
            }
        }
        _ => {}
    }
}

// ---------- L22: syntax-case ----------

fn value_to_expr(val: &Value) -> Expr {
    let span = Span { line: 0, col: 0 };
    match val {
        Value::Integer(n) => Expr { kind: ExprKind::Integer(*n), span },
        Value::Boolean(b) => Expr { kind: ExprKind::Boolean(*b), span },
        Value::Str(s) => Expr { kind: ExprKind::Str(s.clone()), span },
        Value::Char(c) => Expr { kind: ExprKind::Char(*c), span },
        Value::Symbol(s) => Expr { kind: ExprKind::Symbol(s.clone()), span },
        Value::List(items) => Expr { kind: ExprKind::List(items.iter().map(value_to_expr).collect()), span },
        _ => Expr { kind: ExprKind::Literal(val.clone()), span },
    }
}

fn get_merged_syntax_bindings() -> HashMap<String, PatBinding> {
    SYNTAX_BINDINGS.with(|sb| {
        let stack = sb.borrow();
        let mut merged = HashMap::new();
        for map in stack.iter() {
            merged.extend(map.iter().map(|(k, v)| (k.clone(), v.clone())));
        }
        merged
    })
}

fn eval_syntax_case(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    // (syntax-case stx-expr (literal ...) clause ...)
    if args.len() < 2 {
        return Err(EvalError::Arity("syntax-case requires at least 2 arguments".into()).with_position(span.line, span.col));
    }
    // Evaluate the syntax expression
    let stx_val = eval(&args[0], env)?;
    let form_expr = match &stx_val {
        Value::Syntax(expr) => (**expr).clone(),
        _ => return Err(EvalError::Type("syntax-case: expected syntax object".into()).with_position(span.line, span.col)),
    };
    // Parse literals
    let literals = match &args[1].kind {
        ExprKind::List(lits) => lits.iter().filter_map(|e| {
            if let ExprKind::Symbol(s) = &e.kind { Some(s.clone()) } else { None }
        }).collect::<Vec<_>>(),
        _ => return Err(EvalError::Parse("syntax-case: expected literals list".into()).with_position(span.line, span.col)),
    };
    // Try each clause
    for clause in &args[2..] {
        let parts = match &clause.kind {
            ExprKind::List(p) => p,
            _ => return Err(EvalError::Parse("syntax-case: expected clause".into()).with_position(span.line, span.col)),
        };
        if parts.len() < 2 || parts.len() > 3 {
            return Err(EvalError::Parse("syntax-case: clause must have 2 or 3 parts".into()).with_position(span.line, span.col));
        }
        let pattern = &parts[0];
        let (fender, body) = if parts.len() == 3 {
            (Some(&parts[1]), &parts[2])
        } else {
            (None, &parts[1])
        };
        let mut bindings = HashMap::new();
        let matched = match &pattern.kind {
            ExprKind::List(pelems) => {
                if let ExprKind::List(felems) = &form_expr.kind {
                    match_list_elems(pelems, felems, &literals, &mut bindings)
                } else {
                    false
                }
            }
            _ => match_pattern(pattern, &form_expr, &literals, &mut bindings),
        };
        if matched {
            // Push syntax bindings
            let mut syntax_bindings = HashMap::new();
            for (name, pat) in &bindings {
                match pat {
                    PatBinding::One(expr) => {
                        syntax_bindings.insert(name.clone(), PatBinding::One(expr.clone()));
                    }
                    PatBinding::Many(exprs) => {
                        syntax_bindings.insert(name.clone(), PatBinding::Many(exprs.clone()));
                    }
                }
            }
            SYNTAX_BINDINGS.with(|sb| sb.borrow_mut().push(syntax_bindings));
            // Also bind pattern vars as Value::Syntax in the env for use in fenders and body
            let local = new_env(Some(env.clone()));
            for (name, pat) in &bindings {
                match pat {
                    PatBinding::One(expr) => {
                        env_set(&local, name.clone(), Value::Syntax(Box::new(expr.clone())));
                    }
                    PatBinding::Many(exprs) => {
                        env_set(&local, name.clone(), Value::SyntaxList(exprs.clone()));
                    }
                }
            }
            // Check fender if present
            if let Some(fender_expr) = fender {
                let fender_val = eval(fender_expr, &local)?;
                if !fender_val.is_truthy() {
                    SYNTAX_BINDINGS.with(|sb| { sb.borrow_mut().pop(); });
                    continue;
                }
            }
            let result = eval(body, &local);
            SYNTAX_BINDINGS.with(|sb| { sb.borrow_mut().pop(); });
            return result;
        }
    }
    Err(EvalError::Runtime("syntax-case: no matching clause".into()).with_position(span.line, span.col))
}

fn eval_syntax_form(args: &[Expr], _env: &Env, span: Span) -> Result<Value, EvalError> {
    // (syntax template) — expands template with current syntax bindings
    if args.len() != 1 {
        return Err(EvalError::Arity("syntax requires 1 argument".into()).with_position(span.line, span.col));
    }
    let bindings = get_merged_syntax_bindings();
    let def_env = SYNTAX_DEF_ENV.with(|de| de.borrow().clone());
    let mut renames = HashMap::new();
    let expanded = expand_syntax_template(&args[0], &bindings, &mut renames, &def_env);
    Ok(Value::Syntax(Box::new(expanded)))
}

fn expand_syntax_template(
    template: &Expr,
    bindings: &HashMap<String, PatBinding>,
    renames: &mut HashMap<String, String>,
    def_env: &Option<Env>,
) -> Expr {
    let span = template.span;
    match &template.kind {
        ExprKind::Symbol(name) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    PatBinding::One(expr) => expr.clone(),
                    PatBinding::Many(exprs) => {
                        if let Some(e) = exprs.first() { e.clone() } else {
                            Expr { kind: ExprKind::List(vec![]), span }
                        }
                    }
                }
            } else if is_special_form(name) || name == "..." {
                template.clone()
            } else if let Some(renamed) = renames.get(name) {
                Expr { kind: ExprKind::Symbol(renamed.clone()), span }
            } else if let Some(ref de) = def_env {
                if let Some(val) = env_get(de, name) {
                    match &val {
                        Value::Macro { .. } | Value::MacroTransformer { .. } => {
                            template.clone()
                        }
                        Value::Lambda { .. } | Value::Builtin(_) | Value::DynBuiltin(_)
                        | Value::CallCC | Value::Continuation(_) => {
                            // Functions: inject as literal for hygiene
                            Expr { kind: ExprKind::Literal(val), span }
                        }
                        _ => {
                            // Non-function values: gensym for hygiene
                            let gs = gensym(name);
                            renames.insert(name.clone(), gs.clone());
                            Expr { kind: ExprKind::Symbol(gs), span }
                        }
                    }
                } else {
                    let gs = gensym(name);
                    renames.insert(name.clone(), gs.clone());
                    Expr { kind: ExprKind::Symbol(gs), span }
                }
            } else {
                let gs = gensym(name);
                renames.insert(name.clone(), gs.clone());
                Expr { kind: ExprKind::Symbol(gs), span }
            }
        }
        ExprKind::List(elems) => {
            // Don't expand inside (quote ...)
            if !elems.is_empty() && matches!(&elems[0].kind, ExprKind::Symbol(s) if s == "quote") {
                return template.clone();
            }
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() && matches!(&elems[i + 1].kind, ExprKind::Symbol(s) if s == "...") {
                    let sub = &elems[i];
                    let evars = collect_syntax_ellipsis_vars(sub, bindings);
                    if let Some(first) = evars.first() {
                        if let Some(PatBinding::Many(exprs)) = bindings.get(first) {
                            let count = exprs.len();
                            for j in 0..count {
                                let mut sub_bindings = bindings.clone();
                                for var in &evars {
                                    if let Some(PatBinding::Many(var_exprs)) = bindings.get(var) {
                                        if j < var_exprs.len() {
                                            sub_bindings.insert(var.clone(), PatBinding::One(var_exprs[j].clone()));
                                        }
                                    }
                                }
                                result.push(expand_syntax_template(sub, &sub_bindings, renames, def_env));
                            }
                        }
                    }
                    i += 2;
                } else {
                    result.push(expand_syntax_template(&elems[i], bindings, renames, def_env));
                    i += 1;
                }
            }
            Expr { kind: ExprKind::List(result), span }
        }
        _ => template.clone(),
    }
}

fn collect_syntax_ellipsis_vars(template: &Expr, bindings: &HashMap<String, PatBinding>) -> Vec<String> {
    let mut vars = Vec::new();
    collect_syntax_ellipsis_vars_inner(template, bindings, &mut vars);
    vars
}

fn collect_syntax_ellipsis_vars_inner(template: &Expr, bindings: &HashMap<String, PatBinding>, vars: &mut Vec<String>) {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if matches!(bindings.get(name), Some(PatBinding::Many(_))) {
                vars.push(name.clone());
            }
        }
        ExprKind::List(elems) => {
            for e in elems {
                collect_syntax_ellipsis_vars_inner(e, bindings, vars);
            }
        }
        _ => {}
    }
}

fn eval_with_syntax(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    // (with-syntax ((pattern expr) ...) body ...)
    if args.len() < 2 {
        return Err(EvalError::Arity("with-syntax requires at least 2 arguments".into()).with_position(span.line, span.col));
    }
    let bindings_list = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse("with-syntax: expected bindings list".into()).with_position(span.line, span.col)),
    };
    let mut new_bindings = HashMap::new();
    let local = new_env(Some(env.clone()));
    for binding in bindings_list {
        let parts = match &binding.kind {
            ExprKind::List(p) if p.len() == 2 => p,
            _ => return Err(EvalError::Parse("with-syntax: expected (pattern expr)".into()).with_position(span.line, span.col)),
        };
        let pattern = &parts[0];
        let val = eval(&parts[1], env)?;
        let bound_expr = match &val {
            Value::Syntax(expr) => (**expr).clone(),
            _ => value_to_expr(&val),
        };
        // Simple pattern: just a symbol
        if let ExprKind::Symbol(name) = &pattern.kind {
            new_bindings.insert(name.clone(), PatBinding::One(bound_expr.clone()));
            env_set(&local, name.clone(), Value::Syntax(Box::new(bound_expr)));
        } else {
            // Complex pattern: use match_pattern
            let mut pat_bindings = HashMap::new();
            if match_pattern(pattern, &bound_expr, &[], &mut pat_bindings) {
                for (name, pat) in &pat_bindings {
                    new_bindings.insert(name.clone(), pat.clone());
                    match pat {
                        PatBinding::One(expr) => {
                            env_set(&local, name.clone(), Value::Syntax(Box::new(expr.clone())));
                        }
                        PatBinding::Many(exprs) => {
                            env_set(&local, name.clone(), Value::SyntaxList(exprs.clone()));
                        }
                    }
                }
            }
        }
    }
    SYNTAX_BINDINGS.with(|sb| sb.borrow_mut().push(new_bindings));
    let mut result = Value::Void;
    for body_expr in &args[1..] {
        result = eval(body_expr, &local)?;
    }
    SYNTAX_BINDINGS.with(|sb| { sb.borrow_mut().pop(); });
    Ok(result)
}

fn builtin_syntax_to_datum(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("syntax->datum requires 1 argument".into()));
    }
    match &args[0] {
        Value::Syntax(expr) => Ok(expr_to_value(expr)),
        _ => Err(EvalError::Type("syntax->datum: expected syntax object".into())),
    }
}

fn builtin_datum_to_syntax(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("datum->syntax requires 2 arguments".into()));
    }
    // First arg is template id (syntax object for context), second is datum
    let expr = value_to_expr(&args[1]);
    Ok(Value::Syntax(Box::new(expr)))
}

// ---------- Numeric helpers ----------

fn gcd(mut a: i64, mut b: i64) -> i64 {
    a = a.abs(); b = b.abs();
    while b != 0 { let t = b; b = a % b; a = t; }
    a
}

fn make_rational(n: i64, d: i64) -> Value {
    assert!(d != 0);
    let (n, d) = if d < 0 { (-n, -d) } else { (n, d) };
    let g = gcd(n, d);
    let (n, d) = (n / g, d / g);
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

fn to_f64(v: &Value) -> Result<f64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
        _ => Err(EvalError::Type("expected number".into())),
    }
}

fn to_exact(v: &Value) -> Result<(i64, i64), EvalError> {
    match v {
        Value::Integer(n) => Ok((*n, 1)),
        Value::Rational(n, d) => Ok((*n, *d)),
        _ => Err(EvalError::Type("expected exact number".into())),
    }
}

fn has_inexact(args: &[Value]) -> bool {
    args.iter().any(|a| matches!(a, Value::Float(_)))
}

// ---------- Builtins ----------

fn as_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type("expected integer".into())),
    }
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    if has_inexact(args) {
        let mut sum = 0.0_f64;
        for a in args { sum += to_f64(a)?; }
        Ok(Value::Float(sum))
    } else {
        let mut n: i64 = 0;
        let mut d: i64 = 1;
        for a in args {
            let (an, ad) = to_exact(a)?;
            n = n * ad + an * d;
            d *= ad;
            let g = gcd(n, d);
            n /= g; d /= g;
        }
        Ok(make_rational(n, d))
    }
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    if has_inexact(args) {
        if args.len() == 1 { return Ok(Value::Float(-to_f64(&args[0])?)); }
        let mut r = to_f64(&args[0])?;
        for a in &args[1..] { r -= to_f64(a)?; }
        Ok(Value::Float(r))
    } else {
        if args.len() == 1 {
            let (n, d) = to_exact(&args[0])?;
            return Ok(make_rational(-n, d));
        }
        let (mut n, mut d) = to_exact(&args[0])?;
        for a in &args[1..] {
            let (an, ad) = to_exact(a)?;
            n = n * ad - an * d;
            d *= ad;
            let g = gcd(n, d);
            n /= g; d /= g;
        }
        Ok(make_rational(n, d))
    }
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    if has_inexact(args) {
        let mut prod = 1.0_f64;
        for a in args { prod *= to_f64(a)?; }
        Ok(Value::Float(prod))
    } else {
        let mut n: i64 = 1;
        let mut d: i64 = 1;
        for a in args {
            let (an, ad) = to_exact(a)?;
            n *= an; d *= ad;
            let g = gcd(n, d);
            n /= g; d /= g;
        }
        Ok(make_rational(n, d))
    }
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
    }
    if has_inexact(args) {
        let mut r = to_f64(&args[0])?;
        for a in &args[1..] {
            let dv = to_f64(a)?;
            if dv == 0.0 { return Err(EvalError::Runtime("division by zero".into())); }
            r /= dv;
        }
        Ok(Value::Float(r))
    } else {
        let (mut n, mut d) = to_exact(&args[0])?;
        for a in &args[1..] {
            let (an, ad) = to_exact(a)?;
            if an == 0 { return Err(EvalError::Runtime("division by zero".into())); }
            n *= ad; d *= an;
            let g = gcd(n, d);
            n /= g; d /= g;
        }
        Ok(make_rational(n, d))
    }
}

fn builtin_lt(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a < b) }
fn builtin_gt(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a > b) }
fn builtin_eq(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a == b) }
fn builtin_le(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a <= b) }
fn builtin_ge(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a >= b) }

fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new((car, cdr))))
}

fn value_to_vec(v: &Value) -> Option<Vec<Value>> {
    match v {
        Value::List(items) => Some(items.clone()),
        Value::Pair(_) => {
            let mut result = Vec::new();
            let mut current = v.clone();
            let mut seen = std::collections::HashSet::new();
            loop {
                let next = match &current {
                    Value::Pair(p) => {
                        let ptr = Rc::as_ptr(p) as usize;
                        if !seen.insert(ptr) { return None; }
                        let inner = p.borrow();
                        result.push(inner.0.clone());
                        Some(inner.1.clone())
                    }
                    Value::List(items) => {
                        result.extend(items.iter().cloned());
                        None
                    }
                    _ => return None,
                };
                match next {
                    Some(n) => current = n,
                    None => return Some(result),
                }
            }
        }
        _ => None,
    }
}

fn list_from_vec(items: Vec<Value>) -> Value {
    let mut result: Value = Value::List(vec![]);
    for item in items.into_iter().rev() {
        result = make_pair(item, result);
    }
    result
}

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("cons requires 2 arguments".into()));
    }
    Ok(make_pair(args[0].clone(), args[1].clone()))
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car requires 1 argument".into()));
    }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::Pair(p) => Ok(p.borrow().0.clone()),
        _ => Err(EvalError::Type("car: expected non-empty list".into())),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr requires 1 argument".into()));
    }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        Value::Pair(p) => Ok(p.borrow().1.clone()),
        _ => Err(EvalError::Type("cdr: expected non-empty list".into())),
    }
}

fn builtin_null(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("null? requires 1 argument".into()));
    }
    let is_null = matches!(&args[0], Value::List(items) if items.is_empty());
    Ok(Value::Boolean(is_null))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(list_from_vec(args.to_vec()))
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("length requires 1 argument".into()));
    }
    match value_to_vec(&args[0]) {
        Some(items) => Ok(Value::Integer(items.len() as i64)),
        None => Err(EvalError::Type("length: expected list".into())),
    }
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() { return Ok(Value::List(vec![])); }
    // Last arg can be any value (improper list tail)
    let mut result = Vec::new();
    for (i, a) in args.iter().enumerate() {
        if i < args.len() - 1 {
            match value_to_vec(a) {
                Some(items) => result.extend(items),
                None => return Err(EvalError::Type("append: expected list".into())),
            }
        } else {
            // Last argument: if it's a list, extend; otherwise create improper list
            match value_to_vec(a) {
                Some(items) => {
                    result.extend(items);
                    return Ok(list_from_vec(result));
                }
                None => {
                    // Improper list: build chain ending with this value
                    let mut tail = a.clone();
                    for item in result.into_iter().rev() {
                        tail = make_pair(item, tail);
                    }
                    return Ok(tail);
                }
            }
        }
    }
    Ok(list_from_vec(result))
}

fn builtin_is_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
}

fn builtin_is_number(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("number? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Float(_) | Value::Rational(_, _))))
}

fn builtin_is_boolean(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
}

fn builtin_is_pair(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("pair? requires 1 argument".into())); }
    let is_pair = matches!(&args[0], Value::List(items) if !items.is_empty()) || matches!(&args[0], Value::Pair(_));
    Ok(Value::Boolean(is_pair))
}

fn builtin_is_symbol(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("symbol? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("not requires 1 argument".into()));
    }
    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn builtin_is_char(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
}

fn builtin_string_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = String::new();
    for a in args {
        match a {
            Value::Str(s) => result.push_str(s),
            _ => return Err(EvalError::Type("string-append: expected string".into())),
        }
    }
    Ok(Value::Str(result))
}

fn builtin_string_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-length requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
        _ => Err(EvalError::Type("string-length: expected string".into())),
    }
}

fn builtin_substring(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 { return Err(EvalError::Arity("substring requires 3 arguments".into())); }
    let s = match &args[0] { Value::Str(s) => s, _ => return Err(EvalError::Type("substring: expected string".into())) };
    let start = as_int(&args[1])? as usize;
    let end = as_int(&args[2])? as usize;
    if start > end || end > s.len() {
        return Err(EvalError::Runtime("substring: index out of range".into()));
    }
    Ok(Value::Str(s[start..end].to_string()))
}

fn builtin_string_to_number(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string->number requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => match s.parse::<i64>() {
            Ok(n) => Ok(Value::Integer(n)),
            Err(_) => Ok(Value::Boolean(false)),
        },
        _ => Err(EvalError::Type("string->number: expected string".into())),
    }
}

fn builtin_number_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("number->string requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Str(n.to_string())),
        Value::Float(_) => Ok(Value::Str(args[0].display())),
        Value::Rational(n, d) => Ok(Value::Str(format!("{}/{}", n, d))),
        _ => Err(EvalError::Type("number->string: expected number".into())),
    }
}

fn builtin_symbol_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("symbol->string requires 1 argument".into())); }
    match &args[0] {
        Value::Symbol(s) => Ok(Value::Str(s.clone())),
        _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
    }
}

fn builtin_string_to_symbol(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string->symbol requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Symbol(s.clone())),
        _ => Err(EvalError::Type("string->symbol: expected string".into())),
    }
}

fn builtin_string_copy(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-copy requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.clone())),
        _ => Err(EvalError::Type("string-copy: expected string".into())),
    }
}

fn builtin_string_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string-ref requires 2 arguments".into())); }
    let s = match &args[0] { Value::Str(s) => s, _ => return Err(EvalError::Type("string-ref: expected string".into())) };
    let idx = as_int(&args[1])? as usize;
    if idx >= s.len() {
        return Err(EvalError::Runtime("string-ref: index out of range".into()));
    }
    Ok(Value::Char(s.chars().nth(idx).unwrap()))
}

fn cmp_op(args: &[Value], f: impl Fn(f64, f64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = to_f64(&args[0])?;
    for a in &args[1..] {
        let curr = to_f64(a)?;
        if !f(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

fn builtin_apply(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("apply requires at least 2 arguments".into()));
    }
    let func = &args[0];
    let last = &args[args.len() - 1];
    let tail_list = value_to_vec(last)
        .ok_or_else(|| EvalError::Type("apply: last argument must be a list".into()))?;
    let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    all_args.extend(tail_list);
    apply_value(func, &all_args)
}

// ---------- L13 builtins ----------

fn builtin_abs(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("abs requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(n.abs())),
        _ => Err(EvalError::Type("abs: expected number".into())),
    }
}

fn builtin_modulo(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("modulo requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => {
            if *b == 0 { return Err(EvalError::Runtime("modulo: division by zero".into())); }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        _ => Err(EvalError::Type("modulo: expected numbers".into())),
    }
}

fn builtin_remainder(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("remainder requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => {
            if *b == 0 { return Err(EvalError::Runtime("remainder: division by zero".into())); }
            Ok(Value::Integer(a % b))
        }
        _ => Err(EvalError::Type("remainder: expected numbers".into())),
    }
}

fn builtin_quotient(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("quotient requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => {
            if *b == 0 { return Err(EvalError::Runtime("quotient: division by zero".into())); }
            Ok(Value::Integer(a / b))
        }
        _ => Err(EvalError::Type("quotient: expected numbers".into())),
    }
}

fn builtin_min(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("min requires at least 1 argument".into())); }
    let mut result = match &args[0] {
        Value::Integer(n) => *n,
        _ => return Err(EvalError::Type("min: expected number".into())),
    };
    for a in &args[1..] {
        match a {
            Value::Integer(n) => { if *n < result { result = *n; } }
            _ => return Err(EvalError::Type("min: expected number".into())),
        }
    }
    Ok(Value::Integer(result))
}

fn builtin_max(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("max requires at least 1 argument".into())); }
    let mut result = match &args[0] {
        Value::Integer(n) => *n,
        _ => return Err(EvalError::Type("max: expected number".into())),
    };
    for a in &args[1..] {
        match a {
            Value::Integer(n) => { if *n > result { result = *n; } }
            _ => return Err(EvalError::Type("max: expected number".into())),
        }
    }
    Ok(Value::Integer(result))
}

fn builtin_expt(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("expt requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Integer(base), Value::Integer(exp)) => {
            if *exp < 0 { return Err(EvalError::Runtime("expt: negative exponent".into())); }
            Ok(Value::Integer(base.pow(*exp as u32)))
        }
        _ => Err(EvalError::Type("expt: expected numbers".into())),
    }
}

fn builtin_is_zero(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("zero? requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(*n == 0)),
        _ => Err(EvalError::Type("zero?: expected number".into())),
    }
}

fn builtin_is_positive(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("positive? requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(*n > 0)),
        _ => Err(EvalError::Type("positive?: expected number".into())),
    }
}

fn builtin_is_negative(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("negative? requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(*n < 0)),
        _ => Err(EvalError::Type("negative?: expected number".into())),
    }
}

fn builtin_is_odd(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("odd? requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(n % 2 != 0)),
        _ => Err(EvalError::Type("odd?: expected number".into())),
    }
}

fn builtin_is_even(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("even? requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(n % 2 == 0)),
        _ => Err(EvalError::Type("even?: expected number".into())),
    }
}

fn builtin_list_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("list-ref requires 2 arguments".into())); }
    let idx = match &args[1] {
        Value::Integer(n) => *n as usize,
        _ => return Err(EvalError::Type("list-ref: expected integer index".into())),
    };
    match value_to_vec(&args[0]) {
        Some(items) => items.get(idx).cloned().ok_or_else(|| EvalError::Runtime("list-ref: index out of range".into())),
        None => Err(EvalError::Type("list-ref: expected list".into())),
    }
}

fn builtin_list_tail(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("list-tail requires 2 arguments".into())); }
    let idx = match &args[1] {
        Value::Integer(n) => *n as usize,
        _ => return Err(EvalError::Type("list-tail: expected integer index".into())),
    };
    // Walk the structure directly to preserve pair identity
    let mut current = args[0].clone();
    for _ in 0..idx {
        let next = match &current {
            Value::Pair(p) => p.borrow().1.clone(),
            Value::List(items) if !items.is_empty() => Value::List(items[1..].to_vec()),
            _ => return Err(EvalError::Runtime("list-tail: index out of range".into())),
        };
        current = next;
    }
    Ok(current)
}

fn builtin_is_list(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("list? requires 1 argument".into())); }
    // Tortoise-and-hare cycle detection
    fn is_proper_list(v: &Value) -> bool {
        match v {
            Value::List(_) => true, // includes empty list
            Value::Pair(_) => {
                let mut slow = v.clone();
                let mut fast = v.clone();
                loop {
                    // Advance fast by 2
                    for _ in 0..2 {
                        let next = match &fast {
                            Value::Pair(p) => p.borrow().1.clone(),
                            Value::List(items) if items.is_empty() => return true,
                            _ => return false,
                        };
                        fast = next;
                    }
                    // Advance slow by 1
                    let next = match &slow {
                        Value::Pair(p) => p.borrow().1.clone(),
                        _ => unreachable!(),
                    };
                    slow = next;
                    // Check if they point to the same pair
                    if let (Value::Pair(s), Value::Pair(f)) = (&slow, &fast) {
                        if Rc::ptr_eq(s, f) { return false; } // cycle
                    }
                }
            }
            _ => false,
        }
    }
    Ok(Value::Boolean(is_proper_list(&args[0])))
}

fn values_equal(a: &Value, b: &Value) -> bool {
    values_equal_inner(a, b, &mut std::collections::HashSet::new())
}

fn values_equal_inner(a: &Value, b: &Value, seen: &mut std::collections::HashSet<(usize, usize)>) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(x), Value::List(y)) => {
            x.len() == y.len() && x.iter().zip(y.iter()).all(|(a, b)| values_equal_inner(a, b, seen))
        }
        (Value::Pair(p1), Value::Pair(p2)) => {
            let k = (Rc::as_ptr(p1) as usize, Rc::as_ptr(p2) as usize);
            if Rc::ptr_eq(p1, p2) { return true; }
            if !seen.insert(k) { return true; } // already comparing these
            let (a1, b1) = { let inner = p1.borrow(); (inner.0.clone(), inner.1.clone()) };
            let (a2, b2) = { let inner = p2.borrow(); (inner.0.clone(), inner.1.clone()) };
            values_equal_inner(&a1, &a2, seen) && values_equal_inner(&b1, &b2, seen)
        }
        // Compare List with Pair chain
        (Value::List(items), Value::Pair(_)) | (Value::Pair(_), Value::List(items)) => {
            let (list_val, other) = if matches!(a, Value::List(_)) { (a, b) } else { (b, a) };
            match (value_to_vec(list_val), value_to_vec(other)) {
                (Some(v1), Some(v2)) => v1.len() == v2.len() && v1.iter().zip(v2.iter()).all(|(x, y)| values_equal_inner(x, y, seen)),
                _ => false,
            }
        }
        (Value::Vector(x), Value::Vector(y)) => {
            let xb = x.borrow();
            let yb = y.borrow();
            xb.len() == yb.len() && xb.iter().zip(yb.iter()).all(|(a, b)| values_equal_inner(a, b, seen))
        }
        _ => false,
    }
}

fn builtin_equal(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("equal? requires 2 arguments".into())); }
    Ok(Value::Boolean(values_equal(&args[0], &args[1])))
}

fn builtin_eq_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("eq? requires 2 arguments".into())); }
    let result = match (&args[0], &args[1]) {
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
        (Value::Void, Value::Void) => true,
        _ => false,
    };
    Ok(Value::Boolean(result))
}

fn builtin_assoc(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("assoc requires 2 arguments".into())); }
    let key = &args[0];
    match value_to_vec(&args[1]) {
        Some(alist) => {
            for item in &alist {
                let car = match item {
                    Value::Pair(p) => Some(p.borrow().0.clone()),
                    Value::List(items) if !items.is_empty() => Some(items[0].clone()),
                    _ => None,
                };
                if let Some(k) = car {
                    if values_equal(&k, key) {
                        return Ok(item.clone());
                    }
                }
            }
            Ok(Value::Boolean(false))
        }
        None => Err(EvalError::Type("assoc: expected list".into())),
    }
}

fn builtin_map(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() < 2 { return Err(EvalError::Arity("map requires at least 2 arguments".into())); }
    let func = &args[0];
    let lists: Vec<Vec<Value>> = args[1..].iter().map(|a| {
        value_to_vec(a).ok_or_else(|| EvalError::Type("map: expected list".into()))
    }).collect::<Result<Vec<_>, _>>()?;
    if lists.is_empty() { return Ok(Value::List(vec![])); }
    let len = lists[0].len();
    for l in &lists {
        if l.len() != len {
            return Err(EvalError::Runtime("map: lists must have same length".into()));
        }
    }
    let mut result = Vec::with_capacity(len);
    for i in 0..len {
        let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
        result.push(apply_value(func, &call_args)?);
    }
    Ok(list_from_vec(result))
}

fn builtin_char_alphabetic(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-alphabetic? requires 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Boolean(c.is_ascii_alphabetic())),
        _ => Err(EvalError::Type("char-alphabetic?: expected char".into())),
    }
}

fn builtin_char_numeric(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-numeric? requires 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
        _ => Err(EvalError::Type("char-numeric?: expected char".into())),
    }
}

fn builtin_char_upcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-upcase requires 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
        _ => Err(EvalError::Type("char-upcase: expected char".into())),
    }
}

fn builtin_char_downcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-downcase requires 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
        _ => Err(EvalError::Type("char-downcase: expected char".into())),
    }
}

fn builtin_char_eq(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("char=? requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
        _ => Err(EvalError::Type("char=?: expected chars".into())),
    }
}

fn builtin_char_lt(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("char<? requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
        _ => Err(EvalError::Type("char<?: expected chars".into())),
    }
}

fn builtin_string_eq(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string=? requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
        _ => Err(EvalError::Type("string=?: expected strings".into())),
    }
}

fn builtin_string_lt(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string<? requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
        _ => Err(EvalError::Type("string<?: expected strings".into())),
    }
}

fn builtin_string_ci_eq(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string-ci=? requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
        _ => Err(EvalError::Type("string-ci=?: expected strings".into())),
    }
}

fn builtin_string_upcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-upcase requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
        _ => Err(EvalError::Type("string-upcase: expected string".into())),
    }
}

fn builtin_string_downcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-downcase requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
        _ => Err(EvalError::Type("string-downcase: expected string".into())),
    }
}

fn builtin_is_procedure(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("procedure? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Lambda { .. } | Value::Builtin(_) | Value::DynBuiltin(_) | Value::CallCC | Value::Continuation(_))))
}

// L14: char/integer conversion
fn builtin_char_to_integer(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char->integer requires 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Integer(*c as i64)),
        _ => Err(EvalError::Type("char->integer: expected char".into())),
    }
}

fn builtin_integer_to_char(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("integer->char requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => {
            let c = char::from_u32(*n as u32)
                .ok_or_else(|| EvalError::Runtime(format!("integer->char: invalid code point {}", n)))?;
            Ok(Value::Char(c))
        }
        _ => Err(EvalError::Type("integer->char: expected integer".into())),
    }
}

// L14: string/list conversion
fn builtin_string_to_list(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string->list requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => {
            let chars: Vec<Value> = s.chars().map(Value::Char).collect();
            Ok(list_from_vec(chars))
        }
        _ => Err(EvalError::Type("string->list: expected string".into())),
    }
}

fn builtin_list_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("list->string requires 1 argument".into())); }
    let items = value_to_vec(&args[0])
        .ok_or_else(|| EvalError::Type("list->string: expected proper list".into()))?;
    let mut s = String::new();
    for item in &items {
        match item {
            Value::Char(c) => s.push(*c),
            _ => return Err(EvalError::Type("list->string: list must contain only chars".into())),
        }
    }
    Ok(Value::Str(s))
}

// L15: Vector builtins

fn builtin_vector(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
}

fn builtin_make_vector(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() < 1 || args.len() > 2 {
        return Err(EvalError::Arity("make-vector requires 1 or 2 arguments".into()));
    }
    let len = as_int(&args[0])? as usize;
    let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
    Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
}

fn builtin_vector_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("vector-ref requires 2 arguments".into())); }
    match &args[0] {
        Value::Vector(v) => {
            let idx = as_int(&args[1])? as usize;
            let inner = v.borrow();
            inner.get(idx).cloned().ok_or_else(|| EvalError::Runtime("vector-ref: index out of range".into()))
        }
        _ => Err(EvalError::Type("vector-ref: expected vector".into())),
    }
}

fn builtin_vector_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector-length requires 1 argument".into())); }
    match &args[0] {
        Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
        _ => Err(EvalError::Type("vector-length: expected vector".into())),
    }
}

fn builtin_is_vector(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Vector(_))))
}

fn builtin_vector_to_list(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector->list requires 1 argument".into())); }
    match &args[0] {
        Value::Vector(v) => Ok(list_from_vec(v.borrow().clone())),
        _ => Err(EvalError::Type("vector->list: expected vector".into())),
    }
}

fn builtin_list_to_vector(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("list->vector requires 1 argument".into())); }
    match value_to_vec(&args[0]) {
        Some(l) => Ok(Value::Vector(Rc::new(RefCell::new(l)))),
        None => Err(EvalError::Type("list->vector: expected list".into())),
    }
}

fn builtin_reverse(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("reverse requires 1 argument".into())); }
    match value_to_vec(&args[0]) {
        Some(mut l) => {
            l.reverse();
            Ok(list_from_vec(l))
        }
        None => Err(EvalError::Type("reverse: expected list".into())),
    }
}

// ---------- L19: Exact/Rational builtins ----------

fn builtin_is_exact(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("exact? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Rational(_, _))))
}

fn builtin_is_inexact(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("inexact? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Float(_))))
}

fn builtin_exact_to_inexact(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("exact->inexact requires 1 argument".into())); }
    Ok(Value::Float(to_f64(&args[0])?))
}

fn builtin_inexact_to_exact(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("inexact->exact requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Rational(n, d) => Ok(Value::Rational(*n, *d)),
        Value::Float(f) => {
            if f.fract() == 0.0 && f.is_finite() {
                return Ok(Value::Integer(*f as i64));
            }
            // Continued fraction algorithm to find exact rational
            let mut p0: i64 = 0; let mut q0: i64 = 1;
            let mut p1: i64 = 1; let mut q1: i64 = 0;
            let mut x = *f;
            for _ in 0..64 {
                let a = x.floor() as i64;
                let p2 = a * p1 + p0;
                let q2 = a * q1 + q0;
                if q2 != 0 && (p2 as f64 / q2 as f64 - *f).abs() < 1e-10 {
                    return Ok(make_rational(p2, q2));
                }
                p0 = p1; q0 = q1;
                p1 = p2; q1 = q2;
                let frac = x - a as f64;
                if frac.abs() < 1e-15 { break; }
                x = 1.0 / frac;
            }
            Ok(make_rational(p1, q1))
        }
        _ => Err(EvalError::Type("inexact->exact: expected number".into())),
    }
}

fn builtin_numerator(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("numerator requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Rational(n, _) => Ok(Value::Integer(*n)),
        _ => Err(EvalError::Type("numerator: expected rational".into())),
    }
}

fn builtin_denominator(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("denominator requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(_) => Ok(Value::Integer(1)),
        Value::Rational(_, d) => Ok(Value::Integer(*d)),
        _ => Err(EvalError::Type("denominator: expected rational".into())),
    }
}

fn builtin_is_integer(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("integer? requires 1 argument".into())); }
    Ok(Value::Boolean(match &args[0] {
        Value::Integer(_) => true,
        Value::Float(f) => f.fract() == 0.0,
        _ => false,
    }))
}

fn builtin_is_rational(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("rational? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Rational(_, _))))
}

// ---------- L21: for-each, error ----------

fn builtin_for_each(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() < 2 { return Err(EvalError::Arity("for-each requires at least 2 arguments".into())); }
    let func = &args[0];
    let lists: Vec<Vec<Value>> = args[1..].iter().map(|a| {
        value_to_vec(a).ok_or_else(|| EvalError::Type("for-each: expected list".into()))
    }).collect::<Result<Vec<_>, _>>()?;
    if lists.is_empty() { return Ok(Value::Void); }
    let len = lists[0].len();
    for l in &lists {
        if l.len() != len {
            return Err(EvalError::Runtime("for-each: lists must have same length".into()));
        }
    }
    for i in 0..len {
        let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
        apply_value(func, &call_args)?;
    }
    Ok(Value::Void)
}

fn builtin_error(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SchemeException(Value::Str("error".into())));
    }
    let msg = args[0].display_output();
    if args.len() == 1 {
        return Err(EvalError::SchemeException(Value::Str(msg)));
    }
    let irritants: Vec<String> = args[1..].iter().map(|v| v.display()).collect();
    Err(EvalError::SchemeException(Value::Str(format!("{}: {}", msg, irritants.join(" ")))))
}

fn builtin_member(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("member requires 2 arguments".into())); }
    let key = &args[0];
    let mut current = args[1].clone();
    loop {
        match &current {
            Value::Pair(p) => {
                let inner = p.borrow();
                if values_equal(&inner.0, key) {
                    drop(inner);
                    return Ok(current.clone());
                }
                let next = inner.1.clone();
                drop(inner);
                current = next;
            }
            Value::List(items) if items.is_empty() => return Ok(Value::Boolean(false)),
            Value::List(items) => {
                for (i, item) in items.iter().enumerate() {
                    if values_equal(item, key) {
                        return Ok(Value::List(items[i..].to_vec()));
                    }
                }
                return Ok(Value::Boolean(false));
            }
            _ => return Ok(Value::Boolean(false)),
        }
    }
}

fn builtin_assv(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("assv requires 2 arguments".into())); }
    let key = &args[0];
    match value_to_vec(&args[1]) {
        Some(alist) => {
            for item in &alist {
                let car = match item {
                    Value::Pair(p) => Some(p.borrow().0.clone()),
                    Value::List(items) if !items.is_empty() => Some(items[0].clone()),
                    _ => None,
                };
                if let Some(k) = car {
                    if eqv_match(&k, key) {
                        return Ok(item.clone());
                    }
                }
            }
            Ok(Value::Boolean(false))
        }
        None => Err(EvalError::Type("assv: expected list".into())),
    }
}

fn builtin_gcd(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() { return Ok(Value::Integer(0)); }
    let mut result = as_int(&args[0])?.abs();
    for a in &args[1..] {
        result = gcd(result, as_int(a)?.abs());
    }
    Ok(Value::Integer(result))
}

fn builtin_lcm(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() { return Ok(Value::Integer(1)); }
    let mut result = as_int(&args[0])?.abs();
    for a in &args[1..] {
        let b = as_int(a)?.abs();
        if result == 0 && b == 0 { result = 0; }
        else { result = result / gcd(result, b) * b; }
    }
    Ok(Value::Integer(result))
}

fn builtin_truncate(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("truncate requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Float(f) => Ok(Value::Integer(f.trunc() as i64)),
        _ => Err(EvalError::Type("truncate: expected number".into())),
    }
}

fn builtin_round(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("round requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Float(f) => Ok(Value::Integer(f.round() as i64)),
        _ => Err(EvalError::Type("round: expected number".into())),
    }
}

fn builtin_string(args: &[Value]) -> Result<Value, EvalError> {
    let mut s = String::new();
    for a in args {
        match a {
            Value::Char(c) => s.push(*c),
            _ => return Err(EvalError::Type("string: expected chars".into())),
        }
    }
    Ok(Value::Str(s))
}

fn builtin_string_gt(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string>? requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a > b)),
        _ => Err(EvalError::Type("string>?: expected strings".into())),
    }
}

fn builtin_string_le(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string<=? requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a <= b)),
        _ => Err(EvalError::Type("string<=?: expected strings".into())),
    }
}

fn builtin_string_ge(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string>=? requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a >= b)),
        _ => Err(EvalError::Type("string>=?: expected strings".into())),
    }
}

fn builtin_vector_set_fn(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 { return Err(EvalError::Arity("vector-set! requires 3 arguments".into())); }
    let idx = as_int(&args[1])? as usize;
    match &args[0] {
        Value::Vector(v) => {
            let mut inner = v.borrow_mut();
            if idx >= inner.len() { return Err(EvalError::Runtime("vector-set!: index out of range".into())); }
            inner[idx] = args[2].clone();
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("vector-set!: expected vector".into())),
    }
}

fn builtin_make_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() || args.len() > 2 {
        return Err(EvalError::Arity("make-string requires 1 or 2 arguments".into()));
    }
    let len = as_int(&args[0])? as usize;
    let ch = if args.len() == 2 {
        match &args[1] {
            Value::Char(c) => *c,
            _ => return Err(EvalError::Type("make-string: expected char".into())),
        }
    } else {
        '\0'
    };
    Ok(Value::Str(std::iter::repeat(ch).take(len).collect()))
}

fn make_global_env() -> Env {
    let env = new_env(None);
    let builtins: &[(&str, BuiltinFn)] = &[
        ("+", builtin_add),
        ("-", builtin_sub),
        ("*", builtin_mul),
        ("/", builtin_div),
        ("<", builtin_lt),
        (">", builtin_gt),
        ("=", builtin_eq),
        ("<=", builtin_le),
        (">=", builtin_ge),
        ("not", builtin_not),
        ("cons", builtin_cons),
        ("car", builtin_car),
        ("cdr", builtin_cdr),
        ("null?", builtin_null),
        ("list", builtin_list),
        ("length", builtin_length),
        ("append", builtin_append),
        ("string?", builtin_is_string),
        ("number?", builtin_is_number),
        ("boolean?", builtin_is_boolean),
        ("pair?", builtin_is_pair),
        ("symbol?", builtin_is_symbol),
        ("char?", builtin_is_char),
        ("string-append", builtin_string_append),
        ("string-length", builtin_string_length),
        ("substring", builtin_substring),
        ("string->number", builtin_string_to_number),
        ("number->string", builtin_number_to_string),
        ("symbol->string", builtin_symbol_to_string),
        ("string->symbol", builtin_string_to_symbol),
        ("string-ref", builtin_string_ref),
        ("string-copy", builtin_string_copy),
        ("apply", builtin_apply),
        // L13: numeric
        ("abs", builtin_abs),
        ("modulo", builtin_modulo),
        ("remainder", builtin_remainder),
        ("quotient", builtin_quotient),
        ("min", builtin_min),
        ("max", builtin_max),
        ("expt", builtin_expt),
        // L13: numeric predicates
        ("zero?", builtin_is_zero),
        ("positive?", builtin_is_positive),
        ("negative?", builtin_is_negative),
        ("odd?", builtin_is_odd),
        ("even?", builtin_is_even),
        // L13: list
        ("list-ref", builtin_list_ref),
        ("list-tail", builtin_list_tail),
        ("list?", builtin_is_list),
        ("assoc", builtin_assoc),
        ("map", builtin_map),
        // L13: equality
        ("equal?", builtin_equal),
        ("eq?", builtin_eq_pred),
        ("eqv?", builtin_eq_pred),
        // L13: char
        ("char-alphabetic?", builtin_char_alphabetic),
        ("char-numeric?", builtin_char_numeric),
        ("char-upcase", builtin_char_upcase),
        ("char-downcase", builtin_char_downcase),
        ("char=?", builtin_char_eq),
        ("char<?", builtin_char_lt),
        // L13: string
        ("string=?", builtin_string_eq),
        ("string<?", builtin_string_lt),
        ("string-ci=?", builtin_string_ci_eq),
        ("string-upcase", builtin_string_upcase),
        ("string-downcase", builtin_string_downcase),
        // L13: misc
        ("procedure?", builtin_is_procedure),
        // L14: char/integer conversion
        ("char->integer", builtin_char_to_integer),
        ("integer->char", builtin_integer_to_char),
        // L14: string/list conversion
        ("string->list", builtin_string_to_list),
        ("list->string", builtin_list_to_string),
        // L15: vectors
        ("vector", builtin_vector),
        ("make-vector", builtin_make_vector),
        ("vector-ref", builtin_vector_ref),
        ("vector-length", builtin_vector_length),
        ("vector?", builtin_is_vector),
        ("vector->list", builtin_vector_to_list),
        ("list->vector", builtin_list_to_vector),
        // L16: dynamic-wind helpers
        ("reverse", builtin_reverse),
        // L19: exact/rational
        ("exact?", builtin_is_exact),
        ("inexact?", builtin_is_inexact),
        ("exact->inexact", builtin_exact_to_inexact),
        ("inexact->exact", builtin_inexact_to_exact),
        ("numerator", builtin_numerator),
        ("denominator", builtin_denominator),
        ("integer?", builtin_is_integer),
        ("rational?", builtin_is_rational),
        // L21: pair mutation and misc
        ("set-car!", builtin_set_car),
        ("set-cdr!", builtin_set_cdr),
        ("for-each", builtin_for_each),
        ("error", builtin_error),
        ("make-string", builtin_make_string),
        ("caar", builtin_caar),
        ("cadr", builtin_cadr),
        ("cdar", builtin_cdar),
        ("cddr", builtin_cddr),
        ("caddr", builtin_caddr),
        ("cdddr", builtin_cdddr),
        ("cadddr", builtin_cadddr),
        ("member", builtin_member),
        ("assv", builtin_assv),
        ("gcd", builtin_gcd),
        ("lcm", builtin_lcm),
        ("truncate", builtin_truncate),
        ("round", builtin_round),
        ("string", builtin_string),
        ("string>?", builtin_string_gt),
        ("string<=?", builtin_string_le),
        ("string>=?", builtin_string_ge),
        ("vector-set!", builtin_vector_set_fn),
        // L22: syntax-case support
        ("syntax->datum", builtin_syntax_to_datum),
        ("datum->syntax", builtin_datum_to_syntax),
    ];
    for (name, f) in builtins {
        env_set(&env, name.to_string(), Value::Builtin(*f));
    }
    env_set(&env, "call/cc".to_string(), Value::CallCC);
    env_set(&env, "call-with-current-continuation".to_string(), Value::CallCC);
    env
}

fn eval_top_level(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for (i, expr) in exprs.iter().enumerate() {
        push_body_frame(&exprs[i..], env);
        if i < exprs.len() - 1 {
            match eval(expr, env) {
                Ok(v) => {
                    pop_body_frame();
                    last = v;
                }
                Err(e) => {
                    pop_body_frame();
                    catch_escaped_continuation(Err(e))?;
                }
            }
        } else {
            match eval(expr, env) {
                Ok(v) => {
                    pop_body_frame();
                    last = v;
                }
                Err(e) => {
                    pop_body_frame();
                    return Err(e);
                }
            }
        }
    }
    Ok(last)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    init_cont_state();
    let exprs = parse(input)?;
    let env = make_global_env();
    match eval_top_level(&exprs, &env) {
        Ok(v) => Ok(v.display()),
        Err(EvalError::ContinuationReturn { cont_id }) => {
            let val = CONT_RETURN_VALUE.with(|v| v.borrow_mut().take().unwrap());
            let cont = CONT_REGISTRY.with(|cr| cr.borrow().get(&cont_id).cloned());
            if let Some(cont_data) = cont {
                let result = replay_continuation(&cont_data, val)?;
                Ok(result.display())
            } else {
                Err(EvalError::Runtime("unhandled continuation".into()))
            }
        }
        Err(EvalError::ContinuationResult) => {
            let v = CONT_RESULT_VALUE.with(|v| v.borrow_mut().take().unwrap());
            Ok(v.display())
        }
        Err(e) => Err(e),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    init_cont_state();
    OUTPUT_BUF.with(|buf| buf.borrow_mut().clear());
    let exprs = parse(input)?;
    let env = make_global_env();
    let result = match eval_top_level(&exprs, &env) {
        Ok(v) => v,
        Err(EvalError::ContinuationReturn { cont_id }) => {
            let val = CONT_RETURN_VALUE.with(|v| v.borrow_mut().take().unwrap());
            let cont = CONT_REGISTRY.with(|cr| cr.borrow().get(&cont_id).cloned());
            if let Some(cont_data) = cont {
                replay_continuation(&cont_data, val)?
            } else {
                return Err(EvalError::Runtime("unhandled continuation".into()));
            }
        }
        Err(EvalError::ContinuationResult) => {
            CONT_RESULT_VALUE.with(|v| v.borrow_mut().take().unwrap())
        }
        Err(e) => return Err(e),
    };
    let output = OUTPUT_BUF.with(|buf| buf.borrow().clone());
    Ok((result.display(), output))
}

#[cfg(test)]
mod tests;
