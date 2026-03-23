pub mod error;

pub use error::EvalError;

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

type BuiltinFn = fn(&[Value]) -> Result<Value, EvalError>;

// ---------- Continuation support ----------

#[derive(Clone)]
struct BodyFrame {
    exprs: Vec<Expr>,
    env: Env,
}

struct ContData {
    id: usize,
    frame: Option<BodyFrame>,
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
        CALLCC_OVERRIDE.with(|o| *o.borrow_mut() = Some(val));
        match eval_body_for_replay(&frame.exprs, &frame.env) {
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
    Builtin(BuiltinFn),
    CallCC,
    Continuation(Rc<ContData>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    Void,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "Integer({})", n),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Str(s) => write!(f, "Str({})", s),
            Value::Symbol(s) => write!(f, "Symbol({})", s),
            Value::List(l) => write!(f, "List({:?})", l),
            Value::Lambda { params, .. } => write!(f, "Lambda({:?})", params),
            Value::Char(c) => write!(f, "Char({})", c),
            Value::Builtin(_) => write!(f, "Builtin"),
            Value::CallCC => write!(f, "CallCC"),
            Value::Continuation(_) => write!(f, "Continuation"),
            Value::Macro { .. } => write!(f, "Macro"),
            Value::Void => write!(f, "Void"),
        }
    }
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::Char(c) => format!("#\\{}", c),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda { .. } | Value::Builtin(_) | Value::CallCC | Value::Macro { .. } => "#<procedure>".to_string(),
            Value::Continuation(_) => "#<continuation>".to_string(),
            Value::Void => "".to_string(),
        }
    }

    /// Format for Scheme `display` — strings without quotes.
    fn display_output(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Macro { .. } => "#<macro>".to_string(),
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
                    "define-syntax" => return eval_define_syntax(&elems[1..], env, span).map(Trampoline::Done),
                    _ => {
                        // Check for macro application
                        if let Some(Value::Macro { literals, rules, def_env }) = env_get(env, op) {
                            let expanded = expand_macro(&literals, &rules, &def_env, expr)?;
                            return eval_inner(&expanded, env);
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
                let rest = Value::List(args[params.len()..].to_vec());
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
            let cont_data = Rc::new(ContData { id, frame });
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
            if args.len() != 1 {
                return Err(EvalError::Arity("continuation requires 1 argument".into()));
            }
            CONT_RETURN_VALUE.with(|v| *v.borrow_mut() = Some(args[0].clone()));
            Err(EvalError::ContinuationReturn { cont_id: cont_data.id })
        }
        _ => Err(EvalError::Type("not a procedure".into())),
    }
}

fn apply_value(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match apply_tc(func, args)? {
        Trampoline::Done(v) => Ok(v),
        Trampoline::TailCall { expr, env } => eval(&expr, &env),
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

fn eval_string_set(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("string-set! requires 3 arguments".into()).with_position(span.line, span.col));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("string-set!: first argument must be a variable".into()).with_position(span.line, span.col)),
    };
    let idx_val = eval(&args[1], env)?;
    let idx = as_int(&idx_val)? as usize;
    let char_val = eval(&args[2], env)?;
    let c = match char_val {
        Value::Char(c) => c,
        _ => return Err(EvalError::Type("string-set!: third argument must be a char".into()).with_position(span.line, span.col)),
    };
    let current = env_get(env, &name).ok_or_else(|| EvalError::UnboundVariable(name.clone()))?;
    match current {
        Value::Str(mut s) => {
            if idx >= s.len() {
                return Err(EvalError::Runtime("string-set!: index out of range".into()).with_position(span.line, span.col));
            }
            // Replace char at byte index (assuming ASCII-compatible)
            let bytes = unsafe { s.as_bytes_mut() };
            bytes[idx] = c as u8;
            env_update(env, &name, Value::Str(s))?;
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("string-set!: expected string".into()).with_position(span.line, span.col)),
    }
}

// ---------- Macros (define-syntax / syntax-rules) ----------

thread_local! {
    static GENSYM_COUNTER: Cell<usize> = Cell::new(0);
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
    let sr = match &args[1].kind {
        ExprKind::List(elems) => elems,
        _ => return Err(EvalError::Parse("define-syntax: expected syntax-rules".into()).with_position(span.line, span.col)),
    };
    if sr.is_empty() || !matches!(&sr[0].kind, ExprKind::Symbol(s) if s == "syntax-rules") {
        return Err(EvalError::Parse("define-syntax: expected syntax-rules".into()).with_position(span.line, span.col));
    }
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

fn is_special_form(name: &str) -> bool {
    matches!(name,
        "define" | "if" | "quote" | "lambda" | "and" | "or" | "let" | "begin"
        | "cond" | "set!" | "display" | "write" | "newline" | "string-set!"
        | "define-syntax" | "syntax-rules"
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

// ---------- Builtins ----------

fn as_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type("expected integer".into())),
    }
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum: i64 = 0;
    for a in args { sum += as_int(a)?; }
    Ok(Value::Integer(sum))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    if args.len() == 1 {
        return Ok(Value::Integer(-as_int(&args[0])?));
    }
    let mut r = as_int(&args[0])?;
    for a in &args[1..] { r -= as_int(a)?; }
    Ok(Value::Integer(r))
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut prod: i64 = 1;
    for a in args { prod *= as_int(a)?; }
    Ok(Value::Integer(prod))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
    }
    let mut r = as_int(&args[0])?;
    for a in &args[1..] {
        let d = as_int(a)?;
        if d == 0 { return Err(EvalError::Runtime("division by zero".into())); }
        r /= d;
    }
    Ok(Value::Integer(r))
}

fn builtin_lt(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a < b) }
fn builtin_gt(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a > b) }
fn builtin_eq(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a == b) }
fn builtin_le(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a <= b) }
fn builtin_ge(args: &[Value]) -> Result<Value, EvalError> { cmp_op(args, |a, b| a >= b) }

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("cons requires 2 arguments".into()));
    }
    match &args[1] {
        Value::List(tail) => {
            let mut new_list = vec![args[0].clone()];
            new_list.extend(tail.iter().cloned());
            Ok(Value::List(new_list))
        }
        _ => Err(EvalError::Type("cons: second argument must be a list".into())),
    }
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car requires 1 argument".into()));
    }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        _ => Err(EvalError::Type("car: expected non-empty list".into())),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr requires 1 argument".into()));
    }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        _ => Err(EvalError::Type("cdr: expected non-empty list".into())),
    }
}

fn builtin_null(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("null? requires 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("length requires 1 argument".into()));
    }
    match &args[0] {
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        _ => Err(EvalError::Type("length: expected list".into())),
    }
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for a in args {
        match a {
            Value::List(items) => result.extend(items.iter().cloned()),
            _ => return Err(EvalError::Type("append: expected list".into())),
        }
    }
    Ok(Value::List(result))
}

fn builtin_is_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
}

fn builtin_is_number(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("number? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
}

fn builtin_is_boolean(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
}

fn builtin_is_pair(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("pair? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty())))
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

fn cmp_op(args: &[Value], f: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = as_int(&args[0])?;
    for a in &args[1..] {
        let curr = as_int(a)?;
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
    let tail_list = match last {
        Value::List(l) => l.clone(),
        _ => return Err(EvalError::Type("apply: last argument must be a list".into())),
    };
    let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    all_args.extend(tail_list);
    apply_value(func, &all_args)
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
