use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::EvalError;
use crate::scheme::parser::{Expr, ExprKind, Span};

thread_local! {
    pub static OUTPUT_BUFFER: RefCell<String> = RefCell::new(String::new());
}

pub fn with_output_capture<F, T>(f: F) -> (T, String)
where
    F: FnOnce() -> T,
{
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().clear());
    let result = f();
    let output = OUTPUT_BUFFER.with(|buf| buf.borrow().clone());
    (result, output)
}

fn emit_output(s: &str) {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(s));
}

fn fmt_span(span: Span) -> String {
    format!("{}:{}", span.line, span.col)
}

fn with_span(err: EvalError, span: Span) -> EvalError {
    let pos = fmt_span(span);
    match err {
        EvalError::Parse(msg) if !has_pos(&msg) => EvalError::Parse(format!("{} at {}", msg, pos)),
        EvalError::UnboundVariable(msg) if !has_pos(&msg) => EvalError::UnboundVariable(format!("{} at {}", msg, pos)),
        EvalError::Type(msg) if !has_pos(&msg) => EvalError::Type(format!("{} at {}", msg, pos)),
        EvalError::Arity(msg) if !has_pos(&msg) => EvalError::Arity(format!("{} at {}", msg, pos)),
        EvalError::Runtime(msg) if !has_pos(&msg) => EvalError::Runtime(format!("{} at {}", msg, pos)),
        other => other,
    }
}

fn has_pos(msg: &str) -> bool {
    msg.as_bytes().windows(2).any(|w| w[0].is_ascii_digit() && w[1] == b':')
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    Pair(Box<Value>, Box<Value>),
    Nil,
    Void,
    Builtin(String),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Rc<RefCell<EnvInner>>,
    },
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Void, Value::Void) => true,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    /// write-style display (strings get quotes) — used for eval_str return values
    pub fn to_display(&self) -> String {
        self.fmt_value(true)
    }

    /// display-style output (strings without quotes) — used for `display` builtin
    pub fn to_display_output(&self) -> String {
        self.fmt_value(false)
    }

    fn fmt_value(&self, write_mode: bool) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => {
                if write_mode {
                    format!("\"{}\"", s)
                } else {
                    s.clone()
                }
            }
            Value::Char(c) => {
                if write_mode {
                    match c {
                        ' ' => "#\\space".into(),
                        '\n' => "#\\newline".into(),
                        '\t' => "#\\tab".into(),
                        _ => format!("#\\{}", c),
                    }
                } else {
                    c.to_string()
                }
            }
            Value::Symbol(s) => s.clone(),
            Value::Nil => "()".into(),
            Value::Pair(_, _) => {
                let mut out = String::from("(");
                let mut cur = self;
                let mut first = true;
                loop {
                    match cur {
                        Value::Pair(car, cdr) => {
                            if !first { out.push(' '); }
                            first = false;
                            out.push_str(&car.fmt_value(write_mode));
                            cur = cdr;
                        }
                        Value::Nil => break,
                        other => {
                            out.push_str(" . ");
                            out.push_str(&other.fmt_value(write_mode));
                            break;
                        }
                    }
                }
                out.push(')');
                out
            }
            Value::Void => "".into(),
            Value::Builtin(name) => format!("#<procedure:{}>", name),
            Value::Lambda { .. } => "#<procedure>".into(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

#[derive(Debug)]
pub struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Rc<RefCell<EnvInner>>>,
}

#[derive(Debug, Clone)]
pub struct Env(pub Rc<RefCell<EnvInner>>);

impl Env {
    pub fn default_env() -> Self {
        let mut bindings = HashMap::new();
        for name in ["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                     "cons", "car", "cdr", "null?", "list", "length", "append",
                     "pair?", "number?", "string?", "boolean?", "symbol?", "char?",
                     "display", "write", "newline",
                     "string-append", "string-length", "substring",
                     "string->number", "number->string",
                     "symbol->string", "string->symbol", "string-ref",
                     "string-copy", "make-string", "char->integer", "integer->char"] {
            bindings.insert(name.to_string(), Value::Builtin(name.to_string()));
        }
        Env(Rc::new(RefCell::new(EnvInner {
            bindings,
            parent: None,
        })))
    }

    fn get(&self, name: &str) -> Option<Value> {
        let inner = self.0.borrow();
        if let Some(v) = inner.bindings.get(name) {
            Some(v.clone())
        } else if let Some(parent) = &inner.parent {
            Env(parent.clone()).get(name)
        } else {
            None
        }
    }

    fn define(&self, name: String, val: Value) {
        self.0.borrow_mut().bindings.insert(name, val);
    }

    fn set(&self, name: &str, val: Value) -> Result<(), EvalError> {
        let has_key = self.0.borrow().bindings.contains_key(name);
        if has_key {
            self.0.borrow_mut().bindings.insert(name.to_string(), val);
            Ok(())
        } else {
            let parent = self.0.borrow().parent.clone();
            if let Some(p) = parent {
                Env(p).set(name, val)
            } else {
                Err(EvalError::UnboundVariable(format!("{}", name)))
            }
        }
    }

    fn child(parent: &Env) -> Env {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: HashMap::new(),
            parent: Some(parent.0.clone()),
        })))
    }
}

pub fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let span = expr.span;
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Symbol(name) => {
            env.get(name)
                .ok_or_else(|| EvalError::UnboundVariable(format!("{} at {}", name, fmt_span(span))))
        }
        ExprKind::List(items) if items.is_empty() => {
            Err(EvalError::Runtime(format!("empty application at {}", fmt_span(span))))
        }
        ExprKind::List(items) => {
            let head = &items[0];
            if let ExprKind::Symbol(name) = &head.kind {
                match name.as_str() {
                    "define" => return eval_define(&items[1..], env, span),
                    "if" => return eval_if(&items[1..], env, span),
                    "quote" => return eval_quote(&items[1..], span),
                    "lambda" => return eval_lambda(&items[1..], env, span),
                    "and" => return eval_and(&items[1..], env),
                    "or" => return eval_or(&items[1..], env),
                    "let" => return eval_let(&items[1..], env, span),
                    "begin" => return eval_begin(&items[1..], env),
                    "cond" => return eval_cond(&items[1..], env),
                    "set!" => return eval_set(&items[1..], env, span),
                    "string-set!" => return eval_string_set(&items[1..], env, span),
                    _ => {}
                }
            }

            let func = eval(head, env)?;
            let args: Vec<Value> = items[1..].iter()
                .map(|e| eval(e, env))
                .collect::<Result<_, _>>()?;

            apply_func(&func, &args).map_err(|e| with_span(e, span))
        }
    }
}

fn eval_define(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Runtime(format!("define requires at least 2 arguments at {}", fmt_span(span))));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            let val = eval(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Value::Void)
        }
        ExprKind::List(parts) if !parts.is_empty() => {
            if let ExprKind::Symbol(name) = &parts[0].kind {
                let params: Vec<String> = parts[1..].iter().map(|p| {
                    if let ExprKind::Symbol(s) = &p.kind { Ok(s.clone()) }
                    else { Err(EvalError::Runtime(format!("parameter must be a symbol at {}", fmt_span(p.span)))) }
                }).collect::<Result<_, _>>()?;
                let body = args[1..].to_vec();
                let lambda = Value::Lambda {
                    params,
                    body,
                    env: env.0.clone(),
                };
                env.define(name.clone(), lambda);
                Ok(Value::Void)
            } else {
                Err(EvalError::Runtime(format!("define: expected function name at {}", fmt_span(span))))
            }
        }
        _ => Err(EvalError::Runtime(format!("define: bad syntax at {}", fmt_span(span)))),
    }
}

fn eval_if(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Runtime(format!("if requires 2 or 3 arguments at {}", fmt_span(span))));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_quote(args: &[Expr], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Runtime(format!("quote requires exactly 1 argument at {}", fmt_span(span))));
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
        ExprKind::List(items) => {
            let mut result = Value::Nil;
            for item in items.iter().rev() {
                result = Value::Pair(Box::new(expr_to_value(item)), Box::new(result));
            }
            result
        }
    }
}

fn eval_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Runtime(format!("lambda requires params and body at {}", fmt_span(span))));
    }
    let params = match &args[0].kind {
        ExprKind::List(parts) => {
            parts.iter().map(|p| {
                if let ExprKind::Symbol(s) = &p.kind { Ok(s.clone()) }
                else { Err(EvalError::Runtime(format!("parameter must be a symbol at {}", fmt_span(p.span)))) }
            }).collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Runtime(format!("lambda: expected parameter list at {}", fmt_span(span)))),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        env: env.0.clone(),
    })
}

fn apply_func(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(_) => apply_builtin(func, args),
        Value::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let parent_env = Env(env.clone());
            let local_env = Env::child(&parent_env);
            for (name, val) in params.iter().zip(args.iter()) {
                local_env.define(name.clone(), val.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type(format!("{} is not a procedure", func.to_display()))),
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

fn eval_let(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Runtime(format!("let requires bindings and body at {}", fmt_span(span))));
    }
    // Named let: (let name ((var val) ...) body...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        let bindings = match &args[1].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Runtime(format!("let: expected bindings list at {}", fmt_span(span)))),
        };
        let mut params = Vec::new();
        let mut init_vals = Vec::new();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(p) = &pair[0].kind {
                        params.push(p.clone());
                        init_vals.push(eval(&pair[1], env)?);
                    } else {
                        return Err(EvalError::Runtime(format!("let: binding name must be symbol at {}", fmt_span(b.span))));
                    }
                }
                _ => return Err(EvalError::Runtime(format!("let: bad binding at {}", fmt_span(b.span)))),
            }
        }
        let body = args[2..].to_vec();
        let local_env = Env::child(env);
        let lambda = Value::Lambda {
            params: params.clone(),
            body,
            env: local_env.0.clone(),
        };
        local_env.define(name.clone(), lambda.clone());
        for (p, v) in params.iter().zip(init_vals.iter()) {
            local_env.define(p.clone(), v.clone());
        }
        match &lambda {
            Value::Lambda { body, .. } => {
                let mut result = Value::Void;
                for expr in body {
                    result = eval(expr, &local_env)?;
                }
                Ok(result)
            }
            _ => unreachable!(),
        }
    } else {
        // Regular let: (let ((var val) ...) body...)
        let bindings = match &args[0].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Runtime(format!("let: expected bindings list at {}", fmt_span(span)))),
        };
        let local_env = Env::child(env);
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(name) = &pair[0].kind {
                        let val = eval(&pair[1], env)?;
                        local_env.define(name.clone(), val);
                    } else {
                        return Err(EvalError::Runtime(format!("let: binding name must be symbol at {}", fmt_span(b.span))));
                    }
                }
                _ => return Err(EvalError::Runtime(format!("let: bad binding at {}", fmt_span(b.span)))),
            }
        }
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = eval(expr, &local_env)?;
        }
        Ok(result)
    }
}

fn eval_set(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Runtime(format!("set! requires exactly 2 arguments at {}", fmt_span(span))));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Runtime(format!("set!: first argument must be a symbol at {}", fmt_span(span)))),
    };
    if env.get(&name).is_none() {
        return Err(EvalError::UnboundVariable(format!("{} at {}", name, fmt_span(span))));
    }
    let val = eval(&args[1], env)?;
    env.set(&name, val)?;
    Ok(Value::Void)
}

fn eval_string_set(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!("string-set! requires 3 arguments at {}", fmt_span(span))));
    }
    // Evaluate all arguments
    let target = eval(&args[0], env)?;
    let idx_val = eval(&args[1], env)?;
    let ch_val = eval(&args[2], env)?;

    let mut s = match target {
        Value::Str(s) => s,
        _ => return Err(EvalError::Type("string-set!: expected string".into())),
    };
    let idx = expect_int(&idx_val)? as usize;
    let ch = match ch_val {
        Value::Char(c) => c,
        _ => return Err(EvalError::Type("string-set!: expected char".into())),
    };

    if idx >= s.len() {
        return Err(EvalError::Runtime("string-set!: index out of range".into()));
    }
    // SAFETY: we checked idx < len, and we're replacing a single byte with a single ASCII-range char
    unsafe { s.as_bytes_mut()[idx] = ch as u8; }

    // If the first arg was a variable, update it in the environment
    if let ExprKind::Symbol(name) = &args[0].kind {
        env.set(name, Value::Str(s)).ok();
    }
    Ok(Value::Void)
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &Env) -> Result<Value, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env)?;
                if test.is_truthy() {
                    let mut result = Value::Void;
                    for expr in &parts[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Runtime(format!("cond: bad clause at {}", fmt_span(clause.span)))),
        }
    }
    Ok(Value::Void)
}

fn apply_builtin(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    let name = match func {
        Value::Builtin(name) => name.as_str(),
        _ => return Err(EvalError::Type(format!("{} is not a procedure", func.to_display()))),
    };

    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_int(a)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-expect_int(&args[0])?));
            }
            let mut result = expect_int(&args[0])?;
            for a in &args[1..] {
                result -= expect_int(a)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= expect_int(a)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
            }
            let mut result = expect_int(&args[0])?;
            for a in &args[1..] {
                let d = expect_int(a)?;
                if d == 0 {
                    return Err(EvalError::Runtime("division by zero".into()));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => cmp_op(args, |a, b| a < b),
        ">" => cmp_op(args, |a, b| a > b),
        "=" => cmp_op(args, |a, b| a == b),
        "<=" => cmp_op(args, |a, b| a <= b),
        ">=" => cmp_op(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires exactly 1 argument".into()));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("cons requires exactly 2 arguments".into()));
            }
            Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car requires exactly 1 argument".into()));
            }
            match &args[0] {
                Value::Pair(car, _) => Ok(*car.clone()),
                _ => Err(EvalError::Type("car: not a pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr requires exactly 1 argument".into()));
            }
            match &args[0] {
                Value::Pair(_, cdr) => Ok(*cdr.clone()),
                _ => Err(EvalError::Type("cdr: not a pair".into())),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("null? requires exactly 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(args[0], Value::Nil)))
        }
        "list" => {
            let mut result = Value::Nil;
            for a in args.iter().rev() {
                result = Value::Pair(Box::new(a.clone()), Box::new(result));
            }
            Ok(result)
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length requires exactly 1 argument".into()));
            }
            let mut count = 0i64;
            let mut cur = &args[0];
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(_, cdr) => { count += 1; cur = cdr; }
                    _ => return Err(EvalError::Type("length: not a proper list".into())),
                }
            }
            Ok(Value::Integer(count))
        }
        "append" => {
            if args.is_empty() {
                return Ok(Value::Nil);
            }
            let mut result = args.last().unwrap().clone();
            for a in args[..args.len()-1].iter().rev() {
                let mut elems = Vec::new();
                let mut cur = a;
                loop {
                    match cur {
                        Value::Nil => break,
                        Value::Pair(car, cdr) => { elems.push(*car.clone()); cur = cdr; }
                        _ => return Err(EvalError::Type("append: not a proper list".into())),
                    }
                }
                for e in elems.into_iter().rev() {
                    result = Value::Pair(Box::new(e), Box::new(result));
                }
            }
            Ok(result)
        }
        "pair?" => {
            if args.len() != 1 { return Err(EvalError::Arity("pair? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Pair(_, _))))
        }
        "number?" => {
            if args.len() != 1 { return Err(EvalError::Arity("number? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
        }
        "string?" => {
            if args.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Str(_))))
        }
        "boolean?" => {
            if args.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
        }
        "char?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Char(_))))
        }
        "display" => {
            if args.len() != 1 { return Err(EvalError::Arity("display requires 1 argument".into())); }
            emit_output(&args[0].to_display_output());
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 { return Err(EvalError::Arity("write requires 1 argument".into())); }
            emit_output(&args[0].to_display());
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() { return Err(EvalError::Arity("newline requires 0 arguments".into())); }
            emit_output("\n");
            Ok(Value::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type("string-append: expected string".into())),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-length requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type("string-length: expected string".into())),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity("substring requires 3 arguments".into())); }
            let s = match &args[0] { Value::Str(s) => s, _ => return Err(EvalError::Type("substring: expected string".into())) };
            let start = expect_int(&args[1])? as usize;
            let end = expect_int(&args[2])? as usize;
            if end > s.len() || start > end {
                return Err(EvalError::Runtime("substring: index out of range".into()));
            }
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->number requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type("string->number: expected string".into())),
            }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("number->string requires 1 argument".into())); }
            Ok(Value::Str(expect_int(&args[0])?.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol->string requires 1 argument".into())); }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
            }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->symbol requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type("string->symbol: expected string".into())),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ref requires 2 arguments".into())); }
            let s = match &args[0] { Value::Str(s) => s, _ => return Err(EvalError::Type("string-ref: expected string".into())) };
            let idx = expect_int(&args[1])? as usize;
            if idx >= s.len() {
                return Err(EvalError::Runtime("string-ref: index out of range".into()));
            }
            Ok(Value::Char(s.as_bytes()[idx] as char))
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-copy requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type("string-copy: expected string".into())),
            }
        }
        "make-string" => {
            if args.is_empty() || args.len() > 2 { return Err(EvalError::Arity("make-string requires 1 or 2 arguments".into())); }
            let len = expect_int(&args[0])? as usize;
            let ch = if args.len() == 2 {
                match &args[1] { Value::Char(c) => *c, _ => return Err(EvalError::Type("make-string: expected char".into())) }
            } else { '\0' };
            Ok(Value::Str(std::iter::repeat(ch).take(len).collect()))
        }
        "char->integer" => {
            if args.len() != 1 { return Err(EvalError::Arity("char->integer requires 1 argument".into())); }
            match &args[0] { Value::Char(c) => Ok(Value::Integer(*c as i64)), _ => Err(EvalError::Type("char->integer: expected char".into())) }
        }
        "integer->char" => {
            if args.len() != 1 { return Err(EvalError::Arity("integer->char requires 1 argument".into())); }
            let n = expect_int(&args[0])?;
            Ok(Value::Char(char::from_u32(n as u32).unwrap_or('\u{FFFD}')))
        }
        _ => Err(EvalError::Runtime(format!("unknown builtin: {}", name))),
    }
}

fn expect_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected number, got {}", v.to_display()))),
    }
}

fn cmp_op(args: &[Value], op: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = expect_int(&args[0])?;
    for a in &args[1..] {
        let curr = expect_int(a)?;
        if !op(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}
