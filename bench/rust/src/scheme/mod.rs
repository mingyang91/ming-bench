pub mod error;
mod macros;

pub use error::EvalError;
use error::Span;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

const DUMMY_SPAN: Span = Span { line: 0, col: 0 };

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

pub(crate) fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}__g{}", base, n)
}

pub(crate) type Output = Rc<RefCell<String>>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Spanned>),
    Pair(Box<Value>, Box<Value>), // improper pair (a . b) where b is not a list
    Lambda(Vec<String>, Option<String>, Vec<Spanned>, Env), // params, rest_param, body, closure env
    SyntaxRules {
        literals: Vec<String>,
        rules: Vec<(Spanned, Spanned)>, // (pattern, template)
        def_env: Env,
    },
    Void,
}

/// A value annotated with its source position.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Spanned {
    pub(crate) val: Value,
    pub(crate) span: Span,
}

impl Spanned {
    pub(crate) fn new(val: Value, span: Span) -> Self {
        Self { val, span }
    }
}

impl Value {
    fn display_value(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Char(c) => format!("#\\{}", match c {
                ' ' => "space".to_string(),
                '\n' => "newline".to_string(),
                '\t' => "tab".to_string(),
                _ => c.to_string(),
            }),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.val.display_value()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(a, b) => format!("({} . {})", a.display_value(), b.display_value()),
            Value::Lambda(..) => "#<procedure>".into(),
            Value::SyntaxRules { .. } => "#<syntax>".into(),
            Value::Void => "".into(),
        }
    }

    /// Format for `display` — strings without quotes, chars as raw char.
    fn format_display(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.val.format_display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(a, b) => format!("({} . {})", a.format_display(), b.format_display()),
            _ => self.display_value(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// --- Environment ---

pub(crate) type Env = Rc<RefCell<EnvInner>>;

#[derive(Debug, PartialEq)]
pub(crate) struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

fn new_env(parent: Option<Env>) -> Env {
    Rc::new(RefCell::new(EnvInner {
        bindings: HashMap::new(),
        parent,
    }))
}

pub(crate) fn env_get(env: &Env, name: &str) -> Option<Value> {
    let inner = env.borrow();
    if let Some(val) = inner.bindings.get(name) {
        Some(val.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        None
    }
}

pub(crate) fn env_set(env: &Env, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

fn env_update(env: &Env, name: &str, val: Value) -> bool {
    let mut inner = env.borrow_mut();
    if inner.bindings.contains_key(name) {
        inner.bindings.insert(name.to_string(), val);
        return true;
    }
    if let Some(ref parent) = inner.parent {
        env_update(parent, name, val)
    } else {
        false
    }
}

// --- Parser ---

struct Parser {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    fn current_span(&self) -> Span {
        Span { line: self.line, col: self.col }
    }

    fn advance(&mut self) {
        if self.pos < self.chars.len() {
            if self.chars[self.pos] == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.chars.len() {
            if self.chars[self.pos].is_whitespace() {
                self.advance();
            } else if self.chars[self.pos] == ';' {
                while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
                    self.advance();
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn parse_expr(&mut self) -> Result<Spanned, EvalError> {
        self.skip_whitespace();
        let span = self.current_span();
        match self.peek() {
            None => Err(EvalError::Parse("unexpected end of input".into(), span)),
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string(),
            Some('#') => self.parse_hash(),
            Some('\'') => {
                self.advance();
                let inner = self.parse_expr()?;
                Ok(Spanned::new(
                    Value::List(vec![
                        Spanned::new(Value::Symbol("quote".into()), span),
                        inner,
                    ]),
                    span,
                ))
            }
            Some(_) => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Spanned, EvalError> {
        let span = self.current_span();
        self.advance(); // skip '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace();
            match self.peek() {
                None => return Err(EvalError::Parse("unterminated list".into(), span)),
                Some(')') => {
                    self.advance();
                    return Ok(Spanned::new(Value::List(items), span));
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Spanned, EvalError> {
        let span = self.current_span();
        self.advance();
        let mut s = String::new();
        loop {
            match self.chars.get(self.pos) {
                None => return Err(EvalError::Parse("unterminated string".into(), span)),
                Some('\\') => {
                    self.advance();
                    match self.chars.get(self.pos) {
                        Some('n') => { s.push('\n'); self.advance(); }
                        Some('t') => { s.push('\t'); self.advance(); }
                        Some('\\') => { s.push('\\'); self.advance(); }
                        Some('"') => { s.push('"'); self.advance(); }
                        Some(c) => { s.push(*c); self.advance(); }
                        None => return Err(EvalError::Parse("unterminated escape".into(), span)),
                    }
                }
                Some('"') => {
                    self.advance();
                    return Ok(Spanned::new(Value::Str(s), span));
                }
                Some(c) => {
                    s.push(*c);
                    self.advance();
                }
            }
        }
    }

    fn parse_hash(&mut self) -> Result<Spanned, EvalError> {
        let span = self.current_span();
        self.advance();
        match self.peek() {
            Some('t') => {
                self.advance();
                if self.peek().is_none_or(is_delimiter) {
                    Ok(Spanned::new(Value::Boolean(true), span))
                } else {
                    Err(EvalError::Parse("invalid # literal".into(), span))
                }
            }
            Some('f') => {
                self.advance();
                if self.peek().is_none_or(is_delimiter) {
                    Ok(Spanned::new(Value::Boolean(false), span))
                } else {
                    Err(EvalError::Parse("invalid # literal".into(), span))
                }
            }
            Some('\\') => {
                self.advance(); // skip '\'
                // Named characters
                let start = self.pos;
                while self.pos < self.chars.len() && !is_delimiter(self.chars[self.pos]) {
                    self.advance();
                }
                let name: String = self.chars[start..self.pos].iter().collect();
                let c = match name.as_str() {
                    "space" => ' ',
                    "newline" => '\n',
                    "tab" => '\t',
                    s if s.chars().count() == 1 => s.chars().next().expect("single-char string has a first char"),
                    _ => return Err(EvalError::Parse(format!("unknown character name: {name}"), span)),
                };
                Ok(Spanned::new(Value::Char(c), span))
            }
            _ => Err(EvalError::Parse("invalid # literal".into(), span)),
        }
    }

    fn parse_atom(&mut self) -> Result<Spanned, EvalError> {
        let span = self.current_span();
        let start = self.pos;
        while self.pos < self.chars.len() && !is_delimiter(self.chars[self.pos]) {
            self.advance();
        }
        let token: String = self.chars[start..self.pos].iter().collect();
        if let Ok(n) = token.parse::<i64>() {
            Ok(Spanned::new(Value::Integer(n), span))
        } else {
            Ok(Spanned::new(Value::Symbol(token), span))
        }
    }

    fn parse_all(&mut self) -> Result<Vec<Spanned>, EvalError> {
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

fn is_delimiter(c: char) -> bool {
    c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == ';'
}

/// Parse a parameter list, returning (fixed_params, optional_rest_param).
/// Handles dot notation: `(x y . rest)` → (["x", "y"], Some("rest"))
fn parse_params(values: &[Spanned], context: &str, span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    // Look for dot
    let dot_pos = values.iter().position(|v| matches!(&v.val, Value::Symbol(s) if s == "."));
    if let Some(pos) = dot_pos {
        if pos + 1 != values.len() - 1 {
            return Err(EvalError::Parse(format!("{context}: malformed dot parameter list"), span));
        }
        let fixed: Result<Vec<String>, _> = values[..pos]
            .iter()
            .map(|v| match &v.val {
                Value::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type(format!("{context}: expected symbol in parameter list"), span)),
            })
            .collect();
        let rest = match &values[pos + 1].val {
            Value::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Type(format!("{context}: expected symbol after dot"), span)),
        };
        Ok((fixed?, Some(rest)))
    } else {
        let params: Result<Vec<String>, _> = values
            .iter()
            .map(|v| match &v.val {
                Value::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type(format!("{context}: expected symbol in parameter list"), span)),
            })
            .collect();
        Ok((params?, None))
    }
}

// --- Macros (syntax-rules) ---


fn apply_macro(
    items: &[Spanned],
    literals: &[String],
    rules: &[(Spanned, Spanned)],
    def_env: &Env,
    env: &Env,
    out: &Output,
    span: Span,
) -> Result<Value, EvalError> {
    let Some((expanded, renames)) = macros::try_expand(items, literals, rules) else {
        return Err(EvalError::Type("no matching pattern for macro".into(), span));
    };
    for (orig, gs) in &renames {
        if let Some(val) = env_get(def_env, orig) {
            env_set(env, gs.clone(), val);
        }
    }
    eval(&expanded, env, out)
}

// --- Evaluator ---

fn eval(expr: &Spanned, env: &Env, out: &Output) -> Result<Value, EvalError> {
    let span = expr.span;
    match &expr.val {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Char(_) | Value::Pair(..) | Value::Lambda(..) | Value::SyntaxRules { .. } => Ok(expr.val.clone()),
        Value::Symbol(name) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable(name.clone(), span))
        }
        Value::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into(), span));
            }
            let head = &items[0];
            if let Value::Symbol(name) = &head.val {
                match name.as_str() {
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("quote requires 1 argument".into(), span));
                        }
                        return Ok(items[1].val.clone());
                    }
                    "if" => {
                        if items.len() < 3 || items.len() > 4 {
                            return Err(EvalError::Arity("if requires 2 or 3 arguments".into(), span));
                        }
                        let cond = eval(&items[1], env, out)?;
                        if cond.is_truthy() {
                            return eval(&items[2], env, out);
                        } else if items.len() == 4 {
                            return eval(&items[3], env, out);
                        } else {
                            return Ok(Value::Void);
                        }
                    }
                    "define" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("define requires at least 2 arguments".into(), span));
                        }
                        match &items[1].val {
                            Value::Symbol(var_name) => {
                                let val = eval(&items[2], env, out)?;
                                env_set(env, var_name.clone(), val);
                                return Ok(Value::Void);
                            }
                            Value::List(sig) => {
                                if sig.is_empty() {
                                    return Err(EvalError::Parse("define: empty signature".into(), span));
                                }
                                let func_name = match &sig[0].val {
                                    Value::Symbol(s) => s.clone(),
                                    _ => return Err(EvalError::Type("define: expected symbol for function name".into(), span)),
                                };
                                let (params, rest) = parse_params(&sig[1..], "define", span)?;
                                let body = items[2..].to_vec();
                                let lambda = Value::Lambda(params, rest, body, env.clone());
                                env_set(env, func_name, lambda);
                                return Ok(Value::Void);
                            }
                            _ => return Err(EvalError::Type("define: expected symbol or list".into(), span)),
                        }
                    }
                    "lambda" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("lambda requires at least 2 arguments".into(), span));
                        }
                        let (params, rest) = match &items[1].val {
                            Value::List(param_list) => parse_params(param_list, "lambda", span)?,
                            Value::Symbol(s) => (vec![], Some(s.clone())), // (lambda args body)
                            _ => return Err(EvalError::Type("lambda: expected parameter list".into(), span)),
                        };
                        let body = items[2..].to_vec();
                        return Ok(Value::Lambda(params, rest, body, env.clone()));
                    }
                    "let" => return eval_let(&items[1..], env, out, span),
                    "begin" => {
                        let mut result = Value::Void;
                        for expr in &items[1..] {
                            result = eval(expr, env, out)?;
                        }
                        return Ok(result);
                    }
                    "set!" => {
                        if items.len() != 3 {
                            return Err(EvalError::Arity("set! requires 2 arguments".into(), span));
                        }
                        let Value::Symbol(name) = &items[1].val else {
                            return Err(EvalError::Type("set!: first argument must be a symbol".into(), span));
                        };
                        let val = eval(&items[2], env, out)?;
                        if !env_update(env, name, val) {
                            return Err(EvalError::UnboundVariable(name.clone(), span));
                        }
                        return Ok(Value::Void);
                    }
                    "cond" => return eval_cond(&items[1..], env, out, span),
                    "and" => return eval_and(&items[1..], env, out),
                    "or" => return eval_or(&items[1..], env, out),
                    "string-set!" => {
                        if items.len() != 4 {
                            return Err(EvalError::Arity("string-set! requires 3 arguments".into(), span));
                        }
                        let Value::Symbol(var_name) = &items[1].val else {
                            return Err(EvalError::Type("string-set!: first argument must be a variable".into(), span));
                        };
                        let idx = as_integer(&eval(&items[2], env, out)?, span)? as usize;
                        let ch = match eval(&items[3], env, out)? {
                            Value::Char(c) => c,
                            _ => return Err(EvalError::Type("string-set!: third argument must be a char".into(), span)),
                        };
                        let s = env_get(env, var_name).ok_or_else(|| EvalError::UnboundVariable(var_name.clone(), span))?;
                        match s {
                            Value::Str(st) => {
                                let mut chars: Vec<char> = st.chars().collect();
                                if idx >= chars.len() {
                                    return Err(EvalError::Type("string-set!: index out of bounds".into(), span));
                                }
                                chars[idx] = ch;
                                env_update(env, var_name, Value::Str(chars.into_iter().collect()));
                                return Ok(Value::Void);
                            }
                            _ => return Err(EvalError::Type("string-set!: expected string".into(), span)),
                        }
                    }
                    "not" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("not requires 1 argument".into(), span));
                        }
                        let v = eval(&items[1], env, out)?;
                        return Ok(Value::Boolean(!v.is_truthy()));
                    }
                    "define-syntax" => {
                        if items.len() != 3 {
                            return Err(EvalError::Arity("define-syntax requires 2 arguments".into(), span));
                        }
                        let Value::Symbol(macro_name) = &items[1].val else {
                            return Err(EvalError::Type("define-syntax: expected symbol".into(), span));
                        };
                        let Value::List(sr_parts) = &items[2].val else {
                            return Err(EvalError::Type("define-syntax: expected syntax-rules".into(), span));
                        };
                        if sr_parts.is_empty() || !matches!(&sr_parts[0].val, Value::Symbol(s) if s == "syntax-rules") {
                            return Err(EvalError::Type("define-syntax: expected syntax-rules form".into(), span));
                        }
                        if sr_parts.len() < 2 {
                            return Err(EvalError::Arity("syntax-rules requires literals list".into(), span));
                        }
                        let Value::List(lit_list) = &sr_parts[1].val else {
                            return Err(EvalError::Type("syntax-rules: expected literals list".into(), span));
                        };
                        let literals: Vec<String> = lit_list.iter().filter_map(|l| {
                            if let Value::Symbol(s) = &l.val { Some(s.clone()) } else { None }
                        }).collect();
                        let mut rules = Vec::new();
                        for rule in &sr_parts[2..] {
                            let Value::List(parts) = &rule.val else {
                                return Err(EvalError::Type("syntax-rules: expected rule".into(), span));
                            };
                            if parts.len() != 2 {
                                return Err(EvalError::Arity("syntax-rules: rule needs pattern and template".into(), span));
                            }
                            rules.push((parts[0].clone(), parts[1].clone()));
                        }
                        let val = Value::SyntaxRules { literals, rules, def_env: env.clone() };
                        env_set(env, macro_name.clone(), val);
                        return Ok(Value::Void);
                    }
                    _ => {
                        // Check for macro application
                        if let Some(Value::SyntaxRules { ref literals, ref rules, ref def_env }) = env_get(env, name) {
                            return apply_macro(items, literals, rules, def_env, env, out, span);
                        }
                    }
                }
            }
            // Function application
            let func = eval(head, env, out)?;
            let args: Result<Vec<Value>, _> = items[1..].iter().map(|a| eval(a, env, out)).collect();
            let args = args?;
            apply(&func, &args, out, span)
        }
        Value::Void => Ok(Value::Void),
    }
}

fn apply(func: &Value, args: &[Value], out: &Output, span: Span) -> Result<Value, EvalError> {
    match func {
        Value::Lambda(params, rest, body, closure_env) => {
            if let Some(rest_name) = rest {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {}", params.len(), args.len()
                    ), span));
                }
                let local_env = new_env(Some(closure_env.clone()));
                for (param, arg) in params.iter().zip(args.iter()) {
                    env_set(&local_env, param.clone(), arg.clone());
                }
                let rest_list = args[params.len()..].iter()
                    .map(|a| Spanned::new(a.clone(), DUMMY_SPAN))
                    .collect();
                env_set(&local_env, rest_name.clone(), Value::List(rest_list));
                let mut result = Value::Void;
                for expr in body {
                    result = eval(expr, &local_env, out)?;
                }
                Ok(result)
            } else {
                if args.len() != params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected {} arguments, got {}", params.len(), args.len()
                    ), span));
                }
                let local_env = new_env(Some(closure_env.clone()));
                for (param, arg) in params.iter().zip(args.iter()) {
                    env_set(&local_env, param.clone(), arg.clone());
                }
                let mut result = Value::Void;
                for expr in body {
                    result = eval(expr, &local_env, out)?;
                }
                Ok(result)
            }
        }
        Value::Symbol(name) => apply_builtin(name, args, out, span),
        _ => Err(EvalError::Type("not a procedure".into(), span)),
    }
}

fn eval_and(exprs: &[Spanned], env: &Env, out: &Output) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr, env, out)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Spanned], env: &Env, out: &Output) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in exprs {
        let result = eval(expr, env, out)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_let(args: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("let requires bindings and body".into(), span));
    }
    // Named let: (let name ((var init) ...) body...)
    if let Value::Symbol(name) = &args[0].val {
        if args.len() < 3 {
            return Err(EvalError::Arity("named let requires bindings and body".into(), span));
        }
        let Value::List(bindings) = &args[1].val else {
            return Err(EvalError::Type("named let: expected bindings list".into(), span));
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings {
            let Value::List(pair) = &b.val else {
                return Err(EvalError::Type("let: binding must be a list".into(), span));
            };
            if pair.len() != 2 {
                return Err(EvalError::Arity("let: binding must have 2 elements".into(), span));
            }
            let Value::Symbol(p) = &pair[0].val else {
                return Err(EvalError::Type("let: expected symbol in binding".into(), span));
            };
            params.push(p.clone());
            inits.push(eval(&pair[1], env, out)?);
        }
        let body = args[2..].to_vec();
        let loop_env = new_env(Some(env.clone()));
        let lambda = Value::Lambda(params.clone(), None, body, loop_env.clone());
        env_set(&loop_env, name.clone(), lambda);
        let call_env = new_env(Some(loop_env));
        for (p, v) in params.iter().zip(inits.iter()) {
            env_set(&call_env, p.clone(), v.clone());
        }
        let body_ref = &args[2..];
        let mut result = Value::Void;
        for expr in body_ref {
            result = eval(expr, &call_env, out)?;
        }
        return Ok(result);
    }
    // Regular let: (let ((var init) ...) body...)
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into(), span));
    }
    let Value::List(bindings) = &args[0].val else {
        return Err(EvalError::Type("let: expected bindings list".into(), span));
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings {
        let Value::List(pair) = &b.val else {
            return Err(EvalError::Type("let: binding must be a list".into(), span));
        };
        if pair.len() != 2 {
            return Err(EvalError::Arity("let: binding must have 2 elements".into(), span));
        }
        let Value::Symbol(name) = &pair[0].val else {
            return Err(EvalError::Type("let: expected symbol in binding".into(), span));
        };
        let val = eval(&pair[1], env, out)?;
        env_set(&local_env, name.clone(), val);
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, out)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Value, EvalError> {
    for clause in clauses {
        let Value::List(parts) = &clause.val else {
            return Err(EvalError::Type("cond: expected list clause".into(), span));
        };
        if parts.is_empty() {
            return Err(EvalError::Arity("cond: empty clause".into(), span));
        }
        if let Value::Symbol(s) = &parts[0].val {
            if s == "else" {
                let mut result = Value::Void;
                for expr in &parts[1..] {
                    result = eval(expr, env, out)?;
                }
                return Ok(result);
            }
        }
        let test = eval(&parts[0], env, out)?;
        if test.is_truthy() {
            let mut result = test;
            for expr in &parts[1..] {
                result = eval(expr, env, out)?;
            }
            return Ok(result);
        }
    }
    Ok(Value::Void)
}

fn builtin_arithmetic(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_integer(a, span)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into(), span));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-as_integer(&args[0], span)?));
            }
            let mut result = as_integer(&args[0], span)?;
            for a in &args[1..] {
                result -= as_integer(a, span)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_integer(a, span)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into(), span));
            }
            let mut result = as_integer(&args[0], span)?;
            for a in &args[1..] {
                let d = as_integer(a, span)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero(span));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "modulo" | "remainder" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{name} requires 2 arguments"), span));
            }
            let a = as_integer(&args[0], span)?;
            let b = as_integer(&args[1], span)?;
            if b == 0 { return Err(EvalError::DivisionByZero(span)); }
            Ok(Value::Integer(if name == "modulo" { ((a % b) + b) % b } else { a % b }))
        }
        "<" => compare_nums(args, |a, b| a < b, span),
        ">" => compare_nums(args, |a, b| a > b, span),
        "=" => compare_nums(args, |a, b| a == b, span),
        "<=" => compare_nums(args, |a, b| a <= b, span),
        ">=" => compare_nums(args, |a, b| a >= b, span),
        "zero?" => {
            if args.len() != 1 { return Err(EvalError::Arity("zero? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(as_integer(&args[0], span)? == 0))
        }
        "positive?" => {
            if args.len() != 1 { return Err(EvalError::Arity("positive? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(as_integer(&args[0], span)? > 0))
        }
        "negative?" => {
            if args.len() != 1 { return Err(EvalError::Arity("negative? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(as_integer(&args[0], span)? < 0))
        }
        "odd?" => {
            if args.len() != 1 { return Err(EvalError::Arity("odd? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(as_integer(&args[0], span)? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 { return Err(EvalError::Arity("even? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(as_integer(&args[0], span)? % 2 == 0))
        }
        "abs" => {
            if args.len() != 1 { return Err(EvalError::Arity("abs requires 1 argument".into(), span)); }
            Ok(Value::Integer(as_integer(&args[0], span)?.abs()))
        }
        "quotient" => {
            if args.len() != 2 { return Err(EvalError::Arity("quotient requires 2 arguments".into(), span)); }
            let a = as_integer(&args[0], span)?;
            let b = as_integer(&args[1], span)?;
            if b == 0 { return Err(EvalError::DivisionByZero(span)); }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() { return Err(EvalError::Arity("min requires at least 1 argument".into(), span)); }
            let mut m = as_integer(&args[0], span)?;
            for a in &args[1..] { m = m.min(as_integer(a, span)?); }
            Ok(Value::Integer(m))
        }
        "max" => {
            if args.is_empty() { return Err(EvalError::Arity("max requires at least 1 argument".into(), span)); }
            let mut m = as_integer(&args[0], span)?;
            for a in &args[1..] { m = m.max(as_integer(a, span)?); }
            Ok(Value::Integer(m))
        }
        "expt" => {
            if args.len() != 2 { return Err(EvalError::Arity("expt requires 2 arguments".into(), span)); }
            let base = as_integer(&args[0], span)?;
            let exp = as_integer(&args[1], span)?;
            if exp < 0 { return Err(EvalError::Type("expt: negative exponent".into(), span)); }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        _ => Err(EvalError::UnboundVariable(name.into(), span)),
    }
}

fn builtin_list(name: &str, args: &[Value], out: &Output, span: Span) -> Result<Value, EvalError> {
    match name {
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("cons requires 2 arguments".into(), span));
            }
            match &args[1] {
                Value::List(tail) => {
                    let mut new_list = vec![Spanned::new(args[0].clone(), DUMMY_SPAN)];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => {
                    Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
                }
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car requires 1 argument".into(), span));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].val.clone()),
                Value::Pair(a, _) => Ok(*a.clone()),
                _ => Err(EvalError::Type("car: not a pair".into(), span)),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr requires 1 argument".into(), span));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
                Value::Pair(_, b) => Ok(*b.clone()),
                _ => Err(EvalError::Type("cdr: not a pair".into(), span)),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("null? requires 1 argument".into(), span));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => Ok(Value::List(args.iter().map(|a| Spanned::new(a.clone(), DUMMY_SPAN)).collect())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length requires 1 argument".into(), span));
            }
            match &args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(EvalError::Type("length: not a list".into(), span)),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for (i, arg) in args.iter().enumerate() {
                if i < args.len() - 1 {
                    match arg {
                        Value::List(items) => result.extend(items.iter().cloned()),
                        _ => return Err(EvalError::Type("append: not a list".into(), span)),
                    }
                } else {
                    match arg {
                        Value::List(items) => result.extend(items.iter().cloned()),
                        _ => result.push(Spanned::new(arg.clone(), DUMMY_SPAN)),
                    }
                }
            }
            Ok(Value::List(result))
        }
        "list?" => {
            if args.len() != 1 { return Err(EvalError::Arity("list? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(_))))
        }
        "list-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("list-ref requires 2 arguments".into(), span)); }
            let items = match &args[0] {
                Value::List(items) => items,
                _ => return Err(EvalError::Type("list-ref: not a list".into(), span)),
            };
            let idx = as_integer(&args[1], span)? as usize;
            items.get(idx).map(|s| s.val.clone())
                .ok_or_else(|| EvalError::Type("list-ref: index out of bounds".into(), span))
        }
        "list-tail" => {
            if args.len() != 2 { return Err(EvalError::Arity("list-tail requires 2 arguments".into(), span)); }
            let items = match &args[0] {
                Value::List(items) => items,
                _ => return Err(EvalError::Type("list-tail: not a list".into(), span)),
            };
            let idx = as_integer(&args[1], span)? as usize;
            if idx > items.len() {
                return Err(EvalError::Type("list-tail: index out of bounds".into(), span));
            }
            Ok(Value::List(items[idx..].to_vec()))
        }
        "assoc" => {
            if args.len() != 2 { return Err(EvalError::Arity("assoc requires 2 arguments".into(), span)); }
            let key = &args[0];
            let alist = match &args[1] {
                Value::List(items) => items,
                _ => return Err(EvalError::Type("assoc: not a list".into(), span)),
            };
            for entry in alist {
                if let Value::List(pair) = &entry.val {
                    if !pair.is_empty() && values_equal(&pair[0].val, key) {
                        return Ok(entry.val.clone());
                    }
                }
            }
            Ok(Value::Boolean(false))
        }
        "map" => {
            if args.len() < 2 { return Err(EvalError::Arity("map requires at least 2 arguments".into(), span)); }
            let func = &args[0];
            let lists: Vec<&Vec<Spanned>> = args[1..].iter().map(|a| match a {
                Value::List(items) => Ok(items),
                _ => Err(EvalError::Type("map: expected list".into(), span)),
            }).collect::<Result<_, _>>()?;
            let len = lists[0].len();
            let mut result = Vec::new();
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].val.clone()).collect();
                let val = apply(func, &call_args, out, span)?;
                result.push(Spanned::new(val, DUMMY_SPAN));
            }
            Ok(Value::List(result))
        }
        "for-each" => {
            if args.len() < 2 { return Err(EvalError::Arity("for-each requires at least 2 arguments".into(), span)); }
            let func = &args[0];
            let lists: Vec<&Vec<Spanned>> = args[1..].iter().map(|a| match a {
                Value::List(items) => Ok(items),
                _ => Err(EvalError::Type("for-each: expected list".into(), span)),
            }).collect::<Result<_, _>>()?;
            let len = lists[0].len();
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].val.clone()).collect();
                apply(func, &call_args, out, span)?;
            }
            Ok(Value::Void)
        }
        _ => Err(EvalError::UnboundVariable(name.into(), span)),
    }
}

fn builtin_string_char_io(name: &str, args: &[Value], out: &Output, span: Span) -> Result<Value, EvalError> {
    match name {
        "display" => {
            if args.len() != 1 { return Err(EvalError::Arity("display requires 1 argument".into(), span)); }
            out.borrow_mut().push_str(&args[0].format_display());
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 { return Err(EvalError::Arity("write requires 1 argument".into(), span)); }
            out.borrow_mut().push_str(&args[0].display_value());
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() { return Err(EvalError::Arity("newline requires 0 arguments".into(), span)); }
            out.borrow_mut().push('\n');
            Ok(Value::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type("string-append: expected string".into(), span)),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-length requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type("string-length: expected string".into(), span)),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity("substring requires 3 arguments".into(), span)); }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type("substring: expected string".into(), span)),
            };
            let start = as_integer(&args[1], span)? as usize;
            let end = as_integer(&args[2], span)? as usize;
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->number requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type("string->number: expected string".into(), span)),
            }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("number->string requires 1 argument".into(), span)); }
            let n = as_integer(&args[0], span)?;
            Ok(Value::Str(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol->string requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type("symbol->string: expected symbol".into(), span)),
            }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->symbol requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type("string->symbol: expected string".into(), span)),
            }
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-copy requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type("string-copy: expected string".into(), span)),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ref requires 2 arguments".into(), span)); }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type("string-ref: expected string".into(), span)),
            };
            let idx = as_integer(&args[1], span)? as usize;
            match s.chars().nth(idx) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::Type("string-ref: index out of bounds".into(), span)),
            }
        }
        "char-alphabetic?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-alphabetic? requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(EvalError::Type("char-alphabetic?: expected char".into(), span)),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-numeric? requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(EvalError::Type("char-numeric?: expected char".into(), span)),
            }
        }
        "char-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-upcase requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => Err(EvalError::Type("char-upcase: expected char".into(), span)),
            }
        }
        "char-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-downcase requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                _ => Err(EvalError::Type("char-downcase: expected char".into(), span)),
            }
        }
        "char=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char=? requires 2 arguments".into(), span)); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type("char=?: expected chars".into(), span)),
            }
        }
        "char<?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char<? requires 2 arguments".into(), span)); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type("char<?: expected chars".into(), span)),
            }
        }
        "string=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string=? requires 2 arguments".into(), span)); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type("string=?: expected strings".into(), span)),
            }
        }
        "string<?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string<? requires 2 arguments".into(), span)); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type("string<?: expected strings".into(), span)),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ci=? requires 2 arguments".into(), span)); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
                _ => Err(EvalError::Type("string-ci=?: expected strings".into(), span)),
            }
        }
        "string-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-upcase requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
                _ => Err(EvalError::Type("string-upcase: expected string".into(), span)),
            }
        }
        "string-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-downcase requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
                _ => Err(EvalError::Type("string-downcase: expected string".into(), span)),
            }
        }
        _ => Err(EvalError::UnboundVariable(name.into(), span)),
    }
}

fn apply_builtin(name: &str, args: &[Value], out: &Output, span: Span) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" | "modulo" | "remainder"
        | "<" | ">" | "=" | "<=" | ">="
        | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
        | "abs" | "quotient" | "min" | "max" | "expt" => builtin_arithmetic(name, args, span),

        "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
        | "list?" | "list-ref" | "list-tail" | "assoc"
        | "map" | "for-each" => builtin_list(name, args, out, span),

        "display" | "write" | "newline"
        | "string-append" | "string-length" | "substring"
        | "string->number" | "number->string"
        | "symbol->string" | "string->symbol"
        | "string-copy" | "string-ref"
        | "char-alphabetic?" | "char-numeric?"
        | "char-upcase" | "char-downcase" | "char=?" | "char<?"
        | "string=?" | "string<?" | "string-ci=?"
        | "string-upcase" | "string-downcase" => builtin_string_char_io(name, args, out, span),

        "number?" => {
            if args.len() != 1 { return Err(EvalError::Arity("number? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "string?" => {
            if args.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "pair?" => {
            if args.len() != 1 { return Err(EvalError::Arity("pair? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty()) || matches!(&args[0], Value::Pair(..))))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "char?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("apply requires at least 2 arguments".into(), span));
            }
            let func = &args[0];
            let last = &args[args.len() - 1];
            let tail = match last {
                Value::List(items) => items.iter().map(|s| s.val.clone()).collect::<Vec<_>>(),
                _ => return Err(EvalError::Type("apply: last argument must be a list".into(), span)),
            };
            let mut all_args: Vec<Value> = args[1..args.len()-1].to_vec();
            all_args.extend(tail);
            apply(func, &all_args, out, span)
        }
        "equal?" => {
            if args.len() != 2 { return Err(EvalError::Arity("equal? requires 2 arguments".into(), span)); }
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        "eq?" | "eqv?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("{name} requires 2 arguments"), span)); }
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        _ => Err(EvalError::UnboundVariable(name.into(), span)),
    }
}

fn as_integer(val: &Value, span: Span) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected integer, got {:?}", val), span)),
    }
}

fn compare_nums(args: &[Value], cmp: impl Fn(i64, i64) -> bool, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into(), span));
    }
    let mut prev = as_integer(&args[0], span)?;
    for a in &args[1..] {
        let curr = as_integer(a, span)?;
        if !cmp(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::List(a), Value::List(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal(&x.val, &y.val))
        }
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => values_equal(a1, b1) && values_equal(a2, b2),
        _ => false,
    }
}

fn make_global_env() -> Env {
    let env = new_env(None);
    for name in &["+", "-", "*", "/", "<", ">", "=", "<=", ">=",
                   "cons", "car", "cdr", "null?", "list", "length", "append",
                   "number?", "boolean?", "string?", "pair?", "symbol?",
                   "modulo", "remainder", "quotient",
                   "display", "write", "newline",
                   "string-append", "string-length", "substring",
                   "string->number", "number->string",
                   "symbol->string", "string->symbol",
                   "string-ref", "string-copy", "char?",
                   "apply",
                   "equal?", "eq?", "eqv?",
                   // L09
                   "zero?", "positive?", "negative?", "odd?", "even?",
                   "abs", "min", "max", "expt",
                   "list?", "list-ref", "list-tail", "assoc",
                   "map", "for-each",
                   "char-alphabetic?", "char-numeric?",
                   "char-upcase", "char-downcase", "char=?", "char<?",
                   "string=?", "string<?", "string-ci=?",
                   "string-upcase", "string-downcase"] {
        env_set(&env, name.to_string(), Value::Symbol(name.to_string()));
    }
    env
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into(), Span { line: 1, col: 1 }));
    }
    let env = make_global_env();
    let out = Rc::new(RefCell::new(String::new()));
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env, &out)?;
    }
    Ok(last.display_value())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into(), Span { line: 1, col: 1 }));
    }
    let env = make_global_env();
    let out = Rc::new(RefCell::new(String::new()));
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env, &out)?;
    }
    let output = out.borrow().clone();
    Ok((last.display_value(), output))
}

#[cfg(test)]
mod tests;
