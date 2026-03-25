pub mod error;
mod builtins;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

// ---- AST (parsed code with source positions) ----

#[derive(Debug, Clone)]
pub(crate) struct Ast {
    kind: AstKind,
    line: usize,
    col: usize,
}

#[derive(Debug, Clone)]
enum AstKind {
    Integer(i64),
    Rational(i64, i64),
    Float(f64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Ast>),
}

// ---- Runtime values ----

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Integer(i64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Float(f64),
    Boolean(bool),
    Str(String),
    Char(char),
    List(Vec<Value>),
    Symbol(String),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Ast>,
        env: Env,
    },
    Pair(Box<Value>, Box<Value>),
    Builtin(fn(&[Value], &mut String) -> Result<Value, EvalError>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Ast, Ast)>,
        def_env: Env,
    },
    Void,
}

fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Create a rational or integer value, always simplified.
pub(crate) fn make_rational(n: i64, d: i64) -> Value {
    assert!(d != 0, "division by zero in make_rational");
    let sign = if (n < 0) ^ (d < 0) { -1 } else { 1 };
    let n = n.abs();
    let d = d.abs();
    let g = gcd(n, d);
    let n = sign * (n / g);
    let d = d / g;
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

pub(crate) type Env = Rc<RefCell<Environment>>;

#[derive(Debug)]
pub(crate) struct Environment {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl Environment {
    pub(crate) fn new() -> Env {
        Rc::new(RefCell::new(Environment {
            bindings: HashMap::new(),
            parent: None,
        }))
    }

    fn with_parent(parent: &Env) -> Env {
        Rc::new(RefCell::new(Environment {
            bindings: HashMap::new(),
            parent: Some(Rc::clone(parent)),
        }))
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    pub(crate) fn set(&mut self, name: String, val: Value) {
        self.bindings.insert(name, val);
    }

    fn set_existing(&mut self, name: &str, val: Value) -> bool {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_string(), val);
            true
        } else if let Some(ref parent) = self.parent {
            parent.borrow_mut().set_existing(name, val)
        } else {
            false
        }
    }
}

impl Value {
    pub(crate) fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    pub(crate) fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!("expected number, got {}", self.display_value()))),
        }
    }

    /// Convert any numeric value to f64 for comparison.
    pub(crate) fn as_f64(&self) -> Result<f64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n as f64),
            Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
            Value::Float(f) => Ok(*f),
            _ => Err(EvalError::Type(format!("expected number, got {}", self.display_value()))),
        }
    }

    pub(crate) fn is_number(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Rational(_, _) | Value::Float(_))
    }

    pub(crate) fn is_exact(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Rational(_, _))
    }

    pub(crate) fn display_value(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Float(f) => {
                if f.fract() == 0.0 && f.is_finite() {
                    format!("{:.1}", f)
                } else {
                    format!("{}", f)
                }
            }
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => format!("\"{s}\""),
            Value::Char(c) => match c {
                ' ' => "#\\space".into(),
                '\n' => "#\\newline".into(),
                '\t' => "#\\tab".into(),
                _ => format!("#\\{c}"),
            },
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_value()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(a, b) => format!("({} . {})", a.display_value(), b.display_value()),
            Value::Lambda { .. } | Value::Builtin(_) => "#<procedure>".into(),
            Value::Macro { .. } => "#<macro>".into(),
            Value::Void => "".into(),
        }
    }

    /// Format for `display` — no quotes on strings, no #\ on chars.
    pub(crate) fn display_repr(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_repr()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(a, b) => format!("({} . {})", a.display_repr(), b.display_repr()),
            Value::Integer(_)
            | Value::Rational(_, _)
            | Value::Float(_)
            | Value::Boolean(_)
            | Value::Symbol(_)
            | Value::Lambda { .. }
            | Value::Builtin(_)
            | Value::Macro { .. }
            | Value::Void => self.display_value(),
        }
    }
}

/// Convert an AST node to a runtime Value (for quote).
fn ast_to_value(ast: &Ast) -> Value {
    match &ast.kind {
        AstKind::Integer(n) => Value::Integer(*n),
        AstKind::Rational(n, d) => Value::Rational(*n, *d),
        AstKind::Float(f) => Value::Float(*f),
        AstKind::Boolean(b) => Value::Boolean(*b),
        AstKind::Str(s) => Value::Str(s.clone()),
        AstKind::Char(c) => Value::Char(*c),
        AstKind::Symbol(s) => Value::Symbol(s.clone()),
        AstKind::List(items) => Value::List(items.iter().map(ast_to_value).collect()),
    }
}

// ---- Parser ----

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

    fn skip_whitespace(&mut self) {
        while self.pos < self.chars.len() {
            if self.chars[self.pos].is_whitespace() {
                self.next_char();
            } else if self.chars[self.pos] == ';' {
                while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
                    self.next_char();
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.chars.get(self.pos).copied();
        if let Some(c) = ch {
            self.pos += 1;
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
        ch
    }

    fn parse_expr(&mut self) -> Result<Ast, EvalError> {
        self.skip_whitespace();
        let line = self.line;
        let col = self.col;
        match self.peek() {
            None => Err(EvalError::Parse("unexpected end of input".into())),
            Some('\'') => {
                self.next_char(); // consume quote
                let expr = self.parse_expr()?;
                Ok(Ast {
                    kind: AstKind::List(vec![
                        Ast { kind: AstKind::Symbol("quote".into()), line, col },
                        expr,
                    ]),
                    line,
                    col,
                })
            }
            Some('(') => self.parse_list(line, col),
            Some('"') => self.parse_string(line, col),
            Some('#') => self.parse_hash(line, col),
            _ => self.parse_atom(line, col),
        }
    }

    fn parse_list(&mut self, line: usize, col: usize) -> Result<Ast, EvalError> {
        self.next_char(); // consume '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace();
            match self.peek() {
                None => return Err(EvalError::Parse("unclosed parenthesis".into())),
                Some(')') => {
                    self.next_char();
                    return Ok(Ast { kind: AstKind::List(items), line, col });
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self, line: usize, col: usize) -> Result<Ast, EvalError> {
        self.next_char(); // consume opening '"'
        let mut s = String::new();
        loop {
            match self.next_char() {
                None => return Err(EvalError::Parse("unclosed string".into())),
                Some('"') => return Ok(Ast { kind: AstKind::Str(s), line, col }),
                Some('\\') => match self.next_char() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('\\') => s.push('\\'),
                    Some('"') => s.push('"'),
                    Some(c) => s.push(c),
                    None => return Err(EvalError::Parse("unclosed string escape".into())),
                },
                Some(c) => s.push(c),
            }
        }
    }

    fn parse_hash(&mut self, line: usize, col: usize) -> Result<Ast, EvalError> {
        self.next_char(); // consume '#'
        match self.next_char() {
            Some('t') => {
                if self.peek().is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-' && c != '!' && c != '?') {
                    Ok(Ast { kind: AstKind::Boolean(true), line, col })
                } else {
                    Err(EvalError::Parse("invalid boolean literal".into()))
                }
            }
            Some('f') => {
                if self.peek().is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-' && c != '!' && c != '?') {
                    Ok(Ast { kind: AstKind::Boolean(false), line, col })
                } else {
                    Err(EvalError::Parse("invalid boolean literal".into()))
                }
            }
            Some('\\') => {
                // Character literal: #\x, #\space, #\newline, etc.
                match self.next_char() {
                    None => Err(EvalError::Parse("unexpected end of character literal".into())),
                    Some(c) => {
                        // Check for named characters
                        let mut name = String::new();
                        name.push(c);
                        while let Some(nc) = self.peek() {
                            if nc.is_alphabetic() {
                                name.push(nc);
                                self.next_char();
                            } else {
                                break;
                            }
                        }
                        let ch = if name.len() == 1 {
                            name.chars().next().expect("single-char name is non-empty")
                        } else {
                            match name.as_str() {
                                "space" => ' ',
                                "newline" => '\n',
                                "tab" => '\t',
                                _ => return Err(EvalError::Parse(format!("unknown character name: {}", name))),
                            }
                        };
                        Ok(Ast { kind: AstKind::Char(ch), line, col })
                    }
                }
            }
            _ => Err(EvalError::Parse("invalid hash literal".into())),
        }
    }

    fn parse_atom(&mut self, line: usize, col: usize) -> Result<Ast, EvalError> {
        let mut token = String::new();
        while let Some(c) = self.peek() {
            if c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == ';' {
                break;
            }
            token.push(c);
            self.next_char();
        }
        if token.is_empty() {
            return Err(EvalError::Parse("unexpected character".into()));
        }
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Ast { kind: AstKind::Integer(n), line, col });
        }
        // Rational literal: digits/digits (e.g. 1/3, -5/2)
        if let Some(slash) = token.find('/') {
            if let (Ok(n), Ok(d)) = (token[..slash].parse::<i64>(), token[slash+1..].parse::<i64>()) {
                if d != 0 {
                    let sign = if (n < 0) ^ (d < 0) { -1 } else { 1 };
                    let na = n.abs();
                    let da = d.abs();
                    let g = gcd(na, da);
                    let n2 = sign * (na / g);
                    let d2 = da / g;
                    if d2 == 1 {
                        return Ok(Ast { kind: AstKind::Integer(n2), line, col });
                    }
                    return Ok(Ast { kind: AstKind::Rational(n2, d2), line, col });
                }
            }
        }
        // Float literal
        if let Ok(f) = token.parse::<f64>() {
            return Ok(Ast { kind: AstKind::Float(f), line, col });
        }
        Ok(Ast { kind: AstKind::Symbol(token), line, col })
    }

    fn parse_all(&mut self) -> Result<Vec<Ast>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace();
            if self.pos >= self.chars.len() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        if exprs.is_empty() {
            return Err(EvalError::Parse("empty input".into()));
        }
        Ok(exprs)
    }
}

// ---- Evaluator ----

/// Evaluate an AST node, wrapping any error with source position.
fn eval(ast: &Ast, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    eval_inner(ast, env, output).map_err(|e| match e {
        EvalError::WithPosition(_, _, _) => e,
        _ => EvalError::WithPosition(Box::new(e), ast.line, ast.col),
    })
}

fn eval_inner(ast: &Ast, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    match &ast.kind {
        AstKind::Integer(n) => Ok(Value::Integer(*n)),
        AstKind::Rational(n, d) => Ok(Value::Rational(*n, *d)),
        AstKind::Float(f) => Ok(Value::Float(*f)),
        AstKind::Boolean(b) => Ok(Value::Boolean(*b)),
        AstKind::Str(s) => Ok(Value::Str(s.clone())),
        AstKind::Char(c) => Ok(Value::Char(*c)),
        AstKind::Symbol(name) => {
            env.borrow().get(name).ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        AstKind::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            // Check for special forms
            if let AstKind::Symbol(op) = &items[0].kind {
                match op.as_str() {
                    "define" => return eval_define(&items[1..], env, output),
                    "if" => return eval_if(&items[1..], env, output),
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("quote requires exactly 1 argument".into()));
                        }
                        return Ok(ast_to_value(&items[1]));
                    }
                    "lambda" => return eval_lambda(&items[1..], env),
                    "let" => return eval_let(&items[1..], env, output),
                    "begin" => {
                        let mut result = Value::Void;
                        for expr in &items[1..] {
                            result = eval(expr, env, output)?;
                        }
                        return Ok(result);
                    }
                    "cond" => return eval_cond(&items[1..], env, output),
                    "and" => {
                        if items.len() == 1 {
                            return Ok(Value::Boolean(true));
                        }
                        let mut result = Value::Boolean(true);
                        for a in &items[1..] {
                            result = eval(a, env, output)?;
                            if !result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "set!" => {
                        if items.len() != 3 {
                            return Err(EvalError::Arity("set! requires exactly 2 arguments".into()));
                        }
                        let var_name = match &items[1].kind {
                            AstKind::Symbol(name) => name.clone(),
                            _ => return Err(EvalError::Type("set!: first argument must be a symbol".into())),
                        };
                        let val = eval(&items[2], env, output)?;
                        if !env.borrow_mut().set_existing(&var_name, val) {
                            return Err(EvalError::UnboundVariable(var_name));
                        }
                        return Ok(Value::Void);
                    }
                    "string-set!" => {
                        if items.len() != 4 {
                            return Err(EvalError::Arity("string-set! requires 3 arguments".into()));
                        }
                        let var_name = match &items[1].kind {
                            AstKind::Symbol(name) => name.clone(),
                            _ => return Err(EvalError::Type("string-set!: first argument must be a variable".into())),
                        };
                        let idx = eval(&items[2], env, output)?.as_integer()? as usize;
                        let ch = match eval(&items[3], env, output)? {
                            Value::Char(c) => c,
                            _ => return Err(EvalError::Type("string-set!: third argument must be a char".into())),
                        };
                        let s = env.borrow().get(&var_name)
                            .ok_or_else(|| EvalError::UnboundVariable(var_name.clone()))?;
                        match s {
                            Value::Str(st) => {
                                let mut chars: Vec<char> = st.chars().collect();
                                if idx >= chars.len() {
                                    return Err(EvalError::Type("string-set!: index out of bounds".into()));
                                }
                                chars[idx] = ch;
                                let new_str: String = chars.into_iter().collect();
                                env.borrow_mut().set_existing(&var_name, Value::Str(new_str));
                                return Ok(Value::Void);
                            }
                            _ => return Err(EvalError::Type("string-set!: first argument must be a string".into())),
                        }
                    }
                    "or" => {
                        if items.len() == 1 {
                            return Ok(Value::Boolean(false));
                        }
                        let mut result = Value::Boolean(false);
                        for a in &items[1..] {
                            result = eval(a, env, output)?;
                            if result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "define-syntax" => return eval_define_syntax(&items[1..], env),
                    _ => {
                        // Check for macro invocation
                        let maybe_macro = env.borrow().get(op);
                        if let Some(Value::Macro { literals, rules, def_env }) = maybe_macro {
                            return expand_and_eval_macro(&literals, &rules, &def_env, items, env, output);
                        }
                    }
                }
            }
            // Function application
            let func = eval(&items[0], env, output)?;
            let args: Vec<Value> = items[1..].iter().map(|a| eval(a, env, output)).collect::<Result<_, _>>()?;
            apply(&func, &args, output)
        }
    }
}

fn eval_define(args: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires at least 2 arguments".into()));
    }
    match &args[0].kind {
        // (define x expr)
        AstKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires exactly 2 arguments".into()));
            }
            let val = eval(&args[1], env, output)?;
            env.borrow_mut().set(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...)
        AstKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0].kind {
                AstKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: expected symbol for function name".into())),
            };
            let (params, rest_param) = parse_params(&sig[1..])?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                env: Rc::clone(env),
            };
            env.borrow_mut().set(name, lambda);
            Ok(Value::Void)
        }
        AstKind::Integer(_) | AstKind::Rational(_, _) | AstKind::Float(_) | AstKind::Boolean(_) | AstKind::Str(_) | AstKind::Char(_) => {
            Err(EvalError::Type("define: expected symbol or list".into()))
        }
    }
}

fn eval_if(args: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env, output)?;
    if cond.is_truthy() {
        eval(&args[1], env, output)
    } else if args.len() == 3 {
        eval(&args[2], env, output)
    } else {
        Ok(Value::Void)
    }
}

fn eval_lambda(args: &[Ast], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires at least 2 arguments".into()));
    }
    let (params, rest_param) = match &args[0].kind {
        AstKind::List(ps) => parse_params(ps)?,
        AstKind::Symbol(s) => (vec![], Some(s.clone())),
        _ => return Err(EvalError::Type("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        env: Rc::clone(env),
    })
}

/// Parse a parameter list, handling optional dot notation for rest params.
/// E.g. `[x, y, ., rest]` -> `(["x", "y"], Some("rest"))`
fn parse_params(items: &[Ast]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < items.len() {
        match &items[i].kind {
            AstKind::Symbol(s) if s == "." => {
                if i + 1 != items.len() - 1 {
                    return Err(EvalError::Parse("malformed dotted parameter list".into()));
                }
                rest_param = Some(match &items[i + 1].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("expected symbol after dot in parameters".into())),
                });
                break;
            }
            AstKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(EvalError::Type("expected symbol for parameter".into())),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_let(args: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into()));
    }
    // Named let: (let name ((var init) ...) body...)
    if let AstKind::Symbol(name) = &args[0].kind {
        if args.len() < 3 {
            return Err(EvalError::Arity("named let requires bindings and body".into()));
        }
        let bindings_list = match &args[1].kind {
            AstKind::List(b) => b,
            _ => return Err(EvalError::Type("let: expected bindings list".into())),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for binding in bindings_list {
            match &binding.kind {
                AstKind::List(pair) if pair.len() == 2 => {
                    match &pair[0].kind {
                        AstKind::Symbol(s) => params.push(s.clone()),
                        _ => return Err(EvalError::Type("let: expected symbol in binding".into())),
                    }
                    inits.push(eval(&pair[1], env, output)?);
                }
                _ => return Err(EvalError::Type("let: invalid binding".into())),
            }
        }
        let body = args[2..].to_vec();
        let local_env = Environment::with_parent(env);
        let lambda = Value::Lambda {
            params: params.clone(),
            rest_param: None,
            body: body.clone(),
            env: Rc::clone(&local_env),
        };
        local_env.borrow_mut().set(name.clone(), lambda);
        for (p, v) in params.iter().zip(inits.iter()) {
            local_env.borrow_mut().set(p.clone(), v.clone());
        }
        let mut result = Value::Void;
        for expr in &body {
            result = eval(expr, &local_env, output)?;
        }
        return Ok(result);
    }
    // Regular let: (let ((var init) ...) body...)
    let bindings_list = match &args[0].kind {
        AstKind::List(b) => b,
        _ => return Err(EvalError::Type("let: expected bindings list".into())),
    };
    let local_env = Environment::with_parent(env);
    for binding in bindings_list {
        match &binding.kind {
            AstKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("let: expected symbol in binding".into())),
                };
                let val = eval(&pair[1], env, output)?;
                local_env.borrow_mut().set(name, val);
            }
            _ => return Err(EvalError::Type("let: invalid binding".into())),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, output)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    for clause in clauses {
        match &clause.kind {
            AstKind::List(items) if items.len() >= 2 => {
                if let AstKind::Symbol(s) = &items[0].kind {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &items[1..] {
                            result = eval(expr, env, output)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&items[0], env, output)?;
                if test.is_truthy() {
                    let mut result = Value::Void;
                    for expr in &items[1..] {
                        result = eval(expr, env, output)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Type("cond: invalid clause".into())),
        }
    }
    Ok(Value::Void)
}

// ---- Macro Support (L10) ----

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}__macro_{}", base, n)
}

fn is_special_form(name: &str) -> bool {
    matches!(
        name,
        "define" | "if" | "quote" | "lambda" | "let" | "begin"
            | "cond" | "and" | "or" | "set!" | "string-set!"
            | "define-syntax" | "syntax-rules"
    )
}

#[derive(Debug, Clone)]
enum PatternBinding {
    Single(Ast),
    Ellipsis(Vec<Ast>),
}

fn match_pattern(
    pattern: &[Ast],
    form: &[Ast],
    literals: &[String],
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    let mut pi = 0;
    let mut fi = 0;

    while pi < pattern.len() {
        if matches!(&pattern[pi].kind, AstKind::Symbol(s) if s == "...") {
            pi += 1;
            continue;
        }

        let is_ellipsis = pi + 1 < pattern.len()
            && matches!(&pattern[pi + 1].kind, AstKind::Symbol(s) if s == "...");

        if is_ellipsis {
            match &pattern[pi].kind {
                AstKind::Symbol(name) if !literals.contains(name) && name != "_" => {
                    let remaining = count_non_ellipsis_remaining(&pattern[pi + 2..]);
                    let available = if form.len() >= fi + remaining {
                        form.len() - fi - remaining
                    } else {
                        return false;
                    };
                    bindings.insert(
                        name.clone(),
                        PatternBinding::Ellipsis(form[fi..fi + available].to_vec()),
                    );
                    fi += available;
                    pi += 2;
                }
                _ => return false,
            }
        } else {
            if fi >= form.len() {
                return false;
            }
            match &pattern[pi].kind {
                AstKind::Symbol(name) if literals.contains(name) => {
                    if !matches!(&form[fi].kind, AstKind::Symbol(s) if s == name) {
                        return false;
                    }
                }
                AstKind::Symbol(name) if name == "_" => {}
                AstKind::Symbol(name) => {
                    bindings.insert(name.clone(), PatternBinding::Single(form[fi].clone()));
                }
                AstKind::List(sub_pat) => {
                    if let AstKind::List(sub_form) = &form[fi].kind {
                        if !match_pattern(sub_pat, sub_form, literals, bindings) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                _ => return false,
            }
            pi += 1;
            fi += 1;
        }
    }

    fi == form.len()
}

fn count_non_ellipsis_remaining(pattern: &[Ast]) -> usize {
    let mut count = 0;
    let mut i = 0;
    while i < pattern.len() {
        if matches!(&pattern[i].kind, AstKind::Symbol(s) if s == "...") {
            i += 1;
            continue;
        }
        let is_ellipsis = i + 1 < pattern.len()
            && matches!(&pattern[i + 1].kind, AstKind::Symbol(s) if s == "...");
        if is_ellipsis {
            i += 2;
        } else {
            count += 1;
            i += 1;
        }
    }
    count
}

fn expand_ellipsis_element(
    element: &Ast,
    bindings: &HashMap<String, PatternBinding>,
    renames: &mut HashMap<String, String>,
    expanded: &mut Vec<Ast>,
) {
    let var_name = match find_ellipsis_var(element, bindings) {
        Some(name) => name,
        None => return,
    };
    let values = match bindings.get(&var_name) {
        Some(PatternBinding::Ellipsis(vals)) => vals,
        _ => return,
    };
    for val in values {
        let mut local_bindings = bindings.clone();
        local_bindings.insert(var_name.clone(), PatternBinding::Single(val.clone()));
        expanded.push(expand_template(element, &local_bindings, renames));
    }
}

fn expand_template(
    template: &Ast,
    bindings: &HashMap<String, PatternBinding>,
    renames: &mut HashMap<String, String>,
) -> Ast {
    match &template.kind {
        AstKind::Symbol(name) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    PatternBinding::Single(ast) => return ast.clone(),
                    PatternBinding::Ellipsis(_) => return template.clone(),
                }
            }
            if is_special_form(name) || name == "..." {
                template.clone()
            } else {
                let gensym_name = renames
                    .entry(name.clone())
                    .or_insert_with(|| gensym(name))
                    .clone();
                Ast {
                    kind: AstKind::Symbol(gensym_name),
                    line: template.line,
                    col: template.col,
                }
            }
        }
        AstKind::List(elements) => {
            let mut expanded = Vec::new();
            let mut i = 0;
            while i < elements.len() {
                if matches!(&elements[i].kind, AstKind::Symbol(s) if s == "...") {
                    i += 1;
                    continue;
                }
                let is_ellipsis = i + 1 < elements.len()
                    && matches!(&elements[i + 1].kind, AstKind::Symbol(s) if s == "...");

                if is_ellipsis {
                    expand_ellipsis_element(&elements[i], bindings, renames, &mut expanded);
                    i += 2;
                } else {
                    expanded.push(expand_template(&elements[i], bindings, renames));
                    i += 1;
                }
            }
            Ast {
                kind: AstKind::List(expanded),
                line: template.line,
                col: template.col,
            }
        }
        _ => template.clone(),
    }
}

fn find_ellipsis_var(
    template: &Ast,
    bindings: &HashMap<String, PatternBinding>,
) -> Option<String> {
    match &template.kind {
        AstKind::Symbol(name) => {
            if matches!(bindings.get(name), Some(PatternBinding::Ellipsis(_))) {
                Some(name.clone())
            } else {
                None
            }
        }
        AstKind::List(elements) => {
            for elem in elements {
                if let Some(name) = find_ellipsis_var(elem, bindings) {
                    return Some(name);
                }
            }
            None
        }
        _ => None,
    }
}

fn expand_and_eval_macro(
    literals: &[String],
    rules: &[(Ast, Ast)],
    def_env: &Env,
    form: &[Ast],
    use_env: &Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    let form_args = &form[1..];

    for (pattern, template) in rules {
        let pattern_args = match &pattern.kind {
            AstKind::List(items) if !items.is_empty() => &items[1..],
            _ => continue,
        };

        let mut bindings = HashMap::new();
        if match_pattern(pattern_args, form_args, literals, &mut bindings) {
            let mut renames = HashMap::new();
            let expanded = expand_template(template, &bindings, &mut renames);

            if !renames.is_empty() {
                let child_env = Environment::with_parent(use_env);
                for (original, gensym_name) in &renames {
                    if let Some(val) = def_env.borrow().get(original) {
                        child_env.borrow_mut().set(gensym_name.clone(), val);
                    }
                }
                return eval(&expanded, &child_env, output);
            }

            return eval(&expanded, use_env, output);
        }
    }

    Err(EvalError::Type("no matching macro pattern".into()))
}

fn eval_define_syntax(args: &[Ast], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(
            "define-syntax requires 2 arguments".into(),
        ));
    }
    let name = match &args[0].kind {
        AstKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-syntax: expected symbol".into())),
    };
    let sr = match &args[1].kind {
        AstKind::List(items) => items,
        _ => {
            return Err(EvalError::Type(
                "define-syntax: expected syntax-rules".into(),
            ))
        }
    };
    if sr.is_empty() || !matches!(&sr[0].kind, AstKind::Symbol(s) if s == "syntax-rules") {
        return Err(EvalError::Type(
            "define-syntax: expected syntax-rules".into(),
        ));
    }
    if sr.len() < 2 {
        return Err(EvalError::Type(
            "syntax-rules: missing literals list".into(),
        ));
    }
    let literals = match &sr[1].kind {
        AstKind::List(lits) => lits
            .iter()
            .map(|l| match &l.kind {
                AstKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type(
                    "syntax-rules: expected symbol in literals".into(),
                )),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => {
            return Err(EvalError::Type(
                "syntax-rules: expected literals list".into(),
            ))
        }
    };
    let mut rules = Vec::new();
    for rule in &sr[2..] {
        match &rule.kind {
            AstKind::List(pair) if pair.len() == 2 => {
                rules.push((pair[0].clone(), pair[1].clone()));
            }
            _ => return Err(EvalError::Type("syntax-rules: invalid rule".into())),
        }
    }
    env.borrow_mut().set(
        name,
        Value::Macro {
            literals,
            rules,
            def_env: Rc::clone(env),
        },
    );
    Ok(Value::Void)
}

fn apply(func: &Value, args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(f) => f(args, output),
        Value::Lambda { params, rest_param, body, env } => {
            if rest_param.is_some() {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {}", params.len(), args.len()
                    )));
                }
            } else if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let local_env = Environment::with_parent(env);
            for (p, a) in params.iter().zip(args.iter()) {
                local_env.borrow_mut().set(p.clone(), a.clone());
            }
            if let Some(rest) = rest_param {
                let rest_args = args[params.len()..].to_vec();
                local_env.borrow_mut().set(rest.clone(), Value::List(rest_args));
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env, output)?;
            }
            Ok(result)
        }
        Value::Integer(_)
        | Value::Rational(_, _)
        | Value::Float(_)
        | Value::Boolean(_)
        | Value::Str(_)
        | Value::Char(_)
        | Value::List(_)
        | Value::Pair(_, _)
        | Value::Symbol(_)
        | Value::Macro { .. }
        | Value::Void => Err(EvalError::Type(format!("not a procedure: {}", func.display_value()))),
    }
}


/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let env = builtins::make_global_env();
    let mut output = String::new();
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env, &mut output)?;
    }
    Ok(last.display_value())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let env = builtins::make_global_env();
    let mut output = String::new();
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env, &mut output)?;
    }
    Ok((last.display_value(), output))
}

#[cfg(test)]
mod tests;
