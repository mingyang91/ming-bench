pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// ---- AST (parsed code with source positions) ----

#[derive(Debug, Clone)]
struct Ast {
    kind: AstKind,
    line: usize,
    col: usize,
}

#[derive(Debug, Clone)]
enum AstKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Ast>),
}

// ---- Runtime values ----

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
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
    Builtin(fn(&[Value], &mut String) -> Result<Value, EvalError>),
    Void,
}

type Env = Rc<RefCell<Environment>>;

#[derive(Debug)]
struct Environment {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl Environment {
    fn new() -> Env {
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

    fn set(&mut self, name: String, val: Value) {
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
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!("expected number, got {}", self.display_value()))),
        }
    }

    fn display_value(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => format!("\"{s}\""),
            Value::Char(c) => format!("#\\{c}"),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_value()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda { .. } | Value::Builtin(_) => "#<procedure>".into(),
            Value::Void => "".into(),
        }
    }

    /// Format for `display` — no quotes on strings, no #\ on chars.
    fn display_repr(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_repr()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Integer(_)
            | Value::Boolean(_)
            | Value::Symbol(_)
            | Value::Lambda { .. }
            | Value::Builtin(_)
            | Value::Void => self.display_value(),
        }
    }
}

/// Convert an AST node to a runtime Value (for quote).
fn ast_to_value(ast: &Ast) -> Value {
    match &ast.kind {
        AstKind::Integer(n) => Value::Integer(*n),
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
                    _ => {}
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
        AstKind::Integer(_) | AstKind::Boolean(_) | AstKind::Str(_) | AstKind::Char(_) => {
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
        | Value::Boolean(_)
        | Value::Str(_)
        | Value::Char(_)
        | Value::List(_)
        | Value::Symbol(_)
        | Value::Void => Err(EvalError::Type(format!("not a procedure: {}", func.display_value()))),
    }
}

// ---- Builtins ----

fn args_to_ints(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|a| a.as_integer()).collect()
}

fn builtin_add(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let mut sum: i64 = 0;
    for a in args { sum += a.as_integer()?; }
    Ok(Value::Integer(sum))
}

fn builtin_sub(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("- requires at least 1 argument".into())); }
    if args.len() == 1 { return Ok(Value::Integer(-args[0].as_integer()?)); }
    let mut r = args[0].as_integer()?;
    for a in &args[1..] { r -= a.as_integer()?; }
    Ok(Value::Integer(r))
}

fn builtin_mul(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let mut p: i64 = 1;
    for a in args { p *= a.as_integer()?; }
    Ok(Value::Integer(p))
}

fn builtin_div(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("/ requires at least 1 argument".into())); }
    let mut r = args[0].as_integer()?;
    for a in &args[1..] {
        let d = a.as_integer()?;
        if d == 0 { return Err(EvalError::DivisionByZero); }
        r /= d;
    }
    Ok(Value::Integer(r))
}

fn builtin_lt(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let v = args_to_ints(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] < w[1])))
}
fn builtin_gt(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let v = args_to_ints(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] > w[1])))
}
fn builtin_eq(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let v = args_to_ints(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] == w[1])))
}
fn builtin_le(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let v = args_to_ints(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] <= w[1])))
}
fn builtin_ge(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let v = args_to_ints(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] >= w[1])))
}
fn builtin_cons(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("cons requires exactly 2 arguments".into())); }
    match &args[1] {
        Value::List(tail) => {
            let mut new = vec![args[0].clone()];
            new.extend(tail.iter().cloned());
            Ok(Value::List(new))
        }
        _ => Err(EvalError::Type("cons: second argument must be a list".into())),
    }
}

fn builtin_car(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("car requires exactly 1 argument".into())); }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        _ => Err(EvalError::Type("car: expected non-empty list".into())),
    }
}

fn builtin_cdr(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("cdr requires exactly 1 argument".into())); }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        _ => Err(EvalError::Type("cdr: expected non-empty list".into())),
    }
}

fn builtin_list(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn builtin_null(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("null? requires exactly 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
}

fn builtin_length(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("length requires exactly 1 argument".into())); }
    match &args[0] {
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        _ => Err(EvalError::Type("length: expected list".into())),
    }
}

fn builtin_append(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for a in args {
        match a {
            Value::List(items) => result.extend(items.iter().cloned()),
            _ => return Err(EvalError::Type("append: expected list".into())),
        }
    }
    Ok(Value::List(result))
}

fn builtin_display(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("display requires 1 argument".into())); }
    output.push_str(&args[0].display_repr());
    Ok(Value::Void)
}

fn builtin_write(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("write requires 1 argument".into())); }
    output.push_str(&args[0].display_value());
    Ok(Value::Void)
}

fn builtin_newline(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    if !args.is_empty() { return Err(EvalError::Arity("newline takes no arguments".into())); }
    output.push('\n');
    Ok(Value::Void)
}

fn builtin_string_append(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let mut result = String::new();
    for a in args {
        match a {
            Value::Str(s) => result.push_str(s),
            _ => return Err(EvalError::Type("string-append: expected string".into())),
        }
    }
    Ok(Value::Str(result))
}

fn builtin_string_length(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-length requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
        _ => Err(EvalError::Type("string-length: expected string".into())),
    }
}

fn builtin_substring(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 3 { return Err(EvalError::Arity("substring requires 3 arguments".into())); }
    let s = match &args[0] {
        Value::Str(s) => s,
        _ => return Err(EvalError::Type("substring: expected string".into())),
    };
    let start = args[1].as_integer()? as usize;
    let end = args[2].as_integer()? as usize;
    Ok(Value::Str(s[start..end].to_string()))
}

fn builtin_string_to_number(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string->number requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => match s.parse::<i64>() {
            Ok(n) => Ok(Value::Integer(n)),
            Err(_) => Ok(Value::Boolean(false)),
        },
        _ => Err(EvalError::Type("string->number: expected string".into())),
    }
}

fn builtin_number_to_string(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("number->string requires 1 argument".into())); }
    Ok(Value::Str(args[0].as_integer()?.to_string()))
}

fn builtin_symbol_to_string(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("symbol->string requires 1 argument".into())); }
    match &args[0] {
        Value::Symbol(s) => Ok(Value::Str(s.clone())),
        _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
    }
}

fn builtin_string_to_symbol(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string->symbol requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Symbol(s.clone())),
        _ => Err(EvalError::Type("string->symbol: expected string".into())),
    }
}

fn builtin_string_ref(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string-ref requires 2 arguments".into())); }
    let s = match &args[0] {
        Value::Str(s) => s,
        _ => return Err(EvalError::Type("string-ref: expected string".into())),
    };
    let idx = args[1].as_integer()? as usize;
    Ok(Value::Char(s.chars().nth(idx).ok_or_else(|| {
        EvalError::Type("string-ref: index out of bounds".into())
    })?))
}

fn builtin_string_copy(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-copy requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.clone())),
        _ => Err(EvalError::Type("string-copy: expected string".into())),
    }
}

fn builtin_apply(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("apply requires at least 2 arguments".into()));
    }
    let func = &args[0];
    let last = &args[args.len() - 1];
    let tail = match last {
        Value::List(items) => items.clone(),
        _ => return Err(EvalError::Type("apply: last argument must be a list".into())),
    };
    let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    all_args.extend(tail);
    apply(func, &all_args, output)
}

fn builtin_is_char(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Char(_))))
}

fn builtin_is_string(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Str(_))))
}
fn builtin_is_number(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("number? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
}
fn builtin_is_boolean(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
}
fn builtin_is_pair(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("pair? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty())))
}
fn builtin_is_symbol(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("symbol? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
}

fn builtin_not(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("not requires exactly 1 argument".into())); }
    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn make_global_env() -> Env {
    let env = Environment::new();
    {
        let mut e = env.borrow_mut();
        e.set("+".into(), Value::Builtin(builtin_add));
        e.set("-".into(), Value::Builtin(builtin_sub));
        e.set("*".into(), Value::Builtin(builtin_mul));
        e.set("/".into(), Value::Builtin(builtin_div));
        e.set("<".into(), Value::Builtin(builtin_lt));
        e.set(">".into(), Value::Builtin(builtin_gt));
        e.set("=".into(), Value::Builtin(builtin_eq));
        e.set("<=".into(), Value::Builtin(builtin_le));
        e.set(">=".into(), Value::Builtin(builtin_ge));
        e.set("not".into(), Value::Builtin(builtin_not));
        e.set("cons".into(), Value::Builtin(builtin_cons));
        e.set("car".into(), Value::Builtin(builtin_car));
        e.set("cdr".into(), Value::Builtin(builtin_cdr));
        e.set("list".into(), Value::Builtin(builtin_list));
        e.set("null?".into(), Value::Builtin(builtin_null));
        e.set("length".into(), Value::Builtin(builtin_length));
        e.set("append".into(), Value::Builtin(builtin_append));
        e.set("string?".into(), Value::Builtin(builtin_is_string));
        e.set("number?".into(), Value::Builtin(builtin_is_number));
        e.set("boolean?".into(), Value::Builtin(builtin_is_boolean));
        e.set("pair?".into(), Value::Builtin(builtin_is_pair));
        e.set("symbol?".into(), Value::Builtin(builtin_is_symbol));
        e.set("char?".into(), Value::Builtin(builtin_is_char));
        e.set("display".into(), Value::Builtin(builtin_display));
        e.set("write".into(), Value::Builtin(builtin_write));
        e.set("newline".into(), Value::Builtin(builtin_newline));
        e.set("string-append".into(), Value::Builtin(builtin_string_append));
        e.set("string-length".into(), Value::Builtin(builtin_string_length));
        e.set("substring".into(), Value::Builtin(builtin_substring));
        e.set("string->number".into(), Value::Builtin(builtin_string_to_number));
        e.set("number->string".into(), Value::Builtin(builtin_number_to_string));
        e.set("symbol->string".into(), Value::Builtin(builtin_symbol_to_string));
        e.set("string->symbol".into(), Value::Builtin(builtin_string_to_symbol));
        e.set("string-ref".into(), Value::Builtin(builtin_string_ref));
        e.set("string-copy".into(), Value::Builtin(builtin_string_copy));
        e.set("apply".into(), Value::Builtin(builtin_apply));
    }
    env
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let env = make_global_env();
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
    let env = make_global_env();
    let mut output = String::new();
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env, &mut output)?;
    }
    Ok((last.display_value(), output))
}

#[cfg(test)]
mod tests;
