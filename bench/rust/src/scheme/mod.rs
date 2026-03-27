pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

type Frame = Rc<RefCell<HashMap<String, Value>>>;
type Env = Vec<Frame>;

#[derive(Debug, Clone, Copy, Default)]
struct Span {
    line: usize,
    col: usize,
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Char(char),
    Str(Rc<RefCell<String>>),
    Symbol(String),
    List(Vec<Value>),
    Procedure(Vec<String>, Vec<Expr>, Env),
}

fn make_str(s: String) -> Value {
    Value::Str(Rc::new(RefCell::new(s)))
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Str(s) => write!(f, "\"{}\"", s.borrow()),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
            Value::Procedure(..) => write!(f, "#<procedure>"),
        }
    }
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
    Char(char),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

// ── Parser ──

struct Parser<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input: input.as_bytes(), pos: 0 }
    }

    fn current_span(&self) -> Span {
        let mut line = 1;
        let mut col = 1;
        for &b in &self.input[..self.pos] {
            if b == b'\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        Span { line, col }
    }

    fn skip_whitespace_and_comments(&mut self) {
        while self.pos < self.input.len() {
            let b = self.input[self.pos];
            if b.is_ascii_whitespace() {
                self.pos += 1;
            } else if b == b';' {
                while self.pos < self.input.len() && self.input[self.pos] != b'\n' {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.pos < self.input.len() { Some(self.input[self.pos]) } else { None }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace_and_comments();
        let span = self.current_span();
        match self.peek() {
            None => Err(EvalError::Parse("unexpected end of input".into())),
            Some(b'\'') => {
                self.pos += 1;
                let inner = self.parse_expr()?;
                Ok(Expr { kind: ExprKind::List(vec![
                    Expr { kind: ExprKind::Symbol("quote".into()), span },
                    inner,
                ]), span })
            }
            Some(b'(') => self.parse_list(span),
            Some(b'"') => self.parse_string(span),
            Some(b'#') => self.parse_hash(span),
            _ => self.parse_atom(span),
        }
    }

    fn parse_list(&mut self, span: Span) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            match self.peek() {
                None => return Err(EvalError::Parse("unterminated list".into())),
                Some(b')') => { self.pos += 1; return Ok(Expr { kind: ExprKind::List(items), span }); }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self, span: Span) -> Result<Expr, EvalError> {
        self.pos += 1; // skip opening "
        let mut s = String::new();
        loop {
            if self.pos >= self.input.len() {
                return Err(EvalError::Parse("unterminated string".into()));
            }
            let b = self.input[self.pos];
            if b == b'"' { self.pos += 1; return Ok(Expr { kind: ExprKind::Str(s), span }); }
            if b == b'\\' {
                self.pos += 1;
                if self.pos >= self.input.len() {
                    return Err(EvalError::Parse("unterminated escape".into()));
                }
                match self.input[self.pos] {
                    b'n' => s.push('\n'),
                    b't' => s.push('\t'),
                    b'\\' => s.push('\\'),
                    b'"' => s.push('"'),
                    c => { s.push('\\'); s.push(c as char); }
                }
            } else {
                s.push(b as char);
            }
            self.pos += 1;
        }
    }

    fn parse_hash(&mut self, span: Span) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '#'
        match self.peek() {
            Some(b't') => { self.pos += 1; Ok(Expr { kind: ExprKind::Boolean(true), span }) }
            Some(b'f') => { self.pos += 1; Ok(Expr { kind: ExprKind::Boolean(false), span }) }
            Some(b'\\') => self.parse_char(span),
            _ => Err(EvalError::Parse("unexpected # literal".into())),
        }
    }

    fn parse_char(&mut self, span: Span) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '\'
        if self.pos >= self.input.len() {
            return Err(EvalError::Parse("unexpected end of character literal".into()));
        }
        // Check for named characters like #\space, #\newline
        let start = self.pos;
        while self.pos < self.input.len() {
            let b = self.input[self.pos];
            if b.is_ascii_whitespace() || b == b'(' || b == b')' || b == b'"' || b == b';' {
                break;
            }
            self.pos += 1;
        }
        let token = std::str::from_utf8(&self.input[start..self.pos]).expect("valid UTF-8");
        let c = match token {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().expect("single-char string has a first char"),
            _ => return Err(EvalError::Parse(format!("unknown character name: {token}"))),
        };
        Ok(Expr { kind: ExprKind::Char(c), span })
    }

    fn parse_atom(&mut self, span: Span) -> Result<Expr, EvalError> {
        let start = self.pos;
        while self.pos < self.input.len() {
            let b = self.input[self.pos];
            if b.is_ascii_whitespace() || b == b'(' || b == b')' || b == b'"' || b == b';' {
                break;
            }
            self.pos += 1;
        }
        if self.pos == start {
            return Err(EvalError::Parse("unexpected character".into()));
        }
        let token = std::str::from_utf8(&self.input[start..self.pos]).expect("input is valid UTF-8");
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Expr { kind: ExprKind::Integer(n), span });
        }
        Ok(Expr { kind: ExprKind::Symbol(token.to_string()), span })
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.pos >= self.input.len() { break; }
            exprs.push(self.parse_expr()?);
        }
        if exprs.is_empty() {
            return Err(EvalError::Parse("empty input".into()));
        }
        Ok(exprs)
    }
}

// ── Evaluator ──

fn new_frame() -> Frame {
    Rc::new(RefCell::new(HashMap::new()))
}

fn env_lookup(env: &Env, name: &str) -> Result<Value, EvalError> {
    for frame in env.iter().rev() {
        if let Some(v) = frame.borrow().get(name).cloned() {
            return Ok(v);
        }
    }
    Err(EvalError::UnboundVariable(name.to_string()))
}

fn env_define(env: &Env, name: String, val: Value) {
    env.last().expect("env must have at least one frame").borrow_mut().insert(name, val);
}

fn env_set(env: &Env, name: &str, val: Value) -> Result<(), EvalError> {
    for frame in env.iter().rev() {
        let mut f = frame.borrow_mut();
        if f.contains_key(name) {
            f.insert(name.to_string(), val);
            return Ok(());
        }
    }
    Err(EvalError::UnboundVariable(name.to_string()))
}

fn with_span(span: Span, err: EvalError) -> EvalError {
    let msg = err.to_string();
    if msg.as_bytes().windows(2).any(|w| w[0].is_ascii_digit() && w[1] == b':') {
        return err;
    }
    EvalError::Generic(format!("{}:{}: {}", span.line, span.col, msg))
}

fn eval(expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let span = expr.span;
    eval_inner(expr, env, output).map_err(|e| with_span(span, e))
}

fn eval_inner(expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Str(s) => Ok(make_str(s.clone())),
        ExprKind::Symbol(name) => env_lookup(env, name),
        ExprKind::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            if let ExprKind::Symbol(op) = &items[0].kind {
                match op.as_str() {
                    "define" => return eval_define(&items[1..], env, output),
                    "set!" => {
                        if items.len() != 3 {
                            return Err(EvalError::Arity("set! requires 2 arguments".into()));
                        }
                        if let ExprKind::Symbol(name) = &items[1].kind {
                            let val = eval(&items[2], env, output)?;
                            env_set(env, name, val)?;
                            return Ok(Value::Boolean(false));
                        } else {
                            return Err(EvalError::Type("set! requires a symbol".into()));
                        }
                    }
                    "if" => return eval_if(&items[1..], env, output),
                    "quote" => return eval_quote(&items[1..]),
                    "lambda" => return eval_lambda(&items[1..], env),
                    "and" => return eval_and(&items[1..], env, output),
                    "or" => return eval_or(&items[1..], env, output),
                    "let" => return eval_let(&items[1..], env, output),
                    "begin" => return eval_begin(&items[1..], env, output),
                    "cond" => return eval_cond(&items[1..], env, output),
                    _ => {}
                }
                if is_builtin(op) {
                    let args: Vec<Value> = items[1..].iter().map(|a| eval(a, env, output)).collect::<Result<_, _>>()?;
                    return eval_builtin(op, &args, output);
                }
            }
            let func = eval(&items[0], env, output)?;
            let args: Vec<Value> = items[1..].iter().map(|a| eval(a, env, output)).collect::<Result<_, _>>()?;
            apply_proc(&func, &args, output)
        }
    }
}

fn apply_proc(func: &Value, args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Procedure(params, body, closure_env) => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let mut new_env = closure_env.clone();
            let frame = new_frame();
            for (p, a) in params.iter().zip(args.iter()) {
                frame.borrow_mut().insert(p.clone(), a.clone());
            }
            new_env.push(frame);
            let mut result = Value::Boolean(false);
            for expr in body {
                result = eval(expr, &mut new_env, output)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type("not a procedure".into())),
    }
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

fn expect_integer(v: &Value, context: &str) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{context}: expected integer, got {v}"))),
    }
}

fn eval_define(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires at least 2 arguments".into()));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires exactly 2 arguments".into()));
            }
            let val = eval(&args[1], env, output)?;
            env_define(env, name.clone(), val);
            Ok(Value::Boolean(false))
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: expected symbol as function name".into())),
            };
            let params: Vec<String> = sig[1..].iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type("define: expected symbol as parameter".into())),
            }).collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            let proc = Value::Procedure(params, body, env.clone());
            env_define(env, name, proc);
            Ok(Value::Boolean(false))
        }
        _ => Err(EvalError::Type("define: expected symbol or list".into())),
    }
}

fn eval_if(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env, output)?;
    if is_truthy(&cond) {
        eval(&args[1], env, output)
    } else if args.len() == 3 {
        eval(&args[2], env, output)
    } else {
        Ok(Value::Boolean(false))
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("quote requires 1 argument".into()));
    }
    Ok(expr_to_value(&args[0]))
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::Str(s) => make_str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
    }
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires at least 2 arguments".into()));
    }
    let params = match &args[0].kind {
        ExprKind::List(items) => {
            items.iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type("lambda: expected symbol as parameter".into())),
            }).collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Type("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Procedure(params, body, env.clone()))
}

fn eval_and(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for a in args {
        result = eval(a, env, output)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for a in args {
        result = eval(a, env, output)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn is_builtin(op: &str) -> bool {
    matches!(op, "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
        | "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
        | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
        | "display" | "write" | "newline"
        | "string-append" | "string-length" | "substring"
        | "string->number" | "number->string"
        | "symbol->string" | "string->symbol"
        | "string-ref" | "string-copy" | "string-set!")
}

fn display_value(v: &Value) -> String {
    match v {
        Value::Str(s) => s.borrow().clone(),
        Value::Char(c) => c.to_string(),
        Value::List(items) => {
            let mut s = String::from("(");
            for (i, item) in items.iter().enumerate() {
                if i > 0 { s.push(' '); }
                s.push_str(&display_value(item));
            }
            s.push(')');
            s
        }
        other => other.to_string(),
    }
}

fn eval_builtin(op: &str, args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_integer(a, "+")?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            let first = expect_integer(&args[0], "-")?;
            if args.len() == 1 {
                return Ok(Value::Integer(-first));
            }
            let mut result = first;
            for a in &args[1..] {
                result -= expect_integer(a, "-")?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= expect_integer(a, "*")?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let first = expect_integer(&args[0], "/")?;
            if args.len() == 1 {
                if first == 0 { return Err(EvalError::DivisionByZero); }
                return Ok(Value::Integer(1 / first));
            }
            let mut result = first;
            for a in &args[1..] {
                let d = expect_integer(a, "/")?;
                if d == 0 { return Err(EvalError::DivisionByZero); }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" | ">" | "=" | "<=" | ">=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{op} requires 2 arguments")));
            }
            let a = expect_integer(&args[0], op)?;
            let b = expect_integer(&args[1], op)?;
            let result = match op {
                "<" => a < b,
                ">" => a > b,
                "=" => a == b,
                "<=" => a <= b,
                ">=" => a >= b,
                _ => unreachable!(),
            };
            Ok(Value::Boolean(result))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            Ok(Value::Boolean(!is_truthy(&args[0])))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("cons requires 2 arguments".into()));
            }
            match &args[1] {
                Value::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => {
                    Ok(Value::List(vec![args[0].clone(), args[1].clone()]))
                }
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                _ => Err(EvalError::Type("car: expected non-empty list".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
                _ => Err(EvalError::Type("cdr: expected non-empty list".into())),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("null? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => {
            Ok(Value::List(args.to_vec()))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(EvalError::Type("length: expected list".into())),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for (i, a) in args.iter().enumerate() {
                if i == args.len() - 1 {
                    match a {
                        Value::List(items) => result.extend(items.iter().cloned()),
                        _ => result.push(a.clone()),
                    }
                } else {
                    match a {
                        Value::List(items) => result.extend(items.iter().cloned()),
                        _ => return Err(EvalError::Type("append: expected list".into())),
                    }
                }
            }
            Ok(Value::List(result))
        }
        "string?" => {
            if args.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "number?" => {
            if args.len() != 1 { return Err(EvalError::Arity("number? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 { return Err(EvalError::Arity("pair? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "char?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "display" => {
            if args.len() != 1 { return Err(EvalError::Arity("display requires 1 argument".into())); }
            output.push_str(&display_value(&args[0]));
            Ok(Value::Boolean(false))
        }
        "write" => {
            if args.len() != 1 { return Err(EvalError::Arity("write requires 1 argument".into())); }
            output.push_str(&args[0].to_string());
            Ok(Value::Boolean(false))
        }
        "newline" => {
            if !args.is_empty() { return Err(EvalError::Arity("newline requires 0 arguments".into())); }
            output.push('\n');
            Ok(Value::Boolean(false))
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(&s.borrow()),
                    _ => return Err(EvalError::Type("string-append: expected string".into())),
                }
            }
            Ok(make_str(result))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-length requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.borrow().len() as i64)),
                _ => Err(EvalError::Type("string-length: expected string".into())),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity("substring requires 3 arguments".into())); }
            let s = match &args[0] {
                Value::Str(s) => s.borrow().clone(),
                _ => return Err(EvalError::Type("substring: expected string".into())),
            };
            let start = expect_integer(&args[1], "substring")? as usize;
            let end = expect_integer(&args[2], "substring")? as usize;
            Ok(make_str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->number requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => match s.borrow().parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type("string->number: expected string".into())),
            }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("number->string requires 1 argument".into())); }
            let n = expect_integer(&args[0], "number->string")?;
            Ok(make_str(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol->string requires 1 argument".into())); }
            match &args[0] {
                Value::Symbol(s) => Ok(make_str(s.clone())),
                _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
            }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->symbol requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.borrow().clone())),
                _ => Err(EvalError::Type("string->symbol: expected string".into())),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ref requires 2 arguments".into())); }
            let s = match &args[0] {
                Value::Str(s) => s.borrow().clone(),
                _ => return Err(EvalError::Type("string-ref: expected string".into())),
            };
            let idx = expect_integer(&args[1], "string-ref")? as usize;
            match s.chars().nth(idx) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::Generic("string-ref: index out of range".into())),
            }
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-copy requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => Ok(make_str(s.borrow().clone())),
                _ => Err(EvalError::Type("string-copy: expected string".into())),
            }
        }
        "string-set!" => {
            if args.len() != 3 { return Err(EvalError::Arity("string-set! requires 3 arguments".into())); }
            let s = match &args[0] {
                Value::Str(s) => s.clone(),
                _ => return Err(EvalError::Type("string-set!: expected string".into())),
            };
            let idx = expect_integer(&args[1], "string-set!")? as usize;
            let c = match &args[2] {
                Value::Char(c) => *c,
                _ => return Err(EvalError::Type("string-set!: expected char".into())),
            };
            let mut borrowed = s.borrow_mut();
            let mut chars: Vec<char> = borrowed.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Generic("string-set!: index out of range".into()));
            }
            chars[idx] = c;
            *borrowed = chars.into_iter().collect();
            Ok(Value::Boolean(false))
        }
        _ => Err(EvalError::UnboundVariable(op.to_string())),
    }
}

fn eval_let(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into()));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 3 {
            return Err(EvalError::Arity("named let requires bindings and body".into()));
        }
        let bindings = match &args[1].kind {
            ExprKind::List(items) => items,
            _ => return Err(EvalError::Type("let: expected bindings list".into())),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env, output)?);
                    } else {
                        return Err(EvalError::Type("let: binding name must be symbol".into()));
                    }
                }
                _ => return Err(EvalError::Type("let: invalid binding".into())),
            }
        }
        let body = args[2..].to_vec();
        let mut let_env = env.clone();
        let frame = new_frame();
        let_env.push(frame.clone());
        let proc = Value::Procedure(params.clone(), body, let_env.clone());
        frame.borrow_mut().insert(name.clone(), proc);
        let func = env_lookup(&let_env, name)?;
        return apply_proc(&func, &inits, output);
    }
    // Regular let: (let ((var init) ...) body ...)
    let bindings = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("let: expected bindings list".into())),
    };
    let frame = new_frame();
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], env, output)?;
                    frame.borrow_mut().insert(s.clone(), val);
                } else {
                    return Err(EvalError::Type("let: binding name must be symbol".into()));
                }
            }
            _ => return Err(EvalError::Type("let: invalid binding".into())),
        }
    }
    env.push(frame);
    let mut result = Value::Boolean(false);
    for expr in &args[1..] {
        result = eval(expr, env, output)?;
    }
    env.pop();
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for expr in args {
        result = eval(expr, env, output)?;
    }
    Ok(result)
}

fn eval_cond(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    for clause in args {
        match &clause.kind {
            ExprKind::List(items) if !items.is_empty() => {
                if let ExprKind::Symbol(s) = &items[0].kind {
                    if s == "else" {
                        let mut result = Value::Boolean(false);
                        for expr in &items[1..] {
                            result = eval(expr, env, output)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&items[0], env, output)?;
                if is_truthy(&test) {
                    if items.len() == 1 {
                        return Ok(test);
                    }
                    let mut result = Value::Boolean(false);
                    for expr in &items[1..] {
                        result = eval(expr, env, output)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Type("cond: invalid clause".into())),
        }
    }
    Ok(Value::Boolean(false))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let mut env = vec![new_frame()];
    let mut output = String::new();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &mut env, &mut output)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let mut env = vec![new_frame()];
    let mut output = String::new();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &mut env, &mut output)?;
    }
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
