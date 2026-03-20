pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type EnvRef = Rc<RefCell<EnvFrame>>;
type OutputBuf = Rc<RefCell<String>>;

#[derive(Debug, Clone, PartialEq)]
struct EnvFrame {
    bindings: HashMap<String, Value>,
    parent: Option<EnvRef>,
}

impl EnvFrame {
    fn new() -> EnvRef {
        Rc::new(RefCell::new(EnvFrame {
            bindings: HashMap::new(),
            parent: None,
        }))
    }

    fn child(parent: &EnvRef) -> EnvRef {
        Rc::new(RefCell::new(EnvFrame {
            bindings: HashMap::new(),
            parent: Some(Rc::clone(parent)),
        }))
    }

    fn get(env: &EnvRef, name: &str) -> Option<Value> {
        let frame = env.borrow();
        if let Some(val) = frame.bindings.get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = frame.parent {
            Self::get(parent, name)
        } else {
            None
        }
    }

    fn set(env: &EnvRef, name: String, val: Value) {
        env.borrow_mut().bindings.insert(name, val);
    }

    fn set_existing(env: &EnvRef, name: &str, val: Value) -> bool {
        let mut frame = env.borrow_mut();
        if frame.bindings.contains_key(name) {
            frame.bindings.insert(name.to_string(), val);
            true
        } else if let Some(ref parent) = frame.parent {
            let parent = Rc::clone(parent);
            drop(frame);
            Self::set_existing(&parent, name, val)
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Span {
    line: usize,
    col: usize,
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Value>),
    Lambda(Vec<String>, Option<String>, Vec<SExpr>, EnvRef),
    Builtin(String),
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Void, Value::Void) => true,
            (Value::Lambda(..), Value::Lambda(..)) => false,
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    fn display_string(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            other => other.to_scheme_string(),
        }
    }

    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Char(c) => format!("#\\{}", c),
            Value::Symbol(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_scheme_string()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda(..) => "#<procedure>".to_string(),
            Value::Builtin(_) => "#<procedure>".to_string(),
            Value::Void => String::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<SExpr>),
}

#[derive(Debug, Clone, PartialEq)]
struct SExpr {
    expr: Expr,
    span: Span,
}

fn err_at(span: Span, msg: impl std::fmt::Display) -> EvalError {
    EvalError::Parse(format!("{}: {}", span, msg))
}

struct Token {
    text: String,
    span: Span,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line = 1usize;
    let mut col = 1usize;
    while i < chars.len() {
        match chars[i] {
            '\n' => {
                i += 1;
                line += 1;
                col = 1;
            }
            ' ' | '\t' | '\r' => {
                i += 1;
                col += 1;
            }
            '(' => {
                tokens.push(Token { text: "(".to_string(), span: Span { line, col } });
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push(Token { text: ")".to_string(), span: Span { line, col } });
                i += 1;
                col += 1;
            }
            '"' => {
                let start = Span { line, col };
                let mut s = String::from('"');
                i += 1;
                col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        i += 2;
                        col += 2;
                    } else {
                        if chars[i] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: s, span: start });
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '\'' => {
                tokens.push(Token { text: "'".to_string(), span: Span { line, col } });
                i += 1;
                col += 1;
            }
            _ => {
                let start = Span { line, col };
                let mut s = String::new();
                while i < chars.len()
                    && !matches!(
                        chars[i],
                        ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '\''
                    )
                {
                    s.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: s, span: start });
            }
        }
    }
    tokens
}

fn parse(tokens: &[Token], pos: usize) -> Result<(SExpr, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("1:1: unexpected end of input".to_string()));
    }
    let tok = &tokens[pos];
    let span = tok.span;
    if tok.text == "(" {
        let mut elems = Vec::new();
        let mut i = pos + 1;
        while i < tokens.len() && tokens[i].text != ")" {
            let (expr, next) = parse(tokens, i)?;
            elems.push(expr);
            i = next;
        }
        if i >= tokens.len() {
            return Err(err_at(span, "missing closing paren"));
        }
        Ok((SExpr { expr: Expr::List(elems), span }, i + 1))
    } else if tok.text == ")" {
        Err(err_at(span, "unexpected )"))
    } else if tok.text == "'" {
        let (inner, next) = parse(tokens, pos + 1)?;
        let quote_sym = SExpr {
            expr: Expr::Symbol("quote".to_string()),
            span,
        };
        Ok((
            SExpr {
                expr: Expr::List(vec![quote_sym, inner]),
                span,
            },
            next,
        ))
    } else {
        Ok((SExpr { expr: parse_atom(&tok.text)?, span }, pos + 1))
    }
}

fn parse_atom(token: &str) -> Result<Expr, EvalError> {
    if token == "#t" {
        return Ok(Expr::Boolean(true));
    }
    if token == "#f" {
        return Ok(Expr::Boolean(false));
    }
    if let Ok(n) = token.parse::<i64>() {
        return Ok(Expr::Integer(n));
    }
    if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
        let inner = &token[1..token.len() - 1];
        return Ok(Expr::Str(inner.to_string()));
    }
    if token.starts_with("#\\") {
        let rest = &token[2..];
        let c = match rest {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().unwrap(),
            _ => return Err(EvalError::Parse(format!("unknown character literal: {}", token))),
        };
        return Ok(Expr::Char(c));
    }
    Ok(Expr::Symbol(token.to_string()))
}

fn parse_all(input: &str) -> Result<Vec<SExpr>, EvalError> {
    let tokens = tokenize(input);
    let mut exprs = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let (expr, next) = parse(&tokens, pos)?;
        exprs.push(expr);
        pos = next;
    }
    Ok(exprs)
}

fn parse_params(param_exprs: &[SExpr], span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i].expr {
            Expr::Symbol(s) if s == "." => {
                if i + 1 != param_exprs.len() - 1 {
                    return Err(err_at(span, "invalid dot syntax in parameters"));
                }
                match &param_exprs[i + 1].expr {
                    Expr::Symbol(r) => return Ok((params, Some(r.clone()))),
                    _ => return Err(err_at(param_exprs[i + 1].span, "rest parameter must be a symbol")),
                }
            }
            Expr::Symbol(s) => {
                params.push(s.clone());
                i += 1;
            }
            _ => return Err(err_at(param_exprs[i].span, "parameter must be a symbol")),
        }
    }
    Ok((params, None))
}

fn sexpr_to_value(se: &SExpr) -> Value {
    match &se.expr {
        Expr::Integer(n) => Value::Integer(*n),
        Expr::Boolean(b) => Value::Boolean(*b),
        Expr::Str(s) => Value::Str(s.clone()),
        Expr::Char(c) => Value::Char(*c),
        Expr::Symbol(s) => Value::Symbol(s.clone()),
        Expr::List(elems) => Value::List(elems.iter().map(sexpr_to_value).collect()),
    }
}

enum Step {
    Done(Value),
    Tail(SExpr, EnvRef),
}

fn eval_expr(se: &SExpr, env: &EnvRef, out: &OutputBuf) -> Result<Value, EvalError> {
    let mut result = eval_step(se, env, out)?;
    loop {
        match result {
            Step::Done(v) => return Ok(v),
            Step::Tail(expr, env) => {
                result = eval_step(&expr, &env, out)?;
            }
        }
    }
}

fn eval_step(se: &SExpr, env: &EnvRef, out: &OutputBuf) -> Result<Step, EvalError> {
    let span = se.span;
    match &se.expr {
        Expr::Integer(n) => Ok(Step::Done(Value::Integer(*n))),
        Expr::Boolean(b) => Ok(Step::Done(Value::Boolean(*b))),
        Expr::Str(s) => Ok(Step::Done(Value::Str(s.clone()))),
        Expr::Char(c) => Ok(Step::Done(Value::Char(*c))),
        Expr::Symbol(s) => EnvFrame::get(env, s)
            .map(Step::Done)
            .ok_or_else(|| err_at(span, format!("unbound variable: {}", s))),
        Expr::List(elems) => {
            if elems.is_empty() {
                return Err(err_at(span, "empty application"));
            }
            if let Expr::Symbol(op) = &elems[0].expr {
                match op.as_str() {
                    "define" => {
                        if elems.len() < 3 {
                            return Err(err_at(span, "define requires exactly 2 arguments"));
                        }
                        match &elems[1].expr {
                            Expr::Symbol(name) => {
                                if elems.len() != 3 {
                                    return Err(err_at(span, "define requires exactly 2 arguments"));
                                }
                                let val = eval_expr(&elems[2], env, out)?;
                                if let Value::Lambda(params, rest, body, closure_env) = &val {
                                    let val = Value::Lambda(
                                        params.clone(),
                                        rest.clone(),
                                        body.clone(),
                                        Rc::clone(closure_env),
                                    );
                                    EnvFrame::set(env, name.clone(), val.clone());
                                    EnvFrame::set(closure_env, name.clone(), val);
                                } else {
                                    EnvFrame::set(env, name.clone(), val);
                                }
                                Ok(Step::Done(Value::Void))
                            }
                            Expr::List(name_and_params) => {
                                if name_and_params.is_empty() {
                                    return Err(err_at(
                                        span,
                                        "define shorthand requires a name",
                                    ));
                                }
                                if let Expr::Symbol(name) = &name_and_params[0].expr {
                                    let (params, rest) = parse_params(&name_and_params[1..], span)?;
                                    let body: Vec<SExpr> = elems[2..].to_vec();
                                    let closure_env = Rc::clone(env);
                                    let val =
                                        Value::Lambda(params, rest, body, closure_env.clone());
                                    EnvFrame::set(env, name.clone(), val.clone());
                                    EnvFrame::set(&closure_env, name.clone(), val);
                                    Ok(Step::Done(Value::Void))
                                } else {
                                    Err(err_at(span, "define requires a symbol as name"))
                                }
                            }
                            _ => Err(err_at(span, "define requires a symbol or list")),
                        }
                    }
                    "set!" => {
                        if elems.len() != 3 {
                            return Err(err_at(span, "set! requires exactly 2 arguments"));
                        }
                        if let Expr::Symbol(name) = &elems[1].expr {
                            let val = eval_expr(&elems[2], env, out)?;
                            if EnvFrame::set_existing(env, name, val) {
                                Ok(Step::Done(Value::Void))
                            } else {
                                Err(err_at(span, format!("set!: unbound variable: {}", name)))
                            }
                        } else {
                            Err(err_at(span, "set! requires a symbol"))
                        }
                    }
                    "lambda" => {
                        if elems.len() < 3 {
                            return Err(err_at(span, "lambda requires params and body"));
                        }
                        match &elems[1].expr {
                            Expr::List(param_exprs) => {
                                let (params, rest) = parse_params(param_exprs, span)?;
                                let body: Vec<SExpr> = elems[2..].to_vec();
                                Ok(Step::Done(Value::Lambda(params, rest, body, Rc::clone(env))))
                            }
                            Expr::Symbol(rest_name) => {
                                let body: Vec<SExpr> = elems[2..].to_vec();
                                Ok(Step::Done(Value::Lambda(vec![], Some(rest_name.clone()), body, Rc::clone(env))))
                            }
                            _ => Err(err_at(span, "lambda params must be a list or symbol")),
                        }
                    }
                    "if" => {
                        if elems.len() < 3 || elems.len() > 4 {
                            return Err(err_at(span, "if requires 2 or 3 arguments"));
                        }
                        let cond = eval_expr(&elems[1], env, out)?;
                        if !is_false(&cond) {
                            Ok(Step::Tail(elems[2].clone(), Rc::clone(env)))
                        } else if elems.len() == 4 {
                            Ok(Step::Tail(elems[3].clone(), Rc::clone(env)))
                        } else {
                            Ok(Step::Done(Value::Void))
                        }
                    }
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "quote requires exactly 1 argument"));
                        }
                        Ok(Step::Done(sexpr_to_value(&elems[1])))
                    }
                    "+" => {
                        let mut sum: i64 = 0;
                        for arg in &elems[1..] {
                            sum += require_int(&eval_expr(arg, env, out)?, arg.span)?;
                        }
                        Ok(Step::Done(Value::Integer(sum)))
                    }
                    "-" => {
                        if elems.len() < 2 {
                            return Err(err_at(span, "- requires at least one argument"));
                        }
                        let first = require_int(&eval_expr(&elems[1], env, out)?, elems[1].span)?;
                        if elems.len() == 2 {
                            Ok(Step::Done(Value::Integer(-first)))
                        } else {
                            let mut result = first;
                            for arg in &elems[2..] {
                                result -= require_int(&eval_expr(arg, env, out)?, arg.span)?;
                            }
                            Ok(Step::Done(Value::Integer(result)))
                        }
                    }
                    "*" => {
                        let mut product: i64 = 1;
                        for arg in &elems[1..] {
                            product *= require_int(&eval_expr(arg, env, out)?, arg.span)?;
                        }
                        Ok(Step::Done(Value::Integer(product)))
                    }
                    "/" => {
                        if elems.len() < 3 {
                            return Err(err_at(span, "/ requires at least two arguments"));
                        }
                        let mut result =
                            require_int(&eval_expr(&elems[1], env, out)?, elems[1].span)?;
                        for arg in &elems[2..] {
                            let divisor = require_int(&eval_expr(arg, env, out)?, arg.span)?;
                            if divisor == 0 {
                                return Err(err_at(arg.span, "division by zero"));
                            }
                            result /= divisor;
                        }
                        Ok(Step::Done(Value::Integer(result)))
                    }
                    "<" => {
                        let (a, b) = require_two_ints(&elems[1..], "<", span, env, out)?;
                        Ok(Step::Done(Value::Boolean(a < b)))
                    }
                    ">" => {
                        let (a, b) = require_two_ints(&elems[1..], ">", span, env, out)?;
                        Ok(Step::Done(Value::Boolean(a > b)))
                    }
                    "=" => {
                        let (a, b) = require_two_ints(&elems[1..], "=", span, env, out)?;
                        Ok(Step::Done(Value::Boolean(a == b)))
                    }
                    "<=" => {
                        let (a, b) = require_two_ints(&elems[1..], "<=", span, env, out)?;
                        Ok(Step::Done(Value::Boolean(a <= b)))
                    }
                    ">=" => {
                        let (a, b) = require_two_ints(&elems[1..], ">=", span, env, out)?;
                        Ok(Step::Done(Value::Boolean(a >= b)))
                    }
                    "not" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "not requires exactly one argument"));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Step::Done(Value::Boolean(is_false(&val))))
                    }
                    "and" => {
                        if elems.len() == 1 {
                            return Ok(Step::Done(Value::Boolean(true)));
                        }
                        for arg in &elems[1..elems.len() - 1] {
                            let result = eval_expr(arg, env, out)?;
                            if is_false(&result) {
                                return Ok(Step::Done(result));
                            }
                        }
                        Ok(Step::Tail(elems[elems.len() - 1].clone(), Rc::clone(env)))
                    }
                    "or" => {
                        if elems.len() == 1 {
                            return Ok(Step::Done(Value::Boolean(false)));
                        }
                        for arg in &elems[1..elems.len() - 1] {
                            let result = eval_expr(arg, env, out)?;
                            if !is_false(&result) {
                                return Ok(Step::Done(result));
                            }
                        }
                        Ok(Step::Tail(elems[elems.len() - 1].clone(), Rc::clone(env)))
                    }
                    "cons" => {
                        if elems.len() != 3 {
                            return Err(err_at(span, "cons requires exactly 2 arguments"));
                        }
                        let head = eval_expr(&elems[1], env, out)?;
                        let tail = eval_expr(&elems[2], env, out)?;
                        match tail {
                            Value::List(mut v) => {
                                v.insert(0, head);
                                Ok(Step::Done(Value::List(v)))
                            }
                            _ => Err(err_at(
                                span,
                                "cons: second argument must be a list",
                            )),
                        }
                    }
                    "car" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "car requires exactly 1 argument"));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        match val {
                            Value::List(v) if !v.is_empty() => Ok(Step::Done(v[0].clone())),
                            _ => Err(err_at(
                                span,
                                "car: argument must be a non-empty list",
                            )),
                        }
                    }
                    "cdr" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "cdr requires exactly 1 argument"));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        match val {
                            Value::List(v) if !v.is_empty() => {
                                Ok(Step::Done(Value::List(v[1..].to_vec())))
                            }
                            _ => Err(err_at(
                                span,
                                "cdr: argument must be a non-empty list",
                            )),
                        }
                    }
                    "null?" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "null? requires exactly 1 argument"));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Step::Done(Value::Boolean(
                            matches!(val, Value::List(ref v) if v.is_empty()),
                        )))
                    }
                    "list" => {
                        let mut items = Vec::new();
                        for arg in &elems[1..] {
                            items.push(eval_expr(arg, env, out)?);
                        }
                        Ok(Step::Done(Value::List(items)))
                    }
                    "string?" => {
                        if elems.len() != 2 {
                            return Err(err_at(
                                span,
                                "string? requires exactly 1 argument",
                            ));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Step::Done(Value::Boolean(matches!(val, Value::Str(_)))))
                    }
                    "number?" => {
                        if elems.len() != 2 {
                            return Err(err_at(
                                span,
                                "number? requires exactly 1 argument",
                            ));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Step::Done(Value::Boolean(matches!(val, Value::Integer(_)))))
                    }
                    "boolean?" => {
                        if elems.len() != 2 {
                            return Err(err_at(
                                span,
                                "boolean? requires exactly 1 argument",
                            ));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Step::Done(Value::Boolean(matches!(val, Value::Boolean(_)))))
                    }
                    "pair?" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "pair? requires exactly 1 argument"));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Step::Done(Value::Boolean(
                            matches!(val, Value::List(ref v) if !v.is_empty()),
                        )))
                    }
                    "symbol?" => {
                        if elems.len() != 2 {
                            return Err(err_at(
                                span,
                                "symbol? requires exactly 1 argument",
                            ));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Step::Done(Value::Boolean(matches!(val, Value::Symbol(_)))))
                    }
                    "length" => {
                        if elems.len() != 2 {
                            return Err(err_at(
                                span,
                                "length requires exactly 1 argument",
                            ));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        match val {
                            Value::List(v) => Ok(Step::Done(Value::Integer(v.len() as i64))),
                            _ => Err(err_at(span, "length: argument must be a list")),
                        }
                    }
                    "begin" => {
                        if elems.len() <= 1 {
                            return Ok(Step::Done(Value::Void));
                        }
                        for arg in &elems[1..elems.len() - 1] {
                            eval_expr(arg, env, out)?;
                        }
                        Ok(Step::Tail(elems[elems.len() - 1].clone(), Rc::clone(env)))
                    }
                    "let" => {
                        if elems.len() < 3 {
                            return Err(err_at(
                                span,
                                "let requires bindings and body",
                            ));
                        }
                        // Named let: (let name ((var init) ...) body...)
                        if let Expr::Symbol(loop_name) = &elems[1].expr {
                            if elems.len() < 4 {
                                return Err(err_at(span, "named let requires bindings and body"));
                            }
                            let bindings = match &elems[2].expr {
                                Expr::List(b) => b,
                                _ => return Err(err_at(elems[2].span, "named let bindings must be a list")),
                            };
                            let mut param_names = Vec::new();
                            let mut init_vals = Vec::new();
                            for binding in bindings {
                                match &binding.expr {
                                    Expr::List(pair) if pair.len() == 2 => {
                                        if let Expr::Symbol(name) = &pair[0].expr {
                                            param_names.push(name.clone());
                                            init_vals.push(eval_expr(&pair[1], env, out)?);
                                        } else {
                                            return Err(err_at(pair[0].span, "let binding name must be a symbol"));
                                        }
                                    }
                                    _ => return Err(err_at(binding.span, "let binding must be a pair")),
                                }
                            }
                            let body: Vec<SExpr> = elems[3..].to_vec();
                            let let_env = EnvFrame::child(env);
                            let lambda = Value::Lambda(param_names.clone(), None, body, let_env.clone());
                            EnvFrame::set(&let_env, loop_name.clone(), lambda);
                            for (name, val) in param_names.iter().zip(init_vals) {
                                EnvFrame::set(&let_env, name.clone(), val);
                            }
                            for expr in &elems[3..elems.len() - 1] {
                                eval_expr(expr, &let_env, out)?;
                            }
                            return Ok(Step::Tail(elems[elems.len() - 1].clone(), let_env));
                        }
                        let bindings = match &elems[1].expr {
                            Expr::List(b) => b,
                            _ => {
                                return Err(err_at(
                                    elems[1].span,
                                    "let bindings must be a list",
                                ))
                            }
                        };
                        let let_env = EnvFrame::child(env);
                        for binding in bindings {
                            match &binding.expr {
                                Expr::List(pair) if pair.len() == 2 => {
                                    if let Expr::Symbol(name) = &pair[0].expr {
                                        let val = eval_expr(&pair[1], env, out)?;
                                        EnvFrame::set(&let_env, name.clone(), val);
                                    } else {
                                        return Err(err_at(
                                            pair[0].span,
                                            "let binding name must be a symbol",
                                        ));
                                    }
                                }
                                _ => {
                                    return Err(err_at(
                                        binding.span,
                                        "let binding must be a pair",
                                    ))
                                }
                            }
                        }
                        for expr in &elems[2..elems.len() - 1] {
                            eval_expr(expr, &let_env, out)?;
                        }
                        Ok(Step::Tail(elems[elems.len() - 1].clone(), let_env))
                    }
                    "cond" => {
                        for clause in &elems[1..] {
                            match &clause.expr {
                                Expr::List(parts) if !parts.is_empty() => {
                                    if let Expr::Symbol(s) = &parts[0].expr {
                                        if s == "else" {
                                            if parts.len() <= 1 {
                                                return Ok(Step::Done(Value::Void));
                                            }
                                            for expr in &parts[1..parts.len() - 1] {
                                                eval_expr(expr, env, out)?;
                                            }
                                            return Ok(Step::Tail(
                                                parts[parts.len() - 1].clone(),
                                                Rc::clone(env),
                                            ));
                                        }
                                    }
                                    let test = eval_expr(&parts[0], env, out)?;
                                    if !is_false(&test) {
                                        if parts.len() == 1 {
                                            return Ok(Step::Done(test));
                                        }
                                        for expr in &parts[1..parts.len() - 1] {
                                            eval_expr(expr, env, out)?;
                                        }
                                        return Ok(Step::Tail(
                                            parts[parts.len() - 1].clone(),
                                            Rc::clone(env),
                                        ));
                                    }
                                }
                                _ => {
                                    return Err(err_at(
                                        clause.span,
                                        "cond clause must be a list",
                                    ))
                                }
                            }
                        }
                        Ok(Step::Done(Value::Void))
                    }
                    "display" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "display requires exactly 1 argument"));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        out.borrow_mut().push_str(&val.display_string());
                        Ok(Step::Done(Value::Void))
                    }
                    "write" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "write requires exactly 1 argument"));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        out.borrow_mut().push_str(&val.to_scheme_string());
                        Ok(Step::Done(Value::Void))
                    }
                    "newline" => {
                        if elems.len() != 1 {
                            return Err(err_at(span, "newline takes no arguments"));
                        }
                        out.borrow_mut().push('\n');
                        Ok(Step::Done(Value::Void))
                    }
                    "string-append" => {
                        let mut result = String::new();
                        for arg in &elems[1..] {
                            match eval_expr(arg, env, out)? {
                                Value::Str(s) => result.push_str(&s),
                                _ => return Err(err_at(arg.span, "string-append: expected string")),
                            }
                        }
                        Ok(Step::Done(Value::Str(result)))
                    }
                    "string-length" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "string-length requires exactly 1 argument"));
                        }
                        match eval_expr(&elems[1], env, out)? {
                            Value::Str(s) => Ok(Step::Done(Value::Integer(s.chars().count() as i64))),
                            _ => Err(err_at(elems[1].span, "string-length: expected string")),
                        }
                    }
                    "substring" => {
                        if elems.len() != 4 {
                            return Err(err_at(span, "substring requires 3 arguments"));
                        }
                        let s = match eval_expr(&elems[1], env, out)? {
                            Value::Str(s) => s,
                            _ => return Err(err_at(elems[1].span, "substring: expected string")),
                        };
                        let start = require_int(&eval_expr(&elems[2], env, out)?, elems[2].span)? as usize;
                        let end = require_int(&eval_expr(&elems[3], env, out)?, elems[3].span)? as usize;
                        let chars: Vec<char> = s.chars().collect();
                        if end > chars.len() || start > end {
                            return Err(err_at(span, "substring: index out of range"));
                        }
                        Ok(Step::Done(Value::Str(chars[start..end].iter().collect())))
                    }
                    "string->number" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "string->number requires exactly 1 argument"));
                        }
                        match eval_expr(&elems[1], env, out)? {
                            Value::Str(s) => match s.parse::<i64>() {
                                Ok(n) => Ok(Step::Done(Value::Integer(n))),
                                Err(_) => Ok(Step::Done(Value::Boolean(false))),
                            },
                            _ => Err(err_at(elems[1].span, "string->number: expected string")),
                        }
                    }
                    "number->string" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "number->string requires exactly 1 argument"));
                        }
                        let n = require_int(&eval_expr(&elems[1], env, out)?, elems[1].span)?;
                        Ok(Step::Done(Value::Str(n.to_string())))
                    }
                    "symbol->string" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "symbol->string requires exactly 1 argument"));
                        }
                        match eval_expr(&elems[1], env, out)? {
                            Value::Symbol(s) => Ok(Step::Done(Value::Str(s))),
                            _ => Err(err_at(elems[1].span, "symbol->string: expected symbol")),
                        }
                    }
                    "string->symbol" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "string->symbol requires exactly 1 argument"));
                        }
                        match eval_expr(&elems[1], env, out)? {
                            Value::Str(s) => Ok(Step::Done(Value::Symbol(s))),
                            _ => Err(err_at(elems[1].span, "string->symbol: expected string")),
                        }
                    }
                    "string-ref" => {
                        if elems.len() != 3 {
                            return Err(err_at(span, "string-ref requires exactly 2 arguments"));
                        }
                        let s = match eval_expr(&elems[1], env, out)? {
                            Value::Str(s) => s,
                            _ => return Err(err_at(elems[1].span, "string-ref: expected string")),
                        };
                        let idx = require_int(&eval_expr(&elems[2], env, out)?, elems[2].span)? as usize;
                        let chars: Vec<char> = s.chars().collect();
                        if idx >= chars.len() {
                            return Err(err_at(span, "string-ref: index out of range"));
                        }
                        Ok(Step::Done(Value::Char(chars[idx])))
                    }
                    "string-copy" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "string-copy requires exactly 1 argument"));
                        }
                        let s = match eval_expr(&elems[1], env, out)? {
                            Value::Str(s) => s,
                            _ => return Err(err_at(elems[1].span, "string-copy: expected string")),
                        };
                        Ok(Step::Done(Value::Str(s)))
                    }
                    "string-set!" => {
                        Err(err_at(span, "string-set!: strings are immutable"))
                    }
                    "string->list" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "string->list requires exactly 1 argument"));
                        }
                        match eval_expr(&elems[1], env, out)? {
                            Value::Str(s) => Ok(Step::Done(Value::List(s.chars().map(Value::Char).collect()))),
                            _ => Err(err_at(elems[1].span, "string->list: expected string")),
                        }
                    }
                    "list->string" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "list->string requires exactly 1 argument"));
                        }
                        match eval_expr(&elems[1], env, out)? {
                            Value::List(items) => {
                                let mut s = String::new();
                                for item in &items {
                                    match item {
                                        Value::Char(c) => s.push(*c),
                                        _ => return Err(err_at(span, "list->string: expected list of characters")),
                                    }
                                }
                                Ok(Step::Done(Value::Str(s)))
                            }
                            _ => Err(err_at(elems[1].span, "list->string: expected list")),
                        }
                    }
                    "char->integer" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "char->integer requires exactly 1 argument"));
                        }
                        match eval_expr(&elems[1], env, out)? {
                            Value::Char(c) => Ok(Step::Done(Value::Integer(c as i64))),
                            _ => Err(err_at(elems[1].span, "char->integer: expected char")),
                        }
                    }
                    "integer->char" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "integer->char requires exactly 1 argument"));
                        }
                        let n = require_int(&eval_expr(&elems[1], env, out)?, elems[1].span)?;
                        if let Some(c) = char::from_u32(n as u32) {
                            Ok(Step::Done(Value::Char(c)))
                        } else {
                            Err(err_at(span, "integer->char: invalid code point"))
                        }
                    }
                    "map" => {
                        if elems.len() != 3 {
                            return Err(err_at(span, "map requires exactly 2 arguments"));
                        }
                        let func = eval_expr(&elems[1], env, out)?;
                        let lst = eval_expr(&elems[2], env, out)?;
                        match lst {
                            Value::List(items) => {
                                let mut results = Vec::new();
                                for item in items {
                                    let r = apply_value(&func, &[item], span, out)?;
                                    results.push(r);
                                }
                                Ok(Step::Done(Value::List(results)))
                            }
                            _ => Err(err_at(elems[2].span, "map: expected list")),
                        }
                    }
                    "char?" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "char? requires exactly 1 argument"));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Step::Done(Value::Boolean(matches!(val, Value::Char(_)))))
                    }
                    _ => apply_proc_step(elems, span, env, out),
                }
            } else {
                apply_proc_step(elems, span, env, out)
            }
        }
    }
}

fn bind_args(params: &[String], rest: &Option<String>, args: &[Value], call_env: &EnvRef, call_span: Span) -> Result<(), EvalError> {
    if let Some(rest_name) = rest {
        if args.len() < params.len() {
            return Err(err_at(call_span, format!("expected at least {} arguments, got {}", params.len(), args.len())));
        }
        for (param, arg) in params.iter().zip(args) {
            EnvFrame::set(call_env, param.clone(), arg.clone());
        }
        EnvFrame::set(call_env, rest_name.clone(), Value::List(args[params.len()..].to_vec()));
    } else {
        if params.len() != args.len() {
            return Err(err_at(call_span, format!("expected {} arguments, got {}", params.len(), args.len())));
        }
        for (param, arg) in params.iter().zip(args) {
            EnvFrame::set(call_env, param.clone(), arg.clone());
        }
    }
    Ok(())
}

fn apply_builtin(name: &str, args: &[Value], span: Span, out: &OutputBuf) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for arg in args { sum += require_int(arg, span)?; }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() { return Err(err_at(span, "- requires at least one argument")); }
            let first = require_int(&args[0], span)?;
            if args.len() == 1 { return Ok(Value::Integer(-first)); }
            let mut result = first;
            for arg in &args[1..] { result -= require_int(arg, span)?; }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for arg in args { product *= require_int(arg, span)?; }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.len() < 2 { return Err(err_at(span, "/ requires at least two arguments")); }
            let mut result = require_int(&args[0], span)?;
            for arg in &args[1..] {
                let d = require_int(arg, span)?;
                if d == 0 { return Err(err_at(span, "division by zero")); }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => { Ok(Value::Boolean(require_int(&args[0], span)? < require_int(&args[1], span)?)) }
        ">" => { Ok(Value::Boolean(require_int(&args[0], span)? > require_int(&args[1], span)?)) }
        "=" => { Ok(Value::Boolean(require_int(&args[0], span)? == require_int(&args[1], span)?)) }
        "<=" => { Ok(Value::Boolean(require_int(&args[0], span)? <= require_int(&args[1], span)?)) }
        ">=" => { Ok(Value::Boolean(require_int(&args[0], span)? >= require_int(&args[1], span)?)) }
        "not" => {
            Ok(Value::Boolean(is_false(&args[0])))
        }
        "cons" => {
            match &args[1] {
                Value::List(v) => {
                    let mut new = vec![args[0].clone()];
                    new.extend(v.iter().cloned());
                    Ok(Value::List(new))
                }
                _ => Err(err_at(span, "cons: second argument must be a list")),
            }
        }
        "car" => {
            match &args[0] {
                Value::List(v) if !v.is_empty() => Ok(v[0].clone()),
                _ => Err(err_at(span, "car: argument must be a non-empty list")),
            }
        }
        "cdr" => {
            match &args[0] {
                Value::List(v) if !v.is_empty() => Ok(Value::List(v[1..].to_vec())),
                _ => Err(err_at(span, "cdr: argument must be a non-empty list")),
            }
        }
        "null?" => {
            Ok(Value::Boolean(matches!(&args[0], Value::List(v) if v.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            match &args[0] {
                Value::List(v) => Ok(Value::Integer(v.len() as i64)),
                _ => Err(err_at(span, "length: argument must be a list")),
            }
        }
        "display" => {
            out.borrow_mut().push_str(&args[0].display_string());
            Ok(Value::Void)
        }
        "write" => {
            out.borrow_mut().push_str(&args[0].to_scheme_string());
            Ok(Value::Void)
        }
        "newline" => {
            out.borrow_mut().push('\n');
            Ok(Value::Void)
        }
        "map" => {
            match &args[1] {
                Value::List(items) => {
                    let mut results = Vec::new();
                    for item in items {
                        results.push(apply_value(&args[0], &[item.clone()], span, out)?);
                    }
                    Ok(Value::List(results))
                }
                _ => Err(err_at(span, "map: expected list")),
            }
        }
        "apply" => {
            if args.len() < 2 { return Err(err_at(span, "apply requires at least 2 arguments")); }
            let func = &args[0];
            let last = &args[args.len() - 1];
            let tail_list = match last {
                Value::List(v) => v.clone(),
                _ => return Err(err_at(span, "apply: last argument must be a list")),
            };
            let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            all_args.extend(tail_list);
            apply_value(func, &all_args, span, out)
        }
        "string?" => Ok(Value::Boolean(matches!(&args[0], Value::Str(_)))),
        "number?" => Ok(Value::Boolean(matches!(&args[0], Value::Integer(_)))),
        "boolean?" => Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_)))),
        "pair?" => Ok(Value::Boolean(matches!(&args[0], Value::List(v) if !v.is_empty()))),
        "symbol?" => Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_)))),
        "char?" => Ok(Value::Boolean(matches!(&args[0], Value::Char(_)))),
        "string-append" => {
            let mut result = String::new();
            for arg in args {
                match arg { Value::Str(s) => result.push_str(s), _ => return Err(err_at(span, "string-append: expected string")) }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            match &args[0] { Value::Str(s) => Ok(Value::Integer(s.chars().count() as i64)), _ => Err(err_at(span, "string-length: expected string")) }
        }
        "substring" => {
            let s = match &args[0] { Value::Str(s) => s, _ => return Err(err_at(span, "substring: expected string")) };
            let start = require_int(&args[1], span)? as usize;
            let end = require_int(&args[2], span)? as usize;
            let chars: Vec<char> = s.chars().collect();
            if end > chars.len() || start > end { return Err(err_at(span, "substring: index out of range")); }
            Ok(Value::Str(chars[start..end].iter().collect()))
        }
        "string->number" => {
            match &args[0] { Value::Str(s) => match s.parse::<i64>() { Ok(n) => Ok(Value::Integer(n)), Err(_) => Ok(Value::Boolean(false)) }, _ => Err(err_at(span, "string->number: expected string")) }
        }
        "number->string" => {
            let n = require_int(&args[0], span)?;
            Ok(Value::Str(n.to_string()))
        }
        "symbol->string" => {
            match &args[0] { Value::Symbol(s) => Ok(Value::Str(s.clone())), _ => Err(err_at(span, "symbol->string: expected symbol")) }
        }
        "string->symbol" => {
            match &args[0] { Value::Str(s) => Ok(Value::Symbol(s.clone())), _ => Err(err_at(span, "string->symbol: expected string")) }
        }
        "string-ref" => {
            let s = match &args[0] { Value::Str(s) => s, _ => return Err(err_at(span, "string-ref: expected string")) };
            let idx = require_int(&args[1], span)? as usize;
            let chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() { return Err(err_at(span, "string-ref: index out of range")); }
            Ok(Value::Char(chars[idx]))
        }
        "string-copy" => {
            match &args[0] { Value::Str(s) => Ok(Value::Str(s.clone())), _ => Err(err_at(span, "string-copy: expected string")) }
        }
        "string-set!" => Err(err_at(span, "string-set!: strings are immutable")),
        "string->list" => {
            match &args[0] { Value::Str(s) => Ok(Value::List(s.chars().map(Value::Char).collect())), _ => Err(err_at(span, "string->list: expected string")) }
        }
        "list->string" => {
            match &args[0] {
                Value::List(items) => {
                    let mut s = String::new();
                    for item in items { match item { Value::Char(c) => s.push(*c), _ => return Err(err_at(span, "list->string: expected list of characters")) } }
                    Ok(Value::Str(s))
                }
                _ => Err(err_at(span, "list->string: expected list")),
            }
        }
        "char->integer" => {
            match &args[0] { Value::Char(c) => Ok(Value::Integer(*c as i64)), _ => Err(err_at(span, "char->integer: expected char")) }
        }
        "integer->char" => {
            let n = require_int(&args[0], span)?;
            match char::from_u32(n as u32) { Some(c) => Ok(Value::Char(c)), None => Err(err_at(span, "integer->char: invalid code point")) }
        }
        _ => Err(err_at(span, format!("unknown builtin: {}", name))),
    }
}

fn apply_value(func: &Value, args: &[Value], call_span: Span, out: &OutputBuf) -> Result<Value, EvalError> {
    match func {
        Value::Lambda(params, rest, body, closure_env) => {
            let call_env = EnvFrame::child(closure_env);
            bind_args(params, rest, args, &call_env, call_span)?;
            let mut result = Value::Void;
            for expr in body {
                result = eval_expr(expr, &call_env, out)?;
            }
            Ok(result)
        }
        Value::Builtin(name) => apply_builtin(name, args, call_span, out),
        _ => Err(err_at(call_span, "not a procedure")),
    }
}

fn apply_proc_step(elems: &[SExpr], call_span: Span, env: &EnvRef, out: &OutputBuf) -> Result<Step, EvalError> {
    let func = eval_expr(&elems[0], env, out)?;
    let args: Result<Vec<Value>, _> = elems[1..].iter().map(|a| eval_expr(a, env, out)).collect();
    let args = args?;
    match func {
        Value::Lambda(params, rest, body, closure_env) => {
            let call_env = EnvFrame::child(&closure_env);
            bind_args(&params, &rest, &args, &call_env, call_span)?;
            if body.is_empty() {
                return Ok(Step::Done(Value::Void));
            }
            for expr in &body[..body.len() - 1] {
                eval_expr(expr, &call_env, out)?;
            }
            Ok(Step::Tail(body[body.len() - 1].clone(), call_env))
        }
        Value::Builtin(name) => Ok(Step::Done(apply_builtin(&name, &args, call_span, out)?)),
        _ => Err(err_at(call_span, "not a procedure")),
    }
}

fn is_false(val: &Value) -> bool {
    matches!(val, Value::Boolean(false))
}

fn require_two_ints(
    args: &[SExpr],
    op: &str,
    call_span: Span,
    env: &EnvRef,
    out: &OutputBuf,
) -> Result<(i64, i64), EvalError> {
    if args.len() != 2 {
        return Err(err_at(
            call_span,
            format!("{} requires exactly two arguments", op),
        ));
    }
    let a = require_int(&eval_expr(&args[0], env, out)?, args[0].span)?;
    let b = require_int(&eval_expr(&args[1], env, out)?, args[1].span)?;
    Ok((a, b))
}

fn require_int(val: &Value, span: Span) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        _ => Err(err_at(span, "expected integer")),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("42"), Ok("42".into()));
/// ```
fn init_builtins(env: &EnvRef) {
    for name in &[
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
        "cons", "car", "cdr", "null?", "list", "length",
        "display", "write", "newline", "map", "apply",
        "string?", "number?", "boolean?", "pair?", "symbol?", "char?",
        "string-append", "string-length", "substring", "string->number",
        "number->string", "symbol->string", "string->symbol",
        "string-ref", "string-copy", "string-set!", "string->list", "list->string",
        "char->integer", "integer->char",
    ] {
        EnvFrame::set(env, name.to_string(), Value::Builtin(name.to_string()));
    }
}

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("1:1: empty input".to_string()));
    }
    let env = EnvFrame::new();
    init_builtins(&env);
    let out: OutputBuf = Rc::new(RefCell::new(String::new()));
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval_expr(expr, &env, &out)?;
    }
    Ok(result.to_scheme_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("1:1: empty input".to_string()));
    }
    let env = EnvFrame::new();
    init_builtins(&env);
    let out: OutputBuf = Rc::new(RefCell::new(String::new()));
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval_expr(expr, &env, &out)?;
    }
    let output = out.borrow().clone();
    Ok((result.to_scheme_string(), output))
}

#[cfg(test)]
mod tests;
