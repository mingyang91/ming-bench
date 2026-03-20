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
    Symbol(String),
    List(Vec<Value>),
    Lambda(Vec<String>, Vec<SExpr>, EnvRef),
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Void, Value::Void) => true,
            (Value::Lambda(..), Value::Lambda(..)) => false,
            _ => false,
        }
    }
}

impl Value {
    fn display_string(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            other => other.to_scheme_string(),
        }
    }

    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_scheme_string()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda(..) => "#<procedure>".to_string(),
            Value::Void => String::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
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

fn sexpr_to_value(se: &SExpr) -> Value {
    match &se.expr {
        Expr::Integer(n) => Value::Integer(*n),
        Expr::Boolean(b) => Value::Boolean(*b),
        Expr::Str(s) => Value::Str(s.clone()),
        Expr::Symbol(s) => Value::Symbol(s.clone()),
        Expr::List(elems) => Value::List(elems.iter().map(sexpr_to_value).collect()),
    }
}

fn eval_expr(se: &SExpr, env: &EnvRef, out: &OutputBuf) -> Result<Value, EvalError> {
    let span = se.span;
    match &se.expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(s) => EnvFrame::get(env, s)
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
                                if let Value::Lambda(params, body, closure_env) = &val {
                                    let val = Value::Lambda(
                                        params.clone(),
                                        body.clone(),
                                        Rc::clone(closure_env),
                                    );
                                    EnvFrame::set(env, name.clone(), val.clone());
                                    EnvFrame::set(closure_env, name.clone(), val);
                                } else {
                                    EnvFrame::set(env, name.clone(), val);
                                }
                                Ok(Value::Void)
                            }
                            Expr::List(name_and_params) => {
                                if name_and_params.is_empty() {
                                    return Err(err_at(
                                        span,
                                        "define shorthand requires a name",
                                    ));
                                }
                                if let Expr::Symbol(name) = &name_and_params[0].expr {
                                    let params: Result<Vec<String>, _> = name_and_params[1..]
                                        .iter()
                                        .map(|p| match &p.expr {
                                            Expr::Symbol(s) => Ok(s.clone()),
                                            _ => Err(err_at(
                                                p.span,
                                                "parameter must be a symbol",
                                            )),
                                        })
                                        .collect();
                                    let params = params?;
                                    let body: Vec<SExpr> = elems[2..].to_vec();
                                    let closure_env = Rc::clone(env);
                                    let val =
                                        Value::Lambda(params, body, closure_env.clone());
                                    EnvFrame::set(env, name.clone(), val.clone());
                                    EnvFrame::set(&closure_env, name.clone(), val);
                                    Ok(Value::Void)
                                } else {
                                    Err(err_at(span, "define requires a symbol as name"))
                                }
                            }
                            _ => Err(err_at(span, "define requires a symbol or list")),
                        }
                    }
                    "lambda" => {
                        if elems.len() < 3 {
                            return Err(err_at(span, "lambda requires params and body"));
                        }
                        if let Expr::List(param_exprs) = &elems[1].expr {
                            let params: Result<Vec<String>, _> = param_exprs
                                .iter()
                                .map(|p| match &p.expr {
                                    Expr::Symbol(s) => Ok(s.clone()),
                                    _ => Err(err_at(p.span, "parameter must be a symbol")),
                                })
                                .collect();
                            let params = params?;
                            let body: Vec<SExpr> = elems[2..].to_vec();
                            Ok(Value::Lambda(params, body, Rc::clone(env)))
                        } else {
                            Err(err_at(span, "lambda params must be a list"))
                        }
                    }
                    "if" => {
                        if elems.len() < 3 || elems.len() > 4 {
                            return Err(err_at(span, "if requires 2 or 3 arguments"));
                        }
                        let cond = eval_expr(&elems[1], env, out)?;
                        if !is_false(&cond) {
                            eval_expr(&elems[2], env, out)
                        } else if elems.len() == 4 {
                            eval_expr(&elems[3], env, out)
                        } else {
                            Ok(Value::Void)
                        }
                    }
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "quote requires exactly 1 argument"));
                        }
                        Ok(sexpr_to_value(&elems[1]))
                    }
                    "+" => {
                        let mut sum: i64 = 0;
                        for arg in &elems[1..] {
                            sum += require_int(&eval_expr(arg, env, out)?, arg.span)?;
                        }
                        Ok(Value::Integer(sum))
                    }
                    "-" => {
                        if elems.len() < 2 {
                            return Err(err_at(span, "- requires at least one argument"));
                        }
                        let first = require_int(&eval_expr(&elems[1], env, out)?, elems[1].span)?;
                        if elems.len() == 2 {
                            Ok(Value::Integer(-first))
                        } else {
                            let mut result = first;
                            for arg in &elems[2..] {
                                result -= require_int(&eval_expr(arg, env, out)?, arg.span)?;
                            }
                            Ok(Value::Integer(result))
                        }
                    }
                    "*" => {
                        let mut product: i64 = 1;
                        for arg in &elems[1..] {
                            product *= require_int(&eval_expr(arg, env, out)?, arg.span)?;
                        }
                        Ok(Value::Integer(product))
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
                        Ok(Value::Integer(result))
                    }
                    "<" => {
                        let (a, b) = require_two_ints(&elems[1..], "<", span, env, out)?;
                        Ok(Value::Boolean(a < b))
                    }
                    ">" => {
                        let (a, b) = require_two_ints(&elems[1..], ">", span, env, out)?;
                        Ok(Value::Boolean(a > b))
                    }
                    "=" => {
                        let (a, b) = require_two_ints(&elems[1..], "=", span, env, out)?;
                        Ok(Value::Boolean(a == b))
                    }
                    "<=" => {
                        let (a, b) = require_two_ints(&elems[1..], "<=", span, env, out)?;
                        Ok(Value::Boolean(a <= b))
                    }
                    ">=" => {
                        let (a, b) = require_two_ints(&elems[1..], ">=", span, env, out)?;
                        Ok(Value::Boolean(a >= b))
                    }
                    "not" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "not requires exactly one argument"));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Value::Boolean(is_false(&val)))
                    }
                    "and" => {
                        let mut result = Value::Boolean(true);
                        for arg in &elems[1..] {
                            result = eval_expr(arg, env, out)?;
                            if is_false(&result) {
                                return Ok(result);
                            }
                        }
                        Ok(result)
                    }
                    "or" => {
                        let mut result = Value::Boolean(false);
                        for arg in &elems[1..] {
                            result = eval_expr(arg, env, out)?;
                            if !is_false(&result) {
                                return Ok(result);
                            }
                        }
                        Ok(result)
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
                                Ok(Value::List(v))
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
                            Value::List(v) if !v.is_empty() => Ok(v[0].clone()),
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
                                Ok(Value::List(v[1..].to_vec()))
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
                        Ok(Value::Boolean(
                            matches!(val, Value::List(ref v) if v.is_empty()),
                        ))
                    }
                    "list" => {
                        let mut items = Vec::new();
                        for arg in &elems[1..] {
                            items.push(eval_expr(arg, env, out)?);
                        }
                        Ok(Value::List(items))
                    }
                    "string?" => {
                        if elems.len() != 2 {
                            return Err(err_at(
                                span,
                                "string? requires exactly 1 argument",
                            ));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Value::Boolean(matches!(val, Value::Str(_))))
                    }
                    "number?" => {
                        if elems.len() != 2 {
                            return Err(err_at(
                                span,
                                "number? requires exactly 1 argument",
                            ));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Value::Boolean(matches!(val, Value::Integer(_))))
                    }
                    "boolean?" => {
                        if elems.len() != 2 {
                            return Err(err_at(
                                span,
                                "boolean? requires exactly 1 argument",
                            ));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Value::Boolean(matches!(val, Value::Boolean(_))))
                    }
                    "pair?" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "pair? requires exactly 1 argument"));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Value::Boolean(
                            matches!(val, Value::List(ref v) if !v.is_empty()),
                        ))
                    }
                    "symbol?" => {
                        if elems.len() != 2 {
                            return Err(err_at(
                                span,
                                "symbol? requires exactly 1 argument",
                            ));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        Ok(Value::Boolean(matches!(val, Value::Symbol(_))))
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
                            Value::List(v) => Ok(Value::Integer(v.len() as i64)),
                            _ => Err(err_at(span, "length: argument must be a list")),
                        }
                    }
                    "begin" => {
                        let mut result = Value::Void;
                        for arg in &elems[1..] {
                            result = eval_expr(arg, env, out)?;
                        }
                        Ok(result)
                    }
                    "let" => {
                        if elems.len() < 3 {
                            return Err(err_at(
                                span,
                                "let requires bindings and body",
                            ));
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
                        let mut result = Value::Void;
                        for expr in &elems[2..] {
                            result = eval_expr(expr, &let_env, out)?;
                        }
                        Ok(result)
                    }
                    "cond" => {
                        for clause in &elems[1..] {
                            match &clause.expr {
                                Expr::List(parts) if !parts.is_empty() => {
                                    if let Expr::Symbol(s) = &parts[0].expr {
                                        if s == "else" {
                                            let mut result = Value::Void;
                                            for expr in &parts[1..] {
                                                result = eval_expr(expr, env, out)?;
                                            }
                                            return Ok(result);
                                        }
                                    }
                                    let test = eval_expr(&parts[0], env, out)?;
                                    if !is_false(&test) {
                                        let mut result = test;
                                        for expr in &parts[1..] {
                                            result = eval_expr(expr, env, out)?;
                                        }
                                        return Ok(result);
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
                        Ok(Value::Void)
                    }
                    "display" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "display requires exactly 1 argument"));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        out.borrow_mut().push_str(&val.display_string());
                        Ok(Value::Void)
                    }
                    "write" => {
                        if elems.len() != 2 {
                            return Err(err_at(span, "write requires exactly 1 argument"));
                        }
                        let val = eval_expr(&elems[1], env, out)?;
                        out.borrow_mut().push_str(&val.to_scheme_string());
                        Ok(Value::Void)
                    }
                    "newline" => {
                        if elems.len() != 1 {
                            return Err(err_at(span, "newline takes no arguments"));
                        }
                        out.borrow_mut().push('\n');
                        Ok(Value::Void)
                    }
                    _ => apply_proc(elems, span, env, out),
                }
            } else {
                apply_proc(elems, span, env, out)
            }
        }
    }
}

fn apply_proc(elems: &[SExpr], call_span: Span, env: &EnvRef, out: &OutputBuf) -> Result<Value, EvalError> {
    let func = eval_expr(&elems[0], env, out)?;
    let args: Result<Vec<Value>, _> = elems[1..].iter().map(|a| eval_expr(a, env, out)).collect();
    let args = args?;
    match func {
        Value::Lambda(params, body, closure_env) => {
            if params.len() != args.len() {
                return Err(err_at(
                    call_span,
                    format!("expected {} arguments, got {}", params.len(), args.len()),
                ));
            }
            let call_env = EnvFrame::child(&closure_env);
            for (param, arg) in params.iter().zip(args) {
                EnvFrame::set(&call_env, param.clone(), arg);
            }
            let mut result = Value::Void;
            for expr in &body {
                result = eval_expr(expr, &call_env, out)?;
            }
            Ok(result)
        }
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
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("1:1: empty input".to_string()));
    }
    let env = EnvFrame::new();
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
