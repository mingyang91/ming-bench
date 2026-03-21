pub mod error;

pub use error::EvalError;
use error::Span;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

thread_local! {
    static OUTPUT: RefCell<String> = RefCell::new(String::new());
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
        body: Vec<Value>,
        env: Env,
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
            | Value::List(_, s) => *s,
            Value::Lambda { span, .. } => *span,
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
            Value::Lambda { .. } => "#<procedure>".to_string(),
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

fn env_set_existing(env: &Env, name: &str, val: Value) {
    let mut inner = env.borrow_mut();
    if inner.bindings.contains_key(name) {
        inner.bindings.insert(name.to_string(), val);
    } else if let Some(ref parent) = inner.parent {
        env_set_existing(parent, name, val);
    }
}

fn default_env() -> Env {
    new_env(None)
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
    match expr {
        Value::Integer(..) | Value::Boolean(..) | Value::Str(..) | Value::Char(..) => Ok(expr.clone()),
        Value::Symbol(name, span) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable(name.clone(), *span))
        }
        Value::List(elems, span) => {
            let form_span = *span;
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".into(), form_span));
            }
            if let Value::Symbol(op, _) = &elems[0] {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], env, form_span),
                    "if" => return eval_if(&elems[1..], env, form_span),
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
                    "lambda" => return eval_lambda(&elems[1..], env, form_span),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "let" => return eval_let(&elems[1..], env, form_span),
                    "begin" => return eval_begin(&elems[1..], env),
                    "cond" => return eval_cond(&elems[1..], env),
                    "string-set!" => return eval_string_set(&elems[1..], env, form_span),
                    "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not" | "cons"
                    | "car" | "cdr" | "null?" | "list" | "length" | "string?" | "number?"
                    | "boolean?" | "pair?" | "symbol?" | "char?"
                    | "display" | "write" | "newline"
                    | "string-append" | "string-length" | "substring"
                    | "string->number" | "number->string"
                    | "symbol->string" | "string->symbol" | "string-ref"
                    | "string-copy" => {
                        return eval_builtin(op, &elems[1..], env, form_span);
                    }
                    _ => {}
                }
            }
            // General function application
            let func = eval(&elems[0], env)?;
            let args: Vec<Value> =
                elems[1..].iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
            apply_function(&func, &args, form_span)
        }
        Value::Void => Ok(Value::Void),
        Value::Lambda { .. } => Ok(expr.clone()),
    }
}

fn apply_function(func: &Value, args: &[Value], call_span: Span) -> Result<Value, EvalError> {
    match func {
        Value::Lambda {
            params, body, env, ..
        } => {
            if args.len() != params.len() {
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
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::NotAProcedure(
            func.display_scheme(),
            call_span,
        )),
    }
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
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|p| match p {
                    Value::Symbol(s, _) => Ok(s.clone()),
                    _ => Err(EvalError::Parse(
                        "define: expected symbol in params".into(),
                        form_span,
                    )),
                })
                .collect::<Result<_, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
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

fn eval_if(args: &[Value], env: &Env, form_span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Parse(
            "if: expected 2 or 3 arguments".into(),
            form_span,
        ));
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

fn eval_lambda(args: &[Value], env: &Env, form_span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse(
            "lambda: expected params and body".into(),
            form_span,
        ));
    }
    let params = match &args[0] {
        Value::List(elems, _) => elems
            .iter()
            .map(|p| match p {
                Value::Symbol(s, _) => Ok(s.clone()),
                _ => Err(EvalError::Parse(
                    "lambda: expected symbol in params".into(),
                    form_span,
                )),
            })
            .collect::<Result<Vec<_>, _>>()?,
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
        body,
        env: env.clone(),
        span: form_span,
    })
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
        _ => Err(EvalError::UnboundVariable(op.to_string(), sp)),
    }
}

fn eval_string_set(args: &[Value], env: &Env, form_span: Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            expected: "3".into(),
            got: args.len(),
            at: form_span,
        });
    }
    let var_name = match &args[0] {
        Value::Symbol(name, _) => name.clone(),
        _ => return Err(EvalError::TypeError("string-set!: first argument must be a variable".into(), form_span)),
    };
    let idx = expect_integer(&eval(&args[1], env)?)? as usize;
    let ch = match eval(&args[2], env)? {
        Value::Char(c, _) => c,
        _ => return Err(EvalError::TypeError("string-set!: third argument must be a character".into(), form_span)),
    };
    let current = env_get(env, &var_name)
        .ok_or_else(|| EvalError::UnboundVariable(var_name.clone(), form_span))?;
    match current {
        Value::Str(s, s_span) => {
            let mut chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::TypeError("string-set!: index out of range".into(), form_span));
            }
            chars[idx] = ch;
            let new_str: String = chars.into_iter().collect();
            env_set_existing(env, &var_name, Value::Str(new_str, s_span));
            Ok(Value::Void)
        }
        _ => Err(EvalError::TypeError("string-set!: expected string".into(), form_span)),
    }
}

fn eval_and(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(true, Span::default()));
    }
    let mut result = Value::Boolean(true, Span::default());
    for a in args {
        result = eval(a, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(false, Span::default()));
    }
    let mut result = Value::Boolean(false, Span::default());
    for a in args {
        result = eval(a, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_let(args: &[Value], env: &Env, form_span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse(
            "let: expected bindings and body".into(),
            form_span,
        ));
    }
    let bindings = match &args[0] {
        Value::List(b, _) => b,
        _ => {
            return Err(EvalError::Parse(
                "let: expected bindings list".into(),
                form_span,
            ))
        }
    };
    let local_env = new_env(Some(env.clone()));
    for binding in bindings {
        match binding {
            Value::List(pair, _) if pair.len() == 2 => {
                let name = match &pair[0] {
                    Value::Symbol(s, _) => s.clone(),
                    _ => {
                        return Err(EvalError::Parse(
                            "let: expected symbol in binding".into(),
                            form_span,
                        ))
                    }
                };
                let val = eval(&pair[1], env)?;
                env_set(&local_env, name, val);
            }
            _ => {
                return Err(EvalError::Parse(
                    "let: invalid binding".into(),
                    form_span,
                ))
            }
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    for clause in args {
        match clause {
            Value::List(elems, _) if elems.len() >= 2 => {
                if matches!(&elems[0], Value::Symbol(s, _) if s == "else") {
                    let mut result = Value::Void;
                    for expr in &elems[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
                let test = eval(&elems[0], env)?;
                if test.is_truthy() {
                    let mut result = Value::Void;
                    for expr in &elems[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => {
                return Err(EvalError::Parse(
                    "cond: invalid clause".into(),
                    clause.span(),
                ))
            }
        }
    }
    Ok(Value::Void)
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
    let exprs = parse_all(input)?;
    let env = default_env();
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    Ok(last.display_scheme())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    OUTPUT.with(|o| o.borrow_mut().clear());
    let exprs = parse_all(input)?;
    let env = default_env();
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    let output = OUTPUT.with(|o| o.borrow().clone());
    Ok((last.display_scheme(), output))
}

#[cfg(test)]
mod tests;
