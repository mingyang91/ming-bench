pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, Default)]
struct Span {
    line: usize,
    col: usize,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Char(char),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        name: Option<String>,
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
    Void,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Char(c) => match c {
                '\n' => write!(f, "#\\newline"),
                ' ' => write!(f, "#\\space"),
                _ => write!(f, "#\\{c}"),
            },
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::Builtin(name) => write!(f, "#<procedure:{name}>"),
            Value::Void => write!(f, "#<void>"),
        }
    }
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

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    span: Span,
}

impl Expr {
    fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }
}

// --- Tokenizer ---

#[derive(Debug, Clone)]
struct Token {
    kind: TokenKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum TokenKind {
    LParen,
    RParen,
    Quote,
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;

    while i < chars.len() {
        let cur_span = Span { line, col };
        match chars[i] {
            '\n' => { line += 1; col = 1; i += 1; }
            ' ' | '\t' | '\r' => { col += 1; i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => { tokens.push(Token { kind: TokenKind::LParen, span: cur_span }); i += 1; col += 1; }
            ')' => { tokens.push(Token { kind: TokenKind::RParen, span: cur_span }); i += 1; col += 1; }
            '\'' => { tokens.push(Token { kind: TokenKind::Quote, span: cur_span }); i += 1; col += 1; }
            '"' => {
                i += 1; col += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1; col += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            c => { s.push('\\'); s.push(c); }
                        }
                    } else {
                        if chars[i] == '\n' { line += 1; col = 0; }
                        s.push(chars[i]);
                    }
                    i += 1; col += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse(format!("unterminated string at {cur_span}")));
                }
                i += 1; col += 1;
                tokens.push(Token { kind: TokenKind::Str(s), span: cur_span });
            }
            '#' => {
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push(Token { kind: TokenKind::Boolean(true), span: cur_span });
                            i += 2; col += 2;
                        }
                        'f' => {
                            tokens.push(Token { kind: TokenKind::Boolean(false), span: cur_span });
                            i += 2; col += 2;
                        }
                        '\\' => {
                            // Character literal: #\x, #\newline, #\space
                            i += 2; col += 2;
                            if i >= chars.len() {
                                return Err(EvalError::Parse(format!("unexpected end of input in character literal at {cur_span}")));
                            }
                            // Try to read a named character or single character
                            let start = i;
                            if chars[i].is_alphabetic() {
                                while i < chars.len() && chars[i].is_alphabetic() {
                                    i += 1; col += 1;
                                }
                                let name: String = chars[start..i].iter().collect();
                                let ch = if name.len() == 1 {
                                    name.chars().next().unwrap()
                                } else {
                                    match name.as_str() {
                                        "newline" => '\n',
                                        "space" => ' ',
                                        "tab" => '\t',
                                        _ => return Err(EvalError::Parse(format!("unknown character name: {name} at {cur_span}"))),
                                    }
                                };
                                tokens.push(Token { kind: TokenKind::Char(ch), span: cur_span });
                            } else {
                                tokens.push(Token { kind: TokenKind::Char(chars[i]), span: cur_span });
                                i += 1; col += 1;
                            }
                        }
                        _ => return Err(EvalError::Parse(format!("unexpected #{} at {cur_span}", chars[i + 1]))),
                    }
                } else {
                    return Err(EvalError::Parse(format!("unexpected # at {cur_span}")));
                }
            }
            c if c == '-' || c == '+' => {
                if i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                    let is_number = i == 0
                        || matches!(tokens.last().map(|t| &t.kind), Some(TokenKind::LParen) | None);
                    if is_number {
                        let start = i;
                        i += 1; col += 1;
                        while i < chars.len() && chars[i].is_ascii_digit() {
                            i += 1; col += 1;
                        }
                        let num_str: String = chars[start..i].iter().collect();
                        tokens.push(Token {
                            kind: TokenKind::Integer(num_str.parse().map_err(|_| {
                                EvalError::Parse(format!("invalid number: {num_str} at {cur_span}"))
                            })?),
                            span: cur_span,
                        });
                    } else {
                        let start = i;
                        i += 1; col += 1;
                        while i < chars.len() && is_symbol_char(chars[i]) {
                            i += 1; col += 1;
                        }
                        let sym: String = chars[start..i].iter().collect();
                        tokens.push(Token { kind: TokenKind::Symbol(sym), span: cur_span });
                    }
                } else {
                    let start = i;
                    i += 1; col += 1;
                    while i < chars.len() && is_symbol_char(chars[i]) {
                        i += 1; col += 1;
                    }
                    let sym: String = chars[start..i].iter().collect();
                    tokens.push(Token { kind: TokenKind::Symbol(sym), span: cur_span });
                }
            }
            c if c.is_ascii_digit() => {
                let start = i;
                while i < chars.len() && chars[i].is_ascii_digit() {
                    i += 1; col += 1;
                }
                let num_str: String = chars[start..i].iter().collect();
                tokens.push(Token {
                    kind: TokenKind::Integer(num_str.parse().map_err(|_| {
                        EvalError::Parse(format!("invalid number: {num_str} at {cur_span}"))
                    })?),
                    span: cur_span,
                });
            }
            c if is_symbol_start(c) => {
                let start = i;
                while i < chars.len() && is_symbol_char(chars[i]) {
                    i += 1; col += 1;
                }
                let sym: String = chars[start..i].iter().collect();
                tokens.push(Token { kind: TokenKind::Symbol(sym), span: cur_span });
            }
            c => return Err(EvalError::Parse(format!("unexpected character: {c} at {cur_span}"))),
        }
    }
    Ok(tokens)
}

fn is_symbol_start(c: char) -> bool {
    c.is_alphabetic() || "!$%&*/<=>?^_~".contains(c)
}

fn is_symbol_char(c: char) -> bool {
    is_symbol_start(c) || c.is_ascii_digit() || "+-.:@#".contains(c)
}

// --- Parser ---

fn parse(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let span = tokens[*pos].span;
    match &tokens[*pos].kind {
        TokenKind::Integer(n) => { let n = *n; *pos += 1; Ok(Expr::new(ExprKind::Integer(n), span)) }
        TokenKind::Boolean(b) => { let b = *b; *pos += 1; Ok(Expr::new(ExprKind::Boolean(b), span)) }
        TokenKind::Char(c) => { let c = *c; *pos += 1; Ok(Expr::new(ExprKind::Char(c), span)) }
        TokenKind::Str(s) => { let s = s.clone(); *pos += 1; Ok(Expr::new(ExprKind::Str(s), span)) }
        TokenKind::Symbol(s) => { let s = s.clone(); *pos += 1; Ok(Expr::new(ExprKind::Symbol(s), span)) }
        TokenKind::Quote => {
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Expr::new(ExprKind::List(vec![Expr::new(ExprKind::Symbol("quote".into()), span), inner]), span))
        }
        TokenKind::LParen => {
            *pos += 1;
            let mut list = Vec::new();
            while *pos < tokens.len() && !matches!(tokens[*pos].kind, TokenKind::RParen) {
                list.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse(format!("missing closing paren at {span}")));
            }
            *pos += 1;
            Ok(Expr::new(ExprKind::List(list), span))
        }
        TokenKind::RParen => Err(EvalError::Parse(format!("unexpected ) at {span}"))),
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// --- Environment ---

#[derive(Debug, Clone)]
struct Env {
    bindings: Rc<RefCell<HashMap<String, Value>>>,
    parent: Option<Box<Env>>,
}

impl Env {
    fn new() -> Self {
        Env { bindings: Rc::new(RefCell::new(HashMap::new())), parent: None }
    }

    fn with_parent(parent: Env) -> Self {
        Env { bindings: Rc::new(RefCell::new(HashMap::new())), parent: Some(Box::new(parent)) }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.bindings.borrow().get(name) {
            Some(v.clone())
        } else if let Some(ref parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    fn set(&self, name: String, val: Value) {
        self.bindings.borrow_mut().insert(name, val);
    }

    fn set_existing(&self, name: &str, val: Value) -> bool {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), val);
            true
        } else if let Some(ref parent) = self.parent {
            parent.set_existing(name, val)
        } else {
            false
        }
    }
}

fn default_env() -> Env {
    let env = Env::new();
    for name in [
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not", "and", "or",
        "cons", "car", "cdr", "null?", "list", "length", "append",
        "string?", "number?", "boolean?", "pair?", "symbol?", "char?",
        "display", "write", "newline",
        "string-append", "string-length", "substring",
        "string->number", "number->string",
        "symbol->string", "string->symbol",
        "string-ref", "string-copy",
    ] {
        env.set(name.into(), Value::Builtin(name.into()));
    }
    env
}

// --- Evaluator ---

fn display_value(v: &Value, out: &mut String) {
    match v {
        Value::Str(s) => out.push_str(s),
        Value::List(elems) => {
            out.push('(');
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { out.push(' '); }
                display_value(e, out);
            }
            out.push(')');
        }
        other => out.push_str(&other.to_string()),
    }
}

fn eval(expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let span = expr.span;
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Symbol(s) => {
            env.get(s).ok_or_else(|| EvalError::Unbound(format!("{s} at {span}")))
        }
        ExprKind::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Syntax(format!("empty application at {span}")));
            }

            // Check for special forms
            if let ExprKind::Symbol(head) = &elems[0].kind {
                match head.as_str() {
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Syntax(format!("quote requires 1 argument at {span}")));
                        }
                        return Ok(expr_to_value(&elems[1]));
                    }
                    "if" => {
                        if elems.len() < 3 || elems.len() > 4 {
                            return Err(EvalError::Syntax(format!("if requires 2 or 3 arguments at {span}")));
                        }
                        let cond = eval(&elems[1], env, output)?;
                        if is_truthy(&cond) {
                            return eval(&elems[2], env, output);
                        } else if elems.len() == 4 {
                            return eval(&elems[3], env, output);
                        } else {
                            return Ok(Value::Void);
                        }
                    }
                    "define" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("define requires at least 2 arguments at {span}")));
                        }
                        match &elems[1].kind {
                            ExprKind::Symbol(name) => {
                                let val = eval(&elems[2], env, output)?;
                                env.set(name.clone(), val);
                                return Ok(Value::Void);
                            }
                            ExprKind::List(sig) => {
                                // (define (f x y) body...)
                                if sig.is_empty() {
                                    return Err(EvalError::Syntax(format!("define: empty signature at {span}")));
                                }
                                let name = match &sig[0].kind {
                                    ExprKind::Symbol(s) => s.clone(),
                                    _ => return Err(EvalError::Syntax(format!("define: expected function name at {span}"))),
                                };
                                let params: Vec<String> = sig[1..].iter().map(|e| match &e.kind {
                                    ExprKind::Symbol(s) => Ok(s.clone()),
                                    _ => Err(EvalError::Syntax(format!("define: expected parameter name at {span}"))),
                                }).collect::<Result<_, _>>()?;
                                let body = elems[2..].to_vec();
                                let lambda = Value::Lambda {
                                    name: Some(name.clone()),
                                    params,
                                    body,
                                    env: env.clone(),
                                };
                                env.set(name, lambda);
                                return Ok(Value::Void);
                            }
                            _ => return Err(EvalError::Syntax(format!("define: expected symbol or list at {span}"))),
                        }
                    }
                    "lambda" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("lambda requires params and body at {span}")));
                        }
                        let params = match &elems[1].kind {
                            ExprKind::List(ps) => {
                                ps.iter().map(|e| match &e.kind {
                                    ExprKind::Symbol(s) => Ok(s.clone()),
                                    _ => Err(EvalError::Syntax(format!("lambda: expected parameter name at {span}"))),
                                }).collect::<Result<Vec<_>, _>>()?
                            }
                            _ => return Err(EvalError::Syntax(format!("lambda: expected parameter list at {span}"))),
                        };
                        let body = elems[2..].to_vec();
                        return Ok(Value::Lambda {
                            name: None,
                            params,
                            body,
                            env: env.clone(),
                        });
                    }
                    "and" => {
                        if elems.len() == 1 {
                            return Ok(Value::Boolean(true));
                        }
                        let args = &elems[1..];
                        let mut result = Value::Boolean(true);
                        for a in args {
                            result = eval(a, env, output)?;
                            if !is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        if elems.len() == 1 {
                            return Ok(Value::Boolean(false));
                        }
                        let args = &elems[1..];
                        let mut result = Value::Boolean(false);
                        for a in args {
                            result = eval(a, env, output)?;
                            if is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "let" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax(format!("let requires bindings and body at {span}")));
                        }
                        let (name, bindings_expr, body_start) = match &elems[1].kind {
                            ExprKind::Symbol(n) => {
                                if elems.len() < 4 {
                                    return Err(EvalError::Syntax(format!("named let requires bindings and body at {span}")));
                                }
                                (Some(n.clone()), &elems[2], 3)
                            }
                            ExprKind::List(_) => (None, &elems[1], 2),
                            _ => return Err(EvalError::Syntax(format!("let: expected bindings list at {span}"))),
                        };
                        let bindings_list = match &bindings_expr.kind {
                            ExprKind::List(bs) => bs,
                            _ => return Err(EvalError::Syntax(format!("let: expected bindings list at {span}"))),
                        };
                        let mut params = Vec::new();
                        let mut init_vals = Vec::new();
                        for b in bindings_list {
                            match &b.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    match &pair[0].kind {
                                        ExprKind::Symbol(s) => {
                                            params.push(s.clone());
                                            init_vals.push(eval(&pair[1], env, output)?);
                                        }
                                        _ => return Err(EvalError::Syntax(format!("let: expected variable name at {span}"))),
                                    }
                                }
                                _ => return Err(EvalError::Syntax(format!("let: bad binding at {span}"))),
                            }
                        }
                        let body = elems[body_start..].to_vec();
                        if let Some(loop_name) = name {
                            let lambda = Value::Lambda {
                                name: Some(loop_name.clone()),
                                params: params.clone(),
                                body,
                                env: env.clone(),
                            };
                            return apply_func(&lambda, &init_vals, span, output);
                        }
                        let mut local_env = Env::with_parent(env.clone());
                        for (p, v) in params.iter().zip(init_vals.iter()) {
                            local_env.set(p.clone(), v.clone());
                        }
                        let mut result = Value::Void;
                        for expr in &elems[body_start..] {
                            result = eval(expr, &mut local_env, output)?;
                        }
                        return Ok(result);
                    }
                    "begin" => {
                        let mut result = Value::Void;
                        for expr in &elems[1..] {
                            result = eval(expr, env, output)?;
                        }
                        return Ok(result);
                    }
                    "cond" => {
                        for clause in &elems[1..] {
                            match &clause.kind {
                                ExprKind::List(parts) if !parts.is_empty() => {
                                    if let ExprKind::Symbol(s) = &parts[0].kind {
                                        if s == "else" {
                                            let mut result = Value::Void;
                                            for expr in &parts[1..] {
                                                result = eval(expr, env, output)?;
                                            }
                                            return Ok(result);
                                        }
                                    }
                                    let test = eval(&parts[0], env, output)?;
                                    if is_truthy(&test) {
                                        let mut result = test;
                                        for expr in &parts[1..] {
                                            result = eval(expr, env, output)?;
                                        }
                                        return Ok(result);
                                    }
                                }
                                _ => return Err(EvalError::Syntax(format!("cond: bad clause at {span}"))),
                            }
                        }
                        return Ok(Value::Void);
                    }
                    "set!" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Syntax(format!("set! requires 2 arguments at {span}")));
                        }
                        let var_name = match &elems[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Syntax(format!("set!: expected variable name at {span}"))),
                        };
                        let val = eval(&elems[2], env, output)?;
                        if !env.set_existing(&var_name, val) {
                            return Err(EvalError::Unbound(format!("{var_name} at {span}")));
                        }
                        return Ok(Value::Void);
                    }
                    "string-set!" => {
                        if elems.len() != 4 {
                            return Err(EvalError::Syntax(format!("string-set! requires 3 arguments at {span}")));
                        }
                        let var_name = match &elems[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Type(format!("string-set!: first argument must be a variable at {span}"))),
                        };
                        let idx_val = eval(&elems[2], env, output)?;
                        let idx = match &idx_val {
                            Value::Integer(n) => *n as usize,
                            _ => return Err(EvalError::Type(format!("string-set!: index must be integer at {span}"))),
                        };
                        let char_val = eval(&elems[3], env, output)?;
                        let ch = match &char_val {
                            Value::Char(c) => *c,
                            _ => return Err(EvalError::Type(format!("string-set!: third argument must be char at {span}"))),
                        };
                        let s = env.get(&var_name).ok_or_else(|| EvalError::Unbound(format!("{var_name} at {span}")))?;
                        let mut chars: Vec<char> = match &s {
                            Value::Str(st) => st.chars().collect(),
                            _ => return Err(EvalError::Type(format!("string-set!: not a string at {span}"))),
                        };
                        if idx >= chars.len() {
                            return Err(EvalError::Type(format!("string-set!: index out of range at {span}")));
                        }
                        chars[idx] = ch;
                        let new_str: String = chars.into_iter().collect();
                        env.set(var_name, Value::Str(new_str));
                        return Ok(Value::Void);
                    }
                    _ => {}
                }
            }

            // Function application
            let func = eval(&elems[0], env, output)?;
            let args: Vec<Value> = elems[1..].iter()
                .map(|a| eval(a, env, output))
                .collect::<Result<_, _>>()?;

            apply_func(&func, &args, span, output)
        }
    }
}

fn apply_func(func: &Value, args: &[Value], call_span: Span, output: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(name) => apply_builtin(name, args, call_span, output),
        Value::Lambda { name, params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {} at {call_span}", params.len(), args.len()
                )));
            }
            let mut local_env = Env::with_parent(env.clone());
            if let Some(n) = name {
                local_env.set(n.clone(), func.clone());
            }
            for (p, a) in params.iter().zip(args.iter()) {
                local_env.set(p.clone(), a.clone());
            }
            let mut define_exprs = Vec::new();
            let mut rest_exprs = Vec::new();
            let mut in_defines = true;
            for expr in body {
                if in_defines {
                    if let ExprKind::List(elems) = &expr.kind {
                        if let Some(first) = elems.first() {
                            if let ExprKind::Symbol(s) = &first.kind {
                                if s == "define" {
                                    define_exprs.push(expr);
                                    continue;
                                }
                            }
                        }
                    }
                    in_defines = false;
                }
                rest_exprs.push(expr);
            }
            for expr in &define_exprs {
                eval(expr, &mut local_env, output)?;
            }
            if define_exprs.len() > 1 {
                let define_names: Vec<String> = define_exprs.iter().filter_map(|expr| {
                    if let ExprKind::List(elems) = &expr.kind {
                        match &elems[1].kind {
                            ExprKind::Symbol(n) => Some(n.clone()),
                            ExprKind::List(sig) if !sig.is_empty() => {
                                if let ExprKind::Symbol(n) = &sig[0].kind { Some(n.clone()) } else { None }
                            }
                            _ => None,
                        }
                    } else { None }
                }).collect();
                let final_bindings: Vec<(String, Value)> = define_names.iter()
                    .filter_map(|n| local_env.bindings.borrow().get(n).map(|v| (n.clone(), v.clone())))
                    .collect();
                // Extract lambdas, modify them outside the borrow, then put them back
                for dn in &define_names {
                    let mut val = local_env.bindings.borrow_mut().remove(dn);
                    if let Some(Value::Lambda { env: ref mut closure_env, .. }) = val {
                        for (sib_name, sib_val) in &final_bindings {
                            closure_env.set(sib_name.clone(), sib_val.clone());
                        }
                    }
                    if let Some(v) = val {
                        local_env.bindings.borrow_mut().insert(dn.clone(), v);
                    }
                }
            }
            let mut result = Value::Void;
            for expr in &rest_exprs {
                result = eval(expr, &mut local_env, output)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type(format!("not a procedure: {func} at {call_span}"))),
    }
}

fn apply_builtin(name: &str, args: &[Value], call_span: Span, output: &mut String) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_int(a, call_span)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("- requires at least 1 argument at {call_span}")));
            }
            if args.len() == 1 {
                Ok(Value::Integer(-as_int(&args[0], call_span)?))
            } else {
                let mut result = as_int(&args[0], call_span)?;
                for a in &args[1..] {
                    result -= as_int(a, call_span)?;
                }
                Ok(Value::Integer(result))
            }
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_int(a, call_span)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("/ requires at least 1 argument at {call_span}")));
            }
            let mut result = as_int(&args[0], call_span)?;
            for a in &args[1..] {
                let divisor = as_int(a, call_span)?;
                if divisor == 0 {
                    return Err(EvalError::DivisionByZero(call_span.to_string()));
                }
                result /= divisor;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            let vals = args_to_ints(args, call_span)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] < w[1])))
        }
        ">" => {
            let vals = args_to_ints(args, call_span)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] > w[1])))
        }
        "=" => {
            let vals = args_to_ints(args, call_span)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] == w[1])))
        }
        "<=" => {
            let vals = args_to_ints(args, call_span)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] <= w[1])))
        }
        ">=" => {
            let vals = args_to_ints(args, call_span)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] >= w[1])))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("not requires 1 argument at {call_span}")));
            }
            Ok(Value::Boolean(!is_truthy(&args[0])))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("cons requires 2 arguments at {call_span}")));
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
                return Err(EvalError::Arity(format!("car requires 1 argument at {call_span}")));
            }
            match &args[0] {
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(EvalError::Type(format!("car: not a pair at {call_span}"))),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("cdr requires 1 argument at {call_span}")));
            }
            match &args[0] {
                Value::List(elems) if !elems.is_empty() => {
                    Ok(Value::List(elems[1..].to_vec()))
                }
                _ => Err(EvalError::Type(format!("cdr: not a pair at {call_span}"))),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("null? requires 1 argument at {call_span}")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(v) if v.is_empty())))
        }
        "list" => {
            Ok(Value::List(args.to_vec()))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("length requires 1 argument at {call_span}")));
            }
            match &args[0] {
                Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                _ => Err(EvalError::Type(format!("length: not a list at {call_span}"))),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for (i, arg) in args.iter().enumerate() {
                if i < args.len() - 1 {
                    match arg {
                        Value::List(elems) => result.extend(elems.iter().cloned()),
                        _ => return Err(EvalError::Type(format!("append: not a list at {call_span}"))),
                    }
                } else {
                    match arg {
                        Value::List(elems) => result.extend(elems.iter().cloned()),
                        _ => result.push(arg.clone()),
                    }
                }
            }
            Ok(Value::List(result))
        }
        "string?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "number?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("number? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("boolean? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("pair? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(v) if !v.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("symbol? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "char?" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("char? requires 1 argument at {call_span}"))); }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "display" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("display requires 1 argument at {call_span}"))); }
            display_value(&args[0], output);
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("write requires 1 argument at {call_span}"))); }
            output.push_str(&args[0].to_string());
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() { return Err(EvalError::Arity(format!("newline requires 0 arguments at {call_span}"))); }
            output.push('\n');
            Ok(Value::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type(format!("string-append: not a string at {call_span}"))),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string-length requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.chars().count() as i64)),
                _ => Err(EvalError::Type(format!("string-length: not a string at {call_span}"))),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity(format!("substring requires 3 arguments at {call_span}"))); }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type(format!("substring: not a string at {call_span}"))),
            };
            let start = as_int(&args[1], call_span)? as usize;
            let end = as_int(&args[2], call_span)? as usize;
            let chars: Vec<char> = s.chars().collect();
            if end > chars.len() || start > end {
                return Err(EvalError::Type(format!("substring: index out of range at {call_span}")));
            }
            Ok(Value::Str(chars[start..end].iter().collect()))
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string->number requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type(format!("string->number: not a string at {call_span}"))),
            }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("number->string requires 1 argument at {call_span}"))); }
            let n = as_int(&args[0], call_span)?;
            Ok(Value::Str(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("symbol->string requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!("symbol->string: not a symbol at {call_span}"))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string->symbol requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type(format!("string->symbol: not a string at {call_span}"))),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("string-ref requires 2 arguments at {call_span}"))); }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type(format!("string-ref: not a string at {call_span}"))),
            };
            let idx = as_int(&args[1], call_span)? as usize;
            let chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type(format!("string-ref: index out of range at {call_span}")));
            }
            Ok(Value::Char(chars[idx]))
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity(format!("string-copy requires 1 argument at {call_span}"))); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!("string-copy: not a string at {call_span}"))),
            }
        }
        _ => Err(EvalError::Unbound(format!("{name} at {call_span}"))),
    }
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(elems) => Value::List(elems.iter().map(expr_to_value).collect()),
    }
}

fn as_int(v: &Value, span: Span) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected integer, got {v} at {span}"))),
    }
}

fn args_to_ints(args: &[Value], span: Span) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|a| as_int(a, span)).collect()
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let mut env = default_env();
    let mut output = String::new();
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &mut env, &mut output)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let mut env = default_env();
    let mut output = String::new();
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &mut env, &mut output)?;
    }
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
