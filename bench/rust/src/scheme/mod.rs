pub mod error;
mod builtins;

pub use error::EvalError;
use builtins::apply_builtin;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Char(char),
    Builtin(String),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    Void,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{s}\""),
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
            Value::Pair(a, d) => {
                write!(f, "({a}")?;
                let mut cur = d.as_ref();
                loop {
                    match cur {
                        Value::Pair(ca, cd) => {
                            write!(f, " {ca}")?;
                            cur = cd.as_ref();
                        }
                        Value::List(elems) if elems.is_empty() => break,
                        other => {
                            write!(f, " . {other}")?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Value::Char(c) => match c {
                ' ' => write!(f, "#\\space"),
                '\n' => write!(f, "#\\newline"),
                '\t' => write!(f, "#\\tab"),
                _ => write!(f, "#\\{c}"),
            },
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::Builtin(name) => write!(f, "#<builtin:{name}>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
            Value::Void => write!(f, ""),
        }
    }
}

impl Value {
    /// Format for `display` — strings without quotes.
    fn display_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Str(s) => write!(f, "{s}"),
            Value::Char(c) => write!(f, "{c}"),
            Value::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    e.display_fmt(f)?;
                }
                write!(f, ")")
            }
            Value::Pair(a, d) => {
                write!(f, "(")?;
                a.display_fmt(f)?;
                let mut cur = d.as_ref();
                loop {
                    match cur {
                        Value::Pair(ca, cd) => {
                            write!(f, " ")?;
                            ca.display_fmt(f)?;
                            cur = cd.as_ref();
                        }
                        Value::List(elems) if elems.is_empty() => break,
                        other => {
                            write!(f, " . ")?;
                            other.display_fmt(f)?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Value::Macro { .. } => write!(f, "#<macro>"),
            other => fmt::Display::fmt(other, f),
        }
    }
}

struct DisplayValue<'a>(&'a Value);
impl<'a> fmt::Display for DisplayValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.display_fmt(f)
    }
}

// ---------- Environment ----------

type Env = Rc<RefCell<EnvInner>>;

struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl fmt::Debug for EnvInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Env")
            .field("bindings", &self.bindings.keys().collect::<Vec<_>>())
            .finish()
    }
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

fn env_update(env: &Env, name: &str, val: Value) -> bool {
    let mut inner = env.borrow_mut();
    if inner.bindings.contains_key(name) {
        inner.bindings.insert(name.to_string(), val);
        true
    } else if let Some(ref parent) = inner.parent {
        env_update(parent, name, val)
    } else {
        false
    }
}

fn default_env() -> Env {
    let env = new_env(None);
    for &name in BUILTINS {
        env_set(&env, name.to_string(), Value::Builtin(name.to_string()));
    }
    env
}

// ---------- Source Position ----------

#[derive(Debug, Clone, Copy, PartialEq)]
struct Pos {
    line: usize,
    col: usize,
}

impl Pos {
    fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// ---------- Tokenizer ----------

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

#[derive(Debug, Clone)]
struct SpannedToken {
    token: Token,
    pos: Pos,
}

/// Parse a character literal starting at `pos` in `chars`.
/// Returns the char and how many characters were consumed.
fn parse_char_literal(chars: &[char], pos: usize, start_pos: Pos) -> Result<(char, usize), EvalError> {
    if pos >= chars.len() {
        return Err(EvalError::Parse(format!("{start_pos}: incomplete character literal")));
    }
    if !chars[pos].is_alphabetic() {
        return Ok((chars[pos], 1));
    }
    let mut end = pos;
    while end < chars.len() && chars[end].is_alphabetic() {
        end += 1;
    }
    let name: String = chars[pos..end].iter().collect();
    let consumed = end - pos;
    if consumed == 1 {
        return Ok((chars[pos], 1));
    }
    let ch = match name.as_str() {
        "space" => ' ',
        "newline" => '\n',
        "tab" => '\t',
        _ => return Err(EvalError::Parse(format!("{start_pos}: unknown character name: {name}"))),
    };
    Ok((ch, consumed))
}

fn tokenize(input: &str) -> Result<Vec<SpannedToken>, EvalError> {
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
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => {
                tokens.push(SpannedToken { token: Token::LParen, pos: Pos::new(line, col) });
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push(SpannedToken { token: Token::RParen, pos: Pos::new(line, col) });
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push(SpannedToken { token: Token::Quote, pos: Pos::new(line, col) });
                i += 1;
                col += 1;
            }
            '"' => {
                let start_pos = Pos::new(line, col);
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
                    return Err(EvalError::Parse(format!("{start_pos}: unterminated string")));
                }
                i += 1;
                col += 1;
                tokens.push(SpannedToken { token: Token::Str(s), pos: start_pos });
            }
            '#' => {
                let start_pos = Pos::new(line, col);
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push(SpannedToken { token: Token::Boolean(true), pos: start_pos });
                            i += 2;
                            col += 2;
                        }
                        'f' => {
                            tokens.push(SpannedToken { token: Token::Boolean(false), pos: start_pos });
                            i += 2;
                            col += 2;
                        }
                        '\\' => {
                            let (ch, advance) = parse_char_literal(&chars, i + 2, start_pos)?;
                            i += 2 + advance;
                            col += 2 + advance;
                            tokens.push(SpannedToken { token: Token::Char(ch), pos: start_pos });
                        }
                        _ => {
                            return Err(EvalError::Parse(format!(
                                "{start_pos}: unexpected character after #: {}",
                                chars[i + 1]
                            )));
                        }
                    }
                } else {
                    return Err(EvalError::Parse(format!("{start_pos}: unexpected #")));
                }
            }
            _ => {
                let start_pos = Pos::new(line, col);
                let start = i;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"' | '\'')
                {
                    i += 1;
                    col += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push(SpannedToken { token: Token::Integer(n), pos: start_pos });
                } else {
                    tokens.push(SpannedToken { token: Token::Symbol(word), pos: start_pos });
                }
            }
        }
    }
    Ok(tokens)
}

// ---------- Parser ----------

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64, Pos),
    Boolean(bool, Pos),
    Str(String, Pos),
    Symbol(String, Pos),
    Char(char, Pos),
    List(Vec<Expr>, Pos),
}

impl Expr {
    fn pos(&self) -> Pos {
        match self {
            Expr::Integer(_, p)
            | Expr::Boolean(_, p)
            | Expr::Str(_, p)
            | Expr::Symbol(_, p)
            | Expr::Char(_, p)
            | Expr::List(_, p) => *p,
        }
    }
}

fn parse(tokens: &[SpannedToken], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let st = &tokens[*pos];
    let src_pos = st.pos;
    match &st.token {
        Token::Integer(n) => {
            let n = *n;
            *pos += 1;
            Ok(Expr::Integer(n, src_pos))
        }
        Token::Boolean(b) => {
            let b = *b;
            *pos += 1;
            Ok(Expr::Boolean(b, src_pos))
        }
        Token::Str(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::Str(s, src_pos))
        }
        Token::Symbol(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::Symbol(s, src_pos))
        }
        Token::Char(c) => {
            let c = *c;
            *pos += 1;
            Ok(Expr::Char(c, src_pos))
        }
        Token::Quote => {
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Expr::List(vec![Expr::Symbol("quote".into(), src_pos), inner], src_pos))
        }
        Token::LParen => {
            *pos += 1;
            let mut elems = Vec::new();
            while *pos < tokens.len() && tokens[*pos].token != Token::RParen {
                elems.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse(format!("{src_pos}: missing closing paren")));
            }
            *pos += 1;
            Ok(Expr::List(elems, src_pos))
        }
        Token::RParen => Err(EvalError::Parse(format!("{src_pos}: unexpected )"))),
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

// ---------- Evaluator ----------

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

const BUILTINS: &[&str] = &[
    "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
    "cons", "car", "cdr", "null?", "list", "length", "append",
    "number?", "boolean?", "string?", "pair?", "symbol?",
    "display", "write", "newline",
    "string-append", "string-length", "substring",
    "string->number", "number->string",
    "symbol->string", "string->symbol",
    "string-ref", "char?", "string-copy",
    "apply", "eq?", "equal?", "map",
    "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
    "zero?", "positive?", "negative?", "odd?", "even?",
    "list-ref", "list-tail", "list?", "assoc",
    "char=?", "char<?", "char-alphabetic?", "char-numeric?",
    "char-upcase", "char-downcase",
    "string=?", "string<?", "string-ci=?",
    "string-upcase", "string-downcase",
];

fn eval(expr: &Expr, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let p = expr.pos();
    match expr {
        Expr::Integer(n, _) => Ok(Value::Integer(*n)),
        Expr::Boolean(b, _) => Ok(Value::Boolean(*b)),
        Expr::Str(s, _) => Ok(Value::Str(s.clone())),
        Expr::Char(c, _) => Ok(Value::Char(*c)),
        Expr::Symbol(name, _) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable(format!("{p}: {name}")))
        }
        Expr::List(elems, _) => {
            if elems.is_empty() {
                return Ok(Value::List(vec![]));
            }
            // Check for special forms
            if let Expr::Symbol(op, _) = &elems[0] {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], p, env, output),
                    "if" => return eval_if(&elems[1..], p, env, output),
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity(format!("{p}: quote expects 1 argument")));
                        }
                        return Ok(expr_to_value(&elems[1]));
                    }
                    "lambda" => return eval_lambda(&elems[1..], p, env),
                    "let" => return eval_let(&elems[1..], p, env, output),
                    "begin" => return eval_begin(&elems[1..], env, output),
                    "cond" => return eval_cond(&elems[1..], env, output),
                    "and" => return eval_and(&elems[1..], env, output),
                    "or" => return eval_or(&elems[1..], env, output),
                    "set!" => return eval_set_bang(&elems[1..], p, env, output),
                    "string-set!" => return eval_string_set(&elems[1..], p, env, output),
                    "not" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity(format!("{p}: not expects 1 argument")));
                        }
                        let val = eval(&elems[1], env, output)?;
                        return Ok(Value::Boolean(!is_truthy(&val)));
                    }
                    "define-syntax" => return eval_define_syntax(&elems[1..], p, env),
                    _ => {
                        // Check for macro invocation
                        if let Some(Value::Macro { literals, rules, def_env }) = env_get(env, op) {
                            let (expanded, hygiene_bindings) = expand_macro(&literals, &rules, elems, p, &def_env)?;
                            if hygiene_bindings.is_empty() {
                                return eval(&expanded, env, output);
                            }
                            let hyg_env = new_env(Some(env.clone()));
                            for (name, val) in hygiene_bindings {
                                env_set(&hyg_env, name, val);
                            }
                            return eval(&expanded, &hyg_env, output);
                        }
                    }
                }
            }
            // Function call
            let func = eval(&elems[0], env, output)?;
            let args: Vec<Value> = elems[1..]
                .iter()
                .map(|e| eval(e, env, output))
                .collect::<Result<_, _>>()?;
            apply_func(&func, &args, p, output)
        }
    }
}

fn expr_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(n, _) => Value::Integer(*n),
        Expr::Boolean(b, _) => Value::Boolean(*b),
        Expr::Str(s, _) => Value::Str(s.clone()),
        Expr::Symbol(s, _) => Value::Symbol(s.clone()),
        Expr::Char(c, _) => Value::Char(*c),
        Expr::List(elems, _) => Value::List(elems.iter().map(expr_to_value).collect()),
    }
}

fn eval_define(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!("{call_pos}: define requires at least 2 arguments")));
    }
    match &args[0] {
        // (define x expr)
        Expr::Symbol(name, _) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: define requires exactly 2 arguments")));
            }
            let val = eval(&args[1], env, output)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...) or (define (f params... . rest) body...)
        Expr::List(sig, _) => {
            if sig.is_empty() {
                return Err(EvalError::Parse(format!("{call_pos}: define: empty signature")));
            }
            let name = match &sig[0] {
                Expr::Symbol(s, _) => s.clone(),
                _ => return Err(EvalError::Parse(format!("{call_pos}: define: expected function name"))),
            };
            let (params, rest_param) = parse_params(&sig[1..], call_pos)?;
            let body = args[1..].to_vec();
            if body.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: define: empty body")));
            }
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                env: env.clone(),
            };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse(format!("{call_pos}: define: expected symbol or list"))),
    }
}

fn eval_if(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity(format!("{call_pos}: if requires 2 or 3 arguments")));
    }
    let cond = eval(&args[0], env, output)?;
    if is_truthy(&cond) {
        eval(&args[1], env, output)
    } else if args.len() == 3 {
        eval(&args[2], env, output)
    } else {
        Ok(Value::Void)
    }
}

fn parse_params(param_exprs: &[Expr], call_pos: Pos) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i] {
            Expr::Symbol(s, _) if s == "." => {
                if i + 1 >= param_exprs.len() {
                    return Err(EvalError::Parse(format!("{call_pos}: expected rest parameter after .")));
                }
                rest_param = Some(match &param_exprs[i + 1] {
                    Expr::Symbol(s, _) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("{call_pos}: expected symbol for rest parameter"))),
                });
                break;
            }
            Expr::Symbol(s, _) => params.push(s.clone()),
            _ => return Err(EvalError::Parse(format!("{call_pos}: expected parameter name"))),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_lambda(args: &[Expr], call_pos: Pos, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{call_pos}: lambda requires params and body")));
    }
    let (params, rest_param) = match &args[0] {
        Expr::List(elems, _) => parse_params(elems, call_pos)?,
        _ => return Err(EvalError::Parse(format!("{call_pos}: lambda: expected parameter list"))),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        env: env.clone(),
    })
}

fn eval_and(exprs: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for e in exprs {
        result = eval(e, env, output)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for e in exprs {
        let result = eval(e, env, output)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_set_bang(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("{call_pos}: set! requires 2 arguments")));
    }
    let name = match &args[0] {
        Expr::Symbol(s, _) => s.clone(),
        _ => return Err(EvalError::Parse(format!("{call_pos}: set!: expected symbol"))),
    };
    let val = eval(&args[1], env, output)?;
    if !env_update(env, &name, val) {
        return Err(EvalError::UnboundVariable(format!("{call_pos}: {name}")));
    }
    Ok(Value::Void)
}

fn eval_string_set(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!("{call_pos}: string-set! requires 3 arguments")));
    }
    let var_name = match &args[0] {
        Expr::Symbol(name, _) => name.clone(),
        _ => return Err(EvalError::Type(format!("{call_pos}: string-set!: first argument must be a variable"))),
    };
    let idx_val = eval(&args[1], env, output)?;
    let idx = match &idx_val {
        Value::Integer(n) => *n as usize,
        _ => return Err(EvalError::Type(format!("{call_pos}: string-set!: expected integer index"))),
    };
    let ch = match eval(&args[2], env, output)? {
        Value::Char(c) => c,
        _ => return Err(EvalError::Type(format!("{call_pos}: string-set!: expected character"))),
    };
    let mut s = match env_get(env, &var_name) {
        Some(Value::Str(s)) => s,
        Some(_) => return Err(EvalError::Type(format!("{call_pos}: string-set!: expected string"))),
        None => return Err(EvalError::UnboundVariable(format!("{call_pos}: {var_name}"))),
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::Type(format!("{call_pos}: string-set!: index out of range")));
    }
    chars[idx] = ch;
    s = chars.into_iter().collect();
    env_update(env, &var_name, Value::Str(s));
    Ok(Value::Void)
}

fn eval_let(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{call_pos}: let requires bindings and body")));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let Expr::Symbol(name, _) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::Arity(format!("{call_pos}: named let requires bindings and body")));
        }
        let bindings_expr = match &args[1] {
            Expr::List(b, _) => b,
            _ => return Err(EvalError::Parse(format!("{call_pos}: let: expected bindings list"))),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings_expr {
            match b {
                Expr::List(pair, _) if pair.len() == 2 => {
                    if let Expr::Symbol(s, _) = &pair[0] {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env, output)?);
                    } else {
                        return Err(EvalError::Parse(format!("{call_pos}: let: expected symbol in binding")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("{call_pos}: let: invalid binding"))),
            }
        }
        let body = args[2..].to_vec();
        let local_env = new_env(Some(env.clone()));
        let lambda = Value::Lambda {
            params: params.clone(),
            rest_param: None,
            body,
            env: local_env.clone(),
        };
        env_set(&local_env, name.clone(), lambda);
        for (param, init) in params.iter().zip(inits.iter()) {
            env_set(&local_env, param.clone(), init.clone());
        }
        let mut result = Value::Void;
        for expr in &args[2..] {
            result = eval(expr, &local_env, output)?;
        }
        return Ok(result);
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => return Err(EvalError::Parse(format!("{call_pos}: let: expected bindings list"))),
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                if let Expr::Symbol(s, _) = &pair[0] {
                    let val = eval(&pair[1], env, output)?;
                    env_set(&local_env, s.clone(), val);
                } else {
                    return Err(EvalError::Parse(format!("{call_pos}: let: expected symbol in binding")));
                }
            }
            _ => return Err(EvalError::Parse(format!("{call_pos}: let: invalid binding"))),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, output)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval(expr, env, output)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    for clause in clauses {
        match clause {
            Expr::List(parts, _) if !parts.is_empty() => {
                if let Expr::Symbol(s, _) = &parts[0] {
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
            _ => return Err(EvalError::Parse("cond: invalid clause".into())),
        }
    }
    Ok(Value::Void)
}

fn as_integer(v: &Value, call_pos: Pos) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{call_pos}: expected integer, got {v}"))),
    }
}

fn apply_func(func: &Value, args: &[Value], call_pos: Pos, output: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(name) => apply_builtin(name, args, call_pos, output),
        Value::Lambda {
            params, rest_param, body, env, ..
        } => {
            if let Some(rest) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "{call_pos}: expected at least {} arguments, got {}",
                        params.len(),
                        args.len()
                    )));
                }
                let local_env = new_env(Some(env.clone()));
                for (param, arg) in params.iter().zip(args.iter()) {
                    env_set(&local_env, param.clone(), arg.clone());
                }
                env_set(&local_env, rest.clone(), Value::List(args[params.len()..].to_vec()));
                let mut result = Value::Void;
                for expr in body {
                    result = eval(expr, &local_env, output)?;
                }
                Ok(result)
            } else {
                if args.len() != params.len() {
                    return Err(EvalError::Arity(format!(
                        "{call_pos}: expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    )));
                }
                let local_env = new_env(Some(env.clone()));
                for (param, arg) in params.iter().zip(args.iter()) {
                    env_set(&local_env, param.clone(), arg.clone());
                }
                let mut result = Value::Void;
                for expr in body {
                    result = eval(expr, &local_env, output)?;
                }
                Ok(result)
            }
        }
        _ => Err(EvalError::Type(format!("{call_pos}: not a procedure: {func}"))),
    }
}

fn values_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        _ => false,
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::List(a), Value::List(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal(x, y))
        }
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => {
            values_equal(a1, b1) && values_equal(a2, b2)
        }
        _ => false,
    }
}

fn is_proper_list(v: &Value) -> bool {
    match v {
        Value::List(_) => true,
        Value::Pair(_, d) => is_proper_list(d),
        _ => false,
    }
}

// ---------- Macros (syntax-rules) ----------

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{base}__hyg_{n}")
}

const SPECIAL_FORMS: &[&str] = &[
    "define", "if", "quote", "lambda", "let", "begin", "cond", "and", "or",
    "set!", "string-set!", "not", "define-syntax", "syntax-rules",
];

#[derive(Clone)]
enum PatternBinding {
    Single(Expr),
    Repeated(Vec<Expr>),
}

fn eval_define_syntax(args: &[Expr], call_pos: Pos, env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("{call_pos}: define-syntax requires 2 arguments")));
    }
    let name = match &args[0] {
        Expr::Symbol(s, _) => s.clone(),
        _ => return Err(EvalError::Parse(format!("{call_pos}: define-syntax: expected symbol"))),
    };
    let (literals, rules) = match &args[1] {
        Expr::List(elems, _) if !elems.is_empty() => {
            if let Expr::Symbol(s, _) = &elems[0] {
                if s == "syntax-rules" {
                    parse_syntax_rules(&elems[1..], call_pos)?
                } else {
                    return Err(EvalError::Parse(format!("{call_pos}: define-syntax: expected syntax-rules")));
                }
            } else {
                return Err(EvalError::Parse(format!("{call_pos}: define-syntax: expected syntax-rules")));
            }
        }
        _ => return Err(EvalError::Parse(format!("{call_pos}: define-syntax: expected syntax-rules"))),
    };
    env_set(env, name, Value::Macro { literals, rules, def_env: env.clone() });
    Ok(Value::Void)
}

type SyntaxRulesResult = Result<(Vec<String>, Vec<(Expr, Expr)>), EvalError>;

fn parse_syntax_rules(args: &[Expr], call_pos: Pos) -> SyntaxRulesResult {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("{call_pos}: syntax-rules: expected literals list")));
    }
    let literals = match &args[0] {
        Expr::List(elems, _) => {
            let mut lits = Vec::new();
            for e in elems {
                if let Expr::Symbol(s, _) = e {
                    lits.push(s.clone());
                } else {
                    return Err(EvalError::Parse(format!("{call_pos}: syntax-rules: literals must be symbols")));
                }
            }
            lits
        }
        _ => return Err(EvalError::Parse(format!("{call_pos}: syntax-rules: expected literals list"))),
    };
    let mut rules = Vec::new();
    for rule_expr in &args[1..] {
        match rule_expr {
            Expr::List(parts, _) if parts.len() == 2 => {
                rules.push((parts[0].clone(), parts[1].clone()));
            }
            _ => return Err(EvalError::Parse(format!("{call_pos}: syntax-rules: each rule must be (pattern template)"))),
        }
    }
    Ok((literals, rules))
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    match pattern {
        Expr::Symbol(name, _) => {
            if name == "_" {
                return true;
            }
            if literals.contains(name) {
                if let Expr::Symbol(input_name, _) = input {
                    return input_name == name;
                }
                return false;
            }
            bindings.insert(name.clone(), PatternBinding::Single(input.clone()));
            true
        }
        Expr::List(pelems, _) => {
            if let Expr::List(ielems, _) = input {
                match_pattern_list(pelems, ielems, literals, bindings)
            } else {
                false
            }
        }
        Expr::Integer(n, _) => matches!(input, Expr::Integer(m, _) if *m == *n),
        Expr::Boolean(b, _) => matches!(input, Expr::Boolean(c, _) if *c == *b),
        Expr::Str(s, _) => matches!(input, Expr::Str(t, _) if t == s),
        _ => false,
    }
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(s, _) if s == "...")
}

fn collect_repeated_bindings(
    pat_vars: &[String],
    sub_bindings: &mut HashMap<String, PatternBinding>,
    repeated: &mut HashMap<String, Vec<Expr>>,
) {
    for var in pat_vars {
        if let Some(PatternBinding::Single(expr)) = sub_bindings.remove(var) {
            repeated.get_mut(var).expect("pattern var was pre-inserted").push(expr);
        }
    }
}

fn match_pattern_list(
    patterns: &[Expr],
    inputs: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    let mut pi = 0;
    let mut ii = 0;

    while pi < patterns.len() {
        // Check for ellipsis following current pattern
        if pi + 1 < patterns.len() && is_ellipsis(&patterns[pi + 1]) {
            let pat = &patterns[pi];
            let pat_vars = collect_pattern_var_names_vec(pat, literals);
            let mut repeated: HashMap<String, Vec<Expr>> = HashMap::new();
            for var in &pat_vars {
                repeated.insert(var.clone(), Vec::new());
            }
            let remaining_patterns = patterns.len() - pi - 2;
            let available = if inputs.len() >= ii + remaining_patterns {
                inputs.len() - ii - remaining_patterns
            } else {
                return false;
            };
            for j in 0..available {
                let mut sub_bindings = HashMap::new();
                if !match_pattern(pat, &inputs[ii + j], literals, &mut sub_bindings) {
                    return false;
                }
                collect_repeated_bindings(&pat_vars, &mut sub_bindings, &mut repeated);
            }
            for (var, exprs) in repeated {
                bindings.insert(var, PatternBinding::Repeated(exprs));
            }
            ii += available;
            pi += 2;
            continue;
        }
        if ii >= inputs.len() {
            return false;
        }
        if !match_pattern(&patterns[pi], &inputs[ii], literals, bindings) {
            return false;
        }
        pi += 1;
        ii += 1;
    }

    ii == inputs.len()
}

fn collect_pattern_var_names_vec(pattern: &Expr, literals: &[String]) -> Vec<String> {
    let mut vars = Vec::new();
    collect_pattern_var_names_inner(pattern, literals, &mut vars);
    vars
}

fn collect_pattern_var_names_inner(pattern: &Expr, literals: &[String], vars: &mut Vec<String>) {
    match pattern {
        Expr::Symbol(name, _) => {
            if name != "..." && name != "_" && !literals.contains(name) {
                vars.push(name.clone());
            }
        }
        Expr::List(elems, _) => {
            for e in elems {
                collect_pattern_var_names_inner(e, literals, vars);
            }
        }
        _ => {}
    }
}

fn collect_template_free_symbols(template: &Expr, pattern_vars: &HashSet<String>) -> HashSet<String> {
    let special: HashSet<&str> = SPECIAL_FORMS.iter().copied().collect();
    let builtins: HashSet<&str> = BUILTINS.iter().copied().collect();
    let mut result = HashSet::new();
    collect_free_inner(template, pattern_vars, &special, &builtins, &mut result);
    result
}

fn collect_free_inner(
    template: &Expr,
    pattern_vars: &HashSet<String>,
    special: &HashSet<&str>,
    builtins: &HashSet<&str>,
    result: &mut HashSet<String>,
) {
    match template {
        Expr::Symbol(name, _) => {
            if !pattern_vars.contains(name)
                && !special.contains(name.as_str())
                && !builtins.contains(name.as_str())
                && name != "..."
                && name != "_"
                && name != "else"
            {
                result.insert(name.clone());
            }
        }
        Expr::List(elems, _) => {
            for e in elems {
                collect_free_inner(e, pattern_vars, special, builtins, result);
            }
        }
        _ => {}
    }
}

fn expand_ellipsis_template(
    elem: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    hygiene_map: &HashMap<String, String>,
    result: &mut Vec<Expr>,
) {
    let rep_vars = find_repeated_vars(elem, bindings);
    let Some(first) = rep_vars.first() else { return };
    let Some(PatternBinding::Repeated(items)) = bindings.get(first) else { return };
    let count = items.len();
    for j in 0..count {
        let indexed = index_bindings(bindings, &rep_vars, j);
        result.push(expand_template(elem, &indexed, hygiene_map));
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    hygiene_map: &HashMap<String, String>,
) -> Expr {
    match template {
        Expr::Symbol(name, pos) => {
            if let Some(PatternBinding::Single(expr)) = bindings.get(name) {
                return expr.clone();
            }
            if let Some(renamed) = hygiene_map.get(name) {
                return Expr::Symbol(renamed.clone(), *pos);
            }
            template.clone()
        }
        Expr::List(elems, pos) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() && is_ellipsis(&elems[i + 1]) {
                    expand_ellipsis_template(&elems[i], bindings, hygiene_map, &mut result);
                    i += 2;
                    continue;
                }
                result.push(expand_template(&elems[i], bindings, hygiene_map));
                i += 1;
            }
            Expr::List(result, *pos)
        }
        _ => template.clone(),
    }
}

fn find_repeated_vars(template: &Expr, bindings: &HashMap<String, PatternBinding>) -> Vec<String> {
    let mut vars = Vec::new();
    match template {
        Expr::Symbol(name, _) => {
            if matches!(bindings.get(name), Some(PatternBinding::Repeated(_))) {
                vars.push(name.clone());
            }
        }
        Expr::List(elems, _) => {
            for e in elems {
                vars.extend(find_repeated_vars(e, bindings));
            }
        }
        _ => {}
    }
    vars
}

fn index_bindings(
    bindings: &HashMap<String, PatternBinding>,
    rep_vars: &[String],
    index: usize,
) -> HashMap<String, PatternBinding> {
    let mut new_bindings = bindings.clone();
    for var in rep_vars {
        if let Some(PatternBinding::Repeated(items)) = bindings.get(var) {
            new_bindings.insert(var.clone(), PatternBinding::Single(items[index].clone()));
        }
    }
    new_bindings
}

fn expand_macro(
    literals: &[String],
    rules: &[(Expr, Expr)],
    call_elems: &[Expr],
    call_pos: Pos,
    def_env: &Env,
) -> Result<(Expr, Vec<(String, Value)>), EvalError> {
    let call_args = &call_elems[1..];

    for (pattern, template) in rules {
        let pat_args = match pattern {
            Expr::List(elems, _) => &elems[1..],
            _ => continue,
        };

        let mut bindings = HashMap::new();
        if match_pattern_list(pat_args, call_args, literals, &mut bindings) {
            let mut pattern_vars = HashSet::new();
            for pe in pat_args {
                for v in collect_pattern_var_names_vec(pe, literals) {
                    pattern_vars.insert(v);
                }
            }

            let free_syms = collect_template_free_symbols(template, &pattern_vars);
            let mut hygiene_map = HashMap::new();
            let mut hygiene_bindings = Vec::new();
            for sym in &free_syms {
                let gs = gensym(sym);
                hygiene_map.insert(sym.clone(), gs.clone());
                if let Some(val) = env_get(def_env, sym) {
                    hygiene_bindings.push((gs, val));
                }
            }

            let expanded = expand_template(template, &bindings, &hygiene_map);
            return Ok((expanded, hygiene_bindings));
        }
    }

    Err(EvalError::Parse(format!("{call_pos}: no matching syntax-rules pattern")))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = default_env();
    let mut output = String::new();
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &env, &mut output)?;
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
    let env = default_env();
    let mut output = String::new();
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &env, &mut output)?;
    }
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
