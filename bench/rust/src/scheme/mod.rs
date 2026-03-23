pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

thread_local! {
    static OUTPUT_BUFFER: RefCell<String> = RefCell::new(String::new());
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    Pair(Box<Value>, Box<Value>),
    Nil,
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Void,
    Builtin(&'static str, fn(&[Value]) -> Result<Value, EvalError>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Char(c) => format!("#\\{}", c),
            Value::Symbol(s) => s.clone(),
            Value::Nil => "()".to_string(),
            Value::Pair(_, _) => {
                let mut out = String::from("(");
                let mut cur = self;
                let mut first = true;
                loop {
                    match cur {
                        Value::Pair(car, cdr) => {
                            if !first {
                                out.push(' ');
                            }
                            first = false;
                            out.push_str(&car.display());
                            cur = cdr;
                        }
                        Value::Nil => break,
                        other => {
                            out.push_str(" . ");
                            out.push_str(&other.display());
                            break;
                        }
                    }
                }
                out.push(')');
                out
            }
            Value::Lambda { .. } | Value::Builtin(_, _) => "#<procedure>".to_string(),
            Value::Void => "#<void>".to_string(),
        }
    }

    fn display_for_display(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::Pair(_, _) => {
                let mut out = String::from("(");
                let mut cur = self;
                let mut first = true;
                loop {
                    match cur {
                        Value::Pair(car, cdr) => {
                            if !first { out.push(' '); }
                            first = false;
                            out.push_str(&car.display_for_display());
                            cur = cdr;
                        }
                        Value::Nil => break,
                        other => {
                            out.push_str(" . ");
                            out.push_str(&other.display_for_display());
                            break;
                        }
                    }
                }
                out.push(')');
                out
            }
            _ => self.display(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::Type(format!(
                "expected integer, got {}",
                other.display()
            ))),
        }
    }
}

// --- Expr with position ---

#[derive(Debug, Clone, PartialEq)]
struct Expr {
    kind: ExprKind,
    line: usize,
    col: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

impl Expr {
    fn new(kind: ExprKind, line: usize, col: usize) -> Self {
        Self { kind, line, col }
    }

    fn wrap_err(&self, err: EvalError) -> EvalError {
        match &err {
            EvalError::WithPosition { .. } => err,
            _ => EvalError::WithPosition {
                source: Box::new(err),
                line: self.line,
                col: self.col,
            },
        }
    }
}

// --- Environment ---

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

fn env_get(env: &Env, name: &str) -> Result<Value, EvalError> {
    let inner = env.borrow();
    if let Some(v) = inner.bindings.get(name) {
        Ok(v.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        Err(EvalError::UnboundVariable(name.to_string()))
    }
}

fn env_set(env: &Env, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

fn env_update(env: &Env, name: &str, val: Value) -> Result<(), EvalError> {
    let mut cur = env.clone();
    loop {
        {
            let mut inner = cur.borrow_mut();
            if inner.bindings.contains_key(name) {
                inner.bindings.insert(name.to_string(), val);
                return Ok(());
            }
        }
        let parent = cur.borrow().parent.clone();
        match parent {
            Some(p) => cur = p,
            None => return Err(EvalError::UnboundVariable(name.to_string())),
        }
    }
}

// --- Builtins ---

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum: i64 = 0;
    for a in args {
        sum += a.as_integer()?;
    }
    Ok(Value::Integer(sum))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    let first = args[0].as_integer()?;
    if args.len() == 1 {
        return Ok(Value::Integer(-first));
    }
    let mut result = first;
    for a in &args[1..] {
        result -= a.as_integer()?;
    }
    Ok(Value::Integer(result))
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product: i64 = 1;
    for a in args {
        product *= a.as_integer()?;
    }
    Ok(Value::Integer(product))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("/ requires at least 1 argument".into()));
    }
    let first = args[0].as_integer()?;
    if args.len() == 1 {
        if first == 0 {
            return Err(EvalError::DivisionByZero);
        }
        return Ok(Value::Integer(1 / first));
    }
    let mut result = first;
    for a in &args[1..] {
        let v = a.as_integer()?;
        if v == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= v;
    }
    Ok(Value::Integer(result))
}

fn compare_values(args: &[Value], cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(
            "comparison requires at least 2 arguments".into(),
        ));
    }
    let vals: Result<Vec<i64>, _> = args.iter().map(|a| a.as_integer()).collect();
    let vals = vals?;
    for w in vals.windows(2) {
        if !cmp(w[0], w[1]) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}

fn builtin_lt(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |a, b| a < b)
}
fn builtin_gt(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |a, b| a > b)
}
fn builtin_eq(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |a, b| a == b)
}
fn builtin_le(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |a, b| a <= b)
}
fn builtin_ge(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |a, b| a >= b)
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("not requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("cons requires exactly 2 arguments".into()));
    }
    Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Pair(car, _) => Ok(*car.clone()),
        _ => Err(EvalError::Type("car: not a pair".into())),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Pair(_, cdr) => Ok(*cdr.clone()),
        _ => Err(EvalError::Type("cdr: not a pair".into())),
    }
}

fn builtin_null(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("null? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Nil)))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Nil;
    for a in args.iter().rev() {
        result = Value::Pair(Box::new(a.clone()), Box::new(result));
    }
    Ok(result)
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("length requires exactly 1 argument".into()));
    }
    let mut count = 0i64;
    let mut cur = &args[0];
    loop {
        match cur {
            Value::Nil => return Ok(Value::Integer(count)),
            Value::Pair(_, cdr) => {
                count += 1;
                cur = cdr;
            }
            _ => return Err(EvalError::Type("length: not a proper list".into())),
        }
    }
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Nil);
    }
    let mut result = args[args.len() - 1].clone();
    for i in (0..args.len() - 1).rev() {
        let mut elems = Vec::new();
        let mut cur = &args[i];
        loop {
            match cur {
                Value::Nil => break,
                Value::Pair(car, cdr) => {
                    elems.push(*car.clone());
                    cur = cdr;
                }
                _ => return Err(EvalError::Type("append: not a proper list".into())),
            }
        }
        for e in elems.into_iter().rev() {
            result = Value::Pair(Box::new(e), Box::new(result));
        }
    }
    Ok(result)
}

fn builtin_number_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("number? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
}

fn builtin_string_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Str(_))))
}

fn builtin_boolean_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("boolean? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
}

fn builtin_pair_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("pair? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Pair(_, _))))
}

fn builtin_symbol_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("symbol? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
}

fn builtin_char_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("char? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Char(_))))
}

fn builtin_display(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("display requires exactly 1 argument".into()));
    }
    let s = args[0].display_for_display();
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(&s));
    Ok(Value::Void)
}

fn builtin_write(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("write requires exactly 1 argument".into()));
    }
    let s = args[0].display();
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(&s));
    Ok(Value::Void)
}

fn builtin_newline(args: &[Value]) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::Arity("newline requires 0 arguments".into()));
    }
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push('\n'));
    Ok(Value::Void)
}

fn builtin_string_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = String::new();
    for a in args {
        match a {
            Value::Str(s) => result.push_str(s),
            _ => return Err(EvalError::Type("string-append: expected string".into())),
        }
    }
    Ok(Value::Str(result))
}

fn builtin_string_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-length requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
        _ => Err(EvalError::Type("string-length: expected string".into())),
    }
}

fn builtin_substring(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("substring requires exactly 3 arguments".into()));
    }
    let s = match &args[0] {
        Value::Str(s) => s,
        _ => return Err(EvalError::Type("substring: expected string".into())),
    };
    let start = args[1].as_integer()? as usize;
    let end = args[2].as_integer()? as usize;
    Ok(Value::Str(s[start..end].to_string()))
}

fn builtin_string_to_number(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->number requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Str(s) => match s.parse::<i64>() {
            Ok(n) => Ok(Value::Integer(n)),
            Err(_) => Ok(Value::Boolean(false)),
        },
        _ => Err(EvalError::Type("string->number: expected string".into())),
    }
}

fn builtin_number_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("number->string requires exactly 1 argument".into()));
    }
    let n = args[0].as_integer()?;
    Ok(Value::Str(n.to_string()))
}

fn builtin_symbol_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("symbol->string requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Symbol(s) => Ok(Value::Str(s.clone())),
        _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
    }
}

fn builtin_string_to_symbol(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->symbol requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Str(s) => Ok(Value::Symbol(s.clone())),
        _ => Err(EvalError::Type("string->symbol: expected string".into())),
    }
}

fn builtin_string_copy(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-copy requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.clone())),
        _ => Err(EvalError::Type("string-copy: expected string".into())),
    }
}

fn builtin_string_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("string-ref requires exactly 2 arguments".into()));
    }
    let s = match &args[0] {
        Value::Str(s) => s,
        _ => return Err(EvalError::Type("string-ref: expected string".into())),
    };
    let idx = args[1].as_integer()? as usize;
    match s.chars().nth(idx) {
        Some(c) => Ok(Value::Char(c)),
        None => Err(EvalError::Type("string-ref: index out of bounds".into())),
    }
}

fn default_env() -> Env {
    let env = new_env(None);
    let builtins: &[(&'static str, fn(&[Value]) -> Result<Value, EvalError>)] = &[
        ("+", builtin_add),
        ("-", builtin_sub),
        ("*", builtin_mul),
        ("/", builtin_div),
        ("<", builtin_lt),
        (">", builtin_gt),
        ("=", builtin_eq),
        ("<=", builtin_le),
        (">=", builtin_ge),
        ("not", builtin_not),
        ("cons", builtin_cons),
        ("car", builtin_car),
        ("cdr", builtin_cdr),
        ("null?", builtin_null),
        ("list", builtin_list),
        ("length", builtin_length),
        ("number?", builtin_number_pred),
        ("string?", builtin_string_pred),
        ("boolean?", builtin_boolean_pred),
        ("pair?", builtin_pair_pred),
        ("symbol?", builtin_symbol_pred),
        ("char?", builtin_char_pred),
        ("append", builtin_append),
        ("display", builtin_display),
        ("write", builtin_write),
        ("newline", builtin_newline),
        ("string-append", builtin_string_append),
        ("string-length", builtin_string_length),
        ("substring", builtin_substring),
        ("string->number", builtin_string_to_number),
        ("number->string", builtin_number_to_string),
        ("symbol->string", builtin_symbol_to_string),
        ("string->symbol", builtin_string_to_symbol),
        ("string-ref", builtin_string_ref),
        ("string-copy", builtin_string_copy),
    ];
    for &(name, func) in builtins {
        env_set(&env, name.to_string(), Value::Builtin(name, func));
    }
    env
}

// --- Token with position ---

#[derive(Debug, Clone)]
struct Token {
    text: String,
    line: usize,
    col: usize,
}

// --- Parser ---

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
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' | ')' => {
                tokens.push(Token { text: chars[i].to_string(), line, col });
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push(Token { text: "'".to_string(), line, col });
                i += 1;
                col += 1;
            }
            '"' => {
                let start_col = col;
                let start_line = line;
                let mut s = String::new();
                s.push('"');
                i += 1;
                col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        i += 1;
                        col += 1;
                        s.push(chars[i]);
                        i += 1;
                        col += 1;
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
                tokens.push(Token { text: s, line: start_line, col: start_col });
            }
            '#' if i + 1 < chars.len() && chars[i + 1] == '\\' => {
                let start_col = col;
                i += 2; // skip #\
                col += 2;
                if i < chars.len() {
                    // Check for named characters like #\space, #\newline
                    let ch_start = i;
                    if chars[i].is_alphabetic() {
                        while i < chars.len() && chars[i].is_alphabetic() {
                            i += 1;
                            col += 1;
                        }
                        let name = &chars[ch_start..i];
                        let name_str: String = name.iter().collect();
                        let tok = format!("#\\{}", name_str);
                        tokens.push(Token { text: tok, line, col: start_col });
                    } else {
                        let tok = format!("#\\{}", chars[i]);
                        i += 1;
                        col += 1;
                        tokens.push(Token { text: tok, line, col: start_col });
                    }
                }
            }
            '#' if i + 1 < chars.len() && (chars[i + 1] == 't' || chars[i + 1] == 'f') => {
                let start_col = col;
                let mut tok = String::from('#');
                i += 1;
                col += 1;
                tok.push(chars[i]);
                i += 1;
                col += 1;
                tokens.push(Token { text: tok, line, col: start_col });
            }
            _ => {
                let start_col = col;
                let mut tok = String::new();
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '\'')
                {
                    tok.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: tok, line, col: start_col });
            }
        }
    }
    tokens
}

fn parse_tokens(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let token = &tokens[*pos];
    let tline = token.line;
    let tcol = token.col;

    if token.text == "'" {
        *pos += 1;
        let inner = parse_tokens(tokens, pos)?;
        Ok(Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("quote".into()), tline, tcol),
            inner,
        ]), tline, tcol))
    } else if token.text == "(" {
        *pos += 1;
        let mut list = Vec::new();
        while *pos < tokens.len() && tokens[*pos].text != ")" {
            list.push(parse_tokens(tokens, pos)?);
        }
        if *pos >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".into()));
        }
        *pos += 1;
        Ok(Expr::new(ExprKind::List(list), tline, tcol))
    } else if token.text == ")" {
        Err(EvalError::Parse("unexpected ')'".into()))
    } else if token.text == "#t" {
        *pos += 1;
        Ok(Expr::new(ExprKind::Boolean(true), tline, tcol))
    } else if token.text == "#f" {
        *pos += 1;
        Ok(Expr::new(ExprKind::Boolean(false), tline, tcol))
    } else if token.text.starts_with('"') {
        *pos += 1;
        let inner = &token.text[1..token.text.len() - 1];
        Ok(Expr::new(ExprKind::Str(inner.to_string()), tline, tcol))
    } else if token.text.starts_with("#\\") {
        *pos += 1;
        let char_name = &token.text[2..];
        let ch = match char_name {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().unwrap(),
            _ => return Err(EvalError::Parse(format!("unknown character: {}", token.text))),
        };
        Ok(Expr::new(ExprKind::Char(ch), tline, tcol))
    } else if let Ok(n) = token.text.parse::<i64>() {
        *pos += 1;
        Ok(Expr::new(ExprKind::Integer(n), tline, tcol))
    } else {
        *pos += 1;
        Ok(Expr::new(ExprKind::Symbol(token.text.clone()), tline, tcol))
    }
}

fn parse(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_tokens(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// --- Evaluator ---

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let mut cur_expr = expr.clone();
    let mut cur_env = env.clone();

    loop {
        match cur_expr.kind.clone() {
            ExprKind::Integer(n) => return Ok(Value::Integer(n)),
            ExprKind::Boolean(b) => return Ok(Value::Boolean(b)),
            ExprKind::Str(s) => return Ok(Value::Str(s)),
            ExprKind::Char(c) => return Ok(Value::Char(c)),
            ExprKind::Symbol(name) => return env_get(&cur_env, &name).map_err(|e| cur_expr.wrap_err(e)),
            ExprKind::List(items) => {
                if items.is_empty() {
                    return Err(cur_expr.wrap_err(EvalError::BadSyntax("empty application".into())));
                }
                if let ExprKind::Symbol(ref op) = items[0].kind {
                    match op.as_str() {
                        "define" => return eval_define(&cur_expr, &items[1..], &cur_env),
                        "quote" => return eval_quote(&cur_expr, &items[1..]),
                        "lambda" => return eval_lambda(&cur_expr, &items[1..], &cur_env),
                        "set!" => {
                            let args = &items[1..];
                            if args.len() != 2 {
                                return Err(cur_expr.wrap_err(EvalError::BadSyntax("set! requires exactly 2 arguments".into())));
                            }
                            let name = match &args[0].kind {
                                ExprKind::Symbol(n) => n.clone(),
                                _ => return Err(cur_expr.wrap_err(EvalError::BadSyntax("set!: first argument must be a variable".into()))),
                            };
                            let val = eval(&args[1], &cur_env)?;
                            env_update(&cur_env, &name, val).map_err(|e| cur_expr.wrap_err(e))?;
                            return Ok(Value::Void);
                        }
                        "string-set!" => return eval_string_set(&cur_expr, &items[1..], &cur_env),
                        "if" => {
                            let args = &items[1..];
                            if args.len() < 2 || args.len() > 3 {
                                return Err(cur_expr.wrap_err(EvalError::BadSyntax("if: expected 2 or 3 parts".into())));
                            }
                            let cond = eval(&args[0], &cur_env)?;
                            if cond.is_truthy() {
                                cur_expr = args[1].clone();
                            } else if args.len() == 3 {
                                cur_expr = args[2].clone();
                            } else {
                                return Ok(Value::Void);
                            }
                            continue;
                        }
                        "begin" => {
                            let args = &items[1..];
                            if args.is_empty() {
                                return Ok(Value::Void);
                            }
                            for e in &args[..args.len() - 1] {
                                eval(e, &cur_env)?;
                            }
                            cur_expr = args[args.len() - 1].clone();
                            continue;
                        }
                        "cond" => {
                            let clauses = &items[1..];
                            let mut tail = None;
                            for clause in clauses {
                                match &clause.kind {
                                    ExprKind::List(citems) if !citems.is_empty() => {
                                        let is_else = matches!(&citems[0].kind, ExprKind::Symbol(s) if s == "else");
                                        if is_else {
                                            if citems.len() == 1 {
                                                return Ok(Value::Void);
                                            }
                                            for e in &citems[1..citems.len() - 1] {
                                                eval(e, &cur_env)?;
                                            }
                                            tail = Some(citems[citems.len() - 1].clone());
                                            break;
                                        }
                                        let test = eval(&citems[0], &cur_env)?;
                                        if test.is_truthy() {
                                            if citems.len() == 1 {
                                                return Ok(test);
                                            }
                                            for e in &citems[1..citems.len() - 1] {
                                                eval(e, &cur_env)?;
                                            }
                                            tail = Some(citems[citems.len() - 1].clone());
                                            break;
                                        }
                                    }
                                    _ => return Err(clause.wrap_err(EvalError::BadSyntax("cond: bad clause".into()))),
                                }
                            }
                            match tail {
                                Some(e) => { cur_expr = e; continue; }
                                None => return Ok(Value::Void),
                            }
                        }
                        "and" => {
                            let args = &items[1..];
                            if args.is_empty() {
                                return Ok(Value::Boolean(true));
                            }
                            for a in &args[..args.len() - 1] {
                                let result = eval(a, &cur_env)?;
                                if !result.is_truthy() {
                                    return Ok(result);
                                }
                            }
                            cur_expr = args[args.len() - 1].clone();
                            continue;
                        }
                        "or" => {
                            let args = &items[1..];
                            if args.is_empty() {
                                return Ok(Value::Boolean(false));
                            }
                            for a in &args[..args.len() - 1] {
                                let result = eval(a, &cur_env)?;
                                if result.is_truthy() {
                                    return Ok(result);
                                }
                            }
                            cur_expr = args[args.len() - 1].clone();
                            continue;
                        }
                        "let" => {
                            let args = &items[1..];
                            if args.len() < 2 {
                                return Err(cur_expr.wrap_err(EvalError::BadSyntax("let: expected bindings and body".into())));
                            }
                            // Named let
                            if let ExprKind::Symbol(name) = &args[0].kind {
                                if args.len() < 3 {
                                    return Err(cur_expr.wrap_err(EvalError::BadSyntax("let: expected bindings and body".into())));
                                }
                                let bindings = match &args[1].kind {
                                    ExprKind::List(b) => b,
                                    _ => return Err(cur_expr.wrap_err(EvalError::BadSyntax("let: expected binding list".into()))),
                                };
                                let mut param_names = Vec::new();
                                let mut init_vals = Vec::new();
                                for binding in bindings {
                                    match &binding.kind {
                                        ExprKind::List(pair) if pair.len() == 2 => {
                                            let pname = match &pair[0].kind {
                                                ExprKind::Symbol(s) => s.clone(),
                                                _ => return Err(cur_expr.wrap_err(EvalError::BadSyntax("let: expected symbol".into()))),
                                            };
                                            init_vals.push(eval(&pair[1], &cur_env)?);
                                            param_names.push(pname);
                                        }
                                        _ => return Err(cur_expr.wrap_err(EvalError::BadSyntax("let: bad binding".into()))),
                                    }
                                }
                                let body = args[2..].to_vec();
                                let local_env = new_env(Some(cur_env.clone()));
                                let lambda = Value::Lambda {
                                    params: param_names.clone(),
                                    body,
                                    env: local_env.clone(),
                                };
                                env_set(&local_env, name.clone(), lambda);
                                for (p, v) in param_names.iter().zip(init_vals.iter()) {
                                    env_set(&local_env, p.clone(), v.clone());
                                }
                                let body_exprs = &args[2..];
                                if body_exprs.is_empty() {
                                    return Ok(Value::Void);
                                }
                                for e in &body_exprs[..body_exprs.len() - 1] {
                                    eval(e, &local_env)?;
                                }
                                cur_expr = body_exprs[body_exprs.len() - 1].clone();
                                cur_env = local_env;
                                continue;
                            }
                            // Regular let
                            let bindings = match &args[0].kind {
                                ExprKind::List(b) => b,
                                _ => return Err(cur_expr.wrap_err(EvalError::BadSyntax("let: expected binding list".into()))),
                            };
                            let local_env = new_env(Some(cur_env.clone()));
                            for binding in bindings {
                                match &binding.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let bname = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(cur_expr.wrap_err(EvalError::BadSyntax("let: expected symbol in binding".into()))),
                                        };
                                        let val = eval(&pair[1], &cur_env)?;
                                        env_set(&local_env, bname, val);
                                    }
                                    _ => return Err(cur_expr.wrap_err(EvalError::BadSyntax("let: bad binding".into()))),
                                }
                            }
                            let body = &args[1..];
                            if body.is_empty() {
                                return Ok(Value::Void);
                            }
                            for e in &body[..body.len() - 1] {
                                eval(e, &local_env)?;
                            }
                            cur_expr = body[body.len() - 1].clone();
                            cur_env = local_env;
                            continue;
                        }
                        _ => {}
                    }
                }
                // Function application
                let func = eval(&items[0], &cur_env)?;
                let args: Result<Vec<Value>, _> =
                    items[1..].iter().map(|a| eval(a, &cur_env)).collect();
                let args = args?;
                match func {
                    Value::Lambda { params, body, env: closure_env } => {
                        if args.len() != params.len() {
                            return Err(cur_expr.wrap_err(EvalError::Arity(format!(
                                "expected {} arguments, got {}",
                                params.len(),
                                args.len()
                            ))));
                        }
                        let local_env = new_env(Some(closure_env));
                        for (p, a) in params.iter().zip(args.iter()) {
                            env_set(&local_env, p.clone(), a.clone());
                        }
                        if body.is_empty() {
                            return Ok(Value::Void);
                        }
                        for e in &body[..body.len() - 1] {
                            eval(e, &local_env)?;
                        }
                        cur_expr = body[body.len() - 1].clone();
                        cur_env = local_env;
                        continue;
                    }
                    Value::Builtin(_, func_ptr) => {
                        return func_ptr(&args).map_err(|e| cur_expr.wrap_err(e));
                    }
                    _ => {
                        return Err(cur_expr.wrap_err(EvalError::Type(format!(
                            "not a procedure: {}",
                            func.display()
                        ))));
                    }
                }
            }
        }
    }
}

fn eval_define(form: &Expr, args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(form.wrap_err(EvalError::BadSyntax("define: missing name".into())));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(form.wrap_err(EvalError::BadSyntax("define: expected 2 parts".into())));
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(form.wrap_err(EvalError::BadSyntax("define: empty signature".into())));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(n) => n.clone(),
                _ => return Err(form.wrap_err(EvalError::BadSyntax("define: expected symbol".into()))),
            };
            let params: Result<Vec<String>, _> = sig[1..]
                .iter()
                .map(|e| match &e.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(form.wrap_err(EvalError::BadSyntax("define: expected parameter name".into()))),
                })
                .collect();
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params: params?,
                body,
                env: env.clone(),
            };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(form.wrap_err(EvalError::BadSyntax("define: bad syntax".into()))),
    }
}

fn eval_quote(form: &Expr, args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(form.wrap_err(EvalError::BadSyntax("quote: expected 1 argument".into())));
    }
    expr_to_value(&args[0])
}

fn expr_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Symbol(s) => Ok(Value::Symbol(s.clone())),
        ExprKind::List(items) => {
            let mut result = Value::Nil;
            for item in items.iter().rev() {
                let v = expr_to_value(item)?;
                result = Value::Pair(Box::new(v), Box::new(result));
            }
            Ok(result)
        }
    }
}

fn eval_lambda(form: &Expr, args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(form.wrap_err(EvalError::BadSyntax(
            "lambda: expected params and body".into(),
        )));
    }
    let params = match &args[0].kind {
        ExprKind::List(param_exprs) => {
            let mut params = Vec::new();
            for p in param_exprs {
                match &p.kind {
                    ExprKind::Symbol(s) => params.push(s.clone()),
                    _ => {
                        return Err(form.wrap_err(EvalError::BadSyntax(
                            "lambda: expected parameter name".into(),
                        )))
                    }
                }
            }
            params
        }
        _ => {
            return Err(form.wrap_err(EvalError::BadSyntax(
                "lambda: expected parameter list".into(),
            )))
        }
    };
    Ok(Value::Lambda {
        params,
        body: args[1..].to_vec(),
        env: env.clone(),
    })
}

fn eval_string_set(form: &Expr, args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(form.wrap_err(EvalError::Arity("string-set! requires exactly 3 arguments".into())));
    }
    let var_name = match &args[0].kind {
        ExprKind::Symbol(name) => name.clone(),
        _ => return Err(form.wrap_err(EvalError::Type("string-set!: first argument must be a variable".into()))),
    };
    let idx = eval(&args[1], env)?.as_integer()? as usize;
    let ch = match eval(&args[2], env)? {
        Value::Char(c) => c,
        _ => return Err(form.wrap_err(EvalError::Type("string-set!: third argument must be a character".into()))),
    };
    let s = env_get(env, &var_name).map_err(|e| form.wrap_err(e))?;
    match s {
        Value::Str(mut string) => {
            let mut chars: Vec<char> = string.chars().collect();
            if idx >= chars.len() {
                return Err(form.wrap_err(EvalError::Type("string-set!: index out of bounds".into())));
            }
            chars[idx] = ch;
            string = chars.into_iter().collect();
            env_update(env, &var_name, Value::Str(string)).map_err(|e| form.wrap_err(e))?;
            Ok(Value::Void)
        }
        _ => Err(form.wrap_err(EvalError::Type("string-set!: expected string".into()))),
    }
}

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse(input)?;
    let env = default_env();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &env)?;
    }
    Ok(result.display())
}

pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().clear());
    let exprs = parse(input)?;
    let env = default_env();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &env)?;
    }
    let output = OUTPUT_BUFFER.with(|buf| buf.borrow().clone());
    Ok((result.display(), output))
}

#[cfg(test)]
mod tests;
