pub mod error;

pub use error::EvalError;

use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
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
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

// --- Tokenizer ---

#[derive(Debug, Clone)]
enum Token {
    LParen,
    RParen,
    Quote,
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    Str(String),
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => { tokens.push(Token::LParen); i += 1; }
            ')' => { tokens.push(Token::RParen); i += 1; }
            '\'' => { tokens.push(Token::Quote); i += 1; }
            '"' => {
                i += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            c => { s.push('\\'); s.push(c); }
                        }
                    } else {
                        s.push(chars[i]);
                    }
                    i += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse("unterminated string".into()));
                }
                i += 1;
                tokens.push(Token::Str(s));
            }
            '#' => {
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push(Token::Boolean(true));
                            i += 2;
                        }
                        'f' => {
                            tokens.push(Token::Boolean(false));
                            i += 2;
                        }
                        _ => return Err(EvalError::Parse(format!("unexpected #{}", chars[i + 1]))),
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into()));
                }
            }
            c if c == '-' || c == '+' => {
                if i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                    let is_number = i == 0
                        || matches!(tokens.last(), Some(Token::LParen) | None);
                    if is_number {
                        let start = i;
                        i += 1;
                        while i < chars.len() && chars[i].is_ascii_digit() {
                            i += 1;
                        }
                        let num_str: String = chars[start..i].iter().collect();
                        tokens.push(Token::Integer(num_str.parse().map_err(|_| {
                            EvalError::Parse(format!("invalid number: {num_str}"))
                        })?));
                    } else {
                        let start = i;
                        i += 1;
                        while i < chars.len() && is_symbol_char(chars[i]) {
                            i += 1;
                        }
                        let sym: String = chars[start..i].iter().collect();
                        tokens.push(Token::Symbol(sym));
                    }
                } else {
                    let start = i;
                    i += 1;
                    while i < chars.len() && is_symbol_char(chars[i]) {
                        i += 1;
                    }
                    let sym: String = chars[start..i].iter().collect();
                    tokens.push(Token::Symbol(sym));
                }
            }
            c if c.is_ascii_digit() => {
                let start = i;
                while i < chars.len() && chars[i].is_ascii_digit() {
                    i += 1;
                }
                let num_str: String = chars[start..i].iter().collect();
                tokens.push(Token::Integer(num_str.parse().map_err(|_| {
                    EvalError::Parse(format!("invalid number: {num_str}"))
                })?));
            }
            c if is_symbol_start(c) => {
                let start = i;
                while i < chars.len() && is_symbol_char(chars[i]) {
                    i += 1;
                }
                let sym: String = chars[start..i].iter().collect();
                tokens.push(Token::Symbol(sym));
            }
            c => return Err(EvalError::Parse(format!("unexpected character: {c}"))),
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
    match &tokens[*pos] {
        Token::Integer(n) => { let n = *n; *pos += 1; Ok(Expr::Integer(n)) }
        Token::Boolean(b) => { let b = *b; *pos += 1; Ok(Expr::Boolean(b)) }
        Token::Str(s) => { let s = s.clone(); *pos += 1; Ok(Expr::Str(s)) }
        Token::Symbol(s) => { let s = s.clone(); *pos += 1; Ok(Expr::Symbol(s)) }
        Token::Quote => {
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Expr::List(vec![Expr::Symbol("quote".into()), inner]))
        }
        Token::LParen => {
            *pos += 1;
            let mut list = Vec::new();
            while *pos < tokens.len() && !matches!(tokens[*pos], Token::RParen) {
                list.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into()));
            }
            *pos += 1;
            Ok(Expr::List(list))
        }
        Token::RParen => Err(EvalError::Parse("unexpected )".into())),
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
    bindings: HashMap<String, Value>,
    parent: Option<Box<Env>>,
}

impl Env {
    fn new() -> Self {
        Env { bindings: HashMap::new(), parent: None }
    }

    fn with_parent(parent: Env) -> Self {
        Env { bindings: HashMap::new(), parent: Some(Box::new(parent)) }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.bindings.get(name) {
            Some(v.clone())
        } else if let Some(ref parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    fn set(&mut self, name: String, val: Value) {
        self.bindings.insert(name, val);
    }
}

fn default_env() -> Env {
    let mut env = Env::new();
    for name in [
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not", "and", "or",
        "cons", "car", "cdr", "null?", "list", "length", "append",
        "string?", "number?", "boolean?", "pair?", "symbol?",
    ] {
        env.set(name.into(), Value::Builtin(name.into()));
    }
    env
}

// --- Evaluator ---

fn eval(expr: &Expr, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(s) => {
            env.get(s).ok_or_else(|| EvalError::Unbound(s.clone()))
        }
        Expr::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Syntax("empty application".into()));
            }

            // Check for special forms
            if let Expr::Symbol(head) = &elems[0] {
                match head.as_str() {
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Syntax("quote requires 1 argument".into()));
                        }
                        return Ok(expr_to_value(&elems[1]));
                    }
                    "if" => {
                        if elems.len() < 3 || elems.len() > 4 {
                            return Err(EvalError::Syntax("if requires 2 or 3 arguments".into()));
                        }
                        let cond = eval(&elems[1], env)?;
                        if is_truthy(&cond) {
                            return eval(&elems[2], env);
                        } else if elems.len() == 4 {
                            return eval(&elems[3], env);
                        } else {
                            return Ok(Value::Void);
                        }
                    }
                    "define" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax("define requires at least 2 arguments".into()));
                        }
                        match &elems[1] {
                            Expr::Symbol(name) => {
                                let val = eval(&elems[2], env)?;
                                env.set(name.clone(), val);
                                return Ok(Value::Void);
                            }
                            Expr::List(sig) => {
                                // (define (f x y) body...)
                                if sig.is_empty() {
                                    return Err(EvalError::Syntax("define: empty signature".into()));
                                }
                                let name = match &sig[0] {
                                    Expr::Symbol(s) => s.clone(),
                                    _ => return Err(EvalError::Syntax("define: expected function name".into())),
                                };
                                let params: Vec<String> = sig[1..].iter().map(|e| match e {
                                    Expr::Symbol(s) => Ok(s.clone()),
                                    _ => Err(EvalError::Syntax("define: expected parameter name".into())),
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
                            _ => return Err(EvalError::Syntax("define: expected symbol or list".into())),
                        }
                    }
                    "lambda" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax("lambda requires params and body".into()));
                        }
                        let params = match &elems[1] {
                            Expr::List(ps) => {
                                ps.iter().map(|e| match e {
                                    Expr::Symbol(s) => Ok(s.clone()),
                                    _ => Err(EvalError::Syntax("lambda: expected parameter name".into())),
                                }).collect::<Result<Vec<_>, _>>()?
                            }
                            _ => return Err(EvalError::Syntax("lambda: expected parameter list".into())),
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
                            result = eval(a, env)?;
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
                            result = eval(a, env)?;
                            if is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "let" => {
                        // (let ((x 1) (y 2)) body...)
                        // or named let: (let name ((x 1) (y 2)) body...)
                        if elems.len() < 3 {
                            return Err(EvalError::Syntax("let requires bindings and body".into()));
                        }
                        let (name, bindings_expr, body_start) = match &elems[1] {
                            Expr::Symbol(n) => {
                                // Named let
                                if elems.len() < 4 {
                                    return Err(EvalError::Syntax("named let requires bindings and body".into()));
                                }
                                (Some(n.clone()), &elems[2], 3)
                            }
                            Expr::List(_) => (None, &elems[1], 2),
                            _ => return Err(EvalError::Syntax("let: expected bindings list".into())),
                        };
                        let bindings_list = match bindings_expr {
                            Expr::List(bs) => bs,
                            _ => return Err(EvalError::Syntax("let: expected bindings list".into())),
                        };
                        let mut params = Vec::new();
                        let mut init_vals = Vec::new();
                        for b in bindings_list {
                            match b {
                                Expr::List(pair) if pair.len() == 2 => {
                                    match &pair[0] {
                                        Expr::Symbol(s) => {
                                            params.push(s.clone());
                                            init_vals.push(eval(&pair[1], env)?);
                                        }
                                        _ => return Err(EvalError::Syntax("let: expected variable name".into())),
                                    }
                                }
                                _ => return Err(EvalError::Syntax("let: bad binding".into())),
                            }
                        }
                        let body = elems[body_start..].to_vec();
                        if let Some(loop_name) = name {
                            // Named let: create a lambda and call it
                            let lambda = Value::Lambda {
                                name: Some(loop_name.clone()),
                                params: params.clone(),
                                body,
                                env: env.clone(),
                            };
                            return apply_func(&lambda, &init_vals);
                        }
                        let mut local_env = Env::with_parent(env.clone());
                        for (p, v) in params.iter().zip(init_vals.iter()) {
                            local_env.set(p.clone(), v.clone());
                        }
                        let mut result = Value::Void;
                        for expr in &elems[body_start..] {
                            result = eval(expr, &mut local_env)?;
                        }
                        return Ok(result);
                    }
                    "begin" => {
                        let mut result = Value::Void;
                        for expr in &elems[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                    "cond" => {
                        for clause in &elems[1..] {
                            match clause {
                                Expr::List(parts) if !parts.is_empty() => {
                                    // Check for else clause
                                    if let Expr::Symbol(s) = &parts[0] {
                                        if s == "else" {
                                            let mut result = Value::Void;
                                            for expr in &parts[1..] {
                                                result = eval(expr, env)?;
                                            }
                                            return Ok(result);
                                        }
                                    }
                                    let test = eval(&parts[0], env)?;
                                    if is_truthy(&test) {
                                        let mut result = test;
                                        for expr in &parts[1..] {
                                            result = eval(expr, env)?;
                                        }
                                        return Ok(result);
                                    }
                                }
                                _ => return Err(EvalError::Syntax("cond: bad clause".into())),
                            }
                        }
                        return Ok(Value::Void);
                    }
                    _ => {}
                }
            }

            // Function application
            let func = eval(&elems[0], env)?;
            let args: Vec<Value> = elems[1..].iter()
                .map(|a| eval(a, env))
                .collect::<Result<_, _>>()?;

            apply_func(&func, &args)
        }
    }
}

fn apply_func(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(name) => apply_builtin(name, args),
        Value::Lambda { name, params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let mut local_env = Env::with_parent(env.clone());
            // For named functions, bind self for recursion
            if let Some(n) = name {
                local_env.set(n.clone(), func.clone());
            }
            for (p, a) in params.iter().zip(args.iter()) {
                local_env.set(p.clone(), a.clone());
            }
            // Split body into leading defines and remaining expressions
            let mut define_exprs = Vec::new();
            let mut rest_exprs = Vec::new();
            let mut in_defines = true;
            for expr in body {
                if in_defines {
                    if let Expr::List(elems) = expr {
                        if let Some(Expr::Symbol(s)) = elems.first() {
                            if s == "define" {
                                define_exprs.push(expr);
                                continue;
                            }
                        }
                    }
                    in_defines = false;
                }
                rest_exprs.push(expr);
            }
            // Evaluate all defines
            for expr in &define_exprs {
                eval(expr, &mut local_env)?;
            }
            // Patch internal define closures to see all siblings
            if define_exprs.len() > 1 {
                let define_names: Vec<String> = define_exprs.iter().filter_map(|expr| {
                    if let Expr::List(elems) = expr {
                        match &elems[1] {
                            Expr::Symbol(n) => Some(n.clone()),
                            Expr::List(sig) if !sig.is_empty() => {
                                if let Expr::Symbol(n) = &sig[0] { Some(n.clone()) } else { None }
                            }
                            _ => None,
                        }
                    } else { None }
                }).collect();
                let final_bindings: Vec<(String, Value)> = define_names.iter()
                    .filter_map(|n| local_env.bindings.get(n).map(|v| (n.clone(), v.clone())))
                    .collect();
                for dn in &define_names {
                    if let Some(Value::Lambda { env: ref mut closure_env, .. }) = local_env.bindings.get_mut(dn) {
                        for (sib_name, sib_val) in &final_bindings {
                            closure_env.set(sib_name.clone(), sib_val.clone());
                        }
                    }
                }
            }
            // Evaluate remaining expressions
            let mut result = Value::Void;
            for expr in &rest_exprs {
                result = eval(expr, &mut local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type(format!("not a procedure: {func}"))),
    }
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_int(a)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                Ok(Value::Integer(-as_int(&args[0])?))
            } else {
                let mut result = as_int(&args[0])?;
                for a in &args[1..] {
                    result -= as_int(a)?;
                }
                Ok(Value::Integer(result))
            }
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_int(a)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let mut result = as_int(&args[0])?;
            for a in &args[1..] {
                let divisor = as_int(a)?;
                if divisor == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= divisor;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            let vals = args_to_ints(args)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] < w[1])))
        }
        ">" => {
            let vals = args_to_ints(args)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] > w[1])))
        }
        "=" => {
            let vals = args_to_ints(args)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] == w[1])))
        }
        "<=" => {
            let vals = args_to_ints(args)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] <= w[1])))
        }
        ">=" => {
            let vals = args_to_ints(args)?;
            Ok(Value::Boolean(vals.windows(2).all(|w| w[0] >= w[1])))
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
                    // Dotted pair - for now represent as 2-element list
                    Ok(Value::List(vec![args[0].clone(), args[1].clone()]))
                }
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(EvalError::Type("car: not a pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(elems) if !elems.is_empty() => {
                    Ok(Value::List(elems[1..].to_vec()))
                }
                _ => Err(EvalError::Type("cdr: not a pair".into())),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("null? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(v) if v.is_empty())))
        }
        "list" => {
            Ok(Value::List(args.to_vec()))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                _ => Err(EvalError::Type("length: not a list".into())),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for (i, arg) in args.iter().enumerate() {
                if i < args.len() - 1 {
                    match arg {
                        Value::List(elems) => result.extend(elems.iter().cloned()),
                        _ => return Err(EvalError::Type("append: not a list".into())),
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
            Ok(Value::Boolean(matches!(&args[0], Value::List(v) if !v.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        _ => Err(EvalError::Unbound(name.to_string())),
    }
}

fn expr_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(n) => Value::Integer(*n),
        Expr::Boolean(b) => Value::Boolean(*b),
        Expr::Str(s) => Value::Str(s.clone()),
        Expr::Symbol(s) => Value::Symbol(s.clone()),
        Expr::List(elems) => Value::List(elems.iter().map(expr_to_value).collect()),
    }
}

fn as_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected integer, got {v}"))),
    }
}

fn args_to_ints(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|a| as_int(a)).collect()
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
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &mut env)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
