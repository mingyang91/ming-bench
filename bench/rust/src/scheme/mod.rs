pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
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

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
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
    // Last arg is returned as-is (tail). All others must be proper lists.
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
        ("append", builtin_append),
    ];
    for &(name, func) in builtins {
        env_set(&env, name.to_string(), Value::Builtin(name, func));
    }
    env
}

// --- Parser ---

fn tokenize(input: &str) -> Vec<String> {
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
            '(' | ')' => {
                tokens.push(chars[i].to_string());
                i += 1;
            }
            '\'' => {
                tokens.push("'".to_string());
                i += 1;
            }
            '"' => {
                let mut s = String::new();
                s.push('"');
                i += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        i += 1;
                        s.push(chars[i]);
                        i += 1;
                    } else {
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                }
                tokens.push(s);
            }
            '#' if i + 1 < chars.len() && (chars[i + 1] == 't' || chars[i + 1] == 'f') => {
                let mut tok = String::from('#');
                i += 1;
                tok.push(chars[i]);
                i += 1;
                tokens.push(tok);
            }
            _ => {
                let mut tok = String::new();
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '\'')
                {
                    tok.push(chars[i]);
                    i += 1;
                }
                tokens.push(tok);
            }
        }
    }
    tokens
}

fn parse_tokens(tokens: &[String], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let token = &tokens[*pos];
    if token == "'" {
        *pos += 1;
        let inner = parse_tokens(tokens, pos)?;
        Ok(Expr::List(vec![Expr::Symbol("quote".into()), inner]))
    } else if token == "(" {
        *pos += 1;
        let mut list = Vec::new();
        while *pos < tokens.len() && tokens[*pos] != ")" {
            list.push(parse_tokens(tokens, pos)?);
        }
        if *pos >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".into()));
        }
        *pos += 1;
        Ok(Expr::List(list))
    } else if token == ")" {
        Err(EvalError::Parse("unexpected ')'".into()))
    } else if token == "#t" {
        *pos += 1;
        Ok(Expr::Boolean(true))
    } else if token == "#f" {
        *pos += 1;
        Ok(Expr::Boolean(false))
    } else if token.starts_with('"') {
        *pos += 1;
        let inner = &token[1..token.len() - 1];
        Ok(Expr::Str(inner.to_string()))
    } else if let Ok(n) = token.parse::<i64>() {
        *pos += 1;
        Ok(Expr::Integer(n))
    } else {
        *pos += 1;
        Ok(Expr::Symbol(token.clone()))
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
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => env_get(env, name),
        Expr::List(items) => {
            if items.is_empty() {
                return Err(EvalError::BadSyntax("empty application".into()));
            }
            // Check for special forms
            if let Expr::Symbol(ref op) = items[0] {
                match op.as_str() {
                    "define" => return eval_define(&items[1..], env),
                    "if" => return eval_if(&items[1..], env),
                    "quote" => return eval_quote(&items[1..]),
                    "lambda" => return eval_lambda(&items[1..], env),
                    "let" => return eval_let(&items[1..], env),
                    "begin" => return eval_begin(&items[1..], env),
                    "cond" => return eval_cond(&items[1..], env),
                    "and" => return eval_and(&items[1..], env),
                    "or" => return eval_or(&items[1..], env),
                    _ => {}
                }
            }
            // Function application
            let func = eval(&items[0], env)?;
            let args: Result<Vec<Value>, _> =
                items[1..].iter().map(|a| eval(a, env)).collect();
            let args = args?;
            apply_func(&func, &args)
        }
    }
}

fn apply_func(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Lambda {
            params,
            body,
            env: closure_env,
        } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}",
                    params.len(),
                    args.len()
                )));
            }
            let local_env = new_env(Some(closure_env.clone()));
            for (p, a) in params.iter().zip(args.iter()) {
                env_set(&local_env, p.clone(), a.clone());
            }
            let mut result = Value::Void;
            for e in body {
                result = eval(e, &local_env)?;
            }
            Ok(result)
        }
        Value::Builtin(_, func_ptr) => func_ptr(args),
        _ => Err(EvalError::Type(format!(
            "not a procedure: {}",
            func.display()
        ))),
    }
}

fn eval_define(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::BadSyntax("define: missing name".into()));
    }
    match &args[0] {
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::BadSyntax("define: expected 2 parts".into()));
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        Expr::List(sig) => {
            // (define (f params...) body...)
            if sig.is_empty() {
                return Err(EvalError::BadSyntax("define: empty signature".into()));
            }
            let name = match &sig[0] {
                Expr::Symbol(n) => n.clone(),
                _ => return Err(EvalError::BadSyntax("define: expected symbol".into())),
            };
            let params: Result<Vec<String>, _> = sig[1..]
                .iter()
                .map(|e| match e {
                    Expr::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::BadSyntax("define: expected parameter name".into())),
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
        _ => Err(EvalError::BadSyntax("define: bad syntax".into())),
    }
}

fn eval_if(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::BadSyntax("if: expected 2 or 3 parts".into()));
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

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::BadSyntax("quote: expected 1 argument".into()));
    }
    expr_to_value(&args[0])
}

fn expr_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(s) => Ok(Value::Symbol(s.clone())),
        Expr::List(items) => {
            let mut result = Value::Nil;
            for item in items.iter().rev() {
                let v = expr_to_value(item)?;
                result = Value::Pair(Box::new(v), Box::new(result));
            }
            Ok(result)
        }
    }
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::BadSyntax(
            "lambda: expected params and body".into(),
        ));
    }
    let params = match &args[0] {
        Expr::List(param_exprs) => {
            let mut params = Vec::new();
            for p in param_exprs {
                match p {
                    Expr::Symbol(s) => params.push(s.clone()),
                    _ => {
                        return Err(EvalError::BadSyntax(
                            "lambda: expected parameter name".into(),
                        ))
                    }
                }
            }
            params
        }
        _ => {
            return Err(EvalError::BadSyntax(
                "lambda: expected parameter list".into(),
            ))
        }
    };
    Ok(Value::Lambda {
        params,
        body: args[1..].to_vec(),
        env: env.clone(),
    })
}

fn eval_let(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::BadSyntax("let: expected bindings and body".into()));
    }
    // Named let: (let name ((var init) ...) body...)
    if let Expr::Symbol(name) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::BadSyntax("let: expected bindings and body".into()));
        }
        let bindings = match &args[1] {
            Expr::List(b) => b,
            _ => return Err(EvalError::BadSyntax("let: expected binding list".into())),
        };
        let mut param_names = Vec::new();
        let mut init_vals = Vec::new();
        for binding in bindings {
            match binding {
                Expr::List(pair) if pair.len() == 2 => {
                    let pname = match &pair[0] {
                        Expr::Symbol(s) => s.clone(),
                        _ => return Err(EvalError::BadSyntax("let: expected symbol".into())),
                    };
                    init_vals.push(eval(&pair[1], env)?);
                    param_names.push(pname);
                }
                _ => return Err(EvalError::BadSyntax("let: bad binding".into())),
            }
        }
        let body = args[2..].to_vec();
        let local_env = new_env(Some(env.clone()));
        let lambda = Value::Lambda {
            params: param_names.clone(),
            body,
            env: local_env.clone(),
        };
        env_set(&local_env, name.clone(), lambda);
        for (p, v) in param_names.iter().zip(init_vals.iter()) {
            env_set(&local_env, p.clone(), v.clone());
        }
        let mut result = Value::Void;
        for e in &args[2..] {
            result = eval(e, &local_env)?;
        }
        return Ok(result);
    }
    let bindings = match &args[0] {
        Expr::List(b) => b,
        _ => return Err(EvalError::BadSyntax("let: expected binding list".into())),
    };
    let local_env = new_env(Some(env.clone()));
    for binding in bindings {
        match binding {
            Expr::List(pair) if pair.len() == 2 => {
                let name = match &pair[0] {
                    Expr::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::BadSyntax("let: expected symbol in binding".into())),
                };
                let val = eval(&pair[1], env)?;
                env_set(&local_env, name, val);
            }
            _ => return Err(EvalError::BadSyntax("let: bad binding".into())),
        }
    }
    let mut result = Value::Void;
    for e in &args[1..] {
        result = eval(e, &local_env)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for e in args {
        result = eval(e, env)?;
    }
    Ok(result)
}

fn eval_cond(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    for clause in args {
        match clause {
            Expr::List(items) if !items.is_empty() => {
                // Check for else clause
                if let Expr::Symbol(ref s) = items[0] {
                    if s == "else" {
                        let mut result = Value::Void;
                        for e in &items[1..] {
                            result = eval(e, env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&items[0], env)?;
                if test.is_truthy() {
                    if items.len() == 1 {
                        return Ok(test);
                    }
                    let mut result = Value::Void;
                    for e in &items[1..] {
                        result = eval(e, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::BadSyntax("cond: bad clause".into())),
        }
    }
    Ok(Value::Void)
}

fn eval_and(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for a in args {
        result = eval(a, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for a in args {
        result = eval(a, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
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

pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    let result = eval_str(_input)?;
    Ok((result, String::new()))
}

#[cfg(test)]
mod tests;
