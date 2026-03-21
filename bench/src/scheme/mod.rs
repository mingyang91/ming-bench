pub mod error;

pub use error::EvalError;

use std::collections::HashMap;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        body: Box<Ast>,
        env: Env,
    },
}

type Env = HashMap<String, Value>;

impl Value {
    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::Char(c) => format!("#\\{}", c),
            Value::Lambda { .. } => "#<procedure>".to_string(),
            Value::List(items) => {
                let parts: Vec<String> = items.iter().map(|v| v.to_scheme_string()).collect();
                format!("({})", parts.join(" "))
            }
        }
    }
}

/// Token with source position.
struct Token {
    text: String,
    line: usize,
    col: usize,
}

/// AST node with source position.
#[derive(Debug, Clone, PartialEq)]
struct Ast {
    kind: AstKind,
    line: usize,
    col: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum AstKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Ast>),
}

/// Tokenize input into a list of tokens with positions.
fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;
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
                        s.push(chars[i + 1]);
                        if chars[i + 1] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 2;
                        }
                        i += 2;
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
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' | ')' | '\'' => {
                tokens.push(Token {
                    text: chars[i].to_string(),
                    line,
                    col,
                });
                i += 1;
                col += 1;
            }
            _ => {
                let start_col = col;
                let start_line = line;
                let mut s = String::new();
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
                {
                    s.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: s, line: start_line, col: start_col });
            }
        }
    }
    tokens
}

/// Parse a single expression from tokens, returning (ast, next_index).
fn parse(tokens: &[Token], pos: usize) -> Result<(Ast, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let token = &tokens[pos];
    let line = token.line;
    let col = token.col;
    if token.text == "(" {
        let mut items = Vec::new();
        let mut i = pos + 1;
        while i < tokens.len() && tokens[i].text != ")" {
            let (val, next) = parse(tokens, i)?;
            items.push(val);
            i = next;
        }
        if i >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".into())
                .with_position(line, col));
        }
        Ok((Ast { kind: AstKind::List(items), line, col }, i + 1))
    } else if token.text == "'" {
        let (inner, next) = parse(tokens, pos + 1)?;
        Ok((
            Ast {
                kind: AstKind::List(vec![
                    Ast { kind: AstKind::Symbol("quote".into()), line, col },
                    inner,
                ]),
                line,
                col,
            },
            next,
        ))
    } else if token.text == ")" {
        Err(EvalError::Parse("unexpected )".into()).with_position(line, col))
    } else if token.text.starts_with("#\\") {
        let ch = if token.text.len() == 3 {
            token.text.chars().nth(2).unwrap()
        } else {
            match &token.text[2..] {
                "space" => ' ',
                "newline" => '\n',
                "tab" => '\t',
                _ => return Err(EvalError::Parse(format!("unknown character literal: {}", token.text))
                    .with_position(line, col)),
            }
        };
        Ok((Ast { kind: AstKind::Char(ch), line, col }, pos + 1))
    } else if token.text == "#t" {
        Ok((Ast { kind: AstKind::Boolean(true), line, col }, pos + 1))
    } else if token.text == "#f" {
        Ok((Ast { kind: AstKind::Boolean(false), line, col }, pos + 1))
    } else if token.text.starts_with('"') {
        let inner = &token.text[1..token.text.len() - 1];
        Ok((Ast { kind: AstKind::Str(inner.to_string()), line, col }, pos + 1))
    } else if let Ok(n) = token.text.parse::<i64>() {
        Ok((Ast { kind: AstKind::Integer(n), line, col }, pos + 1))
    } else {
        Ok((Ast { kind: AstKind::Symbol(token.text.clone()), line, col }, pos + 1))
    }
}

/// Convert an AST back to a Value (for quote).
fn ast_to_value(ast: &Ast) -> Value {
    match &ast.kind {
        AstKind::Integer(n) => Value::Integer(*n),
        AstKind::Boolean(b) => Value::Boolean(*b),
        AstKind::Str(s) => Value::Str(s.clone()),
        AstKind::Symbol(s) => Value::Symbol(s.clone()),
        AstKind::Char(c) => Value::Char(*c),
        AstKind::List(items) => Value::List(items.iter().map(ast_to_value).collect()),
    }
}

/// Evaluate a parsed Scheme expression.
fn eval(ast: &Ast, env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let line = ast.line;
    let col = ast.col;
    match &ast.kind {
        AstKind::Integer(n) => Ok(Value::Integer(*n)),
        AstKind::Boolean(b) => Ok(Value::Boolean(*b)),
        AstKind::Str(s) => Ok(Value::Str(s.clone())),
        AstKind::Char(c) => Ok(Value::Char(*c)),
        AstKind::Symbol(s) => env
            .get(s)
            .cloned()
            .ok_or_else(|| EvalError::UndefinedVariable(s.clone()).with_position(line, col)),
        AstKind::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into())
                    .with_position(line, col));
            }
            // Check for special forms by symbol name
            if let AstKind::Symbol(op) = &items[0].kind {
                match op.as_str() {
                    "define" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity.with_position(line, col));
                        }
                        match &items[1].kind {
                            AstKind::Symbol(name) => {
                                if items.len() != 3 {
                                    return Err(EvalError::Arity.with_position(line, col));
                                }
                                let val = eval(&items[2], env, out)?;
                                env.insert(name.clone(), val);
                                return Ok(Value::Symbol("ok".into()));
                            }
                            AstKind::List(sig) => {
                                if sig.is_empty() {
                                    return Err(EvalError::Parse(
                                        "define: empty signature".into(),
                                    ).with_position(line, col));
                                }
                                let name = match &sig[0].kind {
                                    AstKind::Symbol(s) => s.clone(),
                                    _ => {
                                        return Err(EvalError::TypeError(
                                            "define expects symbol".into(),
                                        ).with_position(line, col))
                                    }
                                };
                                let params: Result<Vec<String>, _> = sig[1..]
                                    .iter()
                                    .map(|v| match &v.kind {
                                        AstKind::Symbol(s) => Ok(s.clone()),
                                        _ => Err(EvalError::TypeError(
                                            "parameter must be symbol".into(),
                                        ).with_position(v.line, v.col)),
                                    })
                                    .collect();
                                let params = params?;
                                let body = if items.len() == 3 {
                                    items[2].clone()
                                } else {
                                    let mut begin_items = vec![Ast {
                                        kind: AstKind::Symbol("begin".into()),
                                        line,
                                        col,
                                    }];
                                    begin_items.extend(items[2..].iter().cloned());
                                    Ast {
                                        kind: AstKind::List(begin_items),
                                        line,
                                        col,
                                    }
                                };
                                let lambda = Value::Lambda {
                                    params: params.clone(),
                                    body: Box::new(body.clone()),
                                    env: env.clone(),
                                };
                                env.insert(name.clone(), lambda);
                                let lambda = Value::Lambda {
                                    params,
                                    body: Box::new(body),
                                    env: env.clone(),
                                };
                                env.insert(name, lambda);
                                return Ok(Value::Symbol("ok".into()));
                            }
                            _ => {
                                return Err(EvalError::TypeError(
                                    "define expects symbol or list".into(),
                                ).with_position(line, col))
                            }
                        }
                    }
                    "lambda" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity.with_position(line, col));
                        }
                        let param_list = match &items[1].kind {
                            AstKind::List(ps) => ps,
                            _ => {
                                return Err(EvalError::TypeError(
                                    "lambda params must be a list".into(),
                                ).with_position(line, col))
                            }
                        };
                        let params: Result<Vec<String>, _> = param_list
                            .iter()
                            .map(|v| match &v.kind {
                                AstKind::Symbol(s) => Ok(s.clone()),
                                _ => Err(EvalError::TypeError(
                                    "parameter must be symbol".into(),
                                ).with_position(v.line, v.col)),
                            })
                            .collect();
                        let body = if items.len() == 3 {
                            items[2].clone()
                        } else {
                            let mut begin_items = vec![Ast {
                                kind: AstKind::Symbol("begin".into()),
                                line,
                                col,
                            }];
                            begin_items.extend(items[2..].iter().cloned());
                            Ast {
                                kind: AstKind::List(begin_items),
                                line,
                                col,
                            }
                        };
                        return Ok(Value::Lambda {
                            params: params?,
                            body: Box::new(body),
                            env: env.clone(),
                        });
                    }
                    "if" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity.with_position(line, col));
                        }
                        let cond = eval(&items[1], env, out)?;
                        if cond != Value::Boolean(false) {
                            return eval(&items[2], env, out);
                        } else if items.len() > 3 {
                            return eval(&items[3], env, out);
                        } else {
                            return Ok(Value::Symbol("ok".into()));
                        }
                    }
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity.with_position(line, col));
                        }
                        return Ok(ast_to_value(&items[1]));
                    }
                    "and" => {
                        let mut result = Value::Boolean(true);
                        for a in &items[1..] {
                            result = eval(a, env, out)?;
                            if result == Value::Boolean(false) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        let mut result = Value::Boolean(false);
                        for a in &items[1..] {
                            result = eval(a, env, out)?;
                            if result != Value::Boolean(false) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "begin" => {
                        let mut result = Value::Symbol("ok".into());
                        for expr in &items[1..] {
                            result = eval(expr, env, out)?;
                        }
                        return Ok(result);
                    }
                    "let" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity.with_position(line, col));
                        }
                        let bindings = match &items[1].kind {
                            AstKind::List(bs) => bs,
                            _ => return Err(EvalError::TypeError("let: bindings must be a list".into())
                                .with_position(line, col)),
                        };
                        let mut local_env = env.clone();
                        for b in bindings {
                            match &b.kind {
                                AstKind::List(pair) if pair.len() == 2 => {
                                    let name = match &pair[0].kind {
                                        AstKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::TypeError("let: binding name must be symbol".into())
                                            .with_position(pair[0].line, pair[0].col)),
                                    };
                                    let val = eval(&pair[1], env, out)?;
                                    local_env.insert(name, val);
                                }
                                _ => return Err(EvalError::TypeError("let: bad binding".into())
                                    .with_position(b.line, b.col)),
                            }
                        }
                        let mut result = Value::Symbol("ok".into());
                        for expr in &items[2..] {
                            result = eval(expr, &mut local_env, out)?;
                        }
                        return Ok(result);
                    }
                    "string-set!" => {
                        if items.len() != 4 {
                            return Err(EvalError::Arity.with_position(line, col));
                        }
                        let var_name = match &items[1].kind {
                            AstKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::TypeError(
                                "string-set!: first argument must be a variable".into(),
                            ).with_position(items[1].line, items[1].col)),
                        };
                        let idx = expect_integer(&eval(&items[2], env, out)?)?;
                        let ch = match eval(&items[3], env, out)? {
                            Value::Char(c) => c,
                            _ => return Err(EvalError::TypeError(
                                "string-set!: third argument must be a character".into(),
                            ).with_position(items[3].line, items[3].col)),
                        };
                        let s = env.get_mut(&var_name).ok_or_else(|| {
                            EvalError::UndefinedVariable(var_name.clone())
                                .with_position(items[1].line, items[1].col)
                        })?;
                        return match s {
                            Value::Str(ref mut string) => {
                                let idx = idx as usize;
                                if idx >= string.len() {
                                    return Err(EvalError::TypeError(
                                        "string-set!: index out of range".into(),
                                    ).with_position(line, col));
                                }
                                unsafe { string.as_bytes_mut()[idx] = ch as u8; }
                                Ok(Value::Symbol("ok".into()))
                            }
                            _ => Err(EvalError::TypeError(
                                "string-set!: not a string".into(),
                            ).with_position(line, col)),
                        };
                    }
                    "cond" => {
                        for clause in &items[1..] {
                            match &clause.kind {
                                AstKind::List(parts) if parts.len() >= 2 => {
                                    if let AstKind::Symbol(s) = &parts[0].kind {
                                        if s == "else" {
                                            let mut result = Value::Symbol("ok".into());
                                            for expr in &parts[1..] {
                                                result = eval(expr, env, out)?;
                                            }
                                            return Ok(result);
                                        }
                                    }
                                    let test = eval(&parts[0], env, out)?;
                                    if test != Value::Boolean(false) {
                                        let mut result = Value::Symbol("ok".into());
                                        for expr in &parts[1..] {
                                            result = eval(expr, env, out)?;
                                        }
                                        return Ok(result);
                                    }
                                }
                                _ => return Err(EvalError::TypeError("cond: bad clause".into())
                                    .with_position(clause.line, clause.col)),
                            }
                        }
                        return Ok(Value::Symbol("ok".into()));
                    }
                    _ => {}
                }
            }
            // General application: evaluate operator and arguments
            let mut args = Vec::new();
            for a in &items[1..] {
                args.push(eval(a, env, out)?);
            }
            // Try builtin first if operator is a symbol not in env
            if let AstKind::Symbol(s) = &items[0].kind {
                if !env.contains_key(s.as_str()) {
                    return apply_builtin(s, &args, out)
                        .map_err(|e| e.with_position(line, col));
                }
            }
            let func = eval(&items[0], env, out)?;
            match func {
                Value::Lambda {
                    params,
                    body,
                    env: closed_env,
                } => {
                    if args.len() != params.len() {
                        return Err(EvalError::Arity.with_position(line, col));
                    }
                    let mut local_env = env.clone();
                    for (k, v) in &closed_env {
                        local_env.insert(k.clone(), v.clone());
                    }
                    for (p, a) in params.iter().zip(args) {
                        local_env.insert(p.clone(), a);
                    }
                    eval(&body, &mut local_env, out)
                }
                _ => Err(EvalError::NotAProcedure.with_position(items[0].line, items[0].col)),
            }
        }
    }
}

fn expect_integer(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::TypeError("expected integer".into())),
    }
}

fn apply_builtin(op: &str, args: &[Value], out: &mut String) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_integer(a)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity);
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-expect_integer(&args[0])?));
            }
            let mut result = expect_integer(&args[0])?;
            for a in &args[1..] {
                result -= expect_integer(a)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= expect_integer(a)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity);
            }
            let mut result = expect_integer(&args[0])?;
            for a in &args[1..] {
                let d = expect_integer(a)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? < expect_integer(&args[1])?))
        }
        ">" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? > expect_integer(&args[1])?))
        }
        "=" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? == expect_integer(&args[1])?))
        }
        "<=" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? <= expect_integer(&args[1])?))
        }
        ">=" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? >= expect_integer(&args[1])?))
        }
        "not" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(args[0] == Value::Boolean(false)))
        }
        "cons" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            match &args[1] {
                Value::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => Err(EvalError::TypeError("cons: second argument must be a list".into())),
            }
        }
        "car" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                Value::List(_) => Err(EvalError::TypeError("car: empty list".into())),
                _ => Err(EvalError::TypeError("car: not a pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
                Value::List(_) => Err(EvalError::TypeError("cdr: empty list".into())),
                _ => Err(EvalError::TypeError("cdr: not a pair".into())),
            }
        }
        "null?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => {
            Ok(Value::List(args.to_vec()))
        }
        "length" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(EvalError::TypeError("length: not a list".into())),
            }
        }
        "string?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "number?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "display" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Str(s) => out.push_str(s),
                other => out.push_str(&other.to_scheme_string()),
            }
            Ok(Value::Symbol("ok".into()))
        }
        "write" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            out.push_str(&args[0].to_scheme_string());
            Ok(Value::Symbol("ok".into()))
        }
        "newline" => {
            if !args.is_empty() { return Err(EvalError::Arity); }
            out.push('\n');
            Ok(Value::Symbol("ok".into()))
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::TypeError("string-append: expected string".into())),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::TypeError("string-length: expected string".into())),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity); }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::TypeError("substring: expected string".into())),
            };
            let start = expect_integer(&args[1])? as usize;
            let end = expect_integer(&args[2])? as usize;
            if start > end || end > s.len() {
                return Err(EvalError::TypeError("substring: index out of range".into()));
            }
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::TypeError("string->number: expected string".into())),
            }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            let n = expect_integer(&args[0])?;
            Ok(Value::Str(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::TypeError("symbol->string: expected symbol".into())),
            }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::TypeError("string->symbol: expected string".into())),
            }
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::TypeError("string-copy: expected string".into())),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::TypeError("string-ref: expected string".into())),
            };
            let idx = expect_integer(&args[1])? as usize;
            if idx >= s.len() {
                return Err(EvalError::TypeError("string-ref: index out of range".into()));
            }
            Ok(Value::Char(s.as_bytes()[idx] as char))
        }
        "char?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        _ => Err(EvalError::UndefinedVariable(op.to_string())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut last_result = None;
    let mut env = Env::new();
    let mut out = String::new();
    while pos < tokens.len() {
        let (ast, next) = parse(&tokens, pos)?;
        let result = eval(&ast, &mut env, &mut out)?;
        last_result = Some(result);
        pos = next;
    }
    match last_result {
        Some(v) => Ok(v.to_scheme_string()),
        None => Err(EvalError::Parse("empty input".into())),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut last_result = None;
    let mut env = Env::new();
    let mut out = String::new();
    while pos < tokens.len() {
        let (ast, next) = parse(&tokens, pos)?;
        let result = eval(&ast, &mut env, &mut out)?;
        last_result = Some(result);
        pos = next;
    }
    match last_result {
        Some(v) => Ok((v.to_scheme_string(), out)),
        None => Err(EvalError::Parse("empty input".into())),
    }
}

#[cfg(test)]
mod tests;
