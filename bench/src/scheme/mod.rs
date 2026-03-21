pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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

type EnvCell = Rc<RefCell<Value>>;
type Env = HashMap<String, EnvCell>;

fn env_get(env: &Env, name: &str) -> Option<Value> {
    env.get(name).map(|cell| cell.borrow().clone())
}

fn env_set(env: &mut Env, name: String, val: Value) {
    env.insert(name, Rc::new(RefCell::new(val)));
}

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

/// Evaluate a parsed Scheme expression with tail call optimization.
fn eval(ast: &Ast, env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let mut cur_ast = ast.clone();
    let mut tco_env: Option<Env> = None;

    'tco: loop {
        let e = match &mut tco_env {
            Some(le) => le,
            None => env,
        };
        let line = cur_ast.line;
        let col = cur_ast.col;
        match cur_ast.kind.clone() {
            AstKind::Integer(n) => return Ok(Value::Integer(n)),
            AstKind::Boolean(b) => return Ok(Value::Boolean(b)),
            AstKind::Str(s) => return Ok(Value::Str(s)),
            AstKind::Char(c) => return Ok(Value::Char(c)),
            AstKind::Symbol(s) => {
                return env_get(e, &s)
                    .ok_or_else(|| EvalError::UndefinedVariable(s).with_position(line, col));
            }
            AstKind::List(items) => {
                if items.is_empty() {
                    return Err(EvalError::Parse("empty application".into())
                        .with_position(line, col));
                }
                // Check for special forms by symbol name
                if let AstKind::Symbol(ref op) = items[0].kind {
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
                                    let val = eval(&items[2], e, out)?;
                                    env_set(e, name.clone(), val);
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
                                        env: e.clone(),
                                    };
                                    env_set(e, name.clone(), lambda);
                                    // Re-capture env so recursive calls see the binding
                                    // Mutate the existing cell so the Rc in the closure's env updates too
                                    let lambda = Value::Lambda {
                                        params,
                                        body: Box::new(body),
                                        env: e.clone(),
                                    };
                                    *e.get(&name).unwrap().borrow_mut() = lambda;
                                    return Ok(Value::Symbol("ok".into()));
                                }
                                _ => {
                                    return Err(EvalError::TypeError(
                                        "define expects symbol or list".into(),
                                    ).with_position(line, col))
                                }
                            }
                        }
                        "set!" => {
                            if items.len() != 3 {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            let name = match &items[1].kind {
                                AstKind::Symbol(s) => s.clone(),
                                _ => return Err(EvalError::TypeError(
                                    "set!: expected symbol".into(),
                                ).with_position(line, col)),
                            };
                            let val = eval(&items[2], e, out)?;
                            match e.get(&name) {
                                Some(cell) => {
                                    *cell.borrow_mut() = val;
                                    return Ok(Value::Symbol("ok".into()));
                                }
                                None => {
                                    return Err(EvalError::UndefinedVariable(name)
                                        .with_position(line, col));
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
                                env: e.clone(),
                            });
                        }
                        "if" => {
                            if items.len() < 3 {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            let cond = eval(&items[1], e, out)?;
                            if cond != Value::Boolean(false) {
                                cur_ast = items[2].clone();
                                continue;
                            } else if items.len() > 3 {
                                cur_ast = items[3].clone();
                                continue;
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
                            if items.len() <= 1 {
                                return Ok(Value::Boolean(true));
                            }
                            for a in &items[1..items.len() - 1] {
                                let result = eval(a, e, out)?;
                                if result == Value::Boolean(false) {
                                    return Ok(result);
                                }
                            }
                            cur_ast = items.last().unwrap().clone();
                            continue;
                        }
                        "or" => {
                            if items.len() <= 1 {
                                return Ok(Value::Boolean(false));
                            }
                            for a in &items[1..items.len() - 1] {
                                let result = eval(a, e, out)?;
                                if result != Value::Boolean(false) {
                                    return Ok(result);
                                }
                            }
                            cur_ast = items.last().unwrap().clone();
                            continue;
                        }
                        "begin" => {
                            if items.len() <= 1 {
                                return Ok(Value::Symbol("ok".into()));
                            }
                            for expr in &items[1..items.len() - 1] {
                                eval(expr, e, out)?;
                            }
                            cur_ast = items.last().unwrap().clone();
                            continue;
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
                            let mut local_env = e.clone();
                            for b in bindings {
                                match &b.kind {
                                    AstKind::List(pair) if pair.len() == 2 => {
                                        let name = match &pair[0].kind {
                                            AstKind::Symbol(s) => s.clone(),
                                            _ => return Err(EvalError::TypeError("let: binding name must be symbol".into())
                                                .with_position(pair[0].line, pair[0].col)),
                                        };
                                        let val = eval(&pair[1], e, out)?;
                                        env_set(&mut local_env, name, val);
                                    }
                                    _ => return Err(EvalError::TypeError("let: bad binding".into())
                                        .with_position(b.line, b.col)),
                                }
                            }
                            for expr in &items[2..items.len() - 1] {
                                eval(expr, &mut local_env, out)?;
                            }
                            cur_ast = items.last().unwrap().clone();
                            tco_env = Some(local_env);
                            continue;
                        }
                        "string-set!" => {
                            return Err(EvalError::TypeError(
                                "string-set!: strings are immutable".into(),
                            ).with_position(line, col));
                        }
                        "cond" => {
                            for clause in &items[1..] {
                                match &clause.kind {
                                    AstKind::List(parts) if parts.len() >= 2 => {
                                        if let AstKind::Symbol(s) = &parts[0].kind {
                                            if s == "else" {
                                                for expr in &parts[1..parts.len() - 1] {
                                                    eval(expr, e, out)?;
                                                }
                                                cur_ast = parts.last().unwrap().clone();
                                                continue 'tco;
                                            }
                                        }
                                        let test = eval(&parts[0], e, out)?;
                                        if test != Value::Boolean(false) {
                                            for expr in &parts[1..parts.len() - 1] {
                                                eval(expr, e, out)?;
                                            }
                                            cur_ast = parts.last().unwrap().clone();
                                            continue 'tco;
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
                    args.push(eval(a, e, out)?);
                }
                // Try builtin first if operator is a symbol not in env
                if let AstKind::Symbol(s) = &items[0].kind {
                    if !e.contains_key(s.as_str()) {
                        return apply_builtin(s, &args, out)
                            .map_err(|err| err.with_position(line, col));
                    }
                }
                let func = eval(&items[0], e, out)?;
                match func {
                    Value::Lambda {
                        params,
                        body,
                        env: closed_env,
                    } => {
                        if args.len() != params.len() {
                            return Err(EvalError::Arity.with_position(line, col));
                        }
                        let mut local_env = closed_env.clone();
                        for (p, a) in params.iter().zip(args) {
                            env_set(&mut local_env, p.clone(), a);
                        }
                        cur_ast = *body;
                        tco_env = Some(local_env);
                        continue;
                    }
                    _ => return Err(EvalError::NotAProcedure.with_position(items[0].line, items[0].col)),
                }
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
        "map" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            let func = &args[0];
            let items = match &args[1] {
                Value::List(items) => items,
                _ => return Err(EvalError::TypeError("map: expected list".into())),
            };
            let mut results = Vec::new();
            for item in items {
                match func {
                    Value::Lambda { params, body, env: closed_env } => {
                        if params.len() != 1 {
                            return Err(EvalError::Arity);
                        }
                        let mut local_env = closed_env.clone();
                        env_set(&mut local_env, params[0].clone(), item.clone());
                        results.push(eval(&body, &mut local_env, out)?);
                    }
                    _ => return Err(EvalError::NotAProcedure),
                }
            }
            Ok(Value::List(results))
        }
        "string->list" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Str(s) => Ok(Value::List(s.chars().map(Value::Char).collect())),
                _ => Err(EvalError::TypeError("string->list: expected string".into())),
            }
        }
        "list->string" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::List(items) => {
                    let mut s = String::new();
                    for item in items {
                        match item {
                            Value::Char(c) => s.push(*c),
                            _ => return Err(EvalError::TypeError("list->string: expected list of characters".into())),
                        }
                    }
                    Ok(Value::Str(s))
                }
                _ => Err(EvalError::TypeError("list->string: expected list".into())),
            }
        }
        "char->integer" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => Err(EvalError::TypeError("char->integer: expected character".into())),
            }
        }
        "integer->char" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            let n = expect_integer(&args[0])?;
            Ok(Value::Char(char::from_u32(n as u32).unwrap_or('\0')))
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
