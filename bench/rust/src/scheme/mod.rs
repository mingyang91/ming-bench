pub mod error;

pub use error::EvalError;

use std::collections::HashMap;

/// Source position (1-indexed line, 0-indexed column).
#[derive(Debug, Clone, Copy)]
struct Pos {
    line: usize,
    col: usize,
}

impl Pos {
    fn fmt(&self) -> String {
        format!("{}:{}", self.line, self.col)
    }
}

/// A Scheme value.
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
            Value::Lambda { .. } => "<procedure>".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self, pos: Pos) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::Type(format!(
                "expected integer, got {} at {}",
                other.display(),
                pos.fmt()
            ))),
        }
    }
}

/// A parsed S-expression with source position.
#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    pos: Pos,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

// ── Environment ────────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct Env {
    bindings: HashMap<String, Value>,
    parent: Option<Box<Env>>,
}

impl Env {
    fn new() -> Self {
        Env {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    fn with_parent(parent: &Env) -> Self {
        Env {
            bindings: HashMap::new(),
            parent: Some(Box::new(parent.clone())),
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.get(name) {
            Some(val.clone())
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

// ── Parser ──────────────────────────────────────────────────────────

struct Parser {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
            line: 1,
            col: 0,
        }
    }

    fn current_pos(&self) -> Pos {
        Pos {
            line: self.line,
            col: self.col,
        }
    }

    fn advance(&mut self) {
        if self.pos < self.chars.len() {
            if self.chars[self.pos] == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.chars.len() {
            if self.chars[self.pos].is_whitespace() {
                self.advance();
            } else if self.chars[self.pos] == ';' {
                while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
                    self.advance();
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.chars.get(self.pos).copied();
        if c.is_some() {
            self.advance();
        }
        c
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace();
        let start = self.current_pos();
        match self.peek() {
            None => Err(EvalError::Parse(format!(
                "unexpected end of input at {}",
                start.fmt()
            ))),
            Some('(') => self.parse_list(),
            Some('\'') => {
                self.next_char();
                let inner = self.parse_expr()?;
                Ok(Expr {
                    kind: ExprKind::List(vec![
                        Expr {
                            kind: ExprKind::Symbol("quote".into()),
                            pos: start,
                        },
                        inner,
                    ]),
                    pos: start,
                })
            }
            Some('"') => self.parse_string(),
            Some('#') => self.parse_hash(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        let start = self.current_pos();
        self.next_char(); // consume '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace();
            match self.peek() {
                None => {
                    return Err(EvalError::Parse(format!(
                        "unterminated list at {}",
                        start.fmt()
                    )))
                }
                Some(')') => {
                    self.next_char();
                    return Ok(Expr {
                        kind: ExprKind::List(items),
                        pos: start,
                    });
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        let start = self.current_pos();
        self.next_char(); // consume opening "
        let mut s = String::new();
        loop {
            match self.next_char() {
                None => {
                    return Err(EvalError::Parse(format!(
                        "unterminated string at {}",
                        start.fmt()
                    )))
                }
                Some('\\') => match self.next_char() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('\\') => s.push('\\'),
                    Some('"') => s.push('"'),
                    Some(c) => {
                        s.push('\\');
                        s.push(c);
                    }
                    None => {
                        return Err(EvalError::Parse(format!(
                            "unterminated escape at {}",
                            start.fmt()
                        )))
                    }
                },
                Some('"') => {
                    return Ok(Expr {
                        kind: ExprKind::Str(s),
                        pos: start,
                    })
                }
                Some(c) => s.push(c),
            }
        }
    }

    fn parse_hash(&mut self) -> Result<Expr, EvalError> {
        let start = self.current_pos();
        self.next_char(); // consume '#'
        match self.next_char() {
            Some('t') => Ok(Expr {
                kind: ExprKind::Boolean(true),
                pos: start,
            }),
            Some('f') => Ok(Expr {
                kind: ExprKind::Boolean(false),
                pos: start,
            }),
            _ => Err(EvalError::Parse(format!(
                "invalid # literal at {}",
                start.fmt()
            ))),
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.current_pos();
        let char_start = self.pos;
        while self.pos < self.chars.len() {
            let c = self.chars[self.pos];
            if c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == ';' {
                break;
            }
            self.advance();
        }
        let token: String = self.chars[char_start..self.pos].iter().collect();
        if token.is_empty() {
            return Err(EvalError::Parse(format!("empty token at {}", start.fmt())));
        }
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Expr {
                kind: ExprKind::Integer(n),
                pos: start,
            });
        }
        Ok(Expr {
            kind: ExprKind::Symbol(token),
            pos: start,
        })
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace();
            if self.pos >= self.chars.len() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }
}

// ── Evaluator ───────────────────────────────────────────────────────

fn eval(expr: &Expr, env: &mut Env) -> Result<Value, EvalError> {
    let pos = expr.pos;
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Symbol(name) => env
            .get(name)
            .ok_or_else(|| EvalError::UnboundVariable(format!("{} at {}", name, pos.fmt()))),
        ExprKind::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse(format!(
                    "empty application at {}",
                    pos.fmt()
                )));
            }
            // Check for special forms
            if let ExprKind::Symbol(op) = &items[0].kind {
                match op.as_str() {
                    "if" => return eval_if(&items[1..], pos, env),
                    "define" => return eval_define(&items[1..], pos, env),
                    "quote" => return eval_quote(&items[1..], pos),
                    "lambda" => return eval_lambda(&items[1..], pos, env),
                    "and" => return eval_and(&items[1..], env),
                    "or" => return eval_or(&items[1..], env),
                    "let" => return eval_let(&items[1..], pos, env),
                    "begin" => return eval_begin(&items[1..], env),
                    "cond" => return eval_cond(&items[1..], env),
                    _ => {}
                }
            }
            // Check for builtins by name before general eval
            if let ExprKind::Symbol(name) = &items[0].kind {
                let args: Result<Vec<Value>, _> =
                    items[1..].iter().map(|a| eval(a, env)).collect();
                let args = args?;
                if let Some(result) = apply_builtin(name, &args, pos)? {
                    return Ok(result);
                }
                // Not a builtin, look up in env
                let func = env.get(name).ok_or_else(|| {
                    EvalError::UnboundVariable(format!("{} at {}", name, pos.fmt()))
                })?;
                return apply_named(&func, &args, name, env, pos);
            }
            // General application (e.g., lambda expression in head position)
            let func = eval(&items[0], env)?;
            let args: Result<Vec<Value>, _> = items[1..].iter().map(|a| eval(a, env)).collect();
            let args = args?;
            apply(&func, &args, pos)
        }
    }
}

fn apply(func: &Value, args: &[Value], pos: Pos) -> Result<Value, EvalError> {
    apply_named(func, args, "", &Env::new(), pos)
}

fn apply_named(
    func: &Value,
    args: &[Value],
    name: &str,
    calling_env: &Env,
    pos: Pos,
) -> Result<Value, EvalError> {
    match func {
        Value::Lambda {
            params,
            body,
            env,
        } => {
            if params.len() != args.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {} at {}",
                    params.len(),
                    args.len(),
                    pos.fmt()
                )));
            }
            let mut local_env = Env::with_parent(env);
            if !name.is_empty() {
                if let Some(f) = calling_env.get(name) {
                    local_env.set(name.to_string(), f);
                }
            }
            for (p, a) in params.iter().zip(args.iter()) {
                local_env.set(p.clone(), a.clone());
            }
            let mut result = Value::Boolean(false);
            for expr in body {
                result = eval(expr, &mut local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type(format!(
            "not a procedure at {}",
            pos.fmt()
        ))),
    }
}

fn eval_if(args: &[Expr], pos: Pos, env: &mut Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity(format!(
            "if requires 2 or 3 arguments at {}",
            pos.fmt()
        )));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Nil)
    }
}

fn eval_define(args: &[Expr], pos: Pos, env: &mut Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!(
            "define requires arguments at {}",
            pos.fmt()
        )));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "define requires a value at {}",
                    pos.fmt()
                )));
            }
            let val = eval(&args[1], env)?;
            env.set(name.clone(), val);
            Ok(Value::Nil)
        }
        ExprKind::List(parts) => {
            if parts.is_empty() {
                return Err(EvalError::Parse(format!(
                    "define: empty name list at {}",
                    pos.fmt()
                )));
            }
            let name = match &parts[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => {
                    return Err(EvalError::Type(format!(
                        "define: name must be symbol at {}",
                        pos.fmt()
                    )))
                }
            };
            let params: Result<Vec<String>, _> = parts[1..]
                .iter()
                .map(|p| match &p.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Type(format!(
                        "parameter must be symbol at {}",
                        p.pos.fmt()
                    ))),
                })
                .collect();
            let params = params?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env.set(name.clone(), lambda);
            Ok(Value::Nil)
        }
        _ => Err(EvalError::Type(format!(
            "define: first argument must be symbol or list at {}",
            pos.fmt()
        ))),
    }
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(items) => {
            let mut result = Value::Nil;
            for item in items.iter().rev() {
                result = Value::Pair(Box::new(expr_to_value(item)), Box::new(result));
            }
            result
        }
    }
}

fn eval_quote(args: &[Expr], pos: Pos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!(
            "quote requires 1 argument at {}",
            pos.fmt()
        )));
    }
    Ok(expr_to_value(&args[0]))
}

fn eval_lambda(args: &[Expr], pos: Pos, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "lambda requires params and body at {}",
            pos.fmt()
        )));
    }
    let params = match &args[0].kind {
        ExprKind::List(parts) => {
            let mut ps = Vec::new();
            for p in parts {
                match &p.kind {
                    ExprKind::Symbol(s) => ps.push(s.clone()),
                    _ => {
                        return Err(EvalError::Type(format!(
                            "parameter must be symbol at {}",
                            p.pos.fmt()
                        )))
                    }
                }
            }
            ps
        }
        _ => {
            return Err(EvalError::Type(format!(
                "lambda: params must be a list at {}",
                pos.fmt()
            )))
        }
    };
    Ok(Value::Lambda {
        params,
        body: args[1..].to_vec(),
        env: env.clone(),
    })
}

fn eval_and(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for a in args {
        result = eval(a, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for a in args {
        result = eval(a, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_let(args: &[Expr], pos: Pos, env: &mut Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "let requires bindings and body at {}",
            pos.fmt()
        )));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(bs) => bs,
        _ => {
            return Err(EvalError::Type(format!(
                "let: bindings must be a list at {}",
                pos.fmt()
            )))
        }
    };
    let mut local_env = Env::with_parent(env);
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => {
                        return Err(EvalError::Type(format!(
                            "let: binding name must be symbol at {}",
                            pair[0].pos.fmt()
                        )))
                    }
                };
                let val = eval(&pair[1], env)?;
                local_env.set(name, val);
            }
            _ => {
                return Err(EvalError::Type(format!(
                    "let: invalid binding at {}",
                    b.pos.fmt()
                )))
            }
        }
    }
    let mut result = Value::Nil;
    for expr in &args[1..] {
        result = eval(expr, &mut local_env)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Nil;
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    for clause in args {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        let mut result = Value::Nil;
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env)?;
                if test.is_truthy() {
                    let mut result = Value::Nil;
                    for expr in &parts[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => {
                return Err(EvalError::Type(format!(
                    "cond: invalid clause at {}",
                    clause.pos.fmt()
                )))
            }
        }
    }
    Ok(Value::Nil)
}

fn apply_builtin(op: &str, args: &[Value], pos: Pos) -> Result<Option<Value>, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += a.as_integer(pos)?;
            }
            Ok(Some(Value::Integer(sum)))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "- requires at least 1 argument at {}",
                    pos.fmt()
                )));
            }
            if args.len() == 1 {
                return Ok(Some(Value::Integer(-args[0].as_integer(pos)?)));
            }
            let mut result = args[0].as_integer(pos)?;
            for a in &args[1..] {
                result -= a.as_integer(pos)?;
            }
            Ok(Some(Value::Integer(result)))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= a.as_integer(pos)?;
            }
            Ok(Some(Value::Integer(product)))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "/ requires at least 1 argument at {}",
                    pos.fmt()
                )));
            }
            let mut result = args[0].as_integer(pos)?;
            for a in &args[1..] {
                let divisor = a.as_integer(pos)?;
                if divisor == 0 {
                    return Err(EvalError::DivisionByZero(format!("at {}", pos.fmt())));
                }
                result /= divisor;
            }
            Ok(Some(Value::Integer(result)))
        }
        "<" => compare_op_val(args, |a, b| a < b, pos),
        ">" => compare_op_val(args, |a, b| a > b, pos),
        "=" => compare_op_val(args, |a, b| a == b, pos),
        "<=" => compare_op_val(args, |a, b| a <= b, pos),
        ">=" => compare_op_val(args, |a, b| a >= b, pos),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "not requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(!args[0].is_truthy())))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "cons requires 2 arguments at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Pair(
                Box::new(args[0].clone()),
                Box::new(args[1].clone()),
            )))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "car requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Pair(car, _) => Ok(Some(*car.clone())),
                _ => Err(EvalError::Type(format!("car: not a pair at {}", pos.fmt()))),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "cdr requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Pair(_, cdr) => Ok(Some(*cdr.clone())),
                _ => Err(EvalError::Type(format!("cdr: not a pair at {}", pos.fmt()))),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "null? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Nil))))
        }
        "list" => {
            let mut result = Value::Nil;
            for a in args.iter().rev() {
                result = Value::Pair(Box::new(a.clone()), Box::new(result));
            }
            Ok(Some(result))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "length requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            let mut count = 0i64;
            let mut cur = &args[0];
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(_, cdr) => {
                        count += 1;
                        cur = cdr;
                    }
                    _ => {
                        return Err(EvalError::Type(format!(
                            "length: not a proper list at {}",
                            pos.fmt()
                        )))
                    }
                }
            }
            Ok(Some(Value::Integer(count)))
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "string? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Str(_)))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "number? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Integer(_)))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "boolean? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Boolean(_)))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "pair? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Pair(_, _)))))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "symbol? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Symbol(_)))))
        }
        _ => Ok(None),
    }
}

fn compare_op_val(
    args: &[Value],
    cmp: impl Fn(i64, i64) -> bool,
    pos: Pos,
) -> Result<Option<Value>, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "comparison requires at least 2 arguments at {}",
            pos.fmt()
        )));
    }
    let mut prev = args[0].as_integer(pos)?;
    for a in &args[1..] {
        let cur = a.as_integer(pos)?;
        if !cmp(prev, cur) {
            return Ok(Some(Value::Boolean(false)));
        }
        prev = cur;
    }
    Ok(Some(Value::Boolean(true)))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let mut env = Env::new();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &mut env)?;
    }
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
