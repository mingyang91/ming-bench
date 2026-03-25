pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, Default)]
pub struct Span {
    line: usize,
    col: usize,
}

impl Span {
    fn new(line: usize, col: usize) -> Self {
        Span { line, col }
    }
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
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Nil,
    Builtin(String, fn(&[Value], Span) -> Result<Value, EvalError>),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: EnvRef,
    },
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
            Value::Nil => write!(f, "()"),
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
            Value::Builtin(name, _) => write!(f, "#<procedure {name}>"),
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::Void => write!(f, "#<void>"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::List(a), Value::List(b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// --- Tokenizer ---

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Quote,
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
}

fn tokenize(input: &str) -> Result<Vec<(Token, Span)>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line = 1usize;
    let mut col = 1usize;

    while i < chars.len() {
        let span = Span::new(line, col);
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
                    if chars[i] == '\n' {
                        line += 1;
                        col = 1;
                    } else {
                        col += 1;
                    }
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
                        s.push(chars[i]);
                    }
                    i += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse(format!("at {span}: unterminated string")));
                }
                i += 1; // closing quote
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
                        _ => {
                            return Err(EvalError::Parse(format!(
                                "at {span}: unexpected character after #: {}",
                                chars[i + 1]
                            )));
                        }
                    }
                } else {
                    return Err(EvalError::Parse(format!("at {span}: unexpected #")));
                }
            }
            _c => {
                let start = i;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"' | '\'')
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

// --- Parser ---

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

impl Expr {
    fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }
}

fn parse_tokens(tokens: &[(Token, Span)], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let (tok, span) = &tokens[*pos];
    let span = *span;
    match tok {
        Token::Integer(n) => {
            let n = *n;
            *pos += 1;
            Ok(Expr::new(ExprKind::Integer(n), span))
        }
        Token::Boolean(b) => {
            let b = *b;
            *pos += 1;
            Ok(Expr::new(ExprKind::Boolean(b), span))
        }
        Token::Str(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::new(ExprKind::Str(s), span))
        }
        Token::Symbol(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::new(ExprKind::Symbol(s), span))
        }
        Token::Quote => {
            *pos += 1;
            let inner = parse_tokens(tokens, pos)?;
            Ok(Expr::new(
                ExprKind::List(vec![
                    Expr::new(ExprKind::Symbol("quote".into()), span),
                    inner,
                ]),
                span,
            ))
        }
        Token::LParen => {
            *pos += 1;
            let mut elems = Vec::new();
            while *pos < tokens.len() && !matches!(&tokens[*pos].0, Token::RParen) {
                elems.push(parse_tokens(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse(format!("at {span}: missing closing paren")));
            }
            *pos += 1; // consume RParen
            Ok(Expr::new(ExprKind::List(elems), span))
        }
        Token::RParen => Err(EvalError::Parse(format!("at {span}: unexpected )"))),
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_tokens(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// --- Environment ---

#[derive(Debug)]
struct Env {
    bindings: HashMap<String, Value>,
    parent: Option<EnvRef>,
}

type EnvRef = Rc<RefCell<Env>>;

impl Env {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Env {
            bindings: HashMap::new(),
            parent,
        }))
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.get(name) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    fn set(&mut self, name: String, val: Value) {
        self.bindings.insert(name, val);
    }
}

fn default_env() -> EnvRef {
    let env = Env::new(None);

    fn builtin_add(args: &[Value], span: Span) -> Result<Value, EvalError> {
        let mut sum = 0i64;
        for a in args {
            match a {
                Value::Integer(n) => sum += n,
                _ => return Err(EvalError::Type(format!("at {span}: + expects numbers"))),
            }
        }
        Ok(Value::Integer(sum))
    }

    fn builtin_sub(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Arity(format!("at {span}: - requires at least 1 argument")));
        }
        match &args[0] {
            Value::Integer(first) => {
                if args.len() == 1 {
                    return Ok(Value::Integer(-first));
                }
                let mut result = *first;
                for a in &args[1..] {
                    match a {
                        Value::Integer(n) => result -= n,
                        _ => return Err(EvalError::Type(format!("at {span}: - expects numbers"))),
                    }
                }
                Ok(Value::Integer(result))
            }
            _ => Err(EvalError::Type(format!("at {span}: - expects numbers"))),
        }
    }

    fn builtin_mul(args: &[Value], span: Span) -> Result<Value, EvalError> {
        let mut product = 1i64;
        for a in args {
            match a {
                Value::Integer(n) => product *= n,
                _ => return Err(EvalError::Type(format!("at {span}: * expects numbers"))),
            }
        }
        Ok(Value::Integer(product))
    }

    fn builtin_div(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Arity(format!("at {span}: / requires at least 2 arguments")));
        }
        match &args[0] {
            Value::Integer(first) => {
                let mut result = *first;
                for a in &args[1..] {
                    match a {
                        Value::Integer(0) => return Err(EvalError::DivisionByZero(span)),
                        Value::Integer(n) => result /= n,
                        _ => return Err(EvalError::Type(format!("at {span}: / expects numbers"))),
                    }
                }
                Ok(Value::Integer(result))
            }
            _ => Err(EvalError::Type(format!("at {span}: / expects numbers"))),
        }
    }

    fn builtin_lt(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Arity(format!("at {span}: < requires 2 arguments")));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a < b)),
            _ => Err(EvalError::Type(format!("at {span}: < expects numbers"))),
        }
    }

    fn builtin_gt(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Arity(format!("at {span}: > requires 2 arguments")));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a > b)),
            _ => Err(EvalError::Type(format!("at {span}: > expects numbers"))),
        }
    }

    fn builtin_eq(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Arity(format!("at {span}: = requires 2 arguments")));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a == b)),
            _ => Err(EvalError::Type(format!("at {span}: = expects numbers"))),
        }
    }

    fn builtin_le(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Arity(format!("at {span}: <= requires 2 arguments")));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a <= b)),
            _ => Err(EvalError::Type(format!("at {span}: <= expects numbers"))),
        }
    }

    fn builtin_not(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity(format!("at {span}: not requires 1 argument")));
        }
        Ok(Value::Boolean(!args[0].is_truthy()))
    }

    fn builtin_cons(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Arity(format!("at {span}: cons requires 2 arguments")));
        }
        match &args[1] {
            Value::List(elems) => {
                let mut new = vec![args[0].clone()];
                new.extend(elems.iter().cloned());
                Ok(Value::List(new))
            }
            Value::Nil => Ok(Value::List(vec![args[0].clone()])),
            _ => {
                Ok(Value::List(vec![args[0].clone(), args[1].clone()]))
            }
        }
    }

    fn builtin_car(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity(format!("at {span}: car requires 1 argument")));
        }
        match &args[0] {
            Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
            _ => Err(EvalError::Type(format!("at {span}: car: not a pair"))),
        }
    }

    fn builtin_cdr(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity(format!("at {span}: cdr requires 1 argument")));
        }
        match &args[0] {
            Value::List(elems) if !elems.is_empty() => {
                if elems.len() == 1 {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::List(elems[1..].to_vec()))
                }
            }
            _ => Err(EvalError::Type(format!("at {span}: cdr: not a pair"))),
        }
    }

    fn builtin_null(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity(format!("at {span}: null? requires 1 argument")));
        }
        let is_null = matches!(&args[0], Value::Nil) || matches!(&args[0], Value::List(v) if v.is_empty());
        Ok(Value::Boolean(is_null))
    }

    fn builtin_list(args: &[Value], _span: Span) -> Result<Value, EvalError> {
        if args.is_empty() {
            Ok(Value::Nil)
        } else {
            Ok(Value::List(args.to_vec()))
        }
    }

    fn builtin_length(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity(format!("at {span}: length requires 1 argument")));
        }
        match &args[0] {
            Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
            Value::Nil => Ok(Value::Integer(0)),
            _ => Err(EvalError::Type(format!("at {span}: length: not a list"))),
        }
    }

    fn builtin_number_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity(format!("at {span}: number? requires 1 argument")));
        }
        Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
    }

    fn builtin_boolean_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity(format!("at {span}: boolean? requires 1 argument")));
        }
        Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
    }

    fn builtin_string_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity(format!("at {span}: string? requires 1 argument")));
        }
        Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
    }

    fn builtin_pair_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity(format!("at {span}: pair? requires 1 argument")));
        }
        Ok(Value::Boolean(matches!(&args[0], Value::List(v) if !v.is_empty())))
    }

    fn builtin_symbol_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity(format!("at {span}: symbol? requires 1 argument")));
        }
        Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
    }

    fn builtin_append(args: &[Value], span: Span) -> Result<Value, EvalError> {
        let mut result = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            match arg {
                Value::List(elems) => result.extend(elems.iter().cloned()),
                Value::Nil => {}
                _ if i == args.len() - 1 => {
                    result.push(arg.clone());
                }
                _ => return Err(EvalError::Type(format!("at {span}: append: not a list"))),
            }
        }
        if result.is_empty() {
            Ok(Value::Nil)
        } else {
            Ok(Value::List(result))
        }
    }

    {
        let mut e = env.borrow_mut();
        e.set("+".into(), Value::Builtin("+".into(), builtin_add));
        e.set("-".into(), Value::Builtin("-".into(), builtin_sub));
        e.set("*".into(), Value::Builtin("*".into(), builtin_mul));
        e.set("/".into(), Value::Builtin("/".into(), builtin_div));
        e.set("<".into(), Value::Builtin("<".into(), builtin_lt));
        e.set(">".into(), Value::Builtin(">".into(), builtin_gt));
        e.set("=".into(), Value::Builtin("=".into(), builtin_eq));
        e.set("<=".into(), Value::Builtin("<=".into(), builtin_le));
        e.set("not".into(), Value::Builtin("not".into(), builtin_not));
        e.set("cons".into(), Value::Builtin("cons".into(), builtin_cons));
        e.set("car".into(), Value::Builtin("car".into(), builtin_car));
        e.set("cdr".into(), Value::Builtin("cdr".into(), builtin_cdr));
        e.set("null?".into(), Value::Builtin("null?".into(), builtin_null));
        e.set("list".into(), Value::Builtin("list".into(), builtin_list));
        e.set("length".into(), Value::Builtin("length".into(), builtin_length));
        e.set("number?".into(), Value::Builtin("number?".into(), builtin_number_pred));
        e.set("boolean?".into(), Value::Builtin("boolean?".into(), builtin_boolean_pred));
        e.set("string?".into(), Value::Builtin("string?".into(), builtin_string_pred));
        e.set("pair?".into(), Value::Builtin("pair?".into(), builtin_pair_pred));
        e.set("symbol?".into(), Value::Builtin("symbol?".into(), builtin_symbol_pred));
        e.set("append".into(), Value::Builtin("append".into(), builtin_append));
    }

    env
}

// --- Evaluator ---

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(elems) => {
            if elems.is_empty() {
                Value::Nil
            } else {
                Value::List(elems.iter().map(expr_to_value).collect())
            }
        }
    }
}

/// Evaluate a `cond` expression given its clauses.
fn eval_cond(clauses: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for clause in clauses {
        let ExprKind::List(parts) = &clause.kind else {
            return Err(EvalError::Parse(format!("at {}: cond: invalid clause", clause.span)));
        };
        if parts.is_empty() {
            return Err(EvalError::Parse(format!("at {}: cond: invalid clause", clause.span)));
        }
        let is_else = matches!(&parts[0].kind, ExprKind::Symbol(s) if s == "else");
        if is_else {
            let mut result = Value::Void;
            for expr in &parts[1..] {
                result = eval(expr, env)?;
            }
            return Ok(result);
        }
        let test = eval(&parts[0], env)?;
        if !test.is_truthy() {
            continue;
        }
        let mut result = test;
        for expr in &parts[1..] {
            result = eval(expr, env)?;
        }
        return Ok(result);
    }
    Ok(Value::Void)
}

fn eval(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    let span = expr.span;
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Symbol(name) => env
            .borrow()
            .get(name)
            .ok_or_else(|| EvalError::UnboundVariable(format!("at {span}: {name}"))),
        ExprKind::List(elems) => {
            if elems.is_empty() {
                return Ok(Value::Nil);
            }

            // Special forms
            if let ExprKind::Symbol(name) = &elems[0].kind {
                match name.as_str() {
                    "define" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Parse(format!("at {span}: define requires at least 2 arguments")));
                        }
                        match &elems[1].kind {
                            // (define x expr)
                            ExprKind::Symbol(var_name) => {
                                let val = eval(&elems[2], env)?;
                                env.borrow_mut().set(var_name.clone(), val);
                                return Ok(Value::Void);
                            }
                            // (define (f params...) body...)
                            ExprKind::List(name_and_params) => {
                                if name_and_params.is_empty() {
                                    return Err(EvalError::Parse(format!("at {span}: define: empty name list")));
                                }
                                let fn_name = match &name_and_params[0].kind {
                                    ExprKind::Symbol(s) => s.clone(),
                                    _ => return Err(EvalError::Parse(format!("at {span}: define: expected symbol"))),
                                };
                                let params: Vec<String> = name_and_params[1..]
                                    .iter()
                                    .map(|e| match &e.kind {
                                        ExprKind::Symbol(s) => Ok(s.clone()),
                                        _ => Err(EvalError::Parse(format!("at {}: define: expected parameter name", e.span))),
                                    })
                                    .collect::<Result<_, _>>()?;
                                let body = elems[2..].to_vec();
                                let lambda = Value::Lambda {
                                    params,
                                    body,
                                    env: env.clone(),
                                };
                                env.borrow_mut().set(fn_name, lambda);
                                return Ok(Value::Void);
                            }
                            _ => return Err(EvalError::Parse(format!("at {span}: define: invalid syntax"))),
                        }
                    }
                    "if" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Parse(format!("at {span}: if requires a condition and consequent")));
                        }
                        let cond = eval(&elems[1], env)?;
                        if cond.is_truthy() {
                            return eval(&elems[2], env);
                        } else if elems.len() > 3 {
                            return eval(&elems[3], env);
                        } else {
                            return Ok(Value::Void);
                        }
                    }
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Parse(format!("at {span}: quote requires exactly 1 argument")));
                        }
                        return Ok(expr_to_value(&elems[1]));
                    }
                    "lambda" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Parse(format!("at {span}: lambda requires params and body")));
                        }
                        let params = match &elems[1].kind {
                            ExprKind::List(param_exprs) => {
                                param_exprs
                                    .iter()
                                    .map(|e| match &e.kind {
                                        ExprKind::Symbol(s) => Ok(s.clone()),
                                        _ => Err(EvalError::Parse(format!("at {}: lambda: expected parameter name", e.span))),
                                    })
                                    .collect::<Result<Vec<_>, _>>()?
                            }
                            _ => return Err(EvalError::Parse(format!("at {span}: lambda: expected parameter list"))),
                        };
                        let body = elems[2..].to_vec();
                        return Ok(Value::Lambda {
                            params,
                            body,
                            env: env.clone(),
                        });
                    }
                    "begin" => {
                        let mut result = Value::Void;
                        for arg in &elems[1..] {
                            result = eval(arg, env)?;
                        }
                        return Ok(result);
                    }
                    "let" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Parse(format!("at {span}: let requires bindings and body")));
                        }
                        let (loop_name, bindings_expr, body_start) = match &elems[1].kind {
                            ExprKind::Symbol(name) => {
                                if elems.len() < 4 {
                                    return Err(EvalError::Parse(format!("at {span}: named let requires bindings and body")));
                                }
                                (Some(name.clone()), &elems[2], 3)
                            }
                            _ => (None, &elems[1], 2),
                        };
                        let bindings = match &bindings_expr.kind {
                            ExprKind::List(bs) => bs,
                            _ => return Err(EvalError::Parse(format!("at {span}: let: expected bindings list"))),
                        };
                        let mut param_names = Vec::new();
                        let mut init_vals = Vec::new();
                        for b in bindings {
                            match &b.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    let name = match &pair[0].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Parse(format!("at {}: let: expected symbol in binding", pair[0].span))),
                                    };
                                    let val = eval(&pair[1], env)?;
                                    param_names.push(name);
                                    init_vals.push(val);
                                }
                                _ => return Err(EvalError::Parse(format!("at {}: let: invalid binding", b.span))),
                            }
                        }
                        let local_env = Env::new(Some(env.clone()));
                        if let Some(lname) = &loop_name {
                            let body = elems[body_start..].to_vec();
                            let lambda = Value::Lambda {
                                params: param_names.clone(),
                                body,
                                env: local_env.clone(),
                            };
                            local_env.borrow_mut().set(lname.clone(), lambda);
                        }
                        {
                            let mut e = local_env.borrow_mut();
                            for (name, val) in param_names.iter().zip(init_vals.iter()) {
                                e.set(name.clone(), val.clone());
                            }
                        }
                        let mut result = Value::Void;
                        for expr in &elems[body_start..] {
                            result = eval(expr, &local_env)?;
                        }
                        return Ok(result);
                    }
                    "cond" => return eval_cond(&elems[1..], env),
                    "and" => {
                        let mut result = Value::Boolean(true);
                        for arg in &elems[1..] {
                            result = eval(arg, env)?;
                            if !result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        let mut result = Value::Boolean(false);
                        for arg in &elems[1..] {
                            result = eval(arg, env)?;
                            if result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    _ => {}
                }
            }

            // Function application
            let func = eval(&elems[0], env)?;
            let mut args = Vec::new();
            for arg in &elems[1..] {
                args.push(eval(arg, env)?);
            }
            apply_function(&func, &args, span)
        }
    }
}

fn apply_function(func: &Value, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(_, f) => f(args, span),
        Value::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "at {span}: expected {} arguments, got {}",
                    params.len(),
                    args.len()
                )));
            }
            let local_env = Env::new(Some(env.clone()));
            {
                let mut e = local_env.borrow_mut();
                for (param, arg) in params.iter().zip(args.iter()) {
                    e.set(param.clone(), arg.clone());
                }
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type(format!("at {span}: not a procedure: {func}"))),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = default_env();
    let mut result = Value::Nil;
    for expr in &exprs {
        result = eval(expr, &env)?;
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
