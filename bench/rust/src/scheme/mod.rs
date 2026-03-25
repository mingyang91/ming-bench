pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(SchemeString),
    Char(char),
    Symbol(String),
    EmptyList,
    Pair(Box<Value>, Box<Value>),
    Procedure(Rc<Procedure>),
    Void,
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

#[derive(Clone)]
struct SchemeString {
    chars: Rc<RefCell<Vec<char>>>,
}

impl SchemeString {
    fn new(value: impl AsRef<str>) -> Self {
        Self {
            chars: Rc::new(RefCell::new(value.as_ref().chars().collect())),
        }
    }

    fn from_chars(chars: Vec<char>) -> Self {
        Self {
            chars: Rc::new(RefCell::new(chars)),
        }
    }

    fn to_plain_string(&self) -> String {
        self.chars.borrow().iter().collect()
    }

    fn len(&self) -> usize {
        self.chars.borrow().len()
    }

    fn deep_copy(&self) -> Self {
        Self::from_chars(self.chars.borrow().clone())
    }

    fn substring(&self, start: usize, end: usize) -> Result<Self, EvalError> {
        let chars = self.chars.borrow();

        if start > end || end > chars.len() {
            return Err(EvalError::InvalidRange {
                kind: "substring",
                start,
                end,
                length: chars.len(),
            });
        }

        Ok(Self::from_chars(chars[start..end].to_vec()))
    }

    fn get(&self, index: usize, kind: &'static str) -> Result<char, EvalError> {
        let chars = self.chars.borrow();

        chars.get(index).copied().ok_or(EvalError::IndexOutOfBounds {
            kind,
            index,
            length: chars.len(),
        })
    }

    fn set(&self, index: usize, value: char, kind: &'static str) -> Result<(), EvalError> {
        let mut chars = self.chars.borrow_mut();
        let length = chars.len();
        let slot = chars.get_mut(index).ok_or(EvalError::IndexOutOfBounds {
            kind,
            index,
            length,
        })?;
        *slot = value;
        Ok(())
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Integer(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
            Self::Char(_) => "char",
            Self::Symbol(_) => "symbol",
            Self::EmptyList | Self::Pair(_, _) => "pair",
            Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn render(&self) -> String {
        self.render_with_mode(RenderMode::Write)
    }

    fn render_for_display(&self) -> String {
        self.render_with_mode(RenderMode::Display)
    }

    fn render_with_mode(&self, mode: RenderMode) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".to_string(),
            Self::Boolean(false) => "#f".to_string(),
            Self::String(value) => {
                let text = value.to_plain_string();
                match mode {
                    RenderMode::Write => render_string(&text),
                    RenderMode::Display => text,
                }
            }
            Self::Char(value) => match mode {
                RenderMode::Write => render_char(*value),
                RenderMode::Display => value.to_string(),
            },
            Self::Symbol(value) => value.clone(),
            Self::EmptyList => "()".to_string(),
            Self::Pair(_, _) => render_pair(self, mode),
            Self::Procedure(_) => "#<procedure>".to_string(),
            Self::Void => "#<void>".to_string(),
        }
    }
}

fn render_pair(value: &Value, mode: RenderMode) -> String {
    let mut out = String::from("(");
    let mut cursor = value;

    loop {
        let Value::Pair(car, cdr) = cursor else {
            break;
        };

        out.push_str(&car.render_with_mode(mode));

        match cdr.as_ref() {
            Value::EmptyList => break,
            Value::Pair(_, _) => {
                out.push(' ');
                cursor = cdr.as_ref();
            }
            other => {
                out.push_str(" . ");
                out.push_str(&other.render_with_mode(mode));
                break;
            }
        }
    }

    out.push(')');
    out
}

fn render_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');

    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            other => out.push(other),
        }
    }

    out.push('"');
    out
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        other => format!("#\\{other}"),
    }
}

#[derive(Default)]
struct EvalContext {
    output: String,
    capture_output: bool,
}

impl EvalContext {
    fn new(capture_output: bool) -> Self {
        Self {
            output: String::new(),
            capture_output,
        }
    }

    fn emit(&mut self, text: &str) {
        if self.capture_output {
            self.output.push_str(text);
        }
    }

    fn finish(self) -> String {
        self.output
    }
}

type EnvRef = Rc<RefCell<Env>>;
type BuiltinFn = fn(&[Value], &mut EvalContext) -> Result<Value, EvalError>;

struct Env {
    parent: Option<EnvRef>,
    bindings: HashMap<String, Value>,
}

impl Env {
    fn new() -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: None,
            bindings: HashMap::new(),
        }))
    }

    fn child(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
        }))
    }
}

#[derive(Clone)]
enum Procedure {
    Builtin(BuiltinProcedure),
    Lambda(LambdaProcedure),
}

#[derive(Clone)]
struct BuiltinProcedure {
    implementation: BuiltinFn,
}

#[derive(Clone)]
struct LambdaProcedure {
    name: String,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

struct Parser<'a> {
    input: &'a str,
    index: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, index: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "expected at least one expression".to_string(),
            });
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some('\'') => self.parse_quote_shorthand(),
            Some('"') => self.parse_string(),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn parse_quote_shorthand(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        let quoted = self.parse_expr()?;
        Ok(Expr::List(vec![Expr::Symbol("quote".to_string()), quoted]))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek_char() {
                Some(')') => {
                    self.bump_char();
                    break;
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof),
            }
        }

        Ok(Expr::List(items))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        let mut out = String::new();

        loop {
            match self.bump_char() {
                Some('"') => return Ok(Expr::String(out)),
                Some('\\') => match self.bump_char() {
                    Some('"') => out.push('"'),
                    Some('\\') => out.push('\\'),
                    Some('n') => out.push('\n'),
                    Some('r') => out.push('\r'),
                    Some('t') => out.push('\t'),
                    Some(other) => out.push(other),
                    None => return Err(EvalError::UnterminatedString),
                },
                Some(ch) => out.push(ch),
                None => return Err(EvalError::UnterminatedString),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let token =
            self.take_while(|ch| !ch.is_whitespace() && ch != '(' && ch != ')' && ch != ';');

        if token.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "unexpected token".to_string(),
            });
        }

        match token {
            "#t" => Ok(Expr::Boolean(true)),
            "#f" => Ok(Expr::Boolean(false)),
            _ if token.starts_with("#\\") => {
                parse_char_token(token)
                    .map(Expr::Char)
                    .ok_or(EvalError::SyntaxError {
                        message: "invalid character literal".to_string(),
                    })
            }
            _ => {
                if let Some(value) = parse_integer_token(token) {
                    Ok(Expr::Integer(value))
                } else {
                    Ok(Expr::Symbol(token.to_string()))
                }
            }
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            self.take_while(|ch| ch.is_whitespace());

            if self.peek_char() == Some(';') {
                self.take_while(|ch| ch != '\n');
                continue;
            }

            break;
        }
    }

    fn take_while(&mut self, mut predicate: impl FnMut(char) -> bool) -> &'a str {
        let start = self.index;

        while let Some(ch) = self.peek_char() {
            if !predicate(ch) {
                break;
            }
            self.bump_char();
        }

        &self.input[start..self.index]
    }

    fn is_eof(&self) -> bool {
        self.index >= self.input.len()
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.index += ch.len_utf8();
        Some(ch)
    }
}

fn parse_integer_token(token: &str) -> Option<i64> {
    let digits = match token.bytes().next() {
        Some(b'+') | Some(b'-') => &token[1..],
        _ => token,
    };

    if digits.is_empty() || !digits.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }

    token.parse().ok()
}

fn parse_char_token(token: &str) -> Option<char> {
    let literal = token.strip_prefix("#\\")?;

    match literal {
        "space" => Some(' '),
        "newline" => Some('\n'),
        _ => {
            let mut chars = literal.chars();
            let ch = chars.next()?;

            if chars.next().is_some() {
                None
            } else {
                Some(ch)
            }
        }
    }
}

fn line_col_at(input: &str, index: usize) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;

    for ch in input[..index.min(input.len())].chars() {
        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }

    (line, column)
}

fn default_env() -> EnvRef {
    let env = Env::new();

    define_builtin(&env, "+", builtin_add);
    define_builtin(&env, "-", builtin_sub);
    define_builtin(&env, "*", builtin_mul);
    define_builtin(&env, "/", builtin_div);
    define_builtin(&env, "<", builtin_less_than);
    define_builtin(&env, ">", builtin_greater_than);
    define_builtin(&env, "=", builtin_equal_numbers);
    define_builtin(&env, "<=", builtin_less_equal);
    define_builtin(&env, "not", builtin_not);
    define_builtin(&env, "cons", builtin_cons);
    define_builtin(&env, "car", builtin_car);
    define_builtin(&env, "cdr", builtin_cdr);
    define_builtin(&env, "null?", builtin_null_predicate);
    define_builtin(&env, "list", builtin_list);
    define_builtin(&env, "length", builtin_length);
    define_builtin(&env, "append", builtin_append);
    define_builtin(&env, "string?", builtin_string_predicate);
    define_builtin(&env, "number?", builtin_number_predicate);
    define_builtin(&env, "boolean?", builtin_boolean_predicate);
    define_builtin(&env, "pair?", builtin_pair_predicate);
    define_builtin(&env, "symbol?", builtin_symbol_predicate);
    define_builtin(&env, "display", builtin_display);
    define_builtin(&env, "write", builtin_write);
    define_builtin(&env, "newline", builtin_newline);
    define_builtin(&env, "string-append", builtin_string_append);
    define_builtin(&env, "string-length", builtin_string_length);
    define_builtin(&env, "substring", builtin_substring);
    define_builtin(&env, "string->number", builtin_string_to_number);
    define_builtin(&env, "number->string", builtin_number_to_string);
    define_builtin(&env, "symbol->string", builtin_symbol_to_string);
    define_builtin(&env, "string->symbol", builtin_string_to_symbol);
    define_builtin(&env, "string-ref", builtin_string_ref);
    define_builtin(&env, "string-copy", builtin_string_copy);
    define_builtin(&env, "string-set!", builtin_string_set);
    define_builtin(&env, "char?", builtin_char_predicate);

    env
}

fn define_builtin(env: &EnvRef, name: &'static str, implementation: BuiltinFn) {
    let value = Value::Procedure(Rc::new(Procedure::Builtin(BuiltinProcedure {
        implementation,
    })));
    env.borrow_mut().bindings.insert(name.to_string(), value);
}

fn lookup(env: &EnvRef, name: &str) -> Result<Value, EvalError> {
    let (value, parent) = {
        let scope = env.borrow();
        (scope.bindings.get(name).cloned(), scope.parent.clone())
    };

    if let Some(value) = value {
        return Ok(value);
    }

    if let Some(parent) = parent {
        return lookup(&parent, name);
    }

    Err(EvalError::UnboundVariable {
        name: name.to_string(),
    })
}

fn eval_program(exprs: &[Expr], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let env = default_env();
    eval_sequence(exprs, env, ctx)
}

fn eval_sequence(exprs: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Void;

    for expr in exprs {
        last = eval_expr(expr, env.clone(), ctx)?;
    }

    Ok(last)
}

fn eval_expr(expr: &Expr, env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(SchemeString::new(value))),
        Expr::Char(value) => Ok(Value::Char(*value)),
        Expr::Symbol(name) => lookup(&env, name),
        Expr::List(items) => eval_list(items, env, ctx),
    }
}

fn eval_list(items: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if items.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate an empty list".to_string(),
        });
    }

    if let Expr::Symbol(operator) = &items[0] {
        let args = &items[1..];

        match operator.as_str() {
            "and" => return eval_and(args, env, ctx),
            "or" => return eval_or(args, env, ctx),
            "if" => return eval_if(args, env, ctx),
            "begin" => return eval_begin(args, env, ctx),
            "cond" => return eval_cond(args, env, ctx),
            "quote" => return eval_quote(args),
            "define" => return eval_define(args, env, ctx),
            "lambda" => return eval_lambda(args, env),
            "let" => return eval_let(args, env, ctx),
            _ => {}
        }
    }

    let operator = eval_expr(&items[0], env.clone(), ctx)?;
    apply(operator, &items[1..], env, ctx)
}

fn eval_and(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for expr in args {
        let value = eval_expr(expr, env.clone(), ctx)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval_expr(expr, env.clone(), ctx)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_if(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_expr_arity("if", args, 3)?;

    let condition = eval_expr(&args[0], env.clone(), ctx)?;
    if condition.is_truthy() {
        eval_expr(&args[1], env, ctx)
    } else {
        eval_expr(&args[2], env, ctx)
    }
}

fn eval_begin(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    eval_sequence(args, env, ctx)
}

fn eval_cond(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::SyntaxError {
                message: "cond clauses must be lists".to_string(),
            });
        };

        if items.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "cond clauses cannot be empty".to_string(),
            });
        }

        if let Expr::Symbol(symbol) = &items[0] {
            if symbol == "else" {
                if index + 1 != args.len() {
                    return Err(EvalError::SyntaxError {
                        message: "cond else clause must be last".to_string(),
                    });
                }
                if items.len() == 1 {
                    return Ok(Value::Void);
                }
                return eval_sequence(&items[1..], env, ctx);
            }
        }

        let test_value = eval_expr(&items[0], env.clone(), ctx)?;
        if test_value.is_truthy() {
            if items.len() == 1 {
                return Ok(test_value);
            }
            return eval_sequence(&items[1..], env, ctx);
        }
    }

    Ok(Value::Void)
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    expect_expr_arity("quote", args, 1)?;
    Ok(quote_expr(&args[0]))
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(value) => Value::Integer(*value),
        Expr::Boolean(value) => Value::Boolean(*value),
        Expr::String(value) => Value::String(SchemeString::new(value)),
        Expr::Char(value) => Value::Char(*value),
        Expr::Symbol(name) => Value::Symbol(name.clone()),
        Expr::List(items) => items.iter().rev().fold(Value::EmptyList, |tail, item| {
            Value::Pair(Box::new(quote_expr(item)), Box::new(tail))
        }),
    }
}

fn eval_define(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "define requires a binding target".to_string(),
        });
    }

    match &args[0] {
        Expr::Symbol(name) => {
            expect_expr_arity("define", args, 2)?;
            let value = eval_expr(&args[1], env.clone(), ctx)?;
            env.borrow_mut().bindings.insert(name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) => {
            if signature.is_empty() {
                return Err(EvalError::SyntaxError {
                    message: "define requires a function name".to_string(),
                });
            }

            if args.len() < 2 {
                return Err(EvalError::SyntaxError {
                    message: "define requires a function body".to_string(),
                });
            }

            let Expr::Symbol(name) = &signature[0] else {
                return Err(EvalError::SyntaxError {
                    message: "define function name must be a symbol".to_string(),
                });
            };

            let params = parse_param_slice(&signature[1..])?;
            let lambda = make_lambda(name.clone(), params, args[1..].to_vec(), env.clone());
            env.borrow_mut().bindings.insert(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::SyntaxError {
            message: "define requires a symbol or function signature".to_string(),
        }),
    }
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::SyntaxError {
            message: "lambda requires parameters and a body".to_string(),
        });
    }

    let params = parse_param_list(&args[0])?;
    Ok(make_lambda(
        "lambda".to_string(),
        params,
        args[1..].to_vec(),
        env,
    ))
}

fn eval_let(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires bindings".to_string(),
        });
    }

    match &args[0] {
        Expr::Symbol(name) => eval_named_let(name, &args[1..], env, ctx),
        bindings => eval_let_bindings(bindings, &args[1..], env, ctx),
    }
}

fn eval_named_let(
    name: &str,
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::SyntaxError {
            message: "named let requires bindings and a body".to_string(),
        });
    }

    let bindings = parse_let_bindings(&args[0])?;
    let values = eval_let_values(&bindings, env.clone(), ctx)?;
    let params = bindings.iter().map(|(param, _)| param.clone()).collect();
    let named_env = Env::child(env);
    let lambda = make_lambda(
        name.to_string(),
        params,
        args[1..].to_vec(),
        named_env.clone(),
    );
    named_env
        .borrow_mut()
        .bindings
        .insert(name.to_string(), lambda.clone());
    apply_evaluated(lambda, values, ctx)
}

fn eval_let_bindings(
    bindings_expr: &Expr,
    body: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires a body".to_string(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let values = eval_let_values(&bindings, env.clone(), ctx)?;
    let let_env = Env::child(env);
    {
        let mut scope = let_env.borrow_mut();
        for ((name, _), value) in bindings.into_iter().zip(values) {
            scope.bindings.insert(name, value);
        }
    }
    eval_sequence(body, let_env, ctx)
}

fn parse_let_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings) = expr else {
        return Err(EvalError::SyntaxError {
            message: "let bindings must be a list".to_string(),
        });
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let Expr::List(parts) = binding else {
            return Err(EvalError::SyntaxError {
                message: "let bindings must be pairs".to_string(),
            });
        };

        if parts.len() != 2 {
            return Err(EvalError::SyntaxError {
                message: "let bindings must have a name and value".to_string(),
            });
        }

        let Expr::Symbol(name) = &parts[0] else {
            return Err(EvalError::SyntaxError {
                message: "let binding names must be symbols".to_string(),
            });
        };

        parsed.push((name.clone(), parts[1].clone()));
    }

    Ok(parsed)
}

fn eval_let_values(
    bindings: &[(String, Expr)],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::with_capacity(bindings.len());
    for (_, expr) in bindings {
        values.push(eval_expr(expr, env.clone(), ctx)?);
    }
    Ok(values)
}

fn parse_param_list(expr: &Expr) -> Result<Vec<String>, EvalError> {
    let Expr::List(params) = expr else {
        return Err(EvalError::SyntaxError {
            message: "lambda parameters must be a list".to_string(),
        });
    };

    parse_param_slice(params)
}

fn parse_param_slice(params: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut names = Vec::with_capacity(params.len());

    for param in params {
        let Expr::Symbol(name) = param else {
            return Err(EvalError::SyntaxError {
                message: "parameter names must be symbols".to_string(),
            });
        };
        names.push(name.clone());
    }

    Ok(names)
}

fn make_lambda(name: String, params: Vec<String>, body: Vec<Expr>, env: EnvRef) -> Value {
    Value::Procedure(Rc::new(Procedure::Lambda(LambdaProcedure {
        name,
        params,
        body,
        env,
    })))
}

fn apply(
    operator: Value,
    arg_exprs: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    let Value::Procedure(procedure) = operator else {
        return Err(EvalError::NotAProcedure);
    };

    let args = eval_args(arg_exprs, env, ctx)?;

    apply_procedure(procedure, args, ctx)
}

fn apply_evaluated(
    operator: Value,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    let Value::Procedure(procedure) = operator else {
        return Err(EvalError::NotAProcedure);
    };

    apply_procedure(procedure, args, ctx)
}

fn apply_procedure(
    procedure: Rc<Procedure>,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    match procedure.as_ref() {
        Procedure::Builtin(builtin) => (builtin.implementation)(&args, ctx),
        Procedure::Lambda(lambda) => apply_lambda(lambda, args, ctx),
    }
}

fn eval_args(
    arg_exprs: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Vec<Value>, EvalError> {
    let mut args = Vec::with_capacity(arg_exprs.len());

    for expr in arg_exprs {
        args.push(eval_expr(expr, env.clone(), ctx)?);
    }

    Ok(args)
}

fn apply_lambda(
    lambda: &LambdaProcedure,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    if args.len() != lambda.params.len() {
        return Err(EvalError::WrongArgumentCount {
            name: lambda.name.clone(),
            expected: lambda.params.len().to_string(),
            got: args.len(),
        });
    }

    let call_env = Env::child(lambda.env.clone());
    {
        let mut scope = call_env.borrow_mut();
        for (param, arg) in lambda.params.iter().cloned().zip(args) {
            scope.bindings.insert(param, arg);
        }
    }

    eval_sequence(&lambda.body, call_env, ctx)
}

fn builtin_not(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("not", args, 1)?;
    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn builtin_cons(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("cons", args, 2)?;
    Ok(Value::Pair(
        Box::new(args[0].clone()),
        Box::new(args[1].clone()),
    ))
}

fn builtin_car(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("car", args, 1)?;
    let Value::Pair(car, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            actual: args[0].type_name(),
        });
    };
    Ok((**car).clone())
}

fn builtin_cdr(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("cdr", args, 1)?;
    let Value::Pair(_, cdr) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            actual: args[0].type_name(),
        });
    };
    Ok((**cdr).clone())
}

fn builtin_null_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("null?", args, 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::EmptyList)))
}

fn builtin_list(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    Ok(args.iter().rev().fold(Value::EmptyList, |tail, value| {
        Value::Pair(Box::new(value.clone()), Box::new(tail))
    }))
}

fn builtin_length(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("length", args, 1)?;
    Ok(Value::Integer(list_length(&args[0])? as i64))
}

fn builtin_append(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut result = args.last().cloned().unwrap_or(Value::EmptyList);

    for value in args[..args.len().saturating_sub(1)].iter().rev() {
        result = copy_list_with_tail(value, result)?;
    }

    Ok(result)
}

fn builtin_string_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("string?", args, |value| matches!(value, Value::String(_)))
}

fn builtin_number_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("number?", args, |value| matches!(value, Value::Integer(_)))
}

fn builtin_boolean_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("boolean?", args, |value| matches!(value, Value::Boolean(_)))
}

fn builtin_pair_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("pair?", args, |value| matches!(value, Value::Pair(_, _)))
}

fn builtin_symbol_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("symbol?", args, |value| matches!(value, Value::Symbol(_)))
}

fn builtin_char_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("char?", args, |value| matches!(value, Value::Char(_)))
}

fn builtin_display(args: &[Value], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("display", args, 1)?;
    ctx.emit(&args[0].render_for_display());
    Ok(Value::Void)
}

fn builtin_write(args: &[Value], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("write", args, 1)?;
    ctx.emit(&args[0].render());
    Ok(Value::Void)
}

fn builtin_newline(args: &[Value], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("newline", args, 0)?;
    ctx.emit("\n");
    Ok(Value::Void)
}

fn builtin_string_append(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut result = String::new();

    for value in args {
        result.push_str(&expect_string(value)?.to_plain_string());
    }

    Ok(Value::String(SchemeString::new(result)))
}

fn builtin_string_length(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string-length", args, 1)?;
    Ok(Value::Integer(expect_string(&args[0])?.len() as i64))
}

fn builtin_substring(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("substring", args, 3)?;
    let source = expect_string(&args[0])?;
    let start = expect_index(&args[1], "substring")?;
    let end = expect_index(&args[2], "substring")?;

    Ok(Value::String(source.substring(start, end)?))
}

fn builtin_string_to_number(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string->number", args, 1)?;
    let source = expect_string(&args[0])?.to_plain_string();
    Ok(parse_integer_token(&source)
        .map(Value::Integer)
        .unwrap_or(Value::Boolean(false)))
}

fn builtin_number_to_string(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("number->string", args, 1)?;
    let Value::Integer(value) = args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "number",
            actual: args[0].type_name(),
        });
    };
    Ok(Value::String(SchemeString::new(value.to_string())))
}

fn builtin_symbol_to_string(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("symbol->string", args, 1)?;
    let Value::Symbol(ref value) = args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "symbol",
            actual: args[0].type_name(),
        });
    };
    Ok(Value::String(SchemeString::new(value)))
}

fn builtin_string_to_symbol(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string->symbol", args, 1)?;
    Ok(Value::Symbol(expect_string(&args[0])?.to_plain_string()))
}

fn builtin_string_ref(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string-ref", args, 2)?;
    let source = expect_string(&args[0])?;
    let index = expect_index(&args[1], "string-ref")?;

    Ok(Value::Char(source.get(index, "string-ref")?))
}

fn builtin_string_copy(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string-copy", args, 1)?;
    Ok(Value::String(expect_string(&args[0])?.deep_copy()))
}

fn builtin_string_set(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string-set!", args, 3)?;
    let string = expect_string(&args[0])?;
    let index = expect_index(&args[1], "string-set!")?;
    let ch = expect_char(&args[2])?;

    string.set(index, ch, "string-set!")?;
    Ok(Value::Void)
}

fn builtin_add(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;
    Ok(Value::Integer(numbers.into_iter().sum()))
}

fn builtin_mul(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;
    Ok(Value::Integer(numbers.into_iter().product()))
}

fn builtin_sub(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;

    match numbers.as_slice() {
        [] => Err(EvalError::WrongArgumentCount {
            name: "-".to_string(),
            expected: "at least 1".to_string(),
            got: 0,
        }),
        [value] => Ok(Value::Integer(-(*value))),
        [first, rest @ ..] => Ok(Value::Integer(
            rest.iter().fold(*first, |acc, value| acc - *value),
        )),
    }
}

fn builtin_div(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;

    match numbers.as_slice() {
        [] | [_] => Err(EvalError::WrongArgumentCount {
            name: "/".to_string(),
            expected: "at least 2".to_string(),
            got: numbers.len(),
        }),
        [first, rest @ ..] => {
            let mut result = *first;
            for divisor in rest {
                if *divisor == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= *divisor;
            }
            Ok(Value::Integer(result))
        }
    }
}

fn builtin_less_than(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_compare("<", args, |left, right| left < right)
}

fn builtin_greater_than(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_compare(">", args, |left, right| left > right)
}

fn builtin_equal_numbers(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_compare("=", args, |left, right| left == right)
}

fn builtin_less_equal(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_compare("<=", args, |left, right| left <= right)
}

fn builtin_compare(
    name: &str,
    args: &[Value],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;

    if numbers.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: "at least 2".to_string(),
            got: numbers.len(),
        });
    }

    let is_match = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Boolean(is_match))
}

fn builtin_type_predicate(
    name: &str,
    args: &[Value],
    predicate: impl Fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    expect_value_arity(name, args, 1)?;
    Ok(Value::Boolean(predicate(&args[0])))
}

fn list_length(value: &Value) -> Result<usize, EvalError> {
    let mut count = 0;
    let mut cursor = value;

    loop {
        match cursor {
            Value::EmptyList => return Ok(count),
            Value::Pair(_, cdr) => {
                count += 1;
                cursor = cdr.as_ref();
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "pair",
                    actual: other.type_name(),
                });
            }
        }
    }
}

fn copy_list_with_tail(list: &Value, tail: Value) -> Result<Value, EvalError> {
    match list {
        Value::EmptyList => Ok(tail),
        Value::Pair(car, cdr) => Ok(Value::Pair(
            Box::new((**car).clone()),
            Box::new(copy_list_with_tail(cdr.as_ref(), tail)?),
        )),
        other => Err(EvalError::TypeMismatch {
            expected: "pair",
            actual: other.type_name(),
        }),
    }
}

fn expect_numbers(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    let mut values = Vec::with_capacity(args.len());

    for value in args {
        match value {
            Value::Integer(number) => values.push(*number),
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "number",
                    actual: other.type_name(),
                });
            }
        }
    }

    Ok(values)
}

fn expect_string(value: &Value) -> Result<&SchemeString, EvalError> {
    match value {
        Value::String(text) => Ok(text),
        other => Err(EvalError::TypeMismatch {
            expected: "string",
            actual: other.type_name(),
        }),
    }
}

fn expect_char(value: &Value) -> Result<char, EvalError> {
    match value {
        Value::Char(ch) => Ok(*ch),
        other => Err(EvalError::TypeMismatch {
            expected: "char",
            actual: other.type_name(),
        }),
    }
}

fn expect_index(value: &Value, kind: &'static str) -> Result<usize, EvalError> {
    match value {
        Value::Integer(index) if *index >= 0 => Ok(*index as usize),
        Value::Integer(index) => Err(EvalError::NegativeIndex {
            kind,
            value: *index,
        }),
        other => Err(EvalError::TypeMismatch {
            expected: "number",
            actual: other.type_name(),
        }),
    }
}

fn expect_expr_arity(name: &str, args: &[Expr], expected: usize) -> Result<(), EvalError> {
    if args.len() != expected {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: expected.to_string(),
            got: args.len(),
        });
    }

    Ok(())
}

fn expect_value_arity(name: &str, args: &[Value], expected: usize) -> Result<(), EvalError> {
    if args.len() != expected {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: expected.to_string(),
            got: args.len(),
        });
    }

    Ok(())
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (value, _) = eval_input(input, false)?;
    Ok(value.render())
}

fn eval_input(input: &str, capture_output: bool) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_program().map_err(|error| {
        let (line, column) = line_col_at(input, parser.index);
        error.with_position(line, column)
    })?;
    let mut ctx = EvalContext::new(capture_output);
    let value = eval_program(&exprs, &mut ctx).map_err(|error| error.with_position(1, 1))?;
    Ok((value, ctx.finish()))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_input(input, true)?;
    Ok((value.render(), output))
}

#[cfg(test)]
mod tests;
