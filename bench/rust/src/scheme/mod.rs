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

        chars
            .get(index)
            .copied()
            .ok_or(EvalError::IndexOutOfBounds {
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
    params: ParameterList,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone)]
struct ParameterList {
    required: Vec<String>,
    rest: Option<String>,
}

impl ParameterList {
    fn expected_arity(&self) -> String {
        match self.rest {
            Some(_) => format!("at least {}", self.required.len()),
            None => self.required.len().to_string(),
        }
    }

    fn accepts(&self, arg_count: usize) -> bool {
        if self.rest.is_some() {
            arg_count >= self.required.len()
        } else {
            arg_count == self.required.len()
        }
    }
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
    define_builtin(&env, "eq?", builtin_eq);
    define_builtin(&env, "equal?", builtin_equal);
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
    define_builtin(&env, "apply", builtin_apply);
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
    define_builtin(&env, "abs", builtin_abs);
    define_builtin(&env, "modulo", builtin_modulo);
    define_builtin(&env, "remainder", builtin_remainder);
    define_builtin(&env, "quotient", builtin_quotient);
    define_builtin(&env, "min", builtin_min);
    define_builtin(&env, "max", builtin_max);
    define_builtin(&env, "expt", builtin_expt);
    define_builtin(&env, "zero?", builtin_zero_predicate);
    define_builtin(&env, "positive?", builtin_positive_predicate);
    define_builtin(&env, "negative?", builtin_negative_predicate);
    define_builtin(&env, "odd?", builtin_odd_predicate);
    define_builtin(&env, "even?", builtin_even_predicate);
    define_builtin(&env, "list-ref", builtin_list_ref);
    define_builtin(&env, "list-tail", builtin_list_tail);
    define_builtin(&env, "list?", builtin_list_predicate);
    define_builtin(&env, "assoc", builtin_assoc);
    define_builtin(&env, "map", builtin_map);
    define_builtin(&env, "char-alphabetic?", builtin_char_alphabetic_predicate);
    define_builtin(&env, "char-numeric?", builtin_char_numeric_predicate);
    define_builtin(&env, "char-upcase", builtin_char_upcase);
    define_builtin(&env, "char-downcase", builtin_char_downcase);
    define_builtin(&env, "char=?", builtin_char_equal);
    define_builtin(&env, "char<?", builtin_char_less_than);
    define_builtin(&env, "string=?", builtin_string_equal);
    define_builtin(&env, "string<?", builtin_string_less_than);
    define_builtin(&env, "string-ci=?", builtin_string_ci_equal);
    define_builtin(&env, "string-upcase", builtin_string_upcase);
    define_builtin(&env, "string-downcase", builtin_string_downcase);

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

fn assign(env: &EnvRef, name: &str, value: Value) -> Result<(), EvalError> {
    let parent = {
        let mut scope = env.borrow_mut();

        if let Some(slot) = scope.bindings.get_mut(name) {
            *slot = value;
            return Ok(());
        }

        scope.parent.clone()
    };

    if let Some(parent) = parent {
        assign(&parent, name, value)
    } else {
        Err(EvalError::UnboundVariable {
            name: name.to_string(),
        })
    }
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
            "set!" => return eval_set(args, env, ctx),
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

fn eval_set(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_expr_arity("set!", args, 2)?;

    let Expr::Symbol(name) = &args[0] else {
        return Err(EvalError::SyntaxError {
            message: "set! requires a symbol target".to_string(),
        });
    };

    let value = eval_expr(&args[1], env.clone(), ctx)?;
    assign(&env, name, value)?;
    Ok(Value::Void)
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
    let params = ParameterList {
        required: bindings.iter().map(|(param, _)| param.clone()).collect(),
        rest: None,
    };
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

fn parse_param_list(expr: &Expr) -> Result<ParameterList, EvalError> {
    let Expr::List(params) = expr else {
        return Err(EvalError::SyntaxError {
            message: "lambda parameters must be a list".to_string(),
        });
    };

    parse_param_slice(params)
}

fn parse_param_slice(params: &[Expr]) -> Result<ParameterList, EvalError> {
    let mut required = Vec::with_capacity(params.len());
    let mut index = 0;

    while index < params.len() {
        let Expr::Symbol(name) = &params[index] else {
            return Err(EvalError::SyntaxError {
                message: "parameter names must be symbols".to_string(),
            });
        };

        if name == "." {
            if index + 2 != params.len() {
                return Err(EvalError::SyntaxError {
                    message: "dot notation requires exactly one rest parameter".to_string(),
                });
            }

            let Expr::Symbol(rest) = &params[index + 1] else {
                return Err(EvalError::SyntaxError {
                    message: "parameter names must be symbols".to_string(),
                });
            };

            if rest == "." {
                return Err(EvalError::SyntaxError {
                    message: "parameter names must be symbols".to_string(),
                });
            }

            return Ok(ParameterList {
                required,
                rest: Some(rest.clone()),
            });
        }

        required.push(name.clone());
        index += 1;
    }

    Ok(ParameterList {
        required,
        rest: None,
    })
}

fn make_lambda(name: String, params: ParameterList, body: Vec<Expr>, env: EnvRef) -> Value {
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
    if !lambda.params.accepts(args.len()) {
        return Err(EvalError::WrongArgumentCount {
            name: lambda.name.clone(),
            expected: lambda.params.expected_arity(),
            got: args.len(),
        });
    }

    let call_env = Env::child(lambda.env.clone());
    {
        let mut scope = call_env.borrow_mut();
        let mut args = args.into_iter();

        for param in lambda.params.required.iter().cloned() {
            let arg = args
                .next()
                .expect("arity check should ensure enough arguments");
            scope.bindings.insert(param, arg);
        }

        if let Some(rest) = &lambda.params.rest {
            scope
                .bindings
                .insert(rest.clone(), list_from_values(args.collect()));
        }
    }

    eval_sequence(&lambda.body, call_env, ctx)
}

fn builtin_not(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("not", args, 1)?;
    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn builtin_eq(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("eq?", args, 2)?;
    Ok(Value::Boolean(equal_values(&args[0], &args[1])))
}

fn builtin_equal(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("equal?", args, 2)?;
    Ok(Value::Boolean(equal_values(&args[0], &args[1])))
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

fn builtin_apply(args: &[Value], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: "apply".to_string(),
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }

    let (operator, rest) = args
        .split_first()
        .expect("arity check should ensure operator");
    let mut applied_args = rest[..rest.len() - 1].to_vec();
    applied_args.extend(expect_list_values(&rest[rest.len() - 1])?);

    apply_evaluated(operator.clone(), applied_args, ctx)
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

fn builtin_abs(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("abs", args, 1)?;
    Ok(Value::Integer(
        expect_number(&args[0])?
            .checked_abs()
            .ok_or(EvalError::NumericOverflow { operation: "abs" })?,
    ))
}

fn builtin_modulo(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let (dividend, divisor) = expect_two_numbers("modulo", args)?;

    let remainder = dividend % divisor;
    let result = if remainder != 0 && (remainder < 0) != (divisor < 0) {
        remainder + divisor
    } else {
        remainder
    };

    Ok(Value::Integer(result))
}

fn builtin_remainder(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let (dividend, divisor) = expect_two_numbers("remainder", args)?;
    Ok(Value::Integer(dividend % divisor))
}

fn builtin_quotient(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let (dividend, divisor) = expect_two_numbers("quotient", args)?;
    Ok(Value::Integer(dividend.checked_div(divisor).ok_or(
        EvalError::NumericOverflow {
            operation: "quotient",
        },
    )?))
}

fn builtin_min(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_min_max("min", args, |left, right| left.min(right))
}

fn builtin_max(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_min_max("max", args, |left, right| left.max(right))
}

fn builtin_expt(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("expt", args, 2)?;
    let base = expect_number(&args[0])?;
    let exponent = expect_number(&args[1])?;

    if exponent < 0 {
        return Err(EvalError::NegativeExponent { value: exponent });
    }

    let mut result = 1_i64;
    for _ in 0..exponent {
        result = result
            .checked_mul(base)
            .ok_or(EvalError::NumericOverflow { operation: "expt" })?;
    }

    Ok(Value::Integer(result))
}

fn builtin_zero_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_number_predicate_by("zero?", args, |value| value == 0)
}

fn builtin_positive_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_number_predicate_by("positive?", args, |value| value > 0)
}

fn builtin_negative_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_number_predicate_by("negative?", args, |value| value < 0)
}

fn builtin_odd_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_number_predicate_by("odd?", args, |value| value % 2 != 0)
}

fn builtin_even_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_number_predicate_by("even?", args, |value| value % 2 == 0)
}

fn builtin_list_ref(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("list-ref", args, 2)?;
    let index = expect_index(&args[1], "list-ref")?;

    match list_tail_at(&args[0], index, "list-ref")? {
        Value::Pair(car, _) => Ok(*car),
        Value::EmptyList => Err(EvalError::IndexOutOfBounds {
            kind: "list-ref",
            index,
            length: index,
        }),
        other => Err(EvalError::TypeMismatch {
            expected: "pair",
            actual: other.type_name(),
        }),
    }
}

fn builtin_list_tail(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("list-tail", args, 2)?;
    let index = expect_index(&args[1], "list-tail")?;
    list_tail_at(&args[0], index, "list-tail")
}

fn builtin_list_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("list?", args, 1)?;
    Ok(Value::Boolean(is_proper_list(&args[0])))
}

fn builtin_assoc(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("assoc", args, 2)?;
    let key = &args[0];
    let mut cursor = &args[1];

    loop {
        match cursor {
            Value::EmptyList => return Ok(Value::Boolean(false)),
            Value::Pair(entry, rest) => {
                let Value::Pair(found_key, _) = entry.as_ref() else {
                    return Err(EvalError::TypeMismatch {
                        expected: "pair",
                        actual: entry.as_ref().type_name(),
                    });
                };

                if equal_values(key, found_key.as_ref()) {
                    return Ok((**entry).clone());
                }

                cursor = rest.as_ref();
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

fn builtin_map(args: &[Value], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: "map".to_string(),
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }

    let procedure = expect_procedure(&args[0])?;
    let list_values = args[1..]
        .iter()
        .map(expect_list_values)
        .collect::<Result<Vec<_>, _>>()?;

    let expected_len = list_values.first().map_or(0, Vec::len);
    for values in &list_values[1..] {
        if values.len() != expected_len {
            return Err(EvalError::MismatchedListLengths {
                name: "map".to_string(),
                expected: expected_len,
                got: values.len(),
            });
        }
    }

    let mut results = Vec::with_capacity(expected_len);
    for index in 0..expected_len {
        let call_args = list_values
            .iter()
            .map(|values| values[index].clone())
            .collect();
        results.push(apply_procedure(procedure.clone(), call_args, ctx)?);
    }

    Ok(list_from_values(results))
}

fn builtin_char_alphabetic_predicate(
    args: &[Value],
    _ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    expect_value_arity("char-alphabetic?", args, 1)?;
    Ok(Value::Boolean(expect_char(&args[0])?.is_alphabetic()))
}

fn builtin_char_numeric_predicate(
    args: &[Value],
    _ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    expect_value_arity("char-numeric?", args, 1)?;
    Ok(Value::Boolean(expect_char(&args[0])?.is_numeric()))
}

fn builtin_char_upcase(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("char-upcase", args, 1)?;
    let ch = expect_char(&args[0])?;
    Ok(Value::Char(ch.to_uppercase().next().unwrap_or(ch)))
}

fn builtin_char_downcase(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("char-downcase", args, 1)?;
    let ch = expect_char(&args[0])?;
    Ok(Value::Char(ch.to_lowercase().next().unwrap_or(ch)))
}

fn builtin_char_equal(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_char_compare("char=?", args, |left, right| left == right)
}

fn builtin_char_less_than(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_char_compare("char<?", args, |left, right| left < right)
}

fn builtin_string_equal(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_string_compare(
        "string=?",
        args,
        |value| value.to_string(),
        |left, right| left == right,
    )
}

fn builtin_string_less_than(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_string_compare(
        "string<?",
        args,
        |value| value.to_string(),
        |left, right| left < right,
    )
}

fn builtin_string_ci_equal(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_string_compare(
        "string-ci=?",
        args,
        |value| value.chars().flat_map(char::to_lowercase).collect(),
        |left, right| left == right,
    )
}

fn builtin_string_upcase(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string-upcase", args, 1)?;
    let string = expect_string(&args[0])?.to_plain_string();
    Ok(Value::String(SchemeString::new(
        string
            .chars()
            .flat_map(char::to_uppercase)
            .collect::<String>(),
    )))
}

fn builtin_string_downcase(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string-downcase", args, 1)?;
    let string = expect_string(&args[0])?.to_plain_string();
    Ok(Value::String(SchemeString::new(
        string
            .chars()
            .flat_map(char::to_lowercase)
            .collect::<String>(),
    )))
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

fn builtin_min_max(
    name: &str,
    args: &[Value],
    pick: impl Fn(i64, i64) -> i64,
) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;

    let Some(first) = numbers.first().copied() else {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: "at least 1".to_string(),
            got: 0,
        });
    };

    let value = numbers
        .into_iter()
        .skip(1)
        .fold(first, |current, next| pick(current, next));
    Ok(Value::Integer(value))
}

fn builtin_number_predicate_by(
    name: &str,
    args: &[Value],
    predicate: impl Fn(i64) -> bool,
) -> Result<Value, EvalError> {
    expect_value_arity(name, args, 1)?;
    Ok(Value::Boolean(predicate(expect_number(&args[0])?)))
}

fn builtin_char_compare(
    name: &str,
    args: &[Value],
    predicate: impl Fn(char, char) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }

    let mut chars = Vec::with_capacity(args.len());
    for value in args {
        chars.push(expect_char(value)?);
    }

    Ok(Value::Boolean(
        chars.windows(2).all(|pair| predicate(pair[0], pair[1])),
    ))
}

fn builtin_string_compare(
    name: &str,
    args: &[Value],
    normalize: impl Fn(&str) -> String,
    predicate: impl Fn(&str, &str) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }

    let mut strings = Vec::with_capacity(args.len());
    for value in args {
        strings.push(normalize(&expect_string(value)?.to_plain_string()));
    }

    Ok(Value::Boolean(
        strings
            .windows(2)
            .all(|pair| predicate(pair[0].as_str(), pair[1].as_str())),
    ))
}

fn is_proper_list(value: &Value) -> bool {
    let mut cursor = value;

    loop {
        match cursor {
            Value::EmptyList => return true,
            Value::Pair(_, cdr) => cursor = cdr.as_ref(),
            _ => return false,
        }
    }
}

fn list_tail_at(value: &Value, index: usize, kind: &'static str) -> Result<Value, EvalError> {
    let mut cursor = value;

    for depth in 0..index {
        cursor = match cursor {
            Value::Pair(_, cdr) => cdr.as_ref(),
            Value::EmptyList => {
                return Err(EvalError::IndexOutOfBounds {
                    kind,
                    index,
                    length: depth,
                });
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "pair",
                    actual: other.type_name(),
                });
            }
        };
    }

    Ok(cursor.clone())
}

fn equal_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Integer(left), Value::Integer(right)) => left == right,
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => {
            left.to_plain_string() == right.to_plain_string()
        }
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left_car, left_cdr), Value::Pair(right_car, right_cdr)) => {
            equal_values(left_car.as_ref(), right_car.as_ref())
                && equal_values(left_cdr.as_ref(), right_cdr.as_ref())
        }
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
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

fn list_from_values(values: Vec<Value>) -> Value {
    values
        .into_iter()
        .rev()
        .fold(Value::EmptyList, |tail, value| {
            Value::Pair(Box::new(value), Box::new(tail))
        })
}

fn expect_list_values(value: &Value) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::new();
    let mut cursor = value;

    loop {
        match cursor {
            Value::EmptyList => return Ok(values),
            Value::Pair(car, cdr) => {
                values.push((**car).clone());
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

fn expect_numbers(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    let mut values = Vec::with_capacity(args.len());

    for value in args {
        values.push(expect_number(value)?);
    }

    Ok(values)
}

fn expect_number(value: &Value) -> Result<i64, EvalError> {
    match value {
        Value::Integer(number) => Ok(*number),
        other => Err(EvalError::TypeMismatch {
            expected: "number",
            actual: other.type_name(),
        }),
    }
}

fn expect_two_numbers(name: &str, args: &[Value]) -> Result<(i64, i64), EvalError> {
    expect_value_arity(name, args, 2)?;
    let left = expect_number(&args[0])?;
    let right = expect_number(&args[1])?;

    if right == 0 {
        return Err(EvalError::DivisionByZero);
    }

    Ok((left, right))
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

fn expect_procedure(value: &Value) -> Result<Rc<Procedure>, EvalError> {
    match value {
        Value::Procedure(procedure) => Ok(procedure.clone()),
        _ => Err(EvalError::NotAProcedure),
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
