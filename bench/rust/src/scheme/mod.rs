pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type EnvRef = Rc<RefCell<Environment>>;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    eval_str_with_output(input).map(|(result, _)| result)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let expressions = Parser::new(input).parse_program()?;
    if expressions.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let env = global_env();
    let mut interpreter = Interpreter::default();
    let mut last = Value::Void;

    for expr in &expressions {
        last = interpreter.eval_expr(expr, env.clone())?;
    }

    Ok((last.to_scheme_string(), interpreter.output))
}

#[derive(Default)]
struct Interpreter {
    output: String,
}

impl Interpreter {
    fn eval_expr(&mut self, expr: &Expr, env: EnvRef) -> Result<Value, EvalError> {
        match expr {
            Expr::Int(value) => Ok(Value::Int(*value)),
            Expr::Bool(value) => Ok(Value::Bool(*value)),
            Expr::String(value) => Ok(Value::String(value.clone())),
            Expr::Char(value) => Ok(Value::Char(*value)),
            Expr::Symbol(name) => env
                .borrow()
                .lookup(name)
                .ok_or_else(|| EvalError::UnboundVariable(name.clone())),
            Expr::List(items) => self.eval_list(items, env),
        }
    }

    fn eval_list(&mut self, items: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
        if items.is_empty() {
            return Err(EvalError::InvalidForm(
                "cannot evaluate an empty list".to_string(),
            ));
        }

        if let Expr::Symbol(name) = &items[0] {
            match name.as_str() {
                "define" => return self.eval_define(&items[1..], env),
                "lambda" => return self.eval_lambda(&items[1..], env),
                "if" => return self.eval_if(&items[1..], env),
                "let" => return self.eval_let(&items[1..], env),
                "and" => return self.eval_and(&items[1..], env),
                "or" => return self.eval_or(&items[1..], env),
                "begin" => return self.eval_body(&items[1..], env),
                _ => {}
            }
        }

        let operator = self.eval_expr(&items[0], env.clone())?;
        let mut args = Vec::with_capacity(items.len().saturating_sub(1));
        for expr in &items[1..] {
            args.push(self.eval_expr(expr, env.clone())?);
        }
        self.apply(operator, args)
    }

    fn eval_define(&mut self, args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::InvalidForm(
                "define expects a target and at least one body expression".to_string(),
            ));
        }

        match &args[0] {
            Expr::Symbol(name) => {
                if args.len() != 2 {
                    return Err(EvalError::InvalidForm(
                        "variable define expects exactly one value expression".to_string(),
                    ));
                }
                let value = self.eval_expr(&args[1], env.clone())?;
                env.borrow_mut().define(name.clone(), value);
                Ok(Value::Void)
            }
            Expr::List(signature) => {
                if signature.is_empty() {
                    return Err(EvalError::InvalidForm(
                        "function define requires a name".to_string(),
                    ));
                }

                let name = match &signature[0] {
                    Expr::Symbol(name) => name.clone(),
                    _ => {
                        return Err(EvalError::InvalidForm(
                            "function define requires a symbol name".to_string(),
                        ));
                    }
                };

                let params = parse_parameters(&signature[1..])?;
                let body = args[1..].to_vec();
                let procedure = Value::Procedure(Procedure::Lambda(Lambda {
                    name: Some(name.clone()),
                    params,
                    body,
                    env: env.clone(),
                }));
                env.borrow_mut().define(name, procedure);
                Ok(Value::Void)
            }
            _ => Err(EvalError::InvalidForm("invalid define target".to_string())),
        }
    }

    fn eval_lambda(&mut self, args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::InvalidForm(
                "lambda expects parameters and at least one body expression".to_string(),
            ));
        }

        let params = match &args[0] {
            Expr::List(items) => parse_parameters(items)?,
            _ => {
                return Err(EvalError::InvalidForm(
                    "lambda parameters must be a list".to_string(),
                ));
            }
        };

        Ok(Value::Procedure(Procedure::Lambda(Lambda {
            name: None,
            params,
            body: args[1..].to_vec(),
            env,
        })))
    }

    fn eval_if(&mut self, args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
        if !(2..=3).contains(&args.len()) {
            return Err(EvalError::WrongArity {
                name: "if".to_string(),
                expected: "2 or 3".to_string(),
                got: args.len(),
            });
        }

        let condition = self.eval_expr(&args[0], env.clone())?;
        if is_truthy(&condition) {
            self.eval_expr(&args[1], env)
        } else if args.len() == 3 {
            self.eval_expr(&args[2], env)
        } else {
            Ok(Value::Void)
        }
    }

    fn eval_let(&mut self, args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::InvalidForm(
                "let expects bindings and at least one body expression".to_string(),
            ));
        }

        let bindings = match &args[0] {
            Expr::List(bindings) => bindings,
            _ => {
                return Err(EvalError::InvalidForm(
                    "let bindings must be a list".to_string(),
                ));
            }
        };

        let mut evaluated = Vec::with_capacity(bindings.len());
        for binding in bindings {
            let pair = match binding {
                Expr::List(pair) if pair.len() == 2 => pair,
                _ => {
                    return Err(EvalError::InvalidForm(
                        "let bindings must be (name value) pairs".to_string(),
                    ));
                }
            };

            let name = match &pair[0] {
                Expr::Symbol(name) => name.clone(),
                _ => {
                    return Err(EvalError::InvalidForm(
                        "let binding names must be symbols".to_string(),
                    ));
                }
            };

            let value = self.eval_expr(&pair[1], env.clone())?;
            evaluated.push((name, value));
        }

        let child = Environment::child(env);
        {
            let mut scope = child.borrow_mut();
            for (name, value) in evaluated {
                scope.define(name, value);
            }
        }

        self.eval_body(&args[1..], child)
    }

    fn eval_and(&mut self, args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
        let mut last = Value::Bool(true);
        for expr in args {
            last = self.eval_expr(expr, env.clone())?;
            if !is_truthy(&last) {
                return Ok(last);
            }
        }
        Ok(last)
    }

    fn eval_or(&mut self, args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
        for expr in args {
            let value = self.eval_expr(expr, env.clone())?;
            if is_truthy(&value) {
                return Ok(value);
            }
        }
        Ok(Value::Bool(false))
    }

    fn eval_body(&mut self, expressions: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
        if expressions.is_empty() {
            return Ok(Value::Void);
        }

        let mut last = Value::Void;
        for expr in expressions {
            last = self.eval_expr(expr, env.clone())?;
        }
        Ok(last)
    }

    fn apply(&mut self, procedure: Value, args: Vec<Value>) -> Result<Value, EvalError> {
        match procedure {
            Value::Procedure(Procedure::Builtin(builtin)) => self.apply_builtin(builtin, args),
            Value::Procedure(Procedure::Lambda(lambda)) => self.apply_lambda(lambda, args),
            _ => Err(EvalError::Message(
                "attempted to call a non-procedure".to_string(),
            )),
        }
    }

    fn apply_lambda(&mut self, lambda: Lambda, args: Vec<Value>) -> Result<Value, EvalError> {
        if lambda.params.len() != args.len() {
            return Err(EvalError::WrongArity {
                name: lambda.name.clone().unwrap_or_else(|| "lambda".to_string()),
                expected: lambda.params.len().to_string(),
                got: args.len(),
            });
        }

        let child = Environment::child(lambda.env);
        {
            let mut scope = child.borrow_mut();
            for (name, value) in lambda.params.iter().cloned().zip(args) {
                scope.define(name, value);
            }
        }

        self.eval_body(&lambda.body, child)
    }

    fn apply_builtin(&mut self, builtin: Builtin, args: Vec<Value>) -> Result<Value, EvalError> {
        match builtin {
            Builtin::Add => {
                let mut total = 0_i64;
                for arg in &args {
                    total += expect_int(arg, builtin.name())?;
                }
                Ok(Value::Int(total))
            }
            Builtin::Sub => {
                require_at_least_arity(args.len(), 1, builtin.name())?;
                let first = expect_int(&args[0], builtin.name())?;
                if args.len() == 1 {
                    return Ok(Value::Int(-first));
                }

                let mut total = first;
                for arg in &args[1..] {
                    total -= expect_int(arg, builtin.name())?;
                }
                Ok(Value::Int(total))
            }
            Builtin::Less => compare_numbers(&args, builtin.name(), |left, right| left < right),
            Builtin::LessEqual => {
                compare_numbers(&args, builtin.name(), |left, right| left <= right)
            }
            Builtin::Greater => compare_numbers(&args, builtin.name(), |left, right| left > right),
            Builtin::GreaterEqual => {
                compare_numbers(&args, builtin.name(), |left, right| left >= right)
            }
            Builtin::NumericEqual => {
                compare_numbers(&args, builtin.name(), |left, right| left == right)
            }
            Builtin::List => Ok(Value::List(args)),
            Builtin::Map => self.builtin_map(args),
            Builtin::StringToList => {
                require_exact_arity(args.len(), 1, builtin.name())?;
                let value = expect_string(&args[0], builtin.name())?;
                Ok(Value::List(value.chars().map(Value::Char).collect()))
            }
            Builtin::ListToString => {
                require_exact_arity(args.len(), 1, builtin.name())?;
                let items = expect_list(&args[0], builtin.name())?;
                let mut value = String::new();
                for item in items {
                    value.push(expect_char(item, builtin.name())?);
                }
                Ok(Value::String(value))
            }
            Builtin::StringSet => {
                require_exact_arity(args.len(), 3, builtin.name())?;
                Err(EvalError::ImmutableString)
            }
            Builtin::CharToInteger => {
                require_exact_arity(args.len(), 1, builtin.name())?;
                Ok(Value::Int(
                    expect_char(&args[0], builtin.name())? as u32 as i64
                ))
            }
            Builtin::IntegerToChar => {
                require_exact_arity(args.len(), 1, builtin.name())?;
                let value = expect_int(&args[0], builtin.name())?;
                if value < 0 {
                    return Err(EvalError::Type(format!(
                        "{0} expects a valid character code point",
                        builtin.name()
                    )));
                }
                let scalar = u32::try_from(value).map_err(|_| {
                    EvalError::Type(format!(
                        "{0} expects a valid character code point",
                        builtin.name()
                    ))
                })?;
                let ch = char::from_u32(scalar).ok_or_else(|| {
                    EvalError::Type(format!(
                        "{0} expects a valid character code point",
                        builtin.name()
                    ))
                })?;
                Ok(Value::Char(ch))
            }
            Builtin::StringCopy => {
                require_exact_arity(args.len(), 1, builtin.name())?;
                Ok(Value::String(
                    expect_string(&args[0], builtin.name())?.to_string(),
                ))
            }
            Builtin::Display => {
                require_exact_arity(args.len(), 1, builtin.name())?;
                self.output.push_str(&args[0].to_display_string());
                Ok(Value::Void)
            }
            Builtin::Write => {
                require_exact_arity(args.len(), 1, builtin.name())?;
                self.output.push_str(&args[0].to_scheme_string());
                Ok(Value::Void)
            }
            Builtin::Newline => {
                require_exact_arity(args.len(), 0, builtin.name())?;
                self.output.push('\n');
                Ok(Value::Void)
            }
        }
    }

    fn builtin_map(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
        require_at_least_arity(args.len(), 2, "map")?;
        let procedure = args[0].clone();
        let lists = args[1..]
            .iter()
            .map(|value| expect_list(value, "map").map(|items| items.to_vec()))
            .collect::<Result<Vec<_>, _>>()?;

        let expected_len = lists[0].len();
        if lists.iter().any(|items| items.len() != expected_len) {
            return Err(EvalError::Message(
                "map expects lists with the same length".to_string(),
            ));
        }

        let mut result = Vec::with_capacity(expected_len);
        for index in 0..expected_len {
            let mapped_args = lists
                .iter()
                .map(|items| items[index].clone())
                .collect::<Vec<_>>();
            result.push(self.apply(procedure.clone(), mapped_args)?);
        }

        Ok(Value::List(result))
    }
}

#[derive(Clone, Debug)]
enum Expr {
    Int(i64),
    Bool(bool),
    String(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone)]
enum Value {
    Int(i64),
    Bool(bool),
    String(String),
    Char(char),
    List(Vec<Value>),
    Procedure(Procedure),
    Void,
}

impl Value {
    fn to_scheme_string(&self) -> String {
        match self {
            Value::Int(value) => value.to_string(),
            Value::Bool(true) => "#t".to_string(),
            Value::Bool(false) => "#f".to_string(),
            Value::String(value) => quote_string(value),
            Value::Char(value) => char_literal(*value),
            Value::List(items) => {
                let parts = items
                    .iter()
                    .map(Value::to_scheme_string)
                    .collect::<Vec<_>>();
                format!("({})", parts.join(" "))
            }
            Value::Procedure(_) => "#<procedure>".to_string(),
            Value::Void => String::new(),
        }
    }

    fn to_display_string(&self) -> String {
        match self {
            Value::String(value) => value.clone(),
            Value::Char(value) => value.to_string(),
            _ => self.to_scheme_string(),
        }
    }

    fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) => "integer",
            Value::Bool(_) => "boolean",
            Value::String(_) => "string",
            Value::Char(_) => "character",
            Value::List(_) => "list",
            Value::Procedure(_) => "procedure",
            Value::Void => "void",
        }
    }
}

#[derive(Clone)]
enum Procedure {
    Builtin(Builtin),
    Lambda(Lambda),
}

#[derive(Clone)]
struct Lambda {
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone, Copy)]
enum Builtin {
    Add,
    Sub,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    NumericEqual,
    List,
    Map,
    StringToList,
    ListToString,
    StringSet,
    CharToInteger,
    IntegerToChar,
    StringCopy,
    Display,
    Write,
    Newline,
}

impl Builtin {
    fn name(self) -> &'static str {
        match self {
            Builtin::Add => "+",
            Builtin::Sub => "-",
            Builtin::Less => "<",
            Builtin::LessEqual => "<=",
            Builtin::Greater => ">",
            Builtin::GreaterEqual => ">=",
            Builtin::NumericEqual => "=",
            Builtin::List => "list",
            Builtin::Map => "map",
            Builtin::StringToList => "string->list",
            Builtin::ListToString => "list->string",
            Builtin::StringSet => "string-set!",
            Builtin::CharToInteger => "char->integer",
            Builtin::IntegerToChar => "integer->char",
            Builtin::StringCopy => "string-copy",
            Builtin::Display => "display",
            Builtin::Write => "write",
            Builtin::Newline => "newline",
        }
    }
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: HashMap<String, Value>,
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> Self {
        Self {
            parent,
            bindings: HashMap::new(),
        }
    }

    fn child(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self::new(Some(parent))))
    }

    fn define(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        self.bindings.get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().lookup(name))
        })
    }
}

fn global_env() -> EnvRef {
    let env = Rc::new(RefCell::new(Environment::new(None)));
    let builtins = [
        ("+", Builtin::Add),
        ("-", Builtin::Sub),
        ("<", Builtin::Less),
        ("<=", Builtin::LessEqual),
        (">", Builtin::Greater),
        (">=", Builtin::GreaterEqual),
        ("=", Builtin::NumericEqual),
        ("list", Builtin::List),
        ("map", Builtin::Map),
        ("string->list", Builtin::StringToList),
        ("list->string", Builtin::ListToString),
        ("string-set!", Builtin::StringSet),
        ("char->integer", Builtin::CharToInteger),
        ("integer->char", Builtin::IntegerToChar),
        ("string-copy", Builtin::StringCopy),
        ("display", Builtin::Display),
        ("write", Builtin::Write),
        ("newline", Builtin::Newline),
    ];

    {
        let mut scope = env.borrow_mut();
        for (name, builtin) in builtins {
            scope.define(
                name.to_string(),
                Value::Procedure(Procedure::Builtin(builtin)),
            );
        }
    }

    env
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn parse_parameters(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut params = Vec::with_capacity(items.len());
    for item in items {
        match item {
            Expr::Symbol(name) => params.push(name.clone()),
            _ => {
                return Err(EvalError::InvalidForm(
                    "procedure parameters must be symbols".to_string(),
                ));
            }
        }
    }
    Ok(params)
}

fn require_exact_arity(got: usize, expected: usize, name: &str) -> Result<(), EvalError> {
    if got == expected {
        Ok(())
    } else {
        Err(EvalError::WrongArity {
            name: name.to_string(),
            expected: expected.to_string(),
            got,
        })
    }
}

fn require_at_least_arity(got: usize, minimum: usize, name: &str) -> Result<(), EvalError> {
    if got >= minimum {
        Ok(())
    } else {
        Err(EvalError::WrongArity {
            name: name.to_string(),
            expected: format!("at least {minimum}"),
            got,
        })
    }
}

fn expect_int(value: &Value, procedure: &str) -> Result<i64, EvalError> {
    match value {
        Value::Int(value) => Ok(*value),
        _ => Err(EvalError::Type(format!(
            "{procedure} expects an integer, got {}",
            value.type_name()
        ))),
    }
}

fn expect_string<'a>(value: &'a Value, procedure: &str) -> Result<&'a str, EvalError> {
    match value {
        Value::String(value) => Ok(value),
        _ => Err(EvalError::Type(format!(
            "{procedure} expects a string, got {}",
            value.type_name()
        ))),
    }
}

fn expect_char(value: &Value, procedure: &str) -> Result<char, EvalError> {
    match value {
        Value::Char(value) => Ok(*value),
        _ => Err(EvalError::Type(format!(
            "{procedure} expects a character, got {}",
            value.type_name()
        ))),
    }
}

fn expect_list<'a>(value: &'a Value, procedure: &str) -> Result<&'a [Value], EvalError> {
    match value {
        Value::List(items) => Ok(items),
        _ => Err(EvalError::Type(format!(
            "{procedure} expects a list, got {}",
            value.type_name()
        ))),
    }
}

fn compare_numbers(
    args: &[Value],
    name: &str,
    relation: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    require_at_least_arity(args.len(), 2, name)?;
    let mut previous = expect_int(&args[0], name)?;
    for current in &args[1..] {
        let current = expect_int(current, name)?;
        if !relation(previous, current) {
            return Ok(Value::Bool(false));
        }
        previous = current;
    }
    Ok(Value::Bool(true))
}

fn quote_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn char_literal(value: char) -> String {
    match value {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        ch => format!("#\\{ch}"),
    }
}

struct Parser {
    chars: Vec<char>,
    pos: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignorable();
        while self.pos < self.chars.len() {
            expressions.push(self.parse_expr()?);
            self.skip_ignorable();
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignorable();
        match self.peek() {
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string(),
            Some(')') => Err(EvalError::Parse("unexpected ')'".to_string())),
            Some('\'') => Err(EvalError::Parse(
                "quote syntax is not supported in this level".to_string(),
            )),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::Parse("unexpected end of input".to_string())),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.consume('(')?;
        let mut items = Vec::new();
        loop {
            self.skip_ignorable();
            match self.peek() {
                Some(')') => {
                    self.pos += 1;
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => {
                    return Err(EvalError::Parse(
                        "unexpected end of input while reading list".to_string(),
                    ));
                }
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.consume('"')?;
        let mut out = String::new();
        while let Some(ch) = self.next() {
            match ch {
                '"' => return Ok(Expr::String(out)),
                '\\' => {
                    let escaped = self.next().ok_or_else(|| {
                        EvalError::Parse("unterminated string literal".to_string())
                    })?;
                    out.push(match escaped {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                }
                other => out.push(other),
            }
        }

        Err(EvalError::Parse("unterminated string literal".to_string()))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let token = self.read_token();
        if token.is_empty() {
            return Err(EvalError::Parse("expected expression".to_string()));
        }

        if token == "#t" {
            return Ok(Expr::Bool(true));
        }
        if token == "#f" {
            return Ok(Expr::Bool(false));
        }
        if let Some(ch) = parse_char_token(&token)? {
            return Ok(Expr::Char(ch));
        }
        if let Ok(value) = token.parse::<i64>() {
            return Ok(Expr::Int(value));
        }
        Ok(Expr::Symbol(token))
    }

    fn skip_ignorable(&mut self) {
        loop {
            while matches!(self.peek(), Some(ch) if ch.is_whitespace()) {
                self.pos += 1;
            }

            if self.peek() == Some(';') {
                while let Some(ch) = self.next() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn read_token(&mut self) -> String {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.pos += 1;
        }
        self.chars[start..self.pos].iter().collect()
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn next(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += 1;
        Some(ch)
    }

    fn consume(&mut self, expected: char) -> Result<(), EvalError> {
        match self.next() {
            Some(ch) if ch == expected => Ok(()),
            Some(ch) => Err(EvalError::Parse(format!(
                "expected '{expected}', found '{ch}'"
            ))),
            None => Err(EvalError::Parse(format!(
                "expected '{expected}', found end of input"
            ))),
        }
    }
}

fn parse_char_token(token: &str) -> Result<Option<char>, EvalError> {
    if !token.starts_with("#\\") {
        return Ok(None);
    }

    let value = &token[2..];
    let ch = match value {
        "space" => ' ',
        "newline" => '\n',
        _ => {
            let mut chars = value.chars();
            let Some(first) = chars.next() else {
                return Err(EvalError::Parse("invalid character literal".to_string()));
            };
            if chars.next().is_some() {
                return Err(EvalError::Parse(format!(
                    "unsupported character literal: {token}"
                )));
            }
            first
        }
    };

    Ok(Some(ch))
}

#[cfg(test)]
mod tests;
