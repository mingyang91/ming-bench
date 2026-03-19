use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub(super) enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Pair(Box<Value>, Box<Value>),
    Nil,
    Builtin(Builtin),
    Procedure(Procedure),
    Void,
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Integer(value) => Self::Integer(*value),
            Self::Boolean(value) => Self::Boolean(*value),
            Self::String(value) => Self::String(value.clone()),
            Self::Symbol(value) => Self::Symbol(value.clone()),
            Self::Pair(car, cdr) => Self::Pair(car.clone(), cdr.clone()),
            Self::Nil => Self::Nil,
            Self::Builtin(builtin) => Self::Builtin(*builtin),
            Self::Procedure(procedure) => Self::Procedure(procedure.clone()),
            Self::Void => Self::Void,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(value) => write!(formatter, "{value}"),
            Self::Boolean(true) => formatter.write_str("#t"),
            Self::Boolean(false) => formatter.write_str("#f"),
            Self::String(value) => {
                formatter.write_str("\"")?;
                fmt_string_contents(value, formatter)?;
                formatter.write_str("\"")
            }
            Self::Symbol(value) => formatter.write_str(value),
            Self::Pair(car, cdr) => {
                formatter.write_str("(")?;
                fmt_pair(car, cdr, formatter)?;
                formatter.write_str(")")
            }
            Self::Nil => formatter.write_str("()"),
            Self::Builtin(builtin) => write!(formatter, "#<procedure:{}>", builtin.name()),
            Self::Procedure(_) => formatter.write_str("#<procedure>"),
            Self::Void => Ok(()),
        }
    }
}

fn fmt_pair(car: &Value, cdr: &Value, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(formatter, "{car}")?;

    match cdr {
        Value::Nil => Ok(()),
        Value::Pair(next_car, next_cdr) => {
            formatter.write_str(" ")?;
            fmt_pair(next_car, next_cdr, formatter)
        }
        value => write!(formatter, " . {value}"),
    }
}

fn fmt_string_contents(value: &str, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for character in value.chars() {
        match character {
            '"' => formatter.write_str("\\\"")?,
            '\\' => formatter.write_str("\\\\")?,
            '\n' => formatter.write_str("\\n")?,
            '\t' => formatter.write_str("\\t")?,
            _ => write!(formatter, "{character}")?,
        }
    }

    Ok(())
}

#[derive(Clone)]
enum Expr {
    Literal(Value),
    Symbol(String),
    Application(Vec<Expr>),
}

#[derive(Clone, Copy)]
pub(super) enum Builtin {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    Equal,
    LessEqual,
    Not,
    Cons,
    Car,
    Cdr,
    IsNull,
    List,
    Length,
}

impl Builtin {
    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "=",
            Self::LessEqual => "<=",
            Self::Not => "not",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::IsNull => "null?",
            Self::List => "list",
            Self::Length => "length",
        }
    }

    fn apply(self, arguments: &[Value]) -> Result<Value, String> {
        match self {
            Self::Add => eval_add(arguments),
            Self::Subtract => eval_subtract(arguments),
            Self::Multiply => eval_multiply(arguments),
            Self::Divide => eval_divide(arguments),
            Self::LessThan => eval_less_than(arguments),
            Self::GreaterThan => eval_greater_than(arguments),
            Self::Equal => eval_equal(arguments),
            Self::LessEqual => eval_less_equal(arguments),
            Self::Not => eval_not(arguments),
            Self::Cons => eval_cons(arguments),
            Self::Car => eval_car(arguments),
            Self::Cdr => eval_cdr(arguments),
            Self::IsNull => eval_is_null(arguments),
            Self::List => eval_list(arguments),
            Self::Length => eval_length(arguments),
        }
    }
}

#[derive(Clone)]
pub(super) struct Procedure {
    params: Vec<String>,
    body: Vec<Expr>,
    environment: Environment,
}

type Environment = Rc<RefCell<Frame>>;

struct Frame {
    bindings: HashMap<String, Value>,
    parent: Option<Environment>,
}

pub(super) fn eval_program(input: &str) -> Result<Value, String> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program()?;
    let environment = global_environment();
    eval_sequence(&program, &environment)
}

fn global_environment() -> Environment {
    let environment = new_environment(None);

    for builtin in [
        Builtin::Add,
        Builtin::Subtract,
        Builtin::Multiply,
        Builtin::Divide,
        Builtin::LessThan,
        Builtin::GreaterThan,
        Builtin::Equal,
        Builtin::LessEqual,
        Builtin::Not,
        Builtin::Cons,
        Builtin::Car,
        Builtin::Cdr,
        Builtin::IsNull,
        Builtin::List,
        Builtin::Length,
    ] {
        define_binding(
            &environment,
            builtin.name().to_string(),
            Value::Builtin(builtin),
        );
    }

    environment
}

fn new_environment(parent: Option<Environment>) -> Environment {
    Rc::new(RefCell::new(Frame {
        bindings: HashMap::new(),
        parent,
    }))
}

fn define_binding(environment: &Environment, name: String, value: Value) {
    environment.borrow_mut().bindings.insert(name, value);
}

fn lookup_binding(environment: &Environment, name: &str) -> Option<Value> {
    let parent = {
        let frame = environment.borrow();
        if let Some(value) = frame.bindings.get(name) {
            return Some(value.clone());
        }
        frame.parent.clone()
    };

    parent.and_then(|parent| lookup_binding(&parent, name))
}

fn eval_sequence(expressions: &[Expr], environment: &Environment) -> Result<Value, String> {
    let mut last_value = Value::Void;
    for expr in expressions {
        last_value = eval_expr(expr, environment)?;
    }

    Ok(last_value)
}

fn eval_expr(expr: &Expr, environment: &Environment) -> Result<Value, String> {
    match expr {
        Expr::Literal(value) => Ok(value.clone()),
        Expr::Symbol(name) => {
            lookup_binding(environment, name).ok_or_else(|| format!("unbound symbol: {name}"))
        }
        Expr::Application(parts) => eval_application(parts, environment),
    }
}

fn eval_application(parts: &[Expr], environment: &Environment) -> Result<Value, String> {
    let (operator, arguments) = parts
        .split_first()
        .ok_or_else(|| "cannot evaluate empty application".to_string())?;

    if let Some(name) = symbol_name(operator) {
        match name {
            "and" => return eval_and(arguments, environment),
            "or" => return eval_or(arguments, environment),
            "begin" => return eval_begin(arguments, environment),
            "cond" => return eval_cond(arguments, environment),
            "if" => return eval_if(arguments, environment),
            "define" => return eval_define(arguments, environment),
            "let" => return eval_let(arguments, environment),
            "quote" => return eval_quote(arguments),
            "lambda" => return eval_lambda(arguments, environment),
            _ => {}
        }
    }

    let operator = eval_expr(operator, environment)?;
    let arguments = eval_arguments(arguments, environment)?;
    apply_callable(&operator, &arguments)
}

fn symbol_name(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Symbol(name) => Some(name.as_str()),
        _ => None,
    }
}

fn eval_arguments(arguments: &[Expr], environment: &Environment) -> Result<Vec<Value>, String> {
    let mut values = Vec::with_capacity(arguments.len());
    for argument in arguments {
        values.push(eval_expr(argument, environment)?);
    }
    Ok(values)
}

fn apply_callable(callable: &Value, arguments: &[Value]) -> Result<Value, String> {
    match callable {
        Value::Builtin(builtin) => builtin.apply(arguments),
        Value::Procedure(procedure) => apply_procedure(procedure, arguments),
        _ => Err("attempted to call a non-procedure".into()),
    }
}

fn apply_procedure(procedure: &Procedure, arguments: &[Value]) -> Result<Value, String> {
    if procedure.params.len() != arguments.len() {
        return Err(format!(
            "procedure expected {} arguments, got {}",
            procedure.params.len(),
            arguments.len()
        ));
    }

    let call_environment = new_environment(Some(procedure.environment.clone()));
    for (param, argument) in procedure.params.iter().zip(arguments) {
        define_binding(&call_environment, param.clone(), argument.clone());
    }

    eval_sequence(&procedure.body, &call_environment)
}

fn eval_define(arguments: &[Expr], environment: &Environment) -> Result<Value, String> {
    let (target, body) = arguments
        .split_first()
        .ok_or_else(|| "`define` expects at least 2 arguments".to_string())?;

    match target {
        Expr::Symbol(name) => define_value(name, body, environment),
        Expr::Application(signature) => define_function(signature, body, environment),
        _ => Err("`define` expects a symbol name".into()),
    }
}

fn define_value(name: &str, body: &[Expr], environment: &Environment) -> Result<Value, String> {
    let [value_expr] = body else {
        return Err("`define` expects exactly 2 arguments".into());
    };

    let value = eval_expr(value_expr, environment)?;
    define_binding(environment, name.to_string(), value);
    Ok(Value::Void)
}

fn define_function(
    signature: &[Expr],
    body: &[Expr],
    environment: &Environment,
) -> Result<Value, String> {
    let (name, params) = signature
        .split_first()
        .ok_or_else(|| "`define` expects a function name".to_string())?;
    let Expr::Symbol(name) = name else {
        return Err("`define` expects a symbol name".into());
    };

    if body.is_empty() {
        return Err("`define` expects a function body".into());
    }

    let params = parse_parameters(params)?;
    let procedure = Procedure {
        params,
        body: body.to_vec(),
        environment: environment.clone(),
    };
    define_binding(environment, name.clone(), Value::Procedure(procedure));

    Ok(Value::Void)
}

fn eval_lambda(arguments: &[Expr], environment: &Environment) -> Result<Value, String> {
    let (params, body) = arguments
        .split_first()
        .ok_or_else(|| "`lambda` expects a parameter list and body".to_string())?;

    if body.is_empty() {
        return Err("`lambda` expects a body".into());
    }

    let Expr::Application(params) = params else {
        return Err("`lambda` expects a parameter list".into());
    };
    let params = parse_parameters(params)?;

    Ok(Value::Procedure(Procedure {
        params,
        body: body.to_vec(),
        environment: environment.clone(),
    }))
}

fn eval_begin(arguments: &[Expr], environment: &Environment) -> Result<Value, String> {
    eval_sequence(arguments, environment)
}

fn eval_let(arguments: &[Expr], environment: &Environment) -> Result<Value, String> {
    let (bindings, body) = arguments
        .split_first()
        .ok_or_else(|| "`let` expects bindings and a body".to_string())?;

    if body.is_empty() {
        return Err("`let` expects a body".into());
    }

    let Expr::Application(bindings) = bindings else {
        return Err("`let` expects a binding list".into());
    };

    let mut evaluated_bindings = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let (name, value) = eval_let_binding(binding, environment)?;
        evaluated_bindings.push((name, value));
    }

    let let_environment = new_environment(Some(environment.clone()));
    for (name, value) in evaluated_bindings {
        define_binding(&let_environment, name, value);
    }

    eval_sequence(body, &let_environment)
}

fn eval_let_binding(binding: &Expr, environment: &Environment) -> Result<(String, Value), String> {
    let Expr::Application(binding) = binding else {
        return Err("`let` bindings must be pairs".into());
    };

    let [name, value_expr] = binding.as_slice() else {
        return Err("`let` bindings must contain exactly 2 forms".into());
    };

    let Expr::Symbol(name) = name else {
        return Err("`let` binding names must be symbols".into());
    };

    let value = eval_expr(value_expr, environment)?;
    Ok((name.clone(), value))
}

fn parse_parameters(parameters: &[Expr]) -> Result<Vec<String>, String> {
    let mut names = Vec::with_capacity(parameters.len());

    for parameter in parameters {
        let Expr::Symbol(name) = parameter else {
            return Err("parameter names must be symbols".into());
        };
        names.push(name.clone());
    }

    Ok(names)
}

fn eval_if(arguments: &[Expr], environment: &Environment) -> Result<Value, String> {
    let [condition, consequent, alternative] = arguments else {
        return Err("`if` expects exactly 3 arguments".into());
    };

    let condition = eval_expr(condition, environment)?;
    if is_truthy(&condition) {
        eval_expr(consequent, environment)
    } else {
        eval_expr(alternative, environment)
    }
}

fn eval_cond(arguments: &[Expr], environment: &Environment) -> Result<Value, String> {
    for (index, clause) in arguments.iter().enumerate() {
        if let Some(value) = eval_cond_clause(clause, index + 1 == arguments.len(), environment)? {
            return Ok(value);
        }
    }

    Ok(Value::Void)
}

fn eval_cond_clause(
    clause: &Expr,
    is_last: bool,
    environment: &Environment,
) -> Result<Option<Value>, String> {
    let Expr::Application(clause_parts) = clause else {
        return Err("`cond` clauses must be lists".into());
    };

    let (test, body) = clause_parts
        .split_first()
        .ok_or_else(|| "`cond` clauses must not be empty".to_string())?;

    if matches!(test, Expr::Symbol(name) if name == "else") {
        return eval_else_clause(body, is_last, environment).map(Some);
    }

    let test_value = eval_expr(test, environment)?;
    if !is_truthy(&test_value) {
        return Ok(None);
    }

    let value = if body.is_empty() {
        test_value
    } else {
        eval_sequence(body, environment)?
    };
    Ok(Some(value))
}

fn eval_else_clause(
    body: &[Expr],
    is_last: bool,
    environment: &Environment,
) -> Result<Value, String> {
    if !is_last {
        return Err("`cond` `else` clause must be last".into());
    }
    if body.is_empty() {
        return Err("`cond` `else` clause must have a body".into());
    }

    eval_sequence(body, environment)
}

fn eval_quote(arguments: &[Expr]) -> Result<Value, String> {
    let [value] = arguments else {
        return Err("`quote` expects exactly 1 argument".into());
    };

    quote_expr(value)
}

fn quote_expr(expr: &Expr) -> Result<Value, String> {
    match expr {
        Expr::Literal(value) => Ok(value.clone()),
        Expr::Symbol(value) => Ok(Value::Symbol(value.clone())),
        Expr::Application(values) => {
            let mut list = Value::Nil;
            for value in values.iter().rev() {
                list = Value::Pair(Box::new(quote_expr(value)?), Box::new(list));
            }
            Ok(list)
        }
    }
}

fn eval_add(arguments: &[Value]) -> Result<Value, String> {
    arguments
        .iter()
        .try_fold(0_i64, |total, value| {
            let value = expect_integer(value, "+")?;
            total
                .checked_add(value)
                .ok_or_else(|| "integer overflow".to_string())
        })
        .map(Value::Integer)
}

fn eval_subtract(arguments: &[Value]) -> Result<Value, String> {
    let (first, rest) = arguments
        .split_first()
        .ok_or_else(|| "`-` expects at least 1 argument".to_string())?;
    let first = expect_integer(first, "-")?;

    if rest.is_empty() {
        return first
            .checked_neg()
            .map(Value::Integer)
            .ok_or_else(|| "integer overflow".to_string());
    }

    rest.iter()
        .try_fold(first, |total, value| {
            let value = expect_integer(value, "-")?;
            total
                .checked_sub(value)
                .ok_or_else(|| "integer overflow".to_string())
        })
        .map(Value::Integer)
}

fn eval_multiply(arguments: &[Value]) -> Result<Value, String> {
    arguments
        .iter()
        .try_fold(1_i64, |total, value| {
            let value = expect_integer(value, "*")?;
            total
                .checked_mul(value)
                .ok_or_else(|| "integer overflow".to_string())
        })
        .map(Value::Integer)
}

fn eval_divide(arguments: &[Value]) -> Result<Value, String> {
    let (first, rest) = arguments
        .split_first()
        .ok_or_else(|| "`/` expects at least 2 arguments".to_string())?;
    if rest.is_empty() {
        return Err("`/` expects at least 2 arguments".into());
    }

    let first = expect_integer(first, "/")?;
    rest.iter()
        .try_fold(first, |total, value| {
            let value = expect_integer(value, "/")?;
            if value == 0 {
                return Err("division by zero".into());
            }
            total
                .checked_div(value)
                .ok_or_else(|| "integer overflow".to_string())
        })
        .map(Value::Integer)
}

fn eval_less_than(arguments: &[Value]) -> Result<Value, String> {
    eval_numeric_comparison(arguments, "<", |left, right| left < right)
}

fn eval_greater_than(arguments: &[Value]) -> Result<Value, String> {
    eval_numeric_comparison(arguments, ">", |left, right| left > right)
}

fn eval_equal(arguments: &[Value]) -> Result<Value, String> {
    eval_numeric_comparison(arguments, "=", |left, right| left == right)
}

fn eval_less_equal(arguments: &[Value]) -> Result<Value, String> {
    eval_numeric_comparison(arguments, "<=", |left, right| left <= right)
}

fn eval_numeric_comparison<F>(
    arguments: &[Value],
    operator: &str,
    compare: F,
) -> Result<Value, String>
where
    F: Fn(i64, i64) -> bool,
{
    let mut numbers = arguments.iter();
    let first = numbers
        .next()
        .ok_or_else(|| format!("`{operator}` expects at least 2 arguments"))?;
    let mut previous = expect_integer(first, operator)?;
    let mut saw_pair = false;

    for value in numbers {
        saw_pair = true;
        let current = expect_integer(value, operator)?;
        if !compare(previous, current) {
            return Ok(Value::Boolean(false));
        }
        previous = current;
    }

    if !saw_pair {
        return Err(format!("`{operator}` expects at least 2 arguments"));
    }

    Ok(Value::Boolean(true))
}

fn eval_not(arguments: &[Value]) -> Result<Value, String> {
    let [value] = arguments else {
        return Err("`not` expects exactly 1 argument".into());
    };

    Ok(Value::Boolean(!is_truthy(value)))
}

fn eval_cons(arguments: &[Value]) -> Result<Value, String> {
    let [car, cdr] = arguments else {
        return Err("`cons` expects exactly 2 arguments".into());
    };

    Ok(Value::Pair(Box::new(car.clone()), Box::new(cdr.clone())))
}

fn eval_car(arguments: &[Value]) -> Result<Value, String> {
    let [pair] = arguments else {
        return Err("`car` expects exactly 1 argument".into());
    };

    let Value::Pair(car, _) = pair else {
        return Err("`car` expects a pair".into());
    };

    Ok((**car).clone())
}

fn eval_cdr(arguments: &[Value]) -> Result<Value, String> {
    let [pair] = arguments else {
        return Err("`cdr` expects exactly 1 argument".into());
    };

    let Value::Pair(_, cdr) = pair else {
        return Err("`cdr` expects a pair".into());
    };

    Ok((**cdr).clone())
}

fn eval_is_null(arguments: &[Value]) -> Result<Value, String> {
    let [value] = arguments else {
        return Err("`null?` expects exactly 1 argument".into());
    };

    Ok(Value::Boolean(matches!(value, Value::Nil)))
}

fn eval_list(arguments: &[Value]) -> Result<Value, String> {
    Ok(build_list(arguments))
}

fn eval_length(arguments: &[Value]) -> Result<Value, String> {
    let [list] = arguments else {
        return Err("`length` expects exactly 1 argument".into());
    };

    list_length(list).map(Value::Integer)
}

fn build_list(values: &[Value]) -> Value {
    let mut list = Value::Nil;
    for value in values.iter().rev() {
        list = Value::Pair(Box::new(value.clone()), Box::new(list));
    }
    list
}

fn list_length(list: &Value) -> Result<i64, String> {
    let mut length = 0_i64;
    let mut current = list;

    loop {
        match current {
            Value::Nil => return Ok(length),
            Value::Pair(_, cdr) => {
                length = length
                    .checked_add(1)
                    .ok_or_else(|| "integer overflow".to_string())?;
                current = cdr.as_ref();
            }
            _ => return Err("`length` expects a proper list".into()),
        }
    }
}

fn eval_and(arguments: &[Expr], environment: &Environment) -> Result<Value, String> {
    let mut last_value = Value::Boolean(true);

    for argument in arguments {
        let value = eval_expr(argument, environment)?;
        if !is_truthy(&value) {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_or(arguments: &[Expr], environment: &Environment) -> Result<Value, String> {
    for argument in arguments {
        let value = eval_expr(argument, environment)?;
        if is_truthy(&value) {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn expect_integer(value: &Value, operator: &str) -> Result<i64, String> {
    match value {
        Value::Integer(number) => Ok(*number),
        _ => Err(format!("`{operator}` expects integer arguments")),
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Boolean(false))
}

struct Parser<'a> {
    input: &'a str,
    cursor: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, cursor: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, String> {
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if expressions.is_empty() {
            return Err("empty program".into());
        }

        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        match self.peek_char() {
            Some('(') => self.parse_application(),
            Some(')') => Err("unexpected `)`".into()),
            Some('\'') => self.parse_quote(),
            Some('"') => self.parse_string().map(Expr::Literal),
            Some('#') => self.parse_boolean().map(Expr::Literal),
            Some('-') if self.peek_next_is_digit() => self.parse_integer().map(Expr::Literal),
            Some('0'..='9') => self.parse_integer().map(Expr::Literal),
            Some(_) => self.parse_symbol().map(Expr::Symbol),
            None => Err("unexpected end of input".into()),
        }
    }

    fn parse_quote(&mut self) -> Result<Expr, String> {
        self.bump_char();
        self.skip_ignored();

        Ok(Expr::Application(vec![
            Expr::Symbol("quote".to_string()),
            self.parse_expr()?,
        ]))
    }

    fn parse_application(&mut self) -> Result<Expr, String> {
        self.bump_char();
        self.skip_ignored();

        let mut expressions = Vec::new();
        while matches!(self.peek_char(), Some(character) if character != ')') {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if self.peek_char().is_none() {
            return Err("unterminated list".into());
        }

        self.bump_char();
        Ok(Expr::Application(expressions))
    }

    fn parse_symbol(&mut self) -> Result<String, String> {
        let start = self.cursor;
        while matches!(self.peek_char(), Some(character) if !Self::is_delimiter(character)) {
            self.bump_char();
        }

        if start == self.cursor {
            return Err("invalid symbol".into());
        }

        Ok(self.input[start..self.cursor].to_string())
    }

    fn parse_boolean(&mut self) -> Result<Value, String> {
        if self.remaining().starts_with("#t") {
            self.cursor += 2;
            self.ensure_token_boundary("boolean")?;
            return Ok(Value::Boolean(true));
        }

        if self.remaining().starts_with("#f") {
            self.cursor += 2;
            self.ensure_token_boundary("boolean")?;
            return Ok(Value::Boolean(false));
        }

        Err("invalid boolean literal".into())
    }

    fn parse_integer(&mut self) -> Result<Value, String> {
        let start = self.cursor;

        if self.peek_char() == Some('-') {
            self.bump_char();
        }

        let digit_start = self.cursor;
        while matches!(self.peek_char(), Some('0'..='9')) {
            self.bump_char();
        }

        if digit_start == self.cursor {
            return Err("invalid integer literal".into());
        }

        let literal = &self.input[start..self.cursor];
        self.ensure_token_boundary("integer")?;

        literal
            .parse::<i64>()
            .map(Value::Integer)
            .map_err(|_| format!("integer literal out of range: {literal}"))
    }

    fn parse_string(&mut self) -> Result<Value, String> {
        self.bump_char();

        let mut contents = String::new();
        loop {
            match self.bump_char() {
                Some('"') => return Ok(Value::String(contents)),
                Some('\\') => contents.push(self.parse_escape_sequence()?),
                Some(character) => contents.push(character),
                None => return Err("unterminated string literal".into()),
            }
        }
    }

    fn parse_escape_sequence(&mut self) -> Result<char, String> {
        match self.bump_char() {
            Some('"') => Ok('"'),
            Some('\\') => Ok('\\'),
            Some('n') => Ok('\n'),
            Some('t') => Ok('\t'),
            Some(character) => Err(format!("unsupported string escape: \\{character}")),
            None => Err("unterminated string literal".into()),
        }
    }

    fn ensure_token_boundary(&self, kind: &str) -> Result<(), String> {
        match self.peek_char() {
            None => Ok(()),
            Some(character) if Self::is_delimiter(character) => Ok(()),
            Some(character) => Err(format!("unexpected `{character}` after {kind} literal")),
        }
    }

    fn skip_ignored(&mut self) {
        while self.skip_whitespace() || self.skip_line_comment() {}
    }

    fn skip_whitespace(&mut self) -> bool {
        let start = self.cursor;

        while matches!(self.peek_char(), Some(character) if character.is_whitespace()) {
            self.bump_char();
        }

        self.cursor != start
    }

    fn skip_line_comment(&mut self) -> bool {
        if self.peek_char() != Some(';') {
            return false;
        }

        while !matches!(self.peek_char(), None | Some('\n')) {
            self.bump_char();
        }

        true
    }

    fn remaining(&self) -> &'a str {
        &self.input[self.cursor..]
    }

    fn peek_char(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    fn peek_next_is_digit(&self) -> bool {
        self.remaining()
            .chars()
            .nth(1)
            .is_some_and(|character| character.is_ascii_digit())
    }

    fn bump_char(&mut self) -> Option<char> {
        let character = self.peek_char()?;
        self.cursor += character.len_utf8();
        Some(character)
    }

    fn is_eof(&self) -> bool {
        self.cursor == self.input.len()
    }

    fn is_delimiter(character: char) -> bool {
        character.is_whitespace() || matches!(character, '(' | ')' | ';')
    }
}
