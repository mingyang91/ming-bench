pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type EnvRef = Rc<Environment>;

#[derive(Clone)]
enum Expr {
    Number(f64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone)]
enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Pair(Rc<Pair>),
    EmptyList,
    Builtin(BuiltinName),
    Closure(Rc<Closure>),
    Void,
}

#[derive(Clone)]
struct Pair {
    car: Value,
    cdr: Value,
}

#[derive(Clone)]
struct Closure {
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone, Copy)]
enum BuiltinName {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    Eq,
    Le,
    Not,
    Cons,
    Car,
    Cdr,
    Null,
    List,
    Length,
    Append,
    StringPred,
    NumberPred,
    BooleanPred,
    PairPred,
    SymbolPred,
}

#[derive(Clone)]
enum Token {
    LParen,
    RParen,
    Atom(String),
    String(String),
    Quote,
}

#[derive(Clone)]
struct LetBinding {
    name: String,
    value_expr: Expr,
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, Value>>,
}

const BUILTINS: [(&str, BuiltinName); 21] = [
    ("+", BuiltinName::Add),
    ("-", BuiltinName::Sub),
    ("*", BuiltinName::Mul),
    ("/", BuiltinName::Div),
    ("<", BuiltinName::Lt),
    (">", BuiltinName::Gt),
    ("=", BuiltinName::Eq),
    ("<=", BuiltinName::Le),
    ("not", BuiltinName::Not),
    ("cons", BuiltinName::Cons),
    ("car", BuiltinName::Car),
    ("cdr", BuiltinName::Cdr),
    ("null?", BuiltinName::Null),
    ("list", BuiltinName::List),
    ("length", BuiltinName::Length),
    ("append", BuiltinName::Append),
    ("string?", BuiltinName::StringPred),
    ("number?", BuiltinName::NumberPred),
    ("boolean?", BuiltinName::BooleanPred),
    ("pair?", BuiltinName::PairPred),
    ("symbol?", BuiltinName::SymbolPred),
];

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.bindings.borrow_mut().insert(name.into(), value);
    }

    fn lookup(&self, name: &str) -> Result<Value, EvalError> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return Ok(value);
        }

        if let Some(parent) = &self.parent {
            return parent.lookup(name);
        }

        Err(EvalError::msg(format!("unbound symbol: {name}")))
    }
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
    let (result, _) = evaluate_program(input)?;
    Ok(format_value(&result))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (result, output) = evaluate_program(input)?;
    Ok((format_value(&result), output))
}

fn evaluate_program(input: &str) -> Result<(Value, String), EvalError> {
    let expressions = parse_program(input)?;
    if expressions.is_empty() {
        return Err(EvalError::msg("expected at least one expression"));
    }

    let env = create_global_env();
    let mut result = Value::Void;

    for expr in &expressions {
        result = evaluate_expr(expr, env.clone())?;
    }

    Ok((result, String::new()))
}

fn create_global_env() -> EnvRef {
    let env = Environment::new(None);

    for (name, builtin) in BUILTINS {
        env.define(name, Value::Builtin(builtin));
    }

    env
}

fn parse_program(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    let mut expressions = Vec::new();
    let mut index = 0;

    while index < tokens.len() {
        let (expr, next_index) = parse_expr(&tokens, index)?;
        expressions.push(expr);
        index = next_index;
    }

    Ok(expressions)
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let chars: Vec<char> = input.chars().collect();
    let mut tokens = Vec::new();
    let mut index = 0;

    while index < chars.len() {
        let ch = chars[index];

        if ch.is_whitespace() {
            index += 1;
            continue;
        }

        if ch == ';' {
            index = skip_comment(&chars, index);
            continue;
        }

        match ch {
            '(' => {
                tokens.push(Token::LParen);
                index += 1;
            }
            ')' => {
                tokens.push(Token::RParen);
                index += 1;
            }
            '\'' => {
                tokens.push(Token::Quote);
                index += 1;
            }
            '"' => {
                let (value, next_index) = parse_string_token(&chars, index)?;
                tokens.push(Token::String(value));
                index = next_index;
            }
            _ => {
                let start = index;
                while index < chars.len() && !is_delimiter(chars[index]) {
                    index += 1;
                }
                let atom: String = chars[start..index].iter().collect();
                tokens.push(Token::Atom(atom));
            }
        }
    }

    Ok(tokens)
}

fn skip_comment(chars: &[char], start: usize) -> usize {
    let mut index = start;
    while index < chars.len() && chars[index] != '\n' {
        index += 1;
    }
    index
}

fn parse_string_token(chars: &[char], start: usize) -> Result<(String, usize), EvalError> {
    let mut index = start + 1;
    let mut value = String::new();

    while index < chars.len() {
        let ch = chars[index];

        if ch == '"' {
            return Ok((value, index + 1));
        }

        if ch == '\\' {
            index += 1;
            if index >= chars.len() {
                return Err(EvalError::msg("unterminated string literal"));
            }

            let escaped = chars[index];
            match escaped {
                '"' => value.push('"'),
                '\\' => value.push('\\'),
                'n' => value.push('\n'),
                'r' => value.push('\r'),
                't' => value.push('\t'),
                other => value.push(other),
            }

            index += 1;
            continue;
        }

        value.push(ch);
        index += 1;
    }

    Err(EvalError::msg("unterminated string literal"))
}

fn parse_expr(tokens: &[Token], index: usize) -> Result<(Expr, usize), EvalError> {
    let token = tokens
        .get(index)
        .ok_or_else(|| EvalError::msg("unexpected end of input"))?;

    match token {
        Token::Quote => {
            let (expr, next_index) = parse_expr(tokens, index + 1)?;
            Ok((
                Expr::List(vec![Expr::Symbol("quote".into()), expr]),
                next_index,
            ))
        }
        Token::LParen => {
            let mut elements = Vec::new();
            let mut next_index = index + 1;

            while next_index < tokens.len() {
                match tokens.get(next_index) {
                    Some(Token::RParen) => return Ok((Expr::List(elements), next_index + 1)),
                    Some(_) => {
                        let (expr, parsed_next) = parse_expr(tokens, next_index)?;
                        elements.push(expr);
                        next_index = parsed_next;
                    }
                    None => break,
                }
            }

            Err(EvalError::msg("unterminated list"))
        }
        Token::RParen => Err(EvalError::msg("unexpected )")),
        Token::String(value) => Ok((Expr::String(value.clone()), index + 1)),
        Token::Atom(text) => Ok((parse_atom(text), index + 1)),
    }
}

fn parse_atom(text: &str) -> Expr {
    match text {
        "#t" => Expr::Boolean(true),
        "#f" => Expr::Boolean(false),
        _ => match text.parse::<i64>() {
            Ok(value) => Expr::Number(value as f64),
            Err(_) => Expr::Symbol(text.to_string()),
        },
    }
}

fn evaluate_expr(expr: &Expr, env: EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Number(value) => Ok(Value::Number(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => env.lookup(name),
        Expr::List(elements) => evaluate_list(elements, env),
    }
}

fn evaluate_list(elements: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if elements.is_empty() {
        return Err(EvalError::msg("cannot evaluate empty list"));
    }

    let (head, arg_exprs) = elements
        .split_first()
        .ok_or_else(|| EvalError::msg("cannot evaluate empty list"))?;

    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "define" => return evaluate_define(arg_exprs, env),
            "if" => return evaluate_if(arg_exprs, env),
            "quote" => return evaluate_quote(arg_exprs),
            "lambda" => return evaluate_lambda(arg_exprs, env),
            "and" => return evaluate_and(arg_exprs, env),
            "or" => return evaluate_or(arg_exprs, env),
            "let" => return evaluate_let(arg_exprs, env),
            "begin" => return evaluate_begin(arg_exprs, env),
            "cond" => return evaluate_cond(arg_exprs, env),
            _ => {}
        }
    }

    let procedure = evaluate_expr(head, env.clone())?;
    let mut args = Vec::with_capacity(arg_exprs.len());
    for expr in arg_exprs {
        args.push(evaluate_expr(expr, env.clone())?);
    }

    apply_procedure(procedure, args)
}

fn evaluate_define(arg_exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if arg_exprs.len() < 2 {
        return Err(EvalError::msg("define expects a target and a value"));
    }

    let target = &arg_exprs[0];
    let body = &arg_exprs[1..];

    match target {
        Expr::Symbol(name) => {
            if body.len() != 1 {
                return Err(EvalError::msg(
                    "define variable form expects exactly 1 value expression",
                ));
            }

            let value = evaluate_expr(&body[0], env.clone())?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(items) if !items.is_empty() => {
            let (name_expr, param_exprs) = items
                .split_first()
                .ok_or_else(|| EvalError::msg("invalid define form"))?;

            let Expr::Symbol(name) = name_expr else {
                return Err(EvalError::msg("define function form expects a function name"));
            };

            let params = read_parameter_list(param_exprs)?;
            let procedure = Value::Closure(Rc::new(Closure {
                params,
                body: body.to_vec(),
                env: env.clone(),
            }));

            env.define(name.clone(), procedure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::msg("invalid define form")),
    }
}

fn evaluate_if(arg_exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if arg_exprs.len() != 3 {
        return Err(EvalError::msg("if expects exactly 3 arguments"));
    }

    let condition = evaluate_expr(&arg_exprs[0], env.clone())?;
    if is_truthy(&condition) {
        evaluate_expr(&arg_exprs[1], env)
    } else {
        evaluate_expr(&arg_exprs[2], env)
    }
}

fn evaluate_quote(arg_exprs: &[Expr]) -> Result<Value, EvalError> {
    if arg_exprs.len() != 1 {
        return Err(EvalError::msg("quote expects exactly 1 argument"));
    }

    Ok(quote_expr(&arg_exprs[0]))
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Number(value) => Value::Number(*value),
        Expr::Boolean(value) => Value::Boolean(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(name) => Value::Symbol(name.clone()),
        Expr::List(elements) => build_list(elements.iter().map(quote_expr).collect()),
    }
}

fn evaluate_lambda(arg_exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if arg_exprs.len() < 2 {
        return Err(EvalError::msg("lambda expects parameters and a body"));
    }

    let params_expr = &arg_exprs[0];
    let body = &arg_exprs[1..];

    let Expr::List(param_exprs) = params_expr else {
        return Err(EvalError::msg("lambda parameters must be a list"));
    };

    Ok(Value::Closure(Rc::new(Closure {
        params: read_parameter_list(param_exprs)?,
        body: body.to_vec(),
        env,
    })))
}

fn read_parameter_list(exprs: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut params = Vec::with_capacity(exprs.len());

    for expr in exprs {
        let Expr::Symbol(name) = expr else {
            return Err(EvalError::msg("parameter list must contain only symbols"));
        };

        params.push(name.clone());
    }

    Ok(params)
}

fn evaluate_and(arg_exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last_value = Value::Boolean(true);

    for expr in arg_exprs {
        last_value = evaluate_expr(expr, env.clone())?;
        if !is_truthy(&last_value) {
            return Ok(last_value);
        }
    }

    Ok(last_value)
}

fn evaluate_or(arg_exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for expr in arg_exprs {
        let value = evaluate_expr(expr, env.clone())?;
        if is_truthy(&value) {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn evaluate_let(arg_exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if arg_exprs.len() < 2 {
        return Err(EvalError::msg("let expects bindings and a body"));
    }

    if matches!(arg_exprs.first(), Some(Expr::Symbol(_))) {
        return evaluate_named_let(arg_exprs, env);
    }

    let bindings = read_let_bindings(&arg_exprs[0])?;
    evaluate_let_body(&bindings, &arg_exprs[1..], env)
}

fn evaluate_named_let(arg_exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if arg_exprs.len() < 3 {
        return Err(EvalError::msg(
            "named let expects a name, bindings, and a body",
        ));
    }

    let Expr::Symbol(name) = &arg_exprs[0] else {
        return Err(EvalError::msg("named let expects a symbol name"));
    };

    let bindings = read_let_bindings(&arg_exprs[1])?;
    let procedure_env = Environment::new(Some(env.clone()));
    let procedure = Rc::new(Closure {
        params: bindings.iter().map(|binding| binding.name.clone()).collect(),
        body: arg_exprs[2..].to_vec(),
        env: procedure_env.clone(),
    });

    procedure_env.define(name.clone(), Value::Closure(procedure.clone()));

    let mut args = Vec::with_capacity(bindings.len());
    for binding in &bindings {
        args.push(evaluate_expr(&binding.value_expr, env.clone())?);
    }

    apply_closure(procedure, args)
}

fn read_let_bindings(bindings_expr: &Expr) -> Result<Vec<LetBinding>, EvalError> {
    let Expr::List(binding_exprs) = bindings_expr else {
        return Err(EvalError::msg("let bindings must be a list"));
    };

    let mut bindings = Vec::with_capacity(binding_exprs.len());

    for binding_expr in binding_exprs {
        let Expr::List(items) = binding_expr else {
            return Err(EvalError::msg("each let binding must have a name and a value"));
        };

        if items.len() != 2 {
            return Err(EvalError::msg("each let binding must have a name and a value"));
        }

        let Expr::Symbol(name) = &items[0] else {
            return Err(EvalError::msg("let binding names must be symbols"));
        };

        bindings.push(LetBinding {
            name: name.clone(),
            value_expr: items[1].clone(),
        });
    }

    Ok(bindings)
}

fn evaluate_let_body(bindings: &[LetBinding], body: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::msg("let expects a body"));
    }

    let mut values = Vec::with_capacity(bindings.len());
    for binding in bindings {
        values.push(evaluate_expr(&binding.value_expr, env.clone())?);
    }

    let let_env = Environment::new(Some(env));
    for (binding, value) in bindings.iter().zip(values) {
        let_env.define(binding.name.clone(), value);
    }

    evaluate_sequence(body, let_env)
}

fn evaluate_begin(arg_exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    evaluate_sequence(arg_exprs, env)
}

fn evaluate_cond(arg_exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for (index, clause_expr) in arg_exprs.iter().enumerate() {
        let Expr::List(items) = clause_expr else {
            return Err(EvalError::msg("cond clauses must be non-empty lists"));
        };

        if items.is_empty() {
            return Err(EvalError::msg("cond clauses must be non-empty lists"));
        }

        let (test_expr, body) = items
            .split_first()
            .ok_or_else(|| EvalError::msg("cond clauses must be non-empty lists"))?;

        let is_else_clause = matches!(test_expr, Expr::Symbol(name) if name == "else");
        if is_else_clause {
            if index + 1 != arg_exprs.len() {
                return Err(EvalError::msg("else clause must be last in cond"));
            }
            if body.is_empty() {
                return Err(EvalError::msg("else clause requires a body"));
            }

            return evaluate_sequence(body, env.clone());
        }

        let test_value = evaluate_expr(test_expr, env.clone())?;
        if is_truthy(&test_value) {
            if body.is_empty() {
                return Ok(test_value);
            }

            return evaluate_sequence(body, env.clone());
        }
    }

    Ok(Value::Void)
}

fn apply_procedure(procedure: Value, args: Vec<Value>) -> Result<Value, EvalError> {
    match procedure {
        Value::Builtin(name) => apply_builtin(name, &args),
        Value::Closure(procedure) => apply_closure(procedure, args),
        _ => Err(EvalError::msg("attempted to call a non-procedure")),
    }
}

fn apply_closure(procedure: Rc<Closure>, args: Vec<Value>) -> Result<Value, EvalError> {
    if args.len() != procedure.params.len() {
        return Err(EvalError::msg(format!(
            "expected {} arguments, got {}",
            procedure.params.len(),
            args.len()
        )));
    }

    let call_env = Environment::new(Some(procedure.env.clone()));
    for (param, value) in procedure.params.iter().zip(args.into_iter()) {
        call_env.define(param.clone(), value);
    }

    evaluate_sequence(&procedure.body, call_env)
}

fn evaluate_sequence(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut result = Value::Void;

    for expr in exprs {
        result = evaluate_expr(expr, env.clone())?;
    }

    Ok(result)
}

fn apply_builtin(name: BuiltinName, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        BuiltinName::Add => {
            let mut sum = 0.0;
            for arg in args {
                sum += expect_number(arg, "+")?;
            }
            Ok(make_number(sum))
        }
        BuiltinName::Sub => apply_subtraction(args),
        BuiltinName::Mul => {
            let mut product = 1.0;
            for arg in args {
                product *= expect_number(arg, "*")?;
            }
            Ok(make_number(product))
        }
        BuiltinName::Div => apply_division(args),
        BuiltinName::Lt => apply_comparison(args, "<", |left, right| left < right),
        BuiltinName::Gt => apply_comparison(args, ">", |left, right| left > right),
        BuiltinName::Eq => apply_comparison(args, "=", |left, right| left == right),
        BuiltinName::Le => apply_comparison(args, "<=", |left, right| left <= right),
        BuiltinName::Not => {
            expect_arg_count(args, 1, "not")?;
            Ok(Value::Boolean(!is_truthy(&args[0])))
        }
        BuiltinName::Cons => {
            expect_arg_count(args, 2, "cons")?;
            Ok(make_pair(args[0].clone(), args[1].clone()))
        }
        BuiltinName::Car => {
            expect_arg_count(args, 1, "car")?;
            Ok(expect_pair(&args[0], "car")?.car.clone())
        }
        BuiltinName::Cdr => {
            expect_arg_count(args, 1, "cdr")?;
            Ok(expect_pair(&args[0], "cdr")?.cdr.clone())
        }
        BuiltinName::Null => {
            expect_arg_count(args, 1, "null?")?;
            Ok(Value::Boolean(matches!(args[0], Value::EmptyList)))
        }
        BuiltinName::List => Ok(build_list(args.to_vec())),
        BuiltinName::Length => {
            expect_arg_count(args, 1, "length")?;
            Ok(make_number(collect_list_elements(&args[0], "length")?.len() as f64))
        }
        BuiltinName::Append => apply_append(args),
        BuiltinName::StringPred => apply_type_predicate(args, "string?", |value| {
            matches!(value, Value::String(_))
        }),
        BuiltinName::NumberPred => apply_type_predicate(args, "number?", |value| {
            matches!(value, Value::Number(_))
        }),
        BuiltinName::BooleanPred => apply_type_predicate(args, "boolean?", |value| {
            matches!(value, Value::Boolean(_))
        }),
        BuiltinName::PairPred => apply_type_predicate(args, "pair?", |value| {
            matches!(value, Value::Pair(_))
        }),
        BuiltinName::SymbolPred => apply_type_predicate(args, "symbol?", |value| {
            matches!(value, Value::Symbol(_))
        }),
    }
}

fn apply_subtraction(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::msg("- expects at least 1 argument"));
    }

    let numbers: Result<Vec<_>, _> = args.iter().map(|arg| expect_number(arg, "-")).collect();
    let numbers = numbers?;

    if numbers.len() == 1 {
        return Ok(make_number(-numbers[0]));
    }

    let first = numbers[0];
    let result = numbers[1..].iter().fold(first, |acc, value| acc - value);
    Ok(make_number(result))
}

fn apply_division(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::msg("/ expects at least 1 argument"));
    }

    let numbers: Result<Vec<_>, _> = args.iter().map(|arg| expect_number(arg, "/")).collect();
    let numbers = numbers?;

    if numbers.len() == 1 {
        if numbers[0] == 0.0 {
            return Err(EvalError::msg("division by zero"));
        }

        return Ok(make_number(1.0 / numbers[0]));
    }

    let mut result = numbers[0];
    for value in &numbers[1..] {
        if *value == 0.0 {
            return Err(EvalError::msg("division by zero"));
        }

        result /= value;
    }

    Ok(make_number(result))
}

fn apply_comparison<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(f64, f64) -> bool,
{
    if args.len() < 2 {
        return Err(EvalError::msg(format!("{name} expects at least 2 arguments")));
    }

    let numbers: Result<Vec<_>, _> = args.iter().map(|arg| expect_number(arg, name)).collect();
    let numbers = numbers?;

    for pair in numbers.windows(2) {
        if !predicate(pair[0], pair[1]) {
            return Ok(Value::Boolean(false));
        }
    }

    Ok(Value::Boolean(true))
}

fn apply_append(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::EmptyList);
    }

    let mut result = args
        .last()
        .cloned()
        .ok_or_else(|| EvalError::msg("append expects lists"))?;

    for value in args[..args.len() - 1].iter().rev() {
        let elements = collect_list_elements(value, "append")?;
        for element in elements.into_iter().rev() {
            result = make_pair(element, result);
        }
    }

    Ok(result)
}

fn apply_type_predicate<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    expect_arg_count(args, 1, name)?;
    Ok(Value::Boolean(predicate(&args[0])))
}

fn expect_arg_count(args: &[Value], expected: usize, name: &str) -> Result<(), EvalError> {
    if args.len() != expected {
        let suffix = if expected == 1 { "" } else { "s" };
        return Err(EvalError::msg(format!(
            "{name} expects exactly {expected} argument{suffix}"
        )));
    }

    Ok(())
}

fn expect_number(value: &Value, procedure: &str) -> Result<f64, EvalError> {
    match value {
        Value::Number(number) => Ok(*number),
        _ => Err(EvalError::msg(format!(
            "{procedure} expects numeric arguments"
        ))),
    }
}

fn expect_pair<'a>(value: &'a Value, procedure: &str) -> Result<&'a Pair, EvalError> {
    match value {
        Value::Pair(pair) => Ok(pair.as_ref()),
        _ => Err(EvalError::msg(format!("{procedure} expects a pair"))),
    }
}

fn collect_list_elements(value: &Value, procedure: &str) -> Result<Vec<Value>, EvalError> {
    let mut elements = Vec::new();
    let mut current = value.clone();

    loop {
        match current {
            Value::Pair(pair) => {
                elements.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            Value::EmptyList => return Ok(elements),
            _ => {
                return Err(EvalError::msg(format!(
                    "{procedure} expects a proper list"
                )))
            }
        }
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Boolean(false))
}

fn make_number(value: f64) -> Value {
    if value == 0.0 {
        Value::Number(0.0)
    } else {
        Value::Number(value)
    }
}

fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(Pair { car, cdr }))
}

fn build_list(elements: Vec<Value>) -> Value {
    let mut result = Value::EmptyList;

    for element in elements.into_iter().rev() {
        result = make_pair(element, result);
    }

    result
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Number(number) => format_number(*number),
        Value::Boolean(value) => {
            if *value {
                "#t".into()
            } else {
                "#f".into()
            }
        }
        Value::String(value) => format!("\"{}\"", escape_string(value)),
        Value::Symbol(name) => name.clone(),
        Value::Pair(pair) => format_pair(pair.clone()),
        Value::EmptyList => "()".into(),
        Value::Builtin(_) | Value::Closure(_) => "#<procedure>".into(),
        Value::Void => String::new(),
    }
}

fn format_pair(pair: Rc<Pair>) -> String {
    let mut elements = Vec::new();
    let mut current = Value::Pair(pair);

    loop {
        match current {
            Value::Pair(pair) => {
                elements.push(format_value(&pair.car));
                current = pair.cdr.clone();
            }
            Value::EmptyList => return format!("({})", elements.join(" ")),
            value => return format!("({} . {})", elements.join(" "), format_value(&value)),
        }
    }
}

fn format_number(value: f64) -> String {
    if value == 0.0 {
        return "0".into();
    }

    if value.fract() == 0.0 {
        return format!("{}", value as i64);
    }

    value.to_string()
}

fn escape_string(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '\'' | ';')
}

#[cfg(test)]
mod tests;
