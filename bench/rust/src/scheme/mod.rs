pub mod error;

pub use error::EvalError;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let expressions = Parser::new(input).parse_program()?;
    if expressions.is_empty() {
        return Err(EvalError::message("empty input"));
    }

    let env = Env::new(None);
    let result = eval_sequence(&expressions, env)?;
    Ok(render(&result))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

#[cfg(test)]
mod tests;

#[derive(Clone)]
enum Expr {
    Int(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone, Copy)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    NumericEq,
    LessEqual,
    Not,
    Cons,
    Car,
    Cdr,
    NullPred,
    List,
    Length,
    Append,
    StringPred,
    NumberPred,
    BooleanPred,
    PairPred,
    SymbolPred,
}

impl Builtin {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "+" => Some(Self::Add),
            "-" => Some(Self::Sub),
            "*" => Some(Self::Mul),
            "/" => Some(Self::Div),
            "<" => Some(Self::LessThan),
            ">" => Some(Self::GreaterThan),
            "=" => Some(Self::NumericEq),
            "<=" => Some(Self::LessEqual),
            "not" => Some(Self::Not),
            "cons" => Some(Self::Cons),
            "car" => Some(Self::Car),
            "cdr" => Some(Self::Cdr),
            "null?" => Some(Self::NullPred),
            "list" => Some(Self::List),
            "length" => Some(Self::Length),
            "append" => Some(Self::Append),
            "string?" => Some(Self::StringPred),
            "number?" => Some(Self::NumberPred),
            "boolean?" => Some(Self::BooleanPred),
            "pair?" => Some(Self::PairPred),
            "symbol?" => Some(Self::SymbolPred),
            _ => None,
        }
    }

    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::NumericEq => "=",
            Self::LessEqual => "<=",
            Self::Not => "not",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::NullPred => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::Append => "append",
            Self::StringPred => "string?",
            Self::NumberPred => "number?",
            Self::BooleanPred => "boolean?",
            Self::PairPred => "pair?",
            Self::SymbolPred => "symbol?",
        }
    }
}

#[derive(Clone)]
enum Value {
    Int(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    EmptyList,
    Pair(Rc<Pair>),
    Builtin(Builtin),
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
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

type EnvRef = Rc<Env>;

struct Env {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, Value>>,
}

impl Env {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.bindings.borrow_mut().insert(name.into(), value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name) {
            return Some(value.clone());
        }

        if let Some(parent) = &self.parent {
            if let Some(value) = parent.lookup(name) {
                return Some(value);
            }
        }

        Builtin::from_name(name).map(Value::Builtin)
    }
}

fn eval(expr: &Expr, env: EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Int(value) => Ok(Value::Int(*value)),
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => env
            .lookup(name)
            .ok_or_else(|| EvalError::message(format!("unbound variable: {name}"))),
        Expr::List(items) => eval_list(items, env),
    }
}

fn eval_list(items: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::message("cannot evaluate empty list"));
    };

    match head {
        Expr::Symbol(symbol) => match symbol.as_str() {
            "define" => eval_define(tail, env),
            "if" => eval_if(tail, env),
            "quote" => eval_quote(tail),
            "lambda" => eval_lambda(tail, env),
            "and" => eval_and(tail, env),
            "or" => eval_or(tail, env),
            "let" => eval_let(tail, env),
            "begin" => eval_sequence(tail, env),
            "cond" => eval_cond(tail, env),
            _ => {
                let procedure = eval(head, env.clone())?;
                apply(procedure, tail, env)
            }
        },
        _ => {
            let procedure = eval(head, env.clone())?;
            apply(procedure, tail, env)
        }
    }
}

fn eval_define(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name), value_expr] => {
            let value = eval(value_expr, env.clone())?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List(signature), body @ ..] if !body.is_empty() => {
            let Some((Expr::Symbol(name), params)) = signature.split_first() else {
                return Err(EvalError::message("invalid define"));
            };

            let closure = Value::Closure(Rc::new(Closure {
                name: Some(name.clone()),
                params: parse_param_names(params)?,
                body: body.to_vec(),
                env: env.clone(),
            }));
            env.define(name.clone(), closure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::message("invalid define")),
    }
}

fn eval_if(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match args {
        [condition, then_branch, else_branch] => {
            if is_truthy(&eval(condition, env.clone())?) {
                eval(then_branch, env)
            } else {
                eval(else_branch, env)
            }
        }
        _ => Err(EvalError::message("if expects exactly 3 arguments")),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    match args {
        [expr] => Ok(quote(expr)),
        _ => Err(EvalError::message("quote expects exactly 1 argument")),
    }
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match args {
        [params_expr, body @ ..] if !body.is_empty() => Ok(Value::Closure(Rc::new(Closure {
            name: None,
            params: parse_params_expr(params_expr)?,
            body: body.to_vec(),
            env,
        }))),
        _ => Err(EvalError::message(
            "lambda expects parameters and at least one body expression",
        )),
    }
}

fn eval_and(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last_value = Value::Bool(true);
    for expr in args {
        let value = eval(expr, env.clone())?;
        if !is_truthy(&value) {
            return Ok(value);
        }
        last_value = value;
    }
    Ok(last_value)
}

fn eval_or(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval(expr, env.clone())?;
        if is_truthy(&value) {
            return Ok(value);
        }
    }
    Ok(Value::Bool(false))
}

fn eval_let(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name), bindings_expr, body @ ..] if !body.is_empty() => {
            let bindings = parse_bindings(bindings_expr)?;
            let params = bindings
                .iter()
                .map(|(param, _)| param.clone())
                .collect::<Vec<_>>();
            let values = eval_binding_values(&bindings, env.clone())?;
            let closure = Rc::new(Closure {
                name: Some(name.clone()),
                params,
                body: body.to_vec(),
                env,
            });
            call_closure(closure, values)
        }
        [bindings_expr, body @ ..] if !body.is_empty() => {
            let bindings = parse_bindings(bindings_expr)?;
            let values = eval_binding_values(&bindings, env.clone())?;
            let let_env = Env::new(Some(env));
            for ((name, _), value) in bindings.into_iter().zip(values.into_iter()) {
                let_env.define(name, value);
            }
            eval_sequence(body, let_env)
        }
        _ => Err(EvalError::message(
            "let expects bindings and at least one body expression",
        )),
    }
}

fn eval_cond(clauses: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::message("cond clauses must be lists"));
        };

        let Some((test_expr, body)) = items.split_first() else {
            return Err(EvalError::message("cond clauses cannot be empty"));
        };

        if matches!(test_expr, Expr::Symbol(symbol) if symbol == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::message("cond else clause must be last"));
            }
            if body.is_empty() {
                return Err(EvalError::message("cond else clause requires a body"));
            }
            return eval_sequence(body, env.clone());
        }

        let test_value = eval(test_expr, env.clone())?;
        if is_truthy(&test_value) {
            if body.is_empty() {
                return Ok(test_value);
            }
            return eval_sequence(body, env.clone());
        }
    }

    Ok(Value::Void)
}

fn apply(procedure: Value, args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let evaluated_args = args
        .iter()
        .map(|expr| eval(expr, env.clone()))
        .collect::<Result<Vec<_>, _>>()?;

    match procedure {
        Value::Builtin(builtin) => apply_builtin(builtin, &evaluated_args),
        Value::Closure(closure) => call_closure(closure, evaluated_args),
        _ => Err(EvalError::message("attempted to call a non-procedure")),
    }
}

fn call_closure(closure: Rc<Closure>, args: Vec<Value>) -> Result<Value, EvalError> {
    if args.len() != closure.params.len() {
        let proc_name = closure.name.as_deref().unwrap_or("lambda");
        return Err(EvalError::message(format!(
            "{proc_name} expects {} argument(s), got {}",
            closure.params.len(),
            args.len()
        )));
    }

    let call_env = Env::new(Some(closure.env.clone()));
    if let Some(name) = &closure.name {
        call_env.define(name.clone(), Value::Closure(closure.clone()));
    }

    for (param, value) in closure.params.iter().zip(args.into_iter()) {
        call_env.define(param.clone(), value);
    }

    eval_sequence(&closure.body, call_env)
}

fn apply_builtin(builtin: Builtin, args: &[Value]) -> Result<Value, EvalError> {
    match builtin {
        Builtin::Add => Ok(Value::Int(
            collect_numbers(builtin.name(), args)?.into_iter().sum(),
        )),
        Builtin::Sub => {
            let numbers = collect_numbers(builtin.name(), args)?;
            let numbers = require_min_args(builtin.name(), &numbers, 1)?;
            let result = if numbers.len() == 1 {
                -numbers[0]
            } else {
                numbers[1..]
                    .iter()
                    .fold(numbers[0], |acc, value| acc - *value)
            };
            Ok(Value::Int(result))
        }
        Builtin::Mul => Ok(Value::Int(
            collect_numbers(builtin.name(), args)?.into_iter().product(),
        )),
        Builtin::Div => {
            let numbers = collect_numbers(builtin.name(), args)?;
            let numbers = require_min_args(builtin.name(), &numbers, 2)?;
            let mut result = numbers[0];
            for divisor in &numbers[1..] {
                if *divisor == 0 {
                    return Err(EvalError::message("division by zero"));
                }
                result /= *divisor;
            }
            Ok(Value::Int(result))
        }
        Builtin::LessThan => compare_numbers(builtin.name(), args, |left, right| left < right),
        Builtin::GreaterThan => compare_numbers(builtin.name(), args, |left, right| left > right),
        Builtin::NumericEq => compare_numbers(builtin.name(), args, |left, right| left == right),
        Builtin::LessEqual => compare_numbers(builtin.name(), args, |left, right| left <= right),
        Builtin::Not => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(!is_truthy(value)))
        }
        Builtin::Cons => {
            let [car, cdr] = require_exact_args(builtin.name(), args, 2)? else {
                unreachable!();
            };
            Ok(Value::Pair(Rc::new(Pair {
                car: car.clone(),
                cdr: cdr.clone(),
            })))
        }
        Builtin::Car => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(expect_pair(builtin.name(), value)?.car.clone())
        }
        Builtin::Cdr => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(expect_pair(builtin.name(), value)?.cdr.clone())
        }
        Builtin::NullPred => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(matches!(value, Value::EmptyList)))
        }
        Builtin::List => Ok(make_proper_list(args.iter().cloned())),
        Builtin::Length => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Int(proper_list_length(builtin.name(), value)? as i64))
        }
        Builtin::Append => append_lists(args),
        Builtin::StringPred => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(matches!(value, Value::String(_))))
        }
        Builtin::NumberPred => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(matches!(value, Value::Int(_))))
        }
        Builtin::BooleanPred => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(matches!(value, Value::Bool(_))))
        }
        Builtin::PairPred => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(matches!(value, Value::Pair(_))))
        }
        Builtin::SymbolPred => {
            let [value] = require_exact_args(builtin.name(), args, 1)? else {
                unreachable!();
            };
            Ok(Value::Bool(matches!(value, Value::Symbol(_))))
        }
    }
}

fn eval_sequence(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in exprs {
        result = eval(expr, env.clone())?;
    }
    Ok(result)
}

fn compare_numbers(
    name: &str,
    args: &[Value],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = collect_numbers(name, args)?;
    let numbers = require_min_args(name, &numbers, 2)?;
    Ok(Value::Bool(
        numbers.windows(2).all(|pair| predicate(pair[0], pair[1])),
    ))
}

fn collect_numbers(name: &str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Int(number) => Ok(*number),
            other => Err(type_error(name, "number", other)),
        })
        .collect()
}

fn require_exact_args<'a>(
    name: &str,
    args: &'a [Value],
    expected: usize,
) -> Result<&'a [Value], EvalError> {
    if args.len() == expected {
        Ok(args)
    } else {
        Err(EvalError::message(format!(
            "{name} expects {expected} argument(s), got {}",
            args.len()
        )))
    }
}

fn require_min_args<'a, T>(name: &str, args: &'a [T], min: usize) -> Result<&'a [T], EvalError> {
    if args.len() >= min {
        Ok(args)
    } else {
        Err(EvalError::message(format!(
            "{name} expects at least {min} argument(s), got {}",
            args.len()
        )))
    }
}

fn expect_pair(name: &str, value: &Value) -> Result<Rc<Pair>, EvalError> {
    match value {
        Value::Pair(pair) => Ok(pair.clone()),
        other => Err(type_error(name, "pair", other)),
    }
}

fn proper_list_length(name: &str, value: &Value) -> Result<usize, EvalError> {
    let mut current = value.clone();
    let mut count = 0usize;

    loop {
        match current {
            Value::EmptyList => return Ok(count),
            Value::Pair(pair) => {
                count += 1;
                current = pair.cdr.clone();
            }
            other => return Err(type_error(name, "list", &other)),
        }
    }
}

fn append_lists(args: &[Value]) -> Result<Value, EvalError> {
    let Some(last) = args.last() else {
        return Ok(Value::EmptyList);
    };

    let mut result = last.clone();
    for list in args[..args.len() - 1].iter().rev() {
        result = append_front(list, result)?;
    }
    Ok(result)
}

fn append_front(list: &Value, tail: Value) -> Result<Value, EvalError> {
    match list {
        Value::EmptyList => Ok(tail),
        Value::Pair(pair) => Ok(Value::Pair(Rc::new(Pair {
            car: pair.car.clone(),
            cdr: append_front(&pair.cdr, tail)?,
        }))),
        other => Err(type_error("append", "list", other)),
    }
}

fn parse_params_expr(expr: &Expr) -> Result<Vec<String>, EvalError> {
    let Expr::List(params) = expr else {
        return Err(EvalError::message("parameter list must be a list"));
    };
    parse_param_names(params)
}

fn parse_param_names(params: &[Expr]) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|expr| match expr {
            Expr::Symbol(name) => Ok(name.clone()),
            _ => Err(EvalError::message("parameter names must be symbols")),
        })
        .collect()
}

fn parse_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings) = expr else {
        return Err(EvalError::message("let bindings must be a list"));
    };

    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List(items) => match items.as_slice() {
                [Expr::Symbol(name), value_expr] => Ok((name.clone(), value_expr.clone())),
                _ => Err(EvalError::message("invalid let binding")),
            },
            _ => Err(EvalError::message("invalid let binding")),
        })
        .collect()
}

fn eval_binding_values(bindings: &[(String, Expr)], env: EnvRef) -> Result<Vec<Value>, EvalError> {
    bindings
        .iter()
        .map(|(_, expr)| eval(expr, env.clone()))
        .collect()
}

fn quote(expr: &Expr) -> Value {
    match expr {
        Expr::Int(value) => Value::Int(*value),
        Expr::Bool(value) => Value::Bool(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(name) => Value::Symbol(name.clone()),
        Expr::List(items) => make_proper_list(items.iter().map(quote)),
    }
}

fn make_proper_list(items: impl IntoIterator<Item = Value>) -> Value {
    let mut values = items.into_iter().collect::<Vec<_>>();
    let mut result = Value::EmptyList;
    while let Some(value) = values.pop() {
        result = Value::Pair(Rc::new(Pair {
            car: value,
            cdr: result,
        }));
    }
    result
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn type_error(name: &str, expected: &str, value: &Value) -> EvalError {
    EvalError::message(format!(
        "{name} expected {expected}, got {}",
        value_type_name(value)
    ))
}

fn value_type_name(value: &Value) -> &'static str {
    match value {
        Value::Int(_) => "number",
        Value::Bool(_) => "boolean",
        Value::String(_) => "string",
        Value::Symbol(_) => "symbol",
        Value::EmptyList => "null",
        Value::Pair(_) => "pair",
        Value::Builtin(_) | Value::Closure(_) => "procedure",
        Value::Void => "void",
    }
}

fn render(value: &Value) -> String {
    match value {
        Value::Int(number) => number.to_string(),
        Value::Bool(true) => "#t".into(),
        Value::Bool(false) => "#f".into(),
        Value::String(text) => format!("\"{}\"", escape_string(text)),
        Value::Symbol(name) => name.clone(),
        Value::EmptyList => "()".into(),
        Value::Pair(_) => render_pair(value),
        Value::Builtin(_) | Value::Closure(_) => "#<procedure>".into(),
        Value::Void => String::new(),
    }
}

fn render_pair(value: &Value) -> String {
    let mut output = String::from("(");
    let mut current = value.clone();
    let mut first = true;

    loop {
        match current {
            Value::Pair(pair) => {
                if !first {
                    output.push(' ');
                }
                output.push_str(&render(&pair.car));

                match &pair.cdr {
                    Value::EmptyList => {
                        output.push(')');
                        return output;
                    }
                    Value::Pair(_) => {
                        current = pair.cdr.clone();
                        first = false;
                    }
                    other => {
                        output.push_str(" . ");
                        output.push_str(&render(other));
                        output.push(')');
                        return output;
                    }
                }
            }
            _ => unreachable!("render_pair called with non-pair value"),
        }
    }
}

fn escape_string(text: &str) -> String {
    let mut output = String::new();
    for ch in text.chars() {
        match ch {
            '\\' => output.push_str("\\\\"),
            '"' => output.push_str("\\\""),
            '\n' => output.push_str("\\n"),
            '\r' => output.push_str("\\r"),
            '\t' => output.push_str("\\t"),
            _ => output.push(ch),
        }
    }
    output
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
        self.skip_ignored();
        while !self.at_end() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let Some(ch) = self.peek() else {
            return Err(EvalError::message("unexpected end of input"));
        };

        match ch {
            '(' => self.parse_list(),
            ')' => Err(EvalError::message("unexpected ')'")),
            '"' => self.parse_string(),
            '\'' => {
                self.advance();
                Ok(Expr::List(vec![
                    Expr::Symbol("quote".into()),
                    self.parse_expr()?,
                ]))
            }
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.expect('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();
            match self.peek() {
                Some(')') => {
                    self.advance();
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::message("unterminated list")),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.expect('"')?;
        let mut value = String::new();

        loop {
            let Some(ch) = self.peek() else {
                return Err(EvalError::message("unterminated string literal"));
            };

            self.advance();
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let Some(escaped) = self.peek() else {
                        return Err(EvalError::message("unterminated escape sequence"));
                    };
                    self.advance();
                    value.push(match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => other,
                    });
                }
                other => value.push(other),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let mut token = String::new();
        while let Some(ch) = self.peek() {
            if is_delimiter(ch) {
                break;
            }
            token.push(ch);
            self.advance();
        }

        if token.is_empty() {
            return Err(EvalError::message("expected expression"));
        }

        match token.as_str() {
            "#t" => Ok(Expr::Bool(true)),
            "#f" => Ok(Expr::Bool(false)),
            _ if is_integer_token(&token) => {
                Ok(Expr::Int(token.parse().map_err(|_| {
                    EvalError::message(format!("invalid integer literal: {token}"))
                })?))
            }
            _ => Ok(Expr::Symbol(token)),
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek(), Some(ch) if ch.is_whitespace()) {
                self.advance();
            }

            if self.peek() == Some(';') {
                while !self.at_end() && self.peek() != Some('\n') {
                    self.advance();
                }
                continue;
            }

            break;
        }
    }

    fn expect(&mut self, expected: char) -> Result<(), EvalError> {
        match self.peek() {
            Some(ch) if ch == expected => {
                self.advance();
                Ok(())
            }
            Some(ch) => Err(EvalError::message(format!(
                "expected '{expected}', got '{ch}'",
            ))),
            None => Err(EvalError::message(format!(
                "expected '{expected}', got end of input",
            ))),
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn at_end(&self) -> bool {
        self.pos >= self.chars.len()
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '\'' | ';')
}

fn is_integer_token(token: &str) -> bool {
    let digits = match token.chars().next() {
        Some('+') | Some('-') if token.len() > 1 => &token[1..],
        _ => token,
    };

    !digits.is_empty() && digits.chars().all(|ch| ch.is_ascii_digit())
}
