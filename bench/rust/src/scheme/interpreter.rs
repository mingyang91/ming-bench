use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::EvalError;

type Env = Rc<Environment>;
type BuiltinFunc = fn(&[Value]) -> Result<Value, EvalError>;
type ExprRef = Rc<Expr>;

#[derive(Clone, Debug)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<ExprRef>),
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Nil,
    Pair(Rc<Pair>),
    Procedure(Rc<Procedure>),
    Void,
}

#[derive(Clone)]
struct Pair {
    car: Value,
    cdr: Value,
}

#[derive(Clone)]
enum Procedure {
    Builtin {
        name: &'static str,
        func: BuiltinFunc,
    },
    Lambda {
        params: Vec<String>,
        body: Vec<ExprRef>,
        env: Env,
    },
}

enum TailOutcome {
    Value(Value),
    Expr(ExprRef, Env),
}

struct Environment {
    parent: Option<Env>,
    bindings: RefCell<HashMap<String, Value>>,
}

impl Environment {
    fn new(parent: Option<Env>) -> Env {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
        })
    }

    fn define(env: &Env, name: impl Into<String>, value: Value) {
        env.bindings.borrow_mut().insert(name.into(), value);
    }

    fn get(env: &Env, name: &str) -> Option<Value> {
        if let Some(value) = env.bindings.borrow().get(name).cloned() {
            Some(value)
        } else if let Some(parent) = &env.parent {
            Self::get(parent, name)
        } else {
            None
        }
    }
}

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let expressions = Parser::new(input).parse_program()?;
    let env = default_env();
    let mut last = Value::Void;

    for expression in &expressions {
        last = eval(expression.clone(), env.clone())?;
    }

    Ok(match last {
        Value::Void => String::new(),
        value => value.render(),
    })
}

pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

fn default_env() -> Env {
    let env = Environment::new(None);

    for (name, func) in [
        ("+", builtin_add as BuiltinFunc),
        ("-", builtin_sub),
        ("*", builtin_mul),
        ("/", builtin_div),
        ("<", builtin_lt),
        (">", builtin_gt),
        ("=", builtin_num_eq),
        ("<=", builtin_lte),
        ("not", builtin_not),
        ("cons", builtin_cons),
        ("car", builtin_car),
        ("cdr", builtin_cdr),
        ("null?", builtin_null),
        ("list", builtin_list),
        ("length", builtin_length),
        ("append", builtin_append),
        ("reverse", builtin_reverse),
        ("string?", builtin_is_string),
        ("number?", builtin_is_number),
        ("boolean?", builtin_is_boolean),
        ("pair?", builtin_is_pair),
        ("symbol?", builtin_is_symbol),
    ] {
        Environment::define(
            &env,
            name,
            Value::Procedure(Rc::new(Procedure::Builtin { name, func })),
        );
    }

    env
}

fn eval(expression: ExprRef, env: Env) -> Result<Value, EvalError> {
    let mut current_expression = expression;
    let mut current_env = env;

    loop {
        let next = match current_expression.as_ref() {
            Expr::Integer(value) => return Ok(Value::Integer(*value)),
            Expr::Boolean(value) => return Ok(Value::Boolean(*value)),
            Expr::String(value) => return Ok(Value::String(value.clone())),
            Expr::Symbol(name) => {
                return Environment::get(&current_env, name)
                    .ok_or_else(|| EvalError::UnboundVariable(name.clone()))
            }
            Expr::List(items) if items.is_empty() => {
                return Err(EvalError::Runtime("cannot evaluate an empty list".into()))
            }
            Expr::List(items) => match items.first().map(|item| item.as_ref()) {
                Some(Expr::Symbol(symbol)) if symbol == "quote" => {
                    TailOutcome::Value(eval_quote(items)?)
                }
                Some(Expr::Symbol(symbol)) if symbol == "if" => {
                    eval_if(items, current_env.clone())?
                }
                Some(Expr::Symbol(symbol)) if symbol == "define" => {
                    TailOutcome::Value(eval_define(items, current_env.clone())?)
                }
                Some(Expr::Symbol(symbol)) if symbol == "lambda" => {
                    TailOutcome::Value(eval_lambda(items, current_env.clone())?)
                }
                Some(Expr::Symbol(symbol)) if symbol == "and" => {
                    eval_and(&items[1..], current_env.clone())?
                }
                Some(Expr::Symbol(symbol)) if symbol == "or" => {
                    eval_or(&items[1..], current_env.clone())?
                }
                Some(Expr::Symbol(symbol)) if symbol == "let" => {
                    eval_let(items, current_env.clone())?
                }
                Some(Expr::Symbol(symbol)) if symbol == "begin" => {
                    eval_sequence_tail(&items[1..], current_env.clone())?
                }
                Some(Expr::Symbol(symbol)) if symbol == "cond" => {
                    eval_cond(&items[1..], current_env.clone())?
                }
                _ => eval_application(items, current_env.clone())?,
            },
        };

        match next {
            TailOutcome::Value(value) => return Ok(value),
            TailOutcome::Expr(expression, env) => {
                current_expression = expression;
                current_env = env;
            }
        }
    }
}

fn eval_quote(items: &[ExprRef]) -> Result<Value, EvalError> {
    ensure_exact_args("quote", items.len() - 1, 1)?;
    datum_to_value(items[1].as_ref())
}

fn eval_if(items: &[ExprRef], env: Env) -> Result<TailOutcome, EvalError> {
    ensure_exact_args("if", items.len() - 1, 3)?;
    let condition = eval(items[1].clone(), env.clone())?;
    if condition.is_truthy() {
        Ok(TailOutcome::Expr(items[2].clone(), env))
    } else {
        Ok(TailOutcome::Expr(items[3].clone(), env))
    }
}

fn eval_define(items: &[ExprRef], env: Env) -> Result<Value, EvalError> {
    if items.len() < 3 {
        return Err(EvalError::WrongArgCount {
            name: "define".into(),
            expected: "at least 2".into(),
            got: items.len().saturating_sub(1),
        });
    }

    match items[1].as_ref() {
        Expr::Symbol(name) => {
            ensure_exact_args("define", items.len() - 1, 2)?;
            let value = eval(items[2].clone(), env.clone())?;
            Environment::define(&env, name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) if !signature.is_empty() => {
            let Expr::Symbol(name) = signature[0].as_ref() else {
                return Err(EvalError::Syntax(
                    "define: function name must be a symbol".into(),
                ));
            };

            let params = parse_params(&signature[1..], "define")?;
            let body = items[2..].to_vec();
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
                params,
                body,
                env: env.clone(),
            }));

            Environment::define(&env, name.clone(), procedure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Syntax(
            "define: expected a symbol or function signature".into(),
        )),
    }
}

fn eval_lambda(items: &[ExprRef], env: Env) -> Result<Value, EvalError> {
    if items.len() < 3 {
        return Err(EvalError::WrongArgCount {
            name: "lambda".into(),
            expected: "at least 2".into(),
            got: items.len().saturating_sub(1),
        });
    }

    let Expr::List(params_expr) = items[1].as_ref() else {
        return Err(EvalError::Syntax(
            "lambda: parameter list must be a list".into(),
        ));
    };

    let params = parse_params(params_expr, "lambda")?;
    Ok(Value::Procedure(Rc::new(Procedure::Lambda {
        params,
        body: items[2..].to_vec(),
        env,
    })))
}

fn eval_and(items: &[ExprRef], env: Env) -> Result<TailOutcome, EvalError> {
    if items.is_empty() {
        return Ok(TailOutcome::Value(Value::Boolean(true)));
    }

    for item in &items[..items.len() - 1] {
        let value = eval(item.clone(), env.clone())?;
        if !value.is_truthy() {
            return Ok(TailOutcome::Value(value));
        }
    }

    Ok(TailOutcome::Expr(items.last().cloned().unwrap(), env))
}

fn eval_or(items: &[ExprRef], env: Env) -> Result<TailOutcome, EvalError> {
    if items.is_empty() {
        return Ok(TailOutcome::Value(Value::Boolean(false)));
    }

    for item in &items[..items.len() - 1] {
        let value = eval(item.clone(), env.clone())?;
        if value.is_truthy() {
            return Ok(TailOutcome::Value(value));
        }
    }

    Ok(TailOutcome::Expr(items.last().cloned().unwrap(), env))
}

fn eval_let(items: &[ExprRef], env: Env) -> Result<TailOutcome, EvalError> {
    if items.len() < 3 {
        return Err(EvalError::WrongArgCount {
            name: "let".into(),
            expected: "at least 2".into(),
            got: items.len().saturating_sub(1),
        });
    }

    match items[1].as_ref() {
        Expr::Symbol(name) => eval_named_let(name, &items[2], &items[3..], env),
        bindings => eval_plain_let(bindings, &items[2..], env),
    }
}

fn eval_named_let(
    name: &str,
    bindings_expr: &ExprRef,
    body: &[ExprRef],
    env: Env,
) -> Result<TailOutcome, EvalError> {
    let bindings = parse_bindings(bindings_expr, "let")?;
    let mut params = Vec::with_capacity(bindings.len());
    let mut args = Vec::with_capacity(bindings.len());

    for (param, expression) in bindings {
        params.push(param);
        args.push(eval(expression, env.clone())?);
    }

    let loop_env = Environment::new(Some(env));
    let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
        params,
        body: body.to_vec(),
        env: loop_env.clone(),
    }));
    Environment::define(&loop_env, name.to_string(), procedure.clone());
    tail_apply(procedure, args)
}

fn eval_plain_let(
    bindings_expr: &Expr,
    body: &[ExprRef],
    env: Env,
) -> Result<TailOutcome, EvalError> {
    let bindings = parse_bindings(bindings_expr, "let")?;
    let mut values = Vec::with_capacity(bindings.len());

    for (name, expression) in &bindings {
        values.push((name.clone(), eval(expression.clone(), env.clone())?));
    }

    let let_env = Environment::new(Some(env));
    for (name, value) in values {
        Environment::define(&let_env, name, value);
    }

    eval_sequence_tail(body, let_env)
}

fn eval_cond(clauses: &[ExprRef], env: Env) -> Result<TailOutcome, EvalError> {
    for clause in clauses {
        let Expr::List(items) = clause.as_ref() else {
            return Err(EvalError::Syntax("cond: each clause must be a list".into()));
        };

        if items.is_empty() {
            return Err(EvalError::Syntax("cond: empty clause".into()));
        }

        if matches!(items.first().map(|item| item.as_ref()), Some(Expr::Symbol(symbol)) if symbol == "else")
        {
            return if items.len() == 1 {
                Ok(TailOutcome::Value(Value::Boolean(true)))
            } else {
                eval_sequence_tail(&items[1..], env.clone())
            };
        }

        let test_value = eval(items[0].clone(), env.clone())?;
        if test_value.is_truthy() {
            return if items.len() == 1 {
                Ok(TailOutcome::Value(test_value))
            } else {
                eval_sequence_tail(&items[1..], env.clone())
            };
        }
    }

    Ok(TailOutcome::Value(Value::Void))
}

fn eval_sequence_tail(expressions: &[ExprRef], env: Env) -> Result<TailOutcome, EvalError> {
    if expressions.is_empty() {
        return Ok(TailOutcome::Value(Value::Void));
    }

    for expression in &expressions[..expressions.len() - 1] {
        eval(expression.clone(), env.clone())?;
    }

    Ok(TailOutcome::Expr(expressions.last().cloned().unwrap(), env))
}

fn eval_application(items: &[ExprRef], env: Env) -> Result<TailOutcome, EvalError> {
    let operator = eval(items[0].clone(), env.clone())?;
    let mut args = Vec::with_capacity(items.len().saturating_sub(1));

    for expression in &items[1..] {
        args.push(eval(expression.clone(), env.clone())?);
    }

    tail_apply(operator, args)
}

fn tail_apply(procedure: Value, args: Vec<Value>) -> Result<TailOutcome, EvalError> {
    let Value::Procedure(procedure) = procedure else {
        return Err(EvalError::NotAProcedure(procedure.render()));
    };

    match procedure.as_ref() {
        Procedure::Builtin { func, .. } => Ok(TailOutcome::Value(func(&args)?)),
        Procedure::Lambda { params, body, env } => {
            ensure_exact_args("lambda", args.len(), params.len())?;
            let call_env = Environment::new(Some(env.clone()));

            for (param, value) in params.iter().zip(args.into_iter()) {
                Environment::define(&call_env, param.clone(), value);
            }

            eval_sequence_tail(body, call_env)
        }
    }
}

fn parse_params(items: &[ExprRef], context: &str) -> Result<Vec<String>, EvalError> {
    let mut params = Vec::with_capacity(items.len());

    for item in items {
        let Expr::Symbol(symbol) = item.as_ref() else {
            return Err(EvalError::Syntax(format!(
                "{context}: parameters must be symbols"
            )));
        };
        params.push(symbol.clone());
    }

    Ok(params)
}

fn parse_bindings(expression: &Expr, context: &str) -> Result<Vec<(String, ExprRef)>, EvalError> {
    let Expr::List(bindings) = expression else {
        return Err(EvalError::Syntax(format!(
            "{context}: bindings must be a list"
        )));
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let Expr::List(parts) = binding.as_ref() else {
            return Err(EvalError::Syntax(format!(
                "{context}: each binding must be a list"
            )));
        };

        if parts.len() != 2 {
            return Err(EvalError::Syntax(format!(
                "{context}: each binding must contain a name and an expression"
            )));
        }

        let Expr::Symbol(name) = parts[0].as_ref() else {
            return Err(EvalError::Syntax(format!(
                "{context}: binding name must be a symbol"
            )));
        };

        parsed.push((name.clone(), parts[1].clone()));
    }

    Ok(parsed)
}

fn datum_to_value(expression: &Expr) -> Result<Value, EvalError> {
    match expression {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(value) => Ok(Value::Symbol(value.clone())),
        Expr::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(datum_to_value(item.as_ref())?);
            }
            Ok(list_from_vec(values))
        }
    }
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum = 0;
    for arg in args {
        sum += expect_integer("+", arg)?;
    }
    Ok(Value::Integer(sum))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "-".into(),
            expected: "at least 1".into(),
            got: 0,
        });
    }

    let first = expect_integer("-", &args[0])?;
    if args.len() == 1 {
        return Ok(Value::Integer(-first));
    }

    let mut total = first;
    for arg in &args[1..] {
        total -= expect_integer("-", arg)?;
    }
    Ok(Value::Integer(total))
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product = 1;
    for arg in args {
        product *= expect_integer("*", arg)?;
    }
    Ok(Value::Integer(product))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2".into(),
            got: args.len(),
        });
    }

    let mut total = expect_integer("/", &args[0])?;
    for arg in &args[1..] {
        let divisor = expect_integer("/", arg)?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        total /= divisor;
    }
    Ok(Value::Integer(total))
}

fn builtin_lt(args: &[Value]) -> Result<Value, EvalError> {
    compare_numbers("<", args, |a, b| a < b)
}

fn builtin_gt(args: &[Value]) -> Result<Value, EvalError> {
    compare_numbers(">", args, |a, b| a > b)
}

fn builtin_num_eq(args: &[Value]) -> Result<Value, EvalError> {
    compare_numbers("=", args, |a, b| a == b)
}

fn builtin_lte(args: &[Value]) -> Result<Value, EvalError> {
    compare_numbers("<=", args, |a, b| a <= b)
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("not", args.len(), 1)?;
    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("cons", args.len(), 2)?;
    Ok(Value::Pair(Rc::new(Pair {
        car: args[0].clone(),
        cdr: args[1].clone(),
    })))
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("car", args.len(), 1)?;
    let Value::Pair(pair) = &args[0] else {
        return Err(type_mismatch("car", "pair", &args[0]));
    };
    Ok(pair.car.clone())
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("cdr", args.len(), 1)?;
    let Value::Pair(pair) = &args[0] else {
        return Err(type_mismatch("cdr", "pair", &args[0]));
    };
    Ok(pair.cdr.clone())
}

fn builtin_null(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("null?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Nil)))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(list_from_vec(args.to_vec()))
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("length", args.len(), 1)?;
    Ok(Value::Integer(list_to_vec("length", &args[0])?.len() as i64))
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut values = Vec::new();
    for arg in args {
        values.extend(list_to_vec("append", arg)?);
    }
    Ok(list_from_vec(values))
}

fn builtin_reverse(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("reverse", args.len(), 1)?;
    let mut values = list_to_vec("reverse", &args[0])?;
    values.reverse();
    Ok(list_from_vec(values))
}

fn builtin_is_string(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("string?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::String(_))))
}

fn builtin_is_number(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("number?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
}

fn builtin_is_boolean(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("boolean?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
}

fn builtin_is_pair(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("pair?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Pair(_))))
}

fn builtin_is_symbol(args: &[Value]) -> Result<Value, EvalError> {
    ensure_exact_args("symbol?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
}

fn compare_numbers(
    name: &str,
    args: &[Value],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2".into(),
            got: args.len(),
        });
    }

    let mut previous = expect_integer(name, &args[0])?;
    for arg in &args[1..] {
        let current = expect_integer(name, arg)?;
        if !predicate(previous, current) {
            return Ok(Value::Boolean(false));
        }
        previous = current;
    }

    Ok(Value::Boolean(true))
}

fn expect_integer(name: &str, value: &Value) -> Result<i64, EvalError> {
    match value {
        Value::Integer(integer) => Ok(*integer),
        _ => Err(type_mismatch(name, "number", value)),
    }
}

fn list_to_vec(name: &str, value: &Value) -> Result<Vec<Value>, EvalError> {
    let mut result = Vec::new();
    let mut current = value.clone();

    loop {
        match current {
            Value::Nil => return Ok(result),
            Value::Pair(pair) => {
                result.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            other => return Err(type_mismatch(name, "proper list", &other)),
        }
    }
}

fn list_from_vec(values: Vec<Value>) -> Value {
    values.into_iter().rev().fold(Value::Nil, |tail, head| {
        Value::Pair(Rc::new(Pair {
            car: head,
            cdr: tail,
        }))
    })
}

fn ensure_exact_args(name: &str, got: usize, expected: usize) -> Result<(), EvalError> {
    if got == expected {
        Ok(())
    } else {
        Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: expected.to_string(),
            got,
        })
    }
}

fn type_mismatch(name: &str, expected: &str, value: &Value) -> EvalError {
    EvalError::TypeMismatch {
        name: name.into(),
        expected: expected.into(),
        found: value.type_name().into(),
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::Nil => "empty list",
            Value::Pair(_) => "pair",
            Value::Procedure(_) => "procedure",
            Value::Void => "void",
        }
    }

    fn render(&self) -> String {
        match self {
            Value::Integer(value) => value.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::String(value) => format!("\"{}\"", escape_string(value)),
            Value::Symbol(value) => value.clone(),
            Value::Nil => "()".into(),
            Value::Pair(_) => render_pair(self),
            Value::Procedure(procedure) => match procedure.as_ref() {
                Procedure::Builtin { name, .. } => format!("#<procedure:{name}>"),
                Procedure::Lambda { .. } => "#<procedure>".into(),
            },
            Value::Void => String::new(),
        }
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
                output.push_str(&pair.car.render());
                current = pair.cdr.clone();
                first = false;
            }
            Value::Nil => {
                output.push(')');
                return output;
            }
            other => {
                output.push_str(" . ");
                output.push_str(&other.render());
                output.push(')');
                return output;
            }
        }
    }
}

fn escape_string(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len());
    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

struct Parser<'a> {
    input: &'a str,
    offset: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, offset: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<ExprRef>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_space_and_comments();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_space_and_comments();
        }

        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<ExprRef, EvalError> {
        self.skip_space_and_comments();
        let ch = self
            .peek_char()
            .ok_or_else(|| EvalError::Syntax("unexpected end of input".into()))?;

        match ch {
            '(' => self.parse_list(),
            ')' => Err(EvalError::Syntax("unexpected ')'".into())),
            '\'' => {
                self.bump_char();
                let expression = self.parse_expr()?;
                Ok(Rc::new(Expr::List(vec![
                    Rc::new(Expr::Symbol("quote".into())),
                    expression,
                ])))
            }
            '"' => self.parse_string(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<ExprRef, EvalError> {
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_space_and_comments();
            match self.peek_char() {
                Some(')') => {
                    self.bump_char();
                    return Ok(Rc::new(Expr::List(items)));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::Syntax("unterminated list".into())),
            }
        }
    }

    fn parse_string(&mut self) -> Result<ExprRef, EvalError> {
        self.expect_char('"')?;
        let mut value = String::new();

        loop {
            let ch = self
                .bump_char()
                .ok_or_else(|| EvalError::Syntax("unterminated string".into()))?;

            match ch {
                '"' => return Ok(Rc::new(Expr::String(value))),
                '\\' => {
                    let escaped = self
                        .bump_char()
                        .ok_or_else(|| EvalError::Syntax("unterminated escape".into()))?;
                    value.push(match escaped {
                        'n' => '\n',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                }
                other => value.push(other),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<ExprRef, EvalError> {
        let mut token = String::new();

        while let Some(ch) = self.peek_char() {
            if is_delimiter(ch) {
                break;
            }
            token.push(ch);
            self.bump_char();
        }

        if token.is_empty() {
            return Err(EvalError::Syntax("expected expression".into()));
        }

        match token.as_str() {
            "#t" => Ok(Rc::new(Expr::Boolean(true))),
            "#f" => Ok(Rc::new(Expr::Boolean(false))),
            _ if is_integer_token(&token) => token
                .parse::<i64>()
                .map(Expr::Integer)
                .map(Rc::new)
                .map_err(|_| EvalError::Syntax(format!("invalid integer literal: {token}"))),
            _ => Ok(Rc::new(Expr::Symbol(token))),
        }
    }

    fn skip_space_and_comments(&mut self) {
        loop {
            while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
                self.bump_char();
            }

            if self.peek_char() == Some(';') {
                while let Some(ch) = self.bump_char() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        match self.bump_char() {
            Some(ch) if ch == expected => Ok(()),
            Some(ch) => Err(EvalError::Syntax(format!(
                "expected '{expected}', found '{ch}'"
            ))),
            None => Err(EvalError::Syntax(format!("expected '{expected}'"))),
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.offset..].chars().next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.offset += ch.len_utf8();
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.input.len()
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '\'' | ';')
}

fn is_integer_token(token: &str) -> bool {
    let Some(first) = token.chars().next() else {
        return false;
    };

    if first == '-' || first == '+' {
        token.len() > 1 && token[1..].chars().all(|ch| ch.is_ascii_digit())
    } else {
        token.chars().all(|ch| ch.is_ascii_digit())
    }
}
