pub mod error;

pub use error::EvalError;
use error::SourcePos;

use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    Ok(eval_program(input)?.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_program(input)?.to_string(), String::new()))
}

#[cfg(test)]
mod tests;

fn eval_program(input: &str) -> Result<Value, EvalError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program()?;

    if program.is_empty() {
        return Err(
            EvalError::Syntax("expected at least one expression".into())
                .with_position(SourcePos::new(1, 1)),
        );
    }

    let env = Env::global();
    eval_sequence(&program, env)
}

#[derive(Debug, Clone, PartialEq)]
struct Token {
    kind: TokenKind,
    pos: SourcePos,
}

impl Token {
    fn new(kind: TokenKind, pos: SourcePos) -> Self {
        Self { kind, pos }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum TokenKind {
    LParen,
    RParen,
    Quote,
    Bool(bool),
    Number(i64),
    String(String),
    Symbol(String),
}

#[derive(Debug, Clone, PartialEq)]
struct Expr {
    kind: ExprKind,
    pos: SourcePos,
}

impl Expr {
    fn new(kind: ExprKind, pos: SourcePos) -> Self {
        Self { kind, pos }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ExprKind {
    Bool(bool),
    Number(i64),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone)]
enum Value {
    Bool(bool),
    Number(i64),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Builtin(Builtin),
    Procedure(Rc<LambdaProcedure>),
    Void,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Bool(_) => "boolean",
            Self::Number(_) => "number",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::Builtin(_) | Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(true) => f.write_str("#t"),
            Self::Bool(false) => f.write_str("#f"),
            Self::Number(value) => write!(f, "{value}"),
            Self::String(value) => write!(f, "\"{}\"", escape_string(value)),
            Self::Symbol(value) => f.write_str(value),
            Self::List(items) => {
                f.write_str("(")?;
                for (index, item) in items.iter().enumerate() {
                    if index > 0 {
                        f.write_str(" ")?;
                    }
                    write!(f, "{item}")?;
                }
                f.write_str(")")
            }
            Self::Builtin(_) | Self::Procedure(_) => f.write_str("#<procedure>"),
            Self::Void => f.write_str("#<void>"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    Equal,
    LessEqual,
    Not,
    Cons,
    Car,
    Cdr,
    Null,
    List,
    Length,
    StringPred,
    NumberPred,
    BooleanPred,
    PairPred,
    SymbolPred,
}

impl Builtin {
    const ALL: [Self; 20] = [
        Self::Add,
        Self::Sub,
        Self::Mul,
        Self::Div,
        Self::LessThan,
        Self::GreaterThan,
        Self::Equal,
        Self::LessEqual,
        Self::Not,
        Self::Cons,
        Self::Car,
        Self::Cdr,
        Self::Null,
        Self::List,
        Self::Length,
        Self::StringPred,
        Self::NumberPred,
        Self::BooleanPred,
        Self::PairPred,
        Self::SymbolPred,
    ];

    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "=",
            Self::LessEqual => "<=",
            Self::Not => "not",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::Null => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::StringPred => "string?",
            Self::NumberPred => "number?",
            Self::BooleanPred => "boolean?",
            Self::PairPred => "pair?",
            Self::SymbolPred => "symbol?",
        }
    }
}

type EnvRef = Rc<RefCell<Env>>;

#[derive(Debug)]
struct Env {
    parent: Option<EnvRef>,
    bindings: HashMap<String, Value>,
}

impl Env {
    fn global() -> EnvRef {
        let env = Rc::new(RefCell::new(Self {
            parent: None,
            bindings: HashMap::new(),
        }));

        {
            let mut bindings = env.borrow_mut();
            for builtin in Builtin::ALL {
                bindings
                    .bindings
                    .insert(builtin.name().to_string(), Value::Builtin(builtin));
            }
        }

        env
    }

    fn child(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
        }))
    }

    fn define(env: &EnvRef, name: String, value: Value) {
        env.borrow_mut().bindings.insert(name, value);
    }

    fn lookup(env: &EnvRef, name: &str) -> Option<Value> {
        let (value, parent) = {
            let env_ref = env.borrow();
            (env_ref.bindings.get(name).cloned(), env_ref.parent.clone())
        };

        value.or_else(|| parent.and_then(|parent| Self::lookup(&parent, name)))
    }
}

#[derive(Debug)]
struct LambdaProcedure {
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

fn eval_sequence(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expr in exprs {
        last = eval_expr(expr, env.clone())?;
    }
    Ok(last)
}

fn eval_expr(expr: &Expr, env: EnvRef) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Bool(value) => Ok(Value::Bool(*value)),
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::String(value) => Ok(Value::String(value.clone())),
        ExprKind::Symbol(name) => Env::lookup(&env, name)
            .ok_or_else(|| EvalError::UnboundSymbol(name.clone()).with_position(expr.pos)),
        ExprKind::List(items) => {
            eval_list(items, expr.pos, env).map_err(|err| err.with_position(expr.pos))
        }
    }
}

fn eval_list(items: &[Expr], pos: SourcePos, env: EnvRef) -> Result<Value, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::Syntax("cannot evaluate an empty list".into()).with_position(pos));
    };

    match &head.kind {
        ExprKind::Symbol(name) if name == "and" => eval_and(args, env),
        ExprKind::Symbol(name) if name == "or" => eval_or(args, env),
        ExprKind::Symbol(name) if name == "if" => eval_if(args, env),
        ExprKind::Symbol(name) if name == "let" => eval_let(args, env),
        ExprKind::Symbol(name) if name == "begin" => eval_begin(args, env),
        ExprKind::Symbol(name) if name == "cond" => eval_cond(args, env),
        ExprKind::Symbol(name) if name == "quote" => eval_quote(args),
        ExprKind::Symbol(name) if name == "define" => eval_define(args, env),
        ExprKind::Symbol(name) if name == "lambda" => eval_lambda(args, env),
        _ => {
            let procedure = eval_expr(head, env.clone())?;
            let evaluated = args
                .iter()
                .map(|arg| eval_expr(arg, env.clone()))
                .collect::<Result<Vec<_>, EvalError>>()?;
            apply(procedure, &evaluated, pos)
        }
    }
}

fn eval_and(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);
    for arg in args {
        last = eval_expr(arg, env.clone())?;
        if !last.is_truthy() {
            return Ok(last);
        }
    }
    Ok(last)
}

fn eval_or(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for arg in args {
        let value = eval_expr(arg, env.clone())?;
        if value.is_truthy() {
            return Ok(value);
        }
    }
    Ok(Value::Bool(false))
}

fn eval_if(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(wrong_arg_count("if", "exactly 3", args.len()));
    }

    let condition = eval_expr(&args[0], env.clone())?;
    if condition.is_truthy() {
        eval_expr(&args[1], env)
    } else {
        eval_expr(&args[2], env)
    }
}

fn eval_let(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((bindings_expr, body)) = args.split_first() else {
        return Err(wrong_arg_count("let", "at least 2", args.len()));
    };
    if body.is_empty() {
        return Err(wrong_arg_count("let", "at least 2", args.len()));
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let values = bindings
        .iter()
        .map(|(_, expr)| eval_expr(expr, env.clone()))
        .collect::<Result<Vec<_>, EvalError>>()?;

    let local_env = Env::child(env);
    for ((name, _), value) in bindings.into_iter().zip(values) {
        Env::define(&local_env, name, value);
    }

    eval_sequence(body, local_env)
}

fn eval_begin(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    eval_sequence(args, env)
}

fn eval_cond(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::Syntax("cond clauses must be lists".into()));
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::Syntax("cond clauses cannot be empty".into()));
        };

        match &test.kind {
            ExprKind::Symbol(name) if name == "else" => {
                if index + 1 != args.len() {
                    return Err(EvalError::Syntax("cond else clause must be last".into()));
                }

                if body.is_empty() {
                    return Err(EvalError::Syntax("cond else clause requires a body".into()));
                }

                return eval_sequence(body, env);
            }
            _ => {
                let result = eval_expr(test, env.clone())?;
                if result.is_truthy() {
                    if body.is_empty() {
                        return Ok(result);
                    }
                    return eval_sequence(body, env);
                }
            }
        }
    }

    Ok(Value::Void)
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(wrong_arg_count("quote", "exactly 1", args.len()));
    }

    Ok(quote_expr(&args[0]))
}

fn quote_expr(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Bool(value) => Value::Bool(*value),
        ExprKind::Number(value) => Value::Number(*value),
        ExprKind::String(value) => Value::String(value.clone()),
        ExprKind::Symbol(value) => Value::Symbol(value.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_define(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(wrong_arg_count("define", "at least 2", args.len()));
    }

    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(wrong_arg_count("define", "exactly 2", args.len()));
            }

            let value = eval_expr(&args[1], env.clone())?;
            Env::define(&env, name.clone(), value);
            Ok(Value::Void)
        }
        ExprKind::List(signature) => {
            let Some((name, params)) = signature.split_first() else {
                return Err(EvalError::Syntax("define requires a function name".into()));
            };
            let name = expect_symbol(name, "function name")?;
            let params = parse_parameters(params)?;
            let procedure = Value::Procedure(Rc::new(LambdaProcedure {
                name: Some(name.clone()),
                params,
                body: args[1..].to_vec(),
                env: env.clone(),
            }));

            Env::define(&env, name, procedure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Syntax(
            "define requires a symbol or function signature".into(),
        )),
    }
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(wrong_arg_count("lambda", "at least 2", args.len()));
    };
    if body.is_empty() {
        return Err(wrong_arg_count("lambda", "at least 2", args.len()));
    }

    let params = parse_parameter_list(params_expr)?;
    Ok(Value::Procedure(Rc::new(LambdaProcedure {
        name: None,
        params,
        body: body.to_vec(),
        env,
    })))
}

fn parse_parameter_list(expr: &Expr) -> Result<Vec<String>, EvalError> {
    match &expr.kind {
        ExprKind::List(items) => parse_parameters(items),
        _ => Err(EvalError::Syntax(
            "lambda parameter list must be a list".into(),
        )),
    }
}

fn parse_let_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let ExprKind::List(bindings) = &expr.kind else {
        return Err(EvalError::Syntax("let bindings must be a list".into()));
    };

    bindings
        .iter()
        .map(|binding| {
            let ExprKind::List(parts) = &binding.kind else {
                return Err(EvalError::Syntax(
                    "let binding must be a (name expr) pair".into(),
                ));
            };

            match parts.as_slice() {
                [name, value] => Ok((expect_symbol(name, "let binding name")?, value.clone())),
                _ => Err(EvalError::Syntax(
                    "let binding must be a (name expr) pair".into(),
                )),
            }
        })
        .collect()
}

fn parse_parameters(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    items
        .iter()
        .map(|expr| expect_symbol(expr, "parameter"))
        .collect()
}

fn expect_symbol(expr: &Expr, context: &str) -> Result<String, EvalError> {
    match &expr.kind {
        ExprKind::Symbol(name) => Ok(name.clone()),
        _ => Err(EvalError::Syntax(format!("{context} must be a symbol"))),
    }
}

fn apply(function: Value, args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match function {
        Value::Builtin(builtin) => apply_builtin(builtin, args).map_err(|err| err.with_position(pos)),
        Value::Procedure(procedure) => {
            apply_lambda(&procedure, args).map_err(|err| err.with_position(pos))
        }
        other => Err(EvalError::NotAProcedure(other.to_string()).with_position(pos)),
    }
}

fn apply_lambda(procedure: &Rc<LambdaProcedure>, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != procedure.params.len() {
        let expected = format!("exactly {}", procedure.params.len());
        let name = procedure.name.as_deref().unwrap_or("lambda");
        return Err(wrong_arg_count(name, &expected, args.len()));
    }

    let local_env = Env::child(procedure.env.clone());
    for (param, arg) in procedure.params.iter().zip(args.iter()) {
        Env::define(&local_env, param.clone(), arg.clone());
    }

    eval_sequence(&procedure.body, local_env)
}

fn apply_builtin(builtin: Builtin, args: &[Value]) -> Result<Value, EvalError> {
    let name = builtin.name();
    match builtin {
        Builtin::Add => {
            let numbers = expect_numbers(name, args)?;
            let sum = numbers
                .iter()
                .try_fold(0_i64, |acc, value| acc.checked_add(*value))
                .ok_or(EvalError::IntegerOverflow)?;
            Ok(Value::Number(sum))
        }
        Builtin::Sub => {
            let numbers = expect_numbers(name, args)?;
            match numbers.split_first() {
                None => Err(wrong_arg_count(name, "at least 1", args.len())),
                Some((first, [])) => first
                    .checked_neg()
                    .map(Value::Number)
                    .ok_or(EvalError::IntegerOverflow),
                Some((first, rest)) => {
                    let result = rest
                        .iter()
                        .try_fold(*first, |acc, value| acc.checked_sub(*value))
                        .ok_or(EvalError::IntegerOverflow)?;
                    Ok(Value::Number(result))
                }
            }
        }
        Builtin::Mul => {
            let numbers = expect_numbers(name, args)?;
            let product = numbers
                .iter()
                .try_fold(1_i64, |acc, value| acc.checked_mul(*value))
                .ok_or(EvalError::IntegerOverflow)?;
            Ok(Value::Number(product))
        }
        Builtin::Div => {
            let numbers = expect_numbers(name, args)?;
            let Some((first, rest)) = numbers.split_first() else {
                return Err(wrong_arg_count(name, "at least 2", args.len()));
            };
            if rest.is_empty() {
                return Err(wrong_arg_count(name, "at least 2", args.len()));
            }

            let result = rest.iter().try_fold(*first, |acc, value| {
                if *value == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                acc.checked_div(*value).ok_or(EvalError::IntegerOverflow)
            })?;
            Ok(Value::Number(result))
        }
        Builtin::LessThan => compare_numbers(name, args, |left, right| left < right),
        Builtin::GreaterThan => compare_numbers(name, args, |left, right| left > right),
        Builtin::Equal => compare_numbers(name, args, |left, right| left == right),
        Builtin::LessEqual => compare_numbers(name, args, |left, right| left <= right),
        Builtin::Not => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }
            Ok(Value::Bool(!args[0].is_truthy()))
        }
        Builtin::Cons => {
            if args.len() != 2 {
                return Err(wrong_arg_count(name, "exactly 2", args.len()));
            }

            let list = expect_list_value(name, &args[1])?;
            let mut items = Vec::with_capacity(list.len() + 1);
            items.push(args[0].clone());
            items.extend(list.iter().cloned());
            Ok(Value::List(items))
        }
        Builtin::Car => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }

            let list = expect_list_value(name, &args[0])?;
            list.first()
                .cloned()
                .ok_or_else(|| EvalError::TypeMismatch {
                    expected: "non-empty list for car".into(),
                    found: "empty list".into(),
                })
        }
        Builtin::Cdr => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }

            let list = expect_list_value(name, &args[0])?;
            if list.is_empty() {
                return Err(EvalError::TypeMismatch {
                    expected: "non-empty list for cdr".into(),
                    found: "empty list".into(),
                });
            }
            Ok(Value::List(list[1..].to_vec()))
        }
        Builtin::Null => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }
            Ok(Value::Bool(
                matches!(&args[0], Value::List(items) if items.is_empty()),
            ))
        }
        Builtin::List => Ok(Value::List(args.to_vec())),
        Builtin::Length => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }

            let list = expect_list_value(name, &args[0])?;
            let length = i64::try_from(list.len()).map_err(|_| EvalError::IntegerOverflow)?;
            Ok(Value::Number(length))
        }
        Builtin::StringPred => {
            unary_predicate(name, args, |value| matches!(value, Value::String(_)))
        }
        Builtin::NumberPred => {
            unary_predicate(name, args, |value| matches!(value, Value::Number(_)))
        }
        Builtin::BooleanPred => {
            unary_predicate(name, args, |value| matches!(value, Value::Bool(_)))
        }
        Builtin::PairPred => unary_predicate(
            name,
            args,
            |value| matches!(value, Value::List(items) if !items.is_empty()),
        ),
        Builtin::SymbolPred => {
            unary_predicate(name, args, |value| matches!(value, Value::Symbol(_)))
        }
    }
}

fn compare_numbers(
    name: &str,
    args: &[Value],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = expect_numbers(name, args)?;
    if numbers.len() < 2 {
        return Err(wrong_arg_count(name, "at least 2", args.len()));
    }

    let is_true = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Bool(is_true))
}

fn expect_numbers(name: &str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Number(number) => Ok(*number),
            other => Err(EvalError::TypeMismatch {
                expected: format!("number for {name}"),
                found: other.type_name().to_string(),
            }),
        })
        .collect()
}

fn expect_list_value<'a>(name: &str, value: &'a Value) -> Result<&'a [Value], EvalError> {
    match value {
        Value::List(items) => Ok(items),
        other => Err(EvalError::TypeMismatch {
            expected: format!("list for {name}"),
            found: other.type_name().to_string(),
        }),
    }
}

fn unary_predicate(
    name: &str,
    args: &[Value],
    predicate: impl Fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(wrong_arg_count(name, "exactly 1", args.len()));
    }

    Ok(Value::Bool(predicate(&args[0])))
}

fn wrong_arg_count(name: &str, expected: &str, got: usize) -> EvalError {
    EvalError::WrongArgumentCount {
        name: name.to_string(),
        expected: expected.to_string(),
        got,
    }
}

fn escape_string(value: &str) -> String {
    let mut escaped = String::new();
    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

fn advance_position(pos: &mut SourcePos, ch: char) {
    if ch == '\n' {
        pos.line += 1;
        pos.col = 1;
    } else {
        pos.col += 1;
    }
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let mut index = 0;
    let mut pos = SourcePos::new(1, 1);

    while index < input.len() {
        let ch = input[index..]
            .chars()
            .next()
            .expect("index always points to a valid character boundary");

        match ch {
            c if c.is_whitespace() => {
                index += ch.len_utf8();
                advance_position(&mut pos, ch);
            }
            ';' => {
                index += ch.len_utf8();
                advance_position(&mut pos, ch);
                while index < input.len() {
                    let next = input[index..]
                        .chars()
                        .next()
                        .expect("index always points to a valid character boundary");
                    index += next.len_utf8();
                    advance_position(&mut pos, next);
                    if next == '\n' {
                        break;
                    }
                }
            }
            '(' => {
                tokens.push(Token::new(TokenKind::LParen, pos));
                index += ch.len_utf8();
                advance_position(&mut pos, ch);
            }
            ')' => {
                tokens.push(Token::new(TokenKind::RParen, pos));
                index += ch.len_utf8();
                advance_position(&mut pos, ch);
            }
            '\'' => {
                tokens.push(Token::new(TokenKind::Quote, pos));
                index += ch.len_utf8();
                advance_position(&mut pos, ch);
            }
            '"' => {
                let start_pos = pos;
                let (string, next_index, next_pos) = parse_string(input, index, pos)?;
                tokens.push(Token::new(TokenKind::String(string), start_pos));
                index = next_index;
                pos = next_pos;
            }
            _ => {
                let start = index;
                let start_pos = pos;
                while index < input.len() {
                    let next = input[index..]
                        .chars()
                        .next()
                        .expect("index always points to a valid character boundary");
                    if next.is_whitespace()
                        || next == '('
                        || next == ')'
                        || next == '\''
                        || next == ';'
                    {
                        break;
                    }
                    index += next.len_utf8();
                    advance_position(&mut pos, next);
                }

                let atom = &input[start..index];
                tokens.push(Token::new(parse_atom(atom), start_pos));
            }
        }
    }

    Ok(tokens)
}

fn parse_string(
    input: &str,
    start: usize,
    start_pos: SourcePos,
) -> Result<(String, usize, SourcePos), EvalError> {
    let mut result = String::new();
    let mut pos = start_pos;
    let mut index = start + 1;
    advance_position(&mut pos, '"');

    while index < input.len() {
        let ch = input[index..]
            .chars()
            .next()
            .expect("index always points to a valid character boundary");
        index += ch.len_utf8();
        advance_position(&mut pos, ch);

        match ch {
            '"' => return Ok((result, index, pos)),
            '\\' => {
                let escape_pos = pos;
                let escaped = input[index..]
                    .chars()
                    .next()
                    .ok_or_else(|| {
                        EvalError::Syntax("unterminated string literal".into())
                            .with_position(start_pos)
                    })?;
                index += escaped.len_utf8();
                advance_position(&mut pos, escaped);
                match escaped {
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    _ => {
                        return Err(
                            EvalError::Syntax(format!("unsupported escape sequence: \\{escaped}"))
                                .with_position(escape_pos),
                        );
                    }
                }
            }
            _ => result.push(ch),
        }
    }

    Err(EvalError::Syntax("unterminated string literal".into()).with_position(start_pos))
}

fn parse_atom(atom: &str) -> TokenKind {
    match atom {
        "#t" => TokenKind::Bool(true),
        "#f" => TokenKind::Bool(false),
        _ => match atom.parse::<i64>() {
            Ok(value) => TokenKind::Number(value),
            Err(_) => TokenKind::Symbol(atom.to_string()),
        },
    }
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
    last_pos: SourcePos,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            last_pos: SourcePos::new(1, 1),
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        while self.index < self.tokens.len() {
            expressions.push(self.parse_expr()?);
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        let token = self
            .tokens
            .get(self.index)
            .cloned()
            .ok_or_else(|| {
                EvalError::Syntax("unexpected end of input".into()).with_position(self.last_pos)
            })?;
        self.index += 1;
        self.last_pos = token.pos;

        match token.kind {
            TokenKind::LParen => {
                let mut items = Vec::new();
                while self.index < self.tokens.len() {
                    if matches!(
                        self.tokens.get(self.index).map(|next| &next.kind),
                        Some(TokenKind::RParen)
                    ) {
                        self.last_pos = self.tokens[self.index].pos;
                        self.index += 1;
                        return Ok(Expr::new(ExprKind::List(items), token.pos));
                    }
                    items.push(self.parse_expr()?);
                }
                Err(EvalError::Syntax("missing ')'".into()).with_position(token.pos))
            }
            TokenKind::RParen => {
                Err(EvalError::Syntax("unexpected ')'".into()).with_position(token.pos))
            }
            TokenKind::Quote => {
                let quoted = self.parse_expr()?;
                Ok(Expr::new(
                    ExprKind::List(vec![
                        Expr::new(ExprKind::Symbol("quote".to_string()), token.pos),
                        quoted,
                    ]),
                    token.pos,
                ))
            }
            TokenKind::Bool(value) => Ok(Expr::new(ExprKind::Bool(value), token.pos)),
            TokenKind::Number(value) => Ok(Expr::new(ExprKind::Number(value), token.pos)),
            TokenKind::String(value) => Ok(Expr::new(ExprKind::String(value), token.pos)),
            TokenKind::Symbol(name) => Ok(Expr::new(ExprKind::Symbol(name), token.pos)),
        }
    }
}
