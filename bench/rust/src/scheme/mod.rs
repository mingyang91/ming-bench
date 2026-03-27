pub mod error;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use error::EvalError;
use error::SourcePos;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Integer(i64, SourcePos),
    Boolean(bool, SourcePos),
    String(String, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    fn pos(&self) -> SourcePos {
        match self {
            Self::Integer(_, pos)
            | Self::Boolean(_, pos)
            | Self::String(_, pos)
            | Self::Symbol(_, pos)
            | Self::List(_, pos) => *pos,
        }
    }
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Procedure(Rc<Procedure>),
    Void,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn as_integer(&self, name: &str) -> Result<i64, EvalError> {
        match self {
            Self::Integer(value) => Ok(*value),
            _ => Err(EvalError::ExpectedNumber {
                name: name.to_owned(),
            }),
        }
    }

    fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".to_owned(),
            Self::Boolean(false) => "#f".to_owned(),
            Self::String(value) => render_string(value),
            Self::Symbol(value) => value.clone(),
            Self::Char(value) => render_char(*value),
            Self::List(values) => render_list(values),
            Self::Procedure(_) => "#<procedure>".to_owned(),
            Self::Void => "#<void>".to_owned(),
        }
    }

    fn render_display(&self) -> String {
        match self {
            Self::String(value) => value.clone(),
            Self::Char(value) => value.to_string(),
            _ => self.render(),
        }
    }
}

#[derive(Clone, Copy)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    Equal,
    LessThanOrEqual,
    Not,
    Cons,
    Car,
    Cdr,
    Append,
    List,
    Length,
    NullPred,
    PairPred,
    SymbolPred,
    StringPred,
    NumberPred,
    BooleanPred,
    Display,
    Write,
    Newline,
    StringAppend,
    StringLength,
    Substring,
    StringToNumber,
    NumberToString,
    SymbolToString,
    StringToSymbol,
    StringRef,
    CharPred,
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

type EnvRef = Rc<Environment>;

struct Environment {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, Value>>,
    output: Rc<RefCell<String>>,
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        let output = parent.as_ref().map_or_else(
            || Rc::new(RefCell::new(String::new())),
            |env| env.output.clone(),
        );

        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
            output,
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.bindings.borrow_mut().insert(name.into(), value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return Some(value);
        }

        self.parent.as_ref().and_then(|parent| parent.lookup(name))
    }
}

fn render_string(value: &str) -> String {
    let mut rendered = String::with_capacity(value.len() + 2);
    rendered.push('"');

    for ch in value.chars() {
        match ch {
            '\\' => rendered.push_str("\\\\"),
            '"' => rendered.push_str("\\\""),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            _ => rendered.push(ch),
        }
    }

    rendered.push('"');
    rendered
}

fn render_list(values: &[Value]) -> String {
    let mut rendered = String::from("(");

    for (index, value) in values.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&value.render());
    }

    rendered.push(')');
    rendered
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".to_owned(),
        '\n' => "#\\newline".to_owned(),
        _ => format!("#\\{value}"),
    }
}

struct Parser<'a> {
    input: &'a str,
    pos: usize,
    line: usize,
    col: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_ignored();

        while self.peek_char().is_some() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            Err(EvalError::EmptyInput.with_position(self.current_pos()))
        } else {
            Ok(exprs)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let start = self.current_pos();

        match self.peek_char() {
            Some('(') => self.parse_list(start),
            Some(')') => Err(self.parse_error("unexpected ')'")),
            Some('\'') => self.parse_quote(start),
            Some('"') => self.parse_string(start),
            Some(_) => self.parse_atom(start),
            None => Err(self.parse_error("unexpected end of input")),
        }
    }

    fn parse_list(&mut self, start: SourcePos) -> Result<Expr, EvalError> {
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek_char() {
                Some(')') => {
                    self.advance_char();
                    return Ok(Expr::List(items, start));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(self.parse_error("unterminated list")),
            }
        }
    }

    fn parse_string(&mut self, start: SourcePos) -> Result<Expr, EvalError> {
        self.expect_char('"')?;
        let mut value = String::new();

        while let Some(ch) = self.advance_char() {
            match ch {
                '"' => return Ok(Expr::String(value, start)),
                '\\' => {
                    let escaped = self
                        .advance_char()
                        .ok_or_else(|| self.parse_error("unterminated string"))?;
                    let decoded = match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => other,
                    };
                    value.push(decoded);
                }
                other => value.push(other),
            }
        }

        Err(self.parse_error("unterminated string"))
    }

    fn parse_quote(&mut self, start: SourcePos) -> Result<Expr, EvalError> {
        self.expect_char('\'')?;
        let expr = self.parse_expr()?;
        Ok(Expr::List(
            vec![Expr::Symbol("quote".to_owned(), start), expr],
            start,
        ))
    }

    fn parse_atom(&mut self, start: SourcePos) -> Result<Expr, EvalError> {
        let start_index = self.pos;

        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.advance_char();
        }

        let atom = &self.input[start_index..self.pos];

        if atom.is_empty() {
            return Err(EvalError::Parse("expected expression".to_owned()).with_position(start));
        }

        if atom == "#t" {
            return Ok(Expr::Boolean(true, start));
        }

        if atom == "#f" {
            return Ok(Expr::Boolean(false, start));
        }

        if let Ok(value) = atom.parse::<i64>() {
            return Ok(Expr::Integer(value, start));
        }

        Ok(Expr::Symbol(atom.to_owned(), start))
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
                self.advance_char();
            }

            if self.peek_char() == Some(';') {
                while let Some(ch) = self.advance_char() {
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
        match self.peek_char() {
            Some(ch) if ch == expected => {
                self.advance_char();
                Ok(())
            }
            Some(ch) => Err(self.parse_error(format!("expected '{expected}', found '{ch}'"))),
            None => Err(self.parse_error(format!("expected '{expected}'"))),
        }
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos {
            line: self.line,
            col: self.col,
        }
    }

    fn parse_error(&self, message: impl Into<String>) -> EvalError {
        EvalError::Parse(message.into()).with_position(self.current_pos())
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.pos += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }
}

fn root_env() -> EnvRef {
    let env = Environment::new(None);

    for (name, builtin) in [
        ("+", Builtin::Add),
        ("-", Builtin::Sub),
        ("*", Builtin::Mul),
        ("/", Builtin::Div),
        ("<", Builtin::LessThan),
        (">", Builtin::GreaterThan),
        ("=", Builtin::Equal),
        ("<=", Builtin::LessThanOrEqual),
        ("not", Builtin::Not),
        ("cons", Builtin::Cons),
        ("car", Builtin::Car),
        ("cdr", Builtin::Cdr),
        ("append", Builtin::Append),
        ("list", Builtin::List),
        ("length", Builtin::Length),
        ("null?", Builtin::NullPred),
        ("pair?", Builtin::PairPred),
        ("symbol?", Builtin::SymbolPred),
        ("string?", Builtin::StringPred),
        ("number?", Builtin::NumberPred),
        ("boolean?", Builtin::BooleanPred),
        ("display", Builtin::Display),
        ("write", Builtin::Write),
        ("newline", Builtin::Newline),
        ("string-append", Builtin::StringAppend),
        ("string-length", Builtin::StringLength),
        ("substring", Builtin::Substring),
        ("string->number", Builtin::StringToNumber),
        ("number->string", Builtin::NumberToString),
        ("symbol->string", Builtin::SymbolToString),
        ("string->symbol", Builtin::StringToSymbol),
        ("string-ref", Builtin::StringRef),
        ("char?", Builtin::CharPred),
    ] {
        env.define(name, Value::Procedure(Rc::new(Procedure::Builtin(builtin))));
    }

    env
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    let pos = expr.pos();

    match expr {
        Expr::Integer(value, _) => Ok(Value::Integer(*value)),
        Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
        Expr::String(value, _) => Ok(Value::String(value.clone())),
        Expr::Symbol(name, _) => env
            .lookup(name)
            .ok_or_else(|| EvalError::UnboundSymbol(name.clone()).with_position(pos)),
        Expr::List(items, _) => eval_list(items, env).map_err(|err| err.with_position(pos)),
    }
}

fn eval_list(items: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (head, args) = items.split_first().ok_or(EvalError::InvalidApplication)?;

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => return eval_define(args, env),
            "if" => return eval_if(args, env),
            "quote" => return eval_quote(args),
            "lambda" => return eval_lambda(args, env),
            "begin" => return eval_begin(args, env),
            "cond" => return eval_cond(args, env),
            "let" => return eval_let(args, env),
            "and" => return eval_and(args, env),
            "or" => return eval_or(args, env),
            _ => {}
        }
    }

    let operator = eval_expr(head, env)?;
    apply(operator, args, env)
}

fn apply(operator: Value, args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match operator {
        Value::Procedure(procedure) => match procedure.as_ref() {
            Procedure::Builtin(builtin) => apply_builtin(*builtin, args, env),
            Procedure::Lambda(lambda) => apply_lambda(lambda, args, env),
        },
        _ => Err(EvalError::InvalidApplication),
    }
}

fn apply_builtin(builtin: Builtin, args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let values = eval_args(args, env)?;

    match builtin {
        Builtin::Add => eval_add(&values),
        Builtin::Sub => eval_sub(&values),
        Builtin::Mul => eval_mul(&values),
        Builtin::Div => eval_div(&values),
        Builtin::LessThan => eval_compare("<", &values, |left, right| left < right),
        Builtin::GreaterThan => eval_compare(">", &values, |left, right| left > right),
        Builtin::Equal => eval_compare("=", &values, |left, right| left == right),
        Builtin::LessThanOrEqual => eval_compare("<=", &values, |left, right| left <= right),
        Builtin::Not => eval_not(&values),
        Builtin::Cons => eval_cons(&values),
        Builtin::Car => eval_car(&values),
        Builtin::Cdr => eval_cdr(&values),
        Builtin::Append => eval_append(&values),
        Builtin::List => eval_list_builtin(&values),
        Builtin::Length => eval_length(&values),
        Builtin::NullPred => eval_null_pred(&values),
        Builtin::PairPred => eval_pair_pred(&values),
        Builtin::SymbolPred => eval_symbol_pred(&values),
        Builtin::StringPred => eval_string_pred(&values),
        Builtin::NumberPred => eval_number_pred(&values),
        Builtin::BooleanPred => eval_boolean_pred(&values),
        Builtin::Display => eval_display(&values, env),
        Builtin::Write => eval_write(&values, env),
        Builtin::Newline => eval_newline(&values, env),
        Builtin::StringAppend => eval_string_append(&values),
        Builtin::StringLength => eval_string_length(&values),
        Builtin::Substring => eval_substring(&values),
        Builtin::StringToNumber => eval_string_to_number(&values),
        Builtin::NumberToString => eval_number_to_string(&values),
        Builtin::SymbolToString => eval_symbol_to_string(&values),
        Builtin::StringToSymbol => eval_string_to_symbol(&values),
        Builtin::StringRef => eval_string_ref(&values),
        Builtin::CharPred => eval_char_pred(&values),
    }
}

fn apply_lambda(lambda: &Lambda, args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != lambda.params.len() {
        return Err(EvalError::WrongArgCount {
            name: lambda.name.clone().unwrap_or_else(|| "lambda".to_owned()),
            expected: format!("exactly {} arguments", lambda.params.len()),
            got: args.len(),
        });
    }

    let values = eval_args(args, env)?;
    let call_env = Environment::new(Some(lambda.env.clone()));

    for (param, value) in lambda.params.iter().cloned().zip(values) {
        call_env.define(param, value);
    }

    eval_sequence(&lambda.body, &call_env)
}

fn eval_define(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (target, body) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("define requires a target".to_owned()))?;

    match target {
        Expr::Symbol(name, _) => {
            if body.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "define".to_owned(),
                    expected: "exactly 2 forms".to_owned(),
                    got: args.len(),
                });
            }

            let value = eval_expr(&body[0], env)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature, _) => {
            let (name_expr, params_exprs) = signature
                .split_first()
                .ok_or_else(|| EvalError::Parse("define requires a function name".to_owned()))?;
            let name = expect_symbol(name_expr, "define function name")?;

            if body.is_empty() {
                return Err(EvalError::Parse(
                    "define requires a function body".to_owned(),
                ));
            }

            let params = parse_params(params_exprs, "define")?;
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda(Lambda {
                name: Some(name.clone()),
                params,
                body: body.to_vec(),
                env: env.clone(),
            })));

            env.define(name, procedure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse(
            "define target must be a symbol".to_owned(),
        )),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "if".to_owned(),
            expected: "exactly 3 arguments".to_owned(),
            got: args.len(),
        });
    }

    let condition = eval_expr(&args[0], env)?;

    if condition.is_truthy() {
        eval_expr(&args[1], env)
    } else {
        eval_expr(&args[2], env)
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "quote".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(quote_expr(&args[0]))
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(value, _) => Value::Integer(*value),
        Expr::Boolean(value, _) => Value::Boolean(*value),
        Expr::String(value, _) => Value::String(value.clone()),
        Expr::Symbol(value, _) => Value::Symbol(value.clone()),
        Expr::List(items, _) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (params_expr, body) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("lambda requires parameters".to_owned()))?;

    if body.is_empty() {
        return Err(EvalError::Parse("lambda requires a body".to_owned()));
    }

    let params = match params_expr {
        Expr::List(items, _) => parse_params(items, "lambda")?,
        _ => {
            return Err(EvalError::Parse(
                "lambda parameters must be a list".to_owned(),
            ));
        }
    };

    Ok(Value::Procedure(Rc::new(Procedure::Lambda(Lambda {
        name: None,
        params,
        body: body.to_vec(),
        env: env.clone(),
    }))))
}

fn eval_begin(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    eval_sequence(args, env)
}

fn eval_cond(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let items = match clause {
            Expr::List(items, _) => items,
            _ => {
                return Err(EvalError::Parse("cond clauses must be lists".to_owned()));
            }
        };

        let (test, body) = items
            .split_first()
            .ok_or_else(|| EvalError::Parse("cond clause cannot be empty".to_owned()))?;

        if matches!(test, Expr::Symbol(symbol, _) if symbol == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::Parse("cond else clause must be last".to_owned()));
            }
            return eval_sequence(body, env);
        }

        let value = eval_expr(test, env)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(value)
            } else {
                eval_sequence(body, env)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (head, tail) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("let requires bindings".to_owned()))?;

    match head {
        Expr::List(bindings, _) => {
            if tail.is_empty() {
                return Err(EvalError::Parse("let requires a body".to_owned()));
            }
            eval_regular_let(bindings, tail, env)
        }
        Expr::Symbol(name, _) => {
            let (bindings_expr, body) = tail
                .split_first()
                .ok_or_else(|| EvalError::Parse("let requires bindings".to_owned()))?;
            if body.is_empty() {
                return Err(EvalError::Parse("let requires a body".to_owned()));
            }
            let bindings = match bindings_expr {
                Expr::List(bindings, _) => bindings,
                _ => {
                    return Err(EvalError::Parse("let bindings must be a list".to_owned()));
                }
            };
            eval_named_let(name, bindings, body, env)
        }
        _ => Err(EvalError::Parse("let requires bindings".to_owned())),
    }
}

fn eval_regular_let(bindings: &[Expr], body: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let bindings = parse_bindings(bindings, "let")?;
    let values = eval_binding_values(&bindings, env)?;
    let scope = Environment::new(Some(env.clone()));

    for ((name, _), value) in bindings.into_iter().zip(values) {
        scope.define(name, value);
    }

    eval_sequence(body, &scope)
}

fn eval_named_let(
    name: &str,
    bindings: &[Expr],
    body: &[Expr],
    env: &EnvRef,
) -> Result<Value, EvalError> {
    let bindings = parse_bindings(bindings, "let")?;
    let values = eval_binding_values(&bindings, env)?;
    let params = bindings
        .iter()
        .map(|(param, _)| param.clone())
        .collect::<Vec<_>>();
    let recursive_env = Environment::new(Some(env.clone()));

    recursive_env.define(
        name.to_owned(),
        Value::Procedure(Rc::new(Procedure::Lambda(Lambda {
            name: Some(name.to_owned()),
            params: params.clone(),
            body: body.to_vec(),
            env: recursive_env.clone(),
        }))),
    );

    let call_env = Environment::new(Some(recursive_env));
    for (param, value) in params.into_iter().zip(values) {
        call_env.define(param, value);
    }

    eval_sequence(body, &call_env)
}

fn parse_params(params: &[Expr], form: &str) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|expr| expect_symbol(expr, &format!("{form} parameter")))
        .collect()
}

fn parse_bindings(bindings: &[Expr], form: &str) -> Result<Vec<(String, Expr)>, EvalError> {
    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List(items, _) if items.len() == 2 => Ok((
                expect_symbol(&items[0], &format!("{form} binding name"))?,
                items[1].clone(),
            )),
            Expr::List(_, _) => Err(EvalError::Parse(format!(
                "{form} bindings must contain exactly 2 forms"
            ))),
            _ => Err(EvalError::Parse(format!("{form} bindings must be lists"))),
        })
        .collect()
}

fn eval_binding_values(bindings: &[(String, Expr)], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    bindings
        .iter()
        .map(|(_, expr)| eval_expr(expr, env))
        .collect()
}

fn expect_symbol(expr: &Expr, context: &str) -> Result<String, EvalError> {
    match expr {
        Expr::Symbol(name, _) => Ok(name.clone()),
        _ => Err(EvalError::Parse(format!("{context} must be a symbol"))),
    }
}

fn eval_sequence(exprs: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;

    for expr in exprs {
        last = eval_expr(expr, env)?;
    }

    Ok(last)
}

fn eval_args(args: &[Expr], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    args.iter().map(|expr| eval_expr(expr, env)).collect()
}

fn eval_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum = 0_i64;

    for value in eval_integer_args("+", args)? {
        sum += value;
    }

    Ok(Value::Integer(sum))
}

fn eval_sub(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_integer_args("-", args)?;

    match values.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            name: "-".to_owned(),
            expected: "at least 1 argument".to_owned(),
            got: 0,
        }),
        [value] => Ok(Value::Integer(-*value)),
        [first, rest @ ..] => Ok(Value::Integer(
            rest.iter().fold(*first, |acc, value| acc - *value),
        )),
    }
}

fn eval_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product = 1_i64;

    for value in eval_integer_args("*", args)? {
        product *= value;
    }

    Ok(Value::Integer(product))
}

fn eval_div(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_integer_args("/", args)?;

    let (first, rest) = values
        .split_first()
        .ok_or_else(|| EvalError::WrongArgCount {
            name: "/".to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: 0,
        })?;

    if rest.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "/".to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: 1,
        });
    }

    let mut quotient = *first;

    for value in rest {
        if *value == 0 {
            return Err(EvalError::DivisionByZero);
        }
        quotient /= *value;
    }

    Ok(Value::Integer(quotient))
}

fn eval_compare<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = eval_integer_args(name, args)?;
    let result = values
        .windows(2)
        .all(|window| predicate(window[0], window[1]));

    Ok(Value::Boolean(result))
}

fn eval_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn eval_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "cons".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let mut tail = expect_list("cons", &args[1])?.to_vec();
    tail.insert(0, args[0].clone());
    Ok(Value::List(tail))
}

fn eval_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "car".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let items = expect_pair("car", &args[0])?;
    Ok(items[0].clone())
}

fn eval_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "cdr".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let items = expect_pair("cdr", &args[0])?;
    Ok(Value::List(items[1..].to_vec()))
}

fn eval_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut combined = Vec::new();

    for value in args {
        combined.extend(expect_list("append", value)?.iter().cloned());
    }

    Ok(Value::List(combined))
}

fn eval_list_builtin(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn eval_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "length".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let items = expect_list("length", &args[0])?;
    Ok(Value::Integer(items.len() as i64))
}

fn eval_null_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "null?".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(
        matches!(&args[0], Value::List(items) if items.is_empty()),
    ))
}

fn eval_pair_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(
        args,
        "pair?",
        |value| matches!(value, Value::List(items) if !items.is_empty()),
    )
}

fn eval_symbol_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "symbol?", |value| matches!(value, Value::Symbol(_)))
}

fn eval_string_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "string?", |value| matches!(value, Value::String(_)))
}

fn eval_number_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "number?", |value| matches!(value, Value::Integer(_)))
}

fn eval_boolean_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "boolean?", |value| matches!(value, Value::Boolean(_)))
}

fn eval_display(args: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "display".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    env.output.borrow_mut().push_str(&args[0].render_display());
    Ok(Value::Void)
}

fn eval_write(args: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "write".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    env.output.borrow_mut().push_str(&args[0].render());
    Ok(Value::Void)
}

fn eval_newline(args: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "newline".to_owned(),
            expected: "exactly 0 arguments".to_owned(),
            got: args.len(),
        });
    }

    env.output.borrow_mut().push('\n');
    Ok(Value::Void)
}

fn eval_string_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut combined = String::new();

    for value in args {
        combined.push_str(expect_string("string-append", value)?);
    }

    Ok(Value::String(combined))
}

fn eval_string_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string-length".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let value = expect_string("string-length", &args[0])?;
    Ok(Value::Integer(value.chars().count() as i64))
}

fn eval_substring(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "substring".to_owned(),
            expected: "exactly 3 arguments".to_owned(),
            got: args.len(),
        });
    }

    let string = expect_string("substring", &args[0])?;
    let start = args[1].as_integer("substring")?;
    let end = args[2].as_integer("substring")?;
    let chars = string.chars().collect::<Vec<_>>();
    let len = chars.len();

    if start < 0 || end < start || end as usize > len {
        return Err(EvalError::InvalidRange {
            name: "substring".to_owned(),
            start,
            end,
            len,
        });
    }

    Ok(Value::String(
        chars[start as usize..end as usize].iter().collect(),
    ))
}

fn eval_string_to_number(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string->number".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let value = expect_string("string->number", &args[0])?;
    Ok(match value.parse::<i64>() {
        Ok(number) => Value::Integer(number),
        Err(_) => Value::Boolean(false),
    })
}

fn eval_number_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "number->string".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::String(
        args[0].as_integer("number->string")?.to_string(),
    ))
}

fn eval_symbol_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "symbol->string".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::String(
        expect_symbol_value("symbol->string", &args[0])?.to_owned(),
    ))
}

fn eval_string_to_symbol(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string->symbol".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Symbol(
        expect_string("string->symbol", &args[0])?.to_owned(),
    ))
}

fn eval_string_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "string-ref".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let string = expect_string("string-ref", &args[0])?;
    let index = args[1].as_integer("string-ref")?;
    let len = string.chars().count();

    if index < 0 {
        return Err(EvalError::IndexOutOfBounds {
            name: "string-ref".to_owned(),
            index,
            len,
        });
    }

    let Some(ch) = string.chars().nth(index as usize) else {
        return Err(EvalError::IndexOutOfBounds {
            name: "string-ref".to_owned(),
            index,
            len,
        });
    };

    Ok(Value::Char(ch))
}

fn eval_char_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "char?", |value| matches!(value, Value::Char(_)))
}

fn eval_and(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for expr in args {
        let value = eval_expr(expr, env)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval_expr(expr, env)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_integer_args(name: &str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|value| value.as_integer(name)).collect()
}

fn eval_type_predicate<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(predicate(&args[0])))
}

fn expect_string<'a>(name: &str, value: &'a Value) -> Result<&'a str, EvalError> {
    match value {
        Value::String(value) => Ok(value),
        _ => Err(EvalError::ExpectedString {
            name: name.to_owned(),
        }),
    }
}

fn expect_symbol_value<'a>(name: &str, value: &'a Value) -> Result<&'a str, EvalError> {
    match value {
        Value::Symbol(value) => Ok(value),
        _ => Err(EvalError::ExpectedSymbol {
            name: name.to_owned(),
        }),
    }
}

fn expect_list<'a>(name: &str, value: &'a Value) -> Result<&'a [Value], EvalError> {
    match value {
        Value::List(items) => Ok(items),
        _ => Err(EvalError::ExpectedList {
            name: name.to_owned(),
        }),
    }
}

fn expect_pair<'a>(name: &str, value: &'a Value) -> Result<&'a [Value], EvalError> {
    let items = expect_list(name, value)?;
    if items.is_empty() {
        return Err(EvalError::ExpectedPair {
            name: name.to_owned(),
        });
    }
    Ok(items)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
fn eval_program(input: &str) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_program()?;
    let env = root_env();
    let last = eval_sequence(&exprs, &env)
        .map_err(|err| err.with_position(SourcePos { line: 1, col: 1 }))?;
    let output = env.output.borrow().clone();
    Ok((last, output))
}

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (last, _) = eval_program(input)?;
    Ok(last.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (last, output) = eval_program(input)?;
    Ok((last.render(), output))
}

#[cfg(test)]
mod tests;
