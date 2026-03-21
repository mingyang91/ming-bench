pub mod error;

pub use error::EvalError;

use error::SourcePos;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Int(i64, SourcePos),
    Bool(bool, SourcePos),
    String(String, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    fn pos(&self) -> SourcePos {
        match self {
            Self::Int(_, pos)
            | Self::Bool(_, pos)
            | Self::String(_, pos)
            | Self::Symbol(_, pos)
            | Self::List(_, pos) => *pos,
        }
    }
}

#[derive(Clone, Copy, Debug)]
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
    IsNull,
    List,
    Length,
    IsString,
    IsNumber,
    IsBoolean,
    IsPair,
    IsSymbol,
}

type EnvRef = Rc<Environment>;

#[derive(Debug)]
struct Environment {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, Value>>,
}

#[derive(Clone, Debug)]
struct Procedure {
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone, Debug)]
enum Value {
    Int(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Builtin(Builtin),
    Procedure(Rc<Procedure>),
    Void,
}

impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Self::Int(_) => "number",
            Self::Bool(_) => "boolean",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::Builtin(_) | Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn render(&self) -> String {
        match self {
            Self::Int(value) => value.to_string(),
            Self::Bool(true) => "#t".into(),
            Self::Bool(false) => "#f".into(),
            Self::String(value) => render_string(value),
            Self::Symbol(value) => value.clone(),
            Self::List(items) => render_list(items),
            Self::Builtin(_) | Self::Procedure(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
        }
    }
}

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

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return Some(value);
        }

        self.parent.as_ref().and_then(|parent| parent.lookup(name))
    }
}

struct Parser<'a> {
    input: &'a str,
    offset: usize,
    line: usize,
    col: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            offset: 0,
            line: 1,
            col: 1,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_whitespace();

        while !self.is_eof() {
            exprs.push(self.parse_expr()?);
            self.skip_whitespace();
        }

        if exprs.is_empty() {
            return Err(syntax_error(self.current_pos(), "expected expression"));
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace();

        let pos = self.current_pos();
        let next = self.peek_char().ok_or_else(|| unexpected_eof(pos))?;
        match next {
            '(' => self.parse_list(),
            ')' => Err(syntax_error(pos, "unexpected ')'")),
            '\'' => self.parse_quote_sugar(),
            '"' => self.parse_string(),
            _ => self.parse_atom(),
        }
    }

    fn parse_quote_sugar(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.expect_char('\'')?;
        Ok(Expr::List(
            vec![Expr::Symbol("quote".into(), pos), self.parse_expr()?],
            pos,
        ))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_whitespace();

            match self.peek_char() {
                Some(')') => {
                    self.advance_char();
                    break;
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(unexpected_eof(self.current_pos())),
            }
        }

        Ok(Expr::List(items, pos))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        self.expect_char('"')?;
        let mut value = String::new();

        while let Some(ch) = self.advance_char() {
            match ch {
                '"' => return Ok(Expr::String(value, pos)),
                '\\' => {
                    let escaped = self
                        .advance_char()
                        .ok_or_else(|| unexpected_eof(self.current_pos()))?;
                    let resolved = match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => {
                            return Err(syntax_error(
                                self.current_pos(),
                                format!("unsupported string escape: \\{other}"),
                            ));
                        }
                    };
                    value.push(resolved);
                }
                other => value.push(other),
            }
        }

        Err(unexpected_eof(self.current_pos()))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_pos();
        let start = self.offset;

        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')') {
                break;
            }
            self.advance_char();
        }

        let token = &self.input[start..self.offset];
        if token.is_empty() {
            return Err(syntax_error(pos, "expected token"));
        }

        match token {
            "#t" => Ok(Expr::Bool(true, pos)),
            "#f" => Ok(Expr::Bool(false, pos)),
            _ => match token.parse::<i64>() {
                Ok(value) => Ok(Expr::Int(value, pos)),
                Err(_) => Ok(Expr::Symbol(token.into(), pos)),
            },
        }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
            self.advance_char();
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        let pos = self.current_pos();
        match self.advance_char() {
            Some(found) if found == expected => Ok(()),
            Some(found) => Err(syntax_error(
                pos,
                format!("expected '{expected}', found '{found}'"),
            )),
            None => Err(unexpected_eof(pos)),
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.offset..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.offset += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.input.len()
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos {
            line: self.line,
            col: self.col,
        }
    }
}

fn syntax_error(pos: SourcePos, message: impl Into<String>) -> EvalError {
    EvalError::SyntaxError {
        pos,
        message: message.into(),
    }
}

fn unexpected_eof(pos: SourcePos) -> EvalError {
    EvalError::UnexpectedEof { pos }
}

fn unbound_variable(pos: SourcePos, name: impl Into<String>) -> EvalError {
    EvalError::UnboundVariable {
        pos,
        name: name.into(),
    }
}

fn wrong_arity(
    pos: SourcePos,
    name: impl Into<String>,
    expected: impl Into<String>,
    got: usize,
) -> EvalError {
    EvalError::WrongArity {
        pos,
        name: name.into(),
        expected: expected.into(),
        got,
    }
}

fn type_error(pos: SourcePos, expected: &'static str, found: &'static str) -> EvalError {
    EvalError::TypeError {
        pos,
        expected,
        found,
    }
}

fn division_by_zero(pos: SourcePos) -> EvalError {
    EvalError::DivisionByZero { pos }
}

fn not_callable(pos: SourcePos, found: &'static str) -> EvalError {
    EvalError::NotCallable { pos, found }
}

fn expr_pos_or(parts: &[Expr], default: SourcePos) -> SourcePos {
    parts.first().map(Expr::pos).unwrap_or(default)
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
    let mut parser = Parser::new(input);
    let exprs = parser.parse_program()?;
    let env = default_env();
    let mut last_value = None;

    for expr in exprs {
        last_value = Some(eval_expr(&expr, &env)?);
    }

    last_value
        .map(|value| value.render())
        .ok_or_else(|| syntax_error(SourcePos { line: 1, col: 1 }, "expected expression"))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    eval_str(input).map(|value| (value, String::new()))
}

fn default_env() -> EnvRef {
    let env = Environment::new(None);

    for (name, builtin) in [
        ("+", Builtin::Add),
        ("-", Builtin::Sub),
        ("*", Builtin::Mul),
        ("/", Builtin::Div),
        ("<", Builtin::LessThan),
        (">", Builtin::GreaterThan),
        ("=", Builtin::Equal),
        ("<=", Builtin::LessEqual),
        ("not", Builtin::Not),
        ("cons", Builtin::Cons),
        ("car", Builtin::Car),
        ("cdr", Builtin::Cdr),
        ("null?", Builtin::IsNull),
        ("list", Builtin::List),
        ("length", Builtin::Length),
        ("string?", Builtin::IsString),
        ("number?", Builtin::IsNumber),
        ("boolean?", Builtin::IsBoolean),
        ("pair?", Builtin::IsPair),
        ("symbol?", Builtin::IsSymbol),
    ] {
        env.define(name, Value::Builtin(builtin));
    }

    env
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Int(value, _) => Ok(Value::Int(*value)),
        Expr::Bool(value, _) => Ok(Value::Bool(*value)),
        Expr::String(value, _) => Ok(Value::String(value.clone())),
        Expr::Symbol(name, pos) => env
            .lookup(name)
            .ok_or_else(|| unbound_variable(*pos, name.clone())),
        Expr::List(items, pos) => eval_list(items, *pos, env),
    }
}

fn eval_list(items: &[Expr], list_pos: SourcePos, env: &EnvRef) -> Result<Value, EvalError> {
    let Some(head) = items.first() else {
        return Err(syntax_error(list_pos, "cannot evaluate empty list"));
    };

    if let Expr::Symbol(name, pos) = head {
        match name.as_str() {
            "define" => return eval_define(&items[1..], *pos, env),
            "if" => return eval_if(&items[1..], *pos, env),
            "quote" => return eval_quote(&items[1..], *pos),
            "lambda" => return eval_lambda(&items[1..], *pos, env),
            "and" => return eval_and(&items[1..], env),
            "or" => return eval_or(&items[1..], env),
            "let" => return eval_let(&items[1..], *pos, env),
            "begin" => return eval_begin(&items[1..], env),
            "cond" => return eval_cond(&items[1..], *pos, env),
            _ => {}
        }
    }

    let operator = eval_expr(head, env)?;
    let args = eval_arg_values(&items[1..], env)?;
    apply_value(operator, &args, head.pos())
}

fn eval_define(parts: &[Expr], pos: SourcePos, env: &EnvRef) -> Result<Value, EvalError> {
    match parts {
        [Expr::Symbol(name, _), value_expr] => {
            let value = eval_expr(value_expr, env)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List(signature, signature_pos), body @ ..] => {
            if body.is_empty() {
                return Err(syntax_error(pos, "define requires a function body"));
            }

            let Some((name_expr, params_exprs)) = signature.split_first() else {
                return Err(syntax_error(
                    *signature_pos,
                    "define requires a function name",
                ));
            };

            let Expr::Symbol(name, _) = name_expr else {
                return Err(syntax_error(
                    name_expr.pos(),
                    "define function name must be a symbol",
                ));
            };

            let params = parse_param_names(params_exprs)?;
            let procedure = Value::Procedure(Rc::new(Procedure {
                params,
                body: body.to_vec(),
                env: env.clone(),
            }));
            env.define(name.clone(), procedure);
            Ok(Value::Void)
        }
        _ => Err(syntax_error(expr_pos_or(parts, pos), "malformed define")),
    }
}

fn eval_if(parts: &[Expr], pos: SourcePos, env: &EnvRef) -> Result<Value, EvalError> {
    let [condition, consequent, alternate] = parts else {
        return Err(wrong_arity(pos, "if", "exactly 3", parts.len()));
    };

    if eval_expr(condition, env)?.is_truthy() {
        eval_expr(consequent, env)
    } else {
        eval_expr(alternate, env)
    }
}

fn eval_quote(parts: &[Expr], pos: SourcePos) -> Result<Value, EvalError> {
    let [datum] = parts else {
        return Err(wrong_arity(pos, "quote", "exactly 1", parts.len()));
    };

    Ok(quote_expr(datum))
}

fn eval_lambda(parts: &[Expr], pos: SourcePos, env: &EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = parts.split_first() else {
        return Err(wrong_arity(pos, "lambda", "at least 2", parts.len()));
    };

    if body.is_empty() {
        return Err(syntax_error(pos, "lambda requires a body"));
    }

    let Expr::List(params_exprs, _) = params_expr else {
        return Err(syntax_error(
            params_expr.pos(),
            "lambda parameters must be a list",
        ));
    };

    let params = parse_param_names(params_exprs)?;
    Ok(Value::Procedure(Rc::new(Procedure {
        params,
        body: body.to_vec(),
        env: env.clone(),
    })))
}

fn eval_let(parts: &[Expr], pos: SourcePos, env: &EnvRef) -> Result<Value, EvalError> {
    let Some((bindings_expr, body)) = parts.split_first() else {
        return Err(wrong_arity(pos, "let", "at least 2", parts.len()));
    };

    if body.is_empty() {
        return Err(syntax_error(pos, "let requires a body"));
    }

    let Expr::List(bindings, _) = bindings_expr else {
        return Err(syntax_error(
            bindings_expr.pos(),
            "let bindings must be a list",
        ));
    };

    let evaluated_bindings = bindings
        .iter()
        .map(|binding| {
            let Expr::List(parts, _) = binding else {
                return Err(syntax_error(binding.pos(), "let binding must be a list"));
            };

            let [Expr::Symbol(name, _), value_expr] = parts.as_slice() else {
                return Err(syntax_error(
                    binding.pos(),
                    "let binding must contain a name and value",
                ));
            };

            Ok((name.clone(), eval_expr(value_expr, env)?))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let local_env = Environment::new(Some(env.clone()));
    for (name, value) in evaluated_bindings {
        local_env.define(name, value);
    }

    eval_body(body, &local_env)
}

fn eval_begin(parts: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    eval_body(parts, env)
}

fn eval_cond(clauses: &[Expr], _pos: SourcePos, env: &EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items, _) = clause else {
            return Err(syntax_error(clause.pos(), "cond clause must be a list"));
        };

        let Some((test_expr, body)) = items.split_first() else {
            return Err(syntax_error(clause.pos(), "cond clause cannot be empty"));
        };

        if let Expr::Symbol(name, _) = test_expr {
            if name == "else" {
                if index + 1 != clauses.len() {
                    return Err(syntax_error(
                        test_expr.pos(),
                        "cond else clause must be last",
                    ));
                }

                if body.is_empty() {
                    return Err(syntax_error(
                        clause.pos(),
                        "cond else clause requires a body",
                    ));
                }

                return eval_body(body, env);
            }
        }

        let test_value = eval_expr(test_expr, env)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_body(body, env)
            };
        }
    }

    Ok(Value::Void)
}

fn parse_param_names(params: &[Expr]) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|param| match param {
            Expr::Symbol(name, _) => Ok(name.clone()),
            _ => Err(syntax_error(param.pos(), "parameter names must be symbols")),
        })
        .collect()
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Int(value, _) => Value::Int(*value),
        Expr::Bool(value, _) => Value::Bool(*value),
        Expr::String(value, _) => Value::String(value.clone()),
        Expr::Symbol(value, _) => Value::Symbol(value.clone()),
        Expr::List(items, _) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_arg_values(args: &[Expr], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    args.iter().map(|expr| eval_expr(expr, env)).collect()
}

fn apply_value(operator: Value, args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match operator {
        Value::Builtin(builtin) => apply_builtin(builtin, args, pos),
        Value::Procedure(procedure) => apply_procedure(&procedure, args, pos),
        other => Err(not_callable(pos, other.type_name())),
    }
}

fn apply_builtin(builtin: Builtin, args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match builtin {
        Builtin::Add => eval_add(args, pos),
        Builtin::Sub => eval_sub(args, pos),
        Builtin::Mul => eval_mul(args, pos),
        Builtin::Div => eval_div(args, pos),
        Builtin::LessThan => eval_compare(args, "<", pos, |left, right| left < right),
        Builtin::GreaterThan => eval_compare(args, ">", pos, |left, right| left > right),
        Builtin::Equal => eval_compare(args, "=", pos, |left, right| left == right),
        Builtin::LessEqual => eval_compare(args, "<=", pos, |left, right| left <= right),
        Builtin::Not => eval_not(args, pos),
        Builtin::Cons => eval_cons(args, pos),
        Builtin::Car => eval_car(args, pos),
        Builtin::Cdr => eval_cdr(args, pos),
        Builtin::IsNull => eval_null(args, pos),
        Builtin::List => eval_list_builtin(args, pos),
        Builtin::Length => eval_length(args, pos),
        Builtin::IsString => eval_type_predicate(args, "string?", pos, |value| {
            matches!(value, Value::String(_))
        }),
        Builtin::IsNumber => {
            eval_type_predicate(args, "number?", pos, |value| matches!(value, Value::Int(_)))
        }
        Builtin::IsBoolean => eval_type_predicate(args, "boolean?", pos, |value| {
            matches!(value, Value::Bool(_))
        }),
        Builtin::IsPair => eval_type_predicate(
            args,
            "pair?",
            pos,
            |value| matches!(value, Value::List(items) if !items.is_empty()),
        ),
        Builtin::IsSymbol => eval_type_predicate(args, "symbol?", pos, |value| {
            matches!(value, Value::Symbol(_))
        }),
    }
}

fn apply_procedure(
    procedure: &Procedure,
    args: &[Value],
    pos: SourcePos,
) -> Result<Value, EvalError> {
    if args.len() != procedure.params.len() {
        return Err(wrong_arity(
            pos,
            "procedure",
            procedure.params.len().to_string(),
            args.len(),
        ));
    }

    let local_env = Environment::new(Some(procedure.env.clone()));
    for (param, arg) in procedure.params.iter().zip(args) {
        local_env.define(param.clone(), arg.clone());
    }

    eval_body(&procedure.body, &local_env)
}

fn eval_body(body: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;

    for expr in body {
        last = eval_expr(expr, env)?;
    }

    Ok(last)
}

fn eval_add(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args, pos)?;
    Ok(Value::Int(numbers.into_iter().sum()))
}

fn eval_sub(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args, pos)?;
    match numbers.as_slice() {
        [] => Err(wrong_arity(pos, "-", "at least 1", 0)),
        [value] => Ok(Value::Int(-value)),
        [first, rest @ ..] => Ok(Value::Int(
            rest.iter().fold(*first, |acc, value| acc - value),
        )),
    }
}

fn eval_mul(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args, pos)?;
    Ok(Value::Int(numbers.into_iter().product()))
}

fn eval_div(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args, pos)?;
    let [first, rest @ ..] = numbers.as_slice() else {
        return Err(wrong_arity(pos, "/", "at least 2", numbers.len()));
    };

    if rest.is_empty() {
        return Err(wrong_arity(pos, "/", "at least 2", 1));
    }

    let mut result = *first;
    for value in rest {
        if *value == 0 {
            return Err(division_by_zero(pos));
        }
        result /= value;
    }

    Ok(Value::Int(result))
}

fn eval_compare<F>(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let numbers = eval_number_args(args, pos)?;
    if numbers.len() < 2 {
        return Err(wrong_arity(pos, name, "at least 2", numbers.len()));
    }

    for pair in numbers.windows(2) {
        if !predicate(pair[0], pair[1]) {
            return Ok(Value::Bool(false));
        }
    }

    Ok(Value::Bool(true))
}

fn eval_not(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(wrong_arity(pos, "not", "exactly 1", args.len()));
    }

    Ok(Value::Bool(!args[0].is_truthy()))
}

fn eval_cons(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(wrong_arity(pos, "cons", "exactly 2", args.len()));
    };

    let Value::List(items) = tail else {
        return Err(type_error(pos, "list", tail.type_name()));
    };

    let mut result = Vec::with_capacity(items.len() + 1);
    result.push(head.clone());
    result.extend(items.iter().cloned());
    Ok(Value::List(result))
}

fn eval_car(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "car", "exactly 1", args.len()));
    };

    match value {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        other => Err(type_error(pos, "pair", other.type_name())),
    }
}

fn eval_cdr(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "cdr", "exactly 1", args.len()));
    };

    match value {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        other => Err(type_error(pos, "pair", other.type_name())),
    }
}

fn eval_null(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "null?", "exactly 1", args.len()));
    };

    Ok(Value::Bool(
        matches!(value, Value::List(items) if items.is_empty()),
    ))
}

fn eval_list_builtin(args: &[Value], _pos: SourcePos) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn eval_length(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "length", "exactly 1", args.len()));
    };

    match value {
        Value::List(items) => Ok(Value::Int(items.len() as i64)),
        other => Err(type_error(pos, "list", other.type_name())),
    }
}

fn eval_type_predicate<F>(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    let [value] = args else {
        return Err(wrong_arity(pos, name, "exactly 1", args.len()));
    };

    Ok(Value::Bool(predicate(value)))
}

fn eval_and(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);

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

    Ok(Value::Bool(false))
}

fn eval_number_args(args: &[Value], pos: SourcePos) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Int(number) => Ok(*number),
            other => Err(type_error(pos, "number", other.type_name())),
        })
        .collect()
}

fn render_list(items: &[Value]) -> String {
    let mut rendered = String::from("(");

    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&item.render());
    }

    rendered.push(')');
    rendered
}

fn render_string(value: &str) -> String {
    let mut rendered = String::with_capacity(value.len() + 2);
    rendered.push('"');

    for ch in value.chars() {
        match ch {
            '"' => rendered.push_str("\\\""),
            '\\' => rendered.push_str("\\\\"),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            other => rendered.push(other),
        }
    }

    rendered.push('"');
    rendered
}

#[cfg(test)]
mod tests;
