use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub mod error;

pub use error::EvalError;

type EvalResult<T> = Result<T, EvalError>;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Position {
    line: usize,
    col: usize,
}

#[derive(Clone, Debug)]
enum Expr {
    Number(i64, Position),
    Boolean(bool, Position),
    String(String, Position),
    Char(char, Position),
    Symbol(String, Position),
    List(Vec<Expr>, Position),
}

impl Expr {
    fn pos(&self) -> Position {
        match self {
            Self::Number(_, pos)
            | Self::Boolean(_, pos)
            | Self::String(_, pos)
            | Self::Char(_, pos)
            | Self::Symbol(_, pos)
            | Self::List(_, pos) => *pos,
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
    StringCopy,
    StringSet,
    CharPred,
    Apply,
}

impl Builtin {
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
            Self::NullPred => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::Append => "append",
            Self::StringPred => "string?",
            Self::NumberPred => "number?",
            Self::BooleanPred => "boolean?",
            Self::PairPred => "pair?",
            Self::SymbolPred => "symbol?",
            Self::Display => "display",
            Self::Write => "write",
            Self::Newline => "newline",
            Self::StringAppend => "string-append",
            Self::StringLength => "string-length",
            Self::Substring => "substring",
            Self::StringToNumber => "string->number",
            Self::NumberToString => "number->string",
            Self::SymbolToString => "symbol->string",
            Self::StringToSymbol => "string->symbol",
            Self::StringRef => "string-ref",
            Self::StringCopy => "string-copy",
            Self::StringSet => "string-set!",
            Self::CharPred => "char?",
            Self::Apply => "apply",
        }
    }
}

#[derive(Clone)]
enum Value {
    Number(i64),
    Boolean(bool),
    String(Rc<RefCell<SchemeString>>),
    Symbol(String),
    Pair(Rc<Pair>),
    Char(char),
    EmptyList,
    Void,
    Builtin(Builtin),
    Closure(Rc<Closure>),
}

struct SchemeString {
    chars: Vec<char>,
    mutable: bool,
}

struct Pair {
    car: Value,
    cdr: Value,
}

struct Closure {
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Expr>,
    env: Rc<Environment>,
    name: Option<String>,
}

struct Environment {
    parent: Option<Rc<Environment>>,
    bindings: RefCell<HashMap<String, Rc<RefCell<Value>>>>,
}

impl Environment {
    fn new(parent: Option<Rc<Environment>>) -> Self {
        Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
        }
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.bindings
            .borrow_mut()
            .insert(name.into(), Rc::new(RefCell::new(value)));
    }

    fn lookup(&self, name: &str, pos: Position) -> EvalResult<Value> {
        Ok(self.lookup_cell(name, pos)?.borrow().clone())
    }

    fn assign(&self, name: &str, value: Value, pos: Position) -> EvalResult<()> {
        let cell = self.lookup_cell(name, pos)?;
        *cell.borrow_mut() = value;
        Ok(())
    }

    fn lookup_cell(&self, name: &str, pos: Position) -> EvalResult<Rc<RefCell<Value>>> {
        if let Some(cell) = self.bindings.borrow().get(name).cloned() {
            return Ok(cell);
        }

        if let Some(parent) = &self.parent {
            return parent.lookup_cell(name, pos);
        }

        Err(error_at(pos, format!("unbound symbol: {name}")))
    }
}

#[derive(Default)]
struct EvalState {
    output: String,
}

#[derive(Clone)]
enum TokenKind {
    LParen,
    RParen,
    Quote,
    Number(i64),
    Boolean(bool),
    String(String),
    Char(char),
    Symbol(String),
}

#[derive(Clone)]
struct Token {
    kind: TokenKind,
    pos: Position,
}

struct TokenStream {
    tokens: Vec<Token>,
    eof_pos: Position,
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
    eof_pos: Position,
}

impl Parser {
    fn new(stream: TokenStream) -> Self {
        Self {
            tokens: stream.tokens,
            index: 0,
            eof_pos: stream.eof_pos,
        }
    }

    fn parse_program(&mut self) -> EvalResult<Vec<Expr>> {
        let mut expressions = Vec::new();

        while !self.is_at_end() {
            expressions.push(self.parse_expr()?);
        }

        Ok(expressions)
    }

    fn parse_expr(&mut self) -> EvalResult<Expr> {
        let token = self
            .advance()
            .ok_or_else(|| error_at(self.eof_pos, "unexpected end of input"))?;

        match token.kind {
            TokenKind::Number(value) => Ok(Expr::Number(value, token.pos)),
            TokenKind::Boolean(value) => Ok(Expr::Boolean(value, token.pos)),
            TokenKind::String(value) => Ok(Expr::String(value, token.pos)),
            TokenKind::Char(value) => Ok(Expr::Char(value, token.pos)),
            TokenKind::Symbol(value) => Ok(Expr::Symbol(value, token.pos)),
            TokenKind::Quote => Ok(Expr::List(
                vec![
                    Expr::Symbol("quote".into(), token.pos),
                    self.parse_expr()?,
                ],
                token.pos,
            )),
            TokenKind::LParen => {
                let mut elements = Vec::new();

                loop {
                    match self.peek() {
                        None => return Err(error_at(self.eof_pos, "unterminated list")),
                        Some(next) if matches!(&next.kind, TokenKind::RParen) => {
                            self.advance();
                            return Ok(Expr::List(elements, token.pos));
                        }
                        Some(_) => elements.push(self.parse_expr()?),
                    }
                }
            }
            TokenKind::RParen => Err(error_at(token.pos, "unexpected )")),
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn advance(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.index).cloned()?;
        self.index += 1;
        Some(token)
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.tokens.len()
    }
}

struct FormalParameters {
    params: Vec<String>,
    rest_param: Option<String>,
}

struct Binding {
    name: String,
    value_expr: Expr,
}

const BUILTINS: &[Builtin] = &[
    Builtin::Add,
    Builtin::Sub,
    Builtin::Mul,
    Builtin::Div,
    Builtin::LessThan,
    Builtin::GreaterThan,
    Builtin::Equal,
    Builtin::LessEqual,
    Builtin::Not,
    Builtin::Cons,
    Builtin::Car,
    Builtin::Cdr,
    Builtin::NullPred,
    Builtin::List,
    Builtin::Length,
    Builtin::Append,
    Builtin::StringPred,
    Builtin::NumberPred,
    Builtin::BooleanPred,
    Builtin::PairPred,
    Builtin::SymbolPred,
    Builtin::Display,
    Builtin::Write,
    Builtin::Newline,
    Builtin::StringAppend,
    Builtin::StringLength,
    Builtin::Substring,
    Builtin::StringToNumber,
    Builtin::NumberToString,
    Builtin::SymbolToString,
    Builtin::StringToSymbol,
    Builtin::StringRef,
    Builtin::StringCopy,
    Builtin::StringSet,
    Builtin::CharPred,
    Builtin::Apply,
];

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    Ok(evaluate_input(input)?.0)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    evaluate_input(input)
}

fn evaluate_input(input: &str) -> EvalResult<(String, String)> {
    let stream = tokenize(input)?;
    let mut parser = Parser::new(stream);
    let expressions = parser.parse_program()?;

    if expressions.is_empty() {
        return Err(error_at(
            Position { line: 1, col: 1 },
            "expected at least one expression",
        ));
    }

    let env = create_global_environment();
    let mut state = EvalState::default();
    let mut result = Value::Void;

    for expression in &expressions {
        result = evaluate(expression, Rc::clone(&env), &mut state)?;
    }

    Ok((format_value(&result), state.output))
}

fn create_global_environment() -> Rc<Environment> {
    let env = Rc::new(Environment::new(None));

    for builtin in BUILTINS {
        env.define(builtin.name(), Value::Builtin(*builtin));
    }

    env
}

fn evaluate(expression: &Expr, env: Rc<Environment>, state: &mut EvalState) -> EvalResult<Value> {
    match expression {
        Expr::Number(value, _) => Ok(Value::Number(*value)),
        Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
        Expr::String(value, _) => Ok(Value::String(make_string(string_to_chars(value), false))),
        Expr::Char(value, _) => Ok(Value::Char(*value)),
        Expr::Symbol(name, pos) => env.lookup(name, *pos),
        Expr::List(elements, pos) => evaluate_list(elements, *pos, env, state),
    }
}

fn evaluate_list(
    elements: &[Expr],
    pos: Position,
    env: Rc<Environment>,
    state: &mut EvalState,
) -> EvalResult<Value> {
    if elements.is_empty() {
        return Err(error_at(pos, "cannot evaluate empty list"));
    }

    let operator_expr = &elements[0];
    let argument_exprs = &elements[1..];

    if let Expr::Symbol(name, operator_pos) = operator_expr {
        match name.as_str() {
            "and" => return evaluate_and(argument_exprs, env, state),
            "or" => return evaluate_or(argument_exprs, env, state),
            "if" => return evaluate_if(argument_exprs, env, *operator_pos, state),
            "define" => return evaluate_define(argument_exprs, env, *operator_pos, state),
            "quote" => return evaluate_quote(argument_exprs, *operator_pos),
            "lambda" => return evaluate_lambda(argument_exprs, env, *operator_pos),
            "begin" => return evaluate_begin(argument_exprs, env, state),
            "cond" => return evaluate_cond(argument_exprs, env, *operator_pos, state),
            "let" => return evaluate_let(argument_exprs, env, *operator_pos, state),
            "set!" => return evaluate_set(argument_exprs, env, *operator_pos, state),
            _ => {}
        }
    }

    let operator = evaluate(operator_expr, Rc::clone(&env), state)?;
    let mut args = Vec::with_capacity(argument_exprs.len());

    for argument in argument_exprs {
        args.push(evaluate(argument, Rc::clone(&env), state)?);
    }

    apply_procedure(operator, args, operator_expr.pos(), state)
}

fn evaluate_and(expressions: &[Expr], env: Rc<Environment>, state: &mut EvalState) -> EvalResult<Value> {
    let mut result = Value::Boolean(true);

    for expression in expressions {
        result = evaluate(expression, Rc::clone(&env), state)?;
        if is_false(&result) {
            return Ok(result);
        }
    }

    Ok(result)
}

fn evaluate_or(expressions: &[Expr], env: Rc<Environment>, state: &mut EvalState) -> EvalResult<Value> {
    let mut result = Value::Boolean(false);

    for expression in expressions {
        result = evaluate(expression, Rc::clone(&env), state)?;
        if !is_false(&result) {
            return Ok(result);
        }
    }

    Ok(result)
}

fn evaluate_if(
    expressions: &[Expr],
    env: Rc<Environment>,
    pos: Position,
    state: &mut EvalState,
) -> EvalResult<Value> {
    if expressions.len() != 3 {
        return Err(error_at(
            pos,
            format!("if expected 3 argument(s), got {}", expressions.len()),
        ));
    }

    let condition = evaluate(&expressions[0], Rc::clone(&env), state)?;

    if is_false(&condition) {
        evaluate(&expressions[2], env, state)
    } else {
        evaluate(&expressions[1], env, state)
    }
}

fn evaluate_define(
    expressions: &[Expr],
    env: Rc<Environment>,
    pos: Position,
    state: &mut EvalState,
) -> EvalResult<Value> {
    if expressions.len() < 2 {
        return Err(error_at(
            pos,
            format!("define expected at least 2 argument(s), got {}", expressions.len()),
        ));
    }

    let target_expr = &expressions[0];
    let value_exprs = &expressions[1..];

    if let Expr::Symbol(name, _) = target_expr {
        if value_exprs.len() != 1 {
            return Err(error_at(
                pos,
                format!("define expected 1 value expression, got {}", value_exprs.len()),
            ));
        }

        let value = evaluate(&value_exprs[0], env.clone(), state)?;
        env.define(name.clone(), value);
        return Ok(Value::Void);
    }

    let signature = match target_expr {
        Expr::List(elements, _) if !elements.is_empty() => elements,
        _ => {
            return Err(error_at(
                target_expr.pos(),
                "define expected a symbol or function signature",
            ))
        }
    };

    let name = expect_symbol_expr(&signature[0], "define")?;
    let formals = parse_formal_parameter_list(&signature[1..], "define")?;

    if value_exprs.is_empty() {
        return Err(error_at(
            pos,
            "define expected at least one function body expression",
        ));
    }

    let closure = Value::Closure(Rc::new(Closure {
        params: formals.params,
        rest_param: formals.rest_param,
        body: value_exprs.to_vec(),
        env: Rc::clone(&env),
        name: Some(name.clone()),
    }));

    env.define(name, closure);
    Ok(Value::Void)
}

fn evaluate_set(
    expressions: &[Expr],
    env: Rc<Environment>,
    pos: Position,
    state: &mut EvalState,
) -> EvalResult<Value> {
    if expressions.len() != 2 {
        return Err(error_at(
            pos,
            format!("set! expected 2 argument(s), got {}", expressions.len()),
        ));
    }

    let name = expect_symbol_expr(&expressions[0], "set!")?;
    let value = evaluate(&expressions[1], Rc::clone(&env), state)?;
    env.assign(&name, value, expressions[0].pos())?;
    Ok(Value::Void)
}

fn evaluate_quote(expressions: &[Expr], pos: Position) -> EvalResult<Value> {
    if expressions.len() != 1 {
        return Err(error_at(
            pos,
            format!("quote expected 1 argument(s), got {}", expressions.len()),
        ));
    }

    Ok(quote_expr(&expressions[0]))
}

fn evaluate_lambda(expressions: &[Expr], env: Rc<Environment>, pos: Position) -> EvalResult<Value> {
    if expressions.len() < 2 {
        return Err(error_at(
            pos,
            format!("lambda expected at least 2 argument(s), got {}", expressions.len()),
        ));
    }

    let formals = parse_formals(&expressions[0], "lambda")?;

    Ok(Value::Closure(Rc::new(Closure {
        params: formals.params,
        rest_param: formals.rest_param,
        body: expressions[1..].to_vec(),
        env,
        name: None,
    })))
}

fn evaluate_begin(expressions: &[Expr], env: Rc<Environment>, state: &mut EvalState) -> EvalResult<Value> {
    evaluate_sequence(expressions, env, state)
}

fn evaluate_cond(
    clauses: &[Expr],
    env: Rc<Environment>,
    _pos: Position,
    state: &mut EvalState,
) -> EvalResult<Value> {
    for (index, clause) in clauses.iter().enumerate() {
        let elements = match clause {
            Expr::List(elements, _) if !elements.is_empty() => elements,
            _ => return Err(error_at(clause.pos(), "cond expected a non-empty clause")),
        };

        let test_expr = &elements[0];
        let body_exprs = &elements[1..];
        let is_else = matches!(test_expr, Expr::Symbol(name, _) if name == "else");

        if is_else {
            if index != clauses.len() - 1 {
                return Err(error_at(clause.pos(), "cond else clause must be last"));
            }

            return evaluate_sequence(body_exprs, env, state);
        }

        let test_value = evaluate(test_expr, Rc::clone(&env), state)?;
        if !is_false(&test_value) {
            if body_exprs.is_empty() {
                return Ok(test_value);
            }

            return evaluate_sequence(body_exprs, env, state);
        }
    }

    Ok(Value::Void)
}

fn evaluate_let(
    expressions: &[Expr],
    env: Rc<Environment>,
    pos: Position,
    state: &mut EvalState,
) -> EvalResult<Value> {
    if expressions.len() < 2 {
        return Err(error_at(
            pos,
            format!("let expected at least 2 argument(s), got {}", expressions.len()),
        ));
    }

    if let Expr::Symbol(name, name_pos) = &expressions[0] {
        if expressions.len() < 3 {
            return Err(error_at(pos, "let expected bindings and a body"));
        }

        let bindings = parse_bindings(&expressions[1], "let")?;
        let mut values = Vec::with_capacity(bindings.len());

        for binding in &bindings {
            values.push(evaluate(&binding.value_expr, Rc::clone(&env), state)?);
        }

        let let_env = Rc::new(Environment::new(Some(Rc::clone(&env))));
        let closure = Value::Closure(Rc::new(Closure {
            params: bindings.iter().map(|binding| binding.name.clone()).collect(),
            rest_param: None,
            body: expressions[2..].to_vec(),
            env: Rc::clone(&let_env),
            name: Some(name.clone()),
        }));

        let_env.define(name.clone(), closure.clone());
        return apply_procedure(closure, values, *name_pos, state);
    }

    let bindings = parse_bindings(&expressions[0], "let")?;
    let mut values = Vec::with_capacity(bindings.len());

    for binding in &bindings {
        values.push(evaluate(&binding.value_expr, Rc::clone(&env), state)?);
    }

    let let_env = Rc::new(Environment::new(Some(env)));

    for (binding, value) in bindings.into_iter().zip(values.into_iter()) {
        let let_name = binding.name;
        let_env.define(let_name, value);
    }

    evaluate_sequence(&expressions[1..], let_env, state)
}

fn apply_procedure(
    value: Value,
    args: Vec<Value>,
    pos: Position,
    state: &mut EvalState,
) -> EvalResult<Value> {
    match value {
        Value::Builtin(builtin) => apply_builtin(builtin, &args, pos, state),
        Value::Closure(closure) => {
            if closure.rest_param.is_none() && args.len() != closure.params.len() {
                return Err(error_at(
                    pos,
                    format!(
                        "{} expected {} argument(s), got {}",
                        closure.name.as_deref().unwrap_or("lambda"),
                        closure.params.len(),
                        args.len()
                    ),
                ));
            }

            if closure.rest_param.is_some() && args.len() < closure.params.len() {
                return Err(error_at(
                    pos,
                    format!(
                        "{} expected at least {} argument(s), got {}",
                        closure.name.as_deref().unwrap_or("lambda"),
                        closure.params.len(),
                        args.len()
                    ),
                ));
            }

            let call_env = Rc::new(Environment::new(Some(Rc::clone(&closure.env))));

            for (param, arg) in closure.params.iter().zip(args.iter()) {
                call_env.define(param.clone(), arg.clone());
            }

            if let Some(rest_param) = &closure.rest_param {
                call_env.define(
                    rest_param.clone(),
                    list_to_pairs(args[closure.params.len()..].to_vec()),
                );
            }

            evaluate_sequence(&closure.body, call_env, state)
        }
        _ => Err(error_at(pos, "attempted to call a non-procedure value")),
    }
}

fn apply_builtin(
    builtin: Builtin,
    args: &[Value],
    pos: Position,
    state: &mut EvalState,
) -> EvalResult<Value> {
    match builtin {
        Builtin::Add => Ok(Value::Number(sum(args, builtin.name(), pos)?)),
        Builtin::Sub => Ok(Value::Number(subtract(args, builtin.name(), pos)?)),
        Builtin::Mul => Ok(Value::Number(product(args, builtin.name(), pos)?)),
        Builtin::Div => Ok(Value::Number(divide(args, builtin.name(), pos)?)),
        Builtin::LessThan => compare_chain(args, builtin.name(), |left, right| left < right, pos),
        Builtin::GreaterThan => compare_chain(args, builtin.name(), |left, right| left > right, pos),
        Builtin::Equal => compare_chain(args, builtin.name(), |left, right| left == right, pos),
        Builtin::LessEqual => compare_chain(args, builtin.name(), |left, right| left <= right, pos),
        Builtin::Not => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::Boolean(is_false(&args[0])))
        }
        Builtin::Cons => {
            expect_arity(builtin.name(), args, 2, pos)?;
            Ok(Value::Pair(Rc::new(Pair {
                car: args[0].clone(),
                cdr: args[1].clone(),
            })))
        }
        Builtin::Car => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(expect_pair(&args[0], builtin.name(), pos)?.car.clone())
        }
        Builtin::Cdr => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(expect_pair(&args[0], builtin.name(), pos)?.cdr.clone())
        }
        Builtin::NullPred => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::Boolean(matches!(args[0], Value::EmptyList)))
        }
        Builtin::List => Ok(list_to_pairs(args.to_vec())),
        Builtin::Length => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::Number(list_to_vec(&args[0], builtin.name(), pos)?.len() as i64))
        }
        Builtin::Append => Ok(append_lists(args, pos)?),
        Builtin::StringPred => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::Boolean(matches!(args[0], Value::String(_))))
        }
        Builtin::NumberPred => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::Boolean(matches!(args[0], Value::Number(_))))
        }
        Builtin::BooleanPred => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
        }
        Builtin::PairPred => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::Boolean(matches!(args[0], Value::Pair(_))))
        }
        Builtin::SymbolPred => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
        }
        Builtin::Display => {
            expect_arity(builtin.name(), args, 1, pos)?;
            state.output.push_str(&format_display_value(&args[0]));
            Ok(Value::Void)
        }
        Builtin::Write => {
            expect_arity(builtin.name(), args, 1, pos)?;
            state.output.push_str(&format_value(&args[0]));
            Ok(Value::Void)
        }
        Builtin::Newline => {
            expect_arity(builtin.name(), args, 0, pos)?;
            state.output.push('\n');
            Ok(Value::Void)
        }
        Builtin::StringAppend => {
            let mut combined = Vec::new();
            for arg in args {
                combined.extend(expect_string_chars(arg, builtin.name(), pos)?);
            }
            Ok(Value::String(make_string(combined, false)))
        }
        Builtin::StringLength => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::Number(
                expect_string_value(&args[0], builtin.name(), pos)?
                    .borrow()
                    .chars
                    .len() as i64,
            ))
        }
        Builtin::Substring => {
            expect_arity(builtin.name(), args, 3, pos)?;
            let string = expect_string_value(&args[0], builtin.name(), pos)?;
            let chars = string.borrow().chars.clone();
            let start = expect_index(&args[1], builtin.name(), pos)?;
            let end = expect_index(&args[2], builtin.name(), pos)?;

            if start > end || end > chars.len() {
                return Err(error_at(pos, "substring index out of bounds"));
            }

            Ok(Value::String(make_string(chars[start..end].to_vec(), false)))
        }
        Builtin::StringToNumber => {
            expect_arity(builtin.name(), args, 1, pos)?;
            let text = expect_string_content(&args[0], builtin.name(), pos)?;
            if let Some(number) = parse_integer_token(&text) {
                Ok(Value::Number(number))
            } else {
                Ok(Value::Boolean(false))
            }
        }
        Builtin::NumberToString => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::String(make_string(
                expect_number(&args[0], builtin.name(), pos)?
                    .to_string()
                    .chars()
                    .collect(),
                false,
            )))
        }
        Builtin::SymbolToString => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::String(make_string(
                expect_symbol_value(&args[0], builtin.name(), pos)?
                    .chars()
                    .collect(),
                false,
            )))
        }
        Builtin::StringToSymbol => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::Symbol(expect_string_content(&args[0], builtin.name(), pos)?))
        }
        Builtin::StringRef => {
            expect_arity(builtin.name(), args, 2, pos)?;
            let string = expect_string_value(&args[0], builtin.name(), pos)?;
            let chars = string.borrow().chars.clone();
            let index = expect_index(&args[1], builtin.name(), pos)?;

            if index >= chars.len() {
                return Err(error_at(pos, "string-ref index out of bounds"));
            }

            Ok(Value::Char(chars[index]))
        }
        Builtin::StringCopy => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::String(make_string(
                expect_string_chars(&args[0], builtin.name(), pos)?,
                true,
            )))
        }
        Builtin::StringSet => {
            expect_arity(builtin.name(), args, 3, pos)?;
            let string = expect_mutable_string(&args[0], builtin.name(), pos)?;
            let index = expect_index(&args[1], builtin.name(), pos)?;
            let ch = expect_char(&args[2], builtin.name(), pos)?;
            let mut string = string.borrow_mut();

            if index >= string.chars.len() {
                return Err(error_at(pos, "string-set! index out of bounds"));
            }

            string.chars[index] = ch;
            Ok(Value::Void)
        }
        Builtin::CharPred => {
            expect_arity(builtin.name(), args, 1, pos)?;
            Ok(Value::Boolean(matches!(args[0], Value::Char(_))))
        }
        Builtin::Apply => {
            expect_at_least_arity(builtin.name(), args, 2, pos)?;
            let procedure = args[0].clone();
            let mut flattened = args[1..args.len() - 1].to_vec();
            flattened.extend(list_to_vec(&args[args.len() - 1], builtin.name(), pos)?);
            apply_procedure(procedure, flattened, pos, state)
        }
    }
}

fn quote_expr(expression: &Expr) -> Value {
    match expression {
        Expr::Number(value, _) => Value::Number(*value),
        Expr::Boolean(value, _) => Value::Boolean(*value),
        Expr::String(value, _) => Value::String(make_string(string_to_chars(value), false)),
        Expr::Char(value, _) => Value::Char(*value),
        Expr::Symbol(name, _) => Value::Symbol(name.clone()),
        Expr::List(elements, _) => list_to_pairs(elements.iter().map(quote_expr).collect()),
    }
}

fn tokenize(input: &str) -> EvalResult<TokenStream> {
    let chars: Vec<char> = input.chars().collect();
    let mut tokens = Vec::new();
    let mut index = 0usize;
    let mut line = 1usize;
    let mut col = 1usize;

    while index < chars.len() {
        let ch = chars[index];

        if ch.is_whitespace() {
            advance_char(&chars, &mut index, &mut line, &mut col);
            continue;
        }

        if ch == ';' {
            while index < chars.len() && chars[index] != '\n' {
                advance_char(&chars, &mut index, &mut line, &mut col);
            }
            continue;
        }

        let pos = Position { line, col };

        match ch {
            '(' => {
                advance_char(&chars, &mut index, &mut line, &mut col);
                tokens.push(Token {
                    kind: TokenKind::LParen,
                    pos,
                });
            }
            ')' => {
                advance_char(&chars, &mut index, &mut line, &mut col);
                tokens.push(Token {
                    kind: TokenKind::RParen,
                    pos,
                });
            }
            '\'' => {
                advance_char(&chars, &mut index, &mut line, &mut col);
                tokens.push(Token {
                    kind: TokenKind::Quote,
                    pos,
                });
            }
            '"' => {
                let value = read_string_literal(&chars, &mut index, &mut line, &mut col, pos)?;
                tokens.push(Token {
                    kind: TokenKind::String(value),
                    pos,
                });
            }
            _ => {
                let mut raw = String::new();

                while index < chars.len() {
                    let next = chars[index];
                    if next.is_whitespace() || matches!(next, '(' | ')' | ';') {
                        break;
                    }
                    raw.push(advance_char(&chars, &mut index, &mut line, &mut col));
                }

                if raw.is_empty() {
                    return Err(error_at(pos, "unexpected token"));
                }

                if raw == "#t" {
                    tokens.push(Token {
                        kind: TokenKind::Boolean(true),
                        pos,
                    });
                } else if raw == "#f" {
                    tokens.push(Token {
                        kind: TokenKind::Boolean(false),
                        pos,
                    });
                } else if raw.starts_with("#\\") {
                    tokens.push(Token {
                        kind: TokenKind::Char(parse_char_literal(&raw, pos)?),
                        pos,
                    });
                } else if let Some(number) = parse_integer_token(&raw) {
                    tokens.push(Token {
                        kind: TokenKind::Number(number),
                        pos,
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Symbol(raw),
                        pos,
                    });
                }
            }
        }
    }

    Ok(TokenStream {
        tokens,
        eof_pos: Position { line, col },
    })
}

fn advance_char(
    chars: &[char],
    index: &mut usize,
    line: &mut usize,
    col: &mut usize,
) -> char {
    let ch = chars[*index];
    *index += 1;

    if ch == '\n' {
        *line += 1;
        *col = 1;
    } else {
        *col += 1;
    }

    ch
}

fn read_string_literal(
    chars: &[char],
    index: &mut usize,
    line: &mut usize,
    col: &mut usize,
    start_pos: Position,
) -> EvalResult<String> {
    advance_char(chars, index, line, col);
    let mut value = String::new();

    while *index < chars.len() {
        let ch = advance_char(chars, index, line, col);

        if ch == '"' {
            return Ok(value);
        }

        if ch == '\\' {
            if *index >= chars.len() {
                return Err(error_at(start_pos, "unterminated string literal"));
            }

            let escaped = advance_char(chars, index, line, col);
            match escaped {
                'n' => value.push('\n'),
                't' => value.push('\t'),
                '"' | '\\' => value.push(escaped),
                other => value.push(other),
            }
            continue;
        }

        value.push(ch);
    }

    Err(error_at(start_pos, "unterminated string literal"))
}

fn parse_char_literal(raw: &str, pos: Position) -> EvalResult<char> {
    let literal = &raw[2..];

    match literal {
        "space" => Ok(' '),
        "newline" => Ok('\n'),
        _ if literal.chars().count() == 1 => Ok(literal.chars().next().unwrap()),
        _ => Err(error_at(pos, "invalid character literal")),
    }
}

fn parse_integer_token(raw: &str) -> Option<i64> {
    if raw.is_empty() {
        return None;
    }

    let digits = if let Some(rest) = raw.strip_prefix('+') {
        if rest.is_empty() {
            return None;
        }
        rest
    } else if let Some(rest) = raw.strip_prefix('-') {
        if rest.is_empty() {
            return None;
        }
        rest
    } else {
        raw
    };

    if digits.chars().all(|ch| ch.is_ascii_digit()) {
        raw.parse().ok()
    } else {
        None
    }
}

fn make_string(chars: Vec<char>, mutable: bool) -> Rc<RefCell<SchemeString>> {
    Rc::new(RefCell::new(SchemeString { chars, mutable }))
}

fn string_to_chars(value: &str) -> Vec<char> {
    value.chars().collect()
}

fn is_false(value: &Value) -> bool {
    matches!(value, Value::Boolean(false))
}

fn expect_arity(name: &str, args: &[Value], expected: usize, pos: Position) -> EvalResult<()> {
    if args.len() != expected {
        return Err(error_at(
            pos,
            format!("{name} expected {expected} argument(s), got {}", args.len()),
        ));
    }

    Ok(())
}

fn expect_at_least_arity(name: &str, args: &[Value], minimum: usize, pos: Position) -> EvalResult<()> {
    if args.len() < minimum {
        return Err(error_at(
            pos,
            format!("{name} expected at least {minimum} argument(s), got {}", args.len()),
        ));
    }

    Ok(())
}

fn expect_number(value: &Value, name: &str, pos: Position) -> EvalResult<i64> {
    if let Value::Number(number) = value {
        Ok(*number)
    } else {
        Err(error_at(pos, format!("{name} expected a number")))
    }
}

fn expect_string_value(
    value: &Value,
    name: &str,
    pos: Position,
) -> EvalResult<Rc<RefCell<SchemeString>>> {
    if let Value::String(string) = value {
        Ok(Rc::clone(string))
    } else {
        Err(error_at(pos, format!("{name} expected a string")))
    }
}

fn expect_string_content(value: &Value, name: &str, pos: Position) -> EvalResult<String> {
    Ok(expect_string_value(value, name, pos)?
        .borrow()
        .chars
        .iter()
        .collect())
}

fn expect_string_chars(value: &Value, name: &str, pos: Position) -> EvalResult<Vec<char>> {
    Ok(expect_string_value(value, name, pos)?.borrow().chars.clone())
}

fn expect_mutable_string(
    value: &Value,
    name: &str,
    pos: Position,
) -> EvalResult<Rc<RefCell<SchemeString>>> {
    let string = expect_string_value(value, name, pos)?;
    if !string.borrow().mutable {
        return Err(error_at(pos, format!("{name} expected a mutable string")));
    }
    Ok(string)
}

fn expect_pair(value: &Value, name: &str, pos: Position) -> EvalResult<Rc<Pair>> {
    if let Value::Pair(pair) = value {
        Ok(Rc::clone(pair))
    } else {
        Err(error_at(pos, format!("{name} expected a pair")))
    }
}

fn expect_char(value: &Value, name: &str, pos: Position) -> EvalResult<char> {
    if let Value::Char(ch) = value {
        Ok(*ch)
    } else {
        Err(error_at(pos, format!("{name} expected a character")))
    }
}

fn expect_symbol_value(value: &Value, name: &str, pos: Position) -> EvalResult<String> {
    if let Value::Symbol(symbol) = value {
        Ok(symbol.clone())
    } else {
        Err(error_at(pos, format!("{name} expected a symbol")))
    }
}

fn expect_index(value: &Value, name: &str, pos: Position) -> EvalResult<usize> {
    let number = expect_number(value, name, pos)?;
    if number < 0 {
        return Err(error_at(
            pos,
            format!("{name} expected a non-negative integer"),
        ));
    }

    Ok(number as usize)
}

fn expect_symbol_expr(expression: &Expr, name: &str) -> EvalResult<String> {
    if let Expr::Symbol(symbol, _) = expression {
        Ok(symbol.clone())
    } else {
        Err(error_at(expression.pos(), format!("{name} expected a symbol")))
    }
}

fn expect_parameter_name(expression: &Expr, name: &str) -> EvalResult<String> {
    let parameter_name = expect_symbol_expr(expression, name)?;
    if parameter_name == "." {
        return Err(error_at(expression.pos(), format!("{name} expected a symbol")));
    }
    Ok(parameter_name)
}

fn parse_formals(params_expr: &Expr, name: &str) -> EvalResult<FormalParameters> {
    match params_expr {
        Expr::Symbol(_, _) => Ok(FormalParameters {
            params: Vec::new(),
            rest_param: Some(expect_parameter_name(params_expr, name)?),
        }),
        Expr::List(elements, _) => parse_formal_parameter_list(elements, name),
        _ => Err(error_at(
            params_expr.pos(),
            format!("{name} expected a parameter list"),
        )),
    }
}

fn parse_formal_parameter_list(param_exprs: &[Expr], name: &str) -> EvalResult<FormalParameters> {
    let dot_index = param_exprs
        .iter()
        .position(|expr| matches!(expr, Expr::Symbol(symbol, _) if symbol == "."));

    match dot_index {
        None => Ok(FormalParameters {
            params: param_exprs
                .iter()
                .map(|expr| expect_parameter_name(expr, name))
                .collect::<EvalResult<Vec<_>>>()?,
            rest_param: None,
        }),
        Some(index) => {
            let dot_expr = &param_exprs[index];

            if index == param_exprs.len() - 1 {
                return Err(error_at(dot_expr.pos(), format!("{name} expected a symbol after .")));
            }

            if index != param_exprs.len() - 2 {
                return Err(error_at(
                    dot_expr.pos(),
                    format!("{name} expected exactly one rest parameter after ."),
                ));
            }

            Ok(FormalParameters {
                params: param_exprs[..index]
                    .iter()
                    .map(|expr| expect_parameter_name(expr, name))
                    .collect::<EvalResult<Vec<_>>>()?,
                rest_param: Some(expect_parameter_name(&param_exprs[index + 1], name)?),
            })
        }
    }
}

fn parse_bindings(bindings_expr: &Expr, name: &str) -> EvalResult<Vec<Binding>> {
    let bindings = match bindings_expr {
        Expr::List(elements, _) => elements,
        _ => return Err(error_at(bindings_expr.pos(), format!("{name} expected a bindings list"))),
    };

    bindings
        .iter()
        .map(|binding_expr| match binding_expr {
            Expr::List(elements, _) if elements.len() == 2 => Ok(Binding {
                name: expect_symbol_expr(&elements[0], name)?,
                value_expr: elements[1].clone(),
            }),
            _ => Err(error_at(
                binding_expr.pos(),
                format!("{name} expected bindings of the form (name value)"),
            )),
        })
        .collect()
}

fn evaluate_sequence(
    expressions: &[Expr],
    env: Rc<Environment>,
    state: &mut EvalState,
) -> EvalResult<Value> {
    let mut result = Value::Void;

    for expression in expressions {
        result = evaluate(expression, Rc::clone(&env), state)?;
    }

    Ok(result)
}

fn list_to_vec(value: &Value, name: &str, pos: Position) -> EvalResult<Vec<Value>> {
    let mut elements = Vec::new();
    let mut current = value.clone();

    loop {
        match current {
            Value::Pair(pair) => {
                elements.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            Value::EmptyList => return Ok(elements),
            _ => return Err(error_at(pos, format!("{name} expected a list"))),
        }
    }
}

fn list_to_pairs(elements: Vec<Value>) -> Value {
    let mut result = Value::EmptyList;

    for element in elements.into_iter().rev() {
        result = Value::Pair(Rc::new(Pair {
            car: element,
            cdr: result,
        }));
    }

    result
}

fn append_lists(args: &[Value], pos: Position) -> EvalResult<Value> {
    let mut elements = Vec::new();

    for arg in args {
        elements.extend(list_to_vec(arg, "append", pos)?);
    }

    Ok(list_to_pairs(elements))
}

fn sum(args: &[Value], name: &str, pos: Position) -> EvalResult<i64> {
    let mut total = 0i64;
    for arg in args {
        total += expect_number(arg, name, pos)?;
    }
    Ok(total)
}

fn subtract(args: &[Value], name: &str, pos: Position) -> EvalResult<i64> {
    expect_at_least_arity(name, args, 1, pos)?;

    if args.len() == 1 {
        return Ok(-expect_number(&args[0], name, pos)?);
    }

    let mut total = expect_number(&args[0], name, pos)?;
    for arg in &args[1..] {
        total -= expect_number(arg, name, pos)?;
    }

    Ok(total)
}

fn product(args: &[Value], name: &str, pos: Position) -> EvalResult<i64> {
    let mut total = 1i64;
    for arg in args {
        total *= expect_number(arg, name, pos)?;
    }
    Ok(total)
}

fn divide(args: &[Value], name: &str, pos: Position) -> EvalResult<i64> {
    expect_at_least_arity(name, args, 2, pos)?;

    let mut total = expect_number(&args[0], name, pos)?;

    for arg in &args[1..] {
        let divisor = expect_number(arg, name, pos)?;
        if divisor == 0 {
            return Err(error_at(pos, "division by zero"));
        }
        total /= divisor;
    }

    Ok(total)
}

fn compare_chain<F>(args: &[Value], name: &str, predicate: F, pos: Position) -> EvalResult<Value>
where
    F: Fn(i64, i64) -> bool,
{
    expect_at_least_arity(name, args, 2, pos)?;
    let numbers = args
        .iter()
        .map(|arg| expect_number(arg, name, pos))
        .collect::<EvalResult<Vec<_>>>()?;

    for pair in numbers.windows(2) {
        if !predicate(pair[0], pair[1]) {
            return Ok(Value::Boolean(false));
        }
    }

    Ok(Value::Boolean(true))
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Number(number) => number.to_string(),
        Value::Boolean(true) => "#t".into(),
        Value::Boolean(false) => "#f".into(),
        Value::String(string) => format!("\"{}\"", escape_string(&string.borrow().chars.iter().collect::<String>())),
        Value::Symbol(symbol) => symbol.clone(),
        Value::Pair(pair) => format_pair(pair),
        Value::Char(ch) => format_char_literal(*ch),
        Value::EmptyList => "()".into(),
        Value::Void => "#<void>".into(),
        Value::Builtin(builtin) => format!("#<procedure:{}>", builtin.name()),
        Value::Closure(closure) => match &closure.name {
            Some(name) => format!("#<procedure:{name}>"),
            None => "#<procedure>".into(),
        },
    }
}

fn format_display_value(value: &Value) -> String {
    match value {
        Value::String(string) => string.borrow().chars.iter().collect(),
        Value::Char(ch) => ch.to_string(),
        Value::Pair(pair) => format_display_pair(pair),
        _ => format_value(value),
    }
}

fn format_pair(pair: &Rc<Pair>) -> String {
    let mut parts = Vec::new();
    let mut current = Value::Pair(Rc::clone(pair));

    loop {
        match current {
            Value::Pair(pair) => {
                parts.push(format_value(&pair.car));
                current = pair.cdr.clone();
            }
            Value::EmptyList => return format!("({})", parts.join(" ")),
            other => return format!("({} . {})", parts.join(" "), format_value(&other)),
        }
    }
}

fn format_display_pair(pair: &Rc<Pair>) -> String {
    let mut parts = Vec::new();
    let mut current = Value::Pair(Rc::clone(pair));

    loop {
        match current {
            Value::Pair(pair) => {
                parts.push(format_display_value(&pair.car));
                current = pair.cdr.clone();
            }
            Value::EmptyList => return format!("({})", parts.join(" ")),
            other => {
                return format!(
                    "({} . {})",
                    parts.join(" "),
                    format_display_value(&other)
                )
            }
        }
    }
}

fn escape_string(value: &str) -> String {
    let mut escaped = String::new();

    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\t' => escaped.push_str("\\t"),
            other => escaped.push(other),
        }
    }

    escaped
}

fn format_char_literal(value: char) -> String {
    match value {
        ' ' => "#\\space".into(),
        '\n' => "#\\newline".into(),
        other => format!("#\\{other}"),
    }
}

fn error_at(pos: Position, message: impl Into<String>) -> EvalError {
    EvalError::with_position(pos.line, pos.col, message)
}

#[cfg(test)]
mod tests;
