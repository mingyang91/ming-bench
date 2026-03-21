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
    Char(char, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    fn pos(&self) -> SourcePos {
        match self {
            Self::Int(_, pos)
            | Self::Bool(_, pos)
            | Self::String(_, pos)
            | Self::Char(_, pos)
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
    GreaterEqual,
    Not,
    Cons,
    Car,
    Cdr,
    IsNull,
    List,
    Map,
    Length,
    IsString,
    IsNumber,
    IsBoolean,
    IsPair,
    IsSymbol,
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
    StringToList,
    ListToString,
    StringRef,
    StringCopy,
    StringSet,
    IsChar,
    CharToInteger,
    IntegerToChar,
}

#[derive(Clone, Copy, Debug)]
enum RenderMode {
    Write,
    Display,
}

type EnvRef = Rc<Environment>;
type BindingRef = Rc<RefCell<Value>>;

#[derive(Debug)]
struct Environment {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, BindingRef>>,
    output: Rc<RefCell<String>>,
}

#[derive(Clone, Debug)]
struct Procedure {
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Debug)]
enum TailOutcome {
    Value(Value),
    TailCall {
        procedure: Rc<Procedure>,
        args: Vec<Value>,
        pos: SourcePos,
    },
}

#[derive(Clone, Debug)]
struct SchemeString(Rc<RefCell<Vec<char>>>);

impl SchemeString {
    fn new(value: impl Into<String>) -> Self {
        Self(Rc::new(RefCell::new(value.into().chars().collect())))
    }

    fn copy(&self) -> Self {
        Self(Rc::new(RefCell::new(self.0.borrow().clone())))
    }

    fn len(&self) -> usize {
        self.0.borrow().len()
    }

    fn to_plain_string(&self) -> String {
        self.0.borrow().iter().collect()
    }

    fn char_at(&self, index: usize) -> Option<char> {
        self.0.borrow().get(index).copied()
    }
}

#[derive(Clone, Debug)]
enum Value {
    Int(i64),
    Bool(bool),
    String(SchemeString),
    Symbol(String),
    Char(char),
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
            Self::Char(_) => "char",
            Self::List(_) => "list",
            Self::Builtin(_) | Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn render(&self) -> String {
        self.render_with_mode(RenderMode::Write)
    }

    fn render_for_display(&self) -> String {
        self.render_with_mode(RenderMode::Display)
    }

    fn render_with_mode(&self, mode: RenderMode) -> String {
        match self {
            Self::Int(value) => value.to_string(),
            Self::Bool(true) => "#t".into(),
            Self::Bool(false) => "#f".into(),
            Self::String(value) => {
                let rendered = value.to_plain_string();
                match mode {
                    RenderMode::Write => render_string(&rendered),
                    RenderMode::Display => rendered,
                }
            }
            Self::Symbol(value) => value.clone(),
            Self::Char(value) => render_char(*value, mode),
            Self::List(items) => render_list(items, mode),
            Self::Builtin(_) | Self::Procedure(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
        }
    }
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        let output = parent
            .as_ref()
            .map(|parent| parent.output.clone())
            .unwrap_or_else(|| Rc::new(RefCell::new(String::new())));
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
            output,
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.bindings
            .borrow_mut()
            .insert(name.into(), Rc::new(RefCell::new(value)));
    }

    fn lookup_binding(&self, name: &str) -> Option<BindingRef> {
        let binding = self.bindings.borrow().get(name).cloned();
        binding.or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.lookup_binding(name))
        })
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        self.lookup_binding(name)
            .map(|binding| binding.borrow().clone())
    }

    fn set(&self, name: &str, value: Value) -> bool {
        if let Some(binding) = self.lookup_binding(name) {
            *binding.borrow_mut() = value;
            true
        } else {
            false
        }
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
            _ if token.starts_with("#\\") => Ok(Expr::Char(parse_char_literal(token, pos)?, pos)),
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

fn parse_char_literal(token: &str, pos: SourcePos) -> Result<char, EvalError> {
    let Some(literal) = token.strip_prefix("#\\") else {
        return Err(syntax_error(
            pos,
            format!("invalid character literal: {token}"),
        ));
    };

    match literal {
        "space" => Ok(' '),
        "newline" => Ok('\n'),
        _ => {
            let mut chars = literal.chars();
            let Some(value) = chars.next() else {
                return Err(syntax_error(pos, "invalid character literal"));
            };

            if chars.next().is_some() {
                return Err(syntax_error(
                    pos,
                    format!("invalid character literal: {token}"),
                ));
            }

            Ok(value)
        }
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

fn index_out_of_bounds(pos: SourcePos, index: i64, len: usize) -> EvalError {
    EvalError::IndexOutOfBounds { pos, index, len }
}

fn invalid_range(pos: SourcePos, start: i64, end: i64, len: usize) -> EvalError {
    EvalError::InvalidRange {
        pos,
        start,
        end,
        len,
    }
}

fn not_callable(pos: SourcePos, found: &'static str) -> EvalError {
    EvalError::NotCallable { pos, found }
}

fn immutable_string(pos: SourcePos) -> EvalError {
    EvalError::ImmutableString { pos }
}

fn invalid_character_code(pos: SourcePos, value: i64) -> EvalError {
    EvalError::InvalidCharacterCode { pos, value }
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
    eval_program(input).map(|(value, _)| value.render())
}

fn eval_program(input: &str) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_program()?;
    let env = default_env();
    let mut last_value = None;

    for expr in exprs {
        last_value = Some(eval_expr(&expr, &env)?);
    }

    let value = last_value
        .ok_or_else(|| syntax_error(SourcePos { line: 1, col: 1 }, "expected expression"))?;
    let output = env.output.borrow().clone();
    Ok((value, output))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    eval_program(input).map(|(value, output)| (value.render(), output))
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
        (">=", Builtin::GreaterEqual),
        ("not", Builtin::Not),
        ("cons", Builtin::Cons),
        ("car", Builtin::Car),
        ("cdr", Builtin::Cdr),
        ("null?", Builtin::IsNull),
        ("list", Builtin::List),
        ("map", Builtin::Map),
        ("length", Builtin::Length),
        ("string?", Builtin::IsString),
        ("number?", Builtin::IsNumber),
        ("boolean?", Builtin::IsBoolean),
        ("pair?", Builtin::IsPair),
        ("symbol?", Builtin::IsSymbol),
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
        ("string->list", Builtin::StringToList),
        ("list->string", Builtin::ListToString),
        ("string-ref", Builtin::StringRef),
        ("string-copy", Builtin::StringCopy),
        ("string-set!", Builtin::StringSet),
        ("char?", Builtin::IsChar),
        ("char->integer", Builtin::CharToInteger),
        ("integer->char", Builtin::IntegerToChar),
    ] {
        env.define(name, Value::Builtin(builtin));
    }

    env
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Int(value, _) => Ok(Value::Int(*value)),
        Expr::Bool(value, _) => Ok(Value::Bool(*value)),
        Expr::String(value, _) => Ok(Value::String(SchemeString::new(value.clone()))),
        Expr::Char(value, _) => Ok(Value::Char(*value)),
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
            "set!" => return eval_set(&items[1..], *pos, env),
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
    apply_value(operator, &args, head.pos(), &env.output)
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

fn eval_set(parts: &[Expr], pos: SourcePos, env: &EnvRef) -> Result<Value, EvalError> {
    let [target, value_expr] = parts else {
        return Err(wrong_arity(pos, "set!", "exactly 2", parts.len()));
    };

    let Expr::Symbol(name, name_pos) = target else {
        return Err(syntax_error(target.pos(), "set! target must be a symbol"));
    };

    let value = eval_expr(value_expr, env)?;
    if env.set(name, value) {
        Ok(Value::Void)
    } else {
        Err(unbound_variable(*name_pos, name.clone()))
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
    match parts {
        [Expr::Symbol(name, _), bindings_expr, body @ ..] => {
            if body.is_empty() {
                return Err(syntax_error(pos, "let requires a body"));
            }

            let (procedure, args) = build_named_let_call(name, bindings_expr, body, env)?;
            apply_procedure(procedure, args, pos)
        }
        [bindings_expr, body @ ..] => {
            if body.is_empty() {
                return Err(syntax_error(pos, "let requires a body"));
            }

            let evaluated_bindings = eval_let_bindings(bindings_expr, env)?;
            let local_env = Environment::new(Some(env.clone()));
            for (name, value) in evaluated_bindings {
                local_env.define(name, value);
            }

            eval_body(body, &local_env)
        }
        _ => Err(wrong_arity(pos, "let", "at least 2", parts.len())),
    }
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
        Expr::String(value, _) => Value::String(SchemeString::new(value.clone())),
        Expr::Char(value, _) => Value::Char(*value),
        Expr::Symbol(value, _) => Value::Symbol(value.clone()),
        Expr::List(items, _) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_arg_values(args: &[Expr], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    args.iter().map(|expr| eval_expr(expr, env)).collect()
}

fn apply_value(
    operator: Value,
    args: &[Value],
    pos: SourcePos,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    match operator {
        Value::Builtin(builtin) => apply_builtin(builtin, args, pos, output),
        Value::Procedure(procedure) => apply_procedure(procedure, args.to_vec(), pos),
        other => Err(not_callable(pos, other.type_name())),
    }
}

fn apply_builtin(
    builtin: Builtin,
    args: &[Value],
    pos: SourcePos,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    match builtin {
        Builtin::Add => eval_add(args, pos),
        Builtin::Sub => eval_sub(args, pos),
        Builtin::Mul => eval_mul(args, pos),
        Builtin::Div => eval_div(args, pos),
        Builtin::LessThan => eval_compare(args, "<", pos, |left, right| left < right),
        Builtin::GreaterThan => eval_compare(args, ">", pos, |left, right| left > right),
        Builtin::Equal => eval_compare(args, "=", pos, |left, right| left == right),
        Builtin::LessEqual => eval_compare(args, "<=", pos, |left, right| left <= right),
        Builtin::GreaterEqual => eval_compare(args, ">=", pos, |left, right| left >= right),
        Builtin::Not => eval_not(args, pos),
        Builtin::Cons => eval_cons(args, pos),
        Builtin::Car => eval_car(args, pos),
        Builtin::Cdr => eval_cdr(args, pos),
        Builtin::IsNull => eval_null(args, pos),
        Builtin::List => eval_list_builtin(args, pos),
        Builtin::Map => eval_map(args, pos, output),
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
        Builtin::Display => eval_display(args, pos, output),
        Builtin::Write => eval_write(args, pos, output),
        Builtin::Newline => eval_newline(args, pos, output),
        Builtin::StringAppend => eval_string_append(args, pos),
        Builtin::StringLength => eval_string_length(args, pos),
        Builtin::Substring => eval_substring(args, pos),
        Builtin::StringToNumber => eval_string_to_number(args, pos),
        Builtin::NumberToString => eval_number_to_string(args, pos),
        Builtin::SymbolToString => eval_symbol_to_string(args, pos),
        Builtin::StringToSymbol => eval_string_to_symbol(args, pos),
        Builtin::StringToList => eval_string_to_list(args, pos),
        Builtin::ListToString => eval_list_to_string(args, pos),
        Builtin::StringRef => eval_string_ref(args, pos),
        Builtin::StringCopy => eval_string_copy(args, pos),
        Builtin::StringSet => eval_string_set(args, pos),
        Builtin::IsChar => {
            eval_type_predicate(args, "char?", pos, |value| matches!(value, Value::Char(_)))
        }
        Builtin::CharToInteger => eval_char_to_integer(args, pos),
        Builtin::IntegerToChar => eval_integer_to_char(args, pos),
    }
}

fn apply_procedure(
    mut procedure: Rc<Procedure>,
    mut args: Vec<Value>,
    mut pos: SourcePos,
) -> Result<Value, EvalError> {
    loop {
        if args.len() != procedure.params.len() {
            return Err(wrong_arity(
                pos,
                "procedure",
                procedure.params.len().to_string(),
                args.len(),
            ));
        }

        let local_env = Environment::new(Some(procedure.env.clone()));
        for (param, arg) in procedure.params.iter().zip(&args) {
            local_env.define(param.clone(), arg.clone());
        }

        match eval_tail_body(&procedure.body, &local_env)? {
            TailOutcome::Value(value) => return Ok(value),
            TailOutcome::TailCall {
                procedure: next_procedure,
                args: next_args,
                pos: next_pos,
            } => {
                procedure = next_procedure;
                args = next_args;
                pos = next_pos;
            }
        }
    }
}

fn eval_body(body: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;

    for expr in body {
        last = eval_expr(expr, env)?;
    }

    Ok(last)
}

fn eval_tail_body(body: &[Expr], env: &EnvRef) -> Result<TailOutcome, EvalError> {
    let Some((last, init)) = body.split_last() else {
        return Ok(TailOutcome::Value(Value::Void));
    };

    for expr in init {
        eval_expr(expr, env)?;
    }

    eval_tail_expr(last, env)
}

fn eval_tail_expr(mut expr: &Expr, env: &EnvRef) -> Result<TailOutcome, EvalError> {
    let mut current_env = env.clone();

    loop {
        match expr {
            Expr::Int(value, _) => return Ok(TailOutcome::Value(Value::Int(*value))),
            Expr::Bool(value, _) => return Ok(TailOutcome::Value(Value::Bool(*value))),
            Expr::String(value, _) => {
                return Ok(TailOutcome::Value(Value::String(SchemeString::new(
                    value.clone(),
                ))))
            }
            Expr::Char(value, _) => return Ok(TailOutcome::Value(Value::Char(*value))),
            Expr::Symbol(name, pos) => {
                let value = current_env
                    .lookup(name)
                    .ok_or_else(|| unbound_variable(*pos, name.clone()))?;
                return Ok(TailOutcome::Value(value));
            }
            Expr::List(items, list_pos) => {
                let Some(head) = items.first() else {
                    return Err(syntax_error(*list_pos, "cannot evaluate empty list"));
                };

                if let Expr::Symbol(name, pos) = head {
                    match name.as_str() {
                        "define" => {
                            return Ok(TailOutcome::Value(eval_define(
                                &items[1..],
                                *pos,
                                &current_env,
                            )?))
                        }
                        "set!" => {
                            return Ok(TailOutcome::Value(eval_set(
                                &items[1..],
                                *pos,
                                &current_env,
                            )?))
                        }
                        "if" => {
                            let [condition, consequent, alternate] = &items[1..] else {
                                return Err(wrong_arity(*pos, "if", "exactly 3", items.len() - 1));
                            };

                            expr = if eval_expr(condition, &current_env)?.is_truthy() {
                                consequent
                            } else {
                                alternate
                            };
                            continue;
                        }
                        "quote" => {
                            return Ok(TailOutcome::Value(eval_quote(&items[1..], *pos)?));
                        }
                        "lambda" => {
                            return Ok(TailOutcome::Value(eval_lambda(
                                &items[1..],
                                *pos,
                                &current_env,
                            )?))
                        }
                        "and" => {
                            let Some((last, init)) = items[1..].split_last() else {
                                return Ok(TailOutcome::Value(Value::Bool(true)));
                            };

                            for part in init {
                                let value = eval_expr(part, &current_env)?;
                                if !value.is_truthy() {
                                    return Ok(TailOutcome::Value(value));
                                }
                            }

                            expr = last;
                            continue;
                        }
                        "or" => {
                            let Some((last, init)) = items[1..].split_last() else {
                                return Ok(TailOutcome::Value(Value::Bool(false)));
                            };

                            for part in init {
                                let value = eval_expr(part, &current_env)?;
                                if value.is_truthy() {
                                    return Ok(TailOutcome::Value(value));
                                }
                            }

                            expr = last;
                            continue;
                        }
                        "let" => match &items[1..] {
                            [Expr::Symbol(name, _), bindings_expr, body @ ..] => {
                                if body.is_empty() {
                                    return Err(syntax_error(*pos, "let requires a body"));
                                }

                                let (procedure, args) =
                                    build_named_let_call(name, bindings_expr, body, &current_env)?;
                                return Ok(TailOutcome::TailCall {
                                    procedure,
                                    args,
                                    pos: *pos,
                                });
                            }
                            [bindings_expr, body @ ..] => {
                                if body.is_empty() {
                                    return Err(syntax_error(*pos, "let requires a body"));
                                }

                                let evaluated_bindings =
                                    eval_let_bindings(bindings_expr, &current_env)?;
                                let local_env = Environment::new(Some(current_env.clone()));
                                for (name, value) in evaluated_bindings {
                                    local_env.define(name, value);
                                }

                                let Some((last, init)) = body.split_last() else {
                                    return Ok(TailOutcome::Value(Value::Void));
                                };

                                for part in init {
                                    eval_expr(part, &local_env)?;
                                }

                                current_env = local_env;
                                expr = last;
                                continue;
                            }
                            _ => {
                                return Err(wrong_arity(*pos, "let", "at least 2", items.len() - 1))
                            }
                        },
                        "begin" => {
                            let Some((last, init)) = items[1..].split_last() else {
                                return Ok(TailOutcome::Value(Value::Void));
                            };

                            for part in init {
                                eval_expr(part, &current_env)?;
                            }

                            expr = last;
                            continue;
                        }
                        "cond" => return eval_tail_cond(&items[1..], *pos, &current_env),
                        _ => {}
                    }
                }

                let operator = eval_expr(head, &current_env)?;
                let args = eval_arg_values(&items[1..], &current_env)?;
                match operator {
                    Value::Builtin(builtin) => {
                        return Ok(TailOutcome::Value(apply_builtin(
                            builtin,
                            &args,
                            head.pos(),
                            &current_env.output,
                        )?))
                    }
                    Value::Procedure(procedure) => {
                        return Ok(TailOutcome::TailCall {
                            procedure,
                            args,
                            pos: head.pos(),
                        })
                    }
                    other => return Err(not_callable(head.pos(), other.type_name())),
                }
            }
        }
    }
}

fn eval_tail_cond(
    clauses: &[Expr],
    _pos: SourcePos,
    env: &EnvRef,
) -> Result<TailOutcome, EvalError> {
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

                return eval_tail_body(body, env);
            }
        }

        let test_value = eval_expr(test_expr, env)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(TailOutcome::Value(test_value))
            } else {
                eval_tail_body(body, env)
            };
        }
    }

    Ok(TailOutcome::Value(Value::Void))
}

fn eval_let_bindings(
    bindings_expr: &Expr,
    env: &EnvRef,
) -> Result<Vec<(String, Value)>, EvalError> {
    let Expr::List(bindings, _) = bindings_expr else {
        return Err(syntax_error(
            bindings_expr.pos(),
            "let bindings must be a list",
        ));
    };

    bindings
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
        .collect()
}

fn build_named_let_call(
    name: &str,
    bindings_expr: &Expr,
    body: &[Expr],
    env: &EnvRef,
) -> Result<(Rc<Procedure>, Vec<Value>), EvalError> {
    let evaluated_bindings = eval_let_bindings(bindings_expr, env)?;
    let params = evaluated_bindings
        .iter()
        .map(|(param, _)| param.clone())
        .collect();
    let args = evaluated_bindings
        .into_iter()
        .map(|(_, value)| value)
        .collect();

    let local_env = Environment::new(Some(env.clone()));
    let procedure = Rc::new(Procedure {
        params,
        body: body.to_vec(),
        env: local_env.clone(),
    });
    local_env.define(name.to_string(), Value::Procedure(procedure.clone()));
    Ok((procedure, args))
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

fn eval_map(
    args: &[Value],
    pos: SourcePos,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    let [procedure, list] = args else {
        return Err(wrong_arity(pos, "map", "exactly 2", args.len()));
    };

    let Value::List(items) = list else {
        return Err(type_error(pos, "list", list.type_name()));
    };

    let mut mapped = Vec::with_capacity(items.len());
    for item in items {
        mapped.push(apply_value(
            procedure.clone(),
            &[item.clone()],
            pos,
            output,
        )?);
    }

    Ok(Value::List(mapped))
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

fn eval_display(
    args: &[Value],
    pos: SourcePos,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "display", "exactly 1", args.len()));
    };

    output.borrow_mut().push_str(&value.render_for_display());
    Ok(Value::Void)
}

fn eval_write(
    args: &[Value],
    pos: SourcePos,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "write", "exactly 1", args.len()));
    };

    output.borrow_mut().push_str(&value.render());
    Ok(Value::Void)
}

fn eval_newline(
    args: &[Value],
    pos: SourcePos,
    output: &Rc<RefCell<String>>,
) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(wrong_arity(pos, "newline", "exactly 0", args.len()));
    }

    output.borrow_mut().push('\n');
    Ok(Value::Void)
}

fn eval_string_append(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let mut result = String::new();

    for value in args {
        result.push_str(&expect_string(value, pos)?.to_plain_string());
    }

    Ok(Value::String(SchemeString::new(result)))
}

fn eval_string_length(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "string-length", "exactly 1", args.len()));
    };

    Ok(Value::Int(expect_string(value, pos)?.len() as i64))
}

fn eval_substring(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value, start, end] = args else {
        return Err(wrong_arity(pos, "substring", "exactly 3", args.len()));
    };

    let string = expect_string(value, pos)?;
    let start = expect_integer(start, pos)?;
    let end = expect_integer(end, pos)?;
    let len = string.len();

    if start < 0 || end < 0 || start > end || end as usize > len {
        return Err(invalid_range(pos, start, end, len));
    }

    let count = (end - start) as usize;
    let substring = string
        .to_plain_string()
        .chars()
        .skip(start as usize)
        .take(count)
        .collect::<String>();
    Ok(Value::String(SchemeString::new(substring)))
}

fn eval_string_to_number(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "string->number", "exactly 1", args.len()));
    };

    match expect_string(value, pos)?.to_plain_string().parse::<i64>() {
        Ok(number) => Ok(Value::Int(number)),
        Err(_) => Ok(Value::Bool(false)),
    }
}

fn eval_number_to_string(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "number->string", "exactly 1", args.len()));
    };

    Ok(Value::String(SchemeString::new(
        expect_integer(value, pos)?.to_string(),
    )))
}

fn eval_symbol_to_string(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "symbol->string", "exactly 1", args.len()));
    };

    Ok(Value::String(SchemeString::new(
        expect_symbol(value, pos)?.to_string(),
    )))
}

fn eval_string_to_symbol(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "string->symbol", "exactly 1", args.len()));
    };

    Ok(Value::Symbol(expect_string(value, pos)?.to_plain_string()))
}

fn eval_string_to_list(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "string->list", "exactly 1", args.len()));
    };

    let chars = expect_string(value, pos)?
        .to_plain_string()
        .chars()
        .map(Value::Char)
        .collect();
    Ok(Value::List(chars))
}

fn eval_list_to_string(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "list->string", "exactly 1", args.len()));
    };

    let Value::List(items) = value else {
        return Err(type_error(pos, "list", value.type_name()));
    };

    let mut rendered = String::with_capacity(items.len());
    for item in items {
        let Value::Char(ch) = item else {
            return Err(type_error(pos, "char", item.type_name()));
        };
        rendered.push(*ch);
    }

    Ok(Value::String(SchemeString::new(rendered)))
}

fn eval_string_ref(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value, index] = args else {
        return Err(wrong_arity(pos, "string-ref", "exactly 2", args.len()));
    };

    let string = expect_string(value, pos)?;
    let index = expect_integer(index, pos)?;
    let len = string.len();

    if index < 0 || index as usize >= len {
        return Err(index_out_of_bounds(pos, index, len));
    }

    let ch = string
        .char_at(index as usize)
        .ok_or_else(|| index_out_of_bounds(pos, index, len))?;
    Ok(Value::Char(ch))
}

fn eval_string_copy(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "string-copy", "exactly 1", args.len()));
    };

    Ok(Value::String(expect_string(value, pos)?.copy()))
}

fn eval_string_set(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value, _, _] = args else {
        return Err(wrong_arity(pos, "string-set!", "exactly 3", args.len()));
    };

    let _ = expect_string(value, pos)?;
    Err(immutable_string(pos))
}

fn eval_char_to_integer(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "char->integer", "exactly 1", args.len()));
    };

    Ok(Value::Int(expect_char(value, pos)? as i64))
}

fn eval_integer_to_char(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(wrong_arity(pos, "integer->char", "exactly 1", args.len()));
    };

    let code = expect_integer(value, pos)?;
    let Some(ch) = u32::try_from(code).ok().and_then(char::from_u32) else {
        return Err(invalid_character_code(pos, code));
    };

    Ok(Value::Char(ch))
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

fn expect_integer(value: &Value, pos: SourcePos) -> Result<i64, EvalError> {
    match value {
        Value::Int(number) => Ok(*number),
        other => Err(type_error(pos, "number", other.type_name())),
    }
}

fn expect_string(value: &Value, pos: SourcePos) -> Result<SchemeString, EvalError> {
    match value {
        Value::String(string) => Ok(string.clone()),
        other => Err(type_error(pos, "string", other.type_name())),
    }
}

fn expect_symbol<'a>(value: &'a Value, pos: SourcePos) -> Result<&'a str, EvalError> {
    match value {
        Value::Symbol(symbol) => Ok(symbol),
        other => Err(type_error(pos, "symbol", other.type_name())),
    }
}

fn expect_char(value: &Value, pos: SourcePos) -> Result<char, EvalError> {
    match value {
        Value::Char(ch) => Ok(*ch),
        other => Err(type_error(pos, "char", other.type_name())),
    }
}

fn render_list(items: &[Value], mode: RenderMode) -> String {
    let mut rendered = String::from("(");

    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&item.render_with_mode(mode));
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

fn render_char(value: char, mode: RenderMode) -> String {
    match mode {
        RenderMode::Write => format!("#\\{}", render_char_name(value)),
        RenderMode::Display => value.to_string(),
    }
}

fn render_char_name(value: char) -> String {
    match value {
        ' ' => "space".into(),
        '\n' => "newline".into(),
        other => other.to_string(),
    }
}

#[cfg(test)]
mod tests;
