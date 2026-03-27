pub mod error;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use error::EvalError;
use error::SourcePos;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Integer(i64, SourcePos),
    Boolean(bool, SourcePos),
    String(String, SourcePos),
    Char(char, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    fn pos(&self) -> SourcePos {
        match self {
            Self::Integer(_, pos)
            | Self::Boolean(_, pos)
            | Self::String(_, pos)
            | Self::Char(_, pos)
            | Self::Symbol(_, pos)
            | Self::List(_, pos) => *pos,
        }
    }
}

#[derive(Clone)]
struct SchemeString {
    value: Rc<RefCell<String>>,
}

impl SchemeString {
    fn new(value: impl Into<String>) -> Self {
        Self {
            value: Rc::new(RefCell::new(value.into())),
        }
    }

    fn contents(&self) -> String {
        self.value.borrow().clone()
    }

    fn len_chars(&self) -> usize {
        self.value.borrow().chars().count()
    }

    fn char_at(&self, index: usize) -> Option<char> {
        self.value.borrow().chars().nth(index)
    }

    fn copy_string(&self) -> Self {
        Self::new(self.contents())
    }

    fn set_char(&self, index: usize, ch: char) -> bool {
        let mut value = self.value.borrow_mut();
        let Some((start, end)) = char_range(&value, index) else {
            return false;
        };
        let mut encoded = [0_u8; 4];
        value.replace_range(start..end, ch.encode_utf8(&mut encoded));
        true
    }
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(SchemeString),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
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
            Self::String(value) => render_string(&value.contents()),
            Self::Symbol(value) => value.clone(),
            Self::Char(value) => render_char(*value),
            Self::List(values) => render_list(values),
            Self::Pair(car, cdr) => render_pair(car, cdr),
            Self::Procedure(_) => "#<procedure>".to_owned(),
            Self::Void => "#<void>".to_owned(),
        }
    }

    fn render_display(&self) -> String {
        match self {
            Self::String(value) => value.contents(),
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
    EqPred,
    EqualPred,
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
    StringCopy,
    StringSet,
    CharPred,
    CharAlphabeticPred,
    CharNumericPred,
    CharUpcase,
    CharDowncase,
    CharEqual,
    CharLessThan,
    Apply,
    Map,
    Abs,
    Modulo,
    Remainder,
    Quotient,
    Min,
    Max,
    Expt,
    ZeroPred,
    PositivePred,
    NegativePred,
    OddPred,
    EvenPred,
    ListRef,
    ListTail,
    ListPred,
    Assoc,
    StringEqual,
    StringLessThan,
    StringCiEqual,
    StringUpcase,
    StringDowncase,
}

#[derive(Clone)]
enum Procedure {
    Builtin(Builtin),
    Lambda(Lambda),
}

#[derive(Clone)]
struct Lambda {
    name: Option<String>,
    params: LambdaParams,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone)]
struct LambdaParams {
    fixed: Vec<String>,
    rest: Option<String>,
}

impl LambdaParams {
    fn expected_args(&self) -> String {
        match &self.rest {
            Some(_) => format!("at least {} arguments", self.fixed.len()),
            None => format!("exactly {} arguments", self.fixed.len()),
        }
    }
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

    fn set(&self, name: &str, value: Value) -> bool {
        {
            let mut bindings = self.bindings.borrow_mut();
            if let Some(slot) = bindings.get_mut(name) {
                *slot = value;
                return true;
            }
        }

        self.parent
            .as_ref()
            .is_some_and(|parent| parent.set(name, value))
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

fn render_pair(car: &Value, cdr: &Value) -> String {
    let mut rendered = String::from("(");
    render_pair_contents(car, cdr, &mut rendered);
    rendered.push(')');
    rendered
}

fn render_pair_contents(car: &Value, cdr: &Value, rendered: &mut String) {
    rendered.push_str(&car.render());

    match cdr {
        Value::List(items) => {
            for value in items {
                rendered.push(' ');
                rendered.push_str(&value.render());
            }
        }
        Value::Pair(next_car, next_cdr) => {
            rendered.push(' ');
            render_pair_contents(next_car, next_cdr, rendered);
        }
        other => {
            rendered.push_str(" . ");
            rendered.push_str(&other.render());
        }
    }
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".to_owned(),
        '\n' => "#\\newline".to_owned(),
        _ => format!("#\\{value}"),
    }
}

fn char_range(value: &str, index: usize) -> Option<(usize, usize)> {
    let mut indices = value.char_indices();
    let (start, _) = indices.nth(index)?;
    let end = indices.next().map_or(value.len(), |(end, _)| end);
    Some((start, end))
}

fn parse_char_literal(atom: &str) -> Option<char> {
    let suffix = atom.strip_prefix("#\\")?;

    match suffix {
        "space" => Some(' '),
        "newline" => Some('\n'),
        _ => {
            let mut chars = suffix.chars();
            let ch = chars.next()?;
            if chars.next().is_some() {
                None
            } else {
                Some(ch)
            }
        }
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

        if let Some(value) = parse_char_literal(atom) {
            return Ok(Expr::Char(value, start));
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
        ("eq?", Builtin::EqPred),
        ("equal?", Builtin::EqualPred),
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
        ("list?", Builtin::ListPred),
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
        ("string=?", Builtin::StringEqual),
        ("string<?", Builtin::StringLessThan),
        ("string-ci=?", Builtin::StringCiEqual),
        ("string-upcase", Builtin::StringUpcase),
        ("string-downcase", Builtin::StringDowncase),
        ("string-copy", Builtin::StringCopy),
        ("string-set!", Builtin::StringSet),
        ("char?", Builtin::CharPred),
        ("char-alphabetic?", Builtin::CharAlphabeticPred),
        ("char-numeric?", Builtin::CharNumericPred),
        ("char-upcase", Builtin::CharUpcase),
        ("char-downcase", Builtin::CharDowncase),
        ("char=?", Builtin::CharEqual),
        ("char<?", Builtin::CharLessThan),
        ("apply", Builtin::Apply),
        ("map", Builtin::Map),
        ("abs", Builtin::Abs),
        ("modulo", Builtin::Modulo),
        ("remainder", Builtin::Remainder),
        ("quotient", Builtin::Quotient),
        ("min", Builtin::Min),
        ("max", Builtin::Max),
        ("expt", Builtin::Expt),
        ("zero?", Builtin::ZeroPred),
        ("positive?", Builtin::PositivePred),
        ("negative?", Builtin::NegativePred),
        ("odd?", Builtin::OddPred),
        ("even?", Builtin::EvenPred),
        ("list-ref", Builtin::ListRef),
        ("list-tail", Builtin::ListTail),
        ("assoc", Builtin::Assoc),
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
        Expr::String(value, _) => Ok(Value::String(SchemeString::new(value.clone()))),
        Expr::Char(value, _) => Ok(Value::Char(*value)),
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
            "set!" => return eval_set(args, env),
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
    let values = eval_args(args, env)?;
    apply_values(operator, &values, env)
}

fn apply_values(operator: Value, args: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    match operator {
        Value::Procedure(procedure) => match procedure.as_ref() {
            Procedure::Builtin(builtin) => apply_builtin(*builtin, args, env),
            Procedure::Lambda(lambda) => apply_lambda(lambda, args),
        },
        _ => Err(EvalError::InvalidApplication),
    }
}

fn apply_builtin(builtin: Builtin, values: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    match builtin {
        Builtin::Add => eval_add(values),
        Builtin::Sub => eval_sub(values),
        Builtin::Mul => eval_mul(values),
        Builtin::Div => eval_div(values),
        Builtin::EqPred => eval_eq_pred(values),
        Builtin::EqualPred => eval_equal_pred(values),
        Builtin::LessThan => eval_compare("<", values, |left, right| left < right),
        Builtin::GreaterThan => eval_compare(">", values, |left, right| left > right),
        Builtin::Equal => eval_compare("=", values, |left, right| left == right),
        Builtin::LessThanOrEqual => eval_compare("<=", values, |left, right| left <= right),
        Builtin::Not => eval_not(values),
        Builtin::Cons => eval_cons(values),
        Builtin::Car => eval_car(values),
        Builtin::Cdr => eval_cdr(values),
        Builtin::Append => eval_append(values),
        Builtin::List => eval_list_builtin(values),
        Builtin::Length => eval_length(values),
        Builtin::NullPred => eval_null_pred(values),
        Builtin::PairPred => eval_pair_pred(values),
        Builtin::ListPred => eval_list_pred(values),
        Builtin::SymbolPred => eval_symbol_pred(values),
        Builtin::StringPred => eval_string_pred(values),
        Builtin::NumberPred => eval_number_pred(values),
        Builtin::BooleanPred => eval_boolean_pred(values),
        Builtin::Display => eval_display(values, env),
        Builtin::Write => eval_write(values, env),
        Builtin::Newline => eval_newline(values, env),
        Builtin::StringAppend => eval_string_append(values),
        Builtin::StringLength => eval_string_length(values),
        Builtin::Substring => eval_substring(values),
        Builtin::StringToNumber => eval_string_to_number(values),
        Builtin::NumberToString => eval_number_to_string(values),
        Builtin::SymbolToString => eval_symbol_to_string(values),
        Builtin::StringToSymbol => eval_string_to_symbol(values),
        Builtin::StringRef => eval_string_ref(values),
        Builtin::StringEqual => {
            eval_string_compare("string=?", values, |left, right| left == right)
        }
        Builtin::StringLessThan => {
            eval_string_compare("string<?", values, |left, right| left < right)
        }
        Builtin::StringCiEqual => eval_string_compare("string-ci=?", values, |left, right| {
            left.to_lowercase() == right.to_lowercase()
        }),
        Builtin::StringUpcase => eval_string_upcase(values),
        Builtin::StringDowncase => eval_string_downcase(values),
        Builtin::StringCopy => eval_string_copy(values),
        Builtin::StringSet => eval_string_set(values),
        Builtin::CharPred => eval_char_pred(values),
        Builtin::CharAlphabeticPred => eval_char_alphabetic_pred(values),
        Builtin::CharNumericPred => eval_char_numeric_pred(values),
        Builtin::CharUpcase => eval_char_upcase(values),
        Builtin::CharDowncase => eval_char_downcase(values),
        Builtin::CharEqual => eval_char_compare("char=?", values, |left, right| left == right),
        Builtin::CharLessThan => eval_char_compare("char<?", values, |left, right| left < right),
        Builtin::Apply => eval_apply_builtin(values, env),
        Builtin::Map => eval_map_builtin(values, env),
        Builtin::Abs => eval_abs(values),
        Builtin::Modulo => eval_modulo(values),
        Builtin::Remainder => eval_remainder(values),
        Builtin::Quotient => eval_quotient(values),
        Builtin::Min => eval_min(values),
        Builtin::Max => eval_max(values),
        Builtin::Expt => eval_expt(values),
        Builtin::ZeroPred => eval_integer_predicate(values, "zero?", |value| value == 0),
        Builtin::PositivePred => eval_integer_predicate(values, "positive?", |value| value > 0),
        Builtin::NegativePred => eval_integer_predicate(values, "negative?", |value| value < 0),
        Builtin::OddPred => eval_integer_predicate(values, "odd?", |value| value % 2 != 0),
        Builtin::EvenPred => eval_integer_predicate(values, "even?", |value| value % 2 == 0),
        Builtin::ListRef => eval_list_ref(values),
        Builtin::ListTail => eval_list_tail(values),
        Builtin::Assoc => eval_assoc(values),
    }
}

fn apply_lambda(lambda: &Lambda, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() < lambda.params.fixed.len()
        || (lambda.params.rest.is_none() && args.len() != lambda.params.fixed.len())
    {
        return Err(EvalError::WrongArgCount {
            name: lambda.name.clone().unwrap_or_else(|| "lambda".to_owned()),
            expected: lambda.params.expected_args(),
            got: args.len(),
        });
    }

    let call_env = Environment::new(Some(lambda.env.clone()));

    for (param, value) in lambda
        .params
        .fixed
        .iter()
        .cloned()
        .zip(args.iter().take(lambda.params.fixed.len()).cloned())
    {
        call_env.define(param, value);
    }

    if let Some(rest) = &lambda.params.rest {
        call_env.define(
            rest.clone(),
            Value::List(args[lambda.params.fixed.len()..].to_vec()),
        );
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

fn eval_set(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "set!".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let name = expect_symbol(&args[0], "set! target")?;
    let value = eval_expr(&args[1], env)?;

    if env.set(&name, value) {
        Ok(Value::Void)
    } else {
        Err(EvalError::UnboundSymbol(name))
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
        Expr::String(value, _) => Value::String(SchemeString::new(value.clone())),
        Expr::Char(value, _) => Value::Char(*value),
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
    let params = LambdaParams {
        fixed: params,
        rest: None,
    };
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
    for (param, value) in params.fixed.into_iter().zip(values) {
        call_env.define(param, value);
    }

    eval_sequence(body, &call_env)
}

fn parse_params(params: &[Expr], form: &str) -> Result<LambdaParams, EvalError> {
    let mut fixed = Vec::new();
    let mut rest = None;
    let mut index = 0;

    while index < params.len() {
        match &params[index] {
            Expr::Symbol(name, _) if name == "." => {
                if rest.is_some() || index + 1 >= params.len() || index + 2 != params.len() {
                    return Err(EvalError::Parse(format!(
                        "{form} parameters use invalid dotted form"
                    )));
                }
                rest = Some(expect_symbol(
                    &params[index + 1],
                    &format!("{form} rest parameter"),
                )?);
                break;
            }
            expr => fixed.push(expect_symbol(expr, &format!("{form} parameter"))?),
        }
        index += 1;
    }

    Ok(LambdaParams { fixed, rest })
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

fn eval_apply_builtin(args: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "apply".to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let operator = args[0].clone();
    let mut applied_args = args[1..args.len() - 1].to_vec();
    let tail = expect_list("apply", &args[args.len() - 1])?;
    applied_args.extend(tail.iter().cloned());

    apply_values(operator, &applied_args, env)
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

    match &args[1] {
        Value::List(tail) => {
            let mut tail = tail.clone();
            tail.insert(0, args[0].clone());
            Ok(Value::List(tail))
        }
        other => Ok(Value::Pair(
            Box::new(args[0].clone()),
            Box::new(other.clone()),
        )),
    }
}

fn eval_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "car".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::Pair(car, _) => Ok((**car).clone()),
        _ => Err(EvalError::ExpectedPair {
            name: "car".to_owned(),
        }),
    }
}

fn eval_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "cdr".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        Value::Pair(_, cdr) => Ok((**cdr).clone()),
        _ => Err(EvalError::ExpectedPair {
            name: "cdr".to_owned(),
        }),
    }
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
    eval_type_predicate(args, "pair?", |value| {
        matches!(value, Value::List(items) if !items.is_empty())
            || matches!(value, Value::Pair(_, _))
    })
}

fn eval_list_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "list?", |value| matches!(value, Value::List(_)))
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
        combined.push_str(&expect_string("string-append", value)?.contents());
    }

    Ok(Value::String(SchemeString::new(combined)))
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
    Ok(Value::Integer(value.len_chars() as i64))
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
    let chars = string.contents().chars().collect::<Vec<_>>();
    let len = chars.len();

    if start < 0 || end < start || end as usize > len {
        return Err(EvalError::InvalidRange {
            name: "substring".to_owned(),
            start,
            end,
            len,
        });
    }

    Ok(Value::String(SchemeString::new(
        chars[start as usize..end as usize]
            .iter()
            .collect::<String>(),
    )))
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
    Ok(match value.contents().parse::<i64>() {
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

    Ok(Value::String(SchemeString::new(
        args[0].as_integer("number->string")?.to_string(),
    )))
}

fn eval_symbol_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "symbol->string".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::String(SchemeString::new(
        expect_symbol_value("symbol->string", &args[0])?.to_owned(),
    )))
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
        expect_string("string->symbol", &args[0])?.contents(),
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
    let len = string.len_chars();

    if index < 0 {
        return Err(EvalError::IndexOutOfBounds {
            name: "string-ref".to_owned(),
            index,
            len,
        });
    }

    let Some(ch) = string.char_at(index as usize) else {
        return Err(EvalError::IndexOutOfBounds {
            name: "string-ref".to_owned(),
            index,
            len,
        });
    };

    Ok(Value::Char(ch))
}

fn eval_string_copy(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string-copy".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::String(
        expect_string("string-copy", &args[0])?.copy_string(),
    ))
}

fn eval_string_set(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "string-set!".to_owned(),
            expected: "exactly 3 arguments".to_owned(),
            got: args.len(),
        });
    }

    let string = expect_string("string-set!", &args[0])?;
    let index = args[1].as_integer("string-set!")?;
    let ch = expect_char("string-set!", &args[2])?;
    let len = string.len_chars();

    if index < 0 || !string.set_char(index as usize, ch) {
        return Err(EvalError::IndexOutOfBounds {
            name: "string-set!".to_owned(),
            index,
            len,
        });
    }

    Ok(Value::Void)
}

fn eval_char_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_type_predicate(args, "char?", |value| matches!(value, Value::Char(_)))
}

fn eval_char_alphabetic_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_char_predicate(args, "char-alphabetic?", |value| value.is_alphabetic())
}

fn eval_char_numeric_pred(args: &[Value]) -> Result<Value, EvalError> {
    eval_char_predicate(args, "char-numeric?", |value| value.is_numeric())
}

fn eval_char_upcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "char-upcase".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let ch = expect_char("char-upcase", &args[0])?;
    Ok(Value::Char(ch.to_uppercase().next().unwrap_or(ch)))
}

fn eval_char_downcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "char-downcase".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    let ch = expect_char("char-downcase", &args[0])?;
    Ok(Value::Char(ch.to_lowercase().next().unwrap_or(ch)))
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

fn eval_eq_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "eq?".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(values_eq(&args[0], &args[1])))
}

fn eval_equal_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "equal?".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(values_equal(&args[0], &args[1])))
}

fn eval_abs(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "abs".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Integer(args[0].as_integer("abs")?.abs()))
}

fn eval_modulo(args: &[Value]) -> Result<Value, EvalError> {
    let (dividend, divisor) = eval_integer_pair("modulo", args)?;

    if divisor == 0 {
        return Err(EvalError::DivisionByZero);
    }

    let mut result = dividend % divisor;
    if result != 0 && ((result > 0) != (divisor > 0)) {
        result += divisor;
    }

    Ok(Value::Integer(result))
}

fn eval_remainder(args: &[Value]) -> Result<Value, EvalError> {
    let (dividend, divisor) = eval_integer_pair("remainder", args)?;

    if divisor == 0 {
        return Err(EvalError::DivisionByZero);
    }

    Ok(Value::Integer(dividend % divisor))
}

fn eval_quotient(args: &[Value]) -> Result<Value, EvalError> {
    let (dividend, divisor) = eval_integer_pair("quotient", args)?;

    if divisor == 0 {
        return Err(EvalError::DivisionByZero);
    }

    Ok(Value::Integer(dividend / divisor))
}

fn eval_min(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_integer_args("min", args)?;
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| EvalError::WrongArgCount {
            name: "min".to_owned(),
            expected: "at least 1 argument".to_owned(),
            got: 0,
        })?;

    Ok(Value::Integer(
        rest.iter().fold(*first, |acc, value| acc.min(*value)),
    ))
}

fn eval_max(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_integer_args("max", args)?;
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| EvalError::WrongArgCount {
            name: "max".to_owned(),
            expected: "at least 1 argument".to_owned(),
            got: 0,
        })?;

    Ok(Value::Integer(
        rest.iter().fold(*first, |acc, value| acc.max(*value)),
    ))
}

fn eval_expt(args: &[Value]) -> Result<Value, EvalError> {
    let (base, exponent) = eval_integer_pair("expt", args)?;

    if exponent < 0 {
        return Err(EvalError::InvalidArgument {
            name: "expt".to_owned(),
            message: "exponent must be non-negative".to_owned(),
        });
    }

    Ok(Value::Integer(base.pow(exponent as u32)))
}

fn eval_integer_predicate<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(predicate(args[0].as_integer(name)?)))
}

fn eval_list_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "list-ref".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let items = expect_list("list-ref", &args[0])?;
    let index = args[1].as_integer("list-ref")?;

    if index < 0 || index as usize >= items.len() {
        return Err(EvalError::IndexOutOfBounds {
            name: "list-ref".to_owned(),
            index,
            len: items.len(),
        });
    }

    Ok(items[index as usize].clone())
}

fn eval_list_tail(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "list-tail".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let items = expect_list("list-tail", &args[0])?;
    let index = args[1].as_integer("list-tail")?;

    if index < 0 || index as usize > items.len() {
        return Err(EvalError::IndexOutOfBounds {
            name: "list-tail".to_owned(),
            index,
            len: items.len(),
        });
    }

    Ok(Value::List(items[index as usize..].to_vec()))
}

fn eval_assoc(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "assoc".to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let key = &args[0];
    let entries = expect_list("assoc", &args[1])?;

    for entry in entries {
        let candidate = match entry {
            Value::List(items) if !items.is_empty() => &items[0],
            Value::Pair(car, _) => car.as_ref(),
            _ => {
                return Err(EvalError::ExpectedPair {
                    name: "assoc".to_owned(),
                });
            }
        };

        if values_equal(candidate, key) {
            return Ok(entry.clone());
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_map_builtin(args: &[Value], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "map".to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let operator = args[0].clone();
    let lists = args[1..]
        .iter()
        .map(|value| expect_list("map", value))
        .collect::<Result<Vec<_>, _>>()?;
    let len = lists.first().map_or(0, |list| list.len());

    if lists.iter().any(|list| list.len() != len) {
        return Err(EvalError::InvalidArgument {
            name: "map".to_owned(),
            message: "list arguments must have the same length".to_owned(),
        });
    }

    let mut results = Vec::with_capacity(len);
    for index in 0..len {
        let call_args = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        results.push(apply_values(operator.clone(), &call_args, env)?);
    }

    Ok(Value::List(results))
}

fn eval_integer_args(name: &str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|value| value.as_integer(name)).collect()
}

fn eval_integer_pair(name: &str, args: &[Value]) -> Result<(i64, i64), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "exactly 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    Ok((args[0].as_integer(name)?, args[1].as_integer(name)?))
}

fn eval_string_compare<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(&str, &str) -> bool,
{
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let values = args
        .iter()
        .map(|value| expect_string(name, value).map(|string| string.contents()))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Value::Boolean(
        values
            .windows(2)
            .all(|window| predicate(&window[0], &window[1])),
    ))
}

fn eval_string_upcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string-upcase".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::String(SchemeString::new(
        expect_string("string-upcase", &args[0])?
            .contents()
            .to_uppercase(),
    )))
}

fn eval_string_downcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string-downcase".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::String(SchemeString::new(
        expect_string("string-downcase", &args[0])?
            .contents()
            .to_lowercase(),
    )))
}

fn eval_char_compare<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(char, char) -> bool,
{
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: args.len(),
        });
    }

    let values = args
        .iter()
        .map(|value| expect_char(name, value))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Value::Boolean(
        values
            .windows(2)
            .all(|window| predicate(window[0], window[1])),
    ))
}

fn eval_char_predicate<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(char) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: name.to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(predicate(expect_char(name, &args[0])?)))
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

fn expect_string<'a>(name: &str, value: &'a Value) -> Result<&'a SchemeString, EvalError> {
    match value {
        Value::String(value) => Ok(value),
        _ => Err(EvalError::ExpectedString {
            name: name.to_owned(),
        }),
    }
}

fn expect_char(name: &str, value: &Value) -> Result<char, EvalError> {
    match value {
        Value::Char(value) => Ok(*value),
        _ => Err(EvalError::ExpectedChar {
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

fn values_eq(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Integer(left), Value::Integer(right)) => left == right,
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::String(left), Value::String(right)) => Rc::ptr_eq(&left.value, &right.value),
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn values_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Integer(left), Value::Integer(right)) => left == right,
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => left.contents() == right.contents(),
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| values_equal(left, right))
        }
        (Value::Pair(left_car, left_cdr), Value::Pair(right_car, right_cdr)) => {
            values_equal(left_car, right_car) && values_equal(left_cdr, right_cdr)
        }
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
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
