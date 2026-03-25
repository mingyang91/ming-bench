pub mod error;
mod macros;
mod number;

pub use error::EvalError;

use macros::{expand_macro_call, parse_macro_transformer, MacroTransformer};
use number::Number;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Number(Number),
    Boolean(bool),
    String(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

type RecordTypeRef = Rc<RecordType>;

#[derive(Clone)]
struct RecordType {
    name: String,
}

#[derive(Clone)]
struct RecordInstance {
    record_type: RecordTypeRef,
    fields: Vec<Value>,
}

#[derive(Clone)]
enum Value {
    Number(Number),
    Boolean(bool),
    String(SchemeString),
    Char(char),
    Symbol(String),
    EmptyList,
    Pair(Box<Value>, Box<Value>),
    Record(Rc<RecordInstance>),
    Procedure(Rc<Procedure>),
    Void,
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

#[derive(Clone)]
struct SchemeString {
    chars: Rc<RefCell<Vec<char>>>,
}

impl SchemeString {
    fn new(value: impl AsRef<str>) -> Self {
        Self {
            chars: Rc::new(RefCell::new(value.as_ref().chars().collect())),
        }
    }

    fn from_chars(chars: Vec<char>) -> Self {
        Self {
            chars: Rc::new(RefCell::new(chars)),
        }
    }

    fn to_plain_string(&self) -> String {
        self.chars.borrow().iter().collect()
    }

    fn len(&self) -> usize {
        self.chars.borrow().len()
    }

    fn deep_copy(&self) -> Self {
        Self::from_chars(self.chars.borrow().clone())
    }

    fn substring(&self, start: usize, end: usize) -> Result<Self, EvalError> {
        let chars = self.chars.borrow();

        if start > end || end > chars.len() {
            return Err(EvalError::InvalidRange {
                kind: "substring",
                start,
                end,
                length: chars.len(),
            });
        }

        Ok(Self::from_chars(chars[start..end].to_vec()))
    }

    fn get(&self, index: usize, kind: &'static str) -> Result<char, EvalError> {
        let chars = self.chars.borrow();

        chars
            .get(index)
            .copied()
            .ok_or(EvalError::IndexOutOfBounds {
                kind,
                index,
                length: chars.len(),
            })
    }

    fn set(&self, index: usize, value: char, kind: &'static str) -> Result<(), EvalError> {
        let mut chars = self.chars.borrow_mut();
        let length = chars.len();
        let slot = chars.get_mut(index).ok_or(EvalError::IndexOutOfBounds {
            kind,
            index,
            length,
        })?;
        *slot = value;
        Ok(())
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Number(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
            Self::Char(_) => "char",
            Self::Symbol(_) => "symbol",
            Self::EmptyList | Self::Pair(_, _) => "pair",
            Self::Record(_) => "record",
            Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn render(&self) -> String {
        self.render_with_mode(RenderMode::Write)
    }

    fn render_for_display(&self) -> String {
        self.render_with_mode(RenderMode::Display)
    }

    fn render_with_mode(&self, mode: RenderMode) -> String {
        match self {
            Self::Number(value) => value.render(),
            Self::Boolean(true) => "#t".to_string(),
            Self::Boolean(false) => "#f".to_string(),
            Self::String(value) => {
                let text = value.to_plain_string();
                match mode {
                    RenderMode::Write => render_string(&text),
                    RenderMode::Display => text,
                }
            }
            Self::Char(value) => match mode {
                RenderMode::Write => render_char(*value),
                RenderMode::Display => value.to_string(),
            },
            Self::Symbol(value) => value.clone(),
            Self::EmptyList => "()".to_string(),
            Self::Pair(_, _) => render_pair(self, mode),
            Self::Record(record) => format!("#<record {}>", record.record_type.name),
            Self::Procedure(_) => "#<procedure>".to_string(),
            Self::Void => "#<void>".to_string(),
        }
    }
}

fn render_pair(value: &Value, mode: RenderMode) -> String {
    let mut out = String::from("(");
    let mut cursor = value;

    loop {
        let Value::Pair(car, cdr) = cursor else {
            break;
        };

        out.push_str(&car.render_with_mode(mode));

        match cdr.as_ref() {
            Value::EmptyList => break,
            Value::Pair(_, _) => {
                out.push(' ');
                cursor = cdr.as_ref();
            }
            other => {
                out.push_str(" . ");
                out.push_str(&other.render_with_mode(mode));
                break;
            }
        }
    }

    out.push(')');
    out
}

fn render_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');

    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            other => out.push(other),
        }
    }

    out.push('"');
    out
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        other => format!("#\\{other}"),
    }
}

#[derive(Default)]
struct EvalContext {
    output: String,
    capture_output: bool,
    gensym_counter: u64,
}

impl EvalContext {
    fn new(capture_output: bool) -> Self {
        Self {
            output: String::new(),
            capture_output,
            gensym_counter: 0,
        }
    }

    fn emit(&mut self, text: &str) {
        if self.capture_output {
            self.output.push_str(text);
        }
    }

    fn finish(self) -> String {
        self.output
    }

    fn fresh_identifier(&mut self, name: &str) -> String {
        self.gensym_counter += 1;
        format!("__macro_{}_{}", self.gensym_counter, name)
    }
}

type EnvRef = Rc<RefCell<Env>>;
type BindingCell = Rc<RefCell<Value>>;
type BuiltinFn = fn(&[Value], &mut EvalContext) -> Result<Value, EvalError>;

struct Env {
    parent: Option<EnvRef>,
    bindings: HashMap<String, BindingCell>,
    macros: HashMap<String, Rc<MacroTransformer>>,
}

impl Env {
    fn new() -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: None,
            bindings: HashMap::new(),
            macros: HashMap::new(),
        }))
    }

    fn child(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
            macros: HashMap::new(),
        }))
    }
}

#[derive(Clone)]
enum Procedure {
    Builtin(BuiltinProcedure),
    Lambda(LambdaProcedure),
    RecordConstructor(RecordConstructorProcedure),
    RecordPredicate(RecordPredicateProcedure),
    RecordAccessor(RecordAccessorProcedure),
}

#[derive(Clone)]
struct BuiltinProcedure {
    implementation: BuiltinFn,
}

#[derive(Clone)]
struct LambdaProcedure {
    name: String,
    params: ParameterList,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone)]
struct RecordConstructorProcedure {
    name: String,
    record_type: RecordTypeRef,
    field_count: usize,
}

#[derive(Clone)]
struct RecordPredicateProcedure {
    name: String,
    record_type: RecordTypeRef,
}

#[derive(Clone)]
struct RecordAccessorProcedure {
    name: String,
    record_type: RecordTypeRef,
    field_index: usize,
}

#[derive(Clone)]
struct ParameterList {
    required: Vec<String>,
    rest: Option<String>,
}

impl ParameterList {
    fn expected_arity(&self) -> String {
        match self.rest {
            Some(_) => format!("at least {}", self.required.len()),
            None => self.required.len().to_string(),
        }
    }

    fn accepts(&self, arg_count: usize) -> bool {
        if self.rest.is_some() {
            arg_count >= self.required.len()
        } else {
            arg_count == self.required.len()
        }
    }
}

struct Parser<'a> {
    input: &'a str,
    index: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, index: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "expected at least one expression".to_string(),
            });
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some('\'') => self.parse_quote_shorthand(),
            Some('"') => self.parse_string(),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn parse_quote_shorthand(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        let quoted = self.parse_expr()?;
        Ok(Expr::List(vec![Expr::Symbol("quote".to_string()), quoted]))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek_char() {
                Some(')') => {
                    self.bump_char();
                    break;
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof),
            }
        }

        Ok(Expr::List(items))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        let mut out = String::new();

        loop {
            match self.bump_char() {
                Some('"') => return Ok(Expr::String(out)),
                Some('\\') => match self.bump_char() {
                    Some('"') => out.push('"'),
                    Some('\\') => out.push('\\'),
                    Some('n') => out.push('\n'),
                    Some('r') => out.push('\r'),
                    Some('t') => out.push('\t'),
                    Some(other) => out.push(other),
                    None => return Err(EvalError::UnterminatedString),
                },
                Some(ch) => out.push(ch),
                None => return Err(EvalError::UnterminatedString),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let token =
            self.take_while(|ch| !ch.is_whitespace() && ch != '(' && ch != ')' && ch != ';');

        if token.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "unexpected token".to_string(),
            });
        }

        match token {
            "#t" => Ok(Expr::Boolean(true)),
            "#f" => Ok(Expr::Boolean(false)),
            _ if token.starts_with("#\\") => {
                parse_char_token(token)
                    .map(Expr::Char)
                    .ok_or(EvalError::SyntaxError {
                        message: "invalid character literal".to_string(),
                    })
            }
            _ => {
                if let Some(value) = Number::parse_token(token)? {
                    Ok(Expr::Number(value))
                } else {
                    Ok(Expr::Symbol(token.to_string()))
                }
            }
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            self.take_while(|ch| ch.is_whitespace());

            if self.peek_char() == Some(';') {
                self.take_while(|ch| ch != '\n');
                continue;
            }

            break;
        }
    }

    fn take_while(&mut self, mut predicate: impl FnMut(char) -> bool) -> &'a str {
        let start = self.index;

        while let Some(ch) = self.peek_char() {
            if !predicate(ch) {
                break;
            }
            self.bump_char();
        }

        &self.input[start..self.index]
    }

    fn is_eof(&self) -> bool {
        self.index >= self.input.len()
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.index += ch.len_utf8();
        Some(ch)
    }
}

fn parse_char_token(token: &str) -> Option<char> {
    let literal = token.strip_prefix("#\\")?;

    match literal {
        "space" => Some(' '),
        "newline" => Some('\n'),
        _ => {
            let mut chars = literal.chars();
            let ch = chars.next()?;

            if chars.next().is_some() {
                None
            } else {
                Some(ch)
            }
        }
    }
}

fn line_col_at(input: &str, index: usize) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;

    for ch in input[..index.min(input.len())].chars() {
        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }

    (line, column)
}

fn default_env() -> EnvRef {
    let env = Env::new();

    define_builtin(&env, "+", builtin_add);
    define_builtin(&env, "-", builtin_sub);
    define_builtin(&env, "*", builtin_mul);
    define_builtin(&env, "/", builtin_div);
    define_builtin(&env, "<", builtin_less_than);
    define_builtin(&env, ">", builtin_greater_than);
    define_builtin(&env, "=", builtin_equal_numbers);
    define_builtin(&env, "<=", builtin_less_equal);
    define_builtin(&env, "eq?", builtin_eq);
    define_builtin(&env, "equal?", builtin_equal);
    define_builtin(&env, "not", builtin_not);
    define_builtin(&env, "cons", builtin_cons);
    define_builtin(&env, "car", builtin_car);
    define_builtin(&env, "cdr", builtin_cdr);
    define_builtin(&env, "null?", builtin_null_predicate);
    define_builtin(&env, "list", builtin_list);
    define_builtin(&env, "length", builtin_length);
    define_builtin(&env, "append", builtin_append);
    define_builtin(&env, "string?", builtin_string_predicate);
    define_builtin(&env, "number?", builtin_number_predicate);
    define_builtin(&env, "exact?", builtin_exact_predicate);
    define_builtin(&env, "inexact?", builtin_inexact_predicate);
    define_builtin(&env, "exact->inexact", builtin_exact_to_inexact);
    define_builtin(&env, "inexact->exact", builtin_inexact_to_exact);
    define_builtin(&env, "numerator", builtin_numerator);
    define_builtin(&env, "denominator", builtin_denominator);
    define_builtin(&env, "integer?", builtin_integer_predicate);
    define_builtin(&env, "rational?", builtin_rational_predicate);
    define_builtin(&env, "boolean?", builtin_boolean_predicate);
    define_builtin(&env, "pair?", builtin_pair_predicate);
    define_builtin(&env, "symbol?", builtin_symbol_predicate);
    define_builtin(&env, "display", builtin_display);
    define_builtin(&env, "write", builtin_write);
    define_builtin(&env, "newline", builtin_newline);
    define_builtin(&env, "apply", builtin_apply);
    define_builtin(&env, "string-append", builtin_string_append);
    define_builtin(&env, "string-length", builtin_string_length);
    define_builtin(&env, "substring", builtin_substring);
    define_builtin(&env, "string->number", builtin_string_to_number);
    define_builtin(&env, "number->string", builtin_number_to_string);
    define_builtin(&env, "symbol->string", builtin_symbol_to_string);
    define_builtin(&env, "string->symbol", builtin_string_to_symbol);
    define_builtin(&env, "string-ref", builtin_string_ref);
    define_builtin(&env, "string-copy", builtin_string_copy);
    define_builtin(&env, "string-set!", builtin_string_set);
    define_builtin(&env, "char?", builtin_char_predicate);
    define_builtin(&env, "abs", builtin_abs);
    define_builtin(&env, "modulo", builtin_modulo);
    define_builtin(&env, "remainder", builtin_remainder);
    define_builtin(&env, "quotient", builtin_quotient);
    define_builtin(&env, "min", builtin_min);
    define_builtin(&env, "max", builtin_max);
    define_builtin(&env, "expt", builtin_expt);
    define_builtin(&env, "zero?", builtin_zero_predicate);
    define_builtin(&env, "positive?", builtin_positive_predicate);
    define_builtin(&env, "negative?", builtin_negative_predicate);
    define_builtin(&env, "odd?", builtin_odd_predicate);
    define_builtin(&env, "even?", builtin_even_predicate);
    define_builtin(&env, "list-ref", builtin_list_ref);
    define_builtin(&env, "list-tail", builtin_list_tail);
    define_builtin(&env, "list?", builtin_list_predicate);
    define_builtin(&env, "assoc", builtin_assoc);
    define_builtin(&env, "map", builtin_map);
    define_builtin(&env, "char-alphabetic?", builtin_char_alphabetic_predicate);
    define_builtin(&env, "char-numeric?", builtin_char_numeric_predicate);
    define_builtin(&env, "char-upcase", builtin_char_upcase);
    define_builtin(&env, "char-downcase", builtin_char_downcase);
    define_builtin(&env, "char=?", builtin_char_equal);
    define_builtin(&env, "char<?", builtin_char_less_than);
    define_builtin(&env, "string=?", builtin_string_equal);
    define_builtin(&env, "string<?", builtin_string_less_than);
    define_builtin(&env, "string-ci=?", builtin_string_ci_equal);
    define_builtin(&env, "string-upcase", builtin_string_upcase);
    define_builtin(&env, "string-downcase", builtin_string_downcase);

    env
}

fn define_builtin(env: &EnvRef, name: &'static str, implementation: BuiltinFn) {
    let value = Value::Procedure(Rc::new(Procedure::Builtin(BuiltinProcedure {
        implementation,
    })));
    bind_value(env, name.to_string(), value);
}

fn lookup(env: &EnvRef, name: &str) -> Result<Value, EvalError> {
    Ok(lookup_binding_cell(env, name)?.borrow().clone())
}

fn bind_value(env: &EnvRef, name: String, value: Value) {
    env.borrow_mut()
        .bindings
        .insert(name, Rc::new(RefCell::new(value)));
}

fn bind_macro(env: &EnvRef, name: String, transformer: Rc<MacroTransformer>) {
    env.borrow_mut().macros.insert(name, transformer);
}

fn lookup_binding_cell(env: &EnvRef, name: &str) -> Result<BindingCell, EvalError> {
    lookup_binding_cell_opt(env, name).ok_or(EvalError::UnboundVariable {
        name: name.to_string(),
    })
}

fn lookup_binding_cell_opt(env: &EnvRef, name: &str) -> Option<BindingCell> {
    let (value, parent) = {
        let scope = env.borrow();
        (scope.bindings.get(name).cloned(), scope.parent.clone())
    };

    if let Some(value) = value {
        return Some(value);
    }

    if let Some(parent) = parent {
        return lookup_binding_cell_opt(&parent, name);
    }

    None
}

fn lookup_macro(env: &EnvRef, name: &str) -> Option<Rc<MacroTransformer>> {
    let (transformer, parent) = {
        let scope = env.borrow();
        (scope.macros.get(name).cloned(), scope.parent.clone())
    };

    if let Some(transformer) = transformer {
        return Some(transformer);
    }

    parent.and_then(|parent| lookup_macro(&parent, name))
}

fn assign(env: &EnvRef, name: &str, value: Value) -> Result<(), EvalError> {
    let cell = lookup_binding_cell(env, name)?;
    *cell.borrow_mut() = value;
    Ok(())
}

fn eval_program(exprs: &[Expr], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let env = default_env();
    eval_sequence(exprs, env, ctx)
}

fn eval_sequence(exprs: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Void;

    for expr in exprs {
        last = eval_expr(expr, env.clone(), ctx)?;
    }

    Ok(last)
}

fn eval_expr(expr: &Expr, env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    match expr {
        Expr::Number(value) => Ok(Value::Number(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(SchemeString::new(value))),
        Expr::Char(value) => Ok(Value::Char(*value)),
        Expr::Symbol(name) => lookup(&env, name),
        Expr::List(items) => eval_list(items, env, ctx),
    }
}

fn eval_list(items: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if items.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate an empty list".to_string(),
        });
    }

    if let Expr::Symbol(operator) = &items[0] {
        let args = &items[1..];

        match operator.as_str() {
            "and" => return eval_and(args, env, ctx),
            "or" => return eval_or(args, env, ctx),
            "if" => return eval_if(args, env, ctx),
            "begin" => return eval_begin(args, env, ctx),
            "cond" => return eval_cond(args, env, ctx),
            "quote" => return eval_quote(args),
            "define" => return eval_define(args, env, ctx),
            "define-record-type" => return eval_define_record_type(args, env),
            "define-syntax" => return eval_define_syntax(args, env),
            "set!" => return eval_set(args, env, ctx),
            "lambda" => return eval_lambda(args, env),
            "let" => return eval_let(args, env, ctx),
            _ => {}
        }

        if let Some(transformer) = lookup_macro(&env, operator) {
            let (expanded, expansion_env) = expand_macro_call(transformer, items, env, ctx)?;
            return eval_expr(&expanded, expansion_env, ctx);
        }
    }

    let operator = eval_expr(&items[0], env.clone(), ctx)?;
    apply(operator, &items[1..], env, ctx)
}

fn eval_and(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for expr in args {
        let value = eval_expr(expr, env.clone(), ctx)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval_expr(expr, env.clone(), ctx)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_if(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_expr_arity("if", args, 3)?;

    let condition = eval_expr(&args[0], env.clone(), ctx)?;
    if condition.is_truthy() {
        eval_expr(&args[1], env, ctx)
    } else {
        eval_expr(&args[2], env, ctx)
    }
}

fn eval_begin(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    eval_sequence(args, env, ctx)
}

fn eval_cond(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::SyntaxError {
                message: "cond clauses must be lists".to_string(),
            });
        };

        if items.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "cond clauses cannot be empty".to_string(),
            });
        }

        if let Expr::Symbol(symbol) = &items[0] {
            if symbol == "else" {
                if index + 1 != args.len() {
                    return Err(EvalError::SyntaxError {
                        message: "cond else clause must be last".to_string(),
                    });
                }
                if items.len() == 1 {
                    return Ok(Value::Void);
                }
                return eval_sequence(&items[1..], env, ctx);
            }
        }

        let test_value = eval_expr(&items[0], env.clone(), ctx)?;
        if test_value.is_truthy() {
            if items.len() == 1 {
                return Ok(test_value);
            }
            return eval_sequence(&items[1..], env, ctx);
        }
    }

    Ok(Value::Void)
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    expect_expr_arity("quote", args, 1)?;
    Ok(quote_expr(&args[0]))
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Number(value) => Value::Number(*value),
        Expr::Boolean(value) => Value::Boolean(*value),
        Expr::String(value) => Value::String(SchemeString::new(value)),
        Expr::Char(value) => Value::Char(*value),
        Expr::Symbol(name) => Value::Symbol(name.clone()),
        Expr::List(items) => items.iter().rev().fold(Value::EmptyList, |tail, item| {
            Value::Pair(Box::new(quote_expr(item)), Box::new(tail))
        }),
    }
}

fn eval_define(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "define requires a binding target".to_string(),
        });
    }

    match &args[0] {
        Expr::Symbol(name) => {
            expect_expr_arity("define", args, 2)?;
            let value = eval_expr(&args[1], env.clone(), ctx)?;
            bind_value(&env, name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) => {
            if signature.is_empty() {
                return Err(EvalError::SyntaxError {
                    message: "define requires a function name".to_string(),
                });
            }

            if args.len() < 2 {
                return Err(EvalError::SyntaxError {
                    message: "define requires a function body".to_string(),
                });
            }

            let Expr::Symbol(name) = &signature[0] else {
                return Err(EvalError::SyntaxError {
                    message: "define function name must be a symbol".to_string(),
                });
            };

            let params = parse_param_slice(&signature[1..])?;
            let lambda = make_lambda(name.clone(), params, args[1..].to_vec(), env.clone());
            bind_value(&env, name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::SyntaxError {
            message: "define requires a symbol or function signature".to_string(),
        }),
    }
}

fn eval_define_record_type(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() < 3 {
        return Err(EvalError::SyntaxError {
            message: "define-record-type requires a type name, constructor, and predicate"
                .to_string(),
        });
    }

    let type_name = expect_symbol_expr(&args[0], "define-record-type requires a symbol type name")?;
    let (constructor_name, field_count) = parse_record_constructor_spec(&args[1])?;
    let predicate_name = expect_symbol_expr(
        &args[2],
        "define-record-type requires a symbol predicate name",
    )?;
    let accessors = args[3..]
        .iter()
        .map(parse_record_field_spec)
        .collect::<Result<Vec<_>, _>>()?;

    if field_count != accessors.len() {
        return Err(EvalError::SyntaxError {
            message: format!(
                "define-record-type constructor for {type_name} expects {field_count} fields, got {}",
                accessors.len()
            ),
        });
    }

    let record_type = Rc::new(RecordType { name: type_name });

    bind_value(
        &env,
        constructor_name.clone(),
        make_record_constructor(constructor_name, record_type.clone(), field_count),
    );
    bind_value(
        &env,
        predicate_name.clone(),
        make_record_predicate(predicate_name, record_type.clone()),
    );

    for (field_index, accessor_name) in accessors.into_iter().enumerate() {
        bind_value(
            &env,
            accessor_name.clone(),
            make_record_accessor(accessor_name, record_type.clone(), field_index),
        );
    }

    Ok(Value::Void)
}

fn eval_define_syntax(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    expect_expr_arity("define-syntax", args, 2)?;

    let Expr::Symbol(name) = &args[0] else {
        return Err(EvalError::SyntaxError {
            message: "define-syntax requires a symbol name".to_string(),
        });
    };

    let transformer = parse_macro_transformer(name, &args[1], env.clone())?;
    bind_macro(&env, name.clone(), transformer);
    Ok(Value::Void)
}

fn eval_set(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_expr_arity("set!", args, 2)?;

    let Expr::Symbol(name) = &args[0] else {
        return Err(EvalError::SyntaxError {
            message: "set! requires a symbol target".to_string(),
        });
    };

    let value = eval_expr(&args[1], env.clone(), ctx)?;
    assign(&env, name, value)?;
    Ok(Value::Void)
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::SyntaxError {
            message: "lambda requires parameters and a body".to_string(),
        });
    }

    let params = parse_param_list(&args[0])?;
    Ok(make_lambda(
        "lambda".to_string(),
        params,
        args[1..].to_vec(),
        env,
    ))
}

fn eval_let(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires bindings".to_string(),
        });
    }

    match &args[0] {
        Expr::Symbol(name) => eval_named_let(name, &args[1..], env, ctx),
        bindings => eval_let_bindings(bindings, &args[1..], env, ctx),
    }
}

fn eval_named_let(
    name: &str,
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::SyntaxError {
            message: "named let requires bindings and a body".to_string(),
        });
    }

    let bindings = parse_let_bindings(&args[0])?;
    let values = eval_let_values(&bindings, env.clone(), ctx)?;
    let params = ParameterList {
        required: bindings.iter().map(|(param, _)| param.clone()).collect(),
        rest: None,
    };
    let named_env = Env::child(env);
    let lambda = make_lambda(
        name.to_string(),
        params,
        args[1..].to_vec(),
        named_env.clone(),
    );
    named_env
        .borrow_mut()
        .bindings
        .insert(name.to_string(), Rc::new(RefCell::new(lambda.clone())));
    apply_evaluated(lambda, values, ctx)
}

fn eval_let_bindings(
    bindings_expr: &Expr,
    body: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires a body".to_string(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let values = eval_let_values(&bindings, env.clone(), ctx)?;
    let let_env = Env::child(env);
    {
        let mut scope = let_env.borrow_mut();
        for ((name, _), value) in bindings.into_iter().zip(values) {
            scope.bindings.insert(name, Rc::new(RefCell::new(value)));
        }
    }
    eval_sequence(body, let_env, ctx)
}

fn parse_let_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings) = expr else {
        return Err(EvalError::SyntaxError {
            message: "let bindings must be a list".to_string(),
        });
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let Expr::List(parts) = binding else {
            return Err(EvalError::SyntaxError {
                message: "let bindings must be pairs".to_string(),
            });
        };

        if parts.len() != 2 {
            return Err(EvalError::SyntaxError {
                message: "let bindings must have a name and value".to_string(),
            });
        }

        let Expr::Symbol(name) = &parts[0] else {
            return Err(EvalError::SyntaxError {
                message: "let binding names must be symbols".to_string(),
            });
        };

        parsed.push((name.clone(), parts[1].clone()));
    }

    Ok(parsed)
}

fn eval_let_values(
    bindings: &[(String, Expr)],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::with_capacity(bindings.len());
    for (_, expr) in bindings {
        values.push(eval_expr(expr, env.clone(), ctx)?);
    }
    Ok(values)
}

fn parse_param_list(expr: &Expr) -> Result<ParameterList, EvalError> {
    let Expr::List(params) = expr else {
        return Err(EvalError::SyntaxError {
            message: "lambda parameters must be a list".to_string(),
        });
    };

    parse_param_slice(params)
}

fn expect_symbol_expr(expr: &Expr, message: &str) -> Result<String, EvalError> {
    let Expr::Symbol(name) = expr else {
        return Err(EvalError::SyntaxError {
            message: message.to_string(),
        });
    };

    Ok(name.clone())
}

fn parse_record_constructor_spec(expr: &Expr) -> Result<(String, usize), EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::SyntaxError {
            message: "define-record-type constructor must be a list".to_string(),
        });
    };

    let Some((name_expr, fields)) = items.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "define-record-type constructor cannot be empty".to_string(),
        });
    };

    let name = expect_symbol_expr(name_expr, "define-record-type constructor name must be a symbol")?;
    for field in fields {
        let _ = expect_symbol_expr(field, "define-record-type constructor fields must be symbols")?;
    }

    Ok((name, fields.len()))
}

fn parse_record_field_spec(expr: &Expr) -> Result<String, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::SyntaxError {
            message: "define-record-type field clauses must be lists".to_string(),
        });
    };

    if items.len() != 2 {
        return Err(EvalError::SyntaxError {
            message: "define-record-type field clauses must contain a field name and accessor"
                .to_string(),
        });
    }

    let _ = expect_symbol_expr(&items[0], "define-record-type field names must be symbols")?;
    expect_symbol_expr(&items[1], "define-record-type accessors must be symbols")
}

fn parse_param_slice(params: &[Expr]) -> Result<ParameterList, EvalError> {
    let mut required = Vec::with_capacity(params.len());
    let mut index = 0;

    while index < params.len() {
        let Expr::Symbol(name) = &params[index] else {
            return Err(EvalError::SyntaxError {
                message: "parameter names must be symbols".to_string(),
            });
        };

        if name == "." {
            if index + 2 != params.len() {
                return Err(EvalError::SyntaxError {
                    message: "dot notation requires exactly one rest parameter".to_string(),
                });
            }

            let Expr::Symbol(rest) = &params[index + 1] else {
                return Err(EvalError::SyntaxError {
                    message: "parameter names must be symbols".to_string(),
                });
            };

            if rest == "." {
                return Err(EvalError::SyntaxError {
                    message: "parameter names must be symbols".to_string(),
                });
            }

            return Ok(ParameterList {
                required,
                rest: Some(rest.clone()),
            });
        }

        required.push(name.clone());
        index += 1;
    }

    Ok(ParameterList {
        required,
        rest: None,
    })
}

fn make_lambda(name: String, params: ParameterList, body: Vec<Expr>, env: EnvRef) -> Value {
    Value::Procedure(Rc::new(Procedure::Lambda(LambdaProcedure {
        name,
        params,
        body,
        env,
    })))
}

fn make_record_constructor(name: String, record_type: RecordTypeRef, field_count: usize) -> Value {
    Value::Procedure(Rc::new(Procedure::RecordConstructor(
        RecordConstructorProcedure {
            name,
            record_type,
            field_count,
        },
    )))
}

fn make_record_predicate(name: String, record_type: RecordTypeRef) -> Value {
    Value::Procedure(Rc::new(Procedure::RecordPredicate(
        RecordPredicateProcedure { name, record_type },
    )))
}

fn make_record_accessor(name: String, record_type: RecordTypeRef, field_index: usize) -> Value {
    Value::Procedure(Rc::new(Procedure::RecordAccessor(RecordAccessorProcedure {
        name,
        record_type,
        field_index,
    })))
}

fn apply(
    operator: Value,
    arg_exprs: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    let Value::Procedure(procedure) = operator else {
        return Err(EvalError::NotAProcedure);
    };

    let args = eval_args(arg_exprs, env, ctx)?;

    apply_procedure(procedure, args, ctx)
}

fn apply_evaluated(
    operator: Value,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    let Value::Procedure(procedure) = operator else {
        return Err(EvalError::NotAProcedure);
    };

    apply_procedure(procedure, args, ctx)
}

fn apply_procedure(
    procedure: Rc<Procedure>,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    match procedure.as_ref() {
        Procedure::Builtin(builtin) => (builtin.implementation)(&args, ctx),
        Procedure::Lambda(lambda) => apply_lambda(lambda, args, ctx),
        Procedure::RecordConstructor(constructor) => apply_record_constructor(constructor, args),
        Procedure::RecordPredicate(predicate) => apply_record_predicate(predicate, args),
        Procedure::RecordAccessor(accessor) => apply_record_accessor(accessor, args),
    }
}

fn eval_args(
    arg_exprs: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Vec<Value>, EvalError> {
    let mut args = Vec::with_capacity(arg_exprs.len());

    for expr in arg_exprs {
        args.push(eval_expr(expr, env.clone(), ctx)?);
    }

    Ok(args)
}

fn apply_lambda(
    lambda: &LambdaProcedure,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    if !lambda.params.accepts(args.len()) {
        return Err(EvalError::WrongArgumentCount {
            name: lambda.name.clone(),
            expected: lambda.params.expected_arity(),
            got: args.len(),
        });
    }

    let call_env = Env::child(lambda.env.clone());
    {
        let mut scope = call_env.borrow_mut();
        let mut args = args.into_iter();

        for param in lambda.params.required.iter().cloned() {
            let arg = args
                .next()
                .expect("arity check should ensure enough arguments");
            scope.bindings.insert(param, Rc::new(RefCell::new(arg)));
        }

        if let Some(rest) = &lambda.params.rest {
            scope.bindings.insert(
                rest.clone(),
                Rc::new(RefCell::new(list_from_values(args.collect()))),
            );
        }
    }

    eval_sequence(&lambda.body, call_env, ctx)
}

fn apply_record_constructor(
    constructor: &RecordConstructorProcedure,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    if args.len() != constructor.field_count {
        return Err(EvalError::WrongArgumentCount {
            name: constructor.name.clone(),
            expected: constructor.field_count.to_string(),
            got: args.len(),
        });
    }

    Ok(Value::Record(Rc::new(RecordInstance {
        record_type: constructor.record_type.clone(),
        fields: args,
    })))
}

fn apply_record_predicate(
    predicate: &RecordPredicateProcedure,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgumentCount {
            name: predicate.name.clone(),
            expected: "1".to_string(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(matches!(
        args.first(),
        Some(Value::Record(record)) if Rc::ptr_eq(&record.record_type, &predicate.record_type)
    )))
}

fn apply_record_accessor(
    accessor: &RecordAccessorProcedure,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgumentCount {
            name: accessor.name.clone(),
            expected: "1".to_string(),
            got: args.len(),
        });
    }

    let record = expect_record_of_type(&args[0], &accessor.record_type, &accessor.name)?;
    Ok(record
        .fields
        .get(accessor.field_index)
        .cloned()
        .expect("record accessor index should be validated at definition time"))
}

fn builtin_not(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("not", args, 1)?;
    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn builtin_eq(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("eq?", args, 2)?;
    Ok(Value::Boolean(equal_values(&args[0], &args[1])))
}

fn builtin_equal(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("equal?", args, 2)?;
    Ok(Value::Boolean(equal_values(&args[0], &args[1])))
}

fn builtin_cons(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("cons", args, 2)?;
    Ok(Value::Pair(
        Box::new(args[0].clone()),
        Box::new(args[1].clone()),
    ))
}

fn builtin_car(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("car", args, 1)?;
    let Value::Pair(car, _) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            actual: args[0].type_name(),
        });
    };
    Ok((**car).clone())
}

fn builtin_cdr(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("cdr", args, 1)?;
    let Value::Pair(_, cdr) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            actual: args[0].type_name(),
        });
    };
    Ok((**cdr).clone())
}

fn builtin_null_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("null?", args, 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::EmptyList)))
}

fn builtin_list(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    Ok(args.iter().rev().fold(Value::EmptyList, |tail, value| {
        Value::Pair(Box::new(value.clone()), Box::new(tail))
    }))
}

fn builtin_length(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("length", args, 1)?;
    Ok(Value::Number(
        Number::integer(list_length(&args[0])? as i64),
    ))
}

fn builtin_append(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut result = args.last().cloned().unwrap_or(Value::EmptyList);

    for value in args[..args.len().saturating_sub(1)].iter().rev() {
        result = copy_list_with_tail(value, result)?;
    }

    Ok(result)
}

fn builtin_string_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("string?", args, |value| matches!(value, Value::String(_)))
}

fn builtin_number_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("number?", args, |value| matches!(value, Value::Number(_)))
}

fn builtin_exact_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate(
        "exact?",
        args,
        |value| matches!(value, Value::Number(number) if number.is_exact()),
    )
}

fn builtin_inexact_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate(
        "inexact?",
        args,
        |value| matches!(value, Value::Number(number) if number.is_inexact()),
    )
}

fn builtin_exact_to_inexact(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("exact->inexact", args, 1)?;
    Ok(Value::Number(expect_number(&args[0])?.exact_to_inexact()))
}

fn builtin_inexact_to_exact(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("inexact->exact", args, 1)?;
    Ok(Value::Number(expect_number(&args[0])?.inexact_to_exact()?))
}

fn builtin_numerator(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("numerator", args, 1)?;
    Ok(Value::Number(Number::integer(
        expect_number(&args[0])?.numerator()?,
    )))
}

fn builtin_denominator(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("denominator", args, 1)?;
    Ok(Value::Number(Number::integer(
        expect_number(&args[0])?.denominator()?,
    )))
}

fn builtin_integer_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate(
        "integer?",
        args,
        |value| matches!(value, Value::Number(number) if number.is_integer()),
    )
}

fn builtin_rational_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate(
        "rational?",
        args,
        |value| matches!(value, Value::Number(number) if number.is_rational()),
    )
}

fn builtin_boolean_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("boolean?", args, |value| matches!(value, Value::Boolean(_)))
}

fn builtin_pair_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("pair?", args, |value| matches!(value, Value::Pair(_, _)))
}

fn builtin_symbol_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("symbol?", args, |value| matches!(value, Value::Symbol(_)))
}

fn builtin_char_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("char?", args, |value| matches!(value, Value::Char(_)))
}

fn builtin_display(args: &[Value], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("display", args, 1)?;
    ctx.emit(&args[0].render_for_display());
    Ok(Value::Void)
}

fn builtin_write(args: &[Value], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("write", args, 1)?;
    ctx.emit(&args[0].render());
    Ok(Value::Void)
}

fn builtin_newline(args: &[Value], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("newline", args, 0)?;
    ctx.emit("\n");
    Ok(Value::Void)
}

fn builtin_apply(args: &[Value], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: "apply".to_string(),
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }

    let (operator, rest) = args
        .split_first()
        .expect("arity check should ensure operator");
    let mut applied_args = rest[..rest.len() - 1].to_vec();
    applied_args.extend(expect_list_values(&rest[rest.len() - 1])?);

    apply_evaluated(operator.clone(), applied_args, ctx)
}

fn builtin_string_append(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut result = String::new();

    for value in args {
        result.push_str(&expect_string(value)?.to_plain_string());
    }

    Ok(Value::String(SchemeString::new(result)))
}

fn builtin_string_length(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string-length", args, 1)?;
    Ok(Value::Number(Number::integer(
        expect_string(&args[0])?.len() as i64,
    )))
}

fn builtin_substring(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("substring", args, 3)?;
    let source = expect_string(&args[0])?;
    let start = expect_index(&args[1], "substring")?;
    let end = expect_index(&args[2], "substring")?;

    Ok(Value::String(source.substring(start, end)?))
}

fn builtin_string_to_number(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string->number", args, 1)?;
    let source = expect_string(&args[0])?.to_plain_string();
    Ok(Number::parse_token(&source)?
        .map(Value::Number)
        .unwrap_or(Value::Boolean(false)))
}

fn builtin_number_to_string(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("number->string", args, 1)?;
    Ok(Value::String(SchemeString::new(
        expect_number(&args[0])?.render(),
    )))
}

fn builtin_symbol_to_string(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("symbol->string", args, 1)?;
    let Value::Symbol(ref value) = args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "symbol",
            actual: args[0].type_name(),
        });
    };
    Ok(Value::String(SchemeString::new(value)))
}

fn builtin_string_to_symbol(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string->symbol", args, 1)?;
    Ok(Value::Symbol(expect_string(&args[0])?.to_plain_string()))
}

fn builtin_string_ref(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string-ref", args, 2)?;
    let source = expect_string(&args[0])?;
    let index = expect_index(&args[1], "string-ref")?;

    Ok(Value::Char(source.get(index, "string-ref")?))
}

fn builtin_string_copy(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string-copy", args, 1)?;
    Ok(Value::String(expect_string(&args[0])?.deep_copy()))
}

fn builtin_string_set(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string-set!", args, 3)?;
    let string = expect_string(&args[0])?;
    let index = expect_index(&args[1], "string-set!")?;
    let ch = expect_char(&args[2])?;

    string.set(index, ch, "string-set!")?;
    Ok(Value::Void)
}

fn builtin_abs(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("abs", args, 1)?;
    Ok(Value::Number(expect_number(&args[0])?.abs()?))
}

fn builtin_modulo(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let (dividend, divisor) = expect_two_exact_integers("modulo", args)?;

    let remainder = dividend % divisor;
    let result = if remainder != 0 && (remainder < 0) != (divisor < 0) {
        remainder + divisor
    } else {
        remainder
    };

    Ok(Value::Number(Number::integer(result)))
}

fn builtin_remainder(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let (dividend, divisor) = expect_two_exact_integers("remainder", args)?;
    Ok(Value::Number(Number::integer(dividend % divisor)))
}

fn builtin_quotient(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let (dividend, divisor) = expect_two_exact_integers("quotient", args)?;
    Ok(Value::Number(Number::integer(
        dividend
            .checked_div(divisor)
            .ok_or(EvalError::NumericOverflow {
                operation: "quotient",
            })?,
    )))
}

fn builtin_min(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_min_max("min", args, |left, right| {
        if left.compare(right).is_le() {
            left
        } else {
            right
        }
    })
}

fn builtin_max(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_min_max("max", args, |left, right| {
        if left.compare(right).is_ge() {
            left
        } else {
            right
        }
    })
}

fn builtin_expt(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("expt", args, 2)?;
    let base = expect_number(&args[0])?;
    let exponent = expect_exact_integer(&args[1], "expt")?;

    if exponent < 0 {
        return Err(EvalError::NegativeExponent { value: exponent });
    }

    let mut result = Number::integer(1);
    for _ in 0..exponent {
        result = result.mul(base)?;
    }

    Ok(Value::Number(result))
}

fn builtin_zero_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_number_predicate_by("zero?", args, Number::is_zero)
}

fn builtin_positive_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_number_predicate_by("positive?", args, Number::is_positive)
}

fn builtin_negative_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_number_predicate_by("negative?", args, Number::is_negative)
}

fn builtin_odd_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("odd?", args, 1)?;
    Ok(Value::Boolean(
        expect_exact_integer(&args[0], "odd?")? % 2 != 0,
    ))
}

fn builtin_even_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("even?", args, 1)?;
    Ok(Value::Boolean(
        expect_exact_integer(&args[0], "even?")? % 2 == 0,
    ))
}

fn builtin_list_ref(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("list-ref", args, 2)?;
    let index = expect_index(&args[1], "list-ref")?;

    match list_tail_at(&args[0], index, "list-ref")? {
        Value::Pair(car, _) => Ok(*car),
        Value::EmptyList => Err(EvalError::IndexOutOfBounds {
            kind: "list-ref",
            index,
            length: index,
        }),
        other => Err(EvalError::TypeMismatch {
            expected: "pair",
            actual: other.type_name(),
        }),
    }
}

fn builtin_list_tail(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("list-tail", args, 2)?;
    let index = expect_index(&args[1], "list-tail")?;
    list_tail_at(&args[0], index, "list-tail")
}

fn builtin_list_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("list?", args, 1)?;
    Ok(Value::Boolean(is_proper_list(&args[0])))
}

fn builtin_assoc(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("assoc", args, 2)?;
    let key = &args[0];
    let mut cursor = &args[1];

    loop {
        match cursor {
            Value::EmptyList => return Ok(Value::Boolean(false)),
            Value::Pair(entry, rest) => {
                let Value::Pair(found_key, _) = entry.as_ref() else {
                    return Err(EvalError::TypeMismatch {
                        expected: "pair",
                        actual: entry.as_ref().type_name(),
                    });
                };

                if equal_values(key, found_key.as_ref()) {
                    return Ok((**entry).clone());
                }

                cursor = rest.as_ref();
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "pair",
                    actual: other.type_name(),
                });
            }
        }
    }
}

fn builtin_map(args: &[Value], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: "map".to_string(),
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }

    let procedure = expect_procedure(&args[0])?;
    let list_values = args[1..]
        .iter()
        .map(expect_list_values)
        .collect::<Result<Vec<_>, _>>()?;

    let expected_len = list_values.first().map_or(0, Vec::len);
    for values in &list_values[1..] {
        if values.len() != expected_len {
            return Err(EvalError::MismatchedListLengths {
                name: "map".to_string(),
                expected: expected_len,
                got: values.len(),
            });
        }
    }

    let mut results = Vec::with_capacity(expected_len);
    for index in 0..expected_len {
        let call_args = list_values
            .iter()
            .map(|values| values[index].clone())
            .collect();
        results.push(apply_procedure(procedure.clone(), call_args, ctx)?);
    }

    Ok(list_from_values(results))
}

fn builtin_char_alphabetic_predicate(
    args: &[Value],
    _ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    expect_value_arity("char-alphabetic?", args, 1)?;
    Ok(Value::Boolean(expect_char(&args[0])?.is_alphabetic()))
}

fn builtin_char_numeric_predicate(
    args: &[Value],
    _ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    expect_value_arity("char-numeric?", args, 1)?;
    Ok(Value::Boolean(expect_char(&args[0])?.is_numeric()))
}

fn builtin_char_upcase(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("char-upcase", args, 1)?;
    let ch = expect_char(&args[0])?;
    Ok(Value::Char(ch.to_uppercase().next().unwrap_or(ch)))
}

fn builtin_char_downcase(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("char-downcase", args, 1)?;
    let ch = expect_char(&args[0])?;
    Ok(Value::Char(ch.to_lowercase().next().unwrap_or(ch)))
}

fn builtin_char_equal(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_char_compare("char=?", args, |left, right| left == right)
}

fn builtin_char_less_than(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_char_compare("char<?", args, |left, right| left < right)
}

fn builtin_string_equal(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_string_compare(
        "string=?",
        args,
        |value| value.to_string(),
        |left, right| left == right,
    )
}

fn builtin_string_less_than(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_string_compare(
        "string<?",
        args,
        |value| value.to_string(),
        |left, right| left < right,
    )
}

fn builtin_string_ci_equal(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_string_compare(
        "string-ci=?",
        args,
        |value| value.chars().flat_map(char::to_lowercase).collect(),
        |left, right| left == right,
    )
}

fn builtin_string_upcase(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string-upcase", args, 1)?;
    let string = expect_string(&args[0])?.to_plain_string();
    Ok(Value::String(SchemeString::new(
        string
            .chars()
            .flat_map(char::to_uppercase)
            .collect::<String>(),
    )))
}

fn builtin_string_downcase(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string-downcase", args, 1)?;
    let string = expect_string(&args[0])?.to_plain_string();
    Ok(Value::String(SchemeString::new(
        string
            .chars()
            .flat_map(char::to_lowercase)
            .collect::<String>(),
    )))
}

fn builtin_add(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;
    let mut result = Number::integer(0);
    for number in numbers {
        result = result.add(number)?;
    }
    Ok(Value::Number(result))
}

fn builtin_mul(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;
    let mut result = Number::integer(1);
    for number in numbers {
        result = result.mul(number)?;
    }
    Ok(Value::Number(result))
}

fn builtin_sub(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;

    match numbers.as_slice() {
        [] => Err(EvalError::WrongArgumentCount {
            name: "-".to_string(),
            expected: "at least 1".to_string(),
            got: 0,
        }),
        [value] => Ok(Value::Number(value.negate()?)),
        [first, rest @ ..] => {
            let mut result = *first;
            for value in rest {
                result = result.sub(*value)?;
            }
            Ok(Value::Number(result))
        }
    }
}

fn builtin_div(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;

    match numbers.as_slice() {
        [] | [_] => Err(EvalError::WrongArgumentCount {
            name: "/".to_string(),
            expected: "at least 2".to_string(),
            got: numbers.len(),
        }),
        [first, rest @ ..] => {
            let mut result = *first;
            for divisor in rest {
                result = result.div(*divisor)?;
            }
            Ok(Value::Number(result))
        }
    }
}

fn builtin_less_than(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_compare("<", args, |left, right| left.compare(right).is_lt())
}

fn builtin_greater_than(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_compare(">", args, |left, right| left.compare(right).is_gt())
}

fn builtin_equal_numbers(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_compare("=", args, |left, right| left.numeric_eq(right))
}

fn builtin_less_equal(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_compare("<=", args, |left, right| !left.compare(right).is_gt())
}

fn builtin_compare(
    name: &str,
    args: &[Value],
    predicate: impl Fn(Number, Number) -> bool,
) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;

    if numbers.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: "at least 2".to_string(),
            got: numbers.len(),
        });
    }

    let is_match = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Boolean(is_match))
}

fn builtin_type_predicate(
    name: &str,
    args: &[Value],
    predicate: impl Fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    expect_value_arity(name, args, 1)?;
    Ok(Value::Boolean(predicate(&args[0])))
}

fn builtin_min_max(
    name: &str,
    args: &[Value],
    pick: impl Fn(Number, Number) -> Number,
) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;

    let Some(first) = numbers.first().copied() else {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: "at least 1".to_string(),
            got: 0,
        });
    };

    let value = numbers
        .into_iter()
        .skip(1)
        .fold(first, |current, next| pick(current, next));
    Ok(Value::Number(value))
}

fn builtin_number_predicate_by(
    name: &str,
    args: &[Value],
    predicate: impl Fn(Number) -> bool,
) -> Result<Value, EvalError> {
    expect_value_arity(name, args, 1)?;
    Ok(Value::Boolean(predicate(expect_number(&args[0])?)))
}

fn builtin_char_compare(
    name: &str,
    args: &[Value],
    predicate: impl Fn(char, char) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }

    let mut chars = Vec::with_capacity(args.len());
    for value in args {
        chars.push(expect_char(value)?);
    }

    Ok(Value::Boolean(
        chars.windows(2).all(|pair| predicate(pair[0], pair[1])),
    ))
}

fn builtin_string_compare(
    name: &str,
    args: &[Value],
    normalize: impl Fn(&str) -> String,
    predicate: impl Fn(&str, &str) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }

    let mut strings = Vec::with_capacity(args.len());
    for value in args {
        strings.push(normalize(&expect_string(value)?.to_plain_string()));
    }

    Ok(Value::Boolean(
        strings
            .windows(2)
            .all(|pair| predicate(pair[0].as_str(), pair[1].as_str())),
    ))
}

fn is_proper_list(value: &Value) -> bool {
    let mut cursor = value;

    loop {
        match cursor {
            Value::EmptyList => return true,
            Value::Pair(_, cdr) => cursor = cdr.as_ref(),
            _ => return false,
        }
    }
}

fn list_tail_at(value: &Value, index: usize, kind: &'static str) -> Result<Value, EvalError> {
    let mut cursor = value;

    for depth in 0..index {
        cursor = match cursor {
            Value::Pair(_, cdr) => cdr.as_ref(),
            Value::EmptyList => {
                return Err(EvalError::IndexOutOfBounds {
                    kind,
                    index,
                    length: depth,
                });
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "pair",
                    actual: other.type_name(),
                });
            }
        };
    }

    Ok(cursor.clone())
}

fn equal_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left.numeric_eq(*right),
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => {
            left.to_plain_string() == right.to_plain_string()
        }
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left_car, left_cdr), Value::Pair(right_car, right_cdr)) => {
            equal_values(left_car.as_ref(), right_car.as_ref())
                && equal_values(left_cdr.as_ref(), right_cdr.as_ref())
        }
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn list_length(value: &Value) -> Result<usize, EvalError> {
    let mut count = 0;
    let mut cursor = value;

    loop {
        match cursor {
            Value::EmptyList => return Ok(count),
            Value::Pair(_, cdr) => {
                count += 1;
                cursor = cdr.as_ref();
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "pair",
                    actual: other.type_name(),
                });
            }
        }
    }
}

fn copy_list_with_tail(list: &Value, tail: Value) -> Result<Value, EvalError> {
    match list {
        Value::EmptyList => Ok(tail),
        Value::Pair(car, cdr) => Ok(Value::Pair(
            Box::new((**car).clone()),
            Box::new(copy_list_with_tail(cdr.as_ref(), tail)?),
        )),
        other => Err(EvalError::TypeMismatch {
            expected: "pair",
            actual: other.type_name(),
        }),
    }
}

fn list_from_values(values: Vec<Value>) -> Value {
    values
        .into_iter()
        .rev()
        .fold(Value::EmptyList, |tail, value| {
            Value::Pair(Box::new(value), Box::new(tail))
        })
}

fn expect_list_values(value: &Value) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::new();
    let mut cursor = value;

    loop {
        match cursor {
            Value::EmptyList => return Ok(values),
            Value::Pair(car, cdr) => {
                values.push((**car).clone());
                cursor = cdr.as_ref();
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "pair",
                    actual: other.type_name(),
                });
            }
        }
    }
}

fn expect_numbers(args: &[Value]) -> Result<Vec<Number>, EvalError> {
    let mut values = Vec::with_capacity(args.len());

    for value in args {
        values.push(expect_number(value)?);
    }

    Ok(values)
}

fn expect_number(value: &Value) -> Result<Number, EvalError> {
    match value {
        Value::Number(number) => Ok(*number),
        other => Err(EvalError::TypeMismatch {
            expected: "number",
            actual: other.type_name(),
        }),
    }
}

fn expect_two_exact_integers(name: &str, args: &[Value]) -> Result<(i64, i64), EvalError> {
    expect_value_arity(name, args, 2)?;
    let left = expect_exact_integer(&args[0], name)?;
    let right = expect_exact_integer(&args[1], name)?;

    if right == 0 {
        return Err(EvalError::DivisionByZero);
    }

    Ok((left, right))
}

fn expect_exact_integer(value: &Value, _kind: &str) -> Result<i64, EvalError> {
    expect_number(value)?.expect_exact_integer()
}

fn expect_string(value: &Value) -> Result<&SchemeString, EvalError> {
    match value {
        Value::String(text) => Ok(text),
        other => Err(EvalError::TypeMismatch {
            expected: "string",
            actual: other.type_name(),
        }),
    }
}

fn expect_char(value: &Value) -> Result<char, EvalError> {
    match value {
        Value::Char(ch) => Ok(*ch),
        other => Err(EvalError::TypeMismatch {
            expected: "char",
            actual: other.type_name(),
        }),
    }
}

fn expect_procedure(value: &Value) -> Result<Rc<Procedure>, EvalError> {
    match value {
        Value::Procedure(procedure) => Ok(procedure.clone()),
        _ => Err(EvalError::NotAProcedure),
    }
}

fn expect_record_of_type<'a>(
    value: &'a Value,
    record_type: &RecordTypeRef,
    name: &str,
) -> Result<&'a RecordInstance, EvalError> {
    let Value::Record(record) = value else {
        return Err(EvalError::TypeMismatch {
            expected: "record",
            actual: value.type_name(),
        });
    };

    if Rc::ptr_eq(&record.record_type, record_type) {
        Ok(record.as_ref())
    } else {
        Err(EvalError::RecordTypeMismatch {
            name: name.to_string(),
            expected: record_type.name.clone(),
            actual: record.record_type.name.clone(),
        })
    }
}

fn expect_index(value: &Value, kind: &'static str) -> Result<usize, EvalError> {
    let index = expect_exact_integer(value, kind)?;

    if index < 0 {
        Err(EvalError::NegativeIndex { kind, value: index })
    } else {
        Ok(index as usize)
    }
}

fn expect_expr_arity(name: &str, args: &[Expr], expected: usize) -> Result<(), EvalError> {
    if args.len() != expected {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: expected.to_string(),
            got: args.len(),
        });
    }

    Ok(())
}

fn expect_value_arity(name: &str, args: &[Value], expected: usize) -> Result<(), EvalError> {
    if args.len() != expected {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: expected.to_string(),
            got: args.len(),
        });
    }

    Ok(())
}

fn is_special_form_name(name: &str) -> bool {
    matches!(
        name,
        "and"
            | "or"
            | "if"
            | "begin"
            | "cond"
            | "quote"
            | "define"
            | "define-record-type"
            | "define-syntax"
            | "set!"
            | "lambda"
            | "let"
    )
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
    let (value, _) = eval_input(input, false)?;
    Ok(value.render())
}

fn eval_input(input: &str, capture_output: bool) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_program().map_err(|error| {
        let (line, column) = line_col_at(input, parser.index);
        error.with_position(line, column)
    })?;
    let mut ctx = EvalContext::new(capture_output);
    let value = eval_program(&exprs, &mut ctx).map_err(|error| error.with_position(1, 1))?;
    Ok((value, ctx.finish()))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_input(input, true)?;
    Ok((value.render(), output))
}

#[cfg(test)]
mod tests;
