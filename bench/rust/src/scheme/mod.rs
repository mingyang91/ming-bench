pub mod error;
mod macros;
mod number;

pub use error::EvalError;

use macros::{expand_macro_call, parse_macro_transformer, MacroTransformer};
use number::Number;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
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
type PairRef = Rc<RefCell<PairCell>>;

#[derive(Clone)]
struct RecordType {
    name: String,
}

struct PairCell {
    car: Value,
    cdr: Value,
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
    Pair(PairRef),
    Vector(SchemeVector),
    Record(Rc<RecordInstance>),
    Procedure(Rc<Procedure>),
    Values(Vec<Value>),
    Uninitialized,
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
    mutable: bool,
}

impl SchemeString {
    fn new(value: impl AsRef<str>) -> Self {
        Self::from_chars(value.as_ref().chars().collect())
    }

    fn from_chars(chars: Vec<char>) -> Self {
        Self::from_chars_with_mutability(chars, false)
    }

    fn from_mutable_chars(chars: Vec<char>) -> Self {
        Self::from_chars_with_mutability(chars, true)
    }

    fn from_chars_with_mutability(chars: Vec<char>, mutable: bool) -> Self {
        Self {
            chars: Rc::new(RefCell::new(chars)),
            mutable,
        }
    }

    fn to_plain_string(&self) -> String {
        self.chars.borrow().iter().collect()
    }

    fn to_chars(&self) -> Vec<char> {
        self.chars.borrow().clone()
    }

    fn len(&self) -> usize {
        self.chars.borrow().len()
    }

    fn deep_copy(&self) -> Self {
        Self::from_mutable_chars(self.to_chars())
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
        if !self.mutable {
            return Err(EvalError::ImmutableString {
                name: kind.to_string(),
            });
        }

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

#[derive(Clone)]
struct SchemeVector {
    items: Rc<RefCell<Vec<Value>>>,
}

impl SchemeVector {
    fn new(items: Vec<Value>) -> Self {
        Self {
            items: Rc::new(RefCell::new(items)),
        }
    }

    fn len(&self) -> usize {
        self.items.borrow().len()
    }

    fn get(&self, index: usize, kind: &'static str) -> Result<Value, EvalError> {
        let items = self.items.borrow();

        items
            .get(index)
            .cloned()
            .ok_or(EvalError::IndexOutOfBounds {
                kind,
                index,
                length: items.len(),
            })
    }

    fn set(&self, index: usize, value: Value, kind: &'static str) -> Result<(), EvalError> {
        let mut items = self.items.borrow_mut();
        let length = items.len();
        let slot = items.get_mut(index).ok_or(EvalError::IndexOutOfBounds {
            kind,
            index,
            length,
        })?;
        *slot = value;
        Ok(())
    }

    fn to_values(&self) -> Vec<Value> {
        self.items.borrow().clone()
    }
}

fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new(PairCell { car, cdr })))
}

fn pair_id(pair: &PairRef) -> usize {
    Rc::as_ptr(pair) as usize
}

fn pair_parts(pair: &PairRef) -> (Value, Value) {
    let pair = pair.borrow();
    (pair.car.clone(), pair.cdr.clone())
}

fn pair_car(pair: &PairRef) -> Value {
    pair.borrow().car.clone()
}

fn pair_cdr(pair: &PairRef) -> Value {
    pair.borrow().cdr.clone()
}

fn set_pair_car(pair: &PairRef, value: Value) {
    pair.borrow_mut().car = value;
}

fn set_pair_cdr(pair: &PairRef, value: Value) {
    pair.borrow_mut().cdr = value;
}

fn vector_id(vector: &SchemeVector) -> usize {
    Rc::as_ptr(&vector.items) as usize
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
            Self::EmptyList | Self::Pair(_) => "pair",
            Self::Vector(_) => "vector",
            Self::Record(_) => "record",
            Self::Procedure(_) => "procedure",
            Self::Values(_) => "values",
            Self::Uninitialized => "uninitialized",
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
        let mut state = RenderState::default();
        render_value(self, mode, &mut state)
    }
}

#[derive(Default)]
struct RenderState {
    pairs: HashSet<usize>,
    vectors: HashSet<usize>,
}

fn render_value(value: &Value, mode: RenderMode, state: &mut RenderState) -> String {
    match value {
        Value::Number(number) => number.render(),
        Value::Boolean(true) => "#t".to_string(),
        Value::Boolean(false) => "#f".to_string(),
        Value::String(text) => {
            let plain = text.to_plain_string();
            match mode {
                RenderMode::Write => render_string(&plain),
                RenderMode::Display => plain,
            }
        }
        Value::Char(ch) => match mode {
            RenderMode::Write => render_char(*ch),
            RenderMode::Display => ch.to_string(),
        },
        Value::Symbol(symbol) => symbol.clone(),
        Value::EmptyList => "()".to_string(),
        Value::Pair(pair) => render_pair(pair, mode, state),
        Value::Vector(vector) => render_vector(vector, mode, state),
        Value::Record(record) => format!("#<record {}>", record.record_type.name),
        Value::Procedure(_) => "#<procedure>".to_string(),
        Value::Values(values) => render_values(values, mode, state),
        Value::Uninitialized => "#<uninitialized>".to_string(),
        Value::Void => "#<void>".to_string(),
    }
}

fn render_values(values: &[Value], mode: RenderMode, state: &mut RenderState) -> String {
    let mut out = String::from("#<values");

    for value in values {
        out.push(' ');
        out.push_str(&render_value(value, mode, state));
    }

    out.push('>');
    out
}

fn render_pair(pair: &PairRef, mode: RenderMode, state: &mut RenderState) -> String {
    let id = pair_id(pair);
    if !state.pairs.insert(id) {
        return "#<cycle>".to_string();
    }

    let body = render_pair_body(pair, mode, state);
    state.pairs.remove(&id);
    format!("({body})")
}

fn render_pair_body(pair: &PairRef, mode: RenderMode, state: &mut RenderState) -> String {
    let (car, cdr) = pair_parts(pair);
    let mut out = render_value(&car, mode, state);

    match cdr {
        Value::EmptyList => {}
        Value::Pair(next) => {
            let next_id = pair_id(&next);
            if state.pairs.contains(&next_id) {
                out.push_str(" . #<cycle>");
            } else {
                state.pairs.insert(next_id);
                out.push(' ');
                out.push_str(&render_pair_body(&next, mode, state));
                state.pairs.remove(&next_id);
            }
        }
        other => {
            out.push_str(" . ");
            out.push_str(&render_value(&other, mode, state));
        }
    }

    out
}

fn render_vector(vector: &SchemeVector, mode: RenderMode, state: &mut RenderState) -> String {
    let id = vector_id(vector);
    if !state.vectors.insert(id) {
        return "#<cycle>".to_string();
    }

    let items = vector.to_values();
    let mut out = String::from("#(");

    for (index, item) in items.into_iter().enumerate() {
        if index > 0 {
            out.push(' ');
        }
        out.push_str(&render_value(&item, mode, state));
    }

    out.push(')');
    state.vectors.remove(&id);
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
    dynamic_winds: Vec<DynamicWindExtent>,
    dynamic_wind_counter: u64,
    pending_exception: Option<Value>,
}

impl EvalContext {
    fn new(capture_output: bool) -> Self {
        Self {
            output: String::new(),
            capture_output,
            gensym_counter: 0,
            dynamic_winds: Vec::new(),
            dynamic_wind_counter: 0,
            pending_exception: None,
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

    fn fresh_dynamic_wind(&mut self, before: Value, after: Value) -> DynamicWindExtent {
        self.dynamic_wind_counter += 1;
        DynamicWindExtent {
            id: self.dynamic_wind_counter,
            before,
            after,
        }
    }
}

type EnvRef = Rc<RefCell<Env>>;
type BindingCell = Rc<RefCell<Value>>;
type BuiltinFn = fn(&[Value], &mut EvalContext) -> Result<Value, EvalError>;

struct Env {
    parent: Option<EnvRef>,
    bindings: HashMap<String, BindingCell>,
    macros: HashMap<String, Rc<MacroTransformer>>,
    define_in_parent: bool,
}

impl Env {
    fn new() -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: None,
            bindings: HashMap::new(),
            macros: HashMap::new(),
            define_in_parent: false,
        }))
    }

    fn child(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
            macros: HashMap::new(),
            define_in_parent: false,
        }))
    }

    fn macro_expansion(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
            macros: HashMap::new(),
            define_in_parent: true,
        }))
    }
}

#[derive(Clone)]
enum Procedure {
    Builtin(BuiltinProcedure),
    Lambda(LambdaProcedure),
    CaseLambda(CaseLambdaProcedure),
    RecordConstructor(RecordConstructorProcedure),
    RecordPredicate(RecordPredicateProcedure),
    RecordAccessor(RecordAccessorProcedure),
    DynamicWind(DynamicWindProcedure),
    WithExceptionHandler(WithExceptionHandlerProcedure),
    CallWithValues(CallWithValuesProcedure),
    CallCc(CallCcProcedure),
    Continuation(ContinuationProcedure),
}

#[derive(Clone)]
struct BuiltinProcedure {
    implementation: BuiltinFn,
}

#[derive(Clone)]
struct DynamicWindProcedure {
    name: String,
}

#[derive(Clone)]
struct WithExceptionHandlerProcedure {
    name: String,
}

#[derive(Clone)]
struct CallWithValuesProcedure {
    name: String,
}

#[derive(Clone)]
struct CallCcProcedure {
    name: String,
}

#[derive(Clone)]
struct LambdaProcedure {
    name: String,
    params: ParameterList,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone)]
struct CaseLambdaProcedure {
    name: String,
    clauses: Vec<LambdaProcedure>,
}

impl CaseLambdaProcedure {
    fn expected_arity(&self) -> String {
        let mut arities = Vec::with_capacity(self.clauses.len());

        for clause in &self.clauses {
            let arity = clause.params.expected_arity();
            if !arities.iter().any(|existing| existing == &arity) {
                arities.push(arity);
            }
        }

        match arities.as_slice() {
            [] => "no clauses".to_string(),
            [arity] => arity.clone(),
            _ => arities.join(" or "),
        }
    }
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
struct ContinuationProcedure {
    frames: Vec<MachineFrame>,
    wind_stack: Vec<DynamicWindExtent>,
}

#[derive(Clone)]
struct DynamicWindExtent {
    id: u64,
    before: Value,
    after: Value,
}

#[derive(Clone)]
struct ParameterList {
    required: Vec<String>,
    rest: Option<String>,
}

struct DoBinding {
    name: String,
    init: Expr,
    step: Option<Expr>,
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
            Some('#') if self.input[self.index..].starts_with("#'") => {
                self.parse_syntax_quote_shorthand()
            }
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

    fn parse_syntax_quote_shorthand(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        self.bump_char();
        let quoted = self.parse_expr()?;
        Ok(Expr::List(vec![Expr::Symbol("syntax".to_string()), quoted]))
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
    define_builtin(&env, ">=", builtin_greater_equal);
    define_builtin(&env, "=", builtin_equal_numbers);
    define_builtin(&env, "<=", builtin_less_equal);
    define_builtin(&env, "eq?", builtin_eq);
    define_builtin(&env, "eqv?", builtin_eqv);
    define_builtin(&env, "equal?", builtin_equal);
    define_builtin(&env, "not", builtin_not);
    define_builtin(&env, "cons", builtin_cons);
    define_builtin(&env, "car", builtin_car);
    define_builtin(&env, "cdr", builtin_cdr);
    define_builtin(&env, "caar", builtin_caar);
    define_builtin(&env, "cadr", builtin_cadr);
    define_builtin(&env, "cdar", builtin_cdar);
    define_builtin(&env, "cddr", builtin_cddr);
    define_builtin(&env, "caaar", builtin_caaar);
    define_builtin(&env, "caadr", builtin_caadr);
    define_builtin(&env, "cadar", builtin_cadar);
    define_builtin(&env, "caddr", builtin_caddr);
    define_builtin(&env, "cdaar", builtin_cdaar);
    define_builtin(&env, "cdadr", builtin_cdadr);
    define_builtin(&env, "cddar", builtin_cddar);
    define_builtin(&env, "cdddr", builtin_cdddr);
    define_builtin(&env, "caaaar", builtin_caaaar);
    define_builtin(&env, "caaadr", builtin_caaadr);
    define_builtin(&env, "caadar", builtin_caadar);
    define_builtin(&env, "caaddr", builtin_caaddr);
    define_builtin(&env, "cadaar", builtin_cadaar);
    define_builtin(&env, "cadadr", builtin_cadadr);
    define_builtin(&env, "caddar", builtin_caddar);
    define_builtin(&env, "cadddr", builtin_cadddr);
    define_builtin(&env, "cdaaar", builtin_cdaaar);
    define_builtin(&env, "cdaadr", builtin_cdaadr);
    define_builtin(&env, "cdadar", builtin_cdadar);
    define_builtin(&env, "cdaddr", builtin_cdaddr);
    define_builtin(&env, "cddaar", builtin_cddaar);
    define_builtin(&env, "cddadr", builtin_cddadr);
    define_builtin(&env, "cdddar", builtin_cdddar);
    define_builtin(&env, "cddddr", builtin_cddddr);
    define_builtin(&env, "set-car!", builtin_set_car);
    define_builtin(&env, "set-cdr!", builtin_set_cdr);
    define_builtin(&env, "null?", builtin_null_predicate);
    define_builtin(&env, "list", builtin_list);
    define_builtin(&env, "length", builtin_length);
    define_builtin(&env, "append", builtin_append);
    define_builtin(&env, "reverse", builtin_reverse);
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
    define_builtin(&env, "procedure?", builtin_procedure_predicate);
    define_builtin(&env, "display", builtin_display);
    define_builtin(&env, "write", builtin_write);
    define_builtin(&env, "newline", builtin_newline);
    define_builtin(&env, "apply", builtin_apply);
    define_builtin(&env, "string-append", builtin_string_append);
    define_builtin(&env, "make-string", builtin_make_string);
    define_builtin(&env, "string", builtin_string);
    define_builtin(&env, "string-length", builtin_string_length);
    define_builtin(&env, "substring", builtin_substring);
    define_builtin(&env, "string->number", builtin_string_to_number);
    define_builtin(&env, "number->string", builtin_number_to_string);
    define_builtin(&env, "symbol->string", builtin_symbol_to_string);
    define_builtin(&env, "string->symbol", builtin_string_to_symbol);
    define_builtin(&env, "string-ref", builtin_string_ref);
    define_builtin(&env, "string-copy", builtin_string_copy);
    define_builtin(&env, "string->list", builtin_string_to_list);
    define_builtin(&env, "list->string", builtin_list_to_string);
    define_builtin(&env, "string-set!", builtin_string_set);
    define_builtin(&env, "char?", builtin_char_predicate);
    define_builtin(&env, "char->integer", builtin_char_to_integer);
    define_builtin(&env, "integer->char", builtin_integer_to_char);
    define_builtin(&env, "abs", builtin_abs);
    define_builtin(&env, "gcd", builtin_gcd);
    define_builtin(&env, "lcm", builtin_lcm);
    define_builtin(&env, "modulo", builtin_modulo);
    define_builtin(&env, "remainder", builtin_remainder);
    define_builtin(&env, "quotient", builtin_quotient);
    define_builtin(&env, "min", builtin_min);
    define_builtin(&env, "max", builtin_max);
    define_builtin(&env, "truncate", builtin_truncate);
    define_builtin(&env, "round", builtin_round);
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
    define_builtin(&env, "assv", builtin_assv);
    define_builtin(&env, "member", builtin_member);
    define_builtin(&env, "map", builtin_map);
    define_builtin(&env, "for-each", builtin_for_each);
    define_builtin(&env, "char-alphabetic?", builtin_char_alphabetic_predicate);
    define_builtin(&env, "char-numeric?", builtin_char_numeric_predicate);
    define_builtin(&env, "char-upcase", builtin_char_upcase);
    define_builtin(&env, "char-downcase", builtin_char_downcase);
    define_builtin(&env, "char=?", builtin_char_equal);
    define_builtin(&env, "char<?", builtin_char_less_than);
    define_builtin(&env, "string=?", builtin_string_equal);
    define_builtin(&env, "string<?", builtin_string_less_than);
    define_builtin(&env, "string>?", builtin_string_greater_than);
    define_builtin(&env, "string<=?", builtin_string_less_equal);
    define_builtin(&env, "string>=?", builtin_string_greater_equal);
    define_builtin(&env, "string-ci=?", builtin_string_ci_equal);
    define_builtin(&env, "string-upcase", builtin_string_upcase);
    define_builtin(&env, "string-downcase", builtin_string_downcase);
    define_builtin(&env, "vector", builtin_vector);
    define_builtin(&env, "make-vector", builtin_make_vector);
    define_builtin(&env, "vector-ref", builtin_vector_ref);
    define_builtin(&env, "vector-set!", builtin_vector_set);
    define_builtin(&env, "vector-length", builtin_vector_length);
    define_builtin(&env, "vector?", builtin_vector_predicate);
    define_builtin(&env, "vector->list", builtin_vector_to_list);
    define_builtin(&env, "list->vector", builtin_list_to_vector);
    define_builtin(&env, "raise", builtin_raise);
    define_builtin(&env, "values", builtin_values);
    define_dynamic_wind_builtin(&env, "dynamic-wind");
    define_with_exception_handler_builtin(&env, "with-exception-handler");
    define_call_with_values_builtin(&env, "call-with-values");
    define_callcc_builtin(&env, "call/cc");
    define_callcc_builtin(&env, "call-with-current-continuation");

    env
}

fn define_builtin(env: &EnvRef, name: &'static str, implementation: BuiltinFn) {
    let value = Value::Procedure(Rc::new(Procedure::Builtin(BuiltinProcedure {
        implementation,
    })));
    bind_value(env, name.to_string(), value);
}

fn define_dynamic_wind_builtin(env: &EnvRef, name: &'static str) {
    let value = Value::Procedure(Rc::new(Procedure::DynamicWind(DynamicWindProcedure {
        name: name.to_string(),
    })));
    bind_value(env, name.to_string(), value);
}

fn define_with_exception_handler_builtin(env: &EnvRef, name: &'static str) {
    let value = Value::Procedure(Rc::new(Procedure::WithExceptionHandler(
        WithExceptionHandlerProcedure {
            name: name.to_string(),
        },
    )));
    bind_value(env, name.to_string(), value);
}

fn define_call_with_values_builtin(env: &EnvRef, name: &'static str) {
    let value = Value::Procedure(Rc::new(Procedure::CallWithValues(
        CallWithValuesProcedure {
            name: name.to_string(),
        },
    )));
    bind_value(env, name.to_string(), value);
}

fn define_callcc_builtin(env: &EnvRef, name: &'static str) {
    let value = Value::Procedure(Rc::new(Procedure::CallCc(CallCcProcedure {
        name: name.to_string(),
    })));
    bind_value(env, name.to_string(), value);
}

fn lookup(env: &EnvRef, name: &str) -> Result<Value, EvalError> {
    let value = lookup_binding_cell(env, name)?.borrow().clone();

    match value {
        Value::Uninitialized => Err(EvalError::UninitializedBinding {
            name: name.to_string(),
        }),
        other => Ok(other),
    }
}

fn bind_value(env: &EnvRef, name: String, value: Value) {
    env.borrow_mut()
        .bindings
        .insert(name, Rc::new(RefCell::new(value)));
}

fn bind_macro(env: &EnvRef, name: String, transformer: Rc<MacroTransformer>) {
    env.borrow_mut().macros.insert(name, transformer);
}

fn definition_target_env(env: &EnvRef) -> EnvRef {
    let mut current = env.clone();

    loop {
        let parent = {
            let scope = current.borrow();
            if scope.define_in_parent {
                scope.parent.clone()
            } else {
                None
            }
        };

        match parent {
            Some(parent) => current = parent,
            None => return current,
        }
    }
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

enum TailOutcome {
    Value(Value),
    Next { expr: Expr, env: EnvRef },
}

#[derive(Clone)]
enum MachineState {
    Eval(Expr, EnvRef),
    Value(Value),
}

#[derive(Clone)]
enum CondMatchAction {
    ReturnTestValue,
    EvaluateBody(Vec<Expr>),
}

#[derive(Clone)]
enum ExceptionHandlerKind {
    Procedure(Value),
    Guard {
        variable: String,
        clauses: Vec<Expr>,
        env: EnvRef,
    },
}

#[derive(Clone)]
enum WindTransitionStep {
    Exit(DynamicWindExtent),
    Enter(DynamicWindExtent),
}

#[derive(Clone)]
enum MachineFrame {
    Sequence {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    DefineVar {
        name: String,
        env: EnvRef,
    },
    SetVar {
        name: String,
        env: EnvRef,
    },
    If {
        consequent: Expr,
        alternate: Option<Expr>,
        env: EnvRef,
    },
    And {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    Or {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    Cond {
        remaining_clauses: Vec<Expr>,
        match_action: CondMatchAction,
        env: EnvRef,
    },
    Let {
        names: Vec<String>,
        remaining_inits: Vec<Expr>,
        values: Vec<Value>,
        body: Vec<Expr>,
        env: EnvRef,
    },
    CallOperator {
        arg_exprs: Vec<Expr>,
        env: EnvRef,
    },
    CallArg {
        operator: Value,
        evaluated: Vec<Value>,
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    CallWithValuesConsumer {
        consumer: Value,
    },
    GuardHandler {
        variable: String,
        clauses: Vec<Expr>,
        env: EnvRef,
        target_winds: Vec<DynamicWindExtent>,
    },
    GuardClause {
        remaining_clauses: Vec<Expr>,
        match_action: CondMatchAction,
        env: EnvRef,
        exception: Value,
    },
    WithExceptionHandler {
        handler: Value,
        target_winds: Vec<DynamicWindExtent>,
    },
    RunExceptionHandler {
        handler: ExceptionHandlerKind,
        exception: Value,
    },
    DynamicWindEnter {
        wind: DynamicWindExtent,
        thunk: Value,
    },
    DynamicWindBody {
        wind: DynamicWindExtent,
    },
    DynamicWindComplete {
        result: Value,
    },
    WindTransition {
        pending_install: Option<DynamicWindExtent>,
        remaining_steps: Vec<WindTransitionStep>,
        target_frames: Vec<MachineFrame>,
        target_winds: Vec<DynamicWindExtent>,
        result: Value,
    },
}

fn eval_program(exprs: &[Expr], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let env = default_env();
    eval_program_machine(exprs, env, ctx)
}

fn eval_program_machine(
    exprs: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    let mut frames = Vec::new();
    let mut state = schedule_machine_sequence(exprs, env, &mut frames);

    loop {
        let next = match state {
            MachineState::Eval(expr, env) => eval_machine_expr(expr, env, ctx, &mut frames),
            MachineState::Value(value) => match frames.pop() {
                Some(frame) => resume_machine_frame(frame, value, ctx, &mut frames),
                None => return Ok(value),
            },
        };

        state = match next {
            Ok(state) => state,
            Err(EvalError::ExceptionRaisedSignal) => {
                let exception = take_pending_exception(ctx)?;
                handle_machine_exception(exception, ctx, &mut frames)?
            }
            Err(error) => return Err(error),
        };
    }
}

fn schedule_machine_sequence(
    exprs: &[Expr],
    env: EnvRef,
    frames: &mut Vec<MachineFrame>,
) -> MachineState {
    let Some((first, rest)) = exprs.split_first() else {
        return MachineState::Value(Value::Void);
    };

    if !rest.is_empty() {
        frames.push(MachineFrame::Sequence {
            remaining: rest.to_vec(),
            env: env.clone(),
        });
    }

    MachineState::Eval(first.clone(), env)
}

fn eval_machine_expr(
    expr: Expr,
    env: EnvRef,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    match expr {
        Expr::Number(value) => Ok(MachineState::Value(Value::Number(value))),
        Expr::Boolean(value) => Ok(MachineState::Value(Value::Boolean(value))),
        Expr::String(value) => Ok(MachineState::Value(Value::String(SchemeString::new(value)))),
        Expr::Char(value) => Ok(MachineState::Value(Value::Char(value))),
        Expr::Symbol(name) => Ok(MachineState::Value(lookup(&env, &name)?)),
        Expr::List(items) => eval_machine_list(items, env, ctx, frames),
    }
}

fn eval_machine_list(
    items: Vec<Expr>,
    env: EnvRef,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    if items.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate an empty list".to_string(),
        });
    }

    if let Expr::Symbol(operator) = &items[0] {
        let args = &items[1..];

        match operator.as_str() {
            "and" => return Ok(schedule_machine_and(args, env, frames)),
            "or" => return Ok(schedule_machine_or(args, env, frames)),
            "if" => return schedule_machine_if(args, env, frames),
            "begin" => return Ok(schedule_machine_sequence(args, env, frames)),
            "cond" => return schedule_machine_cond(args, env, frames),
            "guard" => return schedule_machine_guard(args, env, ctx, frames),
            "quote" => return Ok(MachineState::Value(eval_quote(args)?)),
            "define" => return schedule_machine_define(args, env, ctx, frames),
            "define-record-type" => {
                return Ok(MachineState::Value(eval_define_record_type(args, env)?));
            }
            "define-syntax" => return Ok(MachineState::Value(eval_define_syntax(args, env)?)),
            "set!" => return schedule_machine_set(args, env, frames),
            "lambda" => return Ok(MachineState::Value(eval_lambda(args, env)?)),
            "case-lambda" => return Ok(MachineState::Value(eval_case_lambda(args, env)?)),
            "let" => return schedule_machine_let(args, env, ctx, frames),
            _ => {}
        }

        if let Some(transformer) = lookup_macro(&env, operator) {
            let (expanded, expansion_env) = expand_macro_call(transformer, &items, env, ctx)?;
            return Ok(MachineState::Eval(expanded, expansion_env));
        }

        if is_special_form_name(operator) {
            return Ok(MachineState::Value(eval_list(&items, env, ctx)?));
        }
    }

    schedule_machine_application(items, env, frames)
}

fn schedule_machine_if(
    args: &[Expr],
    env: EnvRef,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    if !(2..=3).contains(&args.len()) {
        return Err(EvalError::WrongArgumentCount {
            name: "if".to_string(),
            expected: "2 or 3".to_string(),
            got: args.len(),
        });
    }

    frames.push(MachineFrame::If {
        consequent: args[1].clone(),
        alternate: args.get(2).cloned(),
        env: env.clone(),
    });
    Ok(MachineState::Eval(args[0].clone(), env))
}

fn schedule_machine_and(
    args: &[Expr],
    env: EnvRef,
    frames: &mut Vec<MachineFrame>,
) -> MachineState {
    let Some((first, rest)) = args.split_first() else {
        return MachineState::Value(Value::Boolean(true));
    };

    if !rest.is_empty() {
        frames.push(MachineFrame::And {
            remaining: rest.to_vec(),
            env: env.clone(),
        });
    }

    MachineState::Eval(first.clone(), env)
}

fn schedule_machine_or(args: &[Expr], env: EnvRef, frames: &mut Vec<MachineFrame>) -> MachineState {
    let Some((first, rest)) = args.split_first() else {
        return MachineState::Value(Value::Boolean(false));
    };

    if !rest.is_empty() {
        frames.push(MachineFrame::Or {
            remaining: rest.to_vec(),
            env: env.clone(),
        });
    }

    MachineState::Eval(first.clone(), env)
}

fn schedule_machine_cond(
    clauses: &[Expr],
    env: EnvRef,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    let Some((clause, remaining)) = clauses.split_first() else {
        return Ok(MachineState::Value(Value::Void));
    };

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
            if !remaining.is_empty() {
                return Err(EvalError::SyntaxError {
                    message: "cond else clause must be last".to_string(),
                });
            }

            if items.len() == 1 {
                return Ok(MachineState::Value(Value::Void));
            }

            return Ok(schedule_machine_sequence(&items[1..], env, frames));
        }
    }

    let match_action = if items.len() == 1 {
        CondMatchAction::ReturnTestValue
    } else {
        CondMatchAction::EvaluateBody(items[1..].to_vec())
    };

    frames.push(MachineFrame::Cond {
        remaining_clauses: remaining.to_vec(),
        match_action,
        env: env.clone(),
    });
    Ok(MachineState::Eval(items[0].clone(), env))
}

fn schedule_machine_define(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "define requires a binding target".to_string(),
        });
    }

    match &args[0] {
        Expr::Symbol(name) => {
            expect_expr_arity("define", args, 2)?;
            frames.push(MachineFrame::DefineVar {
                name: name.clone(),
                env: definition_target_env(&env),
            });
            Ok(MachineState::Eval(args[1].clone(), env))
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
            let target_env = definition_target_env(&env);
            bind_value(&target_env, name.clone(), lambda);
            let _ = ctx;
            Ok(MachineState::Value(Value::Void))
        }
        _ => Err(EvalError::SyntaxError {
            message: "define requires a symbol or function signature".to_string(),
        }),
    }
}

fn schedule_machine_guard(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::SyntaxError {
            message: "guard requires a handler clause list and a body".to_string(),
        });
    }

    let (variable, clauses) = parse_guard_spec(&args[0])?;
    frames.push(MachineFrame::GuardHandler {
        variable,
        clauses,
        env: env.clone(),
        target_winds: ctx.dynamic_winds.clone(),
    });
    Ok(schedule_machine_sequence(&args[1..], env, frames))
}

fn schedule_machine_guard_clauses(
    clauses: &[Expr],
    env: EnvRef,
    exception: Value,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    let Some((clause, remaining)) = clauses.split_first() else {
        return signal_exception(exception, ctx);
    };

    let Expr::List(items) = clause else {
        return Err(EvalError::SyntaxError {
            message: "guard clauses must be lists".to_string(),
        });
    };

    if items.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "guard clauses cannot be empty".to_string(),
        });
    }

    if let Expr::Symbol(symbol) = &items[0] {
        if symbol == "else" {
            if !remaining.is_empty() {
                return Err(EvalError::SyntaxError {
                    message: "guard else clause must be last".to_string(),
                });
            }

            if items.len() == 1 {
                return Ok(MachineState::Value(Value::Void));
            }

            return Ok(schedule_machine_sequence(&items[1..], env, frames));
        }
    }

    let match_action = if items.len() == 1 {
        CondMatchAction::ReturnTestValue
    } else {
        CondMatchAction::EvaluateBody(items[1..].to_vec())
    };

    frames.push(MachineFrame::GuardClause {
        remaining_clauses: remaining.to_vec(),
        match_action,
        env: env.clone(),
        exception,
    });
    Ok(MachineState::Eval(items[0].clone(), env))
}

fn schedule_machine_set(
    args: &[Expr],
    env: EnvRef,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    expect_expr_arity("set!", args, 2)?;

    let Expr::Symbol(name) = &args[0] else {
        return Err(EvalError::SyntaxError {
            message: "set! requires a symbol target".to_string(),
        });
    };

    frames.push(MachineFrame::SetVar {
        name: name.clone(),
        env: env.clone(),
    });
    Ok(MachineState::Eval(args[1].clone(), env))
}

fn schedule_machine_let(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires bindings".to_string(),
        });
    }

    if matches!(args[0], Expr::Symbol(_)) {
        return Ok(MachineState::Value(eval_let(args, env, ctx)?));
    }

    if args.len() < 2 {
        return Err(EvalError::SyntaxError {
            message: "let requires a body".to_string(),
        });
    }

    let bindings = parse_let_bindings(&args[0])?;
    let names = bindings
        .iter()
        .map(|(name, _)| name.clone())
        .collect::<Vec<_>>();
    let init_exprs = bindings
        .into_iter()
        .map(|(_, expr)| expr)
        .collect::<Vec<_>>();

    schedule_machine_let_bindings(names, init_exprs, args[1..].to_vec(), env, frames)
}

fn schedule_machine_let_bindings(
    names: Vec<String>,
    init_exprs: Vec<Expr>,
    body: Vec<Expr>,
    env: EnvRef,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires a body".to_string(),
        });
    }

    let Some((first_init, rest_inits)) = init_exprs.split_first() else {
        let let_env = Env::child(env);
        return Ok(schedule_machine_sequence(&body, let_env, frames));
    };

    frames.push(MachineFrame::Let {
        names,
        remaining_inits: rest_inits.to_vec(),
        values: Vec::new(),
        body,
        env: env.clone(),
    });
    Ok(MachineState::Eval(first_init.clone(), env))
}

fn schedule_machine_application(
    items: Vec<Expr>,
    env: EnvRef,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    frames.push(MachineFrame::CallOperator {
        arg_exprs: items[1..].to_vec(),
        env: env.clone(),
    });
    Ok(MachineState::Eval(items[0].clone(), env))
}

fn resume_machine_frame(
    frame: MachineFrame,
    value: Value,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    match frame {
        MachineFrame::Sequence { remaining, env } => {
            let _ = value;
            Ok(schedule_machine_sequence(&remaining, env, frames))
        }
        MachineFrame::DefineVar { name, env } => {
            bind_value(&env, name, value);
            Ok(MachineState::Value(Value::Void))
        }
        MachineFrame::SetVar { name, env } => {
            assign(&env, &name, value)?;
            Ok(MachineState::Value(Value::Void))
        }
        MachineFrame::If {
            consequent,
            alternate,
            env,
        } => {
            if value.is_truthy() {
                Ok(MachineState::Eval(consequent, env))
            } else if let Some(alternate) = alternate {
                Ok(MachineState::Eval(alternate, env))
            } else {
                Ok(MachineState::Value(Value::Void))
            }
        }
        MachineFrame::And { remaining, env } => {
            if !value.is_truthy() {
                Ok(MachineState::Value(value))
            } else {
                Ok(schedule_machine_and(&remaining, env, frames))
            }
        }
        MachineFrame::Or { remaining, env } => {
            if value.is_truthy() {
                Ok(MachineState::Value(value))
            } else {
                Ok(schedule_machine_or(&remaining, env, frames))
            }
        }
        MachineFrame::Cond {
            remaining_clauses,
            match_action,
            env,
        } => {
            if value.is_truthy() {
                match match_action {
                    CondMatchAction::ReturnTestValue => Ok(MachineState::Value(value)),
                    CondMatchAction::EvaluateBody(body) => {
                        Ok(schedule_machine_sequence(&body, env, frames))
                    }
                }
            } else {
                schedule_machine_cond(&remaining_clauses, env, frames)
            }
        }
        MachineFrame::Let {
            names,
            remaining_inits,
            mut values,
            body,
            env,
        } => {
            values.push(value);

            if let Some((next_init, rest_inits)) = remaining_inits.split_first() {
                frames.push(MachineFrame::Let {
                    names,
                    remaining_inits: rest_inits.to_vec(),
                    values,
                    body,
                    env: env.clone(),
                });
                Ok(MachineState::Eval(next_init.clone(), env))
            } else {
                let let_env = Env::child(env);
                {
                    let mut scope = let_env.borrow_mut();
                    for (name, value) in names.into_iter().zip(values) {
                        scope.bindings.insert(name, Rc::new(RefCell::new(value)));
                    }
                }
                Ok(schedule_machine_sequence(&body, let_env, frames))
            }
        }
        MachineFrame::CallOperator { arg_exprs, env } => {
            if let Some((last_arg, rest_args)) = arg_exprs.split_last() {
                frames.push(MachineFrame::CallArg {
                    operator: value,
                    evaluated: Vec::new(),
                    remaining: rest_args.to_vec(),
                    env: env.clone(),
                });
                Ok(MachineState::Eval(last_arg.clone(), env))
            } else {
                apply_machine_value(value, Vec::new(), ctx, frames)
            }
        }
        MachineFrame::CallArg {
            operator,
            mut evaluated,
            remaining,
            env,
        } => {
            evaluated.push(value);

            if let Some((next_arg, rest_args)) = remaining.split_last() {
                frames.push(MachineFrame::CallArg {
                    operator,
                    evaluated,
                    remaining: rest_args.to_vec(),
                    env: env.clone(),
                });
                Ok(MachineState::Eval(next_arg.clone(), env))
            } else {
                evaluated.reverse();
                apply_machine_value(operator, evaluated, ctx, frames)
            }
        }
        MachineFrame::CallWithValuesConsumer { consumer } => {
            apply_machine_value(consumer, unpack_values(value), ctx, frames)
        }
        MachineFrame::GuardHandler { .. } => Ok(MachineState::Value(value)),
        MachineFrame::GuardClause {
            remaining_clauses,
            match_action,
            env,
            exception,
        } => {
            if value.is_truthy() {
                match match_action {
                    CondMatchAction::ReturnTestValue => Ok(MachineState::Value(value)),
                    CondMatchAction::EvaluateBody(body) => {
                        Ok(schedule_machine_sequence(&body, env, frames))
                    }
                }
            } else {
                schedule_machine_guard_clauses(&remaining_clauses, env, exception, ctx, frames)
            }
        }
        MachineFrame::WithExceptionHandler { .. } => Ok(MachineState::Value(value)),
        MachineFrame::RunExceptionHandler { handler, exception } => {
            let _ = value;

            match handler {
                ExceptionHandlerKind::Procedure(handler) => {
                    apply_machine_value(handler, vec![exception], ctx, frames)
                }
                ExceptionHandlerKind::Guard {
                    variable,
                    clauses,
                    env,
                } => {
                    let guard_env = Env::child(env);
                    bind_value(&guard_env, variable, exception.clone());
                    schedule_machine_guard_clauses(&clauses, guard_env, exception, ctx, frames)
                }
            }
        }
        MachineFrame::DynamicWindEnter { wind, thunk } => {
            let _ = value;
            ctx.dynamic_winds.push(wind.clone());
            frames.push(MachineFrame::DynamicWindBody { wind });
            apply_machine_value(thunk, Vec::new(), ctx, frames)
        }
        MachineFrame::DynamicWindBody { wind } => {
            pop_dynamic_wind(ctx, wind.id)?;
            frames.push(MachineFrame::DynamicWindComplete { result: value });
            apply_machine_value(wind.after.clone(), Vec::new(), ctx, frames)
        }
        MachineFrame::DynamicWindComplete { result } => {
            let _ = value;
            Ok(MachineState::Value(result))
        }
        MachineFrame::WindTransition {
            pending_install,
            remaining_steps,
            target_frames,
            target_winds,
            result,
        } => {
            let _ = value;

            if let Some(wind) = pending_install {
                ctx.dynamic_winds.push(wind);
            }

            schedule_wind_transition(
                remaining_steps,
                target_frames,
                target_winds,
                result,
                ctx,
                frames,
            )
        }
    }
}

fn apply_machine_value(
    operator: Value,
    args: Vec<Value>,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    let Value::Procedure(procedure) = operator else {
        return Err(EvalError::NotAProcedure);
    };

    match procedure.as_ref() {
        Procedure::Builtin(builtin) => {
            Ok(MachineState::Value((builtin.implementation)(&args, ctx)?))
        }
        Procedure::Lambda(lambda) => {
            let call_env = bind_lambda_call_env(lambda, args)?;
            Ok(schedule_machine_sequence(&lambda.body, call_env, frames))
        }
        Procedure::CaseLambda(case_lambda) => {
            let Some(index) = case_lambda
                .clauses
                .iter()
                .position(|clause| clause.params.accepts(args.len()))
            else {
                return Err(EvalError::WrongArgumentCount {
                    name: case_lambda.name.clone(),
                    expected: case_lambda.expected_arity(),
                    got: args.len(),
                });
            };

            let clause = &case_lambda.clauses[index];
            let call_env = bind_lambda_call_env(clause, args)?;
            Ok(schedule_machine_sequence(&clause.body, call_env, frames))
        }
        Procedure::RecordConstructor(constructor) => Ok(MachineState::Value(
            apply_record_constructor(constructor, args)?,
        )),
        Procedure::RecordPredicate(predicate) => Ok(MachineState::Value(apply_record_predicate(
            predicate, args,
        )?)),
        Procedure::RecordAccessor(accessor) => {
            Ok(MachineState::Value(apply_record_accessor(accessor, args)?))
        }
        Procedure::DynamicWind(dynamic_wind) => {
            apply_machine_dynamic_wind(dynamic_wind, args, ctx, frames)
        }
        Procedure::WithExceptionHandler(with_exception_handler) => {
            apply_machine_with_exception_handler(with_exception_handler, args, ctx, frames)
        }
        Procedure::CallWithValues(call_with_values) => {
            apply_machine_call_with_values(call_with_values, args, ctx, frames)
        }
        Procedure::CallCc(callcc) => {
            expect_value_arity(&callcc.name, &args, 1)?;
            let continuation_frames = sanitize_continuation_frames(frames);
            let continuation =
                Value::Procedure(Rc::new(Procedure::Continuation(ContinuationProcedure {
                    frames: continuation_frames,
                    wind_stack: ctx.dynamic_winds.clone(),
                })));
            apply_machine_value(args[0].clone(), vec![continuation], ctx, frames)
        }
        Procedure::Continuation(continuation) => {
            expect_value_arity("continuation", &args, 1)?;
            apply_machine_continuation(continuation, args[0].clone(), ctx, frames)
        }
    }
}

fn sanitize_continuation_frames(frames: &[MachineFrame]) -> Vec<MachineFrame> {
    let mut sanitized = frames.to_vec();

    loop {
        let Some(frame) = sanitized.last_mut() else {
            break;
        };

        let MachineFrame::Sequence { remaining, .. } = frame else {
            break;
        };

        let skipped = remaining
            .iter()
            .take_while(|expr| is_direct_callcc_expr(expr))
            .count();

        if skipped == 0 {
            break;
        }

        if skipped == remaining.len() {
            sanitized.pop();
        } else {
            remaining.drain(..skipped);
            break;
        }
    }

    sanitized
}

fn is_direct_callcc_expr(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::List(items)
            if matches!(items.first(), Some(Expr::Symbol(operator)) if operator == "call/cc")
    )
}

fn apply_machine_dynamic_wind(
    dynamic_wind: &DynamicWindProcedure,
    args: Vec<Value>,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    expect_value_arity(&dynamic_wind.name, &args, 3)?;

    let wind = ctx.fresh_dynamic_wind(args[0].clone(), args[2].clone());
    frames.push(MachineFrame::DynamicWindEnter {
        wind,
        thunk: args[1].clone(),
    });
    apply_machine_value(args[0].clone(), Vec::new(), ctx, frames)
}

fn apply_machine_with_exception_handler(
    with_exception_handler: &WithExceptionHandlerProcedure,
    args: Vec<Value>,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    expect_value_arity(&with_exception_handler.name, &args, 2)?;

    frames.push(MachineFrame::WithExceptionHandler {
        handler: args[0].clone(),
        target_winds: ctx.dynamic_winds.clone(),
    });
    apply_machine_value(args[1].clone(), Vec::new(), ctx, frames)
}

fn apply_machine_call_with_values(
    call_with_values: &CallWithValuesProcedure,
    args: Vec<Value>,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    expect_value_arity(&call_with_values.name, &args, 2)?;

    frames.push(MachineFrame::CallWithValuesConsumer {
        consumer: args[1].clone(),
    });
    apply_machine_value(args[0].clone(), Vec::new(), ctx, frames)
}

fn apply_machine_continuation(
    continuation: &ContinuationProcedure,
    value: Value,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    let steps = build_wind_transition_steps(&ctx.dynamic_winds, &continuation.wind_stack);
    schedule_wind_transition(
        steps,
        continuation.frames.clone(),
        continuation.wind_stack.clone(),
        value,
        ctx,
        frames,
    )
}

fn schedule_wind_transition(
    steps: Vec<WindTransitionStep>,
    target_frames: Vec<MachineFrame>,
    target_winds: Vec<DynamicWindExtent>,
    result: Value,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    let Some((step, remaining_steps)) = steps.split_first() else {
        ctx.dynamic_winds = target_winds;
        *frames = target_frames;
        return Ok(MachineState::Value(result));
    };

    let (thunk, pending_install) = match step.clone() {
        WindTransitionStep::Exit(wind) => {
            pop_dynamic_wind(ctx, wind.id)?;
            (wind.after, None)
        }
        WindTransitionStep::Enter(wind) => (wind.before.clone(), Some(wind)),
    };

    let mut transition_frames = target_frames.clone();
    transition_frames.push(MachineFrame::WindTransition {
        pending_install,
        remaining_steps: remaining_steps.to_vec(),
        target_frames,
        target_winds,
        result,
    });
    *frames = transition_frames;
    apply_machine_value(thunk, Vec::new(), ctx, frames)
}

fn handle_machine_exception(
    exception: Value,
    ctx: &mut EvalContext,
    frames: &mut Vec<MachineFrame>,
) -> Result<MachineState, EvalError> {
    let Some((index, handler, target_winds)) = find_machine_exception_handler(frames) else {
        return Err(EvalError::UnhandledException {
            value: exception.render(),
        });
    };

    let mut target_frames = frames[..index].to_vec();
    target_frames.push(MachineFrame::RunExceptionHandler { handler, exception });

    let steps = build_wind_transition_steps(&ctx.dynamic_winds, &target_winds);
    schedule_wind_transition(steps, target_frames, target_winds, Value::Void, ctx, frames)
}

fn find_machine_exception_handler(
    frames: &[MachineFrame],
) -> Option<(usize, ExceptionHandlerKind, Vec<DynamicWindExtent>)> {
    for index in (0..frames.len()).rev() {
        match &frames[index] {
            MachineFrame::GuardHandler {
                variable,
                clauses,
                env,
                target_winds,
            } => {
                return Some((
                    index,
                    ExceptionHandlerKind::Guard {
                        variable: variable.clone(),
                        clauses: clauses.clone(),
                        env: env.clone(),
                    },
                    target_winds.clone(),
                ));
            }
            MachineFrame::WithExceptionHandler {
                handler,
                target_winds,
            } => {
                return Some((
                    index,
                    ExceptionHandlerKind::Procedure(handler.clone()),
                    target_winds.clone(),
                ));
            }
            _ => {}
        }
    }

    None
}

fn build_wind_transition_steps(
    current: &[DynamicWindExtent],
    target: &[DynamicWindExtent],
) -> Vec<WindTransitionStep> {
    let mut shared_prefix = 0;

    while shared_prefix < current.len()
        && shared_prefix < target.len()
        && current[shared_prefix].id == target[shared_prefix].id
    {
        shared_prefix += 1;
    }

    let mut steps = current[shared_prefix..]
        .iter()
        .rev()
        .cloned()
        .map(WindTransitionStep::Exit)
        .collect::<Vec<_>>();

    steps.extend(
        target[shared_prefix..]
            .iter()
            .cloned()
            .map(WindTransitionStep::Enter),
    );

    steps
}

fn pop_dynamic_wind(ctx: &mut EvalContext, expected_id: u64) -> Result<(), EvalError> {
    match ctx.dynamic_winds.pop() {
        Some(wind) if wind.id == expected_id => Ok(()),
        Some(wind) => Err(EvalError::InvariantViolation {
            message: format!(
                "dynamic-wind stack mismatch: expected {}, found {}",
                expected_id, wind.id
            ),
        }),
        None => Err(EvalError::InvariantViolation {
            message: format!(
                "attempted to pop dynamic-wind {} from an empty stack",
                expected_id
            ),
        }),
    }
}

fn eval_sequence(exprs: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Void;

    for expr in exprs {
        last = eval_expr(expr, env.clone(), ctx)?;
    }

    Ok(last)
}

fn eval_tail_sequence(
    exprs: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    let Some((last, init)) = exprs.split_last() else {
        return Ok(Value::Void);
    };

    for expr in init {
        let _ = eval_expr(expr, env.clone(), ctx)?;
    }

    eval_tail_expr(last, env, ctx)
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

fn eval_tail_expr(expr: &Expr, env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut current_expr = expr.clone();
    let mut current_env = env;

    loop {
        match current_expr {
            Expr::Number(value) => return Ok(Value::Number(value)),
            Expr::Boolean(value) => return Ok(Value::Boolean(value)),
            Expr::String(value) => return Ok(Value::String(SchemeString::new(value))),
            Expr::Char(value) => return Ok(Value::Char(value)),
            Expr::Symbol(name) => return lookup(&current_env, &name),
            Expr::List(items) => match eval_tail_list(&items, current_env, ctx)? {
                TailOutcome::Value(value) => return Ok(value),
                TailOutcome::Next { expr, env } => {
                    current_expr = expr;
                    current_env = env;
                }
            },
        }
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
            "case" => return eval_case(args, env, ctx),
            "quote" => return eval_quote(args),
            "define" => return eval_define(args, env, ctx),
            "define-record-type" => return eval_define_record_type(args, env),
            "define-syntax" => return eval_define_syntax(args, env),
            "set!" => return eval_set(args, env, ctx),
            "lambda" => return eval_lambda(args, env),
            "case-lambda" => return eval_case_lambda(args, env),
            "let" => return eval_let(args, env, ctx),
            "let*" => return eval_let_star(args, env, ctx),
            "letrec" => return eval_letrec(args, env, ctx, false),
            "letrec*" => return eval_letrec(args, env, ctx, true),
            "do" => return eval_do(args, env, ctx),
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

fn eval_tail_list(
    items: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    if items.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate an empty list".to_string(),
        });
    }

    if let Expr::Symbol(operator) = &items[0] {
        let args = &items[1..];

        match operator.as_str() {
            "and" => return eval_tail_and(args, env, ctx),
            "or" => return eval_tail_or(args, env, ctx),
            "if" => return eval_tail_if(args, env, ctx),
            "begin" => return eval_tail_begin(args, env, ctx),
            "cond" => return eval_tail_cond(args, env, ctx),
            "case" => return eval_tail_case(args, env, ctx),
            "quote" => return Ok(TailOutcome::Value(eval_quote(args)?)),
            "define" => return Ok(TailOutcome::Value(eval_define(args, env, ctx)?)),
            "define-record-type" => {
                return Ok(TailOutcome::Value(eval_define_record_type(args, env)?));
            }
            "define-syntax" => return Ok(TailOutcome::Value(eval_define_syntax(args, env)?)),
            "set!" => return Ok(TailOutcome::Value(eval_set(args, env, ctx)?)),
            "lambda" => return Ok(TailOutcome::Value(eval_lambda(args, env)?)),
            "case-lambda" => return Ok(TailOutcome::Value(eval_case_lambda(args, env)?)),
            "let" => return eval_tail_let(args, env, ctx),
            "let*" => return eval_tail_let_star(args, env, ctx),
            "letrec" => return eval_tail_letrec(args, env, ctx, false),
            "letrec*" => return eval_tail_letrec(args, env, ctx, true),
            "do" => return Ok(TailOutcome::Value(eval_do(args, env, ctx)?)),
            _ => {}
        }

        if let Some(transformer) = lookup_macro(&env, operator) {
            let (expanded, expansion_env) = expand_macro_call(transformer, items, env, ctx)?;
            return Ok(TailOutcome::Next {
                expr: expanded,
                env: expansion_env,
            });
        }
    }

    let operator = eval_expr(&items[0], env.clone(), ctx)?;
    let args = eval_args(&items[1..], env, ctx)?;
    tail_apply_evaluated(operator, args, ctx)
}

fn prepare_tail_sequence(
    exprs: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    let Some((last, init)) = exprs.split_last() else {
        return Ok(TailOutcome::Value(Value::Void));
    };

    for expr in init {
        let _ = eval_expr(expr, env.clone(), ctx)?;
    }

    Ok(TailOutcome::Next {
        expr: last.clone(),
        env,
    })
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

fn eval_tail_and(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    let Some((last, init)) = args.split_last() else {
        return Ok(TailOutcome::Value(Value::Boolean(true)));
    };

    for expr in init {
        let value = eval_expr(expr, env.clone(), ctx)?;
        if !value.is_truthy() {
            return Ok(TailOutcome::Value(value));
        }
    }

    Ok(TailOutcome::Next {
        expr: last.clone(),
        env,
    })
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

fn eval_tail_or(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    let Some((last, init)) = args.split_last() else {
        return Ok(TailOutcome::Value(Value::Boolean(false)));
    };

    for expr in init {
        let value = eval_expr(expr, env.clone(), ctx)?;
        if value.is_truthy() {
            return Ok(TailOutcome::Value(value));
        }
    }

    Ok(TailOutcome::Next {
        expr: last.clone(),
        env,
    })
}

fn eval_if(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if !(2..=3).contains(&args.len()) {
        return Err(EvalError::WrongArgumentCount {
            name: "if".to_string(),
            expected: "2 or 3".to_string(),
            got: args.len(),
        });
    }

    let condition = eval_expr(&args[0], env.clone(), ctx)?;
    if condition.is_truthy() {
        eval_expr(&args[1], env, ctx)
    } else if args.len() == 3 {
        eval_expr(&args[2], env, ctx)
    } else {
        Ok(Value::Void)
    }
}

fn eval_tail_if(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    if !(2..=3).contains(&args.len()) {
        return Err(EvalError::WrongArgumentCount {
            name: "if".to_string(),
            expected: "2 or 3".to_string(),
            got: args.len(),
        });
    }

    let condition = eval_expr(&args[0], env.clone(), ctx)?;
    if condition.is_truthy() {
        Ok(TailOutcome::Next {
            expr: args[1].clone(),
            env,
        })
    } else if args.len() == 3 {
        Ok(TailOutcome::Next {
            expr: args[2].clone(),
            env,
        })
    } else {
        Ok(TailOutcome::Value(Value::Void))
    }
}

fn eval_begin(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    eval_sequence(args, env, ctx)
}

fn eval_tail_begin(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    prepare_tail_sequence(args, env, ctx)
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

fn eval_tail_cond(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
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
                    return Ok(TailOutcome::Value(Value::Void));
                }
                return prepare_tail_sequence(&items[1..], env, ctx);
            }
        }

        let test_value = eval_expr(&items[0], env.clone(), ctx)?;
        if test_value.is_truthy() {
            if items.len() == 1 {
                return Ok(TailOutcome::Value(test_value));
            }
            return prepare_tail_sequence(&items[1..], env, ctx);
        }
    }

    Ok(TailOutcome::Value(Value::Void))
}

fn eval_case(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "case requires a key expression".to_string(),
        });
    }

    let key = eval_expr(&args[0], env.clone(), ctx)?;

    for (index, clause) in args[1..].iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::SyntaxError {
                message: "case clauses must be lists".to_string(),
            });
        };

        if items.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "case clauses cannot be empty".to_string(),
            });
        }

        if let Expr::Symbol(symbol) = &items[0] {
            if symbol == "else" {
                if index + 2 != args.len() {
                    return Err(EvalError::SyntaxError {
                        message: "case else clause must be last".to_string(),
                    });
                }

                if items.len() == 1 {
                    return Ok(Value::Void);
                }

                return eval_sequence(&items[1..], env, ctx);
            }
        }

        let Expr::List(datums) = &items[0] else {
            return Err(EvalError::SyntaxError {
                message: "case clauses must start with a datum list or else".to_string(),
            });
        };

        if datums
            .iter()
            .any(|datum| eqv_values(&key, &quote_expr(datum)))
        {
            if items.len() == 1 {
                return Ok(Value::Void);
            }

            return eval_sequence(&items[1..], env, ctx);
        }
    }

    Ok(Value::Void)
}

fn eval_tail_case(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "case requires a key expression".to_string(),
        });
    }

    let key = eval_expr(&args[0], env.clone(), ctx)?;

    for (index, clause) in args[1..].iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::SyntaxError {
                message: "case clauses must be lists".to_string(),
            });
        };

        if items.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "case clauses cannot be empty".to_string(),
            });
        }

        if let Expr::Symbol(symbol) = &items[0] {
            if symbol == "else" {
                if index + 2 != args.len() {
                    return Err(EvalError::SyntaxError {
                        message: "case else clause must be last".to_string(),
                    });
                }

                if items.len() == 1 {
                    return Ok(TailOutcome::Value(Value::Void));
                }

                return prepare_tail_sequence(&items[1..], env, ctx);
            }
        }

        let Expr::List(datums) = &items[0] else {
            return Err(EvalError::SyntaxError {
                message: "case clauses must start with a datum list or else".to_string(),
            });
        };

        if datums
            .iter()
            .any(|datum| eqv_values(&key, &quote_expr(datum)))
        {
            if items.len() == 1 {
                return Ok(TailOutcome::Value(Value::Void));
            }

            return prepare_tail_sequence(&items[1..], env, ctx);
        }
    }

    Ok(TailOutcome::Value(Value::Void))
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
            make_pair(quote_expr(item), tail)
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
            let target_env = definition_target_env(&env);
            bind_value(&target_env, name.clone(), value);
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
            let target_env = definition_target_env(&env);
            bind_value(&target_env, name.clone(), lambda);
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
    let target_env = definition_target_env(&env);

    bind_value(
        &target_env,
        constructor_name.clone(),
        make_record_constructor(constructor_name, record_type.clone(), field_count),
    );
    bind_value(
        &target_env,
        predicate_name.clone(),
        make_record_predicate(predicate_name, record_type.clone()),
    );

    for (field_index, accessor_name) in accessors.into_iter().enumerate() {
        bind_value(
            &target_env,
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
    let target_env = definition_target_env(&env);
    bind_macro(&target_env, name.clone(), transformer);
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

fn eval_case_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "case-lambda requires at least one clause".to_string(),
        });
    }

    let mut clauses = Vec::with_capacity(args.len());

    for clause in args {
        let Expr::List(items) = clause else {
            return Err(EvalError::SyntaxError {
                message: "case-lambda clauses must be lists".to_string(),
            });
        };

        if items.len() < 2 {
            return Err(EvalError::SyntaxError {
                message: "case-lambda clauses require parameters and a body".to_string(),
            });
        }

        clauses.push(LambdaProcedure {
            name: "case-lambda".to_string(),
            params: parse_param_list(&items[0])?,
            body: items[1..].to_vec(),
            env: env.clone(),
        });
    }

    Ok(make_case_lambda("case-lambda".to_string(), clauses))
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

fn eval_tail_let(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires bindings".to_string(),
        });
    }

    match &args[0] {
        Expr::Symbol(name) => eval_tail_named_let(name, &args[1..], env, ctx),
        bindings => eval_tail_let_bindings(bindings, &args[1..], env, ctx),
    }
}

fn eval_let_star(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::SyntaxError {
            message: "let* requires bindings and a body".to_string(),
        });
    }

    let let_star_env = eval_let_star_bindings(&args[0], env, ctx)?;
    eval_sequence(&args[1..], let_star_env, ctx)
}

fn eval_tail_let_star(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::SyntaxError {
            message: "let* requires bindings and a body".to_string(),
        });
    }

    let let_star_env = eval_let_star_bindings(&args[0], env, ctx)?;
    prepare_tail_sequence(&args[1..], let_star_env, ctx)
}

fn eval_letrec(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
    sequential: bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::SyntaxError {
            message: "letrec requires bindings and a body".to_string(),
        });
    }

    let bindings = parse_let_bindings(&args[0])?;
    let letrec_env = Env::child(env);
    let mut binding_cells = Vec::with_capacity(bindings.len());

    {
        let mut scope = letrec_env.borrow_mut();
        for (name, _) in &bindings {
            let cell = Rc::new(RefCell::new(Value::Uninitialized));
            scope.bindings.insert(name.clone(), cell.clone());
            binding_cells.push(cell);
        }
    }

    if sequential {
        for ((_, expr), cell) in bindings.iter().zip(binding_cells.iter()) {
            let value = eval_expr(expr, letrec_env.clone(), ctx)?;
            *cell.borrow_mut() = value;
        }
    } else {
        let mut values = Vec::with_capacity(bindings.len());
        for (_, expr) in &bindings {
            values.push(eval_expr(expr, letrec_env.clone(), ctx)?);
        }

        for (cell, value) in binding_cells.into_iter().zip(values) {
            *cell.borrow_mut() = value;
        }
    }

    eval_sequence(&args[1..], letrec_env, ctx)
}

fn eval_tail_letrec(
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
    sequential: bool,
) -> Result<TailOutcome, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::SyntaxError {
            message: "letrec requires bindings and a body".to_string(),
        });
    }

    let bindings = parse_let_bindings(&args[0])?;
    let letrec_env = Env::child(env);
    let mut binding_cells = Vec::with_capacity(bindings.len());

    {
        let mut scope = letrec_env.borrow_mut();
        for (name, _) in &bindings {
            let cell = Rc::new(RefCell::new(Value::Uninitialized));
            scope.bindings.insert(name.clone(), cell.clone());
            binding_cells.push(cell);
        }
    }

    if sequential {
        for ((_, expr), cell) in bindings.iter().zip(binding_cells.iter()) {
            let value = eval_expr(expr, letrec_env.clone(), ctx)?;
            *cell.borrow_mut() = value;
        }
    } else {
        let mut values = Vec::with_capacity(bindings.len());
        for (_, expr) in &bindings {
            values.push(eval_expr(expr, letrec_env.clone(), ctx)?);
        }

        for (cell, value) in binding_cells.into_iter().zip(values) {
            *cell.borrow_mut() = value;
        }
    }

    prepare_tail_sequence(&args[1..], letrec_env, ctx)
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

fn eval_tail_named_let(
    name: &str,
    args: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
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
    tail_apply_evaluated(lambda, values, ctx)
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

fn eval_tail_let_bindings(
    bindings_expr: &Expr,
    body: &[Expr],
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
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
    prepare_tail_sequence(body, let_env, ctx)
}

fn eval_let_star_bindings(
    bindings_expr: &Expr,
    env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<EnvRef, EvalError> {
    let bindings = parse_let_bindings(bindings_expr)?;
    let let_star_env = Env::child(env);

    for (name, expr) in bindings {
        let value = eval_expr(&expr, let_star_env.clone(), ctx)?;
        let_star_env
            .borrow_mut()
            .bindings
            .insert(name, Rc::new(RefCell::new(value)));
    }

    Ok(let_star_env)
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

fn eval_do(args: &[Expr], env: EnvRef, ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::SyntaxError {
            message: "do requires bindings and a test clause".to_string(),
        });
    }

    let bindings = parse_do_bindings(&args[0])?;
    let Expr::List(test_clause) = &args[1] else {
        return Err(EvalError::SyntaxError {
            message: "do test clause must be a list".to_string(),
        });
    };

    if test_clause.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "do test clause cannot be empty".to_string(),
        });
    }

    let init_values = bindings
        .iter()
        .map(|binding| eval_expr(&binding.init, env.clone(), ctx))
        .collect::<Result<Vec<_>, _>>()?;
    let loop_env = Env::child(env);
    let mut cells = Vec::with_capacity(bindings.len());

    {
        let mut scope = loop_env.borrow_mut();
        for (binding, value) in bindings.iter().zip(init_values) {
            let cell = Rc::new(RefCell::new(value));
            scope.bindings.insert(binding.name.clone(), cell.clone());
            cells.push(cell);
        }
    }

    loop {
        let test_value = eval_expr(&test_clause[0], loop_env.clone(), ctx)?;
        if test_value.is_truthy() {
            if test_clause.len() == 1 {
                return Ok(Value::Void);
            }

            return eval_sequence(&test_clause[1..], loop_env, ctx);
        }

        if !args[2..].is_empty() {
            let _ = eval_sequence(&args[2..], loop_env.clone(), ctx)?;
        }

        let mut next_values = Vec::with_capacity(bindings.len());
        for (binding, cell) in bindings.iter().zip(cells.iter()) {
            let value = match &binding.step {
                Some(step) => eval_expr(step, loop_env.clone(), ctx)?,
                None => cell.borrow().clone(),
            };
            next_values.push(value);
        }

        for (cell, value) in cells.iter().zip(next_values) {
            *cell.borrow_mut() = value;
        }
    }
}

fn parse_do_bindings(expr: &Expr) -> Result<Vec<DoBinding>, EvalError> {
    let Expr::List(bindings) = expr else {
        return Err(EvalError::SyntaxError {
            message: "do bindings must be a list".to_string(),
        });
    };

    let mut parsed = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let Expr::List(parts) = binding else {
            return Err(EvalError::SyntaxError {
                message: "do bindings must be lists".to_string(),
            });
        };

        if !(2..=3).contains(&parts.len()) {
            return Err(EvalError::SyntaxError {
                message: "do bindings must have a name, init, and optional step".to_string(),
            });
        }

        let Expr::Symbol(name) = &parts[0] else {
            return Err(EvalError::SyntaxError {
                message: "do binding names must be symbols".to_string(),
            });
        };

        parsed.push(DoBinding {
            name: name.clone(),
            init: parts[1].clone(),
            step: parts.get(2).cloned(),
        });
    }

    Ok(parsed)
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

    let name = expect_symbol_expr(
        name_expr,
        "define-record-type constructor name must be a symbol",
    )?;
    for field in fields {
        let _ = expect_symbol_expr(
            field,
            "define-record-type constructor fields must be symbols",
        )?;
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

fn make_case_lambda(name: String, clauses: Vec<LambdaProcedure>) -> Value {
    Value::Procedure(Rc::new(Procedure::CaseLambda(CaseLambdaProcedure {
        name,
        clauses,
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
    Value::Procedure(Rc::new(Procedure::RecordAccessor(
        RecordAccessorProcedure {
            name,
            record_type,
            field_index,
        },
    )))
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

fn tail_apply_evaluated(
    operator: Value,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    let Value::Procedure(procedure) = operator else {
        return Err(EvalError::NotAProcedure);
    };

    tail_apply_procedure(procedure, args, ctx)
}

fn apply_procedure(
    procedure: Rc<Procedure>,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    match procedure.as_ref() {
        Procedure::Builtin(builtin) => (builtin.implementation)(&args, ctx),
        Procedure::Lambda(lambda) => apply_lambda(lambda, args, ctx),
        Procedure::CaseLambda(case_lambda) => apply_case_lambda(case_lambda, args, ctx),
        Procedure::RecordConstructor(constructor) => apply_record_constructor(constructor, args),
        Procedure::RecordPredicate(predicate) => apply_record_predicate(predicate, args),
        Procedure::RecordAccessor(accessor) => apply_record_accessor(accessor, args),
        Procedure::DynamicWind(dynamic_wind) => apply_dynamic_wind(dynamic_wind, args, ctx),
        Procedure::WithExceptionHandler(with_exception_handler) => {
            apply_with_exception_handler(with_exception_handler, args, ctx)
        }
        Procedure::CallWithValues(call_with_values) => {
            apply_call_with_values(call_with_values, args, ctx)
        }
        Procedure::CallCc(callcc) => apply_callcc(callcc, args, ctx),
        Procedure::Continuation(continuation) => apply_continuation(continuation, args, ctx),
    }
}

fn tail_apply_procedure(
    procedure: Rc<Procedure>,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    match procedure.as_ref() {
        Procedure::Builtin(builtin) => {
            Ok(TailOutcome::Value((builtin.implementation)(&args, ctx)?))
        }
        Procedure::Lambda(lambda) => tail_apply_lambda(lambda, args, ctx),
        Procedure::CaseLambda(case_lambda) => tail_apply_case_lambda(case_lambda, args, ctx),
        Procedure::RecordConstructor(constructor) => Ok(TailOutcome::Value(
            apply_record_constructor(constructor, args)?,
        )),
        Procedure::RecordPredicate(predicate) => {
            Ok(TailOutcome::Value(apply_record_predicate(predicate, args)?))
        }
        Procedure::RecordAccessor(accessor) => {
            Ok(TailOutcome::Value(apply_record_accessor(accessor, args)?))
        }
        Procedure::DynamicWind(dynamic_wind) => Ok(TailOutcome::Value(apply_dynamic_wind(
            dynamic_wind,
            args,
            ctx,
        )?)),
        Procedure::WithExceptionHandler(with_exception_handler) => Ok(TailOutcome::Value(
            apply_with_exception_handler(with_exception_handler, args, ctx)?,
        )),
        Procedure::CallWithValues(call_with_values) => {
            tail_apply_call_with_values(call_with_values, args, ctx)
        }
        Procedure::CallCc(callcc) => Ok(TailOutcome::Value(apply_callcc(callcc, args, ctx)?)),
        Procedure::Continuation(continuation) => Ok(TailOutcome::Value(apply_continuation(
            continuation,
            args,
            ctx,
        )?)),
    }
}

fn apply_dynamic_wind(
    dynamic_wind: &DynamicWindProcedure,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    expect_value_arity(&dynamic_wind.name, &args, 3)?;

    let wind = ctx.fresh_dynamic_wind(args[0].clone(), args[2].clone());
    let _ = apply_evaluated(args[0].clone(), Vec::new(), ctx)?;

    ctx.dynamic_winds.push(wind.clone());
    let body_result = apply_evaluated(args[1].clone(), Vec::new(), ctx);
    let pop_result = pop_dynamic_wind(ctx, wind.id);
    let out_result = apply_evaluated(wind.after.clone(), Vec::new(), ctx);

    pop_result?;
    let _ = out_result?;
    body_result
}

fn apply_with_exception_handler(
    with_exception_handler: &WithExceptionHandlerProcedure,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    expect_value_arity(&with_exception_handler.name, &args, 2)?;

    match apply_evaluated(args[1].clone(), Vec::new(), ctx) {
        Ok(value) => Ok(value),
        Err(EvalError::ExceptionRaisedSignal) => {
            let exception = take_pending_exception(ctx)?;
            apply_evaluated(args[0].clone(), vec![exception], ctx)
        }
        Err(error) => Err(error),
    }
}

fn apply_call_with_values(
    call_with_values: &CallWithValuesProcedure,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    expect_value_arity(&call_with_values.name, &args, 2)?;
    let produced = apply_evaluated(args[0].clone(), Vec::new(), ctx)?;
    apply_evaluated(args[1].clone(), unpack_values(produced), ctx)
}

fn tail_apply_call_with_values(
    call_with_values: &CallWithValuesProcedure,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    expect_value_arity(&call_with_values.name, &args, 2)?;
    let produced = apply_evaluated(args[0].clone(), Vec::new(), ctx)?;
    tail_apply_evaluated(args[1].clone(), unpack_values(produced), ctx)
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

fn bind_lambda_call_env(lambda: &LambdaProcedure, args: Vec<Value>) -> Result<EnvRef, EvalError> {
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

    Ok(call_env)
}

fn apply_lambda(
    lambda: &LambdaProcedure,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    let call_env = bind_lambda_call_env(lambda, args)?;
    eval_tail_sequence(&lambda.body, call_env, ctx)
}

fn tail_apply_lambda(
    lambda: &LambdaProcedure,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    let call_env = bind_lambda_call_env(lambda, args)?;
    prepare_tail_sequence(&lambda.body, call_env, ctx)
}

fn apply_case_lambda(
    case_lambda: &CaseLambdaProcedure,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    let Some(index) = case_lambda
        .clauses
        .iter()
        .position(|clause| clause.params.accepts(args.len()))
    else {
        return Err(EvalError::WrongArgumentCount {
            name: case_lambda.name.clone(),
            expected: case_lambda.expected_arity(),
            got: args.len(),
        });
    };

    apply_lambda(&case_lambda.clauses[index], args, ctx)
}

fn tail_apply_case_lambda(
    case_lambda: &CaseLambdaProcedure,
    args: Vec<Value>,
    ctx: &mut EvalContext,
) -> Result<TailOutcome, EvalError> {
    let Some(index) = case_lambda
        .clauses
        .iter()
        .position(|clause| clause.params.accepts(args.len()))
    else {
        return Err(EvalError::WrongArgumentCount {
            name: case_lambda.name.clone(),
            expected: case_lambda.expected_arity(),
            got: args.len(),
        });
    };

    tail_apply_lambda(&case_lambda.clauses[index], args, ctx)
}

fn apply_callcc(
    _callcc: &CallCcProcedure,
    _args: Vec<Value>,
    _ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    Err(EvalError::UnsupportedContinuationContext)
}

fn apply_continuation(
    _continuation: &ContinuationProcedure,
    _args: Vec<Value>,
    _ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    Err(EvalError::UnsupportedContinuationContext)
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
    Ok(Value::Boolean(eqv_values(&args[0], &args[1])))
}

fn builtin_eqv(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("eqv?", args, 2)?;
    Ok(Value::Boolean(eqv_values(&args[0], &args[1])))
}

fn builtin_equal(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("equal?", args, 2)?;
    Ok(Value::Boolean(equal_values(&args[0], &args[1])))
}

fn builtin_cons(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("cons", args, 2)?;
    Ok(make_pair(args[0].clone(), args[1].clone()))
}

fn builtin_car(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("car", args, 1)?;
    let Value::Pair(pair) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            actual: args[0].type_name(),
        });
    };
    Ok(pair_car(pair))
}

fn builtin_cdr(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("cdr", args, 1)?;
    let Value::Pair(pair) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            actual: args[0].type_name(),
        });
    };
    Ok(pair_cdr(pair))
}

fn builtin_caar(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_compound_accessor("caar", args, "aa")
}

fn builtin_cadr(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_compound_accessor("cadr", args, "ad")
}

fn builtin_cdar(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_compound_accessor("cdar", args, "da")
}

fn builtin_cddr(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_compound_accessor("cddr", args, "dd")
}

macro_rules! define_compound_accessor_builtin {
    ($fn_name:ident, $name:literal, $path:literal) => {
        fn $fn_name(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
            builtin_compound_accessor($name, args, $path)
        }
    };
}

define_compound_accessor_builtin!(builtin_caaar, "caaar", "aaa");
define_compound_accessor_builtin!(builtin_caadr, "caadr", "aad");
define_compound_accessor_builtin!(builtin_cadar, "cadar", "ada");
define_compound_accessor_builtin!(builtin_caddr, "caddr", "add");
define_compound_accessor_builtin!(builtin_cdaar, "cdaar", "daa");
define_compound_accessor_builtin!(builtin_cdadr, "cdadr", "dad");
define_compound_accessor_builtin!(builtin_cddar, "cddar", "dda");
define_compound_accessor_builtin!(builtin_cdddr, "cdddr", "ddd");
define_compound_accessor_builtin!(builtin_caaaar, "caaaar", "aaaa");
define_compound_accessor_builtin!(builtin_caaadr, "caaadr", "aaad");
define_compound_accessor_builtin!(builtin_caadar, "caadar", "aada");
define_compound_accessor_builtin!(builtin_caaddr, "caaddr", "aadd");
define_compound_accessor_builtin!(builtin_cadaar, "cadaar", "adaa");
define_compound_accessor_builtin!(builtin_cadadr, "cadadr", "adad");
define_compound_accessor_builtin!(builtin_caddar, "caddar", "adda");
define_compound_accessor_builtin!(builtin_cadddr, "cadddr", "addd");
define_compound_accessor_builtin!(builtin_cdaaar, "cdaaar", "daaa");
define_compound_accessor_builtin!(builtin_cdaadr, "cdaadr", "daad");
define_compound_accessor_builtin!(builtin_cdadar, "cdadar", "dada");
define_compound_accessor_builtin!(builtin_cdaddr, "cdaddr", "dadd");
define_compound_accessor_builtin!(builtin_cddaar, "cddaar", "ddaa");
define_compound_accessor_builtin!(builtin_cddadr, "cddadr", "ddad");
define_compound_accessor_builtin!(builtin_cdddar, "cdddar", "ddda");
define_compound_accessor_builtin!(builtin_cddddr, "cddddr", "dddd");

fn builtin_compound_accessor(name: &str, args: &[Value], path: &str) -> Result<Value, EvalError> {
    expect_value_arity(name, args, 1)?;
    let mut current = args[0].clone();

    for op in path.chars().rev() {
        let Value::Pair(pair) = current else {
            return Err(EvalError::TypeMismatch {
                expected: "pair",
                actual: current.type_name(),
            });
        };

        current = match op {
            'a' => pair_car(&pair),
            'd' => pair_cdr(&pair),
            _ => unreachable!("compound accessor path should only contain car/cdr markers"),
        };
    }

    Ok(current)
}

fn builtin_set_car(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("set-car!", args, 2)?;
    let Value::Pair(pair) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            actual: args[0].type_name(),
        });
    };

    set_pair_car(pair, args[1].clone());
    Ok(Value::Void)
}

fn builtin_set_cdr(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("set-cdr!", args, 2)?;
    let Value::Pair(pair) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            actual: args[0].type_name(),
        });
    };

    set_pair_cdr(pair, args[1].clone());
    Ok(Value::Void)
}

fn builtin_null_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("null?", args, 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::EmptyList)))
}

fn builtin_list(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    Ok(list_from_values(args.to_vec()))
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

fn builtin_reverse(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("reverse", args, 1)?;
    let mut values = expect_list_values(&args[0], "reverse")?;
    values.reverse();
    Ok(list_from_values(values))
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
    builtin_type_predicate("pair?", args, |value| matches!(value, Value::Pair(_)))
}

fn builtin_symbol_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("symbol?", args, |value| matches!(value, Value::Symbol(_)))
}

fn builtin_procedure_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("procedure?", args, |value| {
        matches!(value, Value::Procedure(_))
    })
}

fn builtin_char_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("char?", args, |value| matches!(value, Value::Char(_)))
}

fn builtin_display(args: &[Value], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("display", args, 1)?;
    ctx.emit(&args[0].render_for_display());
    Ok(Value::Void)
}

fn builtin_raise(args: &[Value], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("raise", args, 1)?;
    ctx.pending_exception = Some(args[0].clone());
    Err(EvalError::ExceptionRaisedSignal)
}

fn builtin_values(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    Ok(pack_values(args.to_vec()))
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
    applied_args.extend(expect_list_values(&rest[rest.len() - 1], "apply")?);

    apply_evaluated(operator.clone(), applied_args, ctx)
}

fn builtin_string_append(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut result = String::new();

    for value in args {
        result.push_str(&expect_string(value)?.to_plain_string());
    }

    Ok(Value::String(SchemeString::new(result)))
}

fn builtin_make_string(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if !(1..=2).contains(&args.len()) {
        return Err(EvalError::WrongArgumentCount {
            name: "make-string".to_string(),
            expected: "1 or 2".to_string(),
            got: args.len(),
        });
    }

    let length = expect_index(&args[0], "make-string")?;
    let fill = args.get(1).map(expect_char).transpose()?.unwrap_or('\0');
    Ok(Value::String(SchemeString::from_mutable_chars(vec![
        fill;
        length
    ])))
}

fn builtin_string(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let chars = args
        .iter()
        .map(expect_char)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Value::String(SchemeString::from_mutable_chars(chars)))
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

fn builtin_string_to_list(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("string->list", args, 1)?;
    let chars = expect_string(&args[0])?.to_chars();
    Ok(list_from_values(
        chars.into_iter().map(Value::Char).collect(),
    ))
}

fn builtin_list_to_string(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("list->string", args, 1)?;
    let chars = expect_list_values(&args[0], "list->string")?
        .into_iter()
        .map(|value| expect_char(&value))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Value::String(SchemeString::from_chars(chars)))
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

fn builtin_gcd(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut result = 0_i64;

    for value in args {
        result = gcd_exact_integers(result, expect_exact_integer(value, "gcd")?)?;
    }

    Ok(Value::Number(Number::integer(result)))
}

fn builtin_lcm(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    let mut result = 1_i64;

    for value in args {
        result = lcm_exact_integers(result, expect_exact_integer(value, "lcm")?)?;
    }

    Ok(Value::Number(Number::integer(result)))
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

fn builtin_truncate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("truncate", args, 1)?;
    Ok(Value::Number(truncate_number(expect_number(&args[0])?)?))
}

fn builtin_round(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("round", args, 1)?;
    Ok(Value::Number(round_number(expect_number(&args[0])?)?))
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
        Value::Pair(pair) => Ok(pair_car(&pair)),
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
    builtin_assoc_by("assoc", args, equal_values)
}

fn builtin_assv(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("assv", args, 2)?;
    builtin_assoc_by("assv", args, eqv_values)
}

fn builtin_assoc_by(
    name: &str,
    args: &[Value],
    predicate: fn(&Value, &Value) -> bool,
) -> Result<Value, EvalError> {
    let key = &args[0];
    let mut cursor = args[1].clone();
    let mut seen = HashSet::new();

    loop {
        match cursor {
            Value::EmptyList => return Ok(Value::Boolean(false)),
            Value::Pair(entry_pair) => {
                if !seen.insert(pair_id(&entry_pair)) {
                    return Err(EvalError::CircularList {
                        name: name.to_string(),
                    });
                }

                let (entry, rest) = pair_parts(&entry_pair);
                let Value::Pair(found_entry) = &entry else {
                    return Err(EvalError::TypeMismatch {
                        expected: "pair",
                        actual: entry.type_name(),
                    });
                };

                if predicate(key, &pair_car(found_entry)) {
                    return Ok(entry);
                }

                cursor = rest;
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

fn builtin_member(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("member", args, 2)?;
    let key = &args[0];
    let mut cursor = args[1].clone();
    let mut seen = HashSet::new();

    loop {
        match cursor {
            Value::EmptyList => return Ok(Value::Boolean(false)),
            Value::Pair(pair) => {
                if !seen.insert(pair_id(&pair)) {
                    return Err(EvalError::CircularList {
                        name: "member".to_string(),
                    });
                }

                let car = pair_car(&pair);
                if equal_values(key, &car) {
                    return Ok(Value::Pair(pair));
                }

                cursor = pair_cdr(&pair);
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
        .map(|value| expect_list_values(value, "map"))
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

fn builtin_for_each(args: &[Value], ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: "for-each".to_string(),
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }

    let procedure = expect_procedure(&args[0])?;
    let list_values = args[1..]
        .iter()
        .map(|value| expect_list_values(value, "for-each"))
        .collect::<Result<Vec<_>, _>>()?;

    let expected_len = list_values.first().map_or(0, Vec::len);
    for values in &list_values[1..] {
        if values.len() != expected_len {
            return Err(EvalError::MismatchedListLengths {
                name: "for-each".to_string(),
                expected: expected_len,
                got: values.len(),
            });
        }
    }

    for index in 0..expected_len {
        let call_args = list_values
            .iter()
            .map(|values| values[index].clone())
            .collect();
        let _ = apply_procedure(procedure.clone(), call_args, ctx)?;
    }

    Ok(Value::Void)
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

fn builtin_char_to_integer(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("char->integer", args, 1)?;
    Ok(Value::Number(Number::integer(i64::from(u32::from(
        expect_char(&args[0])?,
    )))))
}

fn builtin_integer_to_char(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("integer->char", args, 1)?;
    let value = expect_exact_integer(&args[0], "integer->char")?;
    let code_point =
        u32::try_from(value).map_err(|_| EvalError::InvalidCharacterCodePoint { value })?;
    let ch = char::from_u32(code_point).ok_or(EvalError::InvalidCharacterCodePoint { value })?;
    Ok(Value::Char(ch))
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

fn builtin_string_greater_than(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_string_compare(
        "string>?",
        args,
        |value| value.to_string(),
        |left, right| left > right,
    )
}

fn builtin_string_less_equal(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_string_compare(
        "string<=?",
        args,
        |value| value.to_string(),
        |left, right| left <= right,
    )
}

fn builtin_string_greater_equal(
    args: &[Value],
    _ctx: &mut EvalContext,
) -> Result<Value, EvalError> {
    builtin_string_compare(
        "string>=?",
        args,
        |value| value.to_string(),
        |left, right| left >= right,
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

fn builtin_vector(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    Ok(Value::Vector(SchemeVector::new(args.to_vec())))
}

fn builtin_make_vector(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    if !(1..=2).contains(&args.len()) {
        return Err(EvalError::WrongArgumentCount {
            name: "make-vector".to_string(),
            expected: "1 or 2".to_string(),
            got: args.len(),
        });
    }

    let length = expect_index(&args[0], "make-vector")?;
    let fill = args.get(1).cloned().unwrap_or(Value::Void);

    Ok(Value::Vector(SchemeVector::new(vec![fill; length])))
}

fn builtin_vector_ref(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("vector-ref", args, 2)?;
    let vector = expect_vector(&args[0])?;
    let index = expect_index(&args[1], "vector-ref")?;
    vector.get(index, "vector-ref")
}

fn builtin_vector_set(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("vector-set!", args, 3)?;
    let vector = expect_vector(&args[0])?;
    let index = expect_index(&args[1], "vector-set!")?;
    vector.set(index, args[2].clone(), "vector-set!")?;
    Ok(Value::Void)
}

fn builtin_vector_length(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("vector-length", args, 1)?;
    Ok(Value::Number(Number::integer(
        expect_vector(&args[0])?.len() as i64,
    )))
}

fn builtin_vector_predicate(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_type_predicate("vector?", args, |value| matches!(value, Value::Vector(_)))
}

fn builtin_vector_to_list(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("vector->list", args, 1)?;
    Ok(list_from_values(expect_vector(&args[0])?.to_values()))
}

fn builtin_list_to_vector(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    expect_value_arity("list->vector", args, 1)?;
    Ok(Value::Vector(SchemeVector::new(expect_list_values(
        &args[0],
        "list->vector",
    )?)))
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

fn builtin_greater_equal(args: &[Value], _ctx: &mut EvalContext) -> Result<Value, EvalError> {
    builtin_compare(">=", args, |left, right| !left.compare(right).is_lt())
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

fn gcd_exact_integers(left: i64, right: i64) -> Result<i64, EvalError> {
    let mut left = i128::from(left).abs();
    let mut right = i128::from(right).abs();

    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }

    i64::try_from(left).map_err(|_| EvalError::NumericOverflow { operation: "gcd" })
}

fn lcm_exact_integers(left: i64, right: i64) -> Result<i64, EvalError> {
    if left == 0 || right == 0 {
        return Ok(0);
    }

    let gcd = i128::from(gcd_exact_integers(left, right)?);
    let lcm = (i128::from(left) / gcd) * i128::from(right);
    i64::try_from(lcm.abs()).map_err(|_| EvalError::NumericOverflow { operation: "lcm" })
}

fn truncate_number(number: Number) -> Result<Number, EvalError> {
    if number.is_exact() {
        let numerator = i128::from(number.numerator()?);
        let denominator = i128::from(number.denominator()?);
        let truncated = numerator / denominator;
        return Ok(Number::integer(i64::try_from(truncated).map_err(|_| {
            EvalError::NumericOverflow {
                operation: "truncate",
            }
        })?));
    }

    Ok(Number::inexact(number.to_f64().trunc()))
}

fn round_number(number: Number) -> Result<Number, EvalError> {
    if number.is_exact() {
        let numerator = i128::from(number.numerator()?);
        let denominator = i128::from(number.denominator()?);
        let mut quotient = numerator / denominator;
        let remainder = (numerator % denominator).abs();

        if remainder * 2 >= denominator {
            quotient += numerator.signum();
        }

        return Ok(Number::integer(
            i64::try_from(quotient)
                .map_err(|_| EvalError::NumericOverflow { operation: "round" })?,
        ));
    }

    Ok(Number::inexact(number.to_f64().round()))
}

fn is_proper_list(value: &Value) -> bool {
    let mut slow = value.clone();
    let mut fast = value.clone();

    loop {
        fast = match fast {
            Value::EmptyList => return true,
            Value::Pair(pair) => pair_cdr(&pair),
            _ => return false,
        };

        fast = match fast {
            Value::EmptyList => return true,
            Value::Pair(pair) => pair_cdr(&pair),
            _ => return false,
        };

        slow = match slow {
            Value::EmptyList => return true,
            Value::Pair(pair) => pair_cdr(&pair),
            _ => return false,
        };

        let (Value::Pair(slow_pair), Value::Pair(fast_pair)) = (&slow, &fast) else {
            continue;
        };

        if Rc::ptr_eq(slow_pair, fast_pair) {
            return false;
        }
    }
}

fn list_tail_at(value: &Value, index: usize, kind: &'static str) -> Result<Value, EvalError> {
    let mut cursor = value.clone();

    for depth in 0..index {
        cursor = match cursor {
            Value::Pair(pair) => pair_cdr(&pair),
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

    Ok(cursor)
}

fn equal_values(left: &Value, right: &Value) -> bool {
    let mut state = EqualityState::default();
    equal_values_inner(left, right, &mut state)
}

#[derive(Default)]
struct EqualityState {
    pairs: HashSet<(usize, usize)>,
    vectors: HashSet<(usize, usize)>,
}

fn equal_values_inner(left: &Value, right: &Value, state: &mut EqualityState) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left.numeric_eq(*right),
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => {
            left.to_plain_string() == right.to_plain_string()
        }
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left_pair), Value::Pair(right_pair)) => {
            let key = (pair_id(left_pair), pair_id(right_pair));
            if !state.pairs.insert(key) {
                return true;
            }

            let (left_car, left_cdr) = pair_parts(left_pair);
            let (right_car, right_cdr) = pair_parts(right_pair);
            equal_values_inner(&left_car, &right_car, state)
                && equal_values_inner(&left_cdr, &right_cdr, state)
        }
        (Value::Vector(left), Value::Vector(right)) => {
            let key = (vector_id(left), vector_id(right));
            if !state.vectors.insert(key) {
                return true;
            }

            let left_items = left.to_values();
            let right_items = right.to_values();

            left_items.len() == right_items.len()
                && left_items
                    .iter()
                    .zip(right_items.iter())
                    .all(|(left, right)| equal_values_inner(left, right, state))
        }
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Values(left), Value::Values(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| equal_values_inner(left, right, state))
        }
        (Value::Uninitialized, Value::Uninitialized) => true,
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn eqv_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left.numeric_eq(*right),
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => Rc::ptr_eq(&left.chars, &right.chars),
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(&left.items, &right.items),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Values(left), Value::Values(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| eqv_values(left, right))
        }
        (Value::Uninitialized, Value::Uninitialized) => true,
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn list_length(value: &Value) -> Result<usize, EvalError> {
    let mut count = 0;
    let mut cursor = value.clone();
    let mut seen = HashSet::new();

    loop {
        match cursor {
            Value::EmptyList => return Ok(count),
            Value::Pair(pair) => {
                if !seen.insert(pair_id(&pair)) {
                    return Err(EvalError::CircularList {
                        name: "length".to_string(),
                    });
                }

                count += 1;
                cursor = pair_cdr(&pair);
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
    let values = expect_list_values(list, "append")?;
    let mut result = tail;

    for value in values.into_iter().rev() {
        result = make_pair(value, result);
    }

    Ok(result)
}

fn list_from_values(values: Vec<Value>) -> Value {
    values
        .into_iter()
        .rev()
        .fold(Value::EmptyList, |tail, value| make_pair(value, tail))
}

fn pack_values(values: Vec<Value>) -> Value {
    match values.as_slice() {
        [value] => value.clone(),
        _ => Value::Values(values),
    }
}

fn unpack_values(value: Value) -> Vec<Value> {
    match value {
        Value::Values(values) => values,
        value => vec![value],
    }
}

fn expect_list_values(value: &Value, name: &str) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::new();
    let mut cursor = value.clone();
    let mut seen = HashSet::new();

    loop {
        match cursor {
            Value::EmptyList => return Ok(values),
            Value::Pair(pair) => {
                if !seen.insert(pair_id(&pair)) {
                    return Err(EvalError::CircularList {
                        name: name.to_string(),
                    });
                }

                let (car, cdr) = pair_parts(&pair);
                values.push(car);
                cursor = cdr;
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

fn expect_vector(value: &Value) -> Result<&SchemeVector, EvalError> {
    match value {
        Value::Vector(vector) => Ok(vector),
        other => Err(EvalError::TypeMismatch {
            expected: "vector",
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

fn parse_guard_spec(expr: &Expr) -> Result<(String, Vec<Expr>), EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::SyntaxError {
            message: "guard requires a handler variable and clause list".to_string(),
        });
    };

    let Some((variable, clauses)) = items.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "guard requires a handler variable".to_string(),
        });
    };

    let Expr::Symbol(variable) = variable else {
        return Err(EvalError::SyntaxError {
            message: "guard handler variable must be a symbol".to_string(),
        });
    };

    Ok((variable.clone(), clauses.to_vec()))
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

fn signal_exception(value: Value, ctx: &mut EvalContext) -> Result<MachineState, EvalError> {
    ctx.pending_exception = Some(value);
    Err(EvalError::ExceptionRaisedSignal)
}

fn take_pending_exception(ctx: &mut EvalContext) -> Result<Value, EvalError> {
    ctx.pending_exception
        .take()
        .ok_or(EvalError::InvariantViolation {
            message: "exception signal without a pending exception value".to_string(),
        })
}

fn is_special_form_name(name: &str) -> bool {
    matches!(
        name,
        "and"
            | "or"
            | "if"
            | "begin"
            | "cond"
            | "case"
            | "quote"
            | "define"
            | "define-record-type"
            | "define-syntax"
            | "set!"
            | "lambda"
            | "case-lambda"
            | "let"
            | "let*"
            | "letrec"
            | "letrec*"
            | "do"
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
