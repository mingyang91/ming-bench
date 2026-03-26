use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

mod builtins;
mod continuation;
pub mod error;
mod number;
mod parser;
mod special_forms;
mod syntax;

use builtins::default_env;
use continuation::{eval_program_with_continuations, program_uses_first_class_continuations};
pub use error::EvalError;
use error::SourcePos;
use number::Number;
use parser::parse_program;
use special_forms::{eval_special_form, eval_special_form_tail};
use syntax::{expand_macro_call, MacroRef};

#[derive(Clone)]
enum ExprKind {
    Number(Number),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
    CapturedSymbol(String, EnvRef),
    List(Vec<Expr>),
    Vector(Vec<Expr>),
}

#[derive(Clone)]
struct Expr {
    kind: ExprKind,
    pos: SourcePos,
}

impl Expr {
    fn new(kind: ExprKind, pos: SourcePos) -> Self {
        Self { kind, pos }
    }
}

type NativeFunc = fn(&[Value], &EvalContext) -> Result<Value, EvalError>;
type EnvRef = Rc<Env>;
type PairRef = Rc<RefCell<PairCell>>;
type RecordRef = Rc<RecordInstance>;
type RecordTypeRef = Rc<RecordType>;
type RecordProcRef = Rc<RecordProcedure>;
type StringRef = Rc<StringCell>;
type ContinuationRef = Rc<Continuation>;
type DynamicWindRef = Rc<DynamicWindContext>;
type VectorRef = Rc<RefCell<Vec<Value>>>;

struct StringCell {
    chars: RefCell<Vec<char>>,
    mutable: bool,
}

struct DynamicWindContext {
    in_thunk: Value,
    body_thunk: Value,
    out_thunk: Value,
}

struct RecordType {
    name: String,
    field_count: usize,
}

struct RecordInstance {
    record_type: RecordTypeRef,
    fields: Vec<Value>,
}

struct RecordProcedure {
    name: String,
    record_type: RecordTypeRef,
    kind: RecordProcedureKind,
}

#[derive(Clone, Copy)]
enum RecordProcedureKind {
    Constructor,
    Predicate,
    Accessor(usize),
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum ControlProc {
    CallCc,
    DynamicWind,
    CallWithValues,
    Raise,
    WithExceptionHandler,
}

impl ControlProc {
    fn name(self) -> &'static str {
        match self {
            Self::CallCc => "call/cc",
            Self::DynamicWind => "dynamic-wind",
            Self::CallWithValues => "call-with-values",
            Self::Raise => "raise",
            Self::WithExceptionHandler => "with-exception-handler",
        }
    }
}

struct EvalContext {
    output: RefCell<String>,
    gensym_counter: RefCell<usize>,
    exception_counter: RefCell<usize>,
    exceptions: RefCell<HashMap<usize, Value>>,
}

impl EvalContext {
    fn new() -> Self {
        Self {
            output: RefCell::new(String::new()),
            gensym_counter: RefCell::new(0),
            exception_counter: RefCell::new(0),
            exceptions: RefCell::new(HashMap::new()),
        }
    }

    fn push_output(&self, value: &str) {
        self.output.borrow_mut().push_str(value);
    }

    fn into_output(self) -> String {
        self.output.into_inner()
    }

    fn fresh_name(&self, base: &str) -> String {
        let mut counter = self.gensym_counter.borrow_mut();
        let name = format!("__ming_macro_{}_{}", base, *counter);
        *counter += 1;
        name
    }

    fn raise(&self, value: Value) -> EvalError {
        let rendered = value.render();
        let mut counter = self.exception_counter.borrow_mut();
        let id = *counter;
        *counter += 1;
        self.exceptions.borrow_mut().insert(id, value);
        EvalError::Raised {
            id,
            value: rendered,
        }
    }

    fn take_exception(&self, id: usize) -> Option<Value> {
        self.exceptions.borrow_mut().remove(&id)
    }

    fn restore_exception(&self, id: usize, value: Value) {
        self.exceptions.borrow_mut().insert(id, value);
    }
}

#[derive(Clone)]
enum Value {
    Number(Number),
    Boolean(bool),
    String(StringRef),
    Vector(VectorRef),
    Char(char),
    Symbol(String),
    Nil,
    Pair(PairRef),
    Record(RecordRef),
    ControlProc(ControlProc),
    NativeProc {
        name: &'static str,
        func: NativeFunc,
    },
    Syntax(Box<Expr>),
    SyntaxList(Vec<Expr>),
    RecordProc(RecordProcRef),
    Closure(Rc<Closure>),
    Continuation(ContinuationRef),
    Multiple(Vec<Value>),
    Void,
}

struct PairCell {
    car: Value,
    cdr: Value,
}

struct Closure {
    clauses: Vec<ClosureClause>,
    env: EnvRef,
}

struct ClosureClause {
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Expr>,
}

struct CallRequest {
    procedure: Value,
    args: Vec<Value>,
    pos: Option<SourcePos>,
}

enum TailOutcome {
    Value(Value),
    Apply(CallRequest),
}

#[derive(Clone)]
struct Continuation {
    frames: Vec<ContinuationFrame>,
}

#[derive(Clone)]
enum ContinuationFrame {
    Sequence {
        rest: Vec<Expr>,
        env: EnvRef,
    },
    If {
        consequent: Expr,
        alternate: Option<Expr>,
        env: EnvRef,
    },
    DefineValue {
        name: String,
        env: EnvRef,
    },
    DefineSyntaxValue {
        name: String,
        env: EnvRef,
    },
    SetValue {
        name: String,
        target_env: EnvRef,
        pos: SourcePos,
    },
    ApplicationOperator {
        args: Vec<Expr>,
        env: EnvRef,
        pos: SourcePos,
    },
    ApplicationArgument {
        procedure: Value,
        evaluated: Vec<Value>,
        remaining: Vec<Expr>,
        env: EnvRef,
        pos: SourcePos,
    },
    CondTest {
        body: Vec<Expr>,
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    LetBinding {
        current_name: String,
        evaluated: Vec<(String, Value)>,
        pending: Vec<(String, Expr)>,
        body: Vec<Expr>,
        env: EnvRef,
    },
    And {
        rest: Vec<Expr>,
        env: EnvRef,
    },
    Or {
        rest: Vec<Expr>,
        env: EnvRef,
    },
    ExceptionHandler {
        handler: Value,
        pos: Option<SourcePos>,
    },
    DynamicWindStart {
        context: DynamicWindRef,
    },
    DynamicWindMarker {
        context: DynamicWindRef,
    },
    DynamicWindBody {
        context: DynamicWindRef,
    },
    DynamicWindCleanup {
        context: DynamicWindRef,
        result: Value,
    },
    CallWithValues {
        consumer: Value,
        pos: Option<SourcePos>,
    },
    DynamicWindTransition {
        remaining: Vec<Value>,
        final_value: Value,
        target_frames: Vec<ContinuationFrame>,
    },
    TransitionApply {
        remaining: Vec<Value>,
        procedure: Value,
        args: Vec<Value>,
        pos: Option<SourcePos>,
        target_frames: Vec<ContinuationFrame>,
    },
    TransitionRaise {
        remaining: Vec<Value>,
        id: usize,
        value: String,
    },
}

struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    syntax_bindings: RefCell<HashMap<String, MacroRef>>,
    parent: Option<EnvRef>,
}

fn make_string(value: impl AsRef<str>) -> Value {
    make_string_with_mutability(value, false)
}

fn make_mutable_string(value: impl AsRef<str>) -> Value {
    make_string_with_mutability(value, true)
}

fn make_string_with_mutability(value: impl AsRef<str>, mutable: bool) -> Value {
    Value::String(Rc::new(StringCell {
        chars: RefCell::new(value.as_ref().chars().collect()),
        mutable,
    }))
}

fn render_string(value: &StringRef) -> String {
    value.chars.borrow().iter().collect()
}

impl Value {
    fn render(&self) -> String {
        match self {
            Self::Number(value) => value.render(),
            Self::Boolean(true) => "#t".to_string(),
            Self::Boolean(false) => "#f".to_string(),
            Self::String(value) => {
                let escaped = render_string(value)
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\n")
                    .replace('\t', "\\t");
                format!("\"{escaped}\"")
            }
            Self::Vector(value) => render_vector(value),
            Self::Char(value) => render_char(*value),
            Self::Symbol(value) => value.clone(),
            Self::Nil => "()".to_string(),
            Self::Pair(pair) => render_pair(pair.clone()),
            Self::Record(record) => format!("#<record:{}>", record.record_type.name),
            Self::ControlProc(proc) => format!("#<procedure:{}>", proc.name()),
            Self::NativeProc { name, .. } => format!("#<procedure:{name}>"),
            Self::Syntax(_) => "#<syntax>".to_string(),
            Self::SyntaxList(items) => format!("#<syntax-list:{}>", items.len()),
            Self::RecordProc(procedure) => format!("#<procedure:{}>", procedure.name),
            Self::Closure(_) => "#<procedure>".to_string(),
            Self::Continuation(_) => "#<procedure>".to_string(),
            Self::Multiple(values) => format!("#<values:{}>", values.len()),
            Self::Void => "#<void>".to_string(),
        }
    }

    fn display_render(&self) -> String {
        match self {
            Self::String(value) => render_string(value),
            Self::Char(value) => value.to_string(),
            _ => self.render(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn as_number(&self, name: &'static str) -> Result<Number, EvalError> {
        match self {
            Self::Number(value) => Ok(*value),
            other => Err(EvalError::ExpectedNumber {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_integer(&self, name: &'static str) -> Result<i64, EvalError> {
        match self {
            Self::Number(value) => {
                value
                    .exact_integer()
                    .ok_or_else(|| EvalError::ExpectedInteger {
                        name,
                        found: value.render(),
                    })
            }
            other => Err(EvalError::ExpectedInteger {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_string(&self, name: &'static str) -> Result<String, EvalError> {
        match self {
            Self::String(value) => Ok(render_string(value)),
            other => Err(EvalError::ExpectedString {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_string_ref(&self, name: &'static str) -> Result<StringRef, EvalError> {
        match self {
            Self::String(value) => Ok(value.clone()),
            other => Err(EvalError::ExpectedString {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_char(&self, name: &'static str) -> Result<char, EvalError> {
        match self {
            Self::Char(value) => Ok(*value),
            other => Err(EvalError::ExpectedChar {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_symbol<'a>(&'a self, name: &'static str) -> Result<&'a str, EvalError> {
        match self {
            Self::Symbol(value) => Ok(value),
            other => Err(EvalError::ExpectedSymbol {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_pair(&self, name: &'static str) -> Result<PairRef, EvalError> {
        match self {
            Self::Pair(pair) => Ok(pair.clone()),
            other => Err(EvalError::ExpectedPair {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_vector(&self, name: &'static str) -> Result<VectorRef, EvalError> {
        match self {
            Self::Vector(vector) => Ok(vector.clone()),
            other => Err(EvalError::ExpectedVector {
                name,
                found: other.render(),
            }),
        }
    }
}

impl Env {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            syntax_bindings: RefCell::new(HashMap::new()),
            parent,
        })
    }

    fn define(&self, name: String, value: Value) {
        self.bindings.borrow_mut().insert(name, value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return Some(value);
        }

        self.parent.as_ref().and_then(|parent| parent.lookup(name))
    }

    fn define_syntax(&self, name: String, value: MacroRef) {
        self.syntax_bindings.borrow_mut().insert(name, value);
    }

    fn lookup_syntax(&self, name: &str) -> Option<MacroRef> {
        if let Some(value) = self.syntax_bindings.borrow().get(name).cloned() {
            return Some(value);
        }

        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup_syntax(name))
    }

    fn set(&self, name: &str, value: Value) -> bool {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), value);
            true
        } else {
            self.parent
                .as_ref()
                .is_some_and(|parent| parent.set(name, value))
        }
    }
}

impl Closure {
    fn new_single(
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: EnvRef,
    ) -> Self {
        Self {
            clauses: vec![ClosureClause {
                params,
                rest_param,
                body,
            }],
            env,
        }
    }

    fn matching_clause(&self, len: usize) -> Option<&ClosureClause> {
        self.clauses.iter().find(|clause| clause.matches_arity(len))
    }

    fn wrong_arg_count(&self, got: usize) -> EvalError {
        if self.clauses.len() == 1 {
            EvalError::WrongArgCount {
                name: "lambda",
                expected: "the declared arity",
                got,
            }
        } else {
            let expected = self
                .clauses
                .iter()
                .map(ClosureClause::arity_description)
                .collect::<Vec<_>>()
                .join(" or ");
            EvalError::WrongArgCountDynamic {
                name: "case-lambda".to_string(),
                expected,
                got,
            }
        }
    }
}

impl ClosureClause {
    fn matches_arity(&self, len: usize) -> bool {
        len >= self.params.len() && (self.rest_param.is_some() || len == self.params.len())
    }

    fn bind_frame(&self, args: &[Value], env: EnvRef) -> EnvRef {
        let frame = Env::new(Some(env));
        for (name, value) in self.params.iter().zip(args.iter()) {
            frame.define(name.clone(), value.clone());
        }

        if let Some(name) = &self.rest_param {
            frame.define(
                name.clone(),
                list_from_values(args[self.params.len()..].to_vec()),
            );
        }

        frame
    }

    fn arity_description(&self) -> String {
        if self.rest_param.is_some() {
            format!("at least {}", self.params.len())
        } else {
            format!("exactly {}", self.params.len())
        }
    }
}

impl RecordProcedure {
    fn call(&self, args: &[Value]) -> Result<Value, EvalError> {
        match self.kind {
            RecordProcedureKind::Constructor => {
                let expected = self.record_type.field_count;
                if args.len() != expected {
                    return Err(self.wrong_arg_count(format!("exactly {expected}"), args.len()));
                }

                Ok(Value::Record(Rc::new(RecordInstance {
                    record_type: self.record_type.clone(),
                    fields: args.to_vec(),
                })))
            }
            RecordProcedureKind::Predicate => {
                if args.len() != 1 {
                    return Err(self.wrong_arg_count("exactly 1", args.len()));
                }

                Ok(Value::Boolean(matches!(
                    &args[0],
                    Value::Record(record) if Rc::ptr_eq(&record.record_type, &self.record_type)
                )))
            }
            RecordProcedureKind::Accessor(index) => {
                if args.len() != 1 {
                    return Err(self.wrong_arg_count("exactly 1", args.len()));
                }

                let record = match &args[0] {
                    Value::Record(record) if Rc::ptr_eq(&record.record_type, &self.record_type) => {
                        record
                    }
                    other => {
                        return Err(EvalError::ExpectedRecordType {
                            name: self.name.clone(),
                            expected: self.record_type.name.clone(),
                            found: other.render(),
                        });
                    }
                };

                record
                    .fields
                    .get(index)
                    .cloned()
                    .ok_or_else(|| EvalError::InvalidSyntax {
                        message: format!("{}: invalid record accessor index {index}", self.name),
                    })
            }
        }
    }

    fn wrong_arg_count(&self, expected: impl Into<String>, got: usize) -> EvalError {
        EvalError::WrongArgCountDynamic {
            name: self.name.clone(),
            expected: expected.into(),
            got,
        }
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
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (result, _) = eval_str_with_output(input)?;
    Ok(result)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_program(input)?;

    if exprs.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let env = default_env();
    let ctx = EvalContext::new();
    if program_uses_first_class_continuations(&exprs) {
        let last = eval_program_with_continuations(&exprs, env, &ctx)?;
        return Ok((last.render(), ctx.into_output()));
    }

    let last = eval_sequence(&exprs, env, &ctx)?;
    Ok((last.render(), ctx.into_output()))
}

fn eval_sequence(exprs: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expr in exprs {
        last = eval(expr, env.clone(), ctx)?;
    }
    Ok(last)
}

fn eval(expr: &Expr, env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::String(value) => Ok(make_string(value)),
        ExprKind::Symbol(name) => lookup_symbol_value(name, &env, expr.pos),
        ExprKind::CapturedSymbol(name, captured_env) => {
            lookup_symbol_value(name, captured_env, expr.pos)
        }
        ExprKind::List(items) => eval_list(expr.pos, items, env, ctx),
        ExprKind::Vector(_) => quote_expr_value(expr),
    }
}

fn eval_list(
    pos: SourcePos,
    items: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    let (head, tail) = items.split_first().ok_or_else(|| {
        EvalError::NotAProcedure {
            found: "()".to_string(),
        }
        .with_position(pos)
    })?;

    if let Some(name) = expr_symbol_name(head) {
        if let Some(result) = eval_special_form(name, tail, env.clone(), ctx) {
            return result.map_err(|err| err.with_position(head.pos));
        }
    }

    if let Some(syntax) = lookup_syntax(head, &env) {
        let expanded = expand_macro_call(pos, items, syntax, ctx)?;
        return eval(&expanded, env, ctx);
    }

    let procedure = eval(head, env.clone(), ctx)?;
    let args = eval_args(tail, env, ctx)?;
    apply_procedure_at(procedure, args, head.pos, ctx)
}

fn eval_sequence_tco(
    exprs: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<TailOutcome, EvalError> {
    let Some((last, init)) = exprs.split_last() else {
        return Ok(TailOutcome::Value(Value::Void));
    };

    for expr in init {
        eval(expr, env.clone(), ctx)?;
    }

    eval_tail(last, env, ctx)
}

fn eval_tail(expr: &Expr, env: EnvRef, ctx: &EvalContext) -> Result<TailOutcome, EvalError> {
    match &expr.kind {
        ExprKind::List(items) => eval_list_tail(expr.pos, items, env, ctx),
        _ => eval(expr, env, ctx).map(TailOutcome::Value),
    }
}

fn eval_list_tail(
    pos: SourcePos,
    items: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<TailOutcome, EvalError> {
    let (head, tail) = items.split_first().ok_or_else(|| {
        EvalError::NotAProcedure {
            found: "()".to_string(),
        }
        .with_position(pos)
    })?;

    if let Some(name) = expr_symbol_name(head) {
        if let Some(result) = eval_special_form_tail(name, tail, env.clone(), ctx) {
            return result.map_err(|err| err.with_position(head.pos));
        }
    }

    if let Some(syntax) = lookup_syntax(head, &env) {
        let expanded = expand_macro_call(pos, items, syntax, ctx)?;
        return eval_tail(&expanded, env, ctx);
    }

    let procedure = eval(head, env.clone(), ctx)?;
    let args = eval_args(tail, env, ctx)?;
    Ok(TailOutcome::Apply(CallRequest {
        procedure,
        args,
        pos: Some(head.pos),
    }))
}

fn eval_args(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::with_capacity(args.len());
    for expr in args {
        values.push(eval(expr, env.clone(), ctx)?);
    }
    Ok(values)
}

fn attach_call_position(error: EvalError, pos: Option<SourcePos>) -> EvalError {
    match pos {
        Some(pos) => error.with_position(pos),
        None => error,
    }
}

fn apply_procedure(
    procedure: Value,
    args: &[Value],
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    apply_call(
        CallRequest {
            procedure,
            args: args.to_vec(),
            pos: None,
        },
        ctx,
    )
}

fn apply_procedure_at(
    procedure: Value,
    args: Vec<Value>,
    pos: SourcePos,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    apply_call(
        CallRequest {
            procedure,
            args,
            pos: Some(pos),
        },
        ctx,
    )
}

fn apply_call(mut call: CallRequest, ctx: &EvalContext) -> Result<Value, EvalError> {
    loop {
        let CallRequest {
            procedure,
            args,
            pos,
        } = call;

        match procedure {
            Value::ControlProc(ControlProc::CallWithValues) => {
                if args.len() != 2 {
                    return Err(attach_call_position(
                        EvalError::WrongArgCount {
                            name: "call-with-values",
                            expected: "exactly 2",
                            got: args.len(),
                        },
                        pos,
                    ));
                }

                let produced = apply_call(
                    CallRequest {
                        procedure: args[0].clone(),
                        args: Vec::new(),
                        pos,
                    },
                    ctx,
                )?;

                call = CallRequest {
                    procedure: args[1].clone(),
                    args: unpack_values(produced),
                    pos,
                };
            }
            Value::ControlProc(ControlProc::DynamicWind) => {
                if args.len() != 3 {
                    return Err(attach_call_position(
                        EvalError::WrongArgCount {
                            name: "dynamic-wind",
                            expected: "exactly 3",
                            got: args.len(),
                        },
                        pos,
                    ));
                }

                apply_call(
                    CallRequest {
                        procedure: args[0].clone(),
                        args: Vec::new(),
                        pos,
                    },
                    ctx,
                )?;
                let result = apply_call(
                    CallRequest {
                        procedure: args[1].clone(),
                        args: Vec::new(),
                        pos,
                    },
                    ctx,
                );

                match result {
                    Ok(result) => {
                        apply_call(
                            CallRequest {
                                procedure: args[2].clone(),
                                args: Vec::new(),
                                pos,
                            },
                            ctx,
                        )?;
                        return Ok(result);
                    }
                    Err(body_err) => {
                        apply_call(
                            CallRequest {
                                procedure: args[2].clone(),
                                args: Vec::new(),
                                pos,
                            },
                            ctx,
                        )?;
                        return Err(body_err);
                    }
                }
            }
            Value::ControlProc(ControlProc::Raise) => {
                if args.len() != 1 {
                    return Err(attach_call_position(
                        EvalError::WrongArgCount {
                            name: "raise",
                            expected: "exactly 1",
                            got: args.len(),
                        },
                        pos,
                    ));
                }

                return Err(ctx.raise(args[0].clone()));
            }
            Value::ControlProc(ControlProc::WithExceptionHandler) => {
                if args.len() != 2 {
                    return Err(attach_call_position(
                        EvalError::WrongArgCount {
                            name: "with-exception-handler",
                            expected: "exactly 2",
                            got: args.len(),
                        },
                        pos,
                    ));
                }

                let handler = args[0].clone();
                let thunk = args[1].clone();
                match apply_call(
                    CallRequest {
                        procedure: thunk,
                        args: Vec::new(),
                        pos,
                    },
                    ctx,
                ) {
                    Ok(value) => return Ok(value),
                    Err(EvalError::Raised { id, .. }) => {
                        let Some(exception) = ctx.take_exception(id) else {
                            return Err(EvalError::InvalidSyntax {
                                message: "internal missing exception payload".to_string(),
                            });
                        };

                        return apply_call(
                            CallRequest {
                                procedure: handler,
                                args: vec![exception],
                                pos,
                            },
                            ctx,
                        );
                    }
                    Err(err) => return Err(err),
                }
            }
            Value::ControlProc(ControlProc::CallCc) | Value::Continuation(_) => {
                return Err(attach_call_position(
                    EvalError::InvalidSyntax {
                        message: "continuations require the continuation-aware evaluator"
                            .to_string(),
                    },
                    pos,
                ));
            }
            Value::NativeProc { func, .. } => {
                return func(&args, ctx).map_err(|err| attach_call_position(err, pos));
            }
            Value::RecordProc(procedure) => {
                return procedure
                    .call(&args)
                    .map_err(|err| attach_call_position(err, pos));
            }
            Value::Closure(closure) => {
                let Some(clause) = closure.matching_clause(args.len()) else {
                    return Err(attach_call_position(
                        closure.wrong_arg_count(args.len()),
                        pos,
                    ));
                };

                let frame = clause.bind_frame(&args, closure.env.clone());
                match eval_sequence_tco(&clause.body, frame, ctx)? {
                    TailOutcome::Value(value) => return Ok(value),
                    TailOutcome::Apply(next_call) => call = next_call,
                }
            }
            other => {
                return Err(attach_call_position(
                    EvalError::NotAProcedure {
                        found: other.render(),
                    },
                    pos,
                ));
            }
        }
    }
}

fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new(PairCell { car, cdr })))
}

fn make_vector(values: Vec<Value>) -> Value {
    Value::Vector(Rc::new(RefCell::new(values)))
}

fn list_from_values(values: Vec<Value>) -> Value {
    values
        .into_iter()
        .rev()
        .fold(Value::Nil, |tail, head| make_pair(head, tail))
}

fn pack_values(mut values: Vec<Value>) -> Value {
    if values.len() == 1 {
        values.pop().expect("checked length above")
    } else {
        Value::Multiple(values)
    }
}

fn unpack_values(value: Value) -> Vec<Value> {
    match value {
        Value::Multiple(values) => values,
        other => vec![other],
    }
}

fn list_to_vec(value: &Value, name: &'static str) -> Result<Vec<Value>, EvalError> {
    let mut items = Vec::new();
    let mut cursor = value.clone();

    loop {
        match cursor {
            Value::Nil => return Ok(items),
            Value::Pair(pair) => {
                let (car, cdr) = {
                    let borrowed = pair.borrow();
                    (borrowed.car.clone(), borrowed.cdr.clone())
                };
                items.push(car);
                cursor = cdr;
            }
            other => {
                return Err(EvalError::ExpectedList {
                    name,
                    found: other.render(),
                });
            }
        }
    }
}

fn render_pair(pair: PairRef) -> String {
    let mut rendered = Vec::new();
    let mut cursor = Value::Pair(pair);

    loop {
        match cursor {
            Value::Pair(pair) => {
                let (car, cdr) = {
                    let borrowed = pair.borrow();
                    (borrowed.car.clone(), borrowed.cdr.clone())
                };
                rendered.push(car.render());
                cursor = cdr;
            }
            Value::Nil => return format!("({})", rendered.join(" ")),
            other => {
                let prefix = rendered.join(" ");
                return format!("({prefix} . {})", other.render());
            }
        }
    }
}

fn render_vector(vector: &VectorRef) -> String {
    let items = vector.borrow();
    let rendered = items
        .iter()
        .map(Value::render)
        .collect::<Vec<_>>()
        .join(" ");
    format!("#({rendered})")
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        _ => format!("#\\{value}"),
    }
}

fn expr_symbol_name(expr: &Expr) -> Option<&str> {
    match &expr.kind {
        ExprKind::Symbol(name) | ExprKind::CapturedSymbol(name, _) => Some(name.as_str()),
        _ => None,
    }
}

fn expr_plain_symbol_name(expr: &Expr) -> Option<&str> {
    match &expr.kind {
        ExprKind::Symbol(name) => Some(name.as_str()),
        _ => None,
    }
}

fn is_ellipsis_expr(expr: &Expr) -> bool {
    expr_symbol_name(expr).is_some_and(|name| name == "...")
}

fn quote_expr_value(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::String(value) => Ok(make_string(value)),
        ExprKind::Symbol(value) | ExprKind::CapturedSymbol(value, _) => {
            Ok(Value::Symbol(value.clone()))
        }
        ExprKind::List(items) => quote_list_items(items),
        ExprKind::Vector(items) => items
            .iter()
            .map(quote_expr_value)
            .collect::<Result<Vec<_>, _>>()
            .map(make_vector),
    }
}

fn quote_list_items(items: &[Expr]) -> Result<Value, EvalError> {
    let Some(dot_index) = dotted_tail_index(items) else {
        return items
            .iter()
            .map(quote_expr_value)
            .collect::<Result<Vec<_>, _>>()
            .map(list_from_values);
    };

    let tail = quote_expr_value(&items[dot_index + 1])?;
    items[..dot_index]
        .iter()
        .rev()
        .try_fold(tail, |cdr, item| quote_expr_value(item).map(|car| make_pair(car, cdr)))
}

fn dotted_tail_index(items: &[Expr]) -> Option<usize> {
    let mut matches = items
        .iter()
        .enumerate()
        .filter_map(|(index, item)| (expr_symbol_name(item) == Some(".")).then_some(index));
    let dot_index = matches.next()?;
    if matches.next().is_some() || dot_index == 0 || dot_index + 2 != items.len() {
        return None;
    }
    Some(dot_index)
}

fn lookup_symbol_value(name: &str, env: &EnvRef, pos: SourcePos) -> Result<Value, EvalError> {
    env.lookup(name).ok_or_else(|| {
        EvalError::UnboundVariable {
            name: name.to_string(),
        }
        .with_position(pos)
    })
}

fn lookup_syntax(expr: &Expr, env: &EnvRef) -> Option<MacroRef> {
    match &expr.kind {
        ExprKind::Symbol(name) => env.lookup_syntax(name),
        ExprKind::CapturedSymbol(name, captured_env) => captured_env.lookup_syntax(name),
        _ => None,
    }
}

#[cfg(test)]
mod tests;
