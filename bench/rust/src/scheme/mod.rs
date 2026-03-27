mod builtins;
mod continuation;
pub mod error;
mod macros;
mod number;
mod parser;
mod record;
mod render;
mod special_forms;
mod value_ops;

use continuation::{
    final_continuation, pop_wind_frame, push_wind_frame, reset_wind_stack,
    resume_continuation_jump, sequence_continuation, CapturedContinuation, Continuation,
    ContinuationRef, EvalResult, EvalSignal, RaisedException,
};
pub use error::{EvalError, SourcePos};
use macros::{MacroEnvRef, MacroEnvironment};
use number::Number;
use parser::Parser;
use record::{define_record_type as eval_define_record_type, NativeProcedure, RecordRef};
use render::{render_display_value, render_value};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
enum Expr {
    Number(Number, SourcePos),
    Boolean(bool, SourcePos),
    String(String, SourcePos),
    Char(char, SourcePos),
    Symbol(String, SourcePos),
    CapturedSymbol(String, BindingRef, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    fn position(&self) -> SourcePos {
        match self {
            Self::Number(_, position)
            | Self::Boolean(_, position)
            | Self::String(_, position)
            | Self::Char(_, position)
            | Self::Symbol(_, position)
            | Self::CapturedSymbol(_, _, position)
            | Self::List(_, position) => *position,
        }
    }
}

type EnvRef = Rc<RefCell<Environment>>;
type OutputRef = Rc<RefCell<String>>;
type BindingRef = Rc<RefCell<Value>>;
type PairRef = Rc<RefCell<PairCell>>;

#[derive(Clone)]
enum Value {
    Number(Number),
    Boolean(bool),
    String(String),
    MutableString(Rc<RefCell<Vec<char>>>),
    Symbol(String),
    Char(char),
    Pair(PairRef),
    List(Vec<Value>),
    Vector(Rc<RefCell<Vec<Value>>>),
    Procedure(Rc<Procedure>),
    NativeProcedure(Rc<NativeProcedure>),
    Builtin(Builtin),
    Continuation(Rc<CapturedContinuation>),
    Values(Vec<Value>),
    Record(RecordRef),
    Uninitialized,
    Void,
}

struct PairCell {
    car: Value,
    cdr: Value,
}

struct Procedure {
    clauses: Vec<ProcedureClause>,
    env: EnvRef,
    macro_env: MacroEnvRef,
}

struct ProcedureClause {
    params: Vec<String>,
    rest_param: Option<String>,
    body: Rc<Expr>,
}

#[derive(Clone)]
struct Builtin {
    kind: BuiltinKind,
    output: OutputRef,
}

#[derive(Clone, Copy)]
enum BuiltinKind {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    Equal,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Not,
    Cons,
    Car,
    Cdr,
    ComposedCarCdr(&'static str),
    SetCar,
    SetCdr,
    NullPred,
    List,
    Length,
    Append,
    Reverse,
    Apply,
    Values,
    CallWithValues,
    CallCc,
    Raise,
    WithExceptionHandler,
    EqPred,
    EqvPred,
    EqualPred,
    Map,
    ForEach,
    StringPred,
    NumberPred,
    IntegerPred,
    RationalPred,
    ExactPred,
    InexactPred,
    BooleanPred,
    PairPred,
    SymbolPred,
    ProcedurePred,
    Display,
    Write,
    Newline,
    MakeString,
    String,
    StringAppend,
    StringLength,
    Substring,
    StringToNumber,
    NumberToString,
    ExactToInexact,
    InexactToExact,
    Numerator,
    Denominator,
    SymbolToString,
    StringToSymbol,
    StringRef,
    StringCopy,
    StringSet,
    StringToList,
    ListToString,
    CharPred,
    CharToInteger,
    IntegerToChar,
    Abs,
    Modulo,
    Remainder,
    Quotient,
    Gcd,
    Lcm,
    Min,
    Max,
    Expt,
    Truncate,
    Round,
    ZeroPred,
    PositivePred,
    NegativePred,
    OddPred,
    EvenPred,
    ListRef,
    ListTail,
    ListPred,
    Member,
    Assoc,
    Assv,
    CharAlphabeticPred,
    CharNumericPred,
    CharUpcase,
    CharDowncase,
    CharEqual,
    CharLessThan,
    StringEqual,
    StringLessThan,
    StringGreaterThan,
    StringLessThanOrEqual,
    StringGreaterThanOrEqual,
    StringCiEqual,
    StringUpcase,
    StringDowncase,
    Vector,
    MakeVector,
    VectorRef,
    VectorSet,
    VectorLength,
    VectorPred,
    VectorToList,
    ListToVector,
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: HashMap<String, BindingRef>,
}

#[derive(Clone)]
struct OwnedExprRef {
    root: Rc<Expr>,
    path: Vec<usize>,
}

enum EvalTarget<'a> {
    BorrowedExpr(&'a Expr),
    BorrowedSequence(&'a [Expr]),
    OwnedExpr(OwnedExprRef),
}

enum EvalStep<'a> {
    Value(Value),
    Tail {
        target: EvalTarget<'a>,
        env: EnvRef,
        macro_env: MacroEnvRef,
    },
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Number(number) if number.as_exact_integer().is_some() => "integer",
            Self::Number(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) | Self::MutableString(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::Char(_) => "character",
            Self::Pair(_) => "pair",
            Self::List(_) => "list",
            Self::Vector(_) => "vector",
            Self::Procedure(_)
            | Self::NativeProcedure(_)
            | Self::Builtin(_)
            | Self::Continuation(_) => "procedure",
            Self::Values(_) => "values",
            Self::Record(_) => "record",
            Self::Uninitialized => "uninitialized",
            Self::Void => "void",
        }
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Self::Number(number) => number.as_exact_integer().ok_or(EvalError::TypeMismatch {
                expected: "integer",
                found: self.type_name().into(),
            }),
            other => Err(EvalError::TypeMismatch {
                expected: "integer",
                found: other.type_name().into(),
            }),
        }
    }

    fn as_number(&self) -> Result<Number, EvalError> {
        match self {
            Self::Number(number) => Ok(*number),
            other => Err(EvalError::TypeMismatch {
                expected: "number",
                found: other.type_name().into(),
            }),
        }
    }

    fn as_string(&self) -> Result<String, EvalError> {
        match self {
            Self::String(value) => Ok(value.clone()),
            Self::MutableString(value) => Ok(value.borrow().iter().collect()),
            other => Err(EvalError::TypeMismatch {
                expected: "string",
                found: other.type_name().into(),
            }),
        }
    }

    fn as_symbol(&self) -> Result<&str, EvalError> {
        match self {
            Self::Symbol(value) => Ok(value),
            other => Err(EvalError::TypeMismatch {
                expected: "symbol",
                found: other.type_name().into(),
            }),
        }
    }

    fn as_char(&self) -> Result<char, EvalError> {
        match self {
            Self::Char(value) => Ok(*value),
            other => Err(EvalError::TypeMismatch {
                expected: "character",
                found: other.type_name().into(),
            }),
        }
    }

    fn render(&self) -> String {
        render_value(self)
    }

    fn render_display(&self) -> String {
        render_display_value(self)
    }

    fn from_values(mut values: Vec<Value>) -> Self {
        if values.len() == 1 {
            values.pop().expect("single-value vector must contain a value")
        } else {
            Self::Values(values)
        }
    }

    fn into_values(self) -> Vec<Value> {
        match self {
            Self::Values(values) => values,
            other => vec![other],
        }
    }
}

impl Builtin {
    fn new(kind: BuiltinKind, output: OutputRef) -> Self {
        Self { kind, output }
    }

    fn apply(&self, args: &[Value], continuation: &ContinuationRef) -> EvalResult<Value> {
        builtins::apply_builtin(self.kind, args, &self.output, continuation)
    }
}

impl BuiltinKind {
    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "=",
            Self::LessThanOrEqual => "<=",
            Self::GreaterThanOrEqual => ">=",
            Self::Not => "not",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::ComposedCarCdr(name) => name,
            Self::SetCar => "set-car!",
            Self::SetCdr => "set-cdr!",
            Self::NullPred => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::Append => "append",
            Self::Reverse => "reverse",
            Self::Apply => "apply",
            Self::Values => "values",
            Self::CallWithValues => "call-with-values",
            Self::CallCc => "call/cc",
            Self::Raise => "raise",
            Self::WithExceptionHandler => "with-exception-handler",
            Self::EqPred => "eq?",
            Self::EqvPred => "eqv?",
            Self::EqualPred => "equal?",
            Self::Map => "map",
            Self::ForEach => "for-each",
            Self::StringPred => "string?",
            Self::NumberPred => "number?",
            Self::IntegerPred => "integer?",
            Self::RationalPred => "rational?",
            Self::ExactPred => "exact?",
            Self::InexactPred => "inexact?",
            Self::BooleanPred => "boolean?",
            Self::PairPred => "pair?",
            Self::SymbolPred => "symbol?",
            Self::ProcedurePred => "procedure?",
            Self::Display => "display",
            Self::Write => "write",
            Self::Newline => "newline",
            Self::MakeString => "make-string",
            Self::String => "string",
            Self::StringAppend => "string-append",
            Self::StringLength => "string-length",
            Self::Substring => "substring",
            Self::StringToNumber => "string->number",
            Self::NumberToString => "number->string",
            Self::ExactToInexact => "exact->inexact",
            Self::InexactToExact => "inexact->exact",
            Self::Numerator => "numerator",
            Self::Denominator => "denominator",
            Self::SymbolToString => "symbol->string",
            Self::StringToSymbol => "string->symbol",
            Self::StringRef => "string-ref",
            Self::StringCopy => "string-copy",
            Self::StringSet => "string-set!",
            Self::StringToList => "string->list",
            Self::ListToString => "list->string",
            Self::CharPred => "char?",
            Self::CharToInteger => "char->integer",
            Self::IntegerToChar => "integer->char",
            Self::Abs => "abs",
            Self::Modulo => "modulo",
            Self::Remainder => "remainder",
            Self::Quotient => "quotient",
            Self::Gcd => "gcd",
            Self::Lcm => "lcm",
            Self::Min => "min",
            Self::Max => "max",
            Self::Expt => "expt",
            Self::Truncate => "truncate",
            Self::Round => "round",
            Self::ZeroPred => "zero?",
            Self::PositivePred => "positive?",
            Self::NegativePred => "negative?",
            Self::OddPred => "odd?",
            Self::EvenPred => "even?",
            Self::ListRef => "list-ref",
            Self::ListTail => "list-tail",
            Self::ListPred => "list?",
            Self::Member => "member",
            Self::Assoc => "assoc",
            Self::Assv => "assv",
            Self::CharAlphabeticPred => "char-alphabetic?",
            Self::CharNumericPred => "char-numeric?",
            Self::CharUpcase => "char-upcase",
            Self::CharDowncase => "char-downcase",
            Self::CharEqual => "char=?",
            Self::CharLessThan => "char<?",
            Self::StringEqual => "string=?",
            Self::StringLessThan => "string<?",
            Self::StringGreaterThan => "string>?",
            Self::StringLessThanOrEqual => "string<=?",
            Self::StringGreaterThanOrEqual => "string>=?",
            Self::StringCiEqual => "string-ci=?",
            Self::StringUpcase => "string-upcase",
            Self::StringDowncase => "string-downcase",
            Self::Vector => "vector",
            Self::MakeVector => "make-vector",
            Self::VectorRef => "vector-ref",
            Self::VectorSet => "vector-set!",
            Self::VectorLength => "vector-length",
            Self::VectorPred => "vector?",
            Self::VectorToList => "vector->list",
            Self::ListToVector => "list->vector",
        }
    }
}

impl ProcedureClause {
    fn new(params: Vec<String>, rest_param: Option<String>, body: Vec<Expr>) -> Self {
        Self {
            params,
            rest_param,
            body: wrap_procedure_body(body),
        }
    }

    fn matches_arity(&self, arg_count: usize) -> bool {
        arg_count >= self.params.len()
            && (self.rest_param.is_some() || arg_count == self.params.len())
    }

    fn expected_arity(&self) -> String {
        if self.rest_param.is_some() {
            format!("at least {} argument(s)", self.params.len())
        } else {
            format!("exactly {} argument(s)", self.params.len())
        }
    }
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent,
            bindings: HashMap::new(),
        }))
    }
}

impl OwnedExprRef {
    fn new(root: Rc<Expr>) -> Self {
        Self {
            root,
            path: Vec::new(),
        }
    }

    fn current(&self) -> &Expr {
        let mut expr = self.root.as_ref();
        for &index in &self.path {
            let Expr::List(items, _) = expr else {
                unreachable!("owned expression paths must stay within list expressions");
            };
            expr = items
                .get(index)
                .expect("owned expression path index must be valid");
        }
        expr
    }

    fn child(&self, index: usize) -> Self {
        let mut path = self.path.clone();
        path.push(index);
        Self {
            root: Rc::clone(&self.root),
            path,
        }
    }
}

fn run_expr_in_cont(
    expr: &Expr,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: ContinuationRef,
) -> EvalResult<Value> {
    eval_expr(expr, env, macro_env, &continuation)
}

fn run_sequence_in_cont(
    exprs: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: ContinuationRef,
) -> EvalResult<Value> {
    let Some((last, prefix)) = exprs.split_last() else {
        return continue_with(continuation, Value::Void);
    };

    for (index, expr) in prefix.iter().enumerate() {
        run_expr_in_cont(
            expr,
            env,
            macro_env,
            sequence_continuation(&exprs[index + 1..], env, macro_env, &continuation),
        )?;
    }

    let value = eval_expr(last, env, macro_env, &continuation)?;
    continue_with(continuation, value)
}

fn run_callable_in_cont(
    callable: Value,
    args: &[Value],
    head_position: SourcePos,
    continuation: ContinuationRef,
) -> EvalResult<Value> {
    let value = apply_callable(callable, args, &continuation)
        .map_err(|signal| signal.with_position(head_position))?;
    continue_with(continuation, value)
}

fn continue_with(mut continuation: ContinuationRef, mut value: Value) -> EvalResult<Value> {
    loop {
        match continuation.as_ref() {
            Continuation::Final => return Ok(value),
            Continuation::Define { env, name, next } => {
                env_define(env, name.clone(), value);
                value = Value::Void;
                continuation = Rc::clone(next);
            }
            Continuation::SetSymbol { env, name, next } => {
                if !env_set(env, name, value) {
                    return Err(EvalError::UnboundVariable { name: name.clone() }.into());
                }
                value = Value::Void;
                continuation = Rc::clone(next);
            }
            Continuation::SetCaptured { binding, next } => {
                *binding.borrow_mut() = value;
                value = Value::Void;
                continuation = Rc::clone(next);
            }
            Continuation::Sequence {
                remaining,
                env,
                macro_env,
                next,
            } => {
                return run_sequence_in_cont(remaining, env, macro_env, Rc::clone(next));
            }
            Continuation::Application {
                callable,
                pending_args,
                evaluated_suffix,
                env,
                macro_env,
                head_position,
                next,
            } => {
                let mut args = Vec::with_capacity(pending_args.len() + evaluated_suffix.len() + 1);
                args.push(value);
                args.extend(evaluated_suffix.iter().cloned());

                for index in (0..pending_args.len()).rev() {
                    let arg = run_expr_in_cont(
                        &pending_args[index],
                        env,
                        macro_env,
                        Rc::new(Continuation::Application {
                            callable: callable.clone(),
                            pending_args: pending_args[..index].to_vec(),
                            evaluated_suffix: args.clone(),
                            env: Rc::clone(env),
                            macro_env: Rc::clone(macro_env),
                            head_position: *head_position,
                            next: Rc::clone(next),
                        }),
                    )?;
                    args.insert(0, arg);
                }

                return run_callable_in_cont(
                    callable.clone(),
                    &args,
                    *head_position,
                    Rc::clone(next),
                );
            }
            Continuation::CallWithValues { consumer, next } => {
                let consumer_args = value.into_values();
                let result = apply_callable(consumer.clone(), &consumer_args, next)?;
                return continue_with(Rc::clone(next), result);
            }
            Continuation::DynamicWindEnter {
                frame,
                body,
                body_position,
                next,
            } => {
                push_wind_frame(frame);
                let body_result = run_callable_in_cont(
                    body.clone(),
                    &[],
                    *body_position,
                    Rc::new(Continuation::DynamicWind {
                        frame: Rc::clone(frame),
                        next: Rc::clone(next),
                    }),
                );

                return match body_result {
                    Ok(value) => Ok(value),
                    Err(EvalSignal::Raise(exception)) => {
                        pop_wind_frame(frame);
                        apply_callable(frame.after.clone(), &[], next)?;
                        Err(EvalSignal::Raise(exception))
                    }
                    Err(signal) => Err(signal),
                };
            }
            Continuation::DynamicWind { frame, next } => {
                pop_wind_frame(frame);
                return run_callable_in_cont(
                    frame.after.clone(),
                    &[],
                    frame.after_position,
                    Rc::new(Continuation::DynamicWindExit {
                        return_value: value,
                        next: Rc::clone(next),
                    }),
                );
            }
            Continuation::DynamicWindExit { return_value, next } => {
                value = return_value.clone();
                continuation = Rc::clone(next);
            }
            Continuation::WindTransition {
                saved_value,
                remaining_exits,
                remaining_entries,
                target,
            } => {
                let mut remaining_exits = remaining_exits.clone();
                let mut remaining_entries = remaining_entries.clone();

                if let Some(frame) = remaining_exits.pop() {
                    pop_wind_frame(&frame);
                    return run_callable_in_cont(
                        frame.after.clone(),
                        &[],
                        frame.after_position,
                        Rc::new(Continuation::WindTransition {
                            saved_value: saved_value.clone(),
                            remaining_exits,
                            remaining_entries,
                            target: Rc::clone(target),
                        }),
                    );
                }

                if let Some(frame) = remaining_entries.pop() {
                    return run_callable_in_cont(
                        frame.before.clone(),
                        &[],
                        frame.before_position,
                        Rc::new(Continuation::WindTransitionEnter {
                            frame,
                            saved_value: saved_value.clone(),
                            remaining_entries,
                            target: Rc::clone(target),
                        }),
                    );
                }

                value = saved_value.clone();
                continuation = Rc::clone(target);
            }
            Continuation::WindTransitionEnter {
                frame,
                saved_value,
                remaining_entries,
                target,
            } => {
                push_wind_frame(frame);
                value = Value::Void;
                continuation = Rc::new(Continuation::WindTransition {
                    saved_value: saved_value.clone(),
                    remaining_exits: Vec::new(),
                    remaining_entries: remaining_entries.clone(),
                    target: Rc::clone(target),
                });
            }
        }
    }
}

fn eval_application_step<'a>(
    callable: Value,
    arg_exprs: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    head_position: SourcePos,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let mut args = Vec::with_capacity(arg_exprs.len());

    for index in (0..arg_exprs.len()).rev() {
        let arg = run_expr_in_cont(
            &arg_exprs[index],
            env,
            macro_env,
            Rc::new(Continuation::Application {
                callable: callable.clone(),
                pending_args: arg_exprs[..index].to_vec(),
                evaluated_suffix: args.clone(),
                env: Rc::clone(env),
                macro_env: Rc::clone(macro_env),
                head_position,
                next: Rc::clone(continuation),
            }),
        )?;
        args.insert(0, arg);
    }

    special_forms::apply_callable_result(callable, &args, continuation)
        .map_err(|signal| signal.with_position(head_position))
}

fn wrap_procedure_body(body: Vec<Expr>) -> Rc<Expr> {
    match body.len() {
        0 => unreachable!("procedure bodies must not be empty"),
        1 => Rc::new(body.into_iter().next().expect("body length checked")),
        _ => {
            let position = body[0].position();
            let mut items = Vec::with_capacity(body.len() + 1);
            items.push(Expr::Symbol("begin".into(), position));
            items.extend(body);
            Rc::new(Expr::List(items, position))
        }
    }
}

fn make_procedure(clauses: Vec<ProcedureClause>, env: &EnvRef, macro_env: &MacroEnvRef) -> Value {
    Value::Procedure(Rc::new(Procedure {
        clauses,
        env: Rc::clone(env),
        macro_env: Rc::clone(macro_env),
    }))
}

fn single_clause_procedure(
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Expr>,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Value {
    make_procedure(
        vec![ProcedureClause::new(params, rest_param, body)],
        env,
        macro_env,
    )
}

fn apply_callable(
    callable: Value,
    args: &[Value],
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    special_forms::apply_callable(callable, args, continuation)
}

fn parse_required_param_names(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    special_forms::parse_required_param_names(items)
}

fn env_define(env: &EnvRef, name: String, value: Value) {
    env.borrow_mut()
        .bindings
        .insert(name, Rc::new(RefCell::new(value)));
}

fn env_lookup_binding(env: &EnvRef, name: &str) -> Option<BindingRef> {
    let (binding, parent) = {
        let borrowed = env.borrow();
        (
            borrowed.bindings.get(name).cloned(),
            borrowed.parent.clone(),
        )
    };

    binding.or_else(|| parent.and_then(|parent| env_lookup_binding(&parent, name)))
}

fn env_lookup(env: &EnvRef, name: &str) -> Option<Value> {
    env_lookup_binding(env, name).map(|binding| binding.borrow().clone())
}

fn env_set(env: &EnvRef, name: &str, value: Value) -> bool {
    let (binding, parent) = {
        let borrowed = env.borrow();
        (
            borrowed.bindings.get(name).cloned(),
            borrowed.parent.clone(),
        )
    };

    if let Some(binding) = binding {
        *binding.borrow_mut() = value;
        true
    } else if let Some(parent) = parent {
        env_set(&parent, name, value)
    } else {
        false
    }
}

fn tail_borrowed_expr<'a>(expr: &'a Expr, env: &EnvRef, macro_env: &MacroEnvRef) -> EvalStep<'a> {
    EvalStep::Tail {
        target: EvalTarget::BorrowedExpr(expr),
        env: Rc::clone(env),
        macro_env: Rc::clone(macro_env),
    }
}

fn tail_borrowed_sequence<'a>(
    exprs: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> EvalStep<'a> {
    EvalStep::Tail {
        target: EvalTarget::BorrowedSequence(exprs),
        env: Rc::clone(env),
        macro_env: Rc::clone(macro_env),
    }
}

fn tail_owned_expr<'a>(expr: OwnedExprRef, env: &EnvRef, macro_env: &MacroEnvRef) -> EvalStep<'a> {
    EvalStep::Tail {
        target: EvalTarget::OwnedExpr(expr),
        env: Rc::clone(env),
        macro_env: Rc::clone(macro_env),
    }
}

fn eval_target<'a>(
    mut target: EvalTarget<'a>,
    mut env: EnvRef,
    mut macro_env: MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    loop {
        let step = match &target {
            EvalTarget::BorrowedExpr(expr) => eval_expr_step(expr, &env, &macro_env, continuation)?,
            EvalTarget::BorrowedSequence(exprs) => {
                eval_sequence_step(exprs, &env, &macro_env, continuation)?
            }
            EvalTarget::OwnedExpr(expr) => {
                eval_owned_expr_step(expr, &env, &macro_env, continuation)?
            }
        };

        match step {
            EvalStep::Value(value) => return Ok(value),
            EvalStep::Tail {
                target: next_target,
                env: next_env,
                macro_env: next_macro_env,
            } => {
                target = next_target;
                env = next_env;
                macro_env = next_macro_env;
            }
        }
    }
}

fn eval_program(exprs: &[Expr], output: OutputRef) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let _wind_stack_guard = reset_wind_stack();
    let env = builtins::default_env(output);
    let macro_env = MacroEnvironment::new(None);
    let root = final_continuation();
    let mut pending_resume: Option<(ContinuationRef, Value)> = None;

    loop {
        let result = match pending_resume.take() {
            Some((continuation, value)) => continue_with(continuation, value),
            None => run_sequence_in_cont(exprs, &env, &macro_env, Rc::clone(&root)),
        };

        match result {
            Ok(value) => return Ok(value),
            Err(EvalSignal::Error(error)) => return Err(error),
            Err(EvalSignal::Raise(exception)) => return Err(uncaught_exception_error(exception)),
            Err(EvalSignal::Jump {
                continuation,
                wind_stack,
                value,
            }) => {
                pending_resume = Some(resume_continuation_jump(continuation, wind_stack, value));
            }
        }
    }
}

fn eval_sequence(
    exprs: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    eval_target(
        EvalTarget::BorrowedSequence(exprs),
        Rc::clone(env),
        Rc::clone(macro_env),
        continuation,
    )
}

fn uncaught_exception_error(exception: RaisedException) -> EvalError {
    let error = EvalError::UncaughtException {
        value: exception.value.render(),
    };

    match exception.position {
        Some(position) => error.with_position(position),
        None => error,
    }
}

fn eval_expr(
    expr: &Expr,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    eval_target(
        EvalTarget::BorrowedExpr(expr),
        Rc::clone(env),
        Rc::clone(macro_env),
        continuation,
    )
}

fn eval_sequence_step<'a>(
    exprs: &'a [Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Some((last, prefix)) = exprs.split_last() else {
        return Ok(EvalStep::Value(Value::Void));
    };

    for (index, expr) in prefix.iter().enumerate() {
        run_expr_in_cont(
            expr,
            env,
            macro_env,
            sequence_continuation(&exprs[index + 1..], env, macro_env, continuation),
        )?;
    }

    Ok(tail_borrowed_expr(last, env, macro_env))
}

fn eval_expr_step<'a>(
    expr: &'a Expr,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    match expr {
        Expr::Number(value, _) => Ok(EvalStep::Value(Value::Number(*value))),
        Expr::Boolean(value, _) => Ok(EvalStep::Value(Value::Boolean(*value))),
        Expr::String(value, _) => Ok(EvalStep::Value(Value::String(value.clone()))),
        Expr::Char(value, _) => Ok(EvalStep::Value(Value::Char(*value))),
        Expr::Symbol(name, position) => {
            let value = env_lookup(env, name).ok_or_else(|| {
                EvalError::UnboundVariable { name: name.clone() }.with_position(*position)
            })?;
            if matches!(value, Value::Uninitialized) {
                Err(EvalError::UninitializedBinding { name: name.clone() }
                    .with_position(*position)
                    .into())
            } else {
                Ok(EvalStep::Value(value))
            }
        }
        Expr::CapturedSymbol(name, binding, position) => {
            let value = binding.borrow().clone();
            if matches!(value, Value::Uninitialized) {
                Err(EvalError::UninitializedBinding { name: name.clone() }
                    .with_position(*position)
                    .into())
            } else {
                Ok(EvalStep::Value(value))
            }
        }
        Expr::List(items, position) => {
            eval_borrowed_list_step(items, *position, env, macro_env, continuation)
                .map_err(|signal| signal.with_position(*position))
        }
    }
}

fn eval_owned_expr_step<'a>(
    expr: &OwnedExprRef,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    match expr.current() {
        Expr::Number(value, _) => Ok(EvalStep::Value(Value::Number(*value))),
        Expr::Boolean(value, _) => Ok(EvalStep::Value(Value::Boolean(*value))),
        Expr::String(value, _) => Ok(EvalStep::Value(Value::String(value.clone()))),
        Expr::Char(value, _) => Ok(EvalStep::Value(Value::Char(*value))),
        Expr::Symbol(name, position) => {
            let value = env_lookup(env, name).ok_or_else(|| {
                EvalError::UnboundVariable { name: name.clone() }.with_position(*position)
            })?;
            if matches!(value, Value::Uninitialized) {
                Err(EvalError::UninitializedBinding { name: name.clone() }
                    .with_position(*position)
                    .into())
            } else {
                Ok(EvalStep::Value(value))
            }
        }
        Expr::CapturedSymbol(name, binding, position) => {
            let value = binding.borrow().clone();
            if matches!(value, Value::Uninitialized) {
                Err(EvalError::UninitializedBinding { name: name.clone() }
                    .with_position(*position)
                    .into())
            } else {
                Ok(EvalStep::Value(value))
            }
        }
        Expr::List(items, position) => {
            eval_owned_list_step(expr, items, *position, env, macro_env, continuation)
                .map_err(|signal| signal.with_position(*position))
        }
    }
}

fn eval_borrowed_list_step<'a>(
    items: &'a [Expr],
    position: SourcePos,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Some(head) = items.first() else {
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate empty list".into(),
        }
        .into());
    };

    let head_position = head.position();

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => {
                return special_forms::eval_define(&items[1..], env, macro_env, continuation)
                    .map(EvalStep::Value)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "define-syntax" => {
                return macros::eval_define_syntax(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position).into())
            }
            "if" => {
                return special_forms::eval_if(&items[1..], env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "quote" => {
                return special_forms::eval_quote(&items[1..])
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position).into())
            }
            "lambda" => {
                return special_forms::eval_lambda(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position).into())
            }
            "case-lambda" => {
                return special_forms::eval_case_lambda(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position).into())
            }
            "define-record-type" => {
                return eval_define_record_type(&items[1..], env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position).into())
            }
            "set!" => {
                return special_forms::eval_set(&items[1..], env, macro_env, continuation)
                    .map(EvalStep::Value)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "and" => {
                return special_forms::eval_and(&items[1..], env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "or" => {
                return special_forms::eval_or(&items[1..], env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "begin" => {
                return special_forms::eval_begin(&items[1..], env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "dynamic-wind" => {
                return special_forms::eval_dynamic_wind(&items[1..], env, macro_env, continuation)
                    .map(EvalStep::Value)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "guard" => {
                return special_forms::eval_guard(&items[1..], env, macro_env, continuation)
                    .map(EvalStep::Value)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "cond" => {
                return special_forms::eval_cond(&items[1..], env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "let" => {
                return special_forms::eval_let(&items[1..], env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "let*" => {
                return special_forms::eval_let_star(&items[1..], env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "letrec" => {
                return special_forms::eval_letrec(&items[1..], env, macro_env, false, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "letrec*" => {
                return special_forms::eval_letrec(&items[1..], env, macro_env, true, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "case" => {
                return special_forms::eval_case(&items[1..], env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "do" => {
                return special_forms::eval_do(&items[1..], env, macro_env, continuation)
                    .map(EvalStep::Value)
                    .map_err(|signal| signal.with_position(head_position))
            }
            _ => {}
        }
    }

    if let Some(expanded) = macros::expand_macro_call(items, position, macro_env)
        .map_err(|err| err.with_position(head_position))?
    {
        return eval_expr(&expanded, env, macro_env, continuation).map(EvalStep::Value);
    }

    let callable = match head {
        Expr::Symbol(name, position) => env_lookup(env, name).ok_or_else(|| {
            EvalError::UnknownOperator { name: name.clone() }.with_position(*position)
        })?,
        _ => eval_expr(head, env, macro_env, continuation)?,
    };

    eval_application_step(
        callable,
        &items[1..],
        env,
        macro_env,
        head_position,
        continuation,
    )
}

fn eval_owned_list_step<'a>(
    expr: &OwnedExprRef,
    items: &[Expr],
    position: SourcePos,
    env: &EnvRef,
    macro_env: &MacroEnvRef,
    continuation: &ContinuationRef,
) -> EvalResult<EvalStep<'a>> {
    let Some(head) = items.first() else {
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate empty list".into(),
        }
        .into());
    };

    let head_position = head.position();

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => {
                return special_forms::eval_define(&items[1..], env, macro_env, continuation)
                    .map(EvalStep::Value)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "define-syntax" => {
                return macros::eval_define_syntax(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position).into())
            }
            "if" => {
                return special_forms::eval_owned_if(expr, env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "quote" => {
                return special_forms::eval_quote(&items[1..])
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position).into())
            }
            "lambda" => {
                return special_forms::eval_lambda(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position).into())
            }
            "case-lambda" => {
                return special_forms::eval_case_lambda(&items[1..], env, macro_env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position).into())
            }
            "define-record-type" => {
                return eval_define_record_type(&items[1..], env)
                    .map(EvalStep::Value)
                    .map_err(|err| err.with_position(head_position).into())
            }
            "set!" => {
                return special_forms::eval_set(&items[1..], env, macro_env, continuation)
                    .map(EvalStep::Value)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "and" => {
                return special_forms::eval_owned_and(expr, env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "or" => {
                return special_forms::eval_owned_or(expr, env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "begin" => {
                return special_forms::eval_owned_begin(expr, env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "dynamic-wind" => {
                return special_forms::eval_dynamic_wind(&items[1..], env, macro_env, continuation)
                    .map(EvalStep::Value)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "guard" => {
                return special_forms::eval_guard(&items[1..], env, macro_env, continuation)
                    .map(EvalStep::Value)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "cond" => {
                return special_forms::eval_owned_cond(expr, env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "let" => {
                return special_forms::eval_owned_let(expr, env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "let*" => {
                return special_forms::eval_owned_let_star(expr, env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "letrec" => {
                return special_forms::eval_owned_letrec(expr, env, macro_env, false, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "letrec*" => {
                return special_forms::eval_owned_letrec(expr, env, macro_env, true, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "case" => {
                return special_forms::eval_owned_case(expr, env, macro_env, continuation)
                    .map_err(|signal| signal.with_position(head_position))
            }
            "do" => {
                return special_forms::eval_do(&items[1..], env, macro_env, continuation)
                    .map(EvalStep::Value)
                    .map_err(|signal| signal.with_position(head_position))
            }
            _ => {}
        }
    }

    if let Some(expanded) = macros::expand_macro_call(items, position, macro_env)
        .map_err(|err| err.with_position(head_position))?
    {
        return eval_expr(&expanded, env, macro_env, continuation).map(EvalStep::Value);
    }

    let callable = match head {
        Expr::Symbol(name, position) => env_lookup(env, name).ok_or_else(|| {
            EvalError::UnknownOperator { name: name.clone() }.with_position(*position)
        })?,
        _ => eval_expr(head, env, macro_env, continuation)?,
    };

    eval_application_step(
        callable,
        &items[1..],
        env,
        macro_env,
        head_position,
        continuation,
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
    let (value, _) = eval_input(input)?;
    Ok(value.render())
}

fn eval_input(input: &str) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_program()?;
    let output = Rc::new(RefCell::new(String::new()));
    let value = eval_program(&exprs, Rc::clone(&output))?;
    let rendered_output = output.borrow().clone();
    Ok((value, rendered_output))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_input(input)?;
    Ok((value.render(), output))
}

#[cfg(test)]
mod tests;
