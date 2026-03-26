pub mod error;
mod macros;
mod number;

pub use error::EvalError;

use macros::{
    expr_from_syntax, match_syntax_pattern, parse_transformer, syntax_from_use_expr,
    transformer_macro, MacroEnv, MacroExpander, PatternBinding, SymbolOrigin, SyntaxExpr,
    SyntaxSymbol,
};
use number::Number;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    panic::{catch_unwind, panic_any, resume_unwind, AssertUnwindSafe},
    rc::Rc,
    sync::OnceLock,
};

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Number(Number),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Position {
    line: usize,
    column: usize,
}

#[derive(Debug, Clone, PartialEq)]
struct PositionedExpr {
    expr: Expr,
    position: Position,
}

#[derive(Debug, Clone)]
struct SchemeString {
    inner: Rc<SchemeStringInner>,
}

#[derive(Debug)]
struct SchemeStringInner {
    chars: RefCell<Vec<char>>,
    mutable: bool,
}

#[derive(Debug, Clone)]
struct SchemeVector {
    inner: Rc<RefCell<Vec<Value>>>,
}

#[derive(Debug)]
struct RecordType {
    name: String,
    field_count: usize,
}

#[derive(Debug, Clone)]
struct RecordValue {
    record_type: Rc<RecordType>,
    fields: Vec<Value>,
}

type PairRef = Rc<RefCell<PairCell>>;
type DynamicWindRef = Rc<DynamicWind>;

#[derive(Debug, Clone)]
struct PairCell {
    car: Value,
    cdr: Value,
}

#[derive(Debug)]
struct DynamicWind {
    in_thunk: Value,
    out_thunk: Value,
}

impl SchemeString {
    fn new_immutable(value: impl AsRef<str>) -> Self {
        Self::from_chars(value.as_ref().chars().collect(), false)
    }

    fn new_mutable(value: impl AsRef<str>) -> Self {
        Self::from_chars(value.as_ref().chars().collect(), true)
    }

    fn new_runtime(value: impl AsRef<str>) -> Self {
        if strings_are_mutable_in_current_level() {
            Self::new_mutable(value)
        } else {
            Self::new_immutable(value)
        }
    }

    fn from_chars(chars: Vec<char>, mutable: bool) -> Self {
        Self {
            inner: Rc::new(SchemeStringInner {
                chars: RefCell::new(chars),
                mutable,
            }),
        }
    }

    fn from_runtime_chars(chars: Vec<char>) -> Self {
        Self::from_chars(chars, strings_are_mutable_in_current_level())
    }

    fn copy_runtime(&self) -> Self {
        Self::from_chars(self.chars(), strings_are_mutable_in_current_level())
    }

    fn chars(&self) -> Vec<char> {
        self.inner.chars.borrow().clone()
    }

    fn len(&self) -> usize {
        self.inner.chars.borrow().len()
    }

    fn get(&self, index: usize) -> char {
        self.inner.chars.borrow()[index]
    }

    fn set(&self, index: usize, value: char, name: &'static str) -> Result<(), EvalError> {
        if !self.inner.mutable {
            return Err(EvalError::ImmutableString { name });
        }

        self.inner.chars.borrow_mut()[index] = value;
        Ok(())
    }

    fn to_plain_string(&self) -> String {
        self.inner.chars.borrow().iter().collect()
    }
}

impl SchemeVector {
    fn new(values: Vec<Value>) -> Self {
        Self {
            inner: Rc::new(RefCell::new(values)),
        }
    }

    fn len(&self) -> usize {
        self.inner.borrow().len()
    }

    fn get(&self, index: usize) -> Value {
        self.inner.borrow()[index].clone()
    }

    fn set(&self, index: usize, value: Value) {
        self.inner.borrow_mut()[index] = value;
    }

    fn values(&self) -> Vec<Value> {
        self.inner.borrow().clone()
    }
}

#[derive(Debug, Clone)]
enum Value {
    Number(Number),
    Boolean(bool),
    String(SchemeString),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Pair(PairRef),
    Vector(SchemeVector),
    Record(Rc<RecordValue>),
    Syntax(Rc<SyntaxExpr>),
    SyntaxList(Vec<Rc<SyntaxExpr>>),
    Procedure(Rc<Procedure>),
    Values(Vec<Value>),
    Uninitialized,
    Void,
}

#[derive(Debug, Clone)]
enum Procedure {
    Builtin {
        name: &'static str,
    },
    Continuation {
        frames: Vec<ContinuationFrame>,
        position: Option<Position>,
        winds: Vec<DynamicWindRef>,
        handlers: Vec<Rc<ExceptionHandler>>,
    },
    RecordConstructor {
        name: String,
        record_type: Rc<RecordType>,
        field_indices: Vec<usize>,
    },
    RecordPredicate {
        name: String,
        record_type: Rc<RecordType>,
    },
    RecordAccessor {
        name: String,
        record_type: Rc<RecordType>,
        field_index: usize,
    },
    Lambda {
        params: LambdaParams,
        body: Vec<Expr>,
        env: EnvRef,
    },
    CaseLambda {
        clauses: Vec<LambdaClause>,
        env: EnvRef,
    },
}

#[derive(Debug, Clone)]
struct LambdaParams {
    required: Vec<String>,
    rest: Option<String>,
}

#[derive(Debug, Clone)]
struct LambdaClause {
    params: LambdaParams,
    body: Vec<Expr>,
}

#[derive(Debug)]
struct RecordConstructorSpec {
    name: String,
    fields: Vec<String>,
}

#[derive(Debug)]
struct RecordFieldSpec {
    name: String,
    accessor: String,
}

#[derive(Debug, Clone)]
struct DoBindingSpec {
    name: String,
    init: Expr,
    step: Option<Expr>,
}

impl LambdaParams {
    fn fixed(required: Vec<String>) -> Self {
        Self {
            required,
            rest: None,
        }
    }

    fn matches_arity(&self, arg_count: usize) -> bool {
        arg_count >= self.required.len()
            && (self.rest.is_some() || arg_count == self.required.len())
    }
}

type EnvRef = Rc<Env>;

#[derive(Debug)]
struct Env {
    values: RefCell<HashMap<String, Rc<RefCell<Value>>>>,
    parent: Option<EnvRef>,
}

#[derive(Debug, Clone)]
struct TopLevelState {
    remaining: Vec<PositionedExpr>,
    env: EnvRef,
    macros: MacroEnv,
    expander: MacroExpander,
}

#[derive(Debug, Clone)]
enum PendingArg {
    Expr(Expr),
    Value(Value),
}

#[derive(Debug, Clone)]
enum ContinuationFrame {
    Program(TopLevelState),
    ApplicationOperator {
        args: Vec<Expr>,
        env: EnvRef,
    },
    ApplicationArg {
        procedure: Value,
        before: Vec<PendingArg>,
        after: Vec<PendingArg>,
        env: EnvRef,
    },
    Sequence {
        remaining: Vec<Expr>,
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
    CaseKey {
        clauses: Vec<Expr>,
        env: EnvRef,
    },
    CondTest {
        body: Vec<Expr>,
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    GuardTest {
        exception: Value,
        body: Vec<Expr>,
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    If {
        consequent: Expr,
        alternate: Option<Expr>,
        env: EnvRef,
    },
    Define {
        name: String,
        env: EnvRef,
    },
    Set {
        name: String,
        env: EnvRef,
    },
    Let {
        name: String,
        evaluated: Vec<(String, Value)>,
        remaining: Vec<(String, Expr)>,
        body: Vec<Expr>,
        env: EnvRef,
    },
    NamedLet {
        name: String,
        params: Vec<String>,
        evaluated: Vec<Value>,
        remaining: Vec<(String, Expr)>,
        body: Vec<Expr>,
        env: EnvRef,
    },
    LetStar {
        name: String,
        let_env: EnvRef,
        remaining: Vec<(String, Expr)>,
        body: Vec<Expr>,
    },
    LetrecSequential {
        cell: Rc<RefCell<Value>>,
        remaining: Vec<(String, Expr)>,
        body: Vec<Expr>,
        letrec_env: EnvRef,
    },
    LetrecParallel {
        cells: Vec<Rc<RefCell<Value>>>,
        evaluated: Vec<Value>,
        remaining: Vec<Expr>,
        body: Vec<Expr>,
        letrec_env: EnvRef,
    },
    MapCall {
        procedure: Value,
        lists: Vec<Vec<Value>>,
        index: usize,
        results: Vec<Value>,
    },
    ForEachCall {
        procedure: Value,
        lists: Vec<Vec<Value>>,
        index: usize,
    },
    DynamicWindAfterIn {
        wind: DynamicWindRef,
        body_thunk: Value,
    },
    DynamicWindAfterBody {
        wind: DynamicWindRef,
    },
    DynamicWindAfterOut {
        result: Value,
    },
}

type ExceptionHandlerRef = Rc<ExceptionHandler>;

#[derive(Debug)]
struct ExceptionHandler {
    kind: ExceptionHandlerKind,
    frames: Vec<ContinuationFrame>,
    winds: Vec<DynamicWindRef>,
    outer_handlers: Vec<ExceptionHandlerRef>,
    position: Option<Position>,
}

#[derive(Debug)]
enum ExceptionHandlerKind {
    Procedure {
        handler: Value,
    },
    Guard {
        variable: String,
        clauses: Vec<Expr>,
        env: EnvRef,
    },
}

#[derive(Debug)]
struct EvalContext {
    output: String,
    frames: Rc<RefCell<Vec<ContinuationFrame>>>,
    dynamic_winds: Rc<RefCell<Vec<DynamicWindRef>>>,
    exception_handlers: Rc<RefCell<Vec<ExceptionHandlerRef>>>,
    position: Rc<RefCell<Option<Position>>>,
}

#[derive(Debug, Clone)]
struct ContinuationJump {
    value: Value,
    frames: Vec<ContinuationFrame>,
    winds: Vec<DynamicWindRef>,
    handlers: Vec<ExceptionHandlerRef>,
    position: Option<Position>,
}

#[derive(Debug)]
struct ContinuationSignal;

#[derive(Debug, Clone)]
struct ExceptionJump {
    value: Value,
    handlers: Vec<ExceptionHandlerRef>,
    position: Option<Position>,
}

#[derive(Debug)]
struct ExceptionSignal;

thread_local! {
    static CONTINUATION_JUMP: RefCell<Option<ContinuationJump>> = const { RefCell::new(None) };
    static EXCEPTION_JUMP: RefCell<Option<ExceptionJump>> = const { RefCell::new(None) };
}

impl Default for EvalContext {
    fn default() -> Self {
        Self {
            output: String::new(),
            frames: Rc::new(RefCell::new(Vec::new())),
            dynamic_winds: Rc::new(RefCell::new(Vec::new())),
            exception_handlers: Rc::new(RefCell::new(Vec::new())),
            position: Rc::new(RefCell::new(None)),
        }
    }
}

#[derive(Debug, Clone)]
enum EvalStep {
    Value(Value),
    Expr(Expr, EnvRef),
    Sequence(Vec<Expr>, EnvRef),
    Apply(Value, Vec<Value>),
}

#[derive(Debug, Clone)]
enum EvalAction {
    Program(TopLevelState),
    Resume {
        value: Value,
        frames: Vec<ContinuationFrame>,
        winds: Vec<DynamicWindRef>,
        handlers: Vec<ExceptionHandlerRef>,
        position: Option<Position>,
    },
    HandleException {
        value: Value,
        handler: ExceptionHandlerRef,
    },
    UncaughtException {
        value: Value,
        position: Option<Position>,
    },
}

fn current_bench_level() -> u32 {
    static BENCH_LEVEL: OnceLock<u32> = OnceLock::new();

    *BENCH_LEVEL.get_or_init(|| {
        std::env::var("BENCH_LEVEL")
            .ok()
            .and_then(|value| value.parse().ok())
            .unwrap_or(u32::MAX)
    })
}

fn continuations_are_enabled_in_current_level() -> bool {
    current_bench_level() >= 18
}

fn exceptions_are_enabled_in_current_level() -> bool {
    current_bench_level() >= 20
}

fn strings_are_mutable_in_current_level() -> bool {
    current_bench_level() < 15
}

struct ContinuationFrameGuard {
    frames: Rc<RefCell<Vec<ContinuationFrame>>>,
    active: bool,
}

impl Drop for ContinuationFrameGuard {
    fn drop(&mut self) {
        if self.active {
            self.frames.borrow_mut().pop();
        }
    }
}

struct PositionGuard {
    position: Rc<RefCell<Option<Position>>>,
    previous: Option<Position>,
}

impl Drop for PositionGuard {
    fn drop(&mut self) {
        *self.position.borrow_mut() = self.previous;
    }
}

struct ExceptionHandlerGuard {
    handlers: Rc<RefCell<Vec<ExceptionHandlerRef>>>,
    expected: ExceptionHandlerRef,
    active: bool,
}

impl Drop for ExceptionHandlerGuard {
    fn drop(&mut self) {
        if !self.active {
            return;
        }

        let popped = self.handlers.borrow_mut().pop();
        debug_assert!(
            popped
                .as_ref()
                .is_some_and(|handler| Rc::ptr_eq(handler, &self.expected)),
            "exception handler stack must unwind in LIFO order",
        );
    }
}

fn push_continuation_frame(
    context: &EvalContext,
    frame: ContinuationFrame,
) -> ContinuationFrameGuard {
    let active = continuations_are_enabled_in_current_level();
    if active {
        context.frames.borrow_mut().push(frame);
    }
    ContinuationFrameGuard {
        frames: context.frames.clone(),
        active,
    }
}

fn push_position(context: &EvalContext, position: Option<Position>) -> PositionGuard {
    let previous = std::mem::replace(&mut *context.position.borrow_mut(), position);
    PositionGuard {
        position: context.position.clone(),
        previous,
    }
}

fn current_position(context: &EvalContext) -> Option<Position> {
    *context.position.borrow()
}

fn push_exception_handler(
    context: &EvalContext,
    handler: ExceptionHandlerRef,
) -> ExceptionHandlerGuard {
    let active = exceptions_are_enabled_in_current_level();
    if active {
        context
            .exception_handlers
            .borrow_mut()
            .push(handler.clone());
    }
    ExceptionHandlerGuard {
        handlers: context.exception_handlers.clone(),
        expected: handler,
        active,
    }
}

fn push_dynamic_wind(wind: DynamicWindRef, context: &EvalContext) {
    context.dynamic_winds.borrow_mut().push(wind);
}

fn pop_dynamic_wind(expected: &DynamicWindRef, context: &EvalContext) {
    let popped = context.dynamic_winds.borrow_mut().pop();
    debug_assert!(
        popped
            .as_ref()
            .is_some_and(|wind| Rc::ptr_eq(wind, expected)),
        "dynamic-wind stack must unwind in LIFO order",
    );
}

fn dynamic_wind_prefix_len(current: &[DynamicWindRef], target: &[DynamicWindRef]) -> usize {
    current
        .iter()
        .zip(target)
        .take_while(|(left, right)| Rc::ptr_eq(left, right))
        .count()
}

fn transition_dynamic_winds(
    target: &[DynamicWindRef],
    context: &mut EvalContext,
) -> Result<(), EvalError> {
    loop {
        let current = context.dynamic_winds.borrow().clone();
        let shared = dynamic_wind_prefix_len(&current, target);

        if shared == current.len() && shared == target.len() {
            return Ok(());
        }

        if current.len() > shared {
            let wind = current
                .last()
                .expect("current dynamic-wind stack is non-empty when unwinding")
                .clone();
            pop_dynamic_wind(&wind, context);
            apply_procedure_single(wind.out_thunk.clone(), Vec::new(), context)?;
        } else {
            let wind = target[shared].clone();
            apply_procedure_single(wind.in_thunk.clone(), Vec::new(), context)?;
            push_dynamic_wind(wind, context);
        }
    }
}

fn signal_continuation(
    value: Value,
    frames: Vec<ContinuationFrame>,
    winds: Vec<DynamicWindRef>,
    handlers: Vec<ExceptionHandlerRef>,
    position: Option<Position>,
) -> ! {
    CONTINUATION_JUMP.with(|slot| {
        *slot.borrow_mut() = Some(ContinuationJump {
            value,
            frames,
            winds,
            handlers,
            position,
        });
    });
    panic_any(ContinuationSignal);
}

fn take_continuation_jump() -> ContinuationJump {
    CONTINUATION_JUMP
        .with(|slot| slot.borrow_mut().take())
        .expect("continuation signal must carry a jump payload")
}

fn signal_exception(
    value: Value,
    handlers: Vec<ExceptionHandlerRef>,
    position: Option<Position>,
) -> ! {
    EXCEPTION_JUMP.with(|slot| {
        *slot.borrow_mut() = Some(ExceptionJump {
            value,
            handlers,
            position,
        });
    });
    panic_any(ExceptionSignal);
}

fn take_exception_jump() -> ExceptionJump {
    EXCEPTION_JUMP
        .with(|slot| slot.borrow_mut().take())
        .expect("exception signal must carry a jump payload")
}

impl Env {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            values: RefCell::new(HashMap::new()),
            parent,
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        let name = name.into();
        let mut values = self.values.borrow_mut();
        if let Some(slot) = values.get(&name) {
            *slot.borrow_mut() = value;
        } else {
            values.insert(name, Rc::new(RefCell::new(value)));
        }
    }

    fn define_cell(&self, name: impl Into<String>, value: Rc<RefCell<Value>>) {
        self.values.borrow_mut().insert(name.into(), value);
    }

    fn get(&self, name: &str) -> Option<Value> {
        self.lookup_cell(name).map(|value| value.borrow().clone())
    }

    fn lookup_cell(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.values.borrow().get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.lookup_cell(name))
        })
    }

    fn set(&self, name: &str, value: Value) -> Result<(), EvalError> {
        if let Some(slot) = self.lookup_cell(name) {
            *slot.borrow_mut() = value;
            Ok(())
        } else {
            Err(EvalError::UnboundVariable {
                name: name.to_string(),
            })
        }
    }
}

impl Procedure {
    fn call_step(
        &self,
        args: Vec<Value>,
        context: &mut EvalContext,
    ) -> Result<EvalStep, EvalError> {
        match self {
            Self::Builtin { name } => {
                if matches!(*name, "call/cc" | "call-with-current-continuation") {
                    if args.len() != 1 {
                        return Err(EvalError::WrongArgCount {
                            name,
                            expected: "exactly 1 argument",
                            got: args.len(),
                        });
                    }

                    let continuation = Value::Procedure(Rc::new(Procedure::Continuation {
                        frames: context.frames.borrow().clone(),
                        position: current_position(context),
                        winds: context.dynamic_winds.borrow().clone(),
                        handlers: context.exception_handlers.borrow().clone(),
                    }));

                    Ok(EvalStep::Apply(args[0].clone(), vec![continuation]))
                } else if *name == "call-with-values" {
                    if args.len() != 2 {
                        return Err(EvalError::WrongArgCount {
                            name,
                            expected: "exactly 2 arguments",
                            got: args.len(),
                        });
                    }

                    let produced = apply_procedure(args[0].clone(), Vec::new(), context)?;
                    Ok(EvalStep::Apply(args[1].clone(), unpack_values(produced)))
                } else if *name == "dynamic-wind" {
                    if args.len() != 3 {
                        return Err(EvalError::WrongArgCount {
                            name,
                            expected: "exactly 3 arguments",
                            got: args.len(),
                        });
                    }

                    let wind = Rc::new(DynamicWind {
                        in_thunk: args[0].clone(),
                        out_thunk: args[2].clone(),
                    });

                    apply_procedure_with_frame_single(
                        args[0].clone(),
                        Vec::new(),
                        ContinuationFrame::DynamicWindAfterIn {
                            wind: wind.clone(),
                            body_thunk: args[1].clone(),
                        },
                        context,
                    )?;

                    push_dynamic_wind(wind.clone(), context);

                    let result = apply_procedure_with_frame(
                        args[1].clone(),
                        Vec::new(),
                        ContinuationFrame::DynamicWindAfterBody { wind: wind.clone() },
                        context,
                    )?;

                    pop_dynamic_wind(&wind, context);

                    apply_procedure_with_frame_single(
                        wind.out_thunk.clone(),
                        Vec::new(),
                        ContinuationFrame::DynamicWindAfterOut {
                            result: result.clone(),
                        },
                        context,
                    )?;

                    Ok(EvalStep::Value(result))
                } else if *name == "apply" {
                    if args.len() < 2 {
                        return Err(EvalError::WrongArgCount {
                            name: "apply",
                            expected: "at least 2 arguments",
                            got: args.len(),
                        });
                    }

                    let mut applied_args = args[1..args.len() - 1].to_vec();
                    let tail = args
                        .last()
                        .expect("apply arity checked before reading tail");
                    applied_args.extend(collect_list("apply", tail)?);

                    Ok(EvalStep::Apply(args[0].clone(), applied_args))
                } else {
                    Ok(EvalStep::Value(apply_builtin(name, &args, context)?))
                }
            }
            Self::Continuation {
                frames,
                position,
                winds,
                handlers,
            } => {
                if args.len() != 1 {
                    return Err(EvalError::WrongArgCountDynamic {
                        name: "continuation".into(),
                        expected: "exactly 1 argument".into(),
                        got: args.len(),
                    });
                }

                signal_continuation(
                    args[0].clone(),
                    frames.clone(),
                    winds.clone(),
                    handlers.clone(),
                    *position,
                );
            }
            Self::RecordConstructor {
                name,
                record_type,
                field_indices,
            } => {
                if args.len() != field_indices.len() {
                    return Err(EvalError::WrongArgCountDynamic {
                        name: name.clone(),
                        expected: format!("exactly {} arguments", field_indices.len()),
                        got: args.len(),
                    });
                }

                let mut fields = vec![Value::Void; record_type.field_count];
                for (value, index) in args.into_iter().zip(field_indices.iter().copied()) {
                    fields[index] = value;
                }

                Ok(EvalStep::Value(Value::Record(Rc::new(RecordValue {
                    record_type: record_type.clone(),
                    fields,
                }))))
            }
            Self::RecordPredicate { name, record_type } => {
                if args.len() != 1 {
                    return Err(EvalError::WrongArgCountDynamic {
                        name: name.clone(),
                        expected: "exactly 1 argument".into(),
                        got: args.len(),
                    });
                }

                Ok(EvalStep::Value(Value::Boolean(matches!(
                    &args[0],
                    Value::Record(record) if Rc::ptr_eq(&record.record_type, record_type)
                ))))
            }
            Self::RecordAccessor {
                name,
                record_type,
                field_index,
            } => {
                if args.len() != 1 {
                    return Err(EvalError::WrongArgCountDynamic {
                        name: name.clone(),
                        expected: "exactly 1 argument".into(),
                        got: args.len(),
                    });
                }

                match &args[0] {
                    Value::Record(record) if Rc::ptr_eq(&record.record_type, record_type) => {
                        Ok(EvalStep::Value(record.fields[*field_index].clone()))
                    }
                    Value::Record(record) => Err(EvalError::ExpectedRecordType {
                        name: name.clone(),
                        expected: record_type.name.clone(),
                        found: record.record_type.name.clone(),
                    }),
                    other => Err(EvalError::ExpectedRecordType {
                        name: name.clone(),
                        expected: record_type.name.clone(),
                        found: other.type_name().into(),
                    }),
                }
            }
            Self::Lambda { params, body, env } => {
                if !params.matches_arity(args.len()) {
                    return Err(EvalError::WrongArgCount {
                        name: "lambda",
                        expected: if params.rest.is_some() {
                            "at least the declared number of required arguments"
                        } else {
                            "exactly the declared number of arguments"
                        },
                        got: args.len(),
                    });
                }

                let call_env = bind_lambda_args(params, args, env);
                Ok(EvalStep::Sequence(body.clone(), call_env))
            }
            Self::CaseLambda { clauses, env } => {
                let Some(clause) = clauses
                    .iter()
                    .find(|clause| clause.params.matches_arity(args.len()))
                else {
                    return Err(EvalError::WrongArgCountDynamic {
                        name: "case-lambda".into(),
                        expected: "a matching clause".into(),
                        got: args.len(),
                    });
                };

                let call_env = bind_lambda_args(&clause.params, args, env);
                Ok(EvalStep::Sequence(clause.body.clone(), call_env))
            }
        }
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
            Self::Symbol(_) => "symbol",
            Self::Char(_) => "character",
            Self::List(_) => "list",
            Self::Pair(_) => "pair",
            Self::Vector(_) => "vector",
            Self::Record(_) => "record",
            Self::Syntax(_) => "syntax",
            Self::SyntaxList(_) => "syntax-list",
            Self::Procedure(_) => "procedure",
            Self::Values(_) => "values",
            Self::Uninitialized => "uninitialized",
            Self::Void => "void",
        }
    }

    fn render(&self) -> String {
        render_value(self, false)
    }

    fn display_render(&self) -> String {
        render_value(self, true)
    }
}

fn pack_values(values: Vec<Value>) -> Value {
    match values.len() {
        1 => values.into_iter().next().unwrap(),
        _ => Value::Values(values),
    }
}

fn unpack_values(value: Value) -> Vec<Value> {
    match value {
        Value::Values(values) => values,
        other => vec![other],
    }
}

fn expect_single_value(value: Value) -> Result<Value, EvalError> {
    match value {
        Value::Values(mut values) if values.len() == 1 => Ok(values.pop().unwrap()),
        Value::Values(values) => Err(EvalError::WrongValueCount { got: values.len() }),
        other => Ok(other),
    }
}

struct Parser<'a> {
    input: &'a str,
    offset: usize,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            offset: 0,
            line: 1,
            column: 1,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<PositionedExpr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            let position = self.current_position();
            let expr = self.parse_expr()?;
            expressions.push(PositionedExpr { expr, position });
            self.skip_ignored();
        }

        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        let Some(ch) = self.peek_char() else {
            return Err(self.error(EvalError::UnexpectedEof));
        };

        match ch {
            '(' => self.parse_list(),
            '\'' => self.parse_quote_shorthand(),
            '"' => self.parse_string(),
            '#' => self.parse_hash_literal(),
            ')' => Err(self.error(EvalError::UnexpectedToken { token: ")".into() })),
            '-' if self
                .peek_second_char()
                .is_some_and(|next| next.is_ascii_digit()) =>
            {
                self.parse_number()
            }
            ch if ch.is_ascii_digit() => self.parse_number(),
            _ => self.parse_symbol(),
        }
    }

    fn parse_quote_shorthand(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('\'')?;
        let quoted = self.parse_expr()?;
        Ok(Expr::List(vec![Expr::Symbol("quote".into()), quoted]))
    }

    fn parse_syntax_shorthand(&mut self) -> Result<Expr, EvalError> {
        let quoted = self.parse_expr()?;
        Ok(Expr::List(vec![Expr::Symbol("syntax".into()), quoted]))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek_char() {
                Some(')') => {
                    self.bump_char();
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(self.error(EvalError::UnexpectedEof)),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('"')?;
        let mut value = String::new();

        while let Some(ch) = self.bump_char() {
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let escaped = self
                        .bump_char()
                        .ok_or_else(|| self.error(EvalError::UnterminatedString))?;
                    value.push(match escaped {
                        'n' => '\n',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                }
                other => value.push(other),
            }
        }

        Err(self.error(EvalError::UnterminatedString))
    }

    fn parse_hash_literal(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('#')?;
        match self.bump_char() {
            Some('t') => Ok(Expr::Boolean(true)),
            Some('f') => Ok(Expr::Boolean(false)),
            Some('\'') => self.parse_syntax_shorthand(),
            Some('\\') => self.parse_character_literal(),
            Some(other) => Err(self.error(EvalError::InvalidBoolean {
                literal: format!("#{other}"),
            })),
            None => Err(self.error(EvalError::UnexpectedEof)),
        }
    }

    fn parse_character_literal(&mut self) -> Result<Expr, EvalError> {
        let Some(first) = self.peek_char() else {
            return Err(self.error(EvalError::InvalidCharacter {
                literal: "#\\".into(),
            }));
        };

        if first.is_whitespace() {
            return Err(self.error(EvalError::InvalidCharacter {
                literal: "#\\".into(),
            }));
        }

        if first.is_ascii_alphabetic() {
            let start = self.offset;
            while self.peek_char().is_some_and(|ch| !is_token_delimiter(ch)) {
                self.bump_char();
            }

            let literal = &self.input[start..self.offset];
            let value = match literal {
                "space" => ' ',
                "newline" => '\n',
                _ if literal.chars().count() == 1 => literal.chars().next().unwrap(),
                _ => {
                    return Err(self.error(EvalError::InvalidCharacter {
                        literal: format!("#\\{literal}"),
                    }));
                }
            };

            Ok(Expr::Char(value))
        } else {
            Ok(Expr::Char(self.bump_char().unwrap()))
        }
    }

    fn parse_number(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;
        while self.peek_char().is_some_and(|ch| !is_token_delimiter(ch)) {
            self.bump_char();
        }

        let literal = &self.input[start..self.offset];
        Number::parse_literal(literal)
            .map(Expr::Number)
            .map_err(|_| {
                self.error(EvalError::InvalidNumber {
                    literal: literal.into(),
                })
            })
    }

    fn parse_symbol(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;

        while self.peek_char().is_some_and(|ch| !is_token_delimiter(ch)) {
            self.bump_char();
        }

        if start == self.offset {
            let token = self
                .peek_char()
                .map(|ch| ch.to_string())
                .unwrap_or_else(|| "<eof>".into());
            return Err(self.error(EvalError::UnexpectedToken { token }));
        }

        Ok(Expr::Symbol(self.input[start..self.offset].into()))
    }

    fn skip_ignored(&mut self) {
        loop {
            while self.peek_char().is_some_and(char::is_whitespace) {
                self.bump_char();
            }

            if self.peek_char() != Some(';') {
                return;
            }

            while let Some(ch) = self.bump_char() {
                if ch == '\n' {
                    break;
                }
            }
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        match self.bump_char() {
            Some(ch) if ch == expected => Ok(()),
            Some(ch) => Err(self.error(EvalError::UnexpectedToken {
                token: ch.to_string(),
            })),
            None => Err(self.error(EvalError::UnexpectedEof)),
        }
    }

    fn bump_char(&mut self) -> Option<char> {
        let mut chars = self.input[self.offset..].chars();
        let ch = chars.next()?;
        self.offset += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(ch)
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.offset..].chars().next()
    }

    fn peek_second_char(&self) -> Option<char> {
        let mut chars = self.input[self.offset..].chars();
        chars.next()?;
        chars.next()
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.input.len()
    }

    fn current_position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }

    fn error(&self, error: EvalError) -> EvalError {
        let position = self.current_position();
        error.with_position(position.line, position.column)
    }
}

fn resolve_eval_step(mut step: EvalStep, context: &mut EvalContext) -> Result<Value, EvalError> {
    loop {
        step = match step {
            EvalStep::Value(value) => return Ok(value),
            EvalStep::Expr(expr, env) => eval_expr_step(expr, env, context)?,
            EvalStep::Sequence(expressions, env) => eval_sequence_step(expressions, env, context)?,
            EvalStep::Apply(procedure, args) => apply_procedure_step(procedure, args, context)?,
        };
    }
}

fn eval_expr_in_env(
    expr: &Expr,
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    resolve_eval_step(EvalStep::Expr(expr.clone(), env.clone()), context)
}

fn eval_expr_in_env_single(
    expr: &Expr,
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    expect_single_value(eval_expr_in_env(expr, env, context)?)
}

fn eval_expr_with_frame(
    expr: &Expr,
    env: &EnvRef,
    frame: ContinuationFrame,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let _guard = push_continuation_frame(context, frame);
    eval_expr_in_env(expr, env, context)
}

fn eval_expr_with_frame_single(
    expr: &Expr,
    env: &EnvRef,
    frame: ContinuationFrame,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    expect_single_value(eval_expr_with_frame(expr, env, frame, context)?)
}

fn apply_procedure_with_frame(
    procedure: Value,
    args: Vec<Value>,
    frame: ContinuationFrame,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let _guard = push_continuation_frame(context, frame);
    apply_procedure(procedure, args, context)
}

fn apply_procedure_single(
    procedure: Value,
    args: Vec<Value>,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    expect_single_value(apply_procedure(procedure, args, context)?)
}

fn apply_procedure_with_frame_single(
    procedure: Value,
    args: Vec<Value>,
    frame: ContinuationFrame,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    expect_single_value(apply_procedure_with_frame(procedure, args, frame, context)?)
}

fn run_eval_action(mut action: EvalAction, context: &mut EvalContext) -> Result<Value, EvalError> {
    loop {
        let outcome = catch_unwind(AssertUnwindSafe(|| match action.clone() {
            EvalAction::Program(state) => eval_top_level_state(state, context),
            EvalAction::Resume {
                value,
                frames,
                winds,
                handlers,
                position,
            } => {
                let _position_guard = push_position(context, position);
                transition_dynamic_winds(&winds, context)?;
                context
                    .exception_handlers
                    .borrow_mut()
                    .clone_from(&handlers);
                resume_continuation_frames(value, frames, context)
            }
            EvalAction::HandleException { value, handler } => {
                handle_exception(value, &handler, context)
            }
            EvalAction::UncaughtException { value, position } => {
                let _position_guard = push_position(context, position);
                transition_dynamic_winds(&[], context)?;
                context.exception_handlers.borrow_mut().clear();

                let error = EvalError::UncaughtException {
                    value: value.render(),
                };
                if let Some(position) = position {
                    Err(error.with_position(position.line, position.column))
                } else {
                    Err(error)
                }
            }
        }));

        match outcome {
            Ok(result) => return result,
            Err(payload) => {
                if payload.downcast_ref::<ContinuationSignal>().is_some() {
                    let jump = take_continuation_jump();
                    action = EvalAction::Resume {
                        value: jump.value,
                        frames: jump.frames,
                        winds: jump.winds,
                        handlers: jump.handlers,
                        position: jump.position,
                    };
                } else if payload.downcast_ref::<ExceptionSignal>().is_some() {
                    let jump = take_exception_jump();
                    if let Some(handler) = jump.handlers.last().cloned() {
                        action = EvalAction::HandleException {
                            value: jump.value,
                            handler,
                        };
                    } else {
                        action = EvalAction::UncaughtException {
                            value: jump.value,
                            position: jump.position,
                        };
                    }
                } else {
                    resume_unwind(payload);
                }
            }
        }
    }
}

fn handle_exception(
    value: Value,
    handler: &ExceptionHandlerRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let frames = handler.frames.clone();
    let winds = handler.winds.clone();
    let outer_handlers = handler.outer_handlers.clone();
    let position = handler.position;

    let _position_guard = push_position(context, position);
    transition_dynamic_winds(&winds, context)?;
    context.frames.borrow_mut().clone_from(&frames);
    context
        .exception_handlers
        .borrow_mut()
        .clone_from(&outer_handlers);

    let result = match &handler.kind {
        ExceptionHandlerKind::Procedure { handler } => {
            apply_procedure(handler.clone(), vec![value], context)?
        }
        ExceptionHandlerKind::Guard {
            variable,
            clauses,
            env,
        } => eval_guard_handler(value, variable, clauses, env, context)?,
    };

    resume_continuation_frames(result, frames, context)
}

fn parse_define_syntax<'a>(expr: &'a Expr) -> Result<Option<(&'a String, &'a Expr)>, EvalError> {
    let Expr::List(items) = expr else {
        return Ok(None);
    };

    let Some(Expr::Symbol(keyword)) = items.first() else {
        return Ok(None);
    };

    if keyword != "define-syntax" {
        return Ok(None);
    }

    if items.len() != 3 {
        return Err(EvalError::InvalidForm {
            name: "define-syntax",
            message: "expected a name and transformer",
        });
    }

    let Expr::Symbol(name) = &items[1] else {
        return Err(EvalError::InvalidForm {
            name: "define-syntax",
            message: "expected a macro name",
        });
    };

    Ok(Some((name, &items[2])))
}

fn register_top_level_macro_definition(
    expr: &Expr,
    env: &EnvRef,
    macros: &mut MacroEnv,
) -> Result<bool, EvalError> {
    let Some((name, transformer_expr)) = parse_define_syntax(expr)? else {
        return Ok(false);
    };

    let macro_def = if matches!(
        transformer_expr,
        Expr::List(items) if matches!(items.first(), Some(Expr::Symbol(symbol)) if symbol == "syntax-rules")
    ) {
        parse_transformer(name.clone(), transformer_expr, env.clone())?
    } else {
        let mut macro_context = EvalContext::default();
        let transformer = eval_expr_in_env_single(transformer_expr, env, &mut macro_context)?;
        if !matches!(transformer, Value::Procedure(_)) {
            return Err(EvalError::InvalidForm {
                name: "define-syntax",
                message: "expected the transformer to evaluate to a procedure",
            });
        }
        transformer_macro(name.clone(), transformer, env.clone())
    };

    macros.insert(name.clone(), Rc::new(macro_def));
    Ok(true)
}

fn run_transformer_macro(invocation: &Expr, transformer: &Value) -> Result<SyntaxExpr, EvalError> {
    let mut context = EvalContext::default();
    let result = apply_procedure_single(
        transformer.clone(),
        vec![Value::Syntax(Rc::new(syntax_from_use_expr(invocation)))],
        &mut context,
    )?;
    let syntax = expect_syntax("macro transformer", &result)?;
    Ok(syntax.as_ref().clone())
}

fn eval_top_level_state(
    mut state: TopLevelState,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let mut last_value = Value::Void;
    let mut expressions = state.remaining.into_iter();

    while let Some(expression) = expressions.next() {
        if register_top_level_macro_definition(&expression.expr, &state.env, &mut state.macros).map_err(
            |error| error.with_position(expression.position.line, expression.position.column),
        )? {
            last_value = Value::Void;
            continue;
        }

        let expanded = state
            .expander
            .expand_expr(&expression.expr, &state.macros)
            .map_err(|error| {
                error.with_position(expression.position.line, expression.position.column)
            })?;

        let remaining = expressions.as_slice().to_vec();
        let value = if remaining.is_empty() {
            let _position_guard = push_position(context, Some(expression.position));
            eval_expr_in_env(&expanded, &state.env, context)
        } else {
            let frame = ContinuationFrame::Program(TopLevelState {
                remaining,
                env: state.env.clone(),
                macros: state.macros.clone(),
                expander: state.expander.clone(),
            });
            let _frame_guard = push_continuation_frame(context, frame);
            let _position_guard = push_position(context, Some(expression.position));
            eval_expr_in_env_single(&expanded, &state.env, context)
        }
        .map_err(|error| {
            error.with_position(expression.position.line, expression.position.column)
        })?;

        last_value = value;
    }

    Ok(last_value)
}

fn resume_continuation_frames(
    mut value: Value,
    mut frames: Vec<ContinuationFrame>,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    while let Some(frame) = frames.pop() {
        if continuations_are_enabled_in_current_level() {
            context.frames.borrow_mut().clone_from(&frames);
        }
        value = match frame {
            ContinuationFrame::Program(state) => {
                expect_single_value(value)?;
                eval_top_level_state(state, context)?
            }
            ContinuationFrame::ApplicationOperator { args, env } => {
                let procedure = expect_single_value(value)?;
                let args = eval_application_args(procedure.clone(), &args, &env, context)?;
                apply_procedure(procedure, args, context)?
            }
            ContinuationFrame::ApplicationArg {
                procedure,
                before,
                after,
                env,
            } => {
                let mut slots = before;
                slots.push(PendingArg::Value(expect_single_value(value)?));
                slots.extend(after);
                let args = eval_application_slots(procedure.clone(), &slots, &env, context)?;
                apply_procedure(procedure, args, context)?
            }
            ContinuationFrame::Sequence { remaining, env } => {
                expect_single_value(value)?;
                eval_sequence(&remaining, &env, context)?
            }
            ContinuationFrame::And { remaining, env } => {
                let value = expect_single_value(value)?;
                if value.is_truthy() {
                    eval_and_values(&remaining, &env, context)?
                } else {
                    value
                }
            }
            ContinuationFrame::Or { remaining, env } => {
                let value = expect_single_value(value)?;
                if value.is_truthy() {
                    value
                } else {
                    eval_or_values(&remaining, &env, context)?
                }
            }
            ContinuationFrame::CaseKey { clauses, env } => {
                let key = expect_single_value(value)?;
                eval_case_clauses(&key, &clauses, &env, context)?
            }
            ContinuationFrame::CondTest {
                body,
                remaining,
                env,
            } => {
                let value = expect_single_value(value)?;
                if value.is_truthy() {
                    if body.is_empty() {
                        value
                    } else {
                        eval_sequence(&body, &env, context)?
                    }
                } else {
                    eval_cond_clauses(&remaining, &env, context)?
                }
            }
            ContinuationFrame::GuardTest {
                exception,
                body,
                remaining,
                env,
            } => {
                let value = expect_single_value(value)?;
                if value.is_truthy() {
                    if body.is_empty() {
                        value
                    } else {
                        eval_sequence(&body, &env, context)?
                    }
                } else {
                    eval_guard_clauses(&exception, &remaining, &env, context)?
                }
            }
            ContinuationFrame::If {
                consequent,
                alternate,
                env,
            } => {
                let test = expect_single_value(value)?;
                if test.is_truthy() {
                    eval_expr_in_env(&consequent, &env, context)?
                } else if let Some(alternate) = alternate {
                    eval_expr_in_env(&alternate, &env, context)?
                } else {
                    Value::Void
                }
            }
            ContinuationFrame::Define { name, env } => {
                let value = expect_single_value(value)?;
                env.define(name, value);
                Value::Void
            }
            ContinuationFrame::Set { name, env } => {
                let value = expect_single_value(value)?;
                env.set(&name, value)?;
                Value::Void
            }
            ContinuationFrame::Let {
                name,
                evaluated,
                remaining,
                body,
                env,
            } => {
                let mut evaluated = evaluated;
                evaluated.push((name, expect_single_value(value)?));
                let let_env =
                    eval_parallel_let_bindings(evaluated, &remaining, &body, &env, context)?;
                eval_sequence(&body, &let_env, context)?
            }
            ContinuationFrame::NamedLet {
                name,
                params,
                evaluated,
                remaining,
                body,
                env,
            } => {
                let mut args = evaluated;
                args.push(expect_single_value(value)?);
                let args =
                    eval_named_let_args(&name, &params, args, &remaining, &body, &env, context)?;
                let (procedure, args) = build_named_let_call(name, params, body, &env, args);
                apply_procedure(procedure, args, context)?
            }
            ContinuationFrame::LetStar {
                name,
                let_env,
                remaining,
                body,
            } => {
                let_env.define(name, expect_single_value(value)?);
                let let_env = eval_let_star_bindings(&let_env, &remaining, &body, context)?;
                eval_sequence(&body, &let_env, context)?
            }
            ContinuationFrame::LetrecSequential {
                cell,
                remaining,
                body,
                letrec_env,
            } => {
                *cell.borrow_mut() = expect_single_value(value)?;
                eval_letrec_sequential_bindings(&remaining, &letrec_env, &body, context)?;
                eval_sequence(&body, &letrec_env, context)?
            }
            ContinuationFrame::LetrecParallel {
                cells,
                evaluated,
                remaining,
                body,
                letrec_env,
            } => {
                let mut values = evaluated;
                values.push(expect_single_value(value)?);
                let values = eval_letrec_parallel_bindings(
                    &cells,
                    values,
                    &remaining,
                    &body,
                    &letrec_env,
                    context,
                )?;
                for (cell, value) in cells.into_iter().zip(values) {
                    *cell.borrow_mut() = value;
                }
                eval_sequence(&body, &letrec_env, context)?
            }
            ContinuationFrame::MapCall {
                procedure,
                lists,
                index,
                results,
            } => {
                let mut results = results;
                results.push(expect_single_value(value)?);
                apply_map_from_index(procedure, &lists, index, results, context)?
            }
            ContinuationFrame::ForEachCall {
                procedure,
                lists,
                index,
            } => {
                expect_single_value(value)?;
                apply_for_each_from_index(procedure, &lists, index, context)?
            }
            ContinuationFrame::DynamicWindAfterIn { wind, body_thunk } => {
                expect_single_value(value)?;
                push_dynamic_wind(wind.clone(), context);
                let result = apply_procedure_with_frame(
                    body_thunk,
                    Vec::new(),
                    ContinuationFrame::DynamicWindAfterBody { wind: wind.clone() },
                    context,
                )?;
                pop_dynamic_wind(&wind, context);
                apply_procedure_with_frame_single(
                    wind.out_thunk.clone(),
                    Vec::new(),
                    ContinuationFrame::DynamicWindAfterOut {
                        result: result.clone(),
                    },
                    context,
                )?;
                result
            }
            ContinuationFrame::DynamicWindAfterBody { wind } => {
                pop_dynamic_wind(&wind, context);
                apply_procedure_with_frame_single(
                    wind.out_thunk.clone(),
                    Vec::new(),
                    ContinuationFrame::DynamicWindAfterOut {
                        result: value.clone(),
                    },
                    context,
                )?;
                value
            }
            ContinuationFrame::DynamicWindAfterOut { result } => {
                expect_single_value(value)?;
                result
            }
        };
    }

    if continuations_are_enabled_in_current_level() {
        context.frames.borrow_mut().clear();
    }
    Ok(value)
}

fn eval_application_args(
    procedure: Value,
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Vec<Value>, EvalError> {
    let slots = args
        .iter()
        .cloned()
        .map(PendingArg::Expr)
        .collect::<Vec<_>>();
    eval_application_slots(procedure, &slots, env, context)
}

fn eval_application_slots(
    procedure: Value,
    slots: &[PendingArg],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Vec<Value>, EvalError> {
    let mut evaluated = Vec::with_capacity(slots.len());

    for (index, slot) in slots.iter().enumerate() {
        let value = match slot {
            PendingArg::Value(value) => value.clone(),
            PendingArg::Expr(expr) => {
                let frame = ContinuationFrame::ApplicationArg {
                    procedure: procedure.clone(),
                    before: slots[..index].to_vec(),
                    after: slots[index + 1..].to_vec(),
                    env: env.clone(),
                };
                eval_expr_with_frame_single(expr, env, frame, context)?
            }
        };
        evaluated.push(value);
    }

    Ok(evaluated)
}

fn eval_and_values(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let Some((last, initial)) = args.split_last() else {
        return Ok(Value::Boolean(true));
    };

    for (index, arg) in initial.iter().enumerate() {
        let frame = ContinuationFrame::And {
            remaining: initial[index + 1..]
                .iter()
                .cloned()
                .chain(std::iter::once(last.clone()))
                .collect(),
            env: env.clone(),
        };
        let value = eval_expr_with_frame_single(arg, env, frame, context)?;
        if !value.is_truthy() {
            return Ok(value);
        }
    }

    eval_expr_in_env(last, env, context)
}

fn eval_or_values(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let Some((last, initial)) = args.split_last() else {
        return Ok(Value::Boolean(false));
    };

    for (index, arg) in initial.iter().enumerate() {
        let frame = ContinuationFrame::Or {
            remaining: initial[index + 1..]
                .iter()
                .cloned()
                .chain(std::iter::once(last.clone()))
                .collect(),
            env: env.clone(),
        };
        let value = eval_expr_with_frame_single(arg, env, frame, context)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    eval_expr_in_env(last, env, context)
}

fn eval_case_clauses(
    key: &Value,
    clauses: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::InvalidForm {
                name: "case",
                message: "expected clauses to be lists",
            });
        };

        let Some((head, body)) = items.split_first() else {
            return Err(EvalError::InvalidForm {
                name: "case",
                message: "expected each clause to contain a datum list or else",
            });
        };

        if matches!(head, Expr::Symbol(symbol) if symbol == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::InvalidForm {
                    name: "case",
                    message: "else clause must be last",
                });
            }

            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env, context)
            };
        }

        let Expr::List(datums) = head else {
            return Err(EvalError::InvalidForm {
                name: "case",
                message: "expected each clause to begin with a datum list or else",
            });
        };

        if datums
            .iter()
            .any(|datum| eq_values(key, &quote_expr(datum)))
        {
            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env, context)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_cond_clauses(
    clauses: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::InvalidForm {
                name: "cond",
                message: "expected clauses to be lists",
            });
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::InvalidForm {
                name: "cond",
                message: "expected each clause to contain a test",
            });
        };

        if matches!(test, Expr::Symbol(symbol) if symbol == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::InvalidForm {
                    name: "cond",
                    message: "else clause must be last",
                });
            }

            if body.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "cond",
                    message: "else clause must contain a body",
                });
            }

            return eval_sequence(body, env, context);
        }

        let frame = ContinuationFrame::CondTest {
            body: body.to_vec(),
            remaining: clauses[index + 1..].to_vec(),
            env: env.clone(),
        };
        let value = eval_expr_with_frame_single(test, env, frame, context)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(value)
            } else {
                eval_sequence(body, env, context)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_parallel_let_bindings(
    mut evaluated: Vec<(String, Value)>,
    remaining: &[(String, Expr)],
    body: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EnvRef, EvalError> {
    let mut remaining = remaining;

    while let Some((binding, rest)) = remaining.split_first() {
        let (name, expr) = binding;
        let frame = ContinuationFrame::Let {
            name: name.clone(),
            evaluated: evaluated.clone(),
            remaining: rest.to_vec(),
            body: body.to_vec(),
            env: env.clone(),
        };
        let value = eval_expr_with_frame_single(expr, env, frame, context)?;
        evaluated.push((name.clone(), value));
        remaining = rest;
    }

    let let_env = Env::new(Some(env.clone()));
    for (name, value) in evaluated {
        let let_env_name = name;
        let_env.define(let_env_name, value);
    }

    Ok(let_env)
}

fn eval_named_let_args(
    name: &str,
    params: &[String],
    mut evaluated: Vec<Value>,
    remaining: &[(String, Expr)],
    body: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Vec<Value>, EvalError> {
    let mut remaining = remaining;

    while let Some(((_, expr), rest)) = remaining.split_first() {
        let frame = ContinuationFrame::NamedLet {
            name: name.to_string(),
            params: params.to_vec(),
            evaluated: evaluated.clone(),
            remaining: rest.to_vec(),
            body: body.to_vec(),
            env: env.clone(),
        };
        let value = eval_expr_with_frame_single(expr, env, frame, context)?;
        evaluated.push(value);
        remaining = rest;
    }

    Ok(evaluated)
}

fn build_named_let_call(
    name: String,
    params: Vec<String>,
    body: Vec<Expr>,
    env: &EnvRef,
    args: Vec<Value>,
) -> (Value, Vec<Value>) {
    let let_env = Env::new(Some(env.clone()));
    let_env.define(name.clone(), Value::Void);

    let procedure = Rc::new(Procedure::Lambda {
        params: LambdaParams::fixed(params),
        body,
        env: let_env.clone(),
    });
    let value = Value::Procedure(procedure.clone());
    let_env.define(name, value.clone());

    (value, args)
}

fn eval_let_star_bindings(
    let_env: &EnvRef,
    remaining: &[(String, Expr)],
    body: &[Expr],
    context: &mut EvalContext,
) -> Result<EnvRef, EvalError> {
    let mut remaining = remaining;

    while let Some((binding, rest)) = remaining.split_first() {
        let (name, init) = binding;
        let frame = ContinuationFrame::LetStar {
            name: name.clone(),
            let_env: let_env.clone(),
            remaining: rest.to_vec(),
            body: body.to_vec(),
        };
        let value = eval_expr_with_frame_single(init, let_env, frame, context)?;
        let_env.define(name.clone(), value);
        remaining = rest;
    }

    Ok(let_env.clone())
}

fn eval_letrec_sequential_bindings(
    remaining: &[(String, Expr)],
    letrec_env: &EnvRef,
    body: &[Expr],
    context: &mut EvalContext,
) -> Result<(), EvalError> {
    let mut remaining = remaining;

    while let Some((binding, rest)) = remaining.split_first() {
        let (name, init) = binding;
        let cell = Rc::new(RefCell::new(Value::Uninitialized));
        letrec_env.define_cell(name.clone(), cell.clone());
        let frame = ContinuationFrame::LetrecSequential {
            cell: cell.clone(),
            remaining: rest.to_vec(),
            body: body.to_vec(),
            letrec_env: letrec_env.clone(),
        };
        let value = eval_expr_with_frame_single(init, letrec_env, frame, context)?;
        *cell.borrow_mut() = value;
        remaining = rest;
    }

    Ok(())
}

fn eval_letrec_parallel_bindings(
    cells: &[Rc<RefCell<Value>>],
    mut evaluated: Vec<Value>,
    remaining: &[Expr],
    body: &[Expr],
    letrec_env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Vec<Value>, EvalError> {
    let mut remaining = remaining;

    while let Some((init, rest)) = remaining.split_first() {
        let frame = ContinuationFrame::LetrecParallel {
            cells: cells.to_vec(),
            evaluated: evaluated.clone(),
            remaining: rest.to_vec(),
            body: body.to_vec(),
            letrec_env: letrec_env.clone(),
        };
        let value = eval_expr_with_frame_single(init, letrec_env, frame, context)?;
        evaluated.push(value);
        remaining = rest;
    }

    Ok(evaluated)
}

fn apply_map_from_index(
    procedure: Value,
    lists: &[Vec<Value>],
    mut index: usize,
    mut results: Vec<Value>,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let length = lists.iter().map(|list| list.len()).min().unwrap_or(0);

    while index < length {
        let call_args = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        let frame = ContinuationFrame::MapCall {
            procedure: procedure.clone(),
            lists: lists.to_vec(),
            index: index + 1,
            results: results.clone(),
        };
        let value =
            apply_procedure_with_frame_single(procedure.clone(), call_args, frame, context)?;
        results.push(value);
        index += 1;
    }

    Ok(list_from_vec(results))
}

fn apply_for_each_from_index(
    procedure: Value,
    lists: &[Vec<Value>],
    mut index: usize,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let length = lists.iter().map(|list| list.len()).min().unwrap_or(0);

    while index < length {
        let call_args = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        let frame = ContinuationFrame::ForEachCall {
            procedure: procedure.clone(),
            lists: lists.to_vec(),
            index: index + 1,
        };
        apply_procedure_with_frame_single(procedure.clone(), call_args, frame, context)?;
        index += 1;
    }

    Ok(Value::Void)
}

fn eval_expr_step(
    expr: Expr,
    env: EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    match expr {
        Expr::Number(value) => Ok(EvalStep::Value(Value::Number(value))),
        Expr::Boolean(value) => Ok(EvalStep::Value(Value::Boolean(value))),
        Expr::Char(value) => Ok(EvalStep::Value(Value::Char(value))),
        Expr::String(value) => Ok(EvalStep::Value(Value::String(SchemeString::new_immutable(
            value,
        )))),
        Expr::Symbol(name) => {
            let value = env
                .get(&name)
                .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() })?;

            if matches!(value, Value::Uninitialized) {
                Err(EvalError::UninitializedBinding { name })
            } else {
                Ok(EvalStep::Value(value))
            }
        }
        Expr::List(items) => eval_application_step(items, env, context),
    }
}

fn eval_application_step(
    items: Vec<Expr>,
    env: EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::NotAProcedure);
    };

    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_and_step(tail, &env, context),
            "begin" => return eval_begin_step(tail, &env),
            "case" => return eval_case_step(tail, &env, context),
            "case-lambda" => return eval_case_lambda_step(tail, &env),
            "cond" => return eval_cond_step(tail, &env, context),
            "define" => return eval_define_step(tail, &env, context),
            "define-record-type" => return eval_define_record_type_step(tail, &env),
            "do" => return eval_do_step(tail, &env, context),
            "guard" => return eval_guard_step(tail, &env, context),
            "if" => return eval_if_step(tail, &env, context),
            "lambda" => return eval_lambda_step(tail, &env),
            "let" => return eval_let_step(tail, &env, context),
            "let*" => return eval_let_star_step(tail, &env, context),
            "letrec" => return eval_letrec_step(tail, &env, context, false),
            "letrec*" => return eval_letrec_step(tail, &env, context, true),
            "or" => return eval_or_step(tail, &env, context),
            "quote" => return eval_quote_step(tail),
            "set!" => return eval_set_step(tail, &env, context),
            "syntax" => return eval_syntax_step(tail, &env),
            "syntax-case" => return eval_syntax_case_step(tail, &env, context),
            "with-syntax" => return eval_with_syntax_step(tail, &env, context),
            _ => {}
        }
    }

    let procedure = eval_expr_with_frame(
        head,
        &env,
        ContinuationFrame::ApplicationOperator {
            args: tail.to_vec(),
            env: env.clone(),
        },
        context,
    )
    .and_then(expect_single_value)?;
    let args = eval_application_args(procedure.clone(), tail, &env, context)?;

    Ok(EvalStep::Apply(procedure, args))
}

fn eval_sequence(
    expressions: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    resolve_eval_step(
        EvalStep::Sequence(expressions.to_vec(), env.clone()),
        context,
    )
}

fn eval_sequence_step(
    expressions: Vec<Expr>,
    env: EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    let mut remaining = expressions.as_slice();

    while let Some((expression, rest)) = remaining.split_first() {
        if rest.is_empty() {
            return Ok(EvalStep::Expr(expression.clone(), env));
        }

        eval_expr_with_frame(
            expression,
            &env,
            ContinuationFrame::Sequence {
                remaining: rest.to_vec(),
                env: env.clone(),
            },
            context,
        )
        .and_then(expect_single_value)?;
        remaining = rest;
    }

    Ok(EvalStep::Value(Value::Void))
}

fn eval_and_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    let Some((last, initial)) = args.split_last() else {
        return Ok(EvalStep::Value(Value::Boolean(true)));
    };

    for (index, arg) in initial.iter().enumerate() {
        let frame = ContinuationFrame::And {
            remaining: initial[index + 1..]
                .iter()
                .cloned()
                .chain(std::iter::once(last.clone()))
                .collect(),
            env: env.clone(),
        };
        let result = eval_expr_with_frame_single(arg, env, frame, context)?;
        if !result.is_truthy() {
            return Ok(EvalStep::Value(result));
        }
    }

    Ok(EvalStep::Expr(last.clone(), env.clone()))
}

fn eval_or_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    let Some((last, initial)) = args.split_last() else {
        return Ok(EvalStep::Value(Value::Boolean(false)));
    };

    for (index, arg) in initial.iter().enumerate() {
        let frame = ContinuationFrame::Or {
            remaining: initial[index + 1..]
                .iter()
                .cloned()
                .chain(std::iter::once(last.clone()))
                .collect(),
            env: env.clone(),
        };
        let value = eval_expr_with_frame_single(arg, env, frame, context)?;
        if value.is_truthy() {
            return Ok(EvalStep::Value(value));
        }
    }

    Ok(EvalStep::Expr(last.clone(), env.clone()))
}

fn eval_begin_step(args: &[Expr], env: &EnvRef) -> Result<EvalStep, EvalError> {
    Ok(EvalStep::Sequence(args.to_vec(), env.clone()))
}

fn eval_case_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    let Some((key_expr, clauses)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "case",
            expected: "a key and at least 1 clause",
            got: 0,
        });
    };

    let key = eval_expr_with_frame(
        key_expr,
        env,
        ContinuationFrame::CaseKey {
            clauses: clauses.to_vec(),
            env: env.clone(),
        },
        context,
    )
    .and_then(expect_single_value)?;

    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::InvalidForm {
                name: "case",
                message: "expected clauses to be lists",
            });
        };

        let Some((head, body)) = items.split_first() else {
            return Err(EvalError::InvalidForm {
                name: "case",
                message: "expected each clause to contain a datum list or else",
            });
        };

        if matches!(head, Expr::Symbol(symbol) if symbol == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::InvalidForm {
                    name: "case",
                    message: "else clause must be last",
                });
            }

            return if body.is_empty() {
                Ok(EvalStep::Value(Value::Void))
            } else {
                Ok(EvalStep::Sequence(body.to_vec(), env.clone()))
            };
        }

        let Expr::List(datums) = head else {
            return Err(EvalError::InvalidForm {
                name: "case",
                message: "expected each clause to begin with a datum list or else",
            });
        };

        if datums
            .iter()
            .any(|datum| eq_values(&key, &quote_expr(datum)))
        {
            return if body.is_empty() {
                Ok(EvalStep::Value(Value::Void))
            } else {
                Ok(EvalStep::Sequence(body.to_vec(), env.clone()))
            };
        }
    }

    Ok(EvalStep::Value(Value::Void))
}

fn eval_cond_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::InvalidForm {
                name: "cond",
                message: "expected clauses to be lists",
            });
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::InvalidForm {
                name: "cond",
                message: "expected each clause to contain a test",
            });
        };

        if matches!(test, Expr::Symbol(symbol) if symbol == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::InvalidForm {
                    name: "cond",
                    message: "else clause must be last",
                });
            }

            if body.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "cond",
                    message: "else clause must contain a body",
                });
            }

            return Ok(EvalStep::Sequence(body.to_vec(), env.clone()));
        }

        let frame = ContinuationFrame::CondTest {
            body: body.to_vec(),
            remaining: args[index + 1..].to_vec(),
            env: env.clone(),
        };
        let value = eval_expr_with_frame_single(test, env, frame, context)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(EvalStep::Value(value))
            } else {
                Ok(EvalStep::Sequence(body.to_vec(), env.clone()))
            };
        }
    }

    Ok(EvalStep::Value(Value::Void))
}

fn eval_guard_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    let Some((spec, body)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "guard",
            expected: "a guard spec and at least 1 body expression",
            got: 0,
        });
    };

    if body.is_empty() {
        return Err(EvalError::InvalidForm {
            name: "guard",
            message: "expected at least one body expression",
        });
    }

    let Expr::List(spec_items) = spec else {
        return Err(EvalError::InvalidForm {
            name: "guard",
            message: "expected a guard variable and clauses",
        });
    };

    let Some((variable_expr, clauses)) = spec_items.split_first() else {
        return Err(EvalError::InvalidForm {
            name: "guard",
            message: "expected a guard variable",
        });
    };

    let Expr::Symbol(variable) = variable_expr else {
        return Err(EvalError::InvalidForm {
            name: "guard",
            message: "expected the guard variable to be a symbol",
        });
    };

    let handler = Rc::new(ExceptionHandler {
        kind: ExceptionHandlerKind::Guard {
            variable: variable.clone(),
            clauses: clauses.to_vec(),
            env: env.clone(),
        },
        frames: context.frames.borrow().clone(),
        winds: context.dynamic_winds.borrow().clone(),
        outer_handlers: context.exception_handlers.borrow().clone(),
        position: current_position(context),
    });

    let _handler_guard = push_exception_handler(context, handler);
    let result = eval_sequence(body, env, context)?;
    Ok(EvalStep::Value(result))
}

fn eval_guard_handler(
    exception: Value,
    variable: &str,
    clauses: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let guard_env = Env::new(Some(env.clone()));
    guard_env.define(variable.to_string(), exception.clone());
    eval_guard_clauses(&exception, clauses, &guard_env, context)
}

fn eval_guard_clauses(
    exception: &Value,
    clauses: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::InvalidForm {
                name: "guard",
                message: "expected clauses to be lists",
            });
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::InvalidForm {
                name: "guard",
                message: "expected each clause to contain a test",
            });
        };

        if matches!(test, Expr::Symbol(symbol) if symbol == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::InvalidForm {
                    name: "guard",
                    message: "else clause must be last",
                });
            }

            if body.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "guard",
                    message: "else clause must contain a body",
                });
            }

            return eval_sequence(body, env, context);
        }

        let frame = ContinuationFrame::GuardTest {
            exception: exception.clone(),
            body: body.to_vec(),
            remaining: clauses[index + 1..].to_vec(),
            env: env.clone(),
        };
        let value = eval_expr_with_frame_single(test, env, frame, context)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(value)
            } else {
                eval_sequence(body, env, context)
            };
        }
    }

    signal_exception(
        exception.clone(),
        context.exception_handlers.borrow().clone(),
        current_position(context),
    )
}

fn eval_define_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    let Some((target, rest)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "define",
            expected: "a binding target and value",
            got: 0,
        });
    };

    match target {
        Expr::Symbol(name) => {
            if rest.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "define",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let value = eval_expr_with_frame(
                &rest[0],
                env,
                ContinuationFrame::Define {
                    name: name.clone(),
                    env: env.clone(),
                },
                context,
            )
            .and_then(expect_single_value)?;
            env.define(name.clone(), value);
            Ok(EvalStep::Value(Value::Void))
        }
        Expr::List(signature) => {
            let Some((name_expr, params_exprs)) = signature.split_first() else {
                return Err(EvalError::InvalidForm {
                    name: "define",
                    message: "expected a function name",
                });
            };

            let Expr::Symbol(name) = name_expr else {
                return Err(EvalError::InvalidForm {
                    name: "define",
                    message: "expected a function name",
                });
            };

            if rest.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "define",
                    message: "expected at least one body expression",
                });
            }

            let params = parse_param_names(params_exprs, "define")?;
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
                params,
                body: rest.to_vec(),
                env: env.clone(),
            }));

            env.define(name.clone(), procedure);
            Ok(EvalStep::Value(Value::Void))
        }
        _ => Err(EvalError::InvalidForm {
            name: "define",
            message: "expected a symbol or function signature",
        }),
    }
}

fn eval_define_record_type_step(args: &[Expr], env: &EnvRef) -> Result<EvalStep, EvalError> {
    if args.len() < 3 {
        return Err(EvalError::WrongArgCount {
            name: "define-record-type",
            expected: "a record name, constructor, predicate, and field specifications",
            got: args.len(),
        });
    }

    let Expr::Symbol(record_name) = &args[0] else {
        return Err(EvalError::InvalidForm {
            name: "define-record-type",
            message: "expected a record type name",
        });
    };

    let constructor = parse_record_constructor(&args[1])?;

    let Expr::Symbol(predicate_name) = &args[2] else {
        return Err(EvalError::InvalidForm {
            name: "define-record-type",
            message: "expected a predicate name",
        });
    };

    let field_specs = args[3..]
        .iter()
        .map(parse_record_field_spec)
        .collect::<Result<Vec<_>, EvalError>>()?;

    let mut field_indices = HashMap::new();
    for (index, field_spec) in field_specs.iter().enumerate() {
        if field_indices
            .insert(field_spec.name.clone(), index)
            .is_some()
        {
            return Err(EvalError::InvalidForm {
                name: "define-record-type",
                message: "expected unique field names",
            });
        }
    }

    let constructor_field_indices = constructor
        .fields
        .iter()
        .map(|field_name| {
            field_indices
                .get(field_name)
                .copied()
                .ok_or(EvalError::InvalidForm {
                    name: "define-record-type",
                    message: "constructor fields must match the declared record fields",
                })
        })
        .collect::<Result<Vec<_>, EvalError>>()?;

    let record_type = Rc::new(RecordType {
        name: record_name.clone(),
        field_count: field_specs.len(),
    });

    env.define(
        constructor.name.clone(),
        Value::Procedure(Rc::new(Procedure::RecordConstructor {
            name: constructor.name,
            record_type: record_type.clone(),
            field_indices: constructor_field_indices,
        })),
    );
    env.define(
        predicate_name.clone(),
        Value::Procedure(Rc::new(Procedure::RecordPredicate {
            name: predicate_name.clone(),
            record_type: record_type.clone(),
        })),
    );

    for (index, field_spec) in field_specs.iter().enumerate() {
        env.define(
            field_spec.accessor.clone(),
            Value::Procedure(Rc::new(Procedure::RecordAccessor {
                name: field_spec.accessor.clone(),
                record_type: record_type.clone(),
                field_index: index,
            })),
        );
    }

    Ok(EvalStep::Value(Value::Void))
}

fn eval_if_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    if !(2..=3).contains(&args.len()) {
        return Err(EvalError::WrongArgCount {
            name: "if",
            expected: "2 or 3 arguments",
            got: args.len(),
        });
    }

    let test = eval_expr_with_frame(
        &args[0],
        env,
        ContinuationFrame::If {
            consequent: args[1].clone(),
            alternate: args.get(2).cloned(),
            env: env.clone(),
        },
        context,
    )
    .and_then(expect_single_value)?;

    if test.is_truthy() {
        Ok(EvalStep::Expr(args[1].clone(), env.clone()))
    } else if args.len() == 3 {
        Ok(EvalStep::Expr(args[2].clone(), env.clone()))
    } else {
        Ok(EvalStep::Value(Value::Void))
    }
}

fn eval_set_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "set!",
            expected: "exactly 2 arguments",
            got: args.len(),
        });
    }

    let Expr::Symbol(name) = &args[0] else {
        return Err(EvalError::InvalidForm {
            name: "set!",
            message: "expected a symbol as the binding target",
        });
    };

    let value = eval_expr_with_frame(
        &args[1],
        env,
        ContinuationFrame::Set {
            name: name.clone(),
            env: env.clone(),
        },
        context,
    )
    .and_then(expect_single_value)?;
    env.set(name, value)?;
    Ok(EvalStep::Value(Value::Void))
}

fn eval_lambda_step(args: &[Expr], env: &EnvRef) -> Result<EvalStep, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "lambda",
            expected: "a parameter list and body",
            got: 0,
        });
    };

    if body.is_empty() {
        return Err(EvalError::InvalidForm {
            name: "lambda",
            message: "expected at least one body expression",
        });
    }

    let params = parse_params(params_expr, "lambda")?;
    Ok(EvalStep::Value(Value::Procedure(Rc::new(
        Procedure::Lambda {
            params,
            body: body.to_vec(),
            env: env.clone(),
        },
    ))))
}

fn eval_case_lambda_step(args: &[Expr], env: &EnvRef) -> Result<EvalStep, EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "case-lambda",
            expected: "at least 1 clause",
            got: 0,
        });
    }

    let clauses = args
        .iter()
        .map(parse_case_lambda_clause)
        .collect::<Result<Vec<_>, EvalError>>()?;

    Ok(EvalStep::Value(Value::Procedure(Rc::new(
        Procedure::CaseLambda {
            clauses,
            env: env.clone(),
        },
    ))))
}

fn eval_let_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "let",
            expected: "bindings and at least one body expression",
            got: 0,
        });
    };

    match first {
        Expr::List(_) => {
            if rest.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "let",
                    message: "expected at least one body expression",
                });
            }

            let bindings = parse_let_bindings(first, "let")?;
            let let_env = eval_parallel_let_bindings(Vec::new(), &bindings, rest, env, context)?;

            Ok(EvalStep::Sequence(rest.to_vec(), let_env))
        }
        Expr::Symbol(name) => {
            let Some((bindings_expr, body)) = rest.split_first() else {
                return Err(EvalError::InvalidForm {
                    name: "let",
                    message: "expected bindings and at least one body expression",
                });
            };

            if body.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "let",
                    message: "expected at least one body expression",
                });
            }

            let bindings = parse_let_bindings(bindings_expr, "let")?;
            let params = bindings
                .iter()
                .map(|(binding_name, _)| binding_name.clone())
                .collect::<Vec<_>>();
            let args =
                eval_named_let_args(name, &params, Vec::new(), &bindings, body, env, context)?;
            let (value, args) =
                build_named_let_call(name.clone(), params, body.to_vec(), env, args);

            Ok(EvalStep::Apply(value, args))
        }
        _ => Err(EvalError::InvalidForm {
            name: "let",
            message: "expected a binding list or let name",
        }),
    }
}

fn eval_let_star_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    let Some((bindings_expr, body)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "let*",
            expected: "bindings and at least one body expression",
            got: 0,
        });
    };

    if body.is_empty() {
        return Err(EvalError::InvalidForm {
            name: "let*",
            message: "expected at least one body expression",
        });
    }

    let bindings = parse_let_bindings(bindings_expr, "let*")?;
    let let_star_env = Env::new(Some(env.clone()));
    let let_star_env = eval_let_star_bindings(&let_star_env, &bindings, body, context)?;

    Ok(EvalStep::Sequence(body.to_vec(), let_star_env))
}

fn eval_letrec(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
    sequential: bool,
) -> Result<Value, EvalError> {
    resolve_eval_step(eval_letrec_step(args, env, context, sequential)?, context)
}

fn eval_letrec_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
    sequential: bool,
) -> Result<EvalStep, EvalError> {
    let form_name = if sequential { "letrec*" } else { "letrec" };
    let Some((bindings_expr, body)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: form_name,
            expected: "bindings and at least one body expression",
            got: 0,
        });
    };

    if body.is_empty() {
        return Err(EvalError::InvalidForm {
            name: form_name,
            message: "expected at least one body expression",
        });
    }

    let bindings = parse_let_bindings(bindings_expr, form_name)?;
    let letrec_env = Env::new(Some(env.clone()));

    if sequential {
        eval_letrec_sequential_bindings(&bindings, &letrec_env, body, context)?;
    } else {
        let mut cells = Vec::with_capacity(bindings.len());
        for (name, _) in &bindings {
            let cell = Rc::new(RefCell::new(Value::Uninitialized));
            letrec_env.define_cell(name.clone(), cell.clone());
            cells.push(cell);
        }

        let init_exprs = bindings
            .iter()
            .map(|(_, init)| init.clone())
            .collect::<Vec<_>>();
        let values = eval_letrec_parallel_bindings(
            &cells,
            Vec::new(),
            &init_exprs,
            body,
            &letrec_env,
            context,
        )?;

        for (cell, value) in cells.into_iter().zip(values) {
            *cell.borrow_mut() = value;
        }
    }

    Ok(EvalStep::Sequence(body.to_vec(), letrec_env))
}

fn eval_do_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "do",
            expected: "bindings and a termination clause",
            got: args.len(),
        });
    }

    let bindings = parse_do_bindings(&args[0])?;
    let (test, result_exprs) = parse_do_termination_clause(&args[1])?;
    let body = &args[2..];

    let init_values = bindings
        .iter()
        .map(|binding| eval_expr_in_env_single(&binding.init, env, context))
        .collect::<Result<Vec<_>, EvalError>>()?;

    let do_env = Env::new(Some(env.clone()));
    let mut cells = Vec::with_capacity(bindings.len());
    for (binding, value) in bindings.iter().zip(init_values) {
        let cell = Rc::new(RefCell::new(value));
        do_env.define_cell(binding.name.clone(), cell.clone());
        cells.push(cell);
    }

    loop {
        if eval_expr_in_env_single(&test, &do_env, context)?.is_truthy() {
            return if result_exprs.is_empty() {
                Ok(EvalStep::Value(Value::Void))
            } else {
                Ok(EvalStep::Sequence(result_exprs.clone(), do_env.clone()))
            };
        }

        expect_single_value(eval_sequence(body, &do_env, context)?)?;

        let updates = bindings
            .iter()
            .zip(cells.iter())
            .filter_map(|(binding, cell)| {
                binding.step.as_ref().map(|step| {
                    eval_expr_in_env_single(step, &do_env, context)
                        .map(|value| (cell.clone(), value))
                })
            })
            .collect::<Result<Vec<_>, EvalError>>()?;

        for (cell, value) in updates {
            *cell.borrow_mut() = value;
        }
    }
}

fn eval_quote_step(args: &[Expr]) -> Result<EvalStep, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "quote",
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    Ok(EvalStep::Value(quote_expr(&args[0])))
}

fn eval_syntax_step(args: &[Expr], env: &EnvRef) -> Result<EvalStep, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "syntax",
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    Ok(EvalStep::Value(Value::Syntax(Rc::new(
        build_syntax_from_template(&args[0], env, None, "syntax")?,
    ))))
}

fn eval_syntax_case_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    if args.len() < 3 {
        return Err(EvalError::WrongArgCount {
            name: "syntax-case",
            expected: "an expression, literal identifiers, and at least 1 clause",
            got: args.len(),
        });
    }

    let target = eval_expr_in_env_single(&args[0], env, context)?;
    let target = expect_syntax("syntax-case", &target)?;
    let target_expr = expr_from_syntax(target.as_ref().clone());
    let literals = parse_syntax_literals(&args[1])?;

    for clause in &args[2..] {
        let Expr::List(items) = clause else {
            return Err(EvalError::InvalidForm {
                name: "syntax-case",
                message: "expected clauses to be lists",
            });
        };

        if items.len() < 2 {
            return Err(EvalError::InvalidForm {
                name: "syntax-case",
                message: "expected each clause to contain a pattern and body",
            });
        }

        let Some(bindings) = match_syntax_pattern(&items[0], &target_expr, &literals) else {
            continue;
        };

        let clause_env = Env::new(Some(env.clone()));
        bind_syntax_pattern_bindings(&clause_env, bindings);

        let body_index = if items.len() == 2 {
            1
        } else {
            let fender = eval_expr_in_env_single(&items[1], &clause_env, context)?;
            if !fender.is_truthy() {
                continue;
            }
            2
        };

        if body_index >= items.len() {
            return Err(EvalError::InvalidForm {
                name: "syntax-case",
                message: "expected each clause to contain a body expression",
            });
        }

        let result = expect_single_value(eval_sequence(&items[body_index..], &clause_env, context)?)?;
        let syntax = expect_syntax("syntax-case", &result)?;
        return Ok(EvalStep::Value(Value::Syntax(syntax)));
    }

    Err(EvalError::NoMatchingSyntaxCase)
}

fn eval_with_syntax_step(
    args: &[Expr],
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    let Some((bindings_expr, body)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "with-syntax",
            expected: "bindings and at least 1 body expression",
            got: 0,
        });
    };

    if body.is_empty() {
        return Err(EvalError::InvalidForm {
            name: "with-syntax",
            message: "expected at least one body expression",
        });
    }

    let Expr::List(bindings) = bindings_expr else {
        return Err(EvalError::InvalidForm {
            name: "with-syntax",
            message: "expected a binding list",
        });
    };

    let evaluated_bindings = bindings
        .iter()
        .map(|binding| {
            let Expr::List(items) = binding else {
                return Err(EvalError::InvalidForm {
                    name: "with-syntax",
                    message: "expected each binding to be a list",
                });
            };

            if items.len() != 2 {
                return Err(EvalError::InvalidForm {
                    name: "with-syntax",
                    message: "expected each binding to contain a pattern and expression",
                });
            }

            let produced = eval_expr_in_env_single(&items[1], env, context)?;
            let produced = expect_syntax("with-syntax", &produced)?;
            Ok((items[0].clone(), expr_from_syntax(produced.as_ref().clone())))
        })
        .collect::<Result<Vec<_>, EvalError>>()?;

    let body_env = Env::new(Some(env.clone()));
    let empty_literals = HashSet::new();
    for (pattern, produced) in evaluated_bindings {
        let Some(bindings) = match_syntax_pattern(&pattern, &produced, &empty_literals) else {
            return Err(EvalError::InvalidForm {
                name: "with-syntax",
                message: "binding pattern did not match the produced syntax",
            });
        };
        bind_syntax_pattern_bindings(&body_env, bindings);
    }

    Ok(EvalStep::Value(eval_sequence(body, &body_env, context)?))
}

fn parse_syntax_literals(expr: &Expr) -> Result<HashSet<String>, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::InvalidForm {
            name: "syntax-case",
            message: "expected a literal identifier list",
        });
    };

    items
        .iter()
        .map(|item| match item {
            Expr::Symbol(symbol) => Ok(symbol.clone()),
            _ => Err(EvalError::InvalidForm {
                name: "syntax-case",
                message: "expected literal identifiers to be symbols",
            }),
        })
        .collect()
}

fn bind_syntax_pattern_bindings(env: &EnvRef, bindings: HashMap<String, PatternBinding>) {
    for (name, binding) in bindings {
        match binding {
            PatternBinding::Single(expr) => {
                env.define(name, Value::Syntax(Rc::new(syntax_from_use_expr(&expr))));
            }
            PatternBinding::Repeated(values) => {
                env.define(
                    name,
                    Value::SyntaxList(
                        values
                            .into_iter()
                            .map(|expr| Rc::new(syntax_from_use_expr(&expr)))
                            .collect(),
                    ),
                );
            }
        }
    }
}

fn build_syntax_from_template(
    template: &Expr,
    env: &EnvRef,
    repetition_index: Option<usize>,
    name: &'static str,
) -> Result<SyntaxExpr, EvalError> {
    Ok(match template {
        Expr::Number(value) => SyntaxExpr::Number(value.clone()),
        Expr::Boolean(value) => SyntaxExpr::Boolean(*value),
        Expr::Char(value) => SyntaxExpr::Char(*value),
        Expr::String(value) => SyntaxExpr::String(value.clone()),
        Expr::Symbol(symbol) => match env.get(symbol) {
            Some(Value::Syntax(syntax)) => syntax.as_ref().clone(),
            Some(Value::SyntaxList(values)) => {
                let Some(index) = repetition_index else {
                    return Err(EvalError::InvalidMacroTemplate {
                        name: name.to_string(),
                        message: "ellipsis variables must appear under ellipsis in templates",
                    });
                };

                let Some(value) = values.get(index) else {
                    return Err(EvalError::InvalidMacroTemplate {
                        name: name.to_string(),
                        message: "ellipsis repetitions had inconsistent lengths",
                    });
                };

                value.as_ref().clone()
            }
            _ => SyntaxExpr::Symbol(SyntaxSymbol {
                name: symbol.clone(),
                origin: SymbolOrigin::Template,
            }),
        },
        Expr::List(items) => {
            let mut expanded = Vec::new();
            let mut index = 0;

            while index < items.len() {
                if matches!(items.get(index + 1), Some(Expr::Symbol(symbol)) if symbol == "...") {
                    let repeat_count = syntax_repetition_count(&items[index], env, name)?;
                    for repeated_index in 0..repeat_count {
                        expanded.push(build_syntax_from_template(
                            &items[index],
                            env,
                            Some(repeated_index),
                            name,
                        )?);
                    }
                    index += 2;
                } else {
                    expanded.push(build_syntax_from_template(
                        &items[index],
                        env,
                        repetition_index,
                        name,
                    )?);
                    index += 1;
                }
            }

            SyntaxExpr::List(expanded)
        }
    })
}

fn syntax_repetition_count(
    template: &Expr,
    env: &EnvRef,
    name: &'static str,
) -> Result<usize, EvalError> {
    let mut repeated = Vec::new();
    collect_repeated_syntax_bindings(template, env, &mut repeated);

    let Some((first, rest)) = repeated.split_first() else {
        return Err(EvalError::InvalidMacroTemplate {
            name: name.to_string(),
            message: "ellipsis must repeat at least one pattern variable",
        });
    };

    if rest.iter().any(|count| *count != *first) {
        return Err(EvalError::InvalidMacroTemplate {
            name: name.to_string(),
            message: "ellipsis repetitions had inconsistent lengths",
        });
    }

    Ok(*first)
}

fn collect_repeated_syntax_bindings(template: &Expr, env: &EnvRef, repeated: &mut Vec<usize>) {
    match template {
        Expr::Symbol(symbol) => {
            if let Some(Value::SyntaxList(values)) = env.get(symbol) {
                repeated.push(values.len());
            }
        }
        Expr::List(items) => {
            for item in items {
                collect_repeated_syntax_bindings(item, env, repeated);
            }
        }
        _ => {}
    }
}

fn syntax_origin(expr: &SyntaxExpr) -> SymbolOrigin {
    match expr {
        SyntaxExpr::Symbol(symbol) => symbol.origin,
        SyntaxExpr::List(items) => {
            if items
                .iter()
                .any(|item| syntax_origin(item) == SymbolOrigin::UseSite)
            {
                SymbolOrigin::UseSite
            } else {
                SymbolOrigin::Template
            }
        }
        _ => SymbolOrigin::Template,
    }
}

fn syntax_to_datum_value(expr: &SyntaxExpr) -> Value {
    quote_expr(&expr_from_syntax(expr.clone()))
}

fn datum_to_syntax(
    value: &Value,
    origin: SymbolOrigin,
    name: &'static str,
) -> Result<SyntaxExpr, EvalError> {
    match value {
        Value::Number(number) => Ok(SyntaxExpr::Number(number.clone())),
        Value::Boolean(boolean) => Ok(SyntaxExpr::Boolean(*boolean)),
        Value::Char(ch) => Ok(SyntaxExpr::Char(*ch)),
        Value::String(string) => Ok(SyntaxExpr::String(string.to_plain_string())),
        Value::Symbol(symbol) => Ok(SyntaxExpr::Symbol(SyntaxSymbol {
            name: symbol.clone(),
            origin,
        })),
        Value::List(_) | Value::Pair(_) => Ok(SyntaxExpr::List(
            collect_list(name, value)?
                .into_iter()
                .map(|item| datum_to_syntax(&item, origin, name))
                .collect::<Result<Vec<_>, EvalError>>()?,
        )),
        Value::Syntax(syntax) => Ok(syntax.as_ref().clone()),
        _ => Err(EvalError::InvalidArgument {
            name,
            message: "expected a datum that can be converted to syntax",
        }),
    }
}

fn parse_params(expr: &Expr, name: &'static str) -> Result<LambdaParams, EvalError> {
    match expr {
        Expr::List(items) => parse_param_names(items, name),
        Expr::Symbol(symbol) => Ok(LambdaParams {
            required: Vec::new(),
            rest: Some(symbol.clone()),
        }),
        _ => Err(EvalError::InvalidForm {
            name,
            message: "expected a parameter list",
        }),
    }
}

fn parse_case_lambda_clause(expr: &Expr) -> Result<LambdaClause, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::InvalidForm {
            name: "case-lambda",
            message: "expected each clause to be a list",
        });
    };

    let Some((params_expr, body)) = items.split_first() else {
        return Err(EvalError::InvalidForm {
            name: "case-lambda",
            message: "expected each clause to contain parameters and a body",
        });
    };

    if body.is_empty() {
        return Err(EvalError::InvalidForm {
            name: "case-lambda",
            message: "expected each clause to contain at least one body expression",
        });
    }

    Ok(LambdaClause {
        params: parse_params(params_expr, "case-lambda")?,
        body: body.to_vec(),
    })
}

fn parse_record_constructor(expr: &Expr) -> Result<RecordConstructorSpec, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::InvalidForm {
            name: "define-record-type",
            message: "expected a constructor specification",
        });
    };

    let Some((name_expr, field_exprs)) = items.split_first() else {
        return Err(EvalError::InvalidForm {
            name: "define-record-type",
            message: "expected a constructor name",
        });
    };

    let Expr::Symbol(name) = name_expr else {
        return Err(EvalError::InvalidForm {
            name: "define-record-type",
            message: "expected a constructor name",
        });
    };

    let fields = field_exprs
        .iter()
        .map(|expr| match expr {
            Expr::Symbol(field_name) => Ok(field_name.clone()),
            _ => Err(EvalError::InvalidForm {
                name: "define-record-type",
                message: "expected constructor fields to be symbols",
            }),
        })
        .collect::<Result<Vec<_>, EvalError>>()?;

    Ok(RecordConstructorSpec {
        name: name.clone(),
        fields,
    })
}

fn parse_record_field_spec(expr: &Expr) -> Result<RecordFieldSpec, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::InvalidForm {
            name: "define-record-type",
            message: "expected each field specification to be a list",
        });
    };

    if items.len() != 2 {
        return Err(EvalError::InvalidForm {
            name: "define-record-type",
            message: "expected each field specification to contain a field and accessor name",
        });
    }

    let Expr::Symbol(name) = &items[0] else {
        return Err(EvalError::InvalidForm {
            name: "define-record-type",
            message: "expected field names to be symbols",
        });
    };

    let Expr::Symbol(accessor) = &items[1] else {
        return Err(EvalError::InvalidForm {
            name: "define-record-type",
            message: "expected accessor names to be symbols",
        });
    };

    Ok(RecordFieldSpec {
        name: name.clone(),
        accessor: accessor.clone(),
    })
}

fn parse_let_bindings(expr: &Expr, name: &'static str) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings) = expr else {
        return Err(EvalError::InvalidForm {
            name,
            message: "expected a binding list",
        });
    };

    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List(items) if items.len() == 2 => match (&items[0], &items[1]) {
                (Expr::Symbol(symbol), value) => Ok((symbol.clone(), value.clone())),
                _ => Err(EvalError::InvalidForm {
                    name,
                    message: "expected binding names to be symbols",
                }),
            },
            _ => Err(EvalError::InvalidForm {
                name,
                message: "expected each binding to have a name and value",
            }),
        })
        .collect()
}

fn parse_do_bindings(expr: &Expr) -> Result<Vec<DoBindingSpec>, EvalError> {
    let Expr::List(bindings) = expr else {
        return Err(EvalError::InvalidForm {
            name: "do",
            message: "expected a binding list",
        });
    };

    bindings
        .iter()
        .map(|binding| {
            let Expr::List(items) = binding else {
                return Err(EvalError::InvalidForm {
                    name: "do",
                    message: "expected each binding to be a list",
                });
            };

            if !(2..=3).contains(&items.len()) {
                return Err(EvalError::InvalidForm {
                    name: "do",
                    message: "expected each binding to have a name, init, and optional step",
                });
            }

            let Expr::Symbol(name) = &items[0] else {
                return Err(EvalError::InvalidForm {
                    name: "do",
                    message: "expected binding names to be symbols",
                });
            };

            Ok(DoBindingSpec {
                name: name.clone(),
                init: items[1].clone(),
                step: items.get(2).cloned(),
            })
        })
        .collect()
}

fn parse_do_termination_clause(expr: &Expr) -> Result<(Expr, Vec<Expr>), EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::InvalidForm {
            name: "do",
            message: "expected a termination clause",
        });
    };

    let Some((test, result_exprs)) = items.split_first() else {
        return Err(EvalError::InvalidForm {
            name: "do",
            message: "expected the termination clause to contain a test",
        });
    };

    Ok((test.clone(), result_exprs.to_vec()))
}

fn parse_param_names(items: &[Expr], name: &'static str) -> Result<LambdaParams, EvalError> {
    let mut required = Vec::new();
    let mut iter = items.iter();

    while let Some(item) = iter.next() {
        match item {
            Expr::Symbol(symbol) if symbol == "." => {
                let Some(Expr::Symbol(rest)) = iter.next() else {
                    return Err(EvalError::InvalidForm {
                        name,
                        message: "expected a rest parameter name after .",
                    });
                };

                if iter.next().is_some() {
                    return Err(EvalError::InvalidForm {
                        name,
                        message: "expected . to appear before the final parameter name only",
                    });
                }

                return Ok(LambdaParams {
                    required,
                    rest: Some(rest.clone()),
                });
            }
            Expr::Symbol(symbol) => required.push(symbol.clone()),
            _ => {
                return Err(EvalError::InvalidForm {
                    name,
                    message: "expected parameter names to be symbols",
                });
            }
        }
    }

    Ok(LambdaParams::fixed(required))
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Number(value) => Value::Number(value.clone()),
        Expr::Boolean(value) => Value::Boolean(*value),
        Expr::Char(value) => Value::Char(*value),
        Expr::String(value) => Value::String(SchemeString::new_immutable(value)),
        Expr::Symbol(value) => Value::Symbol(value.clone()),
        Expr::List(items) => list_from_vec(items.iter().map(quote_expr).collect()),
    }
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".into(),
        '\n' => "#\\newline".into(),
        other => format!("#\\{other}"),
    }
}

fn bind_lambda_args(params: &LambdaParams, args: Vec<Value>, env: &EnvRef) -> EnvRef {
    let call_env = Env::new(Some(env.clone()));
    let mut args = args.into_iter();

    for param in &params.required {
        let value = args
            .next()
            .expect("arity checked before binding lambda args");
        call_env.define(param.clone(), value);
    }

    if let Some(rest) = &params.rest {
        call_env.define(rest.clone(), list_from_vec(args.collect()));
    }

    call_env
}

fn empty_list() -> Value {
    Value::List(Vec::new())
}

fn new_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new(PairCell { car, cdr })))
}

fn list_from_vec(values: Vec<Value>) -> Value {
    values
        .into_iter()
        .rev()
        .fold(empty_list(), |cdr, car| new_pair(car, cdr))
}

fn pair_id(pair: &PairRef) -> usize {
    Rc::as_ptr(pair) as usize
}

fn clear_active_pairs(active: &mut HashSet<usize>, inserted: &mut Vec<usize>) {
    for id in inserted.drain(..).rev() {
        active.remove(&id);
    }
}

fn render_string_literal(value: &SchemeString) -> String {
    let escaped = value
        .to_plain_string()
        .chars()
        .flat_map(|ch| match ch {
            '\\' => ['\\', '\\'].into_iter().collect::<Vec<_>>(),
            '"' => ['\\', '"'].into_iter().collect::<Vec<_>>(),
            '\n' => ['\\', 'n'].into_iter().collect::<Vec<_>>(),
            '\t' => ['\\', 't'].into_iter().collect::<Vec<_>>(),
            other => [other].into_iter().collect::<Vec<_>>(),
        })
        .collect::<String>();

    format!("\"{escaped}\"")
}

fn render_value(value: &Value, display: bool) -> String {
    let mut active = HashSet::new();
    render_value_inner(value, display, &mut active)
}

fn render_value_inner(value: &Value, display: bool, active: &mut HashSet<usize>) -> String {
    match value {
        Value::Number(value) => value.render(),
        Value::Boolean(true) => "#t".into(),
        Value::Boolean(false) => "#f".into(),
        Value::String(value) => {
            if display {
                value.to_plain_string()
            } else {
                render_string_literal(value)
            }
        }
        Value::Symbol(value) => value.clone(),
        Value::Char(value) => {
            if display {
                value.to_string()
            } else {
                render_char(*value)
            }
        }
        Value::List(items) => {
            let rendered = items
                .iter()
                .map(|item| render_value_inner(item, display, active))
                .collect::<Vec<_>>()
                .join(" ");
            format!("({rendered})")
        }
        Value::Pair(pair) => render_pair(pair, display, active),
        Value::Vector(vector) => {
            let rendered = vector
                .values()
                .iter()
                .map(|item| render_value_inner(item, display, active))
                .collect::<Vec<_>>()
                .join(" ");
            format!("#({rendered})")
        }
        Value::Record(record) => format!("#<record {}>", record.record_type.name),
        Value::Syntax(_) => "#<syntax>".into(),
        Value::SyntaxList(_) => "#<syntax-list>".into(),
        Value::Procedure(_) => "#<procedure>".into(),
        Value::Values(values) => match values.as_slice() {
            [] => "#<values>".into(),
            [value] => render_value_inner(value, display, active),
            _ => {
                let rendered = values
                    .iter()
                    .map(|value| render_value_inner(value, display, active))
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("#<values {rendered}>")
            }
        },
        Value::Uninitialized => "#<uninitialized>".into(),
        Value::Void => "#<void>".into(),
    }
}

fn render_pair(pair: &PairRef, display: bool, active: &mut HashSet<usize>) -> String {
    let mut rendered = Vec::new();
    let mut inserted = Vec::new();
    let mut current = pair.clone();

    loop {
        let id = pair_id(&current);
        if !active.insert(id) {
            let result = if rendered.is_empty() {
                "#<circular>".into()
            } else {
                format!("({} . #<circular>)", rendered.join(" "))
            };
            clear_active_pairs(active, &mut inserted);
            return result;
        }

        inserted.push(id);
        let cell = current.borrow();
        rendered.push(render_value_inner(&cell.car, display, active));
        let next = cell.cdr.clone();
        drop(cell);

        match next {
            Value::List(items) => {
                rendered.extend(
                    items
                        .iter()
                        .map(|item| render_value_inner(item, display, active)),
                );
                let result = format!("({})", rendered.join(" "));
                clear_active_pairs(active, &mut inserted);
                return result;
            }
            Value::Pair(next_pair) => current = next_pair,
            other => {
                let tail_rendered = render_value_inner(&other, display, active);
                let result = format!("({} . {tail_rendered})", rendered.join(" "));
                clear_active_pairs(active, &mut inserted);
                return result;
            }
        }
    }
}

fn root_env() -> EnvRef {
    let env = Env::new(None);
    for name in [
        "+",
        "-",
        "*",
        "/",
        "<",
        "<=",
        "=",
        ">",
        ">=",
        "abs",
        "apply",
        "append",
        "assoc",
        "assv",
        "boolean?",
        "call-with-current-continuation",
        "call-with-values",
        "call/cc",
        "caar",
        "cadr",
        "char?",
        "char-alphabetic?",
        "char-downcase",
        "char->integer",
        "char-numeric?",
        "char-upcase",
        "char<?",
        "char=?",
        "car",
        "cdr",
        "cdar",
        "cddr",
        "cons",
        "datum->syntax",
        "display",
        "denominator",
        "dynamic-wind",
        "eq?",
        "eqv?",
        "error",
        "exact->inexact",
        "exact?",
        "equal?",
        "even?",
        "expt",
        "for-each",
        "gcd",
        "identifier?",
        "inexact->exact",
        "inexact?",
        "integer->char",
        "integer?",
        "length",
        "lcm",
        "list",
        "list-ref",
        "list-tail",
        "list->string",
        "list->vector",
        "list?",
        "make-vector",
        "make-string",
        "map",
        "max",
        "member",
        "min",
        "modulo",
        "negative?",
        "newline",
        "not",
        "numerator",
        "number->string",
        "null?",
        "number?",
        "odd?",
        "pair?",
        "positive?",
        "procedure?",
        "quotient",
        "raise",
        "rational?",
        "remainder",
        "reverse",
        "round",
        "set-car!",
        "set-cdr!",
        "string",
        "string-ci=?",
        "string-downcase",
        "string->list",
        "string<?",
        "string=?",
        "string<=?",
        "string>=?",
        "string>?",
        "string->number",
        "string->symbol",
        "string-append",
        "string-copy",
        "string-length",
        "string-ref",
        "string-set!",
        "string-upcase",
        "string?",
        "substring",
        "symbol?",
        "symbol->string",
        "syntax->datum",
        "truncate",
        "vector",
        "vector->list",
        "vector-length",
        "vector-ref",
        "vector-set!",
        "vector?",
        "values",
        "with-exception-handler",
        "write",
        "zero?",
    ] {
        env.define(name, Value::Procedure(Rc::new(Procedure::Builtin { name })));
    }

    env
}

fn apply_procedure(
    procedure: Value,
    args: Vec<Value>,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    resolve_eval_step(EvalStep::Apply(procedure, args), context)
}

fn apply_procedure_step(
    procedure: Value,
    args: Vec<Value>,
    context: &mut EvalContext,
) -> Result<EvalStep, EvalError> {
    match procedure {
        Value::Procedure(procedure) => procedure.call_step(args, context),
        _ => Err(EvalError::NotAProcedure),
    }
}

fn apply_builtin(
    name: &'static str,
    args: &[Value],
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let numbers = extract_numbers("+", args)?;
            let sum = numbers
                .iter()
                .try_fold(Number::integer(0), |acc, value| acc.add(value, "+"))?;
            Ok(Value::Number(sum))
        }
        "abs" => {
            let number = expect_number_arg("abs", args)?;
            Ok(Value::Number(number.abs("abs")?))
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::WrongArgCount {
                    name: "apply",
                    expected: "at least 2 arguments",
                    got: args.len(),
                });
            }

            let mut applied_args = args[1..args.len() - 1].to_vec();
            let tail = args
                .last()
                .expect("apply arity checked before reading tail");
            applied_args.extend(collect_list("apply", tail)?);

            apply_procedure(args[0].clone(), applied_args, context)
        }
        "append" => {
            let mut values = Vec::new();
            for arg in args {
                values.extend(collect_list("append", arg)?);
            }

            Ok(list_from_vec(values))
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "assoc",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let alist = collect_list("assoc", &args[1])?;
            for entry in alist {
                let Some(key) = pair_first(&entry) else {
                    return Err(EvalError::ExpectedPair {
                        name: "assoc",
                        found: entry.type_name(),
                    });
                };

                if equal_values(&args[0], &key) {
                    return Ok(entry);
                }
            }

            Ok(Value::Boolean(false))
        }
        "assv" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "assv",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let alist = collect_list("assv", &args[1])?;
            for entry in alist {
                let Some(key) = pair_first(&entry) else {
                    return Err(EvalError::ExpectedPair {
                        name: "assv",
                        found: entry.type_name(),
                    });
                };

                if eq_values(&args[0], &key) {
                    return Ok(entry);
                }
            }

            Ok(Value::Boolean(false))
        }
        "*" => {
            let numbers = extract_numbers("*", args)?;
            let product = numbers
                .iter()
                .try_fold(Number::integer(1), |acc, value| acc.multiply(value, "*"))?;
            Ok(Value::Number(product))
        }
        "-" => {
            let numbers = extract_numbers("-", args)?;
            let Some((first, rest)) = numbers.split_first() else {
                return Err(EvalError::WrongArgCount {
                    name: "-",
                    expected: "at least 1 argument",
                    got: 0,
                });
            };

            let value = if rest.is_empty() {
                first.negate("-")?
            } else {
                rest.iter()
                    .try_fold(first.clone(), |acc, value| acc.subtract(value, "-"))?
            };

            Ok(Value::Number(value))
        }
        "/" => {
            let numbers = extract_numbers("/", args)?;
            let Some((first, rest)) = numbers.split_first() else {
                return Err(EvalError::WrongArgCount {
                    name: "/",
                    expected: "at least 1 argument",
                    got: 0,
                });
            };

            if rest.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "/",
                    expected: "at least 2 arguments",
                    got: 1,
                });
            }

            let result = rest
                .iter()
                .try_fold(first.clone(), |acc, value| acc.divide(value, "/"))?;

            Ok(Value::Number(result))
        }
        "<" => compare_numbers("<", args, |left, right| {
            left.partial_cmp(right)
                .is_some_and(|ordering| ordering.is_lt())
        }),
        "<=" => compare_numbers("<=", args, |left, right| {
            left.partial_cmp(right)
                .is_some_and(|ordering| !ordering.is_gt())
        }),
        "=" => compare_numbers("=", args, |left, right| left.numeric_eq(right)),
        ">" => compare_numbers(">", args, |left, right| {
            left.partial_cmp(right)
                .is_some_and(|ordering| ordering.is_gt())
        }),
        ">=" => compare_numbers(">=", args, |left, right| {
            left.partial_cmp(right)
                .is_some_and(|ordering| !ordering.is_lt())
        }),
        "boolean?" => {
            predicate_builtin("boolean?", args, |value| matches!(value, Value::Boolean(_)))
        }
        "char?" => predicate_builtin("char?", args, |value| matches!(value, Value::Char(_))),
        "char-alphabetic?" => predicate_builtin(
            "char-alphabetic?",
            args,
            |value| matches!(value, Value::Char(ch) if ch.is_alphabetic()),
        ),
        "char-downcase" => {
            let ch = expect_char_arg("char-downcase", args)?;
            Ok(Value::Char(ch.to_ascii_lowercase()))
        }
        "char->integer" => {
            let ch = expect_char_arg("char->integer", args)?;
            Ok(Value::Number(Number::integer(i64::from(u32::from(ch)))))
        }
        "char-numeric?" => predicate_builtin(
            "char-numeric?",
            args,
            |value| matches!(value, Value::Char(ch) if ch.is_numeric()),
        ),
        "char-upcase" => {
            let ch = expect_char_arg("char-upcase", args)?;
            Ok(Value::Char(ch.to_ascii_uppercase()))
        }
        "char<?" => compare_characters("char<?", args, |left, right| left < right),
        "char=?" => compare_characters("char=?", args, |left, right| left == right),
        "caar" => apply_cxr("caar", args),
        "cadr" => apply_cxr("cadr", args),
        "car" => {
            let value = expect_pair_arg("car", args)?;
            Ok(match value {
                Value::List(items) => items[0].clone(),
                Value::Pair(pair) => pair.borrow().car.clone(),
                _ => unreachable!("expect_pair_value only returns pairs"),
            })
        }
        "cdr" => {
            let value = expect_pair_arg("cdr", args)?;
            Ok(match value {
                Value::List(items) => list_from_vec(items[1..].to_vec()),
                Value::Pair(pair) => pair.borrow().cdr.clone(),
                _ => unreachable!("expect_pair_value only returns pairs"),
            })
        }
        "cdar" => apply_cxr("cdar", args),
        "cddr" => apply_cxr("cddr", args),
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "cons",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            Ok(new_pair(args[0].clone(), args[1].clone()))
        }
        "datum->syntax" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "datum->syntax",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let context_syntax = expect_syntax("datum->syntax", &args[0])?;
            let origin = syntax_origin(context_syntax.as_ref());
            Ok(Value::Syntax(Rc::new(datum_to_syntax(
                &args[1],
                origin,
                "datum->syntax",
            )?)))
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "display",
                    expected: "exactly 1 argument",
                    got: args.len(),
                });
            }

            context.output.push_str(&args[0].display_render());
            Ok(Value::Void)
        }
        "denominator" => {
            let number = expect_exact_number_arg("denominator", args)?;
            Ok(Value::Number(Number::integer(
                number
                    .denominator()
                    .expect("exact numbers always have a denominator"),
            )))
        }
        "error" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "error",
                    expected: "at least 1 argument",
                    got: 0,
                });
            }

            let mut message = args[0].display_render();
            for arg in &args[1..] {
                if !message.is_empty() {
                    message.push(' ');
                }
                message.push_str(&arg.render());
            }

            Err(EvalError::RaisedError { message })
        }
        "for-each" => apply_for_each(args, context),
        "gcd" => apply_gcd(args),
        "identifier?" => predicate_builtin("identifier?", args, |value| {
            matches!(value, Value::Syntax(syntax) if matches!(syntax.as_ref(), SyntaxExpr::Symbol(_)))
        }),
        "length" => {
            let list = collect_list_arg("length", args)?;
            Ok(Value::Number(Number::integer(list.len() as i64)))
        }
        "lcm" => apply_lcm(args),
        "list" => Ok(list_from_vec(args.to_vec())),
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "list-ref",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let list = collect_list("list-ref", &args[0])?;
            let index = expect_index("list-ref", &args[1], list.len())?;
            Ok(list[index].clone())
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "list-tail",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let list = collect_list("list-tail", &args[0])?;
            let index = expect_index_inclusive_end("list-tail", &args[1], list.len())?;
            Ok(list_from_vec(list[index..].to_vec()))
        }
        "list->string" => {
            let list = collect_list_arg("list->string", args)?;
            let chars = list
                .iter()
                .map(|value| expect_char("list->string", value))
                .collect::<Result<Vec<_>, EvalError>>()?;
            Ok(Value::String(SchemeString::from_runtime_chars(chars)))
        }
        "list->vector" => {
            let list = collect_list_arg("list->vector", args)?;
            Ok(Value::Vector(SchemeVector::new(list)))
        }
        "list?" => predicate_builtin("list?", args, is_proper_list),
        "make-string" => {
            if !(1..=2).contains(&args.len()) {
                return Err(EvalError::WrongArgCount {
                    name: "make-string",
                    expected: "1 or 2 arguments",
                    got: args.len(),
                });
            }

            let length = expect_nonnegative_length("make-string", &args[0])?;
            let fill = args
                .get(1)
                .map(|value| expect_char("make-string", value))
                .transpose()?
                .unwrap_or(' ');
            Ok(Value::String(SchemeString::new_runtime(
                std::iter::repeat_n(fill, length).collect::<String>(),
            )))
        }
        "make-vector" => {
            if !(1..=2).contains(&args.len()) {
                return Err(EvalError::WrongArgCount {
                    name: "make-vector",
                    expected: "1 or 2 arguments",
                    got: args.len(),
                });
            }

            let length = expect_nonnegative_length("make-vector", &args[0])?;
            let fill = args.get(1).cloned().unwrap_or(Value::Void);
            Ok(Value::Vector(SchemeVector::new(vec![fill; length])))
        }
        "map" => apply_map(args, context),
        "max" => {
            let numbers = extract_numbers("max", args)?;
            let Some(first) = numbers.first() else {
                return Err(EvalError::WrongArgCount {
                    name: "max",
                    expected: "at least 1 argument",
                    got: 0,
                });
            };

            let maximum = numbers.iter().skip(1).fold(first.clone(), |best, value| {
                if value
                    .partial_cmp(&best)
                    .is_some_and(|ordering| ordering.is_gt())
                {
                    value.clone()
                } else {
                    best
                }
            });

            Ok(Value::Number(maximum))
        }
        "member" => apply_member(args),
        "min" => {
            let numbers = extract_numbers("min", args)?;
            let Some(first) = numbers.first() else {
                return Err(EvalError::WrongArgCount {
                    name: "min",
                    expected: "at least 1 argument",
                    got: 0,
                });
            };

            let minimum = numbers.iter().skip(1).fold(first.clone(), |best, value| {
                if value
                    .partial_cmp(&best)
                    .is_some_and(|ordering| ordering.is_lt())
                {
                    value.clone()
                } else {
                    best
                }
            });

            Ok(Value::Number(minimum))
        }
        "modulo" => {
            let (left, right) = expect_two_exact_integers("modulo", args)?;
            if right == 0 {
                return Err(EvalError::DivisionByZero);
            }

            let mut remainder = left % right;
            if remainder != 0 && (remainder > 0) != (right > 0) {
                remainder += right;
            }

            Ok(Value::Number(Number::integer(remainder)))
        }
        "negative?" => {
            let number = expect_number_arg("negative?", args)?;
            Ok(Value::Boolean(number.is_negative()))
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "newline",
                    expected: "exactly 0 arguments",
                    got: args.len(),
                });
            }

            context.output.push('\n');
            Ok(Value::Void)
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "not",
                    expected: "exactly 1 argument",
                    got: args.len(),
                });
            }

            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "numerator" => {
            let number = expect_exact_number_arg("numerator", args)?;
            Ok(Value::Number(Number::integer(
                number
                    .numerator()
                    .expect("exact numbers always have a numerator"),
            )))
        }
        "number->string" => {
            let number = expect_number_arg("number->string", args)?;
            Ok(Value::String(SchemeString::new_runtime(number.render())))
        }
        "null?" => predicate_builtin(
            "null?",
            args,
            |value| matches!(value, Value::List(items) if items.is_empty()),
        ),
        "number?" => predicate_builtin("number?", args, |value| matches!(value, Value::Number(_))),
        "odd?" => {
            let number = expect_exact_integer_arg("odd?", args)?;
            Ok(Value::Boolean(number.rem_euclid(2) == 1))
        }
        "pair?" => predicate_builtin("pair?", args, |value| match value {
            Value::Pair(_) => true,
            Value::List(items) => !items.is_empty(),
            _ => false,
        }),
        "positive?" => {
            let number = expect_number_arg("positive?", args)?;
            Ok(Value::Boolean(number.is_positive()))
        }
        "procedure?" => predicate_builtin("procedure?", args, |value| {
            matches!(value, Value::Procedure(_))
        }),
        "quotient" => {
            let (left, right) = expect_two_exact_integers("quotient", args)?;
            if right == 0 {
                return Err(EvalError::DivisionByZero);
            }

            Ok(Value::Number(Number::integer(left / right)))
        }
        "raise" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "raise",
                    expected: "exactly 1 argument",
                    got: args.len(),
                });
            }

            signal_exception(
                args[0].clone(),
                context.exception_handlers.borrow().clone(),
                current_position(context),
            )
        }
        "rational?" => predicate_builtin(
            "rational?",
            args,
            |value| matches!(value, Value::Number(number) if number.is_rational()),
        ),
        "exact->inexact" => {
            let number = expect_number_arg("exact->inexact", args)?;
            Ok(Value::Number(number.exact_to_inexact()))
        }
        "exact?" => predicate_builtin(
            "exact?",
            args,
            |value| matches!(value, Value::Number(number) if number.is_exact()),
        ),
        "integer->char" => {
            let code_point = expect_exact_integer_arg("integer->char", args)?;
            let value = u32::try_from(code_point)
                .ok()
                .and_then(char::from_u32)
                .ok_or(EvalError::InvalidArgument {
                    name: "integer->char",
                    message: "expected a valid Unicode scalar value",
                })?;
            Ok(Value::Char(value))
        }
        "inexact->exact" => {
            let number = expect_number_arg("inexact->exact", args)?;
            Ok(Value::Number(number.inexact_to_exact("inexact->exact")?))
        }
        "inexact?" => predicate_builtin(
            "inexact?",
            args,
            |value| matches!(value, Value::Number(number) if number.is_inexact()),
        ),
        "integer?" => predicate_builtin(
            "integer?",
            args,
            |value| matches!(value, Value::Number(number) if number.is_integer()),
        ),
        "remainder" => {
            let (left, right) = expect_two_exact_integers("remainder", args)?;
            if right == 0 {
                return Err(EvalError::DivisionByZero);
            }

            Ok(Value::Number(Number::integer(left % right)))
        }
        "reverse" => {
            let mut list = collect_list_arg("reverse", args)?;
            list.reverse();
            Ok(list_from_vec(list))
        }
        "round" => {
            let number = expect_number_arg("round", args)?;
            Ok(match number {
                Number::Exact(_) => Value::Number(Number::integer(number.to_f64().round() as i64)),
                Number::Inexact(value) => Value::Number(Number::Inexact(value.round())),
            })
        }
        "set-car!" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "set-car!",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let pair = expect_mutable_pair("set-car!", &args[0])?;
            pair.borrow_mut().car = args[1].clone();
            Ok(Value::Void)
        }
        "set-cdr!" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "set-cdr!",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let pair = expect_mutable_pair("set-cdr!", &args[0])?;
            pair.borrow_mut().cdr = args[1].clone();
            Ok(Value::Void)
        }
        "string->number" => {
            let value = expect_string_arg("string->number", args)?;
            match Number::parse_literal(&value.to_plain_string()) {
                Ok(number) => Ok(Value::Number(number)),
                Err(_) => Ok(Value::Boolean(false)),
            }
        }
        "string->list" => {
            let value = expect_string_arg("string->list", args)?;
            Ok(list_from_vec(
                value.chars().into_iter().map(Value::Char).collect(),
            ))
        }
        "string->symbol" => {
            let value = expect_string_arg("string->symbol", args)?;
            Ok(Value::Symbol(value.to_plain_string()))
        }
        "string" => {
            let chars = args
                .iter()
                .map(|value| expect_char("string", value))
                .collect::<Result<Vec<_>, EvalError>>()?;
            Ok(Value::String(SchemeString::new_runtime(
                chars.into_iter().collect::<String>(),
            )))
        }
        "string-append" => {
            let mut result = String::new();
            for arg in args {
                result.push_str(&expect_string("string-append", arg)?.to_plain_string());
            }
            Ok(Value::String(SchemeString::new_runtime(result)))
        }
        "string-copy" => {
            let value = expect_string_arg("string-copy", args)?;
            Ok(Value::String(value.copy_runtime()))
        }
        "string-ci=?" => compare_strings("string-ci=?", args, |left, right| {
            left.to_lowercase() == right.to_lowercase()
        }),
        "string-downcase" => {
            let value = expect_string_arg("string-downcase", args)?;
            Ok(Value::String(SchemeString::new_runtime(
                value.to_plain_string().to_lowercase(),
            )))
        }
        "string=?" => compare_strings("string=?", args, |left, right| left == right),
        "string<=?" => compare_strings("string<=?", args, |left, right| left <= right),
        "string-length" => {
            let value = expect_string_arg("string-length", args)?;
            Ok(Value::Number(Number::integer(value.len() as i64)))
        }
        "string>=?" => compare_strings("string>=?", args, |left, right| left >= right),
        "string>?" => compare_strings("string>?", args, |left, right| left > right),
        "string<?" => compare_strings("string<?", args, |left, right| left < right),
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "string-ref",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let string = expect_string("string-ref", &args[0])?;
            let index = expect_index("string-ref", &args[1], string.len())?;
            Ok(Value::Char(string.get(index)))
        }
        "string-set!" => {
            if args.len() != 3 {
                return Err(EvalError::WrongArgCount {
                    name: "string-set!",
                    expected: "exactly 3 arguments",
                    got: args.len(),
                });
            }

            let string = expect_string("string-set!", &args[0])?;
            let index = expect_index("string-set!", &args[1], string.len())?;
            let value = expect_char("string-set!", &args[2])?;
            string.set(index, value, "string-set!")?;
            Ok(Value::Void)
        }
        "string-upcase" => {
            let value = expect_string_arg("string-upcase", args)?;
            Ok(Value::String(SchemeString::new_runtime(
                value.to_plain_string().to_uppercase(),
            )))
        }
        "string?" => predicate_builtin("string?", args, |value| matches!(value, Value::String(_))),
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::WrongArgCount {
                    name: "substring",
                    expected: "exactly 3 arguments",
                    got: args.len(),
                });
            }

            let chars = expect_string("substring", &args[0])?.chars();
            let start = expect_index_inclusive_end("substring", &args[1], chars.len())?;
            let end = expect_index_inclusive_end("substring", &args[2], chars.len())?;

            if start > end {
                return Err(EvalError::InvalidRange {
                    name: "substring",
                    start: start as i64,
                    end: end as i64,
                });
            }

            Ok(Value::String(SchemeString::new_runtime(
                chars[start..end].iter().collect::<String>(),
            )))
        }
        "symbol?" => predicate_builtin("symbol?", args, |value| matches!(value, Value::Symbol(_))),
        "symbol->string" => {
            let value = expect_symbol_arg("symbol->string", args)?;
            Ok(Value::String(SchemeString::new_runtime(value)))
        }
        "syntax->datum" => {
            let syntax = expect_syntax_arg("syntax->datum", args)?;
            Ok(syntax_to_datum_value(syntax.as_ref()))
        }
        "truncate" => {
            let number = expect_number_arg("truncate", args)?;
            Ok(match number {
                Number::Exact(_) => Value::Number(Number::integer(
                    number.numerator().unwrap() / number.denominator().unwrap(),
                )),
                Number::Inexact(value) => Value::Number(Number::Inexact(value.trunc())),
            })
        }
        "vector" => Ok(Value::Vector(SchemeVector::new(args.to_vec()))),
        "vector->list" => {
            let vector = expect_vector_arg("vector->list", args)?;
            Ok(list_from_vec(vector.values()))
        }
        "vector-length" => {
            let vector = expect_vector_arg("vector-length", args)?;
            Ok(Value::Number(Number::integer(vector.len() as i64)))
        }
        "vector-ref" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "vector-ref",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let vector = expect_vector("vector-ref", &args[0])?;
            let index = expect_index("vector-ref", &args[1], vector.len())?;
            Ok(vector.get(index))
        }
        "vector-set!" => {
            if args.len() != 3 {
                return Err(EvalError::WrongArgCount {
                    name: "vector-set!",
                    expected: "exactly 3 arguments",
                    got: args.len(),
                });
            }

            let vector = expect_vector("vector-set!", &args[0])?;
            let index = expect_index("vector-set!", &args[1], vector.len())?;
            vector.set(index, args[2].clone());
            Ok(Value::Void)
        }
        "vector?" => predicate_builtin("vector?", args, |value| matches!(value, Value::Vector(_))),
        "values" => Ok(pack_values(args.to_vec())),
        "with-exception-handler" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "with-exception-handler",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            if !matches!(args[0], Value::Procedure(_)) || !matches!(args[1], Value::Procedure(_)) {
                return Err(EvalError::NotAProcedure);
            }

            let handler = Rc::new(ExceptionHandler {
                kind: ExceptionHandlerKind::Procedure {
                    handler: args[0].clone(),
                },
                frames: context.frames.borrow().clone(),
                winds: context.dynamic_winds.borrow().clone(),
                outer_handlers: context.exception_handlers.borrow().clone(),
                position: current_position(context),
            });

            let _handler_guard = push_exception_handler(context, handler);
            apply_procedure(args[1].clone(), Vec::new(), context)
        }
        "eq?" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "eq?",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            Ok(Value::Boolean(eq_values(&args[0], &args[1])))
        }
        "eqv?" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "eqv?",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            Ok(Value::Boolean(eq_values(&args[0], &args[1])))
        }
        "equal?" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "equal?",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            Ok(Value::Boolean(equal_values(&args[0], &args[1])))
        }
        "even?" => {
            let number = expect_exact_integer_arg("even?", args)?;
            Ok(Value::Boolean(number.rem_euclid(2) == 0))
        }
        "expt" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "expt",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let base = expect_number("expt", &args[0])?;
            let exponent = expect_exact_integer("expt", &args[1])?;
            Ok(Value::Number(base.expt(exponent, "expt")?))
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "write",
                    expected: "exactly 1 argument",
                    got: args.len(),
                });
            }

            context.output.push_str(&args[0].render());
            Ok(Value::Void)
        }
        "zero?" => {
            let number = expect_number_arg("zero?", args)?;
            Ok(Value::Boolean(number.is_zero()))
        }
        _ => Err(EvalError::UnknownProcedure {
            name: name.to_string(),
        }),
    }
}

fn compare_numbers(
    name: &'static str,
    args: &[Value],
    predicate: impl Fn(&Number, &Number) -> bool,
) -> Result<Value, EvalError> {
    let numbers = extract_numbers(name, args)?;
    if numbers.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2 arguments",
            got: numbers.len(),
        });
    }

    let is_match = numbers.windows(2).all(|pair| predicate(&pair[0], &pair[1]));

    Ok(Value::Boolean(is_match))
}

fn compare_characters(
    name: &'static str,
    args: &[Value],
    predicate: impl Fn(char, char) -> bool,
) -> Result<Value, EvalError> {
    let chars = args
        .iter()
        .map(|value| expect_char(name, value))
        .collect::<Result<Vec<_>, EvalError>>()?;

    if chars.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2 arguments",
            got: chars.len(),
        });
    }

    Ok(Value::Boolean(
        chars.windows(2).all(|pair| predicate(pair[0], pair[1])),
    ))
}

fn compare_strings(
    name: &'static str,
    args: &[Value],
    predicate: impl Fn(&str, &str) -> bool,
) -> Result<Value, EvalError> {
    let strings = args
        .iter()
        .map(|value| expect_string(name, value).map(|string| string.to_plain_string()))
        .collect::<Result<Vec<_>, EvalError>>()?;

    if strings.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2 arguments",
            got: strings.len(),
        });
    }

    Ok(Value::Boolean(
        strings.windows(2).all(|pair| predicate(&pair[0], &pair[1])),
    ))
}

fn extract_numbers(name: &'static str, args: &[Value]) -> Result<Vec<Number>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Number(number) => Ok(number.clone()),
            other => Err(EvalError::ExpectedNumber {
                name,
                found: other.type_name(),
            }),
        })
        .collect()
}

fn expect_number(name: &'static str, value: &Value) -> Result<Number, EvalError> {
    match value {
        Value::Number(number) => Ok(number.clone()),
        other => Err(EvalError::ExpectedNumber {
            name,
            found: other.type_name(),
        }),
    }
}

fn expect_two_exact_integers(name: &'static str, args: &[Value]) -> Result<(i64, i64), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 2 arguments",
            got: args.len(),
        });
    }

    let left = expect_exact_integer(name, &args[0])?;
    let right = expect_exact_integer(name, &args[1])?;

    Ok((left, right))
}

fn expect_number_arg(name: &'static str, args: &[Value]) -> Result<Number, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    expect_number(name, &args[0])
}

fn expect_exact_number_arg(name: &'static str, args: &[Value]) -> Result<Number, EvalError> {
    let number = expect_number_arg(name, args)?;
    if number.is_exact() {
        Ok(number)
    } else {
        Err(EvalError::InvalidArgument {
            name,
            message: "expected an exact number",
        })
    }
}

fn expect_exact_integer(name: &'static str, value: &Value) -> Result<i64, EvalError> {
    let number = expect_number(name, value)?;
    number.as_exact_integer().ok_or(EvalError::InvalidArgument {
        name,
        message: "expected an exact integer",
    })
}

fn expect_exact_integer_arg(name: &'static str, args: &[Value]) -> Result<i64, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    expect_exact_integer(name, &args[0])
}

fn expect_string(name: &'static str, value: &Value) -> Result<SchemeString, EvalError> {
    match value {
        Value::String(string) => Ok(string.clone()),
        other => Err(EvalError::ExpectedString {
            name,
            found: other.type_name(),
        }),
    }
}

fn expect_string_arg(name: &'static str, args: &[Value]) -> Result<SchemeString, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    expect_string(name, &args[0])
}

fn expect_char(name: &'static str, value: &Value) -> Result<char, EvalError> {
    match value {
        Value::Char(ch) => Ok(*ch),
        other => Err(EvalError::ExpectedCharacter {
            name,
            found: other.type_name(),
        }),
    }
}

fn expect_char_arg(name: &'static str, args: &[Value]) -> Result<char, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    expect_char(name, &args[0])
}

fn expect_symbol_arg<'a>(name: &'static str, args: &'a [Value]) -> Result<&'a str, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    match &args[0] {
        Value::Symbol(symbol) => Ok(symbol),
        other => Err(EvalError::ExpectedSymbol {
            name,
            found: other.type_name(),
        }),
    }
}

fn expect_syntax(name: &'static str, value: &Value) -> Result<Rc<SyntaxExpr>, EvalError> {
    match value {
        Value::Syntax(syntax) => Ok(syntax.clone()),
        other => Err(EvalError::ExpectedSyntax {
            name,
            found: other.type_name(),
        }),
    }
}

fn expect_syntax_arg(name: &'static str, args: &[Value]) -> Result<Rc<SyntaxExpr>, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    expect_syntax(name, &args[0])
}

fn expect_index(name: &'static str, value: &Value, len: usize) -> Result<usize, EvalError> {
    let index = expect_exact_integer(name, value)?;

    if index < 0 || index as usize >= len {
        return Err(EvalError::IndexOutOfBounds { name, index });
    }

    Ok(index as usize)
}

fn expect_index_inclusive_end(
    name: &'static str,
    value: &Value,
    len: usize,
) -> Result<usize, EvalError> {
    let index = expect_exact_integer(name, value)?;

    if index < 0 || index as usize > len {
        return Err(EvalError::IndexOutOfBounds { name, index });
    }

    Ok(index as usize)
}

fn predicate_builtin(
    name: &'static str,
    args: &[Value],
    predicate: impl Fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(predicate(&args[0])))
}

fn is_proper_list(value: &Value) -> bool {
    let mut seen = HashSet::new();
    let mut current = value.clone();

    loop {
        match current {
            Value::List(_) => return true,
            Value::Pair(pair) => {
                if !seen.insert(pair_id(&pair)) {
                    return false;
                }

                current = pair.borrow().cdr.clone();
            }
            _ => return false,
        }
    }
}

fn collect_list(name: &'static str, value: &Value) -> Result<Vec<Value>, EvalError> {
    let mut seen = HashSet::new();
    let mut current = value.clone();
    let mut items = Vec::new();

    loop {
        match current {
            Value::List(list_items) => {
                items.extend(list_items);
                return Ok(items);
            }
            Value::Pair(pair) => {
                if !seen.insert(pair_id(&pair)) {
                    return Err(EvalError::ExpectedList {
                        name,
                        found: "pair",
                    });
                }

                let cell = pair.borrow();
                items.push(cell.car.clone());
                current = cell.cdr.clone();
            }
            other => {
                return Err(EvalError::ExpectedList {
                    name,
                    found: other.type_name(),
                })
            }
        }
    }
}

fn collect_list_arg(name: &'static str, args: &[Value]) -> Result<Vec<Value>, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    collect_list(name, &args[0])
}

fn expect_vector(name: &'static str, value: &Value) -> Result<SchemeVector, EvalError> {
    match value {
        Value::Vector(vector) => Ok(vector.clone()),
        other => Err(EvalError::ExpectedVector {
            name,
            found: other.type_name(),
        }),
    }
}

fn expect_vector_arg(name: &'static str, args: &[Value]) -> Result<SchemeVector, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    expect_vector(name, &args[0])
}

fn expect_nonnegative_length(name: &'static str, value: &Value) -> Result<usize, EvalError> {
    let length = expect_exact_integer(name, value)?;
    if length < 0 {
        return Err(EvalError::InvalidArgument {
            name,
            message: "expected a non-negative exact integer",
        });
    }

    Ok(length as usize)
}

fn expect_pair_arg(name: &'static str, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    match &args[0] {
        Value::List(items) if items.is_empty() => Err(EvalError::ExpectedPair {
            name,
            found: "list",
        }),
        value @ Value::List(_) | value @ Value::Pair(_) => Ok(value.clone()),
        other => Err(EvalError::ExpectedPair {
            name,
            found: other.type_name(),
        }),
    }
}

fn expect_mutable_pair(name: &'static str, value: &Value) -> Result<PairRef, EvalError> {
    match value {
        Value::Pair(pair) => Ok(pair.clone()),
        Value::List(_) => Err(EvalError::ExpectedPair {
            name,
            found: "list",
        }),
        other => Err(EvalError::ExpectedPair {
            name,
            found: other.type_name(),
        }),
    }
}

fn pair_first(value: &Value) -> Option<Value> {
    match value {
        Value::List(items) => items.first().cloned(),
        Value::Pair(pair) => Some(pair.borrow().car.clone()),
        _ => None,
    }
}

fn eq_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left.numeric_eq(right),
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::String(left), Value::String(right)) => Rc::ptr_eq(&left.inner, &right.inner),
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(&left.inner, &right.inner),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Syntax(left), Value::Syntax(right)) => Rc::ptr_eq(left, right),
        (Value::SyntaxList(left), Value::SyntaxList(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left_item, right_item)| Rc::ptr_eq(left_item, right_item))
        }
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
        (Value::List(left), Value::List(right)) => left.is_empty() && right.is_empty(),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn equal_values(left: &Value, right: &Value) -> bool {
    let mut seen_pairs = HashSet::new();
    equal_values_inner(left, right, &mut seen_pairs)
}

fn equal_values_inner(
    left: &Value,
    right: &Value,
    seen_pairs: &mut HashSet<(usize, usize)>,
) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left.numeric_eq(right),
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => {
            left.to_plain_string() == right.to_plain_string()
        }
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left_item, right_item)| {
                        equal_values_inner(left_item, right_item, seen_pairs)
                    })
        }
        (Value::Pair(left), Value::Pair(right)) => {
            let pair_key = (pair_id(left), pair_id(right));
            if !seen_pairs.insert(pair_key) {
                return true;
            }

            let left_cell = left.borrow();
            let right_cell = right.borrow();
            equal_values_inner(&left_cell.car, &right_cell.car, seen_pairs)
                && equal_values_inner(&left_cell.cdr, &right_cell.cdr, seen_pairs)
        }
        (Value::Vector(left), Value::Vector(right)) => {
            let left_values = left.values();
            let right_values = right.values();
            left_values.len() == right_values.len()
                && left_values
                    .iter()
                    .zip(right_values.iter())
                    .all(|(left_item, right_item)| {
                        equal_values_inner(left_item, right_item, seen_pairs)
                    })
        }
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Syntax(left), Value::Syntax(right)) => left.as_ref() == right.as_ref(),
        (Value::SyntaxList(left), Value::SyntaxList(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left_item, right_item)| left_item.as_ref() == right_item.as_ref())
        }
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn car_from_value(name: &'static str, value: &Value) -> Result<Value, EvalError> {
    match value {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::List(_) => Err(EvalError::ExpectedPair {
            name,
            found: "list",
        }),
        Value::Pair(pair) => Ok(pair.borrow().car.clone()),
        other => Err(EvalError::ExpectedPair {
            name,
            found: other.type_name(),
        }),
    }
}

fn cdr_from_value(name: &'static str, value: &Value) -> Result<Value, EvalError> {
    match value {
        Value::List(items) if !items.is_empty() => Ok(list_from_vec(items[1..].to_vec())),
        Value::List(_) => Err(EvalError::ExpectedPair {
            name,
            found: "list",
        }),
        Value::Pair(pair) => Ok(pair.borrow().cdr.clone()),
        other => Err(EvalError::ExpectedPair {
            name,
            found: other.type_name(),
        }),
    }
}

fn apply_cxr(name: &'static str, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    let mut value = args[0].clone();
    for op in name[1..name.len() - 1].chars().rev() {
        value = match op {
            'a' => car_from_value(name, &value)?,
            'd' => cdr_from_value(name, &value)?,
            _ => unreachable!("cxr builtin names only contain a and d"),
        };
    }

    Ok(value)
}

fn gcd_i64(left: i64, right: i64) -> i64 {
    let mut left = (left as i128).abs();
    let mut right = (right as i128).abs();

    while right != 0 {
        let next = left % right;
        left = right;
        right = next;
    }

    left as i64
}

fn apply_gcd(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = 0_i64;
    for value in args {
        result = gcd_i64(result, expect_exact_integer("gcd", value)?);
    }

    Ok(Value::Number(Number::integer(result)))
}

fn apply_lcm(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = 1_i64;

    for value in args {
        let next = expect_exact_integer("lcm", value)?;
        if result == 0 || next == 0 {
            result = 0;
            continue;
        }

        let gcd = gcd_i64(result, next) as i128;
        let lcm = ((result as i128) / gcd) * (next as i128);
        result = i64::try_from(lcm.abs()).map_err(|_| EvalError::InvalidArgument {
            name: "lcm",
            message: "result overflowed i64",
        })?;
    }

    Ok(Value::Number(Number::integer(result)))
}

fn apply_member(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "member",
            expected: "exactly 2 arguments",
            got: args.len(),
        });
    }

    let mut seen = HashSet::new();
    let mut current = args[1].clone();

    loop {
        match current {
            Value::List(items) => {
                for (index, item) in items.iter().enumerate() {
                    if equal_values(&args[0], item) {
                        return Ok(list_from_vec(items[index..].to_vec()));
                    }
                }

                return Ok(Value::Boolean(false));
            }
            Value::Pair(pair) => {
                if !seen.insert(pair_id(&pair)) {
                    return Ok(Value::Boolean(false));
                }

                let cell = pair.borrow();
                if equal_values(&args[0], &cell.car) {
                    return Ok(Value::Pair(pair.clone()));
                }
                current = cell.cdr.clone();
            }
            other => {
                return Err(EvalError::ExpectedList {
                    name: "member",
                    found: other.type_name(),
                })
            }
        }
    }
}

fn apply_map(args: &[Value], context: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "map",
            expected: "a procedure and at least one list",
            got: args.len(),
        });
    }

    if !matches!(args[0], Value::Procedure(_)) {
        return Err(EvalError::NotAProcedure);
    }

    let lists = args[1..]
        .iter()
        .map(|value| collect_list("map", value))
        .collect::<Result<Vec<_>, EvalError>>()?;
    apply_map_from_index(args[0].clone(), &lists, 0, Vec::new(), context)
}

fn apply_for_each(args: &[Value], context: &mut EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "for-each",
            expected: "a procedure and at least one list",
            got: args.len(),
        });
    }

    if !matches!(args[0], Value::Procedure(_)) {
        return Err(EvalError::NotAProcedure);
    }

    let lists = args[1..]
        .iter()
        .map(|value| collect_list("for-each", value))
        .collect::<Result<Vec<_>, EvalError>>()?;
    apply_for_each_from_index(args[0].clone(), &lists, 0, context)
}

fn eval_program(input: &str) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program()?;
    if program.is_empty() {
        return Err(EvalError::EmptyInput.with_position(1, 1));
    }

    let env = root_env();
    let mut context = EvalContext::default();
    let last_value = run_eval_action(
        EvalAction::Program(TopLevelState {
            remaining: program,
            env: env.clone(),
            macros: MacroEnv::default(),
            expander: MacroExpander::default(),
        }),
        &mut context,
    )?;

    Ok((last_value, context.output))
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
    let (value, _) = eval_program(input)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_program(input)?;
    Ok((value.render(), output))
}

fn is_token_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';' | '\'' | '"')
}

#[cfg(test)]
mod tests;
