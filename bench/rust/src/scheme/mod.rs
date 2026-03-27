pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

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

fn eval_program(input: &str) -> Result<(Value, String), EvalError> {
    let expressions = Parser::new(input).parse_program()?;
    if expressions.is_empty() {
        return Err(EvalError::msg("input is empty"));
    }

    let env = default_env();
    let output = Rc::new(RefCell::new(String::new()));
    let last = eval_sequence_machine(&expressions, &env, &output)?;

    let captured_output = output.borrow().clone();
    Ok((last, captured_output))
}

type EnvRef = Rc<RefCell<Environment>>;
type CellRef = Rc<RefCell<Value>>;
type OutputRef = Rc<RefCell<String>>;

static MACRO_GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);
static WIND_FRAME_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Number(i128),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone)]
enum Value {
    Number(i128),
    Bool(bool),
    String(String),
    Char(char),
    Symbol(String),
    List(Vec<Value>),
    Procedure(Rc<Procedure>),
    Void,
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

#[derive(Clone)]
enum Procedure {
    Builtin(BuiltinProcedure),
    Special(SpecialProcedure),
    Lambda(LambdaProcedure),
    CaseLambda(CaseLambdaProcedure),
    Continuation(Rc<ContinuationProcedure>),
}

#[derive(Clone, Copy)]
struct BuiltinProcedure {
    name: &'static str,
    func: fn(&[Value], &OutputRef) -> Result<Value, EvalError>,
}

#[derive(Clone, Copy)]
struct SpecialProcedure {
    name: &'static str,
    kind: SpecialProcedureKind,
}

#[derive(Clone, Copy)]
enum SpecialProcedureKind {
    Apply,
    CallCc,
    DynamicWind,
}

#[derive(Clone)]
struct LambdaProcedure {
    name: Option<String>,
    params: Vec<String>,
    rest: Option<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone)]
struct CaseLambdaProcedure {
    name: Option<String>,
    clauses: Vec<LambdaProcedure>,
}

#[derive(Clone)]
struct MacroRule {
    pattern: Vec<Expr>,
    template: Expr,
}

#[derive(Clone)]
struct MacroDefinition {
    name: String,
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
    env: EnvRef,
}

#[derive(Clone, PartialEq)]
enum PatternBinding {
    Single(Expr),
    Sequence(Vec<Expr>),
}

enum TailResult {
    Value(Value),
    Call(Value, Vec<Value>),
}

#[derive(Clone)]
struct ContinuationProcedure {
    continuation: Vec<Frame>,
    wind_stack: Vec<WindFrame>,
}

#[derive(Clone)]
struct WindFrame {
    id: usize,
    in_thunk: Value,
    out_thunk: Value,
}

#[derive(Clone)]
enum WindTransitionStep {
    Out(WindFrame),
    In(WindFrame),
}

enum MachineState {
    Expr(Expr, EnvRef),
    Value(Value),
    Apply(Value, Vec<Value>),
}

#[derive(Clone)]
enum Frame {
    Sequence {
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    If {
        then_expr: Expr,
        else_expr: Option<Expr>,
        env: EnvRef,
    },
    DefineValue {
        name: String,
        env: EnvRef,
    },
    SetValue {
        name: String,
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
        body: Vec<Expr>,
        remaining_clauses: Vec<Expr>,
        env: EnvRef,
    },
    CallOperator {
        arguments: Vec<Expr>,
        env: EnvRef,
    },
    CallArgument {
        procedure: Value,
        evaluated: Vec<Value>,
        remaining: Vec<Expr>,
        env: EnvRef,
    },
    Let {
        names: Vec<String>,
        remaining_exprs: Vec<Expr>,
        evaluated: Vec<Value>,
        body: Vec<Expr>,
        env: EnvRef,
    },
    NamedLet {
        procedure: Value,
        remaining_exprs: Vec<Expr>,
        evaluated: Vec<Value>,
        env: EnvRef,
    },
    DynamicWindAfterIn {
        frame: WindFrame,
        body_thunk: Value,
    },
    DynamicWindAfterBody {
        frame: WindFrame,
    },
    DynamicWindAfterOut {
        result: Value,
    },
    WindTransition {
        pending_push: Option<WindFrame>,
        remaining: Vec<WindTransitionStep>,
        target_continuation: Vec<Frame>,
        target_wind_stack: Vec<WindFrame>,
        value: Value,
    },
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: HashMap<String, CellRef>,
    macros: HashMap<String, Rc<MacroDefinition>>,
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent,
            bindings: HashMap::new(),
            macros: HashMap::new(),
        }))
    }

    fn define(env: &EnvRef, name: impl Into<String>, value: Value) {
        Self::define_cell(env, name, Rc::new(RefCell::new(value)));
    }

    fn define_cell(env: &EnvRef, name: impl Into<String>, cell: CellRef) {
        env.borrow_mut().bindings.insert(name.into(), cell);
    }

    fn define_macro(env: &EnvRef, name: impl Into<String>, definition: Rc<MacroDefinition>) {
        env.borrow_mut().macros.insert(name.into(), definition);
    }

    fn lookup_cell(env: &EnvRef, name: &str) -> Option<CellRef> {
        let (cell, parent) = {
            let borrowed = env.borrow();
            (
                borrowed.bindings.get(name).cloned(),
                borrowed.parent.clone(),
            )
        };

        if cell.is_some() {
            return cell;
        }

        parent.and_then(|parent| Self::lookup_cell(&parent, name))
    }

    fn lookup_macro(env: &EnvRef, name: &str) -> Option<Rc<MacroDefinition>> {
        let (definition, parent) = {
            let borrowed = env.borrow();
            (borrowed.macros.get(name).cloned(), borrowed.parent.clone())
        };

        if definition.is_some() {
            return definition;
        }

        parent.and_then(|parent| Self::lookup_macro(&parent, name))
    }

    fn lookup(env: &EnvRef, name: &str) -> Result<Value, EvalError> {
        if let Some(cell) = Self::lookup_cell(env, name) {
            return Ok(cell.borrow().clone());
        }

        Err(EvalError::msg(format!("unbound variable: {name}")))
    }

    fn set(env: &EnvRef, name: &str, value: Value) -> Result<(), EvalError> {
        if let Some(cell) = Self::lookup_cell(env, name) {
            *cell.borrow_mut() = value;
            return Ok(());
        }

        Err(EvalError::msg(format!("unbound variable: {name}")))
    }
}

impl Value {
    fn render(&self) -> String {
        render_value(self, RenderMode::Write)
    }

    fn display_repr(&self) -> String {
        render_value(self, RenderMode::Display)
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn as_number(&self, procedure_name: &str) -> Result<i128, EvalError> {
        match self {
            Self::Number(number) => Ok(*number),
            _ => Err(EvalError::msg(format!(
                "{procedure_name} expects numeric arguments"
            ))),
        }
    }

    fn as_string<'a>(&'a self, procedure_name: &str) -> Result<&'a str, EvalError> {
        match self {
            Self::String(value) => Ok(value),
            _ => Err(EvalError::msg(format!(
                "{procedure_name} expects string arguments"
            ))),
        }
    }

    fn as_symbol<'a>(&'a self, procedure_name: &str) -> Result<&'a str, EvalError> {
        match self {
            Self::Symbol(name) => Ok(name),
            _ => Err(EvalError::msg(format!(
                "{procedure_name} expects symbol arguments"
            ))),
        }
    }

    fn as_index(&self, procedure_name: &str) -> Result<usize, EvalError> {
        let number = self.as_number(procedure_name)?;
        if number < 0 {
            return Err(EvalError::msg(format!(
                "{procedure_name} expects a non-negative index"
            )));
        }

        usize::try_from(number)
            .map_err(|_| EvalError::msg(format!("{procedure_name} index is too large")))
    }
}

impl Procedure {
    fn render(&self) -> String {
        match self {
            Self::Builtin(builtin) => format!("#<procedure:{}>", builtin.name),
            Self::Special(special) => format!("#<procedure:{}>", special.name),
            Self::Lambda(lambda) => match &lambda.name {
                Some(name) => format!("#<procedure:{name}>"),
                None => "#<procedure>".to_string(),
            },
            Self::CaseLambda(case_lambda) => match &case_lambda.name {
                Some(name) => format!("#<procedure:{name}>"),
                None => "#<procedure>".to_string(),
            },
            Self::Continuation(_) => "#<continuation>".to_string(),
        }
    }
}

impl WindFrame {
    fn new(in_thunk: Value, out_thunk: Value) -> Self {
        Self {
            id: WIND_FRAME_COUNTER.fetch_add(1, Ordering::Relaxed),
            in_thunk,
            out_thunk,
        }
    }
}

fn default_env() -> EnvRef {
    let env = Environment::new(None);
    define_builtin(&env, "+", builtin_add);
    define_builtin(&env, "-", builtin_subtract);
    define_builtin(&env, "*", builtin_multiply);
    define_builtin(&env, "/", builtin_divide);
    define_builtin(&env, "<", builtin_less_than);
    define_builtin(&env, ">", builtin_greater_than);
    define_builtin(&env, "=", builtin_numeric_equals);
    define_builtin(&env, "equal?", builtin_equal);
    define_builtin(&env, "<=", builtin_less_equal);
    define_builtin(&env, "not", builtin_not);
    define_builtin(&env, "cons", builtin_cons);
    define_builtin(&env, "list", builtin_list);
    define_builtin(&env, "null?", builtin_null_predicate);
    define_builtin(&env, "procedure?", builtin_procedure_predicate);
    define_builtin(&env, "car", builtin_car);
    define_builtin(&env, "cdr", builtin_cdr);
    define_builtin(&env, "length", builtin_length);
    define_builtin(&env, "reverse", builtin_reverse);
    define_special(&env, "apply", SpecialProcedureKind::Apply);
    define_special(&env, "call/cc", SpecialProcedureKind::CallCc);
    define_special(
        &env,
        "call-with-current-continuation",
        SpecialProcedureKind::CallCc,
    );
    define_special(&env, "dynamic-wind", SpecialProcedureKind::DynamicWind);
    define_builtin(&env, "display", builtin_display);
    define_builtin(&env, "write", builtin_write);
    define_builtin(&env, "newline", builtin_newline);
    define_builtin(&env, "string-append", builtin_string_append);
    define_builtin(&env, "string-length", builtin_string_length);
    define_builtin(&env, "substring", builtin_substring);
    define_builtin(&env, "string->number", builtin_string_to_number);
    define_builtin(&env, "number->string", builtin_number_to_string);
    define_builtin(&env, "symbol->string", builtin_symbol_to_string);
    define_builtin(&env, "string->symbol", builtin_string_to_symbol);
    define_builtin(&env, "string-ref", builtin_string_ref);
    define_builtin(&env, "char?", builtin_char_predicate);
    env
}

fn define_builtin(
    env: &EnvRef,
    name: &'static str,
    func: fn(&[Value], &OutputRef) -> Result<Value, EvalError>,
) {
    Environment::define(
        env,
        name,
        Value::Procedure(Rc::new(Procedure::Builtin(BuiltinProcedure { name, func }))),
    );
}

fn define_special(env: &EnvRef, name: &'static str, kind: SpecialProcedureKind) {
    Environment::define(
        env,
        name,
        Value::Procedure(Rc::new(Procedure::Special(SpecialProcedure { name, kind }))),
    );
}

fn eval(expr: &Expr, env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    run_machine(
        MachineState::Expr(expr.clone(), Rc::clone(env)),
        Vec::new(),
        Vec::new(),
        output,
    )
}

fn eval_sequence_machine(
    expressions: &[Expr],
    env: &EnvRef,
    output: &OutputRef,
) -> Result<Value, EvalError> {
    let mut state = MachineState::Value(Value::Void);
    let mut continuation = Vec::new();
    start_sequence(expressions, env, &mut state, &mut continuation);
    run_machine(state, continuation, Vec::new(), output)
}

fn run_machine(
    mut state: MachineState,
    mut continuation: Vec<Frame>,
    mut wind_stack: Vec<WindFrame>,
    output: &OutputRef,
) -> Result<Value, EvalError> {
    loop {
        let current_state = std::mem::replace(&mut state, MachineState::Value(Value::Void));
        match current_state {
            MachineState::Expr(expr, env) => match expr {
                Expr::Number(number) => state = MachineState::Value(Value::Number(number)),
                Expr::Bool(value) => state = MachineState::Value(Value::Bool(value)),
                Expr::String(value) => state = MachineState::Value(Value::String(value)),
                Expr::Symbol(name) => {
                    state = MachineState::Value(Environment::lookup(&env, &name)?);
                }
                Expr::List(items) => {
                    eval_machine_list(&items, &env, &mut state, &mut continuation)?;
                }
            },
            MachineState::Value(value) => {
                let Some(frame) = continuation.pop() else {
                    return Ok(value);
                };
                handle_frame(
                    frame,
                    value,
                    &mut state,
                    &mut continuation,
                    &mut wind_stack,
                    output,
                )?;
            }
            MachineState::Apply(procedure, arguments) => {
                handle_apply(
                    procedure,
                    arguments,
                    &mut state,
                    &mut continuation,
                    &mut wind_stack,
                    output,
                )?;
            }
        }
    }
}

fn eval_machine_list(
    items: &[Expr],
    env: &EnvRef,
    state: &mut MachineState,
    continuation: &mut Vec<Frame>,
) -> Result<(), EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::msg("cannot evaluate empty list"));
    };

    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "and" => {
                start_and(tail, env, state, continuation);
                return Ok(());
            }
            "or" => {
                start_or(tail, env, state, continuation);
                return Ok(());
            }
            "begin" => {
                start_sequence(tail, env, state, continuation);
                return Ok(());
            }
            "cond" => return start_cond(tail, env, state, continuation),
            "define" => return start_define(tail, env, state, continuation),
            "define-syntax" => {
                *state = MachineState::Value(eval_define_syntax(tail, env)?);
                return Ok(());
            }
            "if" => return start_if(tail, env, state, continuation),
            "let" => return start_let_form(tail, env, state, continuation),
            "quote" => {
                *state = MachineState::Value(eval_quote(tail)?);
                return Ok(());
            }
            "set!" => return start_set(tail, env, state, continuation),
            "case-lambda" => {
                *state = MachineState::Value(eval_case_lambda(tail, env)?);
                return Ok(());
            }
            "lambda" => {
                *state = MachineState::Value(eval_lambda(tail, env)?);
                return Ok(());
            }
            _ => {}
        }

        if let Some(definition) = Environment::lookup_macro(env, name) {
            let (expanded, expanded_env) = expand_macro_call(definition.as_ref(), tail, env)?;
            *state = MachineState::Expr(expanded, expanded_env);
            return Ok(());
        }
    }

    continuation.push(Frame::CallOperator {
        arguments: tail.to_vec(),
        env: Rc::clone(env),
    });
    *state = MachineState::Expr(head.clone(), Rc::clone(env));
    Ok(())
}

fn start_sequence(
    expressions: &[Expr],
    env: &EnvRef,
    state: &mut MachineState,
    continuation: &mut Vec<Frame>,
) {
    let Some((first, rest)) = expressions.split_first() else {
        *state = MachineState::Value(Value::Void);
        return;
    };

    if !rest.is_empty() {
        continuation.push(Frame::Sequence {
            remaining: rest.to_vec(),
            env: Rc::clone(env),
        });
    }
    *state = MachineState::Expr(first.clone(), Rc::clone(env));
}

fn start_and(
    expressions: &[Expr],
    env: &EnvRef,
    state: &mut MachineState,
    continuation: &mut Vec<Frame>,
) {
    let Some((first, rest)) = expressions.split_first() else {
        *state = MachineState::Value(Value::Bool(true));
        return;
    };

    if !rest.is_empty() {
        continuation.push(Frame::And {
            remaining: rest.to_vec(),
            env: Rc::clone(env),
        });
    }
    *state = MachineState::Expr(first.clone(), Rc::clone(env));
}

fn start_or(
    expressions: &[Expr],
    env: &EnvRef,
    state: &mut MachineState,
    continuation: &mut Vec<Frame>,
) {
    let Some((first, rest)) = expressions.split_first() else {
        *state = MachineState::Value(Value::Bool(false));
        return;
    };

    if !rest.is_empty() {
        continuation.push(Frame::Or {
            remaining: rest.to_vec(),
            env: Rc::clone(env),
        });
    }
    *state = MachineState::Expr(first.clone(), Rc::clone(env));
}

fn start_cond(
    clauses: &[Expr],
    env: &EnvRef,
    state: &mut MachineState,
    continuation: &mut Vec<Frame>,
) -> Result<(), EvalError> {
    let Some((clause, rest)) = clauses.split_first() else {
        *state = MachineState::Value(Value::Void);
        return Ok(());
    };

    let Expr::List(items) = clause else {
        return Err(EvalError::msg("cond clauses must be lists"));
    };
    let Some((test, body)) = items.split_first() else {
        return Err(EvalError::msg("cond clauses cannot be empty"));
    };

    if matches!(test, Expr::Symbol(name) if name == "else") {
        if !rest.is_empty() {
            return Err(EvalError::msg("cond else clause must be last"));
        }
        start_sequence(body, env, state, continuation);
        return Ok(());
    }

    continuation.push(Frame::Cond {
        body: body.to_vec(),
        remaining_clauses: rest.to_vec(),
        env: Rc::clone(env),
    });
    *state = MachineState::Expr(test.clone(), Rc::clone(env));
    Ok(())
}

fn start_define(
    args: &[Expr],
    env: &EnvRef,
    state: &mut MachineState,
    continuation: &mut Vec<Frame>,
) -> Result<(), EvalError> {
    if args.len() < 2 {
        return Err(EvalError::msg("define requires a target and value"));
    }

    match &args[0] {
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::msg(
                    "define variable form requires exactly one value",
                ));
            }

            continuation.push(Frame::DefineValue {
                name: name.clone(),
                env: Rc::clone(env),
            });
            *state = MachineState::Expr(args[1].clone(), Rc::clone(env));
            Ok(())
        }
        Expr::List(signature) => {
            let Some((name_expr, params)) = signature.split_first() else {
                return Err(EvalError::msg("define function form requires a name"));
            };

            let Expr::Symbol(name) = name_expr else {
                return Err(EvalError::msg("function name must be a symbol"));
            };

            let formals = Expr::List(params.to_vec());
            let lambda = build_lambda(&formals, &args[1..], env, Some(name.clone()))?;
            Environment::define(env, name.clone(), lambda);
            *state = MachineState::Value(Value::Void);
            Ok(())
        }
        _ => Err(EvalError::msg(
            "define target must be a symbol or parameter list",
        )),
    }
}

fn start_if(
    args: &[Expr],
    env: &EnvRef,
    state: &mut MachineState,
    continuation: &mut Vec<Frame>,
) -> Result<(), EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::msg(
            "if requires a condition, then branch, and optional else branch",
        ));
    }

    continuation.push(Frame::If {
        then_expr: args[1].clone(),
        else_expr: args.get(2).cloned(),
        env: Rc::clone(env),
    });
    *state = MachineState::Expr(args[0].clone(), Rc::clone(env));
    Ok(())
}

fn start_set(
    args: &[Expr],
    env: &EnvRef,
    state: &mut MachineState,
    continuation: &mut Vec<Frame>,
) -> Result<(), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::msg("set! requires a target and value"));
    }

    let Expr::Symbol(name) = &args[0] else {
        return Err(EvalError::msg("set! target must be a symbol"));
    };

    continuation.push(Frame::SetValue {
        name: name.clone(),
        env: Rc::clone(env),
    });
    *state = MachineState::Expr(args[1].clone(), Rc::clone(env));
    Ok(())
}

fn start_let_form(
    args: &[Expr],
    env: &EnvRef,
    state: &mut MachineState,
    continuation: &mut Vec<Frame>,
) -> Result<(), EvalError> {
    if args.len() < 2 {
        return Err(EvalError::msg("let requires bindings and body"));
    }

    if let Expr::Symbol(name) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::msg("let requires bindings and body"));
        }

        let Expr::List(bindings) = &args[1] else {
            return Err(EvalError::msg("let bindings must be a list"));
        };

        let (params, value_exprs) = parse_let_binding_exprs(bindings)?;
        let named_env = Environment::new(Some(Rc::clone(env)));
        let procedure = Value::Procedure(Rc::new(Procedure::Lambda(LambdaProcedure {
            name: Some(name.clone()),
            params,
            rest: None,
            body: args[2..].to_vec(),
            env: Rc::clone(&named_env),
        })));
        Environment::define(&named_env, name.clone(), procedure.clone());

        if let Some((first, rest)) = value_exprs.split_first() {
            continuation.push(Frame::NamedLet {
                procedure,
                remaining_exprs: rest.to_vec(),
                evaluated: Vec::new(),
                env: Rc::clone(env),
            });
            *state = MachineState::Expr(first.clone(), Rc::clone(env));
        } else {
            *state = MachineState::Apply(procedure, Vec::new());
        }
        return Ok(());
    }

    let Expr::List(bindings) = &args[0] else {
        return Err(EvalError::msg("let bindings must be a list"));
    };

    let (names, value_exprs) = parse_let_binding_exprs(bindings)?;
    if let Some((first, rest)) = value_exprs.split_first() {
        continuation.push(Frame::Let {
            names,
            remaining_exprs: rest.to_vec(),
            evaluated: Vec::new(),
            body: args[1..].to_vec(),
            env: Rc::clone(env),
        });
        *state = MachineState::Expr(first.clone(), Rc::clone(env));
    } else {
        let let_env = Environment::new(Some(Rc::clone(env)));
        start_sequence(&args[1..], &let_env, state, continuation);
    }
    Ok(())
}

fn parse_let_binding_exprs(bindings: &[Expr]) -> Result<(Vec<String>, Vec<Expr>), EvalError> {
    let mut names = Vec::with_capacity(bindings.len());
    let mut values = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let Expr::List(parts) = binding else {
            return Err(EvalError::msg("let bindings must be pairs"));
        };
        if parts.len() != 2 {
            return Err(EvalError::msg("let bindings must be pairs"));
        }

        let Expr::Symbol(name) = &parts[0] else {
            return Err(EvalError::msg("let binding names must be symbols"));
        };

        names.push(name.clone());
        values.push(parts[1].clone());
    }

    Ok((names, values))
}

fn handle_frame(
    frame: Frame,
    value: Value,
    state: &mut MachineState,
    continuation: &mut Vec<Frame>,
    wind_stack: &mut Vec<WindFrame>,
    _output: &OutputRef,
) -> Result<(), EvalError> {
    match frame {
        Frame::Sequence { remaining, env } => {
            start_sequence(&remaining, &env, state, continuation);
        }
        Frame::If {
            then_expr,
            else_expr,
            env,
        } => {
            let next = if value.is_truthy() {
                then_expr
            } else if let Some(else_expr) = else_expr {
                else_expr
            } else {
                *state = MachineState::Value(Value::Void);
                return Ok(());
            };
            *state = MachineState::Expr(next, env);
        }
        Frame::DefineValue { name, env } => {
            Environment::define(&env, name, value);
            *state = MachineState::Value(Value::Void);
        }
        Frame::SetValue { name, env } => {
            Environment::set(&env, &name, value)?;
            *state = MachineState::Value(Value::Void);
        }
        Frame::And { remaining, env } => {
            if !value.is_truthy() {
                *state = MachineState::Value(value);
            } else if let Some((next, rest)) = remaining.split_first() {
                if !rest.is_empty() {
                    continuation.push(Frame::And {
                        remaining: rest.to_vec(),
                        env: Rc::clone(&env),
                    });
                }
                *state = MachineState::Expr(next.clone(), env);
            } else {
                *state = MachineState::Value(value);
            }
        }
        Frame::Or { remaining, env } => {
            if value.is_truthy() {
                *state = MachineState::Value(value);
            } else if let Some((next, rest)) = remaining.split_first() {
                if !rest.is_empty() {
                    continuation.push(Frame::Or {
                        remaining: rest.to_vec(),
                        env: Rc::clone(&env),
                    });
                }
                *state = MachineState::Expr(next.clone(), env);
            } else {
                *state = MachineState::Value(value);
            }
        }
        Frame::Cond {
            body,
            remaining_clauses,
            env,
        } => {
            if value.is_truthy() {
                if body.is_empty() {
                    *state = MachineState::Value(value);
                } else {
                    start_sequence(&body, &env, state, continuation);
                }
            } else {
                start_cond(&remaining_clauses, &env, state, continuation)?;
            }
        }
        Frame::CallOperator { arguments, env } => {
            if let Some((next, rest)) = arguments.split_first() {
                continuation.push(Frame::CallArgument {
                    procedure: value,
                    evaluated: Vec::new(),
                    remaining: rest.to_vec(),
                    env: Rc::clone(&env),
                });
                *state = MachineState::Expr(next.clone(), env);
            } else {
                *state = MachineState::Apply(value, Vec::new());
            }
        }
        Frame::CallArgument {
            procedure,
            mut evaluated,
            remaining,
            env,
        } => {
            evaluated.push(value);
            if let Some((next, rest)) = remaining.split_first() {
                continuation.push(Frame::CallArgument {
                    procedure,
                    evaluated,
                    remaining: rest.to_vec(),
                    env: Rc::clone(&env),
                });
                *state = MachineState::Expr(next.clone(), env);
            } else {
                *state = MachineState::Apply(procedure, evaluated);
            }
        }
        Frame::Let {
            names,
            remaining_exprs,
            mut evaluated,
            body,
            env,
        } => {
            evaluated.push(value);
            if let Some((next, rest)) = remaining_exprs.split_first() {
                continuation.push(Frame::Let {
                    names,
                    remaining_exprs: rest.to_vec(),
                    evaluated,
                    body,
                    env: Rc::clone(&env),
                });
                *state = MachineState::Expr(next.clone(), env);
            } else {
                let let_env = Environment::new(Some(Rc::clone(&env)));
                for (name, value) in names.into_iter().zip(evaluated) {
                    Environment::define(&let_env, name, value);
                }
                start_sequence(&body, &let_env, state, continuation);
            }
        }
        Frame::NamedLet {
            procedure,
            remaining_exprs,
            mut evaluated,
            env,
        } => {
            evaluated.push(value);
            if let Some((next, rest)) = remaining_exprs.split_first() {
                continuation.push(Frame::NamedLet {
                    procedure,
                    remaining_exprs: rest.to_vec(),
                    evaluated,
                    env: Rc::clone(&env),
                });
                *state = MachineState::Expr(next.clone(), env);
            } else {
                *state = MachineState::Apply(procedure, evaluated);
            }
        }
        Frame::DynamicWindAfterIn { frame, body_thunk } => {
            wind_stack.push(frame.clone());
            continuation.push(Frame::DynamicWindAfterBody { frame });
            *state = MachineState::Apply(body_thunk, Vec::new());
        }
        Frame::DynamicWindAfterBody { frame } => {
            let popped = wind_stack
                .pop()
                .ok_or_else(|| EvalError::msg("dynamic-wind stack underflow"))?;
            if popped.id != frame.id {
                return Err(EvalError::msg("dynamic-wind stack mismatch"));
            }
            continuation.push(Frame::DynamicWindAfterOut { result: value });
            *state = MachineState::Apply(frame.out_thunk.clone(), Vec::new());
        }
        Frame::DynamicWindAfterOut { result } => {
            *state = MachineState::Value(result);
        }
        Frame::WindTransition {
            pending_push,
            remaining,
            target_continuation,
            target_wind_stack,
            value: jump_value,
        } => {
            if let Some(frame) = pending_push {
                wind_stack.push(frame);
            }
            continue_wind_transition(
                remaining,
                target_continuation,
                target_wind_stack,
                jump_value,
                state,
                continuation,
                wind_stack,
            )?;
        }
    }

    Ok(())
}

fn handle_apply(
    procedure: Value,
    arguments: Vec<Value>,
    state: &mut MachineState,
    continuation: &mut Vec<Frame>,
    wind_stack: &mut Vec<WindFrame>,
    output: &OutputRef,
) -> Result<(), EvalError> {
    let procedure = into_procedure(procedure)?;
    match procedure.as_ref() {
        Procedure::Builtin(builtin) => {
            *state = MachineState::Value((builtin.func)(&arguments, output)?);
        }
        Procedure::Special(special) => {
            handle_special_apply(
                *special,
                arguments,
                state,
                continuation,
                wind_stack,
                output,
            )?;
        }
        Procedure::Lambda(lambda) => {
            let call_env = bind_lambda_call(lambda, arguments)?;
            start_sequence(&lambda.body, &call_env, state, continuation);
        }
        Procedure::CaseLambda(case_lambda) => {
            let clause = select_case_lambda_clause(case_lambda, arguments.len())?;
            let call_env = bind_lambda_call(clause, arguments)?;
            start_sequence(&clause.body, &call_env, state, continuation);
        }
        Procedure::Continuation(captured) => {
            ensure_exactly("continuation", arguments.len(), 1)?;
            continuation.clear();
            let jump_value = arguments
                .into_iter()
                .next()
                .expect("arity check ensures a single continuation argument");
            let steps = compute_wind_transition_steps(wind_stack, &captured.wind_stack);
            continue_wind_transition(
                steps,
                captured.continuation.clone(),
                captured.wind_stack.clone(),
                jump_value,
                state,
                continuation,
                wind_stack,
            )?;
        }
    }
    Ok(())
}

fn handle_special_apply(
    special: SpecialProcedure,
    arguments: Vec<Value>,
    state: &mut MachineState,
    continuation: &mut Vec<Frame>,
    wind_stack: &mut Vec<WindFrame>,
    _output: &OutputRef,
) -> Result<(), EvalError> {
    match special.kind {
        SpecialProcedureKind::Apply => {
            ensure_at_least("apply", arguments.len(), 2)?;
            let trailing_args = match arguments.last() {
                Some(Value::List(values)) => values.clone(),
                Some(_) => {
                    return Err(EvalError::msg("apply expects a list as its last argument"));
                }
                None => unreachable!("arity check guarantees at least two arguments"),
            };

            let mut applied_args =
                Vec::with_capacity(arguments.len().saturating_sub(1) + trailing_args.len());
            applied_args.extend(arguments[1..arguments.len() - 1].iter().cloned());
            applied_args.extend(trailing_args);
            *state = MachineState::Apply(arguments[0].clone(), applied_args);
        }
        SpecialProcedureKind::CallCc => {
            ensure_exactly(special.name, arguments.len(), 1)?;
            let captured = Value::Procedure(Rc::new(Procedure::Continuation(Rc::new(
                ContinuationProcedure {
                    continuation: continuation.clone(),
                    wind_stack: wind_stack.clone(),
                },
            ))));
            *state = MachineState::Apply(arguments[0].clone(), vec![captured]);
        }
        SpecialProcedureKind::DynamicWind => {
            ensure_exactly("dynamic-wind", arguments.len(), 3)?;
            let frame = WindFrame::new(arguments[0].clone(), arguments[2].clone());
            continuation.push(Frame::DynamicWindAfterIn {
                frame,
                body_thunk: arguments[1].clone(),
            });
            *state = MachineState::Apply(arguments[0].clone(), Vec::new());
        }
    }
    Ok(())
}

fn compute_wind_transition_steps(
    current: &[WindFrame],
    target: &[WindFrame],
) -> Vec<WindTransitionStep> {
    let mut shared = 0;
    while shared < current.len() && shared < target.len() && current[shared].id == target[shared].id
    {
        shared += 1;
    }

    let mut steps = Vec::with_capacity((current.len() - shared) + (target.len() - shared));
    for frame in current[shared..].iter().rev() {
        steps.push(WindTransitionStep::Out(frame.clone()));
    }
    for frame in &target[shared..] {
        steps.push(WindTransitionStep::In(frame.clone()));
    }
    steps
}

fn continue_wind_transition(
    steps: Vec<WindTransitionStep>,
    target_continuation: Vec<Frame>,
    target_wind_stack: Vec<WindFrame>,
    value: Value,
    state: &mut MachineState,
    continuation: &mut Vec<Frame>,
    wind_stack: &mut Vec<WindFrame>,
) -> Result<(), EvalError> {
    let Some((first, rest)) = steps.split_first() else {
        *continuation = target_continuation;
        *wind_stack = target_wind_stack;
        *state = MachineState::Value(value);
        return Ok(());
    };

    match first {
        WindTransitionStep::Out(frame) => {
            let popped = wind_stack
                .pop()
                .ok_or_else(|| EvalError::msg("dynamic-wind stack underflow"))?;
            if popped.id != frame.id {
                return Err(EvalError::msg("dynamic-wind stack mismatch"));
            }
            continuation.push(Frame::WindTransition {
                pending_push: None,
                remaining: rest.to_vec(),
                target_continuation,
                target_wind_stack,
                value,
            });
            *state = MachineState::Apply(frame.out_thunk.clone(), Vec::new());
        }
        WindTransitionStep::In(frame) => {
            continuation.push(Frame::WindTransition {
                pending_push: Some(frame.clone()),
                remaining: rest.to_vec(),
                target_continuation,
                target_wind_stack,
                value,
            });
            *state = MachineState::Apply(frame.in_thunk.clone(), Vec::new());
        }
    }

    Ok(())
}

fn eval_list(items: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::msg("cannot evaluate empty list"));
    };

    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_and(tail, env, output),
            "or" => return eval_or(tail, env, output),
            "begin" => return eval_begin(tail, env, output),
            "cond" => return eval_cond(tail, env, output),
            "define" => return eval_define(tail, env, output),
            "define-syntax" => return eval_define_syntax(tail, env),
            "if" => return eval_if(tail, env, output),
            "let" => return eval_let(tail, env, output),
            "quote" => return eval_quote(tail),
            "set!" => return eval_set(tail, env, output),
            "case-lambda" => return eval_case_lambda(tail, env),
            "lambda" => return eval_lambda(tail, env),
            _ => {}
        }

        if let Some(definition) = Environment::lookup_macro(env, name) {
            let (expanded, expanded_env) = expand_macro_call(definition.as_ref(), tail, env)?;
            return eval(&expanded, &expanded_env, output);
        }
    }

    let procedure = eval(head, env, output)?;
    let mut arguments = Vec::with_capacity(tail.len());
    for expression in tail {
        arguments.push(eval(expression, env, output)?);
    }
    apply(procedure, arguments, output)
}

fn eval_define(args: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::msg("define requires a target and value"));
    }

    match &args[0] {
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::msg(
                    "define variable form requires exactly one value",
                ));
            }

            let value = eval(&args[1], env, output)?;
            Environment::define(env, name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) => {
            let Some((name_expr, params)) = signature.split_first() else {
                return Err(EvalError::msg("define function form requires a name"));
            };

            let Expr::Symbol(name) = name_expr else {
                return Err(EvalError::msg("function name must be a symbol"));
            };

            let formals = Expr::List(params.to_vec());
            let lambda = build_lambda(&formals, &args[1..], env, Some(name.clone()))?;
            Environment::define(env, name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::msg(
            "define target must be a symbol or parameter list",
        )),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::msg(
            "if requires a condition, then branch, and optional else branch",
        ));
    }

    if eval(&args[0], env, output)?.is_truthy() {
        eval(&args[1], env, output)
    } else if args.len() == 3 {
        eval(&args[2], env, output)
    } else {
        Ok(Value::Void)
    }
}

fn eval_let(args: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::msg("let requires bindings and body"));
    }

    if let Expr::Symbol(name) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::msg("let requires bindings and body"));
        }

        let Expr::List(bindings) = &args[1] else {
            return Err(EvalError::msg("let bindings must be a list"));
        };

        let (procedure, arguments) = build_named_let_call(name, bindings, &args[2..], env, output)?;
        return apply(procedure, arguments, output);
    }

    let Expr::List(bindings) = &args[0] else {
        return Err(EvalError::msg("let bindings must be a list"));
    };

    let let_env = Environment::new(Some(Rc::clone(env)));
    let (names, values) = parse_let_bindings(bindings, env, output)?;
    for (name, value) in names.into_iter().zip(values) {
        Environment::define(&let_env, name, value);
    }

    eval_sequence(&args[1..], &let_env, output)
}

fn eval_cond(args: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::msg("cond requires at least one clause"));
    }

    for (index, clause) in args.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::msg("cond clauses must be lists"));
        };
        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::msg("cond clauses cannot be empty"));
        };

        if matches!(test, Expr::Symbol(name) if name == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::msg("cond else clause must be last"));
            }
            return eval_sequence(body, env, output);
        }

        let test_value = eval(test, env, output)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_sequence(body, env, output)
            };
        }
    }

    Ok(Value::Void)
}

fn parse_let_bindings(
    bindings: &[Expr],
    env: &EnvRef,
    output: &OutputRef,
) -> Result<(Vec<String>, Vec<Value>), EvalError> {
    let mut names = Vec::with_capacity(bindings.len());
    let mut values = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let Expr::List(parts) = binding else {
            return Err(EvalError::msg("let bindings must be pairs"));
        };
        if parts.len() != 2 {
            return Err(EvalError::msg("let bindings must be pairs"));
        }

        let Expr::Symbol(name) = &parts[0] else {
            return Err(EvalError::msg("let binding names must be symbols"));
        };

        names.push(name.clone());
        values.push(eval(&parts[1], env, output)?);
    }

    Ok((names, values))
}

fn build_named_let_call(
    name: &str,
    bindings: &[Expr],
    body: &[Expr],
    env: &EnvRef,
    output: &OutputRef,
) -> Result<(Value, Vec<Value>), EvalError> {
    let (params, arguments) = parse_let_bindings(bindings, env, output)?;
    let named_env = Environment::new(Some(Rc::clone(env)));
    let procedure = Value::Procedure(Rc::new(Procedure::Lambda(LambdaProcedure {
        name: Some(name.to_string()),
        params,
        rest: None,
        body: body.to_vec(),
        env: Rc::clone(&named_env),
    })));

    Environment::define(&named_env, name.to_string(), procedure.clone());
    Ok((procedure, arguments))
}

fn eval_set(args: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::msg("set! requires a target and value"));
    }

    let Expr::Symbol(name) = &args[0] else {
        return Err(EvalError::msg("set! target must be a symbol"));
    };

    let value = eval(&args[1], env, output)?;
    Environment::set(env, name, value)?;
    Ok(Value::Void)
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::msg("quote requires exactly one argument"));
    }

    Ok(quote_expr(&args[0]))
}

fn eval_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::msg("lambda requires a parameter list and body"));
    }

    build_lambda(&args[0], &args[1..], env, None)
}

fn eval_case_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    build_case_lambda(args, env, None)
}

fn eval_define_syntax(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::msg(
            "define-syntax requires a name and transformer",
        ));
    }

    let Expr::Symbol(name) = &args[0] else {
        return Err(EvalError::msg("define-syntax name must be a symbol"));
    };

    let definition = parse_syntax_rules(name, &args[1], env)?;
    Environment::define_macro(env, name.clone(), Rc::new(definition));
    Ok(Value::Void)
}

fn build_lambda(
    formals: &Expr,
    body: &[Expr],
    env: &EnvRef,
    name: Option<String>,
) -> Result<Value, EvalError> {
    let lambda = build_lambda_procedure(formals, body, env, name)?;
    Ok(Value::Procedure(Rc::new(Procedure::Lambda(lambda))))
}

fn build_case_lambda(
    clauses: &[Expr],
    env: &EnvRef,
    name: Option<String>,
) -> Result<Value, EvalError> {
    if clauses.is_empty() {
        return Err(EvalError::msg("case-lambda requires at least one clause"));
    }

    let mut parsed_clauses = Vec::with_capacity(clauses.len());
    for clause in clauses {
        let Expr::List(items) = clause else {
            return Err(EvalError::msg("case-lambda clauses must be lists"));
        };

        let Some((formals, body)) = items.split_first() else {
            return Err(EvalError::msg(
                "case-lambda clauses require parameters and body",
            ));
        };

        parsed_clauses.push(build_lambda_procedure(formals, body, env, name.clone())?);
    }

    Ok(Value::Procedure(Rc::new(Procedure::CaseLambda(
        CaseLambdaProcedure {
            name,
            clauses: parsed_clauses,
        },
    ))))
}

fn build_lambda_procedure(
    formals: &Expr,
    body: &[Expr],
    env: &EnvRef,
    name: Option<String>,
) -> Result<LambdaProcedure, EvalError> {
    if body.is_empty() {
        return Err(EvalError::msg(
            "lambda requires at least one body expression",
        ));
    }

    let (param_names, rest_param) = parse_formals(formals)?;

    Ok(LambdaProcedure {
        name,
        params: param_names,
        rest: rest_param,
        body: body.to_vec(),
        env: Rc::clone(env),
    })
}

fn parse_formals(formals: &Expr) -> Result<(Vec<String>, Option<String>), EvalError> {
    match formals {
        Expr::List(params) => parse_parameter_list(params),
        Expr::Symbol(name) => Ok((Vec::new(), Some(name.clone()))),
        _ => Err(EvalError::msg("lambda parameters must be a list or symbol")),
    }
}

fn parse_parameter_list(params: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut param_names = Vec::with_capacity(params.len());
    let mut saw_dot = false;
    let mut rest_param = None;

    for (index, param) in params.iter().enumerate() {
        let Expr::Symbol(name) = param else {
            return Err(EvalError::msg("lambda parameters must be symbols"));
        };

        if name == "." {
            if saw_dot || index + 1 >= params.len() {
                return Err(EvalError::msg("invalid dotted parameter list"));
            }
            saw_dot = true;
            continue;
        }

        if saw_dot {
            if index + 1 != params.len() {
                return Err(EvalError::msg("invalid dotted parameter list"));
            }
            rest_param = Some(name.clone());
            break;
        }

        param_names.push(name.clone());
    }

    if saw_dot && rest_param.is_none() {
        return Err(EvalError::msg("invalid dotted parameter list"));
    }

    Ok((param_names, rest_param))
}

fn parse_syntax_rules(
    macro_name: &str,
    expr: &Expr,
    env: &EnvRef,
) -> Result<MacroDefinition, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::msg(
            "define-syntax expects a syntax-rules transformer",
        ));
    };

    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::msg("syntax-rules form cannot be empty"));
    };

    let Expr::Symbol(name) = head else {
        return Err(EvalError::msg("invalid syntax-rules transformer"));
    };
    if name != "syntax-rules" {
        return Err(EvalError::msg(
            "define-syntax expects a syntax-rules transformer",
        ));
    }
    if tail.len() < 2 {
        return Err(EvalError::msg("syntax-rules requires literals and rules"));
    }

    let Expr::List(literal_exprs) = &tail[0] else {
        return Err(EvalError::msg("syntax-rules literals must be a list"));
    };

    let mut literals = HashSet::with_capacity(literal_exprs.len());
    for literal in literal_exprs {
        let Expr::Symbol(name) = literal else {
            return Err(EvalError::msg("syntax-rules literals must be symbols"));
        };
        literals.insert(name.clone());
    }

    let mut rules = Vec::with_capacity(tail.len() - 1);
    for rule_expr in &tail[1..] {
        let Expr::List(rule_items) = rule_expr else {
            return Err(EvalError::msg("syntax-rules rules must be lists"));
        };
        if rule_items.len() != 2 {
            return Err(EvalError::msg(
                "syntax-rules rules must contain a pattern and template",
            ));
        }

        let Expr::List(pattern_items) = &rule_items[0] else {
            return Err(EvalError::msg("syntax-rules patterns must be lists"));
        };
        let Some((keyword_expr, pattern_tail)) = pattern_items.split_first() else {
            return Err(EvalError::msg("syntax-rules pattern cannot be empty"));
        };
        let Expr::Symbol(keyword) = keyword_expr else {
            return Err(EvalError::msg(
                "syntax-rules pattern must start with a symbol",
            ));
        };
        if keyword != macro_name {
            return Err(EvalError::msg(format!(
                "syntax-rules pattern must start with {macro_name}"
            )));
        }

        rules.push(MacroRule {
            pattern: pattern_tail.to_vec(),
            template: rule_items[1].clone(),
        });
    }

    if rules.is_empty() {
        return Err(EvalError::msg("syntax-rules requires at least one rule"));
    }

    Ok(MacroDefinition {
        name: macro_name.to_string(),
        literals,
        rules,
        env: Rc::clone(env),
    })
}

fn expand_macro_call(
    definition: &MacroDefinition,
    arguments: &[Expr],
    call_env: &EnvRef,
) -> Result<(Expr, EnvRef), EvalError> {
    for rule in &definition.rules {
        let mut bindings = HashMap::new();
        if match_list_pattern(
            &rule.pattern,
            arguments,
            &definition.literals,
            &mut bindings,
        ) {
            let expansion_env = Environment::new(Some(Rc::clone(call_env)));
            let mut expander = TemplateExpander::new(definition, &expansion_env, bindings);
            let expanded = expander.expand(&rule.template)?;
            return Ok((expanded, expansion_env));
        }
    }

    Err(EvalError::msg(format!(
        "no matching syntax-rules clause for {}",
        definition.name
    )))
}

fn match_list_pattern(
    pattern_items: &[Expr],
    input_items: &[Expr],
    literals: &HashSet<String>,
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    if pattern_items.len() >= 2 && is_ellipsis(&pattern_items[pattern_items.len() - 1]) {
        let repeat_pattern = &pattern_items[pattern_items.len() - 2];
        let fixed = &pattern_items[..pattern_items.len() - 2];
        if input_items.len() < fixed.len() {
            return false;
        }

        for (pattern, input) in fixed.iter().zip(&input_items[..fixed.len()]) {
            if !match_pattern(pattern, input, literals, bindings) {
                return false;
            }
        }

        return match_repeated_pattern(
            repeat_pattern,
            &input_items[fixed.len()..],
            literals,
            bindings,
        );
    }

    if pattern_items.len() != input_items.len() {
        return false;
    }

    for (pattern, input) in pattern_items.iter().zip(input_items) {
        if !match_pattern(pattern, input, literals, bindings) {
            return false;
        }
    }

    true
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &HashSet<String>,
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    match pattern {
        Expr::Number(number) => matches!(input, Expr::Number(other) if other == number),
        Expr::Bool(value) => matches!(input, Expr::Bool(other) if other == value),
        Expr::String(value) => matches!(input, Expr::String(other) if other == value),
        Expr::Symbol(name) => {
            if name == "..." {
                return false;
            }
            if literals.contains(name) {
                return matches!(input, Expr::Symbol(other) if other == name);
            }
            bind_pattern_variable(name, PatternBinding::Single(input.clone()), bindings)
        }
        Expr::List(pattern_items) => match input {
            Expr::List(input_items) => {
                match_list_pattern(pattern_items, input_items, literals, bindings)
            }
            _ => false,
        },
    }
}

fn match_repeated_pattern(
    pattern: &Expr,
    inputs: &[Expr],
    literals: &HashSet<String>,
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    match pattern {
        Expr::Symbol(name) if name != "..." && !literals.contains(name) => {
            bind_pattern_variable(name, PatternBinding::Sequence(inputs.to_vec()), bindings)
        }
        _ => false,
    }
}

fn bind_pattern_variable(
    name: &str,
    binding: PatternBinding,
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    match bindings.get(name) {
        Some(existing) => existing == &binding,
        None => {
            bindings.insert(name.to_string(), binding);
            true
        }
    }
}

struct TemplateExpander<'a> {
    definition: &'a MacroDefinition,
    alias_env: EnvRef,
    bindings: HashMap<String, PatternBinding>,
    renamed: HashMap<String, String>,
}

impl<'a> TemplateExpander<'a> {
    fn new(
        definition: &'a MacroDefinition,
        alias_env: &EnvRef,
        bindings: HashMap<String, PatternBinding>,
    ) -> Self {
        Self {
            definition,
            alias_env: Rc::clone(alias_env),
            bindings,
            renamed: HashMap::new(),
        }
    }

    fn expand(&mut self, expr: &Expr) -> Result<Expr, EvalError> {
        match expr {
            Expr::Number(_) | Expr::Bool(_) | Expr::String(_) => Ok(expr.clone()),
            Expr::Symbol(name) => self.expand_symbol(name),
            Expr::List(items) => self.expand_list(items),
        }
    }

    fn expand_symbol(&mut self, name: &str) -> Result<Expr, EvalError> {
        match self.bindings.get(name) {
            Some(PatternBinding::Single(value)) => Ok(value.clone()),
            Some(PatternBinding::Sequence(_)) => Err(EvalError::msg(format!(
                "macro template used repeated variable {name} without ellipsis"
            ))),
            None => Ok(Expr::Symbol(self.hygienic_name(name))),
        }
    }

    fn expand_list(&mut self, items: &[Expr]) -> Result<Expr, EvalError> {
        let mut expanded = Vec::with_capacity(items.len());
        let mut index = 0;

        while index < items.len() {
            if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
                self.expand_repetition(&items[index], &mut expanded)?;
                index += 2;
                continue;
            }

            expanded.push(self.expand(&items[index])?);
            index += 1;
        }

        Ok(Expr::List(expanded))
    }

    fn expand_repetition(
        &mut self,
        expr: &Expr,
        expanded: &mut Vec<Expr>,
    ) -> Result<(), EvalError> {
        match expr {
            Expr::Symbol(name) => match self.bindings.get(name) {
                Some(PatternBinding::Sequence(values)) => {
                    expanded.extend(values.iter().cloned());
                    Ok(())
                }
                Some(PatternBinding::Single(_)) => Err(EvalError::msg(format!(
                    "macro template repeated non-sequence variable {name}"
                ))),
                None => Err(EvalError::msg(format!(
                    "macro template uses ellipsis with non-pattern variable {name}"
                ))),
            },
            _ => Err(EvalError::msg(
                "macro templates only support identifier ellipses at this level",
            )),
        }
    }

    fn hygienic_name(&mut self, name: &str) -> String {
        if name == "..."
            || name == "."
            || name == self.definition.name
            || is_core_syntax(name)
            || Environment::lookup_macro(&self.definition.env, name).is_some()
        {
            return name.to_string();
        }

        if let Some(existing) = self.renamed.get(name) {
            return existing.clone();
        }

        let fresh = fresh_macro_name(name);
        if let Some(cell) = Environment::lookup_cell(&self.definition.env, name) {
            Environment::define_cell(&self.alias_env, fresh.clone(), cell);
        }
        self.renamed.insert(name.to_string(), fresh.clone());
        fresh
    }
}

fn is_core_syntax(name: &str) -> bool {
    matches!(
        name,
        "and"
            | "or"
            | "begin"
            | "case-lambda"
            | "cond"
            | "define"
            | "define-syntax"
            | "if"
            | "lambda"
            | "let"
            | "quote"
            | "set!"
            | "syntax-rules"
    )
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(name) if name == "...")
}

fn fresh_macro_name(name: &str) -> String {
    let counter = MACRO_GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    let sanitized: String = name
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect();
    let suffix = if sanitized.is_empty() {
        "_"
    } else {
        &sanitized
    };
    format!("__macro_{counter}_{suffix}")
}

fn eval_sequence(
    expressions: &[Expr],
    env: &EnvRef,
    output: &OutputRef,
) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expression in expressions {
        last = eval(expression, env, output)?;
    }
    Ok(last)
}

fn eval_tail_expression(
    expr: &Expr,
    env: &EnvRef,
    output: &OutputRef,
) -> Result<TailResult, EvalError> {
    match expr {
        Expr::Number(number) => Ok(TailResult::Value(Value::Number(*number))),
        Expr::Bool(value) => Ok(TailResult::Value(Value::Bool(*value))),
        Expr::String(value) => Ok(TailResult::Value(Value::String(value.clone()))),
        Expr::Symbol(name) => Ok(TailResult::Value(Environment::lookup(env, name)?)),
        Expr::List(items) => eval_tail_list(items, env, output),
    }
}

fn eval_tail_list(
    items: &[Expr],
    env: &EnvRef,
    output: &OutputRef,
) -> Result<TailResult, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::msg("cannot evaluate empty list"));
    };

    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_tail_and(tail, env, output),
            "or" => return eval_tail_or(tail, env, output),
            "begin" => return eval_tail_begin(tail, env, output),
            "cond" => return eval_tail_cond(tail, env, output),
            "define" => return eval_define(tail, env, output).map(TailResult::Value),
            "define-syntax" => return eval_define_syntax(tail, env).map(TailResult::Value),
            "if" => return eval_tail_if(tail, env, output),
            "let" => return eval_tail_let(tail, env, output),
            "quote" => return eval_quote(tail).map(TailResult::Value),
            "set!" => return eval_set(tail, env, output).map(TailResult::Value),
            "case-lambda" => return eval_case_lambda(tail, env).map(TailResult::Value),
            "lambda" => return eval_lambda(tail, env).map(TailResult::Value),
            _ => {}
        }

        if let Some(definition) = Environment::lookup_macro(env, name) {
            let (expanded, expanded_env) = expand_macro_call(definition.as_ref(), tail, env)?;
            return eval_tail_expression(&expanded, &expanded_env, output);
        }
    }

    let procedure = eval(head, env, output)?;
    if !matches!(procedure, Value::Procedure(_)) {
        return Err(EvalError::msg("attempted to call a non-procedure"));
    }

    let mut arguments = Vec::with_capacity(tail.len());
    for expression in tail {
        arguments.push(eval(expression, env, output)?);
    }
    Ok(TailResult::Call(procedure, arguments))
}

fn eval_tail_sequence(
    expressions: &[Expr],
    env: &EnvRef,
    output: &OutputRef,
) -> Result<TailResult, EvalError> {
    let Some((last, prefix)) = expressions.split_last() else {
        return Ok(TailResult::Value(Value::Void));
    };

    for expression in prefix {
        eval(expression, env, output)?;
    }
    eval_tail_expression(last, env, output)
}

fn eval_tail_if(args: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<TailResult, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::msg(
            "if requires a condition, then branch, and optional else branch",
        ));
    }

    if eval(&args[0], env, output)?.is_truthy() {
        eval_tail_expression(&args[1], env, output)
    } else if args.len() == 3 {
        eval_tail_expression(&args[2], env, output)
    } else {
        Ok(TailResult::Value(Value::Void))
    }
}

fn eval_tail_let(args: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<TailResult, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::msg("let requires bindings and body"));
    }

    if let Expr::Symbol(name) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::msg("let requires bindings and body"));
        }

        let Expr::List(bindings) = &args[1] else {
            return Err(EvalError::msg("let bindings must be a list"));
        };

        let (procedure, arguments) = build_named_let_call(name, bindings, &args[2..], env, output)?;
        return Ok(TailResult::Call(procedure, arguments));
    }

    let Expr::List(bindings) = &args[0] else {
        return Err(EvalError::msg("let bindings must be a list"));
    };

    let let_env = Environment::new(Some(Rc::clone(env)));
    let (names, values) = parse_let_bindings(bindings, env, output)?;
    for (name, value) in names.into_iter().zip(values) {
        Environment::define(&let_env, name, value);
    }

    eval_tail_sequence(&args[1..], &let_env, output)
}

fn eval_tail_cond(
    args: &[Expr],
    env: &EnvRef,
    output: &OutputRef,
) -> Result<TailResult, EvalError> {
    if args.is_empty() {
        return Err(EvalError::msg("cond requires at least one clause"));
    }

    for (index, clause) in args.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::msg("cond clauses must be lists"));
        };
        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::msg("cond clauses cannot be empty"));
        };

        if matches!(test, Expr::Symbol(name) if name == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::msg("cond else clause must be last"));
            }
            return eval_tail_sequence(body, env, output);
        }

        let test_value = eval(test, env, output)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(TailResult::Value(test_value))
            } else {
                eval_tail_sequence(body, env, output)
            };
        }
    }

    Ok(TailResult::Value(Value::Void))
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Number(number) => Value::Number(*number),
        Expr::Bool(value) => Value::Bool(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(name) => Value::Symbol(name.clone()),
        Expr::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_and(expressions: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);
    for expression in expressions {
        let value = eval(expression, env, output)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }
    Ok(last)
}

fn eval_or(expressions: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    for expression in expressions {
        let value = eval(expression, env, output)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }
    Ok(Value::Bool(false))
}

fn eval_begin(expressions: &[Expr], env: &EnvRef, output: &OutputRef) -> Result<Value, EvalError> {
    eval_sequence(expressions, env, output)
}

fn eval_tail_and(
    expressions: &[Expr],
    env: &EnvRef,
    output: &OutputRef,
) -> Result<TailResult, EvalError> {
    let Some((last, prefix)) = expressions.split_last() else {
        return Ok(TailResult::Value(Value::Bool(true)));
    };

    for expression in prefix {
        let value = eval(expression, env, output)?;
        if !value.is_truthy() {
            return Ok(TailResult::Value(value));
        }
    }

    eval_tail_expression(last, env, output)
}

fn eval_tail_or(
    expressions: &[Expr],
    env: &EnvRef,
    output: &OutputRef,
) -> Result<TailResult, EvalError> {
    let Some((last, prefix)) = expressions.split_last() else {
        return Ok(TailResult::Value(Value::Bool(false)));
    };

    for expression in prefix {
        let value = eval(expression, env, output)?;
        if value.is_truthy() {
            return Ok(TailResult::Value(value));
        }
    }

    eval_tail_expression(last, env, output)
}

fn eval_tail_begin(
    expressions: &[Expr],
    env: &EnvRef,
    output: &OutputRef,
) -> Result<TailResult, EvalError> {
    eval_tail_sequence(expressions, env, output)
}

fn apply(procedure: Value, arguments: Vec<Value>, output: &OutputRef) -> Result<Value, EvalError> {
    run_machine(
        MachineState::Apply(procedure, arguments),
        Vec::new(),
        Vec::new(),
        output,
    )
}

fn into_procedure(value: Value) -> Result<Rc<Procedure>, EvalError> {
    let Value::Procedure(procedure) = value else {
        return Err(EvalError::msg("attempted to call a non-procedure"));
    };
    Ok(procedure)
}

fn bind_lambda_call(lambda: &LambdaProcedure, arguments: Vec<Value>) -> Result<EnvRef, EvalError> {
    let procedure_name = lambda.name.as_deref().unwrap_or("lambda");
    if lambda.rest.is_some() {
        ensure_at_least(procedure_name, arguments.len(), lambda.params.len())?;
    } else {
        ensure_exactly(procedure_name, arguments.len(), lambda.params.len())?;
    }

    let call_env = Environment::new(Some(Rc::clone(&lambda.env)));
    let mut arguments = arguments.into_iter();
    for name in &lambda.params {
        let value = arguments
            .next()
            .expect("arity check ensures enough arguments for fixed parameters");
        Environment::define(&call_env, name.clone(), value);
    }

    if let Some(rest_name) = &lambda.rest {
        Environment::define(
            &call_env,
            rest_name.clone(),
            Value::List(arguments.collect()),
        );
    }

    Ok(call_env)
}

fn select_case_lambda_clause<'a>(
    case_lambda: &'a CaseLambdaProcedure,
    argument_count: usize,
) -> Result<&'a LambdaProcedure, EvalError> {
    let Some(clause) = case_lambda
        .clauses
        .iter()
        .find(|clause| lambda_accepts_arity(clause, argument_count))
    else {
        let procedure_name = case_lambda.name.as_deref().unwrap_or("case-lambda");
        return Err(EvalError::msg(format!(
            "{procedure_name} has no matching clause for {argument_count} arguments"
        )));
    };

    Ok(clause)
}

fn lambda_accepts_arity(lambda: &LambdaProcedure, argument_count: usize) -> bool {
    if lambda.rest.is_some() {
        argument_count >= lambda.params.len()
    } else {
        argument_count == lambda.params.len()
    }
}

fn builtin_add(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    let mut total = 0_i128;
    for argument in arguments {
        total += argument.as_number("+")?;
    }
    Ok(Value::Number(total))
}

fn builtin_subtract(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_at_least("-", arguments.len(), 1)?;
    let first = arguments[0].as_number("-")?;

    if arguments.len() == 1 {
        return Ok(Value::Number(-first));
    }

    let mut total = first;
    for argument in &arguments[1..] {
        total -= argument.as_number("-")?;
    }
    Ok(Value::Number(total))
}

fn builtin_multiply(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    let mut total = 1_i128;
    for argument in arguments {
        total *= argument.as_number("*")?;
    }
    Ok(Value::Number(total))
}

fn builtin_divide(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_at_least("/", arguments.len(), 1)?;

    let mut total = if arguments.len() == 1 {
        1_i128
    } else {
        arguments[0].as_number("/")?
    };

    let divisors = if arguments.len() == 1 {
        arguments
    } else {
        &arguments[1..]
    };

    for argument in divisors {
        let divisor = argument.as_number("/")?;
        if divisor == 0 {
            return Err(EvalError::msg("division by zero"));
        }
        total /= divisor;
    }

    Ok(Value::Number(total))
}

fn builtin_less_than(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    compare_numbers(arguments, "<", |left, right| left < right)
}

fn builtin_greater_than(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    compare_numbers(arguments, ">", |left, right| left > right)
}

fn builtin_numeric_equals(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    compare_numbers(arguments, "=", |left, right| left == right)
}

fn builtin_equal(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("equal?", arguments.len(), 2)?;
    Ok(Value::Bool(values_equal(&arguments[0], &arguments[1])))
}

fn builtin_less_equal(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    compare_numbers(arguments, "<=", |left, right| left <= right)
}

fn builtin_not(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("not", arguments.len(), 1)?;
    Ok(Value::Bool(!arguments[0].is_truthy()))
}

fn builtin_cons(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("cons", arguments.len(), 2)?;
    match &arguments[1] {
        Value::List(values) => {
            let mut list = Vec::with_capacity(values.len() + 1);
            list.push(arguments[0].clone());
            list.extend(values.iter().cloned());
            Ok(Value::List(list))
        }
        _ => Err(EvalError::msg("cons expects a list as its second argument")),
    }
}

fn builtin_list(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    Ok(Value::List(arguments.to_vec()))
}

fn builtin_null_predicate(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("null?", arguments.len(), 1)?;
    Ok(Value::Bool(matches!(
        &arguments[0],
        Value::List(values) if values.is_empty()
    )))
}

fn builtin_procedure_predicate(
    arguments: &[Value],
    _output: &OutputRef,
) -> Result<Value, EvalError> {
    ensure_exactly("procedure?", arguments.len(), 1)?;
    Ok(Value::Bool(matches!(arguments[0], Value::Procedure(_))))
}

fn builtin_car(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("car", arguments.len(), 1)?;
    match &arguments[0] {
        Value::List(values) if !values.is_empty() => Ok(values[0].clone()),
        Value::List(_) => Err(EvalError::msg("car expects a non-empty list")),
        _ => Err(EvalError::msg("car expects a list")),
    }
}

fn builtin_cdr(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("cdr", arguments.len(), 1)?;
    match &arguments[0] {
        Value::List(values) if !values.is_empty() => Ok(Value::List(values[1..].to_vec())),
        Value::List(_) => Err(EvalError::msg("cdr expects a non-empty list")),
        _ => Err(EvalError::msg("cdr expects a list")),
    }
}

fn builtin_length(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("length", arguments.len(), 1)?;
    match &arguments[0] {
        Value::List(values) => Ok(Value::Number(values.len() as i128)),
        _ => Err(EvalError::msg("length expects a list")),
    }
}

fn builtin_reverse(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("reverse", arguments.len(), 1)?;
    match &arguments[0] {
        Value::List(values) => {
            let mut reversed = values.clone();
            reversed.reverse();
            Ok(Value::List(reversed))
        }
        _ => Err(EvalError::msg("reverse expects a list")),
    }
}

fn builtin_apply(arguments: &[Value], output: &OutputRef) -> Result<Value, EvalError> {
    ensure_at_least("apply", arguments.len(), 2)?;

    let trailing_args = match arguments.last() {
        Some(Value::List(values)) => values.clone(),
        Some(_) => return Err(EvalError::msg("apply expects a list as its last argument")),
        None => unreachable!("arity check guarantees at least two arguments"),
    };

    let mut applied_args =
        Vec::with_capacity(arguments.len().saturating_sub(1) + trailing_args.len());
    applied_args.extend(arguments[1..arguments.len() - 1].iter().cloned());
    applied_args.extend(trailing_args);

    apply(arguments[0].clone(), applied_args, output)
}

fn builtin_display(arguments: &[Value], output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("display", arguments.len(), 1)?;
    output.borrow_mut().push_str(&arguments[0].display_repr());
    Ok(Value::Void)
}

fn builtin_write(arguments: &[Value], output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("write", arguments.len(), 1)?;
    output.borrow_mut().push_str(&arguments[0].render());
    Ok(Value::Void)
}

fn builtin_newline(arguments: &[Value], output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("newline", arguments.len(), 0)?;
    output.borrow_mut().push('\n');
    Ok(Value::Void)
}

fn builtin_string_append(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    let mut result = String::new();
    for argument in arguments {
        result.push_str(argument.as_string("string-append")?);
    }
    Ok(Value::String(result))
}

fn builtin_string_length(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("string-length", arguments.len(), 1)?;
    let length = arguments[0].as_string("string-length")?.chars().count();
    Ok(Value::Number(length as i128))
}

fn builtin_substring(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("substring", arguments.len(), 3)?;
    let value = arguments[0].as_string("substring")?;
    let start = arguments[1].as_index("substring")?;
    let end = arguments[2].as_index("substring")?;
    let chars: Vec<char> = value.chars().collect();

    if start > end || end > chars.len() {
        return Err(EvalError::msg("substring indices are out of bounds"));
    }

    Ok(Value::String(chars[start..end].iter().collect()))
}

fn builtin_string_to_number(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("string->number", arguments.len(), 1)?;
    let value = arguments[0].as_string("string->number")?;
    match value.parse::<i128>() {
        Ok(number) => Ok(Value::Number(number)),
        Err(_) => Ok(Value::Bool(false)),
    }
}

fn builtin_number_to_string(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("number->string", arguments.len(), 1)?;
    let number = arguments[0].as_number("number->string")?;
    Ok(Value::String(number.to_string()))
}

fn builtin_symbol_to_string(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("symbol->string", arguments.len(), 1)?;
    Ok(Value::String(
        arguments[0].as_symbol("symbol->string")?.to_string(),
    ))
}

fn builtin_string_to_symbol(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("string->symbol", arguments.len(), 1)?;
    Ok(Value::Symbol(
        arguments[0].as_string("string->symbol")?.to_string(),
    ))
}

fn builtin_string_ref(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("string-ref", arguments.len(), 2)?;
    let value = arguments[0].as_string("string-ref")?;
    let index = arguments[1].as_index("string-ref")?;
    let chars: Vec<char> = value.chars().collect();
    let Some(ch) = chars.get(index) else {
        return Err(EvalError::msg("string-ref index is out of bounds"));
    };

    Ok(Value::Char(*ch))
}

fn builtin_char_predicate(arguments: &[Value], _output: &OutputRef) -> Result<Value, EvalError> {
    ensure_exactly("char?", arguments.len(), 1)?;
    Ok(Value::Bool(matches!(arguments[0], Value::Char(_))))
}

fn compare_numbers(
    arguments: &[Value],
    name: &str,
    predicate: fn(i128, i128) -> bool,
) -> Result<Value, EvalError> {
    ensure_at_least(name, arguments.len(), 2)?;
    let mut previous = arguments[0].as_number(name)?;

    for argument in &arguments[1..] {
        let current = argument.as_number(name)?;
        if !predicate(previous, current) {
            return Ok(Value::Bool(false));
        }
        previous = current;
    }

    Ok(Value::Bool(true))
}

fn values_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left == right,
        (Value::Bool(left), Value::Bool(right)) => left == right,
        (Value::String(left), Value::String(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| values_equal(left, right))
        }
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn ensure_exactly(name: &str, actual: usize, expected: usize) -> Result<(), EvalError> {
    if actual == expected {
        Ok(())
    } else {
        Err(EvalError::msg(format!(
            "{name} expected {expected} arguments but got {actual}"
        )))
    }
}

fn ensure_at_least(name: &str, actual: usize, minimum: usize) -> Result<(), EvalError> {
    if actual >= minimum {
        Ok(())
    } else {
        Err(EvalError::msg(format!(
            "{name} expected at least {minimum} arguments but got {actual}"
        )))
    }
}

fn render_value(value: &Value, mode: RenderMode) -> String {
    match value {
        Value::Number(number) => number.to_string(),
        Value::Bool(true) => "#t".to_string(),
        Value::Bool(false) => "#f".to_string(),
        Value::String(value) => match mode {
            RenderMode::Write => render_string(value),
            RenderMode::Display => value.clone(),
        },
        Value::Char(ch) => match mode {
            RenderMode::Write => render_char(*ch),
            RenderMode::Display => ch.to_string(),
        },
        Value::Symbol(name) => name.clone(),
        Value::List(values) => render_list(values, mode),
        Value::Procedure(procedure) => procedure.render(),
        Value::Void => "#<void>".to_string(),
    }
}

fn render_list(values: &[Value], mode: RenderMode) -> String {
    let mut rendered = String::from("(");
    for (index, value) in values.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&render_value(value, mode));
    }
    rendered.push(')');
    rendered
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        other => format!("#\\{other}"),
    }
}

fn render_string(value: &str) -> String {
    let mut rendered = String::from("\"");
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

struct Parser<'a> {
    chars: Vec<char>,
    pos: usize,
    _input: &'a str,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
            _input: input,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();
        while !self.is_at_end() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        match self.peek() {
            Some('(') => self.parse_list(),
            Some(')') => Err(EvalError::msg("unexpected ')'")),
            Some('\'') => self.parse_quote_shorthand(),
            Some('"') => self.parse_string(),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::msg("unexpected end of input")),
        }
    }

    fn parse_quote_shorthand(&mut self) -> Result<Expr, EvalError> {
        self.advance();
        Ok(Expr::List(vec![
            Expr::Symbol("quote".to_string()),
            self.parse_expr()?,
        ]))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.advance();
        let mut values = Vec::new();
        self.skip_ignored();

        while let Some(ch) = self.peek() {
            if ch == ')' {
                self.advance();
                return Ok(Expr::List(values));
            }

            values.push(self.parse_expr()?);
            self.skip_ignored();
        }

        Err(EvalError::msg("unterminated list"))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.advance();
        let mut value = String::new();

        while let Some(ch) = self.advance() {
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let Some(escaped) = self.advance() else {
                        return Err(EvalError::msg("unterminated string literal"));
                    };
                    value.push(match escaped {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                }
                other => value.push(other),
            }
        }

        Err(EvalError::msg("unterminated string literal"))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let mut token = String::new();
        while let Some(ch) = self.peek() {
            if is_delimiter(ch) {
                break;
            }
            token.push(ch);
            self.advance();
        }

        if token == "#t" {
            return Ok(Expr::Bool(true));
        }
        if token == "#f" {
            return Ok(Expr::Bool(false));
        }
        if let Ok(number) = token.parse::<i128>() {
            return Ok(Expr::Number(number));
        }

        Ok(Expr::Symbol(token))
    }

    fn skip_ignored(&mut self) {
        loop {
            match self.peek() {
                Some(ch) if ch.is_whitespace() => {
                    self.advance();
                }
                Some(';') => {
                    while let Some(ch) = self.peek() {
                        if ch == '\n' {
                            break;
                        }
                        self.advance();
                    }
                }
                _ => break,
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += 1;
        Some(ch)
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.chars.len()
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';')
}

#[cfg(test)]
mod tests;
