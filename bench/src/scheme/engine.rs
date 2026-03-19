use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type Continuation = Rc<Kont>;

pub(super) enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Pair(Box<Value>, Box<Value>),
    Nil,
    Builtin(Builtin),
    Procedure(Procedure),
    Continuation(Continuation),
    Void,
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Integer(value) => Self::Integer(*value),
            Self::Boolean(value) => Self::Boolean(*value),
            Self::String(value) => Self::String(value.clone()),
            Self::Symbol(value) => Self::Symbol(value.clone()),
            Self::Pair(car, cdr) => Self::Pair(car.clone(), cdr.clone()),
            Self::Nil => Self::Nil,
            Self::Builtin(builtin) => Self::Builtin(*builtin),
            Self::Procedure(procedure) => Self::Procedure(procedure.clone()),
            Self::Continuation(continuation) => Self::Continuation(continuation.clone()),
            Self::Void => Self::Void,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(value) => write!(formatter, "{value}"),
            Self::Boolean(true) => formatter.write_str("#t"),
            Self::Boolean(false) => formatter.write_str("#f"),
            Self::String(value) => {
                formatter.write_str("\"")?;
                fmt_string_contents(value, formatter)?;
                formatter.write_str("\"")
            }
            Self::Symbol(value) => formatter.write_str(value),
            Self::Pair(car, cdr) => {
                formatter.write_str("(")?;
                fmt_pair(car, cdr, formatter)?;
                formatter.write_str(")")
            }
            Self::Nil => formatter.write_str("()"),
            Self::Builtin(builtin) => write!(formatter, "#<procedure:{}>", builtin.name()),
            Self::Procedure(_) => formatter.write_str("#<procedure>"),
            Self::Continuation(_) => formatter.write_str("#<continuation>"),
            Self::Void => Ok(()),
        }
    }
}

fn fmt_pair(car: &Value, cdr: &Value, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(formatter, "{car}")?;

    match cdr {
        Value::Nil => Ok(()),
        Value::Pair(next_car, next_cdr) => {
            formatter.write_str(" ")?;
            fmt_pair(next_car, next_cdr, formatter)
        }
        value => write!(formatter, " . {value}"),
    }
}

fn fmt_string_contents(value: &str, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for character in value.chars() {
        match character {
            '"' => formatter.write_str("\\\"")?,
            '\\' => formatter.write_str("\\\\")?,
            '\n' => formatter.write_str("\\n")?,
            '\t' => formatter.write_str("\\t")?,
            _ => write!(formatter, "{character}")?,
        }
    }

    Ok(())
}

#[derive(Clone)]
pub(super) enum Expr {
    Literal(Value),
    Symbol(String),
    Application(Vec<Expr>),
}

#[derive(Clone, Copy)]
pub(super) enum Builtin {
    Add,
    Subtract,
    Multiply,
    Divide,
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
    Apply,
    CallCc,
}

impl Builtin {
    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "=",
            Self::LessEqual => "<=",
            Self::Not => "not",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::IsNull => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::IsString => "string?",
            Self::IsNumber => "number?",
            Self::IsBoolean => "boolean?",
            Self::IsPair => "pair?",
            Self::IsSymbol => "symbol?",
            Self::Apply => "apply",
            Self::CallCc => "call/cc",
        }
    }

    fn apply(self, arguments: &[Value]) -> Result<Value, String> {
        match self {
            Self::Add => eval_add(arguments),
            Self::Subtract => eval_subtract(arguments),
            Self::Multiply => eval_multiply(arguments),
            Self::Divide => eval_divide(arguments),
            Self::LessThan => eval_less_than(arguments),
            Self::GreaterThan => eval_greater_than(arguments),
            Self::Equal => eval_equal(arguments),
            Self::LessEqual => eval_less_equal(arguments),
            Self::Not => eval_not(arguments),
            Self::Cons => eval_cons(arguments),
            Self::Car => eval_car(arguments),
            Self::Cdr => eval_cdr(arguments),
            Self::IsNull => eval_is_null(arguments),
            Self::List => eval_list(arguments),
            Self::Length => eval_length(arguments),
            Self::IsString => eval_type_predicate(arguments, "string?", |value| {
                matches!(value, Value::String(_))
            }),
            Self::IsNumber => eval_type_predicate(arguments, "number?", |value| {
                matches!(value, Value::Integer(_))
            }),
            Self::IsBoolean => eval_type_predicate(arguments, "boolean?", |value| {
                matches!(value, Value::Boolean(_))
            }),
            Self::IsPair => eval_type_predicate(arguments, "pair?", |value| {
                matches!(value, Value::Pair(_, _))
            }),
            Self::IsSymbol => eval_type_predicate(arguments, "symbol?", |value| {
                matches!(value, Value::Symbol(_))
            }),
            Self::Apply => Err("internal error: `apply` needs the evaluator".into()),
            Self::CallCc => Err("internal error: `call/cc` needs the evaluator".into()),
        }
    }
}

#[derive(Clone)]
pub(super) struct Procedure {
    params: Rc<[String]>,
    rest_param: Option<String>,
    body: Rc<[Expr]>,
    environment: Environment,
}

struct ParameterSpec {
    fixed: Vec<String>,
    rest: Option<String>,
}

type Environment = Rc<RefCell<Frame>>;

pub(super) struct Frame {
    bindings: HashMap<String, Value>,
    parent: Option<Environment>,
}

pub(super) enum Kont {
    Done,
    Sequence {
        remaining: Vec<Expr>,
        environment: Environment,
        next: Continuation,
    },
    DefineValue {
        name: String,
        environment: Environment,
        next: Continuation,
    },
    SetValue {
        name: String,
        environment: Environment,
        next: Continuation,
    },
    If {
        consequent: Expr,
        alternative: Expr,
        environment: Environment,
        next: Continuation,
    },
    And {
        remaining: Vec<Expr>,
        environment: Environment,
        next: Continuation,
    },
    Or {
        remaining: Vec<Expr>,
        environment: Environment,
        next: Continuation,
    },
    CondTest {
        body: Vec<Expr>,
        remaining_clauses: Vec<Expr>,
        environment: Environment,
        next: Continuation,
    },
    ApplyOperator {
        arguments: Vec<Expr>,
        environment: Environment,
        next: Continuation,
    },
    ApplyArgument {
        operator: Value,
        remaining: Vec<Expr>,
        evaluated_suffix: Vec<Value>,
        environment: Environment,
        next: Continuation,
    },
}

enum Control {
    Eval(Expr, Environment),
    Value(Value),
}

struct Machine {
    control: Control,
    continuation: Continuation,
}

enum Step {
    Continue(Machine),
    Done(Value),
}

pub(super) fn eval_program(input: &str) -> Result<Value, String> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program()?;
    let environment = global_environment();
    schedule_sequence(program, environment, Rc::new(Kont::Done))?.run()
}

fn global_environment() -> Environment {
    let environment = new_environment(None);

    for builtin in [
        Builtin::Add,
        Builtin::Subtract,
        Builtin::Multiply,
        Builtin::Divide,
        Builtin::LessThan,
        Builtin::GreaterThan,
        Builtin::Equal,
        Builtin::LessEqual,
        Builtin::Not,
        Builtin::Cons,
        Builtin::Car,
        Builtin::Cdr,
        Builtin::IsNull,
        Builtin::List,
        Builtin::Length,
        Builtin::IsString,
        Builtin::IsNumber,
        Builtin::IsBoolean,
        Builtin::IsPair,
        Builtin::IsSymbol,
        Builtin::Apply,
        Builtin::CallCc,
    ] {
        define_binding(
            &environment,
            builtin.name().to_string(),
            Value::Builtin(builtin),
        );
    }

    environment
}

fn new_environment(parent: Option<Environment>) -> Environment {
    Rc::new(RefCell::new(Frame {
        bindings: HashMap::new(),
        parent,
    }))
}

fn define_binding(environment: &Environment, name: String, value: Value) {
    environment.borrow_mut().bindings.insert(name, value);
}

fn lookup_binding(environment: &Environment, name: &str) -> Option<Value> {
    let parent = {
        let frame = environment.borrow();
        if let Some(value) = frame.bindings.get(name) {
            return Some(value.clone());
        }
        frame.parent.clone()
    };

    parent.and_then(|parent| lookup_binding(&parent, name))
}

fn set_binding(environment: &Environment, name: &str, value: Value) -> Result<(), String> {
    let parent = {
        let mut frame = environment.borrow_mut();
        if let Some(binding) = frame.bindings.get_mut(name) {
            *binding = value;
            return Ok(());
        }
        frame.parent.clone()
    };

    match parent {
        Some(parent) => set_binding(&parent, name, value),
        None => Err(format!("unbound symbol: {name}")),
    }
}

impl Machine {
    fn eval(expr: Expr, environment: Environment, continuation: Continuation) -> Self {
        Self {
            control: Control::Eval(expr, environment),
            continuation,
        }
    }

    fn value(value: Value, continuation: Continuation) -> Self {
        Self {
            control: Control::Value(value),
            continuation,
        }
    }

    fn run(self) -> Result<Value, String> {
        let mut machine = self;

        loop {
            machine = match drive(machine)? {
                Step::Continue(next) => next,
                Step::Done(value) => return Ok(value),
            };
        }
    }
}

fn drive(machine: Machine) -> Result<Step, String> {
    let Machine {
        control,
        continuation,
    } = machine;

    match control {
        Control::Eval(expr, environment) => {
            Ok(Step::Continue(step_eval(expr, environment, continuation)?))
        }
        Control::Value(value) => resume(value, continuation),
    }
}

fn step_eval(
    expr: Expr,
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    match expr {
        Expr::Literal(value) => Ok(Machine::value(value, continuation)),
        Expr::Symbol(name) => lookup_binding(&environment, &name)
            .map(|value| Machine::value(value, continuation))
            .ok_or_else(|| format!("unbound symbol: {name}")),
        Expr::Application(parts) => eval_application(parts, environment, continuation),
    }
}

fn resume(value: Value, continuation: Continuation) -> Result<Step, String> {
    match continuation.as_ref() {
        Kont::Done => Ok(Step::Done(value)),
        Kont::Sequence {
            remaining,
            environment,
            next,
        } => resume_sequence(remaining, environment, next),
        Kont::DefineValue {
            name,
            environment,
            next,
        } => resume_define(value, name, environment, next),
        Kont::SetValue {
            name,
            environment,
            next,
        } => resume_set(value, name, environment, next),
        Kont::If {
            consequent,
            alternative,
            environment,
            next,
        } => resume_if(value, consequent, alternative, environment, next),
        Kont::And {
            remaining,
            environment,
            next,
        } => resume_and(value, remaining, environment, next),
        Kont::Or {
            remaining,
            environment,
            next,
        } => resume_or(value, remaining, environment, next),
        Kont::CondTest {
            body,
            remaining_clauses,
            environment,
            next,
        } => resume_cond(value, body, remaining_clauses, environment, next),
        Kont::ApplyOperator {
            arguments,
            environment,
            next,
        } => resume_apply_operator(value, arguments, environment, next),
        Kont::ApplyArgument {
            operator,
            remaining,
            evaluated_suffix,
            environment,
            next,
        } => resume_apply_argument(
            value,
            operator,
            remaining,
            evaluated_suffix,
            environment,
            next,
        ),
    }
}

fn resume_sequence(
    remaining: &[Expr],
    environment: &Environment,
    next: &Continuation,
) -> Result<Step, String> {
    Ok(Step::Continue(schedule_sequence(
        remaining.to_vec(),
        environment.clone(),
        next.clone(),
    )?))
}

fn resume_define(
    value: Value,
    name: &str,
    environment: &Environment,
    next: &Continuation,
) -> Result<Step, String> {
    define_binding(environment, name.to_string(), value);
    Ok(Step::Continue(Machine::value(Value::Void, next.clone())))
}

fn resume_set(
    value: Value,
    name: &str,
    environment: &Environment,
    next: &Continuation,
) -> Result<Step, String> {
    set_binding(environment, name, value)?;
    Ok(Step::Continue(Machine::value(Value::Void, next.clone())))
}

fn resume_if(
    value: Value,
    consequent: &Expr,
    alternative: &Expr,
    environment: &Environment,
    next: &Continuation,
) -> Result<Step, String> {
    let branch = if is_truthy(&value) {
        consequent.clone()
    } else {
        alternative.clone()
    };
    Ok(Step::Continue(Machine::eval(
        branch,
        environment.clone(),
        next.clone(),
    )))
}

fn resume_and(
    value: Value,
    remaining: &[Expr],
    environment: &Environment,
    next: &Continuation,
) -> Result<Step, String> {
    if !is_truthy(&value) {
        return Ok(Step::Continue(Machine::value(value, next.clone())));
    }

    Ok(Step::Continue(schedule_and(
        remaining.to_vec(),
        environment.clone(),
        next.clone(),
    )?))
}

fn resume_or(
    value: Value,
    remaining: &[Expr],
    environment: &Environment,
    next: &Continuation,
) -> Result<Step, String> {
    if is_truthy(&value) {
        return Ok(Step::Continue(Machine::value(value, next.clone())));
    }

    Ok(Step::Continue(schedule_or(
        remaining.to_vec(),
        environment.clone(),
        next.clone(),
    )?))
}

fn resume_cond(
    value: Value,
    body: &[Expr],
    remaining_clauses: &[Expr],
    environment: &Environment,
    next: &Continuation,
) -> Result<Step, String> {
    if !is_truthy(&value) {
        return Ok(Step::Continue(schedule_cond(
            remaining_clauses.to_vec(),
            environment.clone(),
            next.clone(),
        )?));
    }

    if body.is_empty() {
        return Ok(Step::Continue(Machine::value(value, next.clone())));
    }

    Ok(Step::Continue(schedule_sequence(
        body.to_vec(),
        environment.clone(),
        next.clone(),
    )?))
}

fn resume_apply_operator(
    value: Value,
    arguments: &[Expr],
    environment: &Environment,
    next: &Continuation,
) -> Result<Step, String> {
    Ok(Step::Continue(schedule_argument_evaluation(
        value,
        arguments.to_vec(),
        environment.clone(),
        next.clone(),
    )?))
}

fn resume_apply_argument(
    value: Value,
    operator: &Value,
    remaining: &[Expr],
    evaluated_suffix: &[Value],
    environment: &Environment,
    next: &Continuation,
) -> Result<Step, String> {
    Ok(Step::Continue(continue_argument_evaluation(
        operator.clone(),
        remaining.to_vec(),
        evaluated_suffix.to_vec(),
        value,
        environment.clone(),
        next.clone(),
    )?))
}

fn eval_application(
    parts: Vec<Expr>,
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    let mut parts = parts.into_iter();
    let operator = parts
        .next()
        .ok_or_else(|| "cannot evaluate empty application".to_string())?;
    let arguments: Vec<_> = parts.collect();

    if let Some(name) = symbol_name(&operator) {
        match name {
            "and" => return schedule_and(arguments, environment, continuation),
            "or" => return schedule_or(arguments, environment, continuation),
            "begin" => return schedule_sequence(arguments, environment, continuation),
            "cond" => return schedule_cond(arguments, environment, continuation),
            "if" => return schedule_if(&arguments, environment, continuation),
            "define" => return schedule_define(&arguments, environment, continuation),
            "let" => return schedule_let(&arguments, environment, continuation),
            "quote" => return schedule_quote(&arguments, continuation),
            "lambda" => return schedule_lambda(&arguments, environment, continuation),
            "set!" => return schedule_set(&arguments, environment, continuation),
            _ => {}
        }
    }

    let frame = Kont::ApplyOperator {
        arguments,
        environment: environment.clone(),
        next: continuation,
    };
    Ok(Machine::eval(operator, environment, Rc::new(frame)))
}

fn schedule_if(
    arguments: &[Expr],
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    let [condition, consequent, alternative] = arguments else {
        return Err("`if` expects exactly 3 arguments".into());
    };

    let frame = Kont::If {
        consequent: consequent.clone(),
        alternative: alternative.clone(),
        environment: environment.clone(),
        next: continuation,
    };
    Ok(Machine::eval(condition.clone(), environment, Rc::new(frame)))
}

fn schedule_define(
    arguments: &[Expr],
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    let (target, body) = arguments
        .split_first()
        .ok_or_else(|| "`define` expects at least 2 arguments".to_string())?;

    match target {
        Expr::Symbol(name) => define_value(name, body, environment, continuation),
        Expr::Application(signature) => define_function(signature, body, environment, continuation),
        _ => Err("`define` expects a symbol name".into()),
    }
}

fn define_value(
    name: &str,
    body: &[Expr],
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    let [value_expr] = body else {
        return Err("`define` expects exactly 2 arguments".into());
    };

    let frame = Kont::DefineValue {
        name: name.to_string(),
        environment: environment.clone(),
        next: continuation,
    };
    Ok(Machine::eval(value_expr.clone(), environment, Rc::new(frame)))
}

fn define_function(
    signature: &[Expr],
    body: &[Expr],
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    let (name, params) = signature
        .split_first()
        .ok_or_else(|| "`define` expects a function name".to_string())?;
    let Expr::Symbol(name) = name else {
        return Err("`define` expects a symbol name".into());
    };

    if body.is_empty() {
        return Err("`define` expects a function body".into());
    }

    let params = parse_parameters(params)?;
    let procedure = Procedure {
        params: params.fixed.into(),
        rest_param: params.rest,
        body: body.to_vec().into(),
        environment: environment.clone(),
    };
    define_binding(&environment, name.clone(), Value::Procedure(procedure));

    Ok(Machine::value(Value::Void, continuation))
}

fn schedule_let(
    arguments: &[Expr],
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    let (first, rest) = arguments
        .split_first()
        .ok_or_else(|| "`let` expects bindings and a body".to_string())?;

    match first {
        Expr::Symbol(name) => schedule_named_let(name, rest, environment, continuation),
        Expr::Application(bindings) => schedule_regular_let(bindings, rest, environment, continuation),
        _ => Err("`let` expects a binding list".into()),
    }
}

fn schedule_regular_let(
    bindings: &[Expr],
    body: &[Expr],
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    if body.is_empty() {
        return Err("`let` expects a body".into());
    }

    let (params, arguments) = parse_let_bindings(bindings)?;
    let procedure = Procedure {
        params: params.into(),
        rest_param: None,
        body: body.to_vec().into(),
        environment: environment.clone(),
    };
    schedule_argument_evaluation(Value::Procedure(procedure), arguments, environment, continuation)
}

fn schedule_named_let(
    name: &str,
    arguments: &[Expr],
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    let (bindings, body) = arguments
        .split_first()
        .ok_or_else(|| "`let` expects bindings and a body".to_string())?;

    if body.is_empty() {
        return Err("`let` expects a body".into());
    }

    let Expr::Application(bindings) = bindings else {
        return Err("`let` expects a binding list".into());
    };

    let (params, arguments) = parse_let_bindings(bindings)?;
    let let_environment = new_environment(Some(environment.clone()));
    let procedure = Procedure {
        params: params.into(),
        rest_param: None,
        body: body.to_vec().into(),
        environment: let_environment.clone(),
    };
    define_binding(
        &let_environment,
        name.to_string(),
        Value::Procedure(procedure.clone()),
    );

    schedule_argument_evaluation(Value::Procedure(procedure), arguments, environment, continuation)
}

fn schedule_quote(arguments: &[Expr], continuation: Continuation) -> Result<Machine, String> {
    Ok(Machine::value(eval_quote(arguments)?, continuation))
}

fn schedule_lambda(
    arguments: &[Expr],
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    Ok(Machine::value(eval_lambda(arguments, &environment)?, continuation))
}

fn schedule_set(
    arguments: &[Expr],
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    let [target, value_expr] = arguments else {
        return Err("`set!` expects exactly 2 arguments".into());
    };

    let Expr::Symbol(name) = target else {
        return Err("`set!` expects a symbol name".into());
    };

    let frame = Kont::SetValue {
        name: name.clone(),
        environment: environment.clone(),
        next: continuation,
    };
    Ok(Machine::eval(value_expr.clone(), environment, Rc::new(frame)))
}

fn schedule_sequence(
    expressions: Vec<Expr>,
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    match expressions.as_slice() {
        [] => Ok(Machine::value(Value::Void, continuation)),
        [expr] => Ok(Machine::eval(expr.clone(), environment, continuation)),
        [first, rest @ ..] => {
            let frame = Kont::Sequence {
                remaining: rest.to_vec(),
                environment: environment.clone(),
                next: continuation,
            };
            Ok(Machine::eval(first.clone(), environment, Rc::new(frame)))
        }
    }
}

fn schedule_and(
    arguments: Vec<Expr>,
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    match arguments.as_slice() {
        [] => Ok(Machine::value(Value::Boolean(true), continuation)),
        [expr] => Ok(Machine::eval(expr.clone(), environment, continuation)),
        [first, rest @ ..] => {
            let frame = Kont::And {
                remaining: rest.to_vec(),
                environment: environment.clone(),
                next: continuation,
            };
            Ok(Machine::eval(first.clone(), environment, Rc::new(frame)))
        }
    }
}

fn schedule_or(
    arguments: Vec<Expr>,
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    match arguments.as_slice() {
        [] => Ok(Machine::value(Value::Boolean(false), continuation)),
        [expr] => Ok(Machine::eval(expr.clone(), environment, continuation)),
        [first, rest @ ..] => {
            let frame = Kont::Or {
                remaining: rest.to_vec(),
                environment: environment.clone(),
                next: continuation,
            };
            Ok(Machine::eval(first.clone(), environment, Rc::new(frame)))
        }
    }
}

fn schedule_cond(
    clauses: Vec<Expr>,
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    let mut clauses = clauses.into_iter();
    let Some(clause) = clauses.next() else {
        return Ok(Machine::value(Value::Void, continuation));
    };
    let remaining_clauses: Vec<_> = clauses.collect();

    let Expr::Application(parts) = clause else {
        return Err("`cond` clauses must be lists".into());
    };
    let (test, body) = parts
        .split_first()
        .ok_or_else(|| "`cond` clauses must not be empty".to_string())?;

    if matches!(test, Expr::Symbol(name) if name == "else") {
        if !remaining_clauses.is_empty() {
            return Err("`cond` `else` clause must be last".into());
        }
        if body.is_empty() {
            return Err("`cond` `else` clause must have a body".into());
        }

        return schedule_sequence(body.to_vec(), environment, continuation);
    }

    let frame = Kont::CondTest {
        body: body.to_vec(),
        remaining_clauses,
        environment: environment.clone(),
        next: continuation,
    };
    Ok(Machine::eval(test.clone(), environment, Rc::new(frame)))
}

fn schedule_argument_evaluation(
    operator: Value,
    arguments: Vec<Expr>,
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    let Some((current, remaining)) = split_last_argument(arguments) else {
        return continue_call(operator, Vec::new(), continuation);
    };

    let frame = Kont::ApplyArgument {
        operator,
        remaining,
        evaluated_suffix: Vec::new(),
        environment: environment.clone(),
        next: continuation,
    };
    Ok(Machine::eval(current, environment, Rc::new(frame)))
}

fn continue_argument_evaluation(
    operator: Value,
    remaining: Vec<Expr>,
    mut evaluated_suffix: Vec<Value>,
    value: Value,
    environment: Environment,
    continuation: Continuation,
) -> Result<Machine, String> {
    evaluated_suffix.insert(0, value);

    let Some((next_expr, next_remaining)) = split_last_argument(remaining) else {
        return continue_call(operator, evaluated_suffix, continuation);
    };

    let frame = Kont::ApplyArgument {
        operator,
        remaining: next_remaining,
        evaluated_suffix,
        environment: environment.clone(),
        next: continuation,
    };
    Ok(Machine::eval(next_expr, environment, Rc::new(frame)))
}

fn split_last_argument(mut arguments: Vec<Expr>) -> Option<(Expr, Vec<Expr>)> {
    let current = arguments.pop()?;
    Some((current, arguments))
}

fn continue_call(
    callable: Value,
    arguments: Vec<Value>,
    continuation: Continuation,
) -> Result<Machine, String> {
    match callable {
        Value::Builtin(builtin) => call_builtin(builtin, arguments, continuation),
        Value::Procedure(procedure) => call_procedure(procedure, arguments, continuation),
        Value::Continuation(saved) => {
            let [value] = arguments.as_slice() else {
                return Err("continuation expected exactly 1 argument".into());
            };
            Ok(Machine::value(value.clone(), saved))
        }
        _ => Err("attempted to call a non-procedure".into()),
    }
}

fn call_builtin(
    builtin: Builtin,
    arguments: Vec<Value>,
    continuation: Continuation,
) -> Result<Machine, String> {
    match builtin {
        Builtin::Apply => {
            let (callable, rest) = arguments
                .split_first()
                .ok_or_else(|| "`apply` expects at least 2 arguments".to_string())?;
            let (list, prefix) = rest
                .split_last()
                .ok_or_else(|| "`apply` expects at least 2 arguments".to_string())?;

            let mut applied_arguments = prefix.to_vec();
            applied_arguments.extend(list_to_vec(list)?);
            continue_call(callable.clone(), applied_arguments, continuation)
        }
        Builtin::CallCc => {
            let [callable] = arguments.as_slice() else {
                return Err("`call/cc` expects exactly 1 argument".into());
            };

            continue_call(
                callable.clone(),
                vec![Value::Continuation(continuation.clone())],
                continuation,
            )
        }
        _ => Ok(Machine::value(builtin.apply(&arguments)?, continuation)),
    }
}

fn call_procedure(
    procedure: Procedure,
    arguments: Vec<Value>,
    continuation: Continuation,
) -> Result<Machine, String> {
    validate_procedure_arity(&procedure, arguments.len())?;
    let call_environment = new_environment(Some(procedure.environment.clone()));
    bind_procedure_arguments(&call_environment, &procedure, &arguments);
    schedule_sequence(procedure.body.as_ref().to_vec(), call_environment, continuation)
}

fn validate_procedure_arity(procedure: &Procedure, actual: usize) -> Result<(), String> {
    let expected = procedure.params.len();
    let valid = if procedure.rest_param.is_some() {
        actual >= expected
    } else {
        actual == expected
    };

    if valid {
        return Ok(());
    }

    let expected = if procedure.rest_param.is_some() {
        format!("at least {expected}")
    } else {
        expected.to_string()
    };
    Err(format!(
        "procedure expected {expected} arguments, got {actual}"
    ))
}

fn bind_procedure_arguments(environment: &Environment, procedure: &Procedure, arguments: &[Value]) {
    let (fixed_arguments, rest_arguments) = arguments.split_at(procedure.params.len());

    for (param, argument) in procedure.params.iter().zip(fixed_arguments) {
        define_binding(environment, param.clone(), argument.clone());
    }

    if let Some(rest_param) = &procedure.rest_param {
        define_binding(environment, rest_param.clone(), build_list(rest_arguments));
    }
}

fn parse_let_bindings(bindings: &[Expr]) -> Result<(Vec<String>, Vec<Expr>), String> {
    let mut params = Vec::with_capacity(bindings.len());
    let mut arguments = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let (name, value) = parse_let_binding(binding)?;
        params.push(name);
        arguments.push(value);
    }

    Ok((params, arguments))
}

fn parse_let_binding(binding: &Expr) -> Result<(String, Expr), String> {
    let Expr::Application(binding) = binding else {
        return Err("`let` bindings must be pairs".into());
    };

    let [name, value_expr] = binding.as_slice() else {
        return Err("`let` bindings must contain exactly 2 forms".into());
    };

    let Expr::Symbol(name) = name else {
        return Err("`let` binding names must be symbols".into());
    };

    Ok((name.clone(), value_expr.clone()))
}

fn parse_parameters(parameters: &[Expr]) -> Result<ParameterSpec, String> {
    let mut fixed = Vec::with_capacity(parameters.len());

    for (index, parameter) in parameters.iter().enumerate() {
        let Expr::Symbol(name) = parameter else {
            return Err("parameter names must be symbols".into());
        };

        if name == "." {
            return parse_rest_parameter(&parameters[index + 1..], fixed);
        }

        fixed.push(name.clone());
    }

    Ok(ParameterSpec { fixed, rest: None })
}

fn parse_rest_parameter(parameters: &[Expr], fixed: Vec<String>) -> Result<ParameterSpec, String> {
    let [rest] = parameters else {
        return Err("rest parameter must be the final name".into());
    };
    let Expr::Symbol(rest) = rest else {
        return Err("parameter names must be symbols".into());
    };
    if rest == "." {
        return Err("parameter names must be symbols".into());
    }

    Ok(ParameterSpec {
        fixed,
        rest: Some(rest.clone()),
    })
}

fn eval_lambda(arguments: &[Expr], environment: &Environment) -> Result<Value, String> {
    let (params, body) = arguments
        .split_first()
        .ok_or_else(|| "`lambda` expects a parameter list and body".to_string())?;

    if body.is_empty() {
        return Err("`lambda` expects a body".into());
    }

    let Expr::Application(params) = params else {
        return Err("`lambda` expects a parameter list".into());
    };
    let params = parse_parameters(params)?;

    Ok(Value::Procedure(Procedure {
        params: params.fixed.into(),
        rest_param: params.rest,
        body: body.to_vec().into(),
        environment: environment.clone(),
    }))
}

fn eval_quote(arguments: &[Expr]) -> Result<Value, String> {
    let [value] = arguments else {
        return Err("`quote` expects exactly 1 argument".into());
    };

    quote_expr(value)
}

fn quote_expr(expr: &Expr) -> Result<Value, String> {
    match expr {
        Expr::Literal(value) => Ok(value.clone()),
        Expr::Symbol(value) => Ok(Value::Symbol(value.clone())),
        Expr::Application(values) => {
            let mut list = Value::Nil;
            for value in values.iter().rev() {
                list = Value::Pair(Box::new(quote_expr(value)?), Box::new(list));
            }
            Ok(list)
        }
    }
}

fn eval_add(arguments: &[Value]) -> Result<Value, String> {
    arguments
        .iter()
        .try_fold(0_i64, |total, value| {
            let value = expect_integer(value, "+")?;
            total
                .checked_add(value)
                .ok_or_else(|| "integer overflow".to_string())
        })
        .map(Value::Integer)
}

fn eval_subtract(arguments: &[Value]) -> Result<Value, String> {
    let (first, rest) = arguments
        .split_first()
        .ok_or_else(|| "`-` expects at least 1 argument".to_string())?;
    let first = expect_integer(first, "-")?;

    if rest.is_empty() {
        return first
            .checked_neg()
            .map(Value::Integer)
            .ok_or_else(|| "integer overflow".to_string());
    }

    rest.iter()
        .try_fold(first, |total, value| {
            let value = expect_integer(value, "-")?;
            total
                .checked_sub(value)
                .ok_or_else(|| "integer overflow".to_string())
        })
        .map(Value::Integer)
}

fn eval_multiply(arguments: &[Value]) -> Result<Value, String> {
    arguments
        .iter()
        .try_fold(1_i64, |total, value| {
            let value = expect_integer(value, "*")?;
            total
                .checked_mul(value)
                .ok_or_else(|| "integer overflow".to_string())
        })
        .map(Value::Integer)
}

fn eval_divide(arguments: &[Value]) -> Result<Value, String> {
    let (first, rest) = arguments
        .split_first()
        .ok_or_else(|| "`/` expects at least 2 arguments".to_string())?;
    if rest.is_empty() {
        return Err("`/` expects at least 2 arguments".into());
    }

    let first = expect_integer(first, "/")?;
    rest.iter()
        .try_fold(first, |total, value| {
            let value = expect_integer(value, "/")?;
            if value == 0 {
                return Err("division by zero".into());
            }
            total
                .checked_div(value)
                .ok_or_else(|| "integer overflow".to_string())
        })
        .map(Value::Integer)
}

fn eval_less_than(arguments: &[Value]) -> Result<Value, String> {
    eval_numeric_comparison(arguments, "<", |left, right| left < right)
}

fn eval_greater_than(arguments: &[Value]) -> Result<Value, String> {
    eval_numeric_comparison(arguments, ">", |left, right| left > right)
}

fn eval_equal(arguments: &[Value]) -> Result<Value, String> {
    eval_numeric_comparison(arguments, "=", |left, right| left == right)
}

fn eval_less_equal(arguments: &[Value]) -> Result<Value, String> {
    eval_numeric_comparison(arguments, "<=", |left, right| left <= right)
}

fn eval_numeric_comparison<F>(
    arguments: &[Value],
    operator: &str,
    compare: F,
) -> Result<Value, String>
where
    F: Fn(i64, i64) -> bool,
{
    let mut numbers = arguments.iter();
    let first = numbers
        .next()
        .ok_or_else(|| format!("`{operator}` expects at least 2 arguments"))?;
    let mut previous = expect_integer(first, operator)?;
    let mut saw_pair = false;

    for value in numbers {
        saw_pair = true;
        let current = expect_integer(value, operator)?;
        if !compare(previous, current) {
            return Ok(Value::Boolean(false));
        }
        previous = current;
    }

    if !saw_pair {
        return Err(format!("`{operator}` expects at least 2 arguments"));
    }

    Ok(Value::Boolean(true))
}

fn eval_not(arguments: &[Value]) -> Result<Value, String> {
    let [value] = arguments else {
        return Err("`not` expects exactly 1 argument".into());
    };

    Ok(Value::Boolean(!is_truthy(value)))
}

fn eval_cons(arguments: &[Value]) -> Result<Value, String> {
    let [car, cdr] = arguments else {
        return Err("`cons` expects exactly 2 arguments".into());
    };

    Ok(Value::Pair(Box::new(car.clone()), Box::new(cdr.clone())))
}

fn eval_car(arguments: &[Value]) -> Result<Value, String> {
    let [pair] = arguments else {
        return Err("`car` expects exactly 1 argument".into());
    };

    let Value::Pair(car, _) = pair else {
        return Err("`car` expects a pair".into());
    };

    Ok((**car).clone())
}

fn eval_cdr(arguments: &[Value]) -> Result<Value, String> {
    let [pair] = arguments else {
        return Err("`cdr` expects exactly 1 argument".into());
    };

    let Value::Pair(_, cdr) = pair else {
        return Err("`cdr` expects a pair".into());
    };

    Ok((**cdr).clone())
}

fn eval_is_null(arguments: &[Value]) -> Result<Value, String> {
    let [value] = arguments else {
        return Err("`null?` expects exactly 1 argument".into());
    };

    Ok(Value::Boolean(matches!(value, Value::Nil)))
}

fn eval_list(arguments: &[Value]) -> Result<Value, String> {
    Ok(build_list(arguments))
}

fn eval_length(arguments: &[Value]) -> Result<Value, String> {
    let [list] = arguments else {
        return Err("`length` expects exactly 1 argument".into());
    };

    list_length(list).map(Value::Integer)
}

fn eval_type_predicate<F>(arguments: &[Value], name: &str, predicate: F) -> Result<Value, String>
where
    F: FnOnce(&Value) -> bool,
{
    let [value] = arguments else {
        return Err(format!("`{name}` expects exactly 1 argument"));
    };

    Ok(Value::Boolean(predicate(value)))
}

fn build_list(values: &[Value]) -> Value {
    let mut list = Value::Nil;
    for value in values.iter().rev() {
        list = Value::Pair(Box::new(value.clone()), Box::new(list));
    }
    list
}

fn list_length(list: &Value) -> Result<i64, String> {
    let mut length = 0_i64;
    let mut current = list;

    loop {
        match current {
            Value::Nil => return Ok(length),
            Value::Pair(_, cdr) => {
                length = length
                    .checked_add(1)
                    .ok_or_else(|| "integer overflow".to_string())?;
                current = cdr.as_ref();
            }
            _ => return Err("`length` expects a proper list".into()),
        }
    }
}

fn list_to_vec(list: &Value) -> Result<Vec<Value>, String> {
    let mut values = Vec::new();
    let mut current = list;

    loop {
        match current {
            Value::Nil => return Ok(values),
            Value::Pair(car, cdr) => {
                values.push((**car).clone());
                current = cdr.as_ref();
            }
            _ => return Err("`apply` expects a proper list as its last argument".into()),
        }
    }
}

fn symbol_name(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Symbol(name) => Some(name.as_str()),
        _ => None,
    }
}

fn expect_integer(value: &Value, operator: &str) -> Result<i64, String> {
    match value {
        Value::Integer(number) => Ok(*number),
        _ => Err(format!("`{operator}` expects integer arguments")),
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Boolean(false))
}

struct Parser<'a> {
    input: &'a str,
    cursor: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, cursor: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, String> {
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if expressions.is_empty() {
            return Err("empty program".into());
        }

        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        match self.peek_char() {
            Some('(') => self.parse_application(),
            Some(')') => Err("unexpected `)`".into()),
            Some('\'') => self.parse_quote(),
            Some('"') => self.parse_string().map(Expr::Literal),
            Some('#') => self.parse_boolean().map(Expr::Literal),
            Some('-') if self.peek_next_is_digit() => self.parse_integer().map(Expr::Literal),
            Some('0'..='9') => self.parse_integer().map(Expr::Literal),
            Some(_) => self.parse_symbol().map(Expr::Symbol),
            None => Err("unexpected end of input".into()),
        }
    }

    fn parse_quote(&mut self) -> Result<Expr, String> {
        self.bump_char();
        self.skip_ignored();

        Ok(Expr::Application(vec![
            Expr::Symbol("quote".to_string()),
            self.parse_expr()?,
        ]))
    }

    fn parse_application(&mut self) -> Result<Expr, String> {
        self.bump_char();
        self.skip_ignored();

        let mut expressions = Vec::new();
        while matches!(self.peek_char(), Some(character) if character != ')') {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if self.peek_char().is_none() {
            return Err("unterminated list".into());
        }

        self.bump_char();
        Ok(Expr::Application(expressions))
    }

    fn parse_symbol(&mut self) -> Result<String, String> {
        let start = self.cursor;
        while matches!(self.peek_char(), Some(character) if !Self::is_delimiter(character)) {
            self.bump_char();
        }

        if start == self.cursor {
            return Err("invalid symbol".into());
        }

        Ok(self.input[start..self.cursor].to_string())
    }

    fn parse_boolean(&mut self) -> Result<Value, String> {
        if self.remaining().starts_with("#t") {
            self.cursor += 2;
            self.ensure_token_boundary("boolean")?;
            return Ok(Value::Boolean(true));
        }

        if self.remaining().starts_with("#f") {
            self.cursor += 2;
            self.ensure_token_boundary("boolean")?;
            return Ok(Value::Boolean(false));
        }

        Err("invalid boolean literal".into())
    }

    fn parse_integer(&mut self) -> Result<Value, String> {
        let start = self.cursor;

        if self.peek_char() == Some('-') {
            self.bump_char();
        }

        let digit_start = self.cursor;
        while matches!(self.peek_char(), Some('0'..='9')) {
            self.bump_char();
        }

        if digit_start == self.cursor {
            return Err("invalid integer literal".into());
        }

        let literal = &self.input[start..self.cursor];
        self.ensure_token_boundary("integer")?;

        literal
            .parse::<i64>()
            .map(Value::Integer)
            .map_err(|_| format!("integer literal out of range: {literal}"))
    }

    fn parse_string(&mut self) -> Result<Value, String> {
        self.bump_char();

        let mut contents = String::new();
        loop {
            match self.bump_char() {
                Some('"') => return Ok(Value::String(contents)),
                Some('\\') => contents.push(self.parse_escape_sequence()?),
                Some(character) => contents.push(character),
                None => return Err("unterminated string literal".into()),
            }
        }
    }

    fn parse_escape_sequence(&mut self) -> Result<char, String> {
        match self.bump_char() {
            Some('"') => Ok('"'),
            Some('\\') => Ok('\\'),
            Some('n') => Ok('\n'),
            Some('t') => Ok('\t'),
            Some(character) => Err(format!("unsupported string escape: \\{character}")),
            None => Err("unterminated string literal".into()),
        }
    }

    fn ensure_token_boundary(&self, kind: &str) -> Result<(), String> {
        match self.peek_char() {
            None => Ok(()),
            Some(character) if Self::is_delimiter(character) => Ok(()),
            Some(character) => Err(format!("unexpected `{character}` after {kind} literal")),
        }
    }

    fn skip_ignored(&mut self) {
        while self.skip_whitespace() || self.skip_line_comment() {}
    }

    fn skip_whitespace(&mut self) -> bool {
        let start = self.cursor;

        while matches!(self.peek_char(), Some(character) if character.is_whitespace()) {
            self.bump_char();
        }

        self.cursor != start
    }

    fn skip_line_comment(&mut self) -> bool {
        if self.peek_char() != Some(';') {
            return false;
        }

        while !matches!(self.peek_char(), None | Some('\n')) {
            self.bump_char();
        }

        true
    }

    fn remaining(&self) -> &'a str {
        &self.input[self.cursor..]
    }

    fn peek_char(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    fn peek_next_is_digit(&self) -> bool {
        self.remaining()
            .chars()
            .nth(1)
            .is_some_and(|character| character.is_ascii_digit())
    }

    fn bump_char(&mut self) -> Option<char> {
        let character = self.peek_char()?;
        self.cursor += character.len_utf8();
        Some(character)
    }

    fn is_eof(&self) -> bool {
        self.cursor == self.input.len()
    }

    fn is_delimiter(character: char) -> bool {
        character.is_whitespace() || matches!(character, '(' | ')' | ';')
    }
}
