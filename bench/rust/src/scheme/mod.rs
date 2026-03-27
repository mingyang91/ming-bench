pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type EnvRef = Rc<RefCell<Environment>>;
type EvalResult = Result<Value, EvalError>;
type ContRef = Rc<dyn Fn(Value) -> Action>;
type ValuesContRef = Rc<dyn Fn(Vec<Value>) -> Action>;
type WindFrameRef = Rc<DynamicWindFrame>;
type ExceptionHandlerRef = Rc<ExceptionHandlerFrame>;

enum Action {
    EvalExpr(Expr, EnvRef, ContRef),
    EvalSequence(Rc<Vec<Expr>>, usize, EnvRef, ContRef),
    EvalLetBindings {
        bindings: Rc<Vec<(String, Expr)>>,
        index: usize,
        env: EnvRef,
        evaluated: Vec<(String, Value)>,
        body: Rc<Vec<Expr>>,
        cont: ContRef,
    },
    EvalAnd(Rc<Vec<Expr>>, usize, EnvRef, ContRef),
    EvalOr(Rc<Vec<Expr>>, usize, EnvRef, ContRef),
    EvalCond(Rc<Vec<Expr>>, usize, EnvRef, ContRef),
    EvalExprList {
        expressions: Rc<Vec<Expr>>,
        index: usize,
        env: EnvRef,
        evaluated: Vec<Value>,
        cont: ValuesContRef,
    },
    Apply(Value, Vec<Value>, ContRef),
    MapIter {
        procedure: Value,
        lists: Rc<Vec<Vec<Value>>>,
        index: usize,
        results: Vec<Value>,
        cont: ContRef,
    },
    EvalGuardClauses {
        clauses: Rc<Vec<Expr>>,
        index: usize,
        env: EnvRef,
        exception: Value,
        cont: ContRef,
    },
    EnterDynamicWind {
        frame: WindFrameRef,
        body: Value,
        cont: ContRef,
    },
    ExitDynamicWind {
        frame: WindFrameRef,
        value: Value,
        cont: ContRef,
    },
    SwitchContinuation {
        continuation: CapturedContinuation,
        value: Value,
    },
    ReenterDynamicWind {
        frame: WindFrameRef,
        continuation: CapturedContinuation,
        value: Value,
    },
    ExitExceptionHandler {
        frame: ExceptionHandlerRef,
        value: Value,
        cont: ContRef,
    },
    Raise(Value),
    HandleException {
        frame: ExceptionHandlerRef,
        value: Value,
    },
    Continue(ContRef, Value),
    Done(EvalResult),
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
    eval_str_with_output(input).map(|(result, _)| result)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let input = input.to_string();
    std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(move || eval_str_with_output_inner(&input))
        .map_err(|err| EvalError::Message(format!("failed to start evaluator thread: {err}")))?
        .join()
        .map_err(|_| EvalError::Message("evaluation panicked".to_string()))?
}

fn eval_str_with_output_inner(input: &str) -> Result<(String, String), EvalError> {
    let expressions = Parser::new(input).parse_program()?;
    if expressions.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let env = global_env();
    let mut interpreter = Interpreter::default();
    let last = interpreter.run(Action::EvalSequence(
        Rc::new(expressions),
        0,
        env,
        return_cont(),
    ))?;
    Ok((last.to_scheme_string(), interpreter.output))
}

fn return_cont() -> ContRef {
    Rc::new(|value| Action::Done(Ok(value)))
}

#[derive(Default)]
struct Interpreter {
    output: String,
    dynamic_wind_stack: Vec<WindFrameRef>,
    exception_handler_stack: Vec<ExceptionHandlerRef>,
}

impl Interpreter {
    fn run(&mut self, mut action: Action) -> EvalResult {
        loop {
            action = match action {
                Action::EvalExpr(expr, env, cont) => self.step_eval_expr(expr, env, cont),
                Action::EvalSequence(expressions, index, env, cont) => {
                    self.step_eval_sequence(expressions, index, env, cont)
                }
                Action::EvalLetBindings {
                    bindings,
                    index,
                    env,
                    evaluated,
                    body,
                    cont,
                } => self.step_eval_let_bindings(bindings, index, env, evaluated, body, cont),
                Action::EvalAnd(expressions, index, env, cont) => {
                    self.step_eval_and(expressions, index, env, cont)
                }
                Action::EvalOr(expressions, index, env, cont) => {
                    self.step_eval_or(expressions, index, env, cont)
                }
                Action::EvalCond(clauses, index, env, cont) => {
                    self.step_eval_cond(clauses, index, env, cont)
                }
                Action::EvalExprList {
                    expressions,
                    index,
                    env,
                    evaluated,
                    cont,
                } => self.step_eval_expr_list(expressions, index, env, evaluated, cont),
                Action::Apply(procedure, args, cont) => self.step_apply(procedure, args, cont),
                Action::MapIter {
                    procedure,
                    lists,
                    index,
                    results,
                    cont,
                } => self.step_builtin_map_iter(procedure, lists, index, results, cont),
                Action::EvalGuardClauses {
                    clauses,
                    index,
                    env,
                    exception,
                    cont,
                } => self.step_eval_guard_clauses(clauses, index, env, exception, cont),
                Action::EnterDynamicWind { frame, body, cont } => {
                    self.step_enter_dynamic_wind(frame, body, cont)
                }
                Action::ExitDynamicWind { frame, value, cont } => {
                    self.step_exit_dynamic_wind(frame, value, cont)
                }
                Action::SwitchContinuation {
                    continuation,
                    value,
                } => self.step_switch_continuation(continuation, value),
                Action::ReenterDynamicWind {
                    frame,
                    continuation,
                    value,
                } => self.step_reenter_dynamic_wind(frame, continuation, value),
                Action::ExitExceptionHandler { frame, value, cont } => {
                    self.step_exit_exception_handler(frame, value, cont)
                }
                Action::Raise(value) => self.step_raise(value),
                Action::HandleException { frame, value } => {
                    self.step_handle_exception(frame, value)
                }
                Action::Continue(cont, value) => cont(value),
                Action::Done(result) => return result,
            };
        }
    }

    fn step_eval_expr(&mut self, expr: Expr, env: EnvRef, cont: ContRef) -> Action {
        match expr {
            Expr::Int(value) => Action::Continue(cont, Value::Int(value)),
            Expr::Bool(value) => Action::Continue(cont, Value::Bool(value)),
            Expr::String(value) => Action::Continue(cont, Value::String(value)),
            Expr::Char(value) => Action::Continue(cont, Value::Char(value)),
            Expr::Symbol(name) => match env.borrow().lookup(&name) {
                Some(value) => Action::Continue(cont, value),
                None => Action::Done(Err(EvalError::UnboundVariable(name))),
            },
            Expr::List(items) => self.step_eval_list(items, env, cont),
        }
    }

    fn step_eval_list(&mut self, items: Vec<Expr>, env: EnvRef, cont: ContRef) -> Action {
        if items.is_empty() {
            return Action::Done(Err(EvalError::InvalidForm(
                "cannot evaluate an empty list".to_string(),
            )));
        }

        if let Some(Expr::Symbol(name)) = items.first() {
            match name.as_str() {
                "define" => return self.step_eval_define(&items[1..], env, cont),
                "lambda" => return self.step_eval_lambda(&items[1..], env, cont),
                "case-lambda" => return self.step_eval_case_lambda(&items[1..], env, cont),
                "if" => return self.step_eval_if(&items[1..], env, cont),
                "let" => return self.step_eval_let(&items[1..], env, cont),
                "and" => return Action::EvalAnd(Rc::new(items[1..].to_vec()), 0, env, cont),
                "or" => return Action::EvalOr(Rc::new(items[1..].to_vec()), 0, env, cont),
                "begin" => {
                    return Action::EvalSequence(Rc::new(items[1..].to_vec()), 0, env, cont);
                }
                "quote" => {
                    if let Err(err) = require_exact_arity(items.len() - 1, 1, "quote") {
                        return Action::Done(Err(err));
                    }
                    return Action::Continue(cont, quote_expr(&items[1]));
                }
                "cond" => return Action::EvalCond(Rc::new(items[1..].to_vec()), 0, env, cont),
                "set!" => return self.step_eval_set(&items[1..], env, cont),
                "guard" => return self.step_eval_guard(&items[1..], env, cont),
                _ => {}
            }
        }

        self.step_eval_application(items, env, cont)
    }

    fn step_eval_application(&mut self, items: Vec<Expr>, env: EnvRef, cont: ContRef) -> Action {
        let mut parts = items.into_iter();
        let operator_expr = parts.next().expect("non-empty list");
        let arg_exprs = Rc::new(parts.collect::<Vec<_>>());

        Action::EvalExpr(
            operator_expr,
            env.clone(),
            Rc::new(move |operator| {
                let operator = operator.clone();
                let cont = cont.clone();
                Action::EvalExprList {
                    expressions: arg_exprs.clone(),
                    index: arg_exprs.len(),
                    env: env.clone(),
                    evaluated: Vec::new(),
                    cont: Rc::new(move |args| Action::Apply(operator.clone(), args, cont.clone())),
                }
            }),
        )
    }

    fn step_eval_define(&mut self, args: &[Expr], env: EnvRef, cont: ContRef) -> Action {
        if args.len() < 2 {
            return Action::Done(Err(EvalError::InvalidForm(
                "define expects a target and at least one body expression".to_string(),
            )));
        }

        match &args[0] {
            Expr::Symbol(name) => {
                if args.len() != 2 {
                    return Action::Done(Err(EvalError::InvalidForm(
                        "variable define expects exactly one value expression".to_string(),
                    )));
                }

                let name = name.clone();
                Action::EvalExpr(
                    args[1].clone(),
                    env.clone(),
                    Rc::new(move |value| {
                        env.borrow_mut().define(name.clone(), value);
                        Action::Continue(cont.clone(), Value::Void)
                    }),
                )
            }
            Expr::List(signature) => {
                if signature.is_empty() {
                    return Action::Done(Err(EvalError::InvalidForm(
                        "function define requires a name".to_string(),
                    )));
                }

                let name = match &signature[0] {
                    Expr::Symbol(name) => name.clone(),
                    _ => {
                        return Action::Done(Err(EvalError::InvalidForm(
                            "function define requires a symbol name".to_string(),
                        )));
                    }
                };

                let params = match parse_parameters(&signature[1..]) {
                    Ok(params) => params,
                    Err(err) => return Action::Done(Err(err)),
                };
                let procedure = Value::Procedure(Procedure::Lambda(Lambda {
                    name: Some(name.clone()),
                    params,
                    body: args[1..].to_vec(),
                    env: env.clone(),
                }));
                env.borrow_mut().define(name, procedure);
                Action::Continue(cont, Value::Void)
            }
            _ => Action::Done(Err(EvalError::InvalidForm(
                "invalid define target".to_string(),
            ))),
        }
    }

    fn step_eval_lambda(&mut self, args: &[Expr], env: EnvRef, cont: ContRef) -> Action {
        if args.len() < 2 {
            return Action::Done(Err(EvalError::InvalidForm(
                "lambda expects parameters and at least one body expression".to_string(),
            )));
        }

        let params = match &args[0] {
            Expr::List(items) => match parse_parameters(items) {
                Ok(params) => params,
                Err(err) => return Action::Done(Err(err)),
            },
            _ => {
                return Action::Done(Err(EvalError::InvalidForm(
                    "lambda parameters must be a list".to_string(),
                )));
            }
        };

        Action::Continue(
            cont,
            Value::Procedure(Procedure::Lambda(Lambda {
                name: None,
                params,
                body: args[1..].to_vec(),
                env,
            })),
        )
    }

    fn step_eval_case_lambda(&mut self, args: &[Expr], env: EnvRef, cont: ContRef) -> Action {
        if args.is_empty() {
            return Action::Done(Err(EvalError::InvalidForm(
                "case-lambda expects at least one clause".to_string(),
            )));
        }

        let mut clauses = Vec::with_capacity(args.len());
        for clause in args {
            let Expr::List(items) = clause else {
                return Action::Done(Err(EvalError::InvalidForm(
                    "case-lambda clauses must be lists".to_string(),
                )));
            };

            if items.len() < 2 {
                return Action::Done(Err(EvalError::InvalidForm(
                    "case-lambda clauses require parameters and a body".to_string(),
                )));
            }

            let params = match &items[0] {
                Expr::List(items) => match parse_parameters(items) {
                    Ok(params) => params,
                    Err(err) => return Action::Done(Err(err)),
                },
                _ => {
                    return Action::Done(Err(EvalError::InvalidForm(
                        "case-lambda parameters must be a list".to_string(),
                    )));
                }
            };

            clauses.push(Lambda {
                name: None,
                params,
                body: items[1..].to_vec(),
                env: env.clone(),
            });
        }

        Action::Continue(cont, Value::Procedure(Procedure::CaseLambda(clauses)))
    }

    fn step_eval_if(&mut self, args: &[Expr], env: EnvRef, cont: ContRef) -> Action {
        if !(2..=3).contains(&args.len()) {
            return Action::Done(Err(EvalError::WrongArity {
                name: "if".to_string(),
                expected: "2 or 3".to_string(),
                got: args.len(),
            }));
        }

        let then_branch = args[1].clone();
        let else_branch = args.get(2).cloned();
        Action::EvalExpr(
            args[0].clone(),
            env.clone(),
            Rc::new(move |condition| {
                if is_truthy(&condition) {
                    Action::EvalExpr(then_branch.clone(), env.clone(), cont.clone())
                } else if let Some(else_branch) = &else_branch {
                    Action::EvalExpr(else_branch.clone(), env.clone(), cont.clone())
                } else {
                    Action::Continue(cont.clone(), Value::Void)
                }
            }),
        )
    }

    fn step_eval_let(&mut self, args: &[Expr], env: EnvRef, cont: ContRef) -> Action {
        if args.len() < 2 {
            return Action::Done(Err(EvalError::InvalidForm(
                "let expects bindings and at least one body expression".to_string(),
            )));
        }

        let bindings = match &args[0] {
            Expr::List(bindings) => bindings,
            _ => {
                return Action::Done(Err(EvalError::InvalidForm(
                    "let bindings must be a list".to_string(),
                )));
            }
        };

        let mut parsed = Vec::with_capacity(bindings.len());
        for binding in bindings {
            let pair = match binding {
                Expr::List(pair) if pair.len() == 2 => pair,
                _ => {
                    return Action::Done(Err(EvalError::InvalidForm(
                        "let bindings must be (name value) pairs".to_string(),
                    )));
                }
            };

            let name = match &pair[0] {
                Expr::Symbol(name) => name.clone(),
                _ => {
                    return Action::Done(Err(EvalError::InvalidForm(
                        "let binding names must be symbols".to_string(),
                    )));
                }
            };

            parsed.push((name, pair[1].clone()));
        }

        Action::EvalLetBindings {
            bindings: Rc::new(parsed),
            index: 0,
            env,
            evaluated: Vec::new(),
            body: Rc::new(args[1..].to_vec()),
            cont,
        }
    }

    fn step_eval_let_bindings(
        &mut self,
        bindings: Rc<Vec<(String, Expr)>>,
        index: usize,
        env: EnvRef,
        evaluated: Vec<(String, Value)>,
        body: Rc<Vec<Expr>>,
        cont: ContRef,
    ) -> Action {
        if index >= bindings.len() {
            let child = Environment::child(env);
            {
                let mut scope = child.borrow_mut();
                for (name, value) in evaluated {
                    scope.define(name, value);
                }
            }
            return Action::EvalSequence(body, 0, child, cont);
        }

        let (name, expr) = bindings[index].clone();
        Action::EvalExpr(
            expr,
            env.clone(),
            Rc::new(move |value| {
                let mut next = evaluated.clone();
                next.push((name.clone(), value));
                Action::EvalLetBindings {
                    bindings: bindings.clone(),
                    index: index + 1,
                    env: env.clone(),
                    evaluated: next,
                    body: body.clone(),
                    cont: cont.clone(),
                }
            }),
        )
    }

    fn step_eval_and(
        &mut self,
        expressions: Rc<Vec<Expr>>,
        index: usize,
        env: EnvRef,
        cont: ContRef,
    ) -> Action {
        if index >= expressions.len() {
            return Action::Continue(cont, Value::Bool(true));
        }

        let last = index + 1 == expressions.len();
        Action::EvalExpr(
            expressions[index].clone(),
            env.clone(),
            Rc::new(move |value| {
                if !is_truthy(&value) || last {
                    Action::Continue(cont.clone(), value)
                } else {
                    Action::EvalAnd(expressions.clone(), index + 1, env.clone(), cont.clone())
                }
            }),
        )
    }

    fn step_eval_or(
        &mut self,
        expressions: Rc<Vec<Expr>>,
        index: usize,
        env: EnvRef,
        cont: ContRef,
    ) -> Action {
        if index >= expressions.len() {
            return Action::Continue(cont, Value::Bool(false));
        }

        Action::EvalExpr(
            expressions[index].clone(),
            env.clone(),
            Rc::new(move |value| {
                if is_truthy(&value) {
                    Action::Continue(cont.clone(), value)
                } else {
                    Action::EvalOr(expressions.clone(), index + 1, env.clone(), cont.clone())
                }
            }),
        )
    }

    fn step_eval_cond(
        &mut self,
        clauses: Rc<Vec<Expr>>,
        index: usize,
        env: EnvRef,
        cont: ContRef,
    ) -> Action {
        if index >= clauses.len() {
            return Action::Continue(cont, Value::Void);
        }

        let clause = match &clauses[index] {
            Expr::List(items) if !items.is_empty() => items.clone(),
            _ => {
                return Action::Done(Err(EvalError::InvalidForm(
                    "cond clauses must be non-empty lists".to_string(),
                )));
            }
        };

        if matches!(&clause[0], Expr::Symbol(name) if name == "else") {
            if clause.len() == 1 {
                return Action::Continue(cont, Value::Void);
            }
            return Action::EvalSequence(Rc::new(clause[1..].to_vec()), 0, env, cont);
        }

        let test_expr = clause[0].clone();
        let body = clause[1..].to_vec();
        Action::EvalExpr(
            test_expr,
            env.clone(),
            Rc::new(move |test_value| {
                if is_truthy(&test_value) {
                    if body.is_empty() {
                        Action::Continue(cont.clone(), test_value)
                    } else {
                        Action::EvalSequence(Rc::new(body.clone()), 0, env.clone(), cont.clone())
                    }
                } else {
                    Action::EvalCond(clauses.clone(), index + 1, env.clone(), cont.clone())
                }
            }),
        )
    }

    fn step_eval_set(&mut self, args: &[Expr], env: EnvRef, cont: ContRef) -> Action {
        if args.len() != 2 {
            return Action::Done(Err(EvalError::WrongArity {
                name: "set!".to_string(),
                expected: "2".to_string(),
                got: args.len(),
            }));
        }

        let name = match &args[0] {
            Expr::Symbol(name) => name.clone(),
            _ => {
                return Action::Done(Err(EvalError::InvalidForm(
                    "set! target must be a symbol".to_string(),
                )));
            }
        };

        Action::EvalExpr(
            args[1].clone(),
            env.clone(),
            Rc::new(move |value| {
                if env.borrow_mut().set(&name, value) {
                    Action::Continue(cont.clone(), Value::Void)
                } else {
                    Action::Done(Err(EvalError::UnboundVariable(name.clone())))
                }
            }),
        )
    }

    fn step_eval_guard(&mut self, args: &[Expr], env: EnvRef, cont: ContRef) -> Action {
        if args.len() < 2 {
            return Action::Done(Err(EvalError::InvalidForm(
                "guard expects a clause list and at least one body expression".to_string(),
            )));
        }

        let header = match &args[0] {
            Expr::List(items) if !items.is_empty() => items,
            _ => {
                return Action::Done(Err(EvalError::InvalidForm(
                    "guard expects (variable clause ...) as its first argument".to_string(),
                )));
            }
        };

        let variable = match &header[0] {
            Expr::Symbol(name) => name.clone(),
            _ => {
                return Action::Done(Err(EvalError::InvalidForm(
                    "guard variable must be a symbol".to_string(),
                )));
            }
        };

        let frame = Rc::new(ExceptionHandlerFrame {
            kind: ExceptionHandlerKind::Guard {
                variable,
                clauses: Rc::new(header[1..].to_vec()),
                env: env.clone(),
                cont: cont.clone(),
            },
            wind_stack: self.dynamic_wind_stack.clone(),
        });
        self.exception_handler_stack.push(frame.clone());

        Action::EvalSequence(
            Rc::new(args[1..].to_vec()),
            0,
            env,
            Rc::new(move |value| Action::ExitExceptionHandler {
                frame: frame.clone(),
                value,
                cont: cont.clone(),
            }),
        )
    }

    fn step_eval_sequence(
        &mut self,
        expressions: Rc<Vec<Expr>>,
        index: usize,
        env: EnvRef,
        cont: ContRef,
    ) -> Action {
        if index >= expressions.len() {
            return Action::Continue(cont, Value::Void);
        }

        if index + 1 == expressions.len() {
            return Action::EvalExpr(expressions[index].clone(), env, cont);
        }

        Action::EvalExpr(
            expressions[index].clone(),
            env.clone(),
            Rc::new(move |_| {
                Action::EvalSequence(expressions.clone(), index + 1, env.clone(), cont.clone())
            }),
        )
    }

    fn step_eval_guard_clauses(
        &mut self,
        clauses: Rc<Vec<Expr>>,
        index: usize,
        env: EnvRef,
        exception: Value,
        cont: ContRef,
    ) -> Action {
        if index >= clauses.len() {
            return Action::Raise(exception);
        }

        let clause = match &clauses[index] {
            Expr::List(items) if !items.is_empty() => items.clone(),
            _ => {
                return Action::Done(Err(EvalError::InvalidForm(
                    "guard clauses must be non-empty lists".to_string(),
                )));
            }
        };

        if matches!(&clause[0], Expr::Symbol(name) if name == "else") {
            if clause.len() == 1 {
                return Action::Continue(cont, Value::Void);
            }
            return Action::EvalSequence(Rc::new(clause[1..].to_vec()), 0, env, cont);
        }

        let test_expr = clause[0].clone();
        let body = clause[1..].to_vec();
        Action::EvalExpr(
            test_expr,
            env.clone(),
            Rc::new(move |test_value| {
                if is_truthy(&test_value) {
                    if body.is_empty() {
                        Action::Continue(cont.clone(), test_value)
                    } else {
                        Action::EvalSequence(Rc::new(body.clone()), 0, env.clone(), cont.clone())
                    }
                } else {
                    Action::EvalGuardClauses {
                        clauses: clauses.clone(),
                        index: index + 1,
                        env: env.clone(),
                        exception: exception.clone(),
                        cont: cont.clone(),
                    }
                }
            }),
        )
    }

    fn step_eval_expr_list(
        &mut self,
        expressions: Rc<Vec<Expr>>,
        index: usize,
        env: EnvRef,
        evaluated: Vec<Value>,
        cont: ValuesContRef,
    ) -> Action {
        if index == 0 {
            return cont(evaluated);
        }

        Action::EvalExpr(
            expressions[index - 1].clone(),
            env.clone(),
            Rc::new(move |value| {
                let mut next = evaluated.clone();
                next.insert(0, value);
                Action::EvalExprList {
                    expressions: expressions.clone(),
                    index: index - 1,
                    env: env.clone(),
                    evaluated: next,
                    cont: cont.clone(),
                }
            }),
        )
    }

    fn step_apply(&mut self, procedure: Value, args: Vec<Value>, cont: ContRef) -> Action {
        match procedure {
            Value::Procedure(Procedure::Builtin(builtin)) => {
                self.step_apply_builtin(builtin, args, cont)
            }
            Value::Procedure(Procedure::Lambda(lambda)) => {
                self.step_apply_lambda(lambda, args, cont)
            }
            Value::Procedure(Procedure::CaseLambda(clauses)) => {
                self.step_apply_case_lambda(clauses, args, cont)
            }
            Value::Procedure(Procedure::Continuation(saved)) => {
                if let Err(err) = require_exact_arity(args.len(), 1, "continuation") {
                    Action::Done(Err(err))
                } else {
                    Action::SwitchContinuation {
                        continuation: saved,
                        value: args[0].clone(),
                    }
                }
            }
            _ => Action::Done(Err(EvalError::Message(
                "attempted to call a non-procedure".to_string(),
            ))),
        }
    }

    fn step_apply_lambda(&mut self, lambda: Lambda, args: Vec<Value>, cont: ContRef) -> Action {
        if lambda.params.len() != args.len() {
            return Action::Done(Err(EvalError::WrongArity {
                name: lambda.name.clone().unwrap_or_else(|| "lambda".to_string()),
                expected: lambda.params.len().to_string(),
                got: args.len(),
            }));
        }

        let child = Environment::child(lambda.env);
        {
            let mut scope = child.borrow_mut();
            for (name, value) in lambda.params.iter().cloned().zip(args) {
                scope.define(name, value);
            }
        }

        Action::EvalSequence(Rc::new(lambda.body), 0, child, cont)
    }

    fn step_apply_case_lambda(
        &mut self,
        clauses: Vec<Lambda>,
        args: Vec<Value>,
        cont: ContRef,
    ) -> Action {
        for clause in clauses {
            if clause.params.len() == args.len() {
                return self.step_apply_lambda(clause, args, cont);
            }
        }

        Action::Done(Err(EvalError::WrongArity {
            name: "case-lambda".to_string(),
            expected: "matching clause".to_string(),
            got: args.len(),
        }))
    }

    fn step_apply_builtin(&mut self, builtin: Builtin, args: Vec<Value>, cont: ContRef) -> Action {
        match builtin {
            Builtin::Add => {
                let mut total = 0_i64;
                for arg in &args {
                    match expect_int(arg, builtin.name()) {
                        Ok(value) => total += value,
                        Err(err) => return Action::Done(Err(err)),
                    }
                }
                Action::Continue(cont, Value::Int(total))
            }
            Builtin::Sub => {
                if let Err(err) = require_at_least_arity(args.len(), 1, builtin.name()) {
                    return Action::Done(Err(err));
                }
                let first = match expect_int(&args[0], builtin.name()) {
                    Ok(value) => value,
                    Err(err) => return Action::Done(Err(err)),
                };
                if args.len() == 1 {
                    return Action::Continue(cont, Value::Int(-first));
                }

                let mut total = first;
                for arg in &args[1..] {
                    match expect_int(arg, builtin.name()) {
                        Ok(value) => total -= value,
                        Err(err) => return Action::Done(Err(err)),
                    }
                }
                Action::Continue(cont, Value::Int(total))
            }
            Builtin::Less => {
                match compare_numbers(&args, builtin.name(), |left, right| left < right) {
                    Ok(value) => Action::Continue(cont, value),
                    Err(err) => Action::Done(Err(err)),
                }
            }
            Builtin::LessEqual => {
                match compare_numbers(&args, builtin.name(), |left, right| left <= right) {
                    Ok(value) => Action::Continue(cont, value),
                    Err(err) => Action::Done(Err(err)),
                }
            }
            Builtin::Greater => {
                match compare_numbers(&args, builtin.name(), |left, right| left > right) {
                    Ok(value) => Action::Continue(cont, value),
                    Err(err) => Action::Done(Err(err)),
                }
            }
            Builtin::GreaterEqual => {
                match compare_numbers(&args, builtin.name(), |left, right| left >= right) {
                    Ok(value) => Action::Continue(cont, value),
                    Err(err) => Action::Done(Err(err)),
                }
            }
            Builtin::NumericEqual => {
                match compare_numbers(&args, builtin.name(), |left, right| left == right) {
                    Ok(value) => Action::Continue(cont, value),
                    Err(err) => Action::Done(Err(err)),
                }
            }
            Builtin::Cons => {
                if let Err(err) = require_exact_arity(args.len(), 2, builtin.name()) {
                    return Action::Done(Err(err));
                }

                let tail = match expect_list(&args[1], builtin.name()) {
                    Ok(items) => items,
                    Err(err) => return Action::Done(Err(err)),
                };
                let mut items = Vec::with_capacity(tail.len() + 1);
                items.push(args[0].clone());
                items.extend_from_slice(tail);
                Action::Continue(cont, Value::List(items))
            }
            Builtin::List => Action::Continue(cont, Value::List(args)),
            Builtin::Length => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    return Action::Done(Err(err));
                }
                match expect_list(&args[0], builtin.name()) {
                    Ok(items) => Action::Continue(cont, Value::Int(items.len() as i64)),
                    Err(err) => Action::Done(Err(err)),
                }
            }
            Builtin::Reverse => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    return Action::Done(Err(err));
                }
                match expect_list(&args[0], builtin.name()) {
                    Ok(items) => {
                        let mut reversed = items.to_vec();
                        reversed.reverse();
                        Action::Continue(cont, Value::List(reversed))
                    }
                    Err(err) => Action::Done(Err(err)),
                }
            }
            Builtin::Map => self.step_builtin_map(args, cont),
            Builtin::StringToList => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    return Action::Done(Err(err));
                }
                match expect_string(&args[0], builtin.name()) {
                    Ok(value) => Action::Continue(
                        cont,
                        Value::List(value.chars().map(Value::Char).collect()),
                    ),
                    Err(err) => Action::Done(Err(err)),
                }
            }
            Builtin::ListToString => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    return Action::Done(Err(err));
                }
                let items = match expect_list(&args[0], builtin.name()) {
                    Ok(items) => items,
                    Err(err) => return Action::Done(Err(err)),
                };
                let mut value = String::new();
                for item in items {
                    match expect_char(item, builtin.name()) {
                        Ok(ch) => value.push(ch),
                        Err(err) => return Action::Done(Err(err)),
                    }
                }
                Action::Continue(cont, Value::String(value))
            }
            Builtin::StringSet => {
                if let Err(err) = require_exact_arity(args.len(), 3, builtin.name()) {
                    Action::Done(Err(err))
                } else {
                    Action::Done(Err(EvalError::ImmutableString))
                }
            }
            Builtin::CharToInteger => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    return Action::Done(Err(err));
                }
                match expect_char(&args[0], builtin.name()) {
                    Ok(ch) => Action::Continue(cont, Value::Int(ch as u32 as i64)),
                    Err(err) => Action::Done(Err(err)),
                }
            }
            Builtin::IntegerToChar => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    return Action::Done(Err(err));
                }
                let value = match expect_int(&args[0], builtin.name()) {
                    Ok(value) => value,
                    Err(err) => return Action::Done(Err(err)),
                };
                if value < 0 {
                    return Action::Done(Err(EvalError::Type(format!(
                        "{0} expects a valid character code point",
                        builtin.name()
                    ))));
                }
                let scalar = match u32::try_from(value) {
                    Ok(scalar) => scalar,
                    Err(_) => {
                        return Action::Done(Err(EvalError::Type(format!(
                            "{0} expects a valid character code point",
                            builtin.name()
                        ))));
                    }
                };
                match char::from_u32(scalar) {
                    Some(ch) => Action::Continue(cont, Value::Char(ch)),
                    None => Action::Done(Err(EvalError::Type(format!(
                        "{0} expects a valid character code point",
                        builtin.name()
                    )))),
                }
            }
            Builtin::StringCopy => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    return Action::Done(Err(err));
                }
                match expect_string(&args[0], builtin.name()) {
                    Ok(value) => Action::Continue(cont, Value::String(value.to_string())),
                    Err(err) => Action::Done(Err(err)),
                }
            }
            Builtin::StringAppend => {
                let mut combined = String::new();
                for arg in &args {
                    match expect_string(arg, builtin.name()) {
                        Ok(value) => combined.push_str(value),
                        Err(err) => return Action::Done(Err(err)),
                    }
                }
                Action::Continue(cont, Value::String(combined))
            }
            Builtin::Display => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    return Action::Done(Err(err));
                }
                self.output.push_str(&args[0].to_display_string());
                Action::Continue(cont, Value::Void)
            }
            Builtin::Write => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    return Action::Done(Err(err));
                }
                self.output.push_str(&args[0].to_scheme_string());
                Action::Continue(cont, Value::Void)
            }
            Builtin::Newline => {
                if let Err(err) = require_exact_arity(args.len(), 0, builtin.name()) {
                    return Action::Done(Err(err));
                }
                self.output.push('\n');
                Action::Continue(cont, Value::Void)
            }
            Builtin::Not => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    Action::Done(Err(err))
                } else {
                    Action::Continue(cont, Value::Bool(!is_truthy(&args[0])))
                }
            }
            Builtin::Null => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    Action::Done(Err(err))
                } else {
                    Action::Continue(
                        cont,
                        Value::Bool(matches!(&args[0], Value::List(items) if items.is_empty())),
                    )
                }
            }
            Builtin::NumberPredicate => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    Action::Done(Err(err))
                } else {
                    Action::Continue(cont, Value::Bool(matches!(&args[0], Value::Int(_))))
                }
            }
            Builtin::BooleanPredicate => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    Action::Done(Err(err))
                } else {
                    Action::Continue(cont, Value::Bool(matches!(&args[0], Value::Bool(_))))
                }
            }
            Builtin::StringPredicate => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    Action::Done(Err(err))
                } else {
                    Action::Continue(cont, Value::Bool(matches!(&args[0], Value::String(_))))
                }
            }
            Builtin::SymbolPredicate => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    Action::Done(Err(err))
                } else {
                    Action::Continue(cont, Value::Bool(matches!(&args[0], Value::Symbol(_))))
                }
            }
            Builtin::PairPredicate => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    Action::Done(Err(err))
                } else {
                    Action::Continue(
                        cont,
                        Value::Bool(matches!(&args[0], Value::List(items) if !items.is_empty())),
                    )
                }
            }
            Builtin::ListPredicate => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    Action::Done(Err(err))
                } else {
                    Action::Continue(cont, Value::Bool(matches!(&args[0], Value::List(_))))
                }
            }
            Builtin::ProcedurePredicate => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    Action::Done(Err(err))
                } else {
                    Action::Continue(cont, Value::Bool(matches!(&args[0], Value::Procedure(_))))
                }
            }
            Builtin::CharPredicate => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    Action::Done(Err(err))
                } else {
                    Action::Continue(cont, Value::Bool(matches!(&args[0], Value::Char(_))))
                }
            }
            Builtin::Car => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    return Action::Done(Err(err));
                }
                let items = match expect_list(&args[0], builtin.name()) {
                    Ok(items) => items,
                    Err(err) => return Action::Done(Err(err)),
                };
                match items.first().cloned() {
                    Some(value) => Action::Continue(cont, value),
                    None => Action::Done(Err(EvalError::Type(
                        "car expects a non-empty list".to_string(),
                    ))),
                }
            }
            Builtin::Cdr => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    return Action::Done(Err(err));
                }
                let items = match expect_list(&args[0], builtin.name()) {
                    Ok(items) => items,
                    Err(err) => return Action::Done(Err(err)),
                };
                if items.is_empty() {
                    Action::Done(Err(EvalError::Type(
                        "cdr expects a non-empty list".to_string(),
                    )))
                } else {
                    Action::Continue(cont, Value::List(items[1..].to_vec()))
                }
            }
            Builtin::Raise => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    Action::Done(Err(err))
                } else {
                    Action::Raise(args[0].clone())
                }
            }
            Builtin::WithExceptionHandler => {
                if let Err(err) = require_exact_arity(args.len(), 2, builtin.name()) {
                    return Action::Done(Err(err));
                }
                if let Err(err) = expect_procedure_value(&args[0], builtin.name(), "handler") {
                    return Action::Done(Err(err));
                }
                if let Err(err) = expect_procedure_value(&args[1], builtin.name(), "thunk") {
                    return Action::Done(Err(err));
                }

                let frame = Rc::new(ExceptionHandlerFrame {
                    kind: ExceptionHandlerKind::Procedure {
                        handler: args[0].clone(),
                        cont: cont.clone(),
                    },
                    wind_stack: self.dynamic_wind_stack.clone(),
                });
                self.exception_handler_stack.push(frame.clone());

                Action::Apply(
                    args[1].clone(),
                    vec![],
                    Rc::new(move |value| Action::ExitExceptionHandler {
                        frame: frame.clone(),
                        value,
                        cont: cont.clone(),
                    }),
                )
            }
            Builtin::DynamicWind => {
                if let Err(err) = require_exact_arity(args.len(), 3, builtin.name()) {
                    return Action::Done(Err(err));
                }

                let before = args[0].clone();
                let body = args[1].clone();
                let frame = Rc::new(DynamicWindFrame {
                    before: before.clone(),
                    after: args[2].clone(),
                });

                Action::Apply(
                    before,
                    vec![],
                    Rc::new(move |_| Action::EnterDynamicWind {
                        frame: frame.clone(),
                        body: body.clone(),
                        cont: cont.clone(),
                    }),
                )
            }
            Builtin::CallCc => {
                if let Err(err) = require_exact_arity(args.len(), 1, builtin.name()) {
                    return Action::Done(Err(err));
                }
                let captured = Value::Procedure(Procedure::Continuation(CapturedContinuation {
                    cont: cont.clone(),
                    wind_stack: self.dynamic_wind_stack.clone(),
                    handler_stack: self.exception_handler_stack.clone(),
                }));
                Action::Apply(args[0].clone(), vec![captured], cont)
            }
        }
    }

    fn step_builtin_map(&mut self, args: Vec<Value>, cont: ContRef) -> Action {
        if let Err(err) = require_at_least_arity(args.len(), 2, "map") {
            return Action::Done(Err(err));
        }
        let procedure = args[0].clone();
        let lists = match args[1..]
            .iter()
            .map(|value| expect_list(value, "map").map(|items| items.to_vec()))
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(lists) => lists,
            Err(err) => return Action::Done(Err(err)),
        };

        let expected_len = lists[0].len();
        if lists.iter().any(|items| items.len() != expected_len) {
            return Action::Done(Err(EvalError::Message(
                "map expects lists with the same length".to_string(),
            )));
        }

        Action::MapIter {
            procedure,
            lists: Rc::new(lists),
            index: 0,
            results: Vec::new(),
            cont,
        }
    }

    fn step_builtin_map_iter(
        &mut self,
        procedure: Value,
        lists: Rc<Vec<Vec<Value>>>,
        index: usize,
        results: Vec<Value>,
        cont: ContRef,
    ) -> Action {
        if index >= lists[0].len() {
            return Action::Continue(cont, Value::List(results));
        }

        let mapped_args = lists
            .iter()
            .map(|items| items[index].clone())
            .collect::<Vec<_>>();

        Action::Apply(
            procedure.clone(),
            mapped_args,
            Rc::new(move |value| {
                let mut next = results.clone();
                next.push(value);
                Action::MapIter {
                    procedure: procedure.clone(),
                    lists: lists.clone(),
                    index: index + 1,
                    results: next,
                    cont: cont.clone(),
                }
            }),
        )
    }

    fn step_enter_dynamic_wind(
        &mut self,
        frame: WindFrameRef,
        body: Value,
        cont: ContRef,
    ) -> Action {
        self.dynamic_wind_stack.push(frame.clone());
        Action::Apply(
            body,
            vec![],
            Rc::new(move |value| Action::ExitDynamicWind {
                frame: frame.clone(),
                value,
                cont: cont.clone(),
            }),
        )
    }

    fn step_exit_dynamic_wind(
        &mut self,
        frame: WindFrameRef,
        value: Value,
        cont: ContRef,
    ) -> Action {
        self.remove_dynamic_wind_frame(&frame);
        Action::Apply(
            frame.after.clone(),
            vec![],
            Rc::new(move |_| Action::Continue(cont.clone(), value.clone())),
        )
    }

    fn step_switch_continuation(
        &mut self,
        continuation: CapturedContinuation,
        value: Value,
    ) -> Action {
        let shared =
            common_dynamic_wind_prefix_len(&self.dynamic_wind_stack, &continuation.wind_stack);

        if self.dynamic_wind_stack.len() > shared {
            let frame = self
                .dynamic_wind_stack
                .pop()
                .expect("active dynamic-wind frame");
            return Action::Apply(
                frame.after.clone(),
                vec![],
                Rc::new(move |_| Action::SwitchContinuation {
                    continuation: continuation.clone(),
                    value: value.clone(),
                }),
            );
        }

        if continuation.wind_stack.len() > shared {
            let frame = continuation.wind_stack[shared].clone();
            return Action::Apply(
                frame.before.clone(),
                vec![],
                Rc::new(move |_| Action::ReenterDynamicWind {
                    frame: frame.clone(),
                    continuation: continuation.clone(),
                    value: value.clone(),
                }),
            );
        }

        self.exception_handler_stack = continuation.handler_stack.clone();
        Action::Continue(continuation.cont, value)
    }

    fn step_reenter_dynamic_wind(
        &mut self,
        frame: WindFrameRef,
        continuation: CapturedContinuation,
        value: Value,
    ) -> Action {
        self.dynamic_wind_stack.push(frame);
        Action::SwitchContinuation {
            continuation,
            value,
        }
    }

    fn step_exit_exception_handler(
        &mut self,
        frame: ExceptionHandlerRef,
        value: Value,
        cont: ContRef,
    ) -> Action {
        self.remove_exception_handler(&frame);
        Action::Continue(cont, value)
    }

    fn step_raise(&mut self, value: Value) -> Action {
        let Some(frame) = self.exception_handler_stack.pop() else {
            return Action::Done(Err(EvalError::Message(format!(
                "uncaught exception: {}",
                value.to_scheme_string()
            ))));
        };

        Action::HandleException { frame, value }
    }

    fn step_handle_exception(&mut self, frame: ExceptionHandlerRef, value: Value) -> Action {
        let shared = common_dynamic_wind_prefix_len(&self.dynamic_wind_stack, &frame.wind_stack);

        if self.dynamic_wind_stack.len() > shared {
            let active = self
                .dynamic_wind_stack
                .pop()
                .expect("active dynamic-wind frame");
            return Action::Apply(
                active.after.clone(),
                vec![],
                Rc::new(move |_| Action::HandleException {
                    frame: frame.clone(),
                    value: value.clone(),
                }),
            );
        }

        match &frame.kind {
            ExceptionHandlerKind::Procedure { handler, cont } => {
                Action::Apply(handler.clone(), vec![value], cont.clone())
            }
            ExceptionHandlerKind::Guard {
                variable,
                clauses,
                env,
                cont,
            } => {
                let handler_env = Environment::child(env.clone());
                handler_env
                    .borrow_mut()
                    .define(variable.clone(), value.clone());
                Action::EvalGuardClauses {
                    clauses: clauses.clone(),
                    index: 0,
                    env: handler_env,
                    exception: value,
                    cont: cont.clone(),
                }
            }
        }
    }

    fn remove_dynamic_wind_frame(&mut self, frame: &WindFrameRef) {
        if let Some(active) = self.dynamic_wind_stack.last() {
            if Rc::ptr_eq(active, frame) {
                self.dynamic_wind_stack.pop();
                return;
            }
        }

        if let Some(index) = self
            .dynamic_wind_stack
            .iter()
            .rposition(|active| Rc::ptr_eq(active, frame))
        {
            self.dynamic_wind_stack.remove(index);
        }
    }

    fn remove_exception_handler(&mut self, frame: &ExceptionHandlerRef) {
        if let Some(active) = self.exception_handler_stack.last() {
            if Rc::ptr_eq(active, frame) {
                self.exception_handler_stack.pop();
                return;
            }
        }

        if let Some(index) = self
            .exception_handler_stack
            .iter()
            .rposition(|active| Rc::ptr_eq(active, frame))
        {
            self.exception_handler_stack.remove(index);
        }
    }
}

#[derive(Clone, Debug)]
enum Expr {
    Int(i64),
    Bool(bool),
    String(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone)]
enum Value {
    Int(i64),
    Bool(bool),
    String(String),
    Char(char),
    Symbol(String),
    List(Vec<Value>),
    Procedure(Procedure),
    Void,
}

impl Value {
    fn to_scheme_string(&self) -> String {
        match self {
            Value::Int(value) => value.to_string(),
            Value::Bool(true) => "#t".to_string(),
            Value::Bool(false) => "#f".to_string(),
            Value::String(value) => quote_string(value),
            Value::Char(value) => char_literal(*value),
            Value::Symbol(value) => value.clone(),
            Value::List(items) => {
                let parts = items
                    .iter()
                    .map(Value::to_scheme_string)
                    .collect::<Vec<_>>();
                format!("({})", parts.join(" "))
            }
            Value::Procedure(_) => "#<procedure>".to_string(),
            Value::Void => String::new(),
        }
    }

    fn to_display_string(&self) -> String {
        match self {
            Value::String(value) => value.clone(),
            Value::Char(value) => value.to_string(),
            _ => self.to_scheme_string(),
        }
    }

    fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) => "integer",
            Value::Bool(_) => "boolean",
            Value::String(_) => "string",
            Value::Char(_) => "character",
            Value::Symbol(_) => "symbol",
            Value::List(_) => "list",
            Value::Procedure(_) => "procedure",
            Value::Void => "void",
        }
    }
}

#[derive(Clone)]
enum Procedure {
    Builtin(Builtin),
    Lambda(Lambda),
    CaseLambda(Vec<Lambda>),
    Continuation(CapturedContinuation),
}

#[derive(Clone)]
struct CapturedContinuation {
    cont: ContRef,
    wind_stack: Vec<WindFrameRef>,
    handler_stack: Vec<ExceptionHandlerRef>,
}

struct DynamicWindFrame {
    before: Value,
    after: Value,
}

#[derive(Clone)]
enum ExceptionHandlerKind {
    Procedure {
        handler: Value,
        cont: ContRef,
    },
    Guard {
        variable: String,
        clauses: Rc<Vec<Expr>>,
        env: EnvRef,
        cont: ContRef,
    },
}

struct ExceptionHandlerFrame {
    kind: ExceptionHandlerKind,
    wind_stack: Vec<WindFrameRef>,
}

#[derive(Clone)]
struct Lambda {
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone, Copy)]
enum Builtin {
    Add,
    Sub,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    NumericEqual,
    Cons,
    List,
    Length,
    Reverse,
    Map,
    StringToList,
    ListToString,
    StringSet,
    CharToInteger,
    IntegerToChar,
    StringCopy,
    Display,
    Write,
    Newline,
    StringAppend,
    Not,
    Null,
    NumberPredicate,
    BooleanPredicate,
    StringPredicate,
    SymbolPredicate,
    PairPredicate,
    ListPredicate,
    ProcedurePredicate,
    CharPredicate,
    Car,
    Cdr,
    Raise,
    WithExceptionHandler,
    DynamicWind,
    CallCc,
}

impl Builtin {
    fn name(self) -> &'static str {
        match self {
            Builtin::Add => "+",
            Builtin::Sub => "-",
            Builtin::Less => "<",
            Builtin::LessEqual => "<=",
            Builtin::Greater => ">",
            Builtin::GreaterEqual => ">=",
            Builtin::NumericEqual => "=",
            Builtin::Cons => "cons",
            Builtin::List => "list",
            Builtin::Length => "length",
            Builtin::Reverse => "reverse",
            Builtin::Map => "map",
            Builtin::StringToList => "string->list",
            Builtin::ListToString => "list->string",
            Builtin::StringSet => "string-set!",
            Builtin::CharToInteger => "char->integer",
            Builtin::IntegerToChar => "integer->char",
            Builtin::StringCopy => "string-copy",
            Builtin::Display => "display",
            Builtin::Write => "write",
            Builtin::Newline => "newline",
            Builtin::StringAppend => "string-append",
            Builtin::Not => "not",
            Builtin::Null => "null?",
            Builtin::NumberPredicate => "number?",
            Builtin::BooleanPredicate => "boolean?",
            Builtin::StringPredicate => "string?",
            Builtin::SymbolPredicate => "symbol?",
            Builtin::PairPredicate => "pair?",
            Builtin::ListPredicate => "list?",
            Builtin::ProcedurePredicate => "procedure?",
            Builtin::CharPredicate => "char?",
            Builtin::Car => "car",
            Builtin::Cdr => "cdr",
            Builtin::Raise => "raise",
            Builtin::WithExceptionHandler => "with-exception-handler",
            Builtin::DynamicWind => "dynamic-wind",
            Builtin::CallCc => "call/cc",
        }
    }
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: HashMap<String, Value>,
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> Self {
        Self {
            parent,
            bindings: HashMap::new(),
        }
    }

    fn child(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self::new(Some(parent))))
    }

    fn define(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        self.bindings.get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().lookup(name))
        })
    }

    fn set(&mut self, name: &str, value: Value) -> bool {
        if let Some(slot) = self.bindings.get_mut(name) {
            *slot = value;
            true
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().set(name, value)
        } else {
            false
        }
    }
}

fn global_env() -> EnvRef {
    let env = Rc::new(RefCell::new(Environment::new(None)));
    let builtins = [
        ("+", Builtin::Add),
        ("-", Builtin::Sub),
        ("<", Builtin::Less),
        ("<=", Builtin::LessEqual),
        (">", Builtin::Greater),
        (">=", Builtin::GreaterEqual),
        ("=", Builtin::NumericEqual),
        ("cons", Builtin::Cons),
        ("list", Builtin::List),
        ("length", Builtin::Length),
        ("reverse", Builtin::Reverse),
        ("map", Builtin::Map),
        ("string->list", Builtin::StringToList),
        ("list->string", Builtin::ListToString),
        ("string-set!", Builtin::StringSet),
        ("char->integer", Builtin::CharToInteger),
        ("integer->char", Builtin::IntegerToChar),
        ("string-copy", Builtin::StringCopy),
        ("display", Builtin::Display),
        ("write", Builtin::Write),
        ("newline", Builtin::Newline),
        ("string-append", Builtin::StringAppend),
        ("not", Builtin::Not),
        ("null?", Builtin::Null),
        ("number?", Builtin::NumberPredicate),
        ("boolean?", Builtin::BooleanPredicate),
        ("string?", Builtin::StringPredicate),
        ("symbol?", Builtin::SymbolPredicate),
        ("pair?", Builtin::PairPredicate),
        ("list?", Builtin::ListPredicate),
        ("procedure?", Builtin::ProcedurePredicate),
        ("char?", Builtin::CharPredicate),
        ("car", Builtin::Car),
        ("cdr", Builtin::Cdr),
        ("raise", Builtin::Raise),
        ("with-exception-handler", Builtin::WithExceptionHandler),
        ("dynamic-wind", Builtin::DynamicWind),
        ("call/cc", Builtin::CallCc),
        ("call-with-current-continuation", Builtin::CallCc),
    ];

    {
        let mut scope = env.borrow_mut();
        for (name, builtin) in builtins {
            scope.define(
                name.to_string(),
                Value::Procedure(Procedure::Builtin(builtin)),
            );
        }
    }

    env
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn common_dynamic_wind_prefix_len(current: &[WindFrameRef], target: &[WindFrameRef]) -> usize {
    current
        .iter()
        .zip(target.iter())
        .take_while(|(left, right)| Rc::ptr_eq(left, right))
        .count()
}

fn parse_parameters(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut params = Vec::with_capacity(items.len());
    for item in items {
        match item {
            Expr::Symbol(name) => params.push(name.clone()),
            _ => {
                return Err(EvalError::InvalidForm(
                    "procedure parameters must be symbols".to_string(),
                ));
            }
        }
    }
    Ok(params)
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Int(value) => Value::Int(*value),
        Expr::Bool(value) => Value::Bool(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Char(value) => Value::Char(*value),
        Expr::Symbol(value) => Value::Symbol(value.clone()),
        Expr::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn require_exact_arity(got: usize, expected: usize, name: &str) -> Result<(), EvalError> {
    if got == expected {
        Ok(())
    } else {
        Err(EvalError::WrongArity {
            name: name.to_string(),
            expected: expected.to_string(),
            got,
        })
    }
}

fn require_at_least_arity(got: usize, minimum: usize, name: &str) -> Result<(), EvalError> {
    if got >= minimum {
        Ok(())
    } else {
        Err(EvalError::WrongArity {
            name: name.to_string(),
            expected: format!("at least {minimum}"),
            got,
        })
    }
}

fn expect_int(value: &Value, procedure: &str) -> Result<i64, EvalError> {
    match value {
        Value::Int(value) => Ok(*value),
        _ => Err(EvalError::Type(format!(
            "{procedure} expects an integer, got {}",
            value.type_name()
        ))),
    }
}

fn expect_string<'a>(value: &'a Value, procedure: &str) -> Result<&'a str, EvalError> {
    match value {
        Value::String(value) => Ok(value),
        _ => Err(EvalError::Type(format!(
            "{procedure} expects a string, got {}",
            value.type_name()
        ))),
    }
}

fn expect_char(value: &Value, procedure: &str) -> Result<char, EvalError> {
    match value {
        Value::Char(value) => Ok(*value),
        _ => Err(EvalError::Type(format!(
            "{procedure} expects a character, got {}",
            value.type_name()
        ))),
    }
}

fn expect_list<'a>(value: &'a Value, procedure: &str) -> Result<&'a [Value], EvalError> {
    match value {
        Value::List(items) => Ok(items),
        _ => Err(EvalError::Type(format!(
            "{procedure} expects a list, got {}",
            value.type_name()
        ))),
    }
}

fn expect_procedure_value(value: &Value, procedure: &str, role: &str) -> Result<(), EvalError> {
    match value {
        Value::Procedure(_) => Ok(()),
        _ => Err(EvalError::Type(format!(
            "{procedure} expects {role} to be a procedure, got {}",
            value.type_name()
        ))),
    }
}

fn compare_numbers(
    args: &[Value],
    name: &str,
    relation: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    require_at_least_arity(args.len(), 2, name)?;
    let mut previous = expect_int(&args[0], name)?;
    for current in &args[1..] {
        let current = expect_int(current, name)?;
        if !relation(previous, current) {
            return Ok(Value::Bool(false));
        }
        previous = current;
    }
    Ok(Value::Bool(true))
}

fn quote_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn char_literal(value: char) -> String {
    match value {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        ch => format!("#\\{ch}"),
    }
}

struct Parser {
    chars: Vec<char>,
    pos: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignorable();
        while self.pos < self.chars.len() {
            expressions.push(self.parse_expr()?);
            self.skip_ignorable();
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignorable();
        match self.peek() {
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string(),
            Some(')') => Err(EvalError::Parse("unexpected ')'".to_string())),
            Some('\'') => self.parse_quote_shorthand(),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::Parse("unexpected end of input".to_string())),
        }
    }

    fn parse_quote_shorthand(&mut self) -> Result<Expr, EvalError> {
        self.consume('\'')?;
        let quoted = self.parse_expr()?;
        Ok(Expr::List(vec![Expr::Symbol("quote".to_string()), quoted]))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.consume('(')?;
        let mut items = Vec::new();
        loop {
            self.skip_ignorable();
            match self.peek() {
                Some(')') => {
                    self.pos += 1;
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => {
                    return Err(EvalError::Parse(
                        "unexpected end of input while reading list".to_string(),
                    ));
                }
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.consume('"')?;
        let mut out = String::new();
        while let Some(ch) = self.next() {
            match ch {
                '"' => return Ok(Expr::String(out)),
                '\\' => {
                    let escaped = self.next().ok_or_else(|| {
                        EvalError::Parse("unterminated string literal".to_string())
                    })?;
                    out.push(match escaped {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                }
                other => out.push(other),
            }
        }

        Err(EvalError::Parse("unterminated string literal".to_string()))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let token = self.read_token();
        if token.is_empty() {
            return Err(EvalError::Parse("expected expression".to_string()));
        }

        if token == "#t" {
            return Ok(Expr::Bool(true));
        }
        if token == "#f" {
            return Ok(Expr::Bool(false));
        }
        if let Some(ch) = parse_char_token(&token)? {
            return Ok(Expr::Char(ch));
        }
        if let Ok(value) = token.parse::<i64>() {
            return Ok(Expr::Int(value));
        }
        Ok(Expr::Symbol(token))
    }

    fn skip_ignorable(&mut self) {
        loop {
            while matches!(self.peek(), Some(ch) if ch.is_whitespace()) {
                self.pos += 1;
            }

            if self.peek() == Some(';') {
                while let Some(ch) = self.next() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn read_token(&mut self) -> String {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';' | '\'') {
                break;
            }
            self.pos += 1;
        }
        self.chars[start..self.pos].iter().collect()
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn next(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += 1;
        Some(ch)
    }

    fn consume(&mut self, expected: char) -> Result<(), EvalError> {
        match self.next() {
            Some(ch) if ch == expected => Ok(()),
            Some(ch) => Err(EvalError::Parse(format!(
                "expected '{expected}', found '{ch}'"
            ))),
            None => Err(EvalError::Parse(format!(
                "expected '{expected}', found end of input"
            ))),
        }
    }
}

fn parse_char_token(token: &str) -> Result<Option<char>, EvalError> {
    if !token.starts_with("#\\") {
        return Ok(None);
    }

    let value = &token[2..];
    let ch = match value {
        "space" => ' ',
        "newline" => '\n',
        _ => {
            let mut chars = value.chars();
            let Some(first) = chars.next() else {
                return Err(EvalError::Parse("invalid character literal".to_string()));
            };
            if chars.next().is_some() {
                return Err(EvalError::Parse(format!(
                    "unsupported character literal: {token}"
                )));
            }
            first
        }
    };

    Ok(Some(ch))
}

#[cfg(test)]
mod tests;
