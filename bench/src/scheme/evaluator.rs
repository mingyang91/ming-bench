use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::ast::{BindingKey, Expr, ExprRef, Identifier};
use crate::scheme::env::{child_env, define_placeholder, define_value, lookup_cell, root_env, EnvRef};
use crate::scheme::error::EvalError;
use crate::scheme::forms::{parse_cond, parse_define, parse_lambda, parse_let, DefineForm};
use crate::scheme::macros::{expand, install_macro, is_define_syntax, CaptureStore, MacroBindings};
use crate::scheme::runtime::{
    bool_value, is_truthy, list_from_values, pair_value, quoted_value, value_type, values_from_list,
    BindingName, Builtin, CondClause, ContRef, Continuation, Lambda, LetKind, Parameters, Procedure,
    Value,
};

pub struct Evaluator {
    captures: CaptureStore,
}

enum Machine {
    Eval { expr: ExprRef, env: EnvRef, cont: ContRef },
    Return { value: Value, cont: ContRef },
    Invoke { procedure: Value, args: Vec<Value>, cont: ContRef },
    Done(Value),
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            captures: CaptureStore::new(),
        }
    }

    pub fn evaluate(&mut self, forms: Vec<ExprRef>) -> Result<Value, EvalError> {
        let env = initial_env();
        let mut machine = continue_program(
            Value::Void,
            forms,
            env,
            HashMap::new(),
            Rc::new(Continuation::Done),
            &mut self.captures,
        )?;
        loop {
            machine = match machine {
                Machine::Eval { expr, env, cont } => self.step_eval(expr, env, cont)?,
                Machine::Return { value, cont } => self.step_return(value, cont)?,
                Machine::Invoke {
                    procedure,
                    args,
                    cont,
                } => self.step_invoke(procedure, args, cont)?,
                Machine::Done(value) => return Ok(value),
            };
        }
    }

    fn step_eval(
        &mut self,
        expr: ExprRef,
        env: EnvRef,
        cont: ContRef,
    ) -> Result<Machine, EvalError> {
        match expr.as_ref() {
            Expr::Number(value) => Ok(return_machine(Value::Number(*value), cont)),
            Expr::Bool(value) => Ok(return_machine(Value::Bool(*value), cont)),
            Expr::String(value) => Ok(return_machine(Value::String(value.clone()), cont)),
            Expr::Symbol(identifier) => self.evaluate_symbol(identifier, env, cont),
            Expr::List(items) => self.evaluate_list(items, env, cont),
            Expr::DottedList(_, _) => invalid_syntax("expression", "improper list is not callable"),
        }
    }

    fn step_return(&mut self, value: Value, cont: ContRef) -> Result<Machine, EvalError> {
        continue_return(value, cont, &mut self.captures)
    }

    fn step_invoke(
        &mut self,
        procedure: Value,
        args: Vec<Value>,
        cont: ContRef,
    ) -> Result<Machine, EvalError> {
        match procedure {
            Value::Procedure(closure) => self.apply_procedure(closure, args, cont),
            other => Err(EvalError::NotCallable {
                actual: value_type(&other).to_owned(),
            }),
        }
    }

    fn evaluate_symbol(
        &self,
        identifier: &Identifier,
        env: EnvRef,
        cont: ContRef,
    ) -> Result<Machine, EvalError> {
        let value = lookup_value(identifier, &env, &self.captures)?;
        Ok(return_machine(value, cont))
    }

    fn evaluate_list(
        &mut self,
        items: &[ExprRef],
        env: EnvRef,
        cont: ContRef,
    ) -> Result<Machine, EvalError> {
        if items.is_empty() {
            return Ok(return_machine(Value::Nil, cont));
        }
        match items[0].as_ref().symbol_name() {
            Some("quote") => evaluate_quote(&items[1..], cont),
            Some("if") => evaluate_if(&items[1..], env, cont),
            Some("begin") => Ok(evaluate_sequence(items[1..].to_vec(), env, cont)),
            Some("define") => self.evaluate_define(&items[1..], env, cont),
            Some("lambda") => evaluate_lambda(&items[1..], env, cont),
            Some("set!") => self.evaluate_set(&items[1..], env, cont),
            Some("and") => Ok(evaluate_and(items[1..].to_vec(), env, cont)),
            Some("or") => Ok(evaluate_or(items[1..].to_vec(), env, cont)),
            Some("cond") => evaluate_cond_form(&items[1..], env, cont),
            Some("let") => evaluate_let_form(&items[1..], env, cont),
            Some("define-syntax") => {
                invalid_syntax("define-syntax", "only top-level define-syntax is supported")
            }
            _ => Ok(evaluate_application(items, env, cont)),
        }
    }

    fn evaluate_define(
        &self,
        args: &[ExprRef],
        env: EnvRef,
        cont: ContRef,
    ) -> Result<Machine, EvalError> {
        match parse_define(args)? {
            DefineForm::Variable { target, value } => {
                let cell = define_placeholder(&env, target.key);
                Ok(Machine::Eval {
                    expr: value,
                    env,
                    cont: Rc::new(Continuation::Define { cell, next: cont }),
                })
            }
            DefineForm::Function {
                target,
                params,
                body,
            } => Ok(define_function(target, params, body, env, cont)),
        }
    }

    fn evaluate_set(
        &self,
        args: &[ExprRef],
        env: EnvRef,
        cont: ContRef,
    ) -> Result<Machine, EvalError> {
        if args.len() != 2 {
            return invalid_syntax("set!", "expected a target and value");
        }
        let target = symbol_identifier(&args[0], "set!")?;
        let cell = assignment_cell(target, &env, &self.captures)?;
        Ok(Machine::Eval {
            expr: args[1].clone(),
            env,
            cont: Rc::new(Continuation::Set { cell, next: cont }),
        })
    }

    fn apply_procedure(
        &self,
        closure: Rc<Procedure>,
        args: Vec<Value>,
        cont: ContRef,
    ) -> Result<Machine, EvalError> {
        match closure.as_ref() {
            Procedure::Builtin(builtin) => apply_builtin(builtin, args, cont),
            Procedure::Lambda(lambda) => apply_lambda(lambda, args, cont),
            Procedure::Continuation(saved) => apply_continuation(saved.clone(), args),
        }
    }
}

fn continue_return(
    value: Value,
    cont: ContRef,
    captures: &mut CaptureStore,
) -> Result<Machine, EvalError> {
    match cont.as_ref() {
        Continuation::Done => Ok(Machine::Done(value)),
        Continuation::ProcedureReturn { next } => Ok(return_machine(value, next.clone())),
        Continuation::CallCcReturn { current, caller } => {
            Ok(continue_callcc_return(value, current.clone(), caller.clone()))
        }
        Continuation::Program {
            remaining,
            env,
            macros,
            next,
        } => continue_program(
            value,
            remaining.clone(),
            env.clone(),
            macros.clone(),
            next.clone(),
            captures,
        ),
        other => continue_runtime_return(value, other),
    }
}

fn continue_runtime_return(value: Value, cont: &Continuation) -> Result<Machine, EvalError> {
    match cont {
        Continuation::Sequence { remaining, env, next } => {
            Ok(continue_sequence(value, remaining.clone(), env.clone(), next.clone()))
        }
        Continuation::If {
            consequent,
            alternate,
            env,
            next,
        } => Ok(continue_if(
            value,
            consequent.clone(),
            alternate.clone(),
            env.clone(),
            next.clone(),
        )),
        Continuation::Define { cell, next } => Ok(store_value(cell, value, next.clone())),
        Continuation::Set { cell, next } => Ok(store_value(cell, value, next.clone())),
        Continuation::ApplicationOperator { .. } => Ok(continue_operator_frame(value, cont)),
        Continuation::ApplicationArgument { .. } => Ok(continue_argument_frame(value, cont)),
        Continuation::And { remaining, env, next } => {
            Ok(continue_and(value, remaining.clone(), env.clone(), next.clone()))
        }
        Continuation::Or { remaining, env, next } => {
            Ok(continue_or(value, remaining.clone(), env.clone(), next.clone()))
        }
        Continuation::Cond {
            body,
            remaining,
            env,
            next,
        } => Ok(continue_cond(
            value,
            body.clone(),
            remaining.clone(),
            env.clone(),
            next.clone(),
        )),
        Continuation::Let { .. } => continue_let_frame(value, cont),
        Continuation::Done
        | Continuation::Program { .. }
        | Continuation::ProcedureReturn { .. }
        | Continuation::CallCcReturn { .. } => {
            invalid_syntax("continuation", "unexpected terminal continuation frame")
        }
    }
}

fn continue_operator_frame(value: Value, cont: &Continuation) -> Machine {
    match cont {
        Continuation::ApplicationOperator { operands, env, next } => {
            continue_application(value, operands.clone(), env.clone(), next.clone())
        }
        _ => return_machine(value, Rc::new(Continuation::Done)),
    }
}

fn continue_argument_frame(value: Value, cont: &Continuation) -> Machine {
    match cont {
        Continuation::ApplicationArgument {
            operator,
            evaluated,
            remaining,
            env,
            next,
        } => continue_operand(
            operator.clone(),
            evaluated.clone(),
            value,
            remaining.clone(),
            env.clone(),
            next.clone(),
        ),
        _ => return_machine(value, Rc::new(Continuation::Done)),
    }
}

fn continue_let_frame(value: Value, cont: &Continuation) -> Result<Machine, EvalError> {
    match cont {
        Continuation::Let {
            kind,
            remaining_inits,
            evaluated,
            body,
            outer_env,
            next,
        } => finalize_let_step(
            value,
            kind.clone(),
            remaining_inits.clone(),
            evaluated.clone(),
            body.clone(),
            outer_env.clone(),
            next.clone(),
        ),
        _ => invalid_syntax("continuation", "unexpected let continuation frame"),
    }
}

fn continue_program(
    current: Value,
    forms: Vec<ExprRef>,
    env: EnvRef,
    macros: MacroBindings,
    next: ContRef,
    captures: &mut CaptureStore,
) -> Result<Machine, EvalError> {
    let mut remaining = forms;
    let mut active_macros = macros;
    let mut current_value = current;
    loop {
        let Some((form, rest)) = split_first(&remaining) else {
            return Ok(return_machine(current_value, next));
        };
        if is_define_syntax(&form) {
            active_macros = install_macro(form, env.clone(), &active_macros)?;
            current_value = Value::Void;
            remaining = rest;
            continue;
        }
        let expanded = expand(form, &active_macros, captures)?;
        return Ok(Machine::Eval {
            expr: expanded,
            env: env.clone(),
            cont: Rc::new(Continuation::Program {
                remaining: rest,
                env,
                macros: active_macros,
                next,
            }),
        });
    }
}

fn evaluate_quote(args: &[ExprRef], cont: ContRef) -> Result<Machine, EvalError> {
    if args.len() != 1 {
        return invalid_syntax("quote", "expected exactly one value");
    }
    Ok(return_machine(quoted_value(&args[0]), cont))
}

fn evaluate_if(args: &[ExprRef], env: EnvRef, cont: ContRef) -> Result<Machine, EvalError> {
    if args.len() != 3 {
        return invalid_syntax("if", "expected test, consequent, and alternate");
    }
    Ok(Machine::Eval {
        expr: args[0].clone(),
        env: env.clone(),
        cont: Rc::new(Continuation::If {
            consequent: args[1].clone(),
            alternate: args[2].clone(),
            env,
            next: cont,
        }),
    })
}

fn evaluate_lambda(args: &[ExprRef], env: EnvRef, cont: ContRef) -> Result<Machine, EvalError> {
    let (params, body) = parse_lambda(args)?;
    Ok(return_machine(lambda_value(params, body, env), cont))
}

fn evaluate_cond_form(
    args: &[ExprRef],
    env: EnvRef,
    cont: ContRef,
) -> Result<Machine, EvalError> {
    let clauses = parse_cond(args)?;
    Ok(evaluate_cond(clauses, env, cont))
}

fn evaluate_let_form(
    args: &[ExprRef],
    env: EnvRef,
    cont: ContRef,
) -> Result<Machine, EvalError> {
    let form = parse_let(args)?;
    start_let(form.kind, form.inits, form.body, env, cont)
}

fn continue_sequence(value: Value, remaining: Vec<ExprRef>, env: EnvRef, next: ContRef) -> Machine {
    if let Some((expr, rest)) = split_first(&remaining) {
        if rest.is_empty() {
            return eval_machine(expr, env, next);
        }
        return eval_machine(
            expr,
            env.clone(),
            Rc::new(Continuation::Sequence {
                remaining: rest,
                env,
                next,
            }),
        );
    }
    return_machine(value, next)
}

fn continue_if(
    value: Value,
    consequent: ExprRef,
    alternate: ExprRef,
    env: EnvRef,
    next: ContRef,
) -> Machine {
    let branch = if is_truthy(&value) {
        consequent
    } else {
        alternate
    };
    eval_machine(branch, env, next)
}

fn store_value(cell: &crate::scheme::env::CellRef, value: Value, next: ContRef) -> Machine {
    *cell.borrow_mut() = value;
    return_machine(Value::Void, next)
}

fn continue_application(
    operator: Value,
    operands: Vec<ExprRef>,
    env: EnvRef,
    next: ContRef,
) -> Machine {
    let Some((expr, rest)) = split_last(&operands) else {
        return invoke_machine(operator, Vec::new(), next);
    };
    eval_machine(
        expr,
        env.clone(),
        Rc::new(Continuation::ApplicationArgument {
            operator,
            evaluated: Vec::new(),
            remaining: rest,
            env,
            next,
        }),
    )
}

fn continue_operand(
    operator: Value,
    evaluated: Vec<Value>,
    value: Value,
    remaining: Vec<ExprRef>,
    env: EnvRef,
    next: ContRef,
) -> Machine {
    let mut args = evaluated;
    args.insert(0, value);
    let Some((expr, rest)) = split_last(&remaining) else {
        return invoke_machine(operator, args, next);
    };
    eval_machine(
        expr,
        env.clone(),
        Rc::new(Continuation::ApplicationArgument {
            operator,
            evaluated: args,
            remaining: rest,
            env,
            next,
        }),
    )
}

fn continue_and(value: Value, remaining: Vec<ExprRef>, env: EnvRef, next: ContRef) -> Machine {
    if !is_truthy(&value) {
        return return_machine(value, next);
    }
    continue_truthy_sequence(value, remaining, env, next, true)
}

fn continue_or(value: Value, remaining: Vec<ExprRef>, env: EnvRef, next: ContRef) -> Machine {
    if is_truthy(&value) {
        return return_machine(value, next);
    }
    continue_truthy_sequence(value, remaining, env, next, false)
}

fn continue_cond(
    value: Value,
    body: Vec<ExprRef>,
    remaining: Vec<CondClause>,
    env: EnvRef,
    next: ContRef,
) -> Machine {
    if is_truthy(&value) {
        return evaluate_sequence(body, env, next);
    }
    evaluate_cond(remaining, env, next)
}

fn continue_truthy_sequence(
    value: Value,
    remaining: Vec<ExprRef>,
    env: EnvRef,
    next: ContRef,
    is_and: bool,
) -> Machine {
    let Some((expr, rest)) = split_first(&remaining) else {
        return return_machine(value, next);
    };
    if rest.is_empty() {
        return eval_machine(expr, env, next);
    }
    let frame = truthy_continuation(is_and, rest, env.clone(), next);
    eval_machine(expr, env, frame)
}

fn finalize_let_step(
    value: Value,
    kind: LetKind,
    remaining_inits: Vec<ExprRef>,
    evaluated: Vec<Value>,
    body: Vec<ExprRef>,
    outer_env: EnvRef,
    next: ContRef,
) -> Result<Machine, EvalError> {
    let mut values = evaluated;
    values.push(value);
    let Some((expr, rest)) = split_first(&remaining_inits) else {
        return finalize_let(kind, values, body, outer_env, next);
    };
    Ok(Machine::Eval {
        expr,
        env: outer_env.clone(),
        cont: Rc::new(Continuation::Let {
            kind,
            remaining_inits: rest,
            evaluated: values,
            body,
            outer_env,
            next,
        }),
    })
}

fn evaluate_application(items: &[ExprRef], env: EnvRef, cont: ContRef) -> Machine {
    let operator = items[0].clone();
    let operands = items[1..].to_vec();
    eval_machine(
        operator,
        env.clone(),
        Rc::new(Continuation::ApplicationOperator {
            operands,
            env,
            next: cont,
        }),
    )
}

fn evaluate_sequence(exprs: Vec<ExprRef>, env: EnvRef, cont: ContRef) -> Machine {
    let Some((expr, rest)) = split_first(&exprs) else {
        return return_machine(Value::Void, cont);
    };
    if rest.is_empty() {
        return eval_machine(expr, env, cont);
    }
    eval_machine(
        expr,
        env.clone(),
        Rc::new(Continuation::Sequence {
            remaining: rest,
            env,
            next: cont,
        }),
    )
}

fn evaluate_and(exprs: Vec<ExprRef>, env: EnvRef, cont: ContRef) -> Machine {
    let Some((expr, rest)) = split_first(&exprs) else {
        return return_machine(bool_value(true), cont);
    };
    if rest.is_empty() {
        return eval_machine(expr, env, cont);
    }
    eval_machine(
        expr,
        env.clone(),
        Rc::new(Continuation::And {
            remaining: rest,
            env,
            next: cont,
        }),
    )
}

fn evaluate_or(exprs: Vec<ExprRef>, env: EnvRef, cont: ContRef) -> Machine {
    let Some((expr, rest)) = split_first(&exprs) else {
        return return_machine(bool_value(false), cont);
    };
    if rest.is_empty() {
        return eval_machine(expr, env, cont);
    }
    eval_machine(
        expr,
        env.clone(),
        Rc::new(Continuation::Or {
            remaining: rest,
            env,
            next: cont,
        }),
    )
}

fn evaluate_cond(clauses: Vec<CondClause>, env: EnvRef, cont: ContRef) -> Machine {
    let Some((clause, rest)) = split_first(&clauses) else {
        return return_machine(Value::Void, cont);
    };
    match clause.test {
        Some(test) => eval_machine(
            test,
            env.clone(),
            Rc::new(Continuation::Cond {
                body: clause.body,
                remaining: rest,
                env,
                next: cont,
            }),
        ),
        None => evaluate_sequence(clause.body, env, cont),
    }
}

fn start_let(
    kind: LetKind,
    inits: Vec<ExprRef>,
    body: Vec<ExprRef>,
    env: EnvRef,
    cont: ContRef,
) -> Result<Machine, EvalError> {
    let Some((expr, rest)) = split_first(&inits) else {
        return finalize_let(kind, Vec::new(), body, env, cont);
    };
    Ok(eval_machine(
        expr,
        env.clone(),
        Rc::new(Continuation::Let {
            kind,
            remaining_inits: rest,
            evaluated: Vec::new(),
            body,
            outer_env: env,
            next: cont,
        }),
    ))
}

fn finalize_let(
    kind: LetKind,
    values: Vec<Value>,
    body: Vec<ExprRef>,
    outer_env: EnvRef,
    next: ContRef,
) -> Result<Machine, EvalError> {
    match kind {
        LetKind::Plain(names) => {
            let call_env = child_env(outer_env);
            bind_names(&call_env, &names, &values);
            Ok(evaluate_sequence(body, call_env, next))
        }
        LetKind::Named { name, params } => {
            let recursive_env = child_env(outer_env);
            let cell = define_placeholder(&recursive_env, name.key);
            let lambda = lambda_value(Parameters::Fixed(params), body, recursive_env.clone());
            *cell.borrow_mut() = lambda.clone();
            Ok(invoke_machine(lambda, values, next))
        }
    }
}

fn apply_builtin(builtin: &Builtin, args: Vec<Value>, cont: ContRef) -> Result<Machine, EvalError> {
    match builtin {
        Builtin::Add => arithmetic_result(builtin, sum_numbers(&args)?, cont),
        Builtin::Sub => arithmetic_result(builtin, subtract_numbers(&args)?, cont),
        Builtin::Mul => arithmetic_result(builtin, multiply_numbers(&args)?, cont),
        Builtin::Div => arithmetic_result(builtin, divide_numbers(&args)?, cont),
        Builtin::LessThan => comparison_result(builtin, &args, |left, right| left < right, cont),
        Builtin::GreaterThan => comparison_result(builtin, &args, |left, right| left > right, cont),
        Builtin::NumberEq => comparison_result(builtin, &args, |left, right| left == right, cont),
        Builtin::LessEqual => comparison_result(builtin, &args, |left, right| left <= right, cont),
        Builtin::Not => unary_result(builtin, &args, |value| bool_value(!is_truthy(value)), cont),
        Builtin::Cons => cons_result(&args, cont),
        Builtin::Car => car_result(&args, cont),
        Builtin::Cdr => cdr_result(&args, cont),
        Builtin::NullPredicate => predicate_result(builtin, &args, |value| matches!(value, Value::Nil), cont),
        Builtin::List => Ok(return_machine(list_from_values(&args), cont)),
        Builtin::Length => length_result(&args, cont),
        Builtin::StringPredicate => predicate_result(builtin, &args, |value| matches!(value, Value::String(_)), cont),
        Builtin::NumberPredicate => predicate_result(builtin, &args, |value| matches!(value, Value::Number(_)), cont),
        Builtin::BooleanPredicate => predicate_result(builtin, &args, |value| matches!(value, Value::Bool(_)), cont),
        Builtin::PairPredicate => predicate_result(builtin, &args, |value| matches!(value, Value::Pair(_)), cont),
        Builtin::SymbolPredicate => predicate_result(builtin, &args, |value| matches!(value, Value::Symbol(_)), cont),
        Builtin::Apply => apply_result(args, cont),
        Builtin::CallCc => callcc_result(args, cont),
    }
}

fn apply_lambda(lambda: &Lambda, args: Vec<Value>, cont: ContRef) -> Result<Machine, EvalError> {
    let call_env = child_env(lambda.env.clone());
    bind_parameters(&call_env, &lambda.params, &args)?;
    Ok(evaluate_sequence(
        lambda.body.clone(),
        call_env,
        Rc::new(Continuation::ProcedureReturn { next: cont }),
    ))
}

fn apply_continuation(saved: ContRef, args: Vec<Value>) -> Result<Machine, EvalError> {
    ensure_exact("continuation", 1, args.len())?;
    Ok(return_machine(args[0].clone(), saved))
}

fn arithmetic_result(
    builtin: &Builtin,
    value: i64,
    cont: ContRef,
) -> Result<Machine, EvalError> {
    let _ = builtin;
    Ok(return_machine(Value::Number(value), cont))
}

fn comparison_result(
    builtin: &Builtin,
    args: &[Value],
    compare: fn(i64, i64) -> bool,
    cont: ContRef,
) -> Result<Machine, EvalError> {
    ensure_minimum(builtin.name(), 2, args.len())?;
    let numbers = numbers(args, builtin.name())?;
    let result = numbers.windows(2).all(|pair| compare(pair[0], pair[1]));
    Ok(return_machine(bool_value(result), cont))
}

fn unary_result(
    builtin: &Builtin,
    args: &[Value],
    action: fn(&Value) -> Value,
    cont: ContRef,
) -> Result<Machine, EvalError> {
    ensure_exact(builtin.name(), 1, args.len())?;
    Ok(return_machine(action(&args[0]), cont))
}

fn predicate_result(
    builtin: &Builtin,
    args: &[Value],
    predicate: fn(&Value) -> bool,
    cont: ContRef,
) -> Result<Machine, EvalError> {
    ensure_exact(builtin.name(), 1, args.len())?;
    Ok(return_machine(bool_value(predicate(&args[0])), cont))
}

fn cons_result(args: &[Value], cont: ContRef) -> Result<Machine, EvalError> {
    ensure_exact("cons", 2, args.len())?;
    Ok(return_machine(
        pair_value(args[0].clone(), args[1].clone()),
        cont,
    ))
}

fn car_result(args: &[Value], cont: ContRef) -> Result<Machine, EvalError> {
    ensure_exact("car", 1, args.len())?;
    let pair = pair_arg(&args[0], "car")?;
    Ok(return_machine(pair.car.clone(), cont))
}

fn cdr_result(args: &[Value], cont: ContRef) -> Result<Machine, EvalError> {
    ensure_exact("cdr", 1, args.len())?;
    let pair = pair_arg(&args[0], "cdr")?;
    Ok(return_machine(pair.cdr.clone(), cont))
}

fn length_result(args: &[Value], cont: ContRef) -> Result<Machine, EvalError> {
    ensure_exact("length", 1, args.len())?;
    let values = values_from_list(&args[0], "length")?;
    let length = i64::try_from(values.len()).map_err(|_| EvalError::ArithmeticOverflow {
        procedure: "length".to_owned(),
    })?;
    Ok(return_machine(Value::Number(length), cont))
}

fn apply_result(args: Vec<Value>, cont: ContRef) -> Result<Machine, EvalError> {
    ensure_minimum("apply", 2, args.len())?;
    let Some((procedure, rest)) = args.split_first() else {
        return invalid_syntax("apply", "expected a procedure");
    };
    let Some((list_arg, prefix)) = rest.split_last() else {
        return invalid_syntax("apply", "expected a final list argument");
    };
    let mut applied = prefix.to_vec();
    applied.extend(values_from_list(list_arg, "apply")?);
    Ok(invoke_machine(procedure.clone(), applied, cont))
}

fn callcc_result(args: Vec<Value>, cont: ContRef) -> Result<Machine, EvalError> {
    ensure_exact("call/cc", 1, args.len())?;
    let continuation = Value::Procedure(Rc::new(Procedure::Continuation(cont.clone())));
    Ok(invoke_machine(
        args[0].clone(),
        vec![continuation],
        Rc::new(Continuation::CallCcReturn {
            current: cont.clone(),
            caller: caller_continuation(&cont),
        }),
    ))
}

fn sum_numbers(args: &[Value]) -> Result<i64, EvalError> {
    numbers(args, "+")?
        .into_iter()
        .try_fold(0_i64, |total, value| checked(total.checked_add(value), "+"))
}

fn subtract_numbers(args: &[Value]) -> Result<i64, EvalError> {
    ensure_minimum("-", 1, args.len())?;
    let values = numbers(args, "-")?;
    let Some((first, rest)) = values.split_first() else {
        return invalid_syntax("-", "expected at least one number");
    };
    if rest.is_empty() {
        return checked(first.checked_neg(), "-");
    }
    rest.iter().try_fold(*first, |total, value| checked(total.checked_sub(*value), "-"))
}

fn multiply_numbers(args: &[Value]) -> Result<i64, EvalError> {
    numbers(args, "*")?
        .into_iter()
        .try_fold(1_i64, |total, value| checked(total.checked_mul(value), "*"))
}

fn divide_numbers(args: &[Value]) -> Result<i64, EvalError> {
    ensure_minimum("/", 2, args.len())?;
    let values = numbers(args, "/")?;
    let Some((first, rest)) = values.split_first() else {
        return invalid_syntax("/", "expected at least two numbers");
    };
    rest.iter()
        .try_fold(*first, |total, value| divide_step(total, *value))
}

fn numbers(args: &[Value], procedure: &str) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|value| number_arg(value, procedure)).collect()
}

fn divide_step(total: i64, value: i64) -> Result<i64, EvalError> {
    if value == 0 {
        return Err(EvalError::DivisionByZero {
            procedure: "/".to_owned(),
        });
    }
    checked(total.checked_div(value), "/")
}

fn number_arg(value: &Value, _procedure: &str) -> Result<i64, EvalError> {
    match value {
        Value::Number(number) => Ok(*number),
        other => Err(EvalError::TypeMismatch {
            expected: "number".to_owned(),
            actual: value_type(other).to_owned(),
        }),
    }
}

fn pair_arg<'a>(value: &'a Value, _procedure: &str) -> Result<&'a crate::scheme::runtime::Pair, EvalError> {
    match value {
        Value::Pair(pair) => Ok(pair.as_ref()),
        other => Err(EvalError::TypeMismatch {
            expected: "pair".to_owned(),
            actual: value_type(other).to_owned(),
        }),
    }
}

fn checked(value: Option<i64>, procedure: &str) -> Result<i64, EvalError> {
    value.ok_or_else(|| EvalError::ArithmeticOverflow {
        procedure: procedure.to_owned(),
    })
}

fn bind_parameters(env: &EnvRef, params: &Parameters, args: &[Value]) -> Result<(), EvalError> {
    match params {
        Parameters::Fixed(names) => {
            ensure_exact("lambda", names.len(), args.len())?;
            bind_names(env, names, args);
        }
        Parameters::Variadic { fixed, rest } => {
            ensure_minimum("lambda", fixed.len(), args.len())?;
            bind_names(env, fixed, &args[..fixed.len()]);
            define_value(env, rest.key.clone(), list_from_values(&args[fixed.len()..]));
        }
    }
    Ok(())
}

fn bind_names(env: &EnvRef, names: &[BindingName], values: &[Value]) {
    for (name, value) in names.iter().zip(values) {
        define_value(env, name.key.clone(), value.clone());
    }
}

fn ensure_exact(procedure: &str, expected: usize, actual: usize) -> Result<(), EvalError> {
    if actual == expected {
        return Ok(());
    }
    Err(EvalError::ArgumentCountExact {
        procedure: procedure.to_owned(),
        expected,
        actual,
    })
}

fn ensure_minimum(procedure: &str, minimum: usize, actual: usize) -> Result<(), EvalError> {
    if actual >= minimum {
        return Ok(());
    }
    Err(EvalError::ArgumentCountAtLeast {
        procedure: procedure.to_owned(),
        minimum,
        actual,
    })
}

fn define_function(
    target: BindingName,
    params: Parameters,
    body: Vec<ExprRef>,
    env: EnvRef,
    cont: ContRef,
) -> Machine {
    let cell = define_placeholder(&env, target.key);
    *cell.borrow_mut() = lambda_value(params, body, env);
    return_machine(Value::Void, cont)
}

fn lambda_value(params: Parameters, body: Vec<ExprRef>, env: EnvRef) -> Value {
    Value::Procedure(Rc::new(Procedure::Lambda(Lambda { params, body, env })))
}

fn lookup_value(identifier: &Identifier, env: &EnvRef, captures: &CaptureStore) -> Result<Value, EvalError> {
    if let Some(id) = identifier.captured_value_id() {
        let cell = captures.lookup_value(id).ok_or_else(|| EvalError::MissingCapture {
            kind: "value".to_owned(),
            id,
        })?;
        return Ok(cell.borrow().clone());
    }
    let Some(key) = identifier.binding_key() else {
        return Err(EvalError::UnboundVariable {
            name: identifier.name().to_owned(),
        });
    };
    lookup_cell(env, &key)
        .map(|cell| cell.borrow().clone())
        .ok_or_else(|| EvalError::UnboundVariable {
            name: identifier.name().to_owned(),
        })
}

fn assignment_cell(
    identifier: &Identifier,
    env: &EnvRef,
    captures: &CaptureStore,
) -> Result<crate::scheme::env::CellRef, EvalError> {
    if let Some(id) = identifier.captured_value_id() {
        return captures.lookup_value(id).ok_or_else(|| EvalError::MissingCapture {
            kind: "value".to_owned(),
            id,
        });
    }
    let Some(key) = identifier.binding_key() else {
        return Err(EvalError::InvalidBindingTarget {
            context: "set!".to_owned(),
        });
    };
    lookup_cell(env, &key).ok_or_else(|| EvalError::UnboundVariable {
        name: identifier.name().to_owned(),
    })
}

fn symbol_identifier<'a>(expr: &'a ExprRef, form: &str) -> Result<&'a Identifier, EvalError> {
    match expr.as_ref() {
        Expr::Symbol(identifier) => Ok(identifier),
        _ => Err(EvalError::InvalidBindingTarget {
            context: form.to_owned(),
        }),
    }
}

fn split_first<T: Clone>(items: &[T]) -> Option<(T, Vec<T>)> {
    let (first, rest) = items.split_first()?;
    Some((first.clone(), rest.to_vec()))
}

fn split_last<T: Clone>(items: &[T]) -> Option<(T, Vec<T>)> {
    let (last, rest) = items.split_last()?;
    Some((last.clone(), rest.to_vec()))
}

fn initial_env() -> EnvRef {
    let env = root_env();
    for (name, builtin) in builtin_entries() {
        define_value(&env, BindingKey::Plain(name.to_owned()), builtin_value(builtin));
    }
    env
}

fn builtin_entries() -> [(&'static str, Builtin); 22] {
    [
        ("+", Builtin::Add),
        ("-", Builtin::Sub),
        ("*", Builtin::Mul),
        ("/", Builtin::Div),
        ("<", Builtin::LessThan),
        (">", Builtin::GreaterThan),
        ("=", Builtin::NumberEq),
        ("<=", Builtin::LessEqual),
        ("not", Builtin::Not),
        ("cons", Builtin::Cons),
        ("car", Builtin::Car),
        ("cdr", Builtin::Cdr),
        ("null?", Builtin::NullPredicate),
        ("list", Builtin::List),
        ("length", Builtin::Length),
        ("string?", Builtin::StringPredicate),
        ("number?", Builtin::NumberPredicate),
        ("boolean?", Builtin::BooleanPredicate),
        ("pair?", Builtin::PairPredicate),
        ("symbol?", Builtin::SymbolPredicate),
        ("apply", Builtin::Apply),
        ("call/cc", Builtin::CallCc),
    ]
}

fn builtin_value(builtin: Builtin) -> Value {
    Value::Procedure(Rc::new(Procedure::Builtin(builtin)))
}

fn eval_machine(expr: ExprRef, env: EnvRef, cont: ContRef) -> Machine {
    Machine::Eval { expr, env, cont }
}

fn return_machine(value: Value, cont: ContRef) -> Machine {
    Machine::Return { value, cont }
}

fn invoke_machine(procedure: Value, args: Vec<Value>, cont: ContRef) -> Machine {
    Machine::Invoke {
        procedure,
        args,
        cont,
    }
}

fn truthy_continuation(
    is_and: bool,
    remaining: Vec<ExprRef>,
    env: EnvRef,
    next: ContRef,
) -> ContRef {
    if is_and {
        return Rc::new(Continuation::And {
            remaining,
            env,
            next,
        });
    }
    Rc::new(Continuation::Or {
        remaining,
        env,
        next,
    })
}

fn continue_callcc_return(value: Value, current: ContRef, caller: ContRef) -> Machine {
    if matches!(value, Value::Void) {
        return_machine(Value::Void, caller)
    } else {
        return_machine(value, current)
    }
}

fn caller_continuation(cont: &ContRef) -> ContRef {
    let mut cursor = cont.clone();
    loop {
        match cursor.as_ref() {
            Continuation::ProcedureReturn { next } => return next.clone(),
            Continuation::Done | Continuation::Program { .. } => return cursor,
            Continuation::CallCcReturn { caller, .. } => return caller.clone(),
            Continuation::Sequence { next, .. }
            | Continuation::If { next, .. }
            | Continuation::Define { next, .. }
            | Continuation::Set { next, .. }
            | Continuation::ApplicationOperator { next, .. }
            | Continuation::ApplicationArgument { next, .. }
            | Continuation::And { next, .. }
            | Continuation::Or { next, .. }
            | Continuation::Cond { next, .. }
            | Continuation::Let { next, .. } => cursor = next.clone(),
        }
    }
}

fn invalid_syntax<T>(form: &str, detail: &str) -> Result<T, EvalError> {
    Err(EvalError::InvalidSyntax {
        form: form.to_owned(),
        detail: detail.to_owned(),
    })
}
