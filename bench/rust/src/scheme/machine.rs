use super::*;
use std::mem;
use std::rc::Rc;

pub(super) fn evaluate_program(
    expressions: &[Expr],
    env: Rc<Environment>,
    state: &mut EvalState,
) -> EvalResult<Value> {
    let Some((first, remaining)) = expressions.split_first() else {
        return Ok(Value::Void);
    };

    let mut machine = EvalMachine {
        state,
        frames: Vec::new(),
        winds: Vec::new(),
        control: MachineControl::Expr(first.clone(), Rc::clone(&env)),
        current_invocation_id: None,
    };

    if !remaining.is_empty() {
        machine.frames.push(MachineFrame::Sequence {
            remaining: remaining.to_vec(),
            env,
        });
    }

    machine.run()
}

struct EvalMachine<'a> {
    state: &'a mut EvalState,
    frames: Vec<MachineFrame>,
    winds: Vec<DynamicWindFrame>,
    control: MachineControl,
    current_invocation_id: Option<usize>,
}

impl EvalMachine<'_> {
    fn run(&mut self) -> EvalResult<Value> {
        loop {
            let control = mem::replace(&mut self.control, MachineControl::Value(Value::Void));
            match control {
                MachineControl::Expr(expression, env) => self.step_expr(expression, env)?,
                MachineControl::Value(value) => {
                    if let Some(result) = self.step_value(value)? {
                        return Ok(result);
                    }
                }
                MachineControl::Apply(procedure, args, pos) => {
                    self.step_apply(procedure, args, pos)?
                }
            }
        }
    }

    fn step_expr(&mut self, expression: Expr, env: Rc<Environment>) -> EvalResult<()> {
        match expression {
            Expr::Number(value, _) => self.control = MachineControl::Value(Value::Number(value)),
            Expr::Boolean(value, _) => {
                self.control = MachineControl::Value(Value::Boolean(value));
            }
            Expr::String(value, _) => {
                self.control = MachineControl::Value(Value::String(make_string(
                    string_to_chars(&value),
                    false,
                )));
            }
            Expr::Char(value, _) => self.control = MachineControl::Value(Value::Char(value)),
            Expr::Symbol(name, pos) => {
                self.control = MachineControl::Value(env.lookup(&name, pos)?);
            }
            Expr::ResolvedSymbol(symbol) => {
                self.control = MachineControl::Value(evaluate_symbol(&symbol, env)?);
            }
            Expr::List(elements, pos) => self.step_list(elements, pos, env)?,
        }

        Ok(())
    }

    fn step_list(
        &mut self,
        elements: Vec<Expr>,
        pos: Position,
        env: Rc<Environment>,
    ) -> EvalResult<()> {
        if elements.is_empty() {
            return Err(error_at(pos, "cannot evaluate empty list"));
        }

        let operator_expr = elements[0].clone();
        let argument_exprs = elements[1..].to_vec();

        if let Some(name) = symbol_name(&operator_expr) {
            match name {
                "and" => return self.start_and(&argument_exprs, env),
                "or" => return self.start_or(&argument_exprs, env),
                "if" => return self.start_if(&argument_exprs, env, operator_expr.pos()),
                "define" => return self.start_define(&argument_exprs, env, operator_expr.pos()),
                "define-record-type" => {
                    let value =
                        evaluate_define_record_type(&argument_exprs, env, operator_expr.pos())?;
                    self.control = MachineControl::Value(value);
                    return Ok(());
                }
                "define-syntax" => {
                    let value = evaluate_define_syntax(&argument_exprs, env, operator_expr.pos())?;
                    self.control = MachineControl::Value(value);
                    return Ok(());
                }
                "quote" => {
                    let value = evaluate_quote(&argument_exprs, operator_expr.pos())?;
                    self.control = MachineControl::Value(value);
                    return Ok(());
                }
                "lambda" => {
                    let value = evaluate_lambda(&argument_exprs, env, operator_expr.pos())?;
                    self.control = MachineControl::Value(value);
                    return Ok(());
                }
                "begin" => {
                    self.schedule_sequence(argument_exprs, env);
                    return Ok(());
                }
                "cond" => return self.start_cond(argument_exprs, env),
                "let" => return self.start_let(&argument_exprs, env, operator_expr.pos()),
                "set!" => return self.start_set(&argument_exprs, env, operator_expr.pos()),
                _ => {}
            }

            if let Some(transformer) = lookup_syntax(&operator_expr, &env) {
                let expanded = expand_macro(&transformer, &Expr::List(elements, pos), self.state)?;
                self.control = MachineControl::Expr(expanded, env);
                return Ok(());
            }
        }

        self.frames.push(MachineFrame::ApplyOperator {
            arguments: argument_exprs,
            env: Rc::clone(&env),
            pos: operator_expr.pos(),
        });
        self.control = MachineControl::Expr(operator_expr, env);
        Ok(())
    }

    fn step_value(&mut self, value: Value) -> EvalResult<Option<Value>> {
        let Some(frame) = self.frames.pop() else {
            return Ok(Some(value));
        };

        match frame {
            MachineFrame::CallBoundary {
                previous_invocation_id,
            } => {
                self.current_invocation_id = previous_invocation_id;
                self.control = MachineControl::Value(value);
            }
            MachineFrame::Sequence { remaining, env } => {
                if remaining.is_empty() {
                    self.control = MachineControl::Value(value);
                } else {
                    if remaining.len() > 1 {
                        self.frames.push(MachineFrame::Sequence {
                            remaining: remaining[1..].to_vec(),
                            env: Rc::clone(&env),
                        });
                    }
                    self.control = MachineControl::Expr(remaining[0].clone(), env);
                }
            }
            MachineFrame::And { remaining, env } => {
                if is_false(&value) || remaining.is_empty() {
                    self.control = MachineControl::Value(value);
                } else {
                    if remaining.len() > 1 {
                        self.frames.push(MachineFrame::And {
                            remaining: remaining[1..].to_vec(),
                            env: Rc::clone(&env),
                        });
                    }
                    self.control = MachineControl::Expr(remaining[0].clone(), env);
                }
            }
            MachineFrame::Or { remaining, env } => {
                if !is_false(&value) || remaining.is_empty() {
                    self.control = MachineControl::Value(value);
                } else {
                    if remaining.len() > 1 {
                        self.frames.push(MachineFrame::Or {
                            remaining: remaining[1..].to_vec(),
                            env: Rc::clone(&env),
                        });
                    }
                    self.control = MachineControl::Expr(remaining[0].clone(), env);
                }
            }
            MachineFrame::If {
                consequent,
                alternate,
                env,
            } => {
                self.control = if is_false(&value) {
                    MachineControl::Expr(alternate, env)
                } else {
                    MachineControl::Expr(consequent, env)
                };
            }
            MachineFrame::DefineValue { name, env } => {
                env.define(name, value);
                self.control = MachineControl::Value(Value::Void);
            }
            MachineFrame::SetValue { target, env } => {
                assign_symbol(&target, value, env, "set!")?;
                self.control = MachineControl::Value(Value::Void);
            }
            MachineFrame::ApplyOperator {
                arguments,
                env,
                pos,
            } => {
                if arguments.is_empty() {
                    self.control = MachineControl::Apply(value, Vec::new(), pos);
                } else {
                    let next = arguments[0].clone();
                    self.frames.push(MachineFrame::ApplyArgument {
                        operator: value,
                        evaluated: Vec::new(),
                        remaining: arguments[1..].to_vec(),
                        env: Rc::clone(&env),
                        pos,
                    });
                    self.control = MachineControl::Expr(next, env);
                }
            }
            MachineFrame::ApplyArgument {
                operator,
                mut evaluated,
                remaining,
                env,
                pos,
            } => {
                evaluated.push(value);
                if remaining.is_empty() {
                    self.control = MachineControl::Apply(operator, evaluated, pos);
                } else {
                    let next = remaining[0].clone();
                    self.frames.push(MachineFrame::ApplyArgument {
                        operator,
                        evaluated,
                        remaining: remaining[1..].to_vec(),
                        env: Rc::clone(&env),
                        pos,
                    });
                    self.control = MachineControl::Expr(next, env);
                }
            }
            MachineFrame::CondTest {
                body,
                remaining_clauses,
                env,
            } => {
                if is_false(&value) {
                    self.start_cond(remaining_clauses, env)?;
                } else if body.is_empty() {
                    self.control = MachineControl::Value(value);
                } else {
                    self.schedule_sequence(body, env);
                }
            }
            MachineFrame::LetBinding {
                bindings,
                next_index,
                mut values,
                body,
                env,
                named,
            } => {
                values.push(value);
                if next_index < bindings.len() {
                    let next = bindings[next_index].value_expr.clone();
                    self.frames.push(MachineFrame::LetBinding {
                        bindings,
                        next_index: next_index + 1,
                        values,
                        body,
                        env: Rc::clone(&env),
                        named,
                    });
                    self.control = MachineControl::Expr(next, env);
                } else {
                    self.finish_let(bindings, values, body, env, named);
                }
            }
            MachineFrame::MapCall {
                procedure,
                lists,
                index,
                mut results,
                pos,
            } => {
                results.push(value);
                if index >= lists[0].len() {
                    self.control = MachineControl::Value(list_to_pairs(results));
                } else {
                    let call_args = lists
                        .iter()
                        .map(|list| list[index].clone())
                        .collect::<Vec<_>>();
                    self.frames.push(MachineFrame::MapCall {
                        procedure: procedure.clone(),
                        lists,
                        index: index + 1,
                        results,
                        pos,
                    });
                    self.control = MachineControl::Apply(procedure, call_args, pos);
                }
            }
            MachineFrame::DynamicWindAfterIn {
                wind,
                body_thunk,
                pos,
            } => {
                self.winds.push(wind.clone());
                self.frames
                    .push(MachineFrame::DynamicWindAfterBody { wind, pos });
                self.control = MachineControl::Apply(body_thunk, Vec::new(), pos);
            }
            MachineFrame::DynamicWindAfterBody { wind, pos } => {
                self.deactivate_wind(wind.id);
                self.frames
                    .push(MachineFrame::DynamicWindAfterOut { body_value: value });
                self.control = MachineControl::Apply(wind.out_thunk, Vec::new(), pos);
            }
            MachineFrame::DynamicWindAfterOut { body_value } => {
                self.control = MachineControl::Value(body_value);
            }
            MachineFrame::ContinuationTransferOut {
                target,
                value,
                common_prefix,
                pos,
            } => {
                self.continue_continuation_transfer_out(target, value, common_prefix, pos);
            }
            MachineFrame::ContinuationTransferIn {
                target,
                value,
                next_index,
                pos,
            } => {
                self.winds.push(target.winds[next_index].clone());
                self.continue_continuation_transfer_in(target, value, next_index + 1, pos);
            }
        }

        Ok(None)
    }

    fn step_apply(&mut self, procedure: Value, args: Vec<Value>, pos: Position) -> EvalResult<()> {
        match procedure {
            Value::Builtin(Builtin::Apply) => {
                expect_at_least_arity("apply", &args, 2, pos)?;
                let procedure = args[0].clone();
                let mut flattened = args[1..args.len() - 1].to_vec();
                flattened.extend(list_to_vec(&args[args.len() - 1], "apply", pos)?);
                self.control = MachineControl::Apply(procedure, flattened, pos);
            }
            Value::Builtin(Builtin::Map) => self.start_map(&args, pos)?,
            Value::Builtin(Builtin::CallCc) => {
                expect_arity("call/cc", &args, 1, pos)?;
                let captured = Rc::new(CapturedContinuation {
                    frames: self.frames.clone(),
                    winds: self.winds.clone(),
                    name: Some("continuation".into()),
                    invocation_id: self.current_invocation_id,
                });
                if let Some(invocation_id) = captured.invocation_id {
                    self.state
                        .latest_continuations
                        .insert(invocation_id, Rc::clone(&captured));
                }
                let continuation = Value::Continuation(captured);
                self.control = MachineControl::Apply(args[0].clone(), vec![continuation], pos);
            }
            Value::Builtin(Builtin::CallWithCurrentContinuation) => {
                expect_arity("call-with-current-continuation", &args, 1, pos)?;
                let captured = Rc::new(CapturedContinuation {
                    frames: self.frames.clone(),
                    winds: self.winds.clone(),
                    name: Some("continuation".into()),
                    invocation_id: self.current_invocation_id,
                });
                if let Some(invocation_id) = captured.invocation_id {
                    self.state
                        .latest_continuations
                        .insert(invocation_id, Rc::clone(&captured));
                }
                let continuation = Value::Continuation(captured);
                self.control = MachineControl::Apply(args[0].clone(), vec![continuation], pos);
            }
            Value::Builtin(Builtin::DynamicWind) => {
                expect_arity("dynamic-wind", &args, 3, pos)?;
                let wind = DynamicWindFrame {
                    id: self.state.next_wind_id,
                    in_thunk: args[0].clone(),
                    out_thunk: args[2].clone(),
                };
                self.state.next_wind_id += 1;
                self.frames.push(MachineFrame::DynamicWindAfterIn {
                    wind,
                    body_thunk: args[1].clone(),
                    pos,
                });
                self.control = MachineControl::Apply(args[0].clone(), Vec::new(), pos);
            }
            Value::Builtin(builtin) => {
                let value = apply_builtin(builtin, &args, pos, self.state)?;
                self.control = MachineControl::Value(value);
            }
            Value::Closure(closure) => {
                if closure.rest_param.is_none() && args.len() != closure.params.len() {
                    return Err(error_at(
                        pos,
                        format!(
                            "{} expected {} argument(s), got {}",
                            closure.name.as_deref().unwrap_or("lambda"),
                            closure.params.len(),
                            args.len()
                        ),
                    ));
                }

                if closure.rest_param.is_some() && args.len() < closure.params.len() {
                    return Err(error_at(
                        pos,
                        format!(
                            "{} expected at least {} argument(s), got {}",
                            closure.name.as_deref().unwrap_or("lambda"),
                            closure.params.len(),
                            args.len()
                        ),
                    ));
                }

                let call_env = Rc::new(Environment::new(Some(Rc::clone(&closure.env))));

                for (param, arg) in closure.params.iter().zip(args.iter()) {
                    call_env.define(param.clone(), arg.clone());
                }

                if let Some(rest_param) = &closure.rest_param {
                    call_env.define(
                        rest_param.clone(),
                        list_to_pairs(args[closure.params.len()..].to_vec()),
                    );
                }

                self.state.next_invocation_id += 1;
                let invocation_id = self.state.next_invocation_id;
                self.frames.push(MachineFrame::CallBoundary {
                    previous_invocation_id: self.current_invocation_id,
                });
                self.current_invocation_id = Some(invocation_id);
                self.schedule_sequence(closure.body.clone(), call_env);
            }
            Value::Continuation(continuation) => {
                let name = continuation.name.as_deref().unwrap_or("continuation");
                expect_arity(name, &args, 1, pos)?;
                self.start_continuation_transfer(continuation, args[0].clone(), pos);
            }
            Value::RecordConstructor(record_type, name, field_indices) => {
                if args.len() != field_indices.len() {
                    return Err(error_at(
                        pos,
                        format!(
                            "{name} expected {} argument(s), got {}",
                            field_indices.len(),
                            args.len()
                        ),
                    ));
                }

                let mut fields = vec![Value::Void; record_type.field_names.len()];
                for (arg, field_index) in args.into_iter().zip(field_indices.into_iter()) {
                    fields[field_index] = arg;
                }

                self.control = MachineControl::Value(Value::Record(Rc::new(Record {
                    record_type,
                    fields,
                })));
            }
            Value::RecordPredicate(record_type, name) => {
                expect_arity(&name, &args, 1, pos)?;
                self.control = MachineControl::Value(Value::Boolean(matches_record_type(
                    &args[0],
                    &record_type,
                )));
            }
            Value::RecordAccessor(record_type, field_index, name) => {
                expect_arity(&name, &args, 1, pos)?;
                let record = expect_record_type(&args[0], &record_type, &name, pos)?;
                self.control = MachineControl::Value(record.fields[field_index].clone());
            }
            _ => return Err(error_at(pos, "attempted to call a non-procedure value")),
        }

        Ok(())
    }

    fn start_and(&mut self, expressions: &[Expr], env: Rc<Environment>) -> EvalResult<()> {
        if expressions.is_empty() {
            self.control = MachineControl::Value(Value::Boolean(true));
            return Ok(());
        }

        self.frames.push(MachineFrame::And {
            remaining: expressions[1..].to_vec(),
            env: Rc::clone(&env),
        });
        self.control = MachineControl::Expr(expressions[0].clone(), env);
        Ok(())
    }

    fn start_or(&mut self, expressions: &[Expr], env: Rc<Environment>) -> EvalResult<()> {
        if expressions.is_empty() {
            self.control = MachineControl::Value(Value::Boolean(false));
            return Ok(());
        }

        self.frames.push(MachineFrame::Or {
            remaining: expressions[1..].to_vec(),
            env: Rc::clone(&env),
        });
        self.control = MachineControl::Expr(expressions[0].clone(), env);
        Ok(())
    }

    fn start_if(
        &mut self,
        expressions: &[Expr],
        env: Rc<Environment>,
        pos: Position,
    ) -> EvalResult<()> {
        if expressions.len() != 2 && expressions.len() != 3 {
            return Err(error_at(
                pos,
                format!("if expected 2 or 3 argument(s), got {}", expressions.len()),
            ));
        }

        let alternate = expressions
            .get(2)
            .cloned()
            .unwrap_or_else(|| Expr::List(vec![Expr::Symbol("begin".into(), pos)], pos));

        self.frames.push(MachineFrame::If {
            consequent: expressions[1].clone(),
            alternate,
            env: Rc::clone(&env),
        });
        self.control = MachineControl::Expr(expressions[0].clone(), env);
        Ok(())
    }

    fn start_define(
        &mut self,
        expressions: &[Expr],
        env: Rc<Environment>,
        pos: Position,
    ) -> EvalResult<()> {
        if expressions.len() < 2 {
            return Err(error_at(
                pos,
                format!(
                    "define expected at least 2 argument(s), got {}",
                    expressions.len()
                ),
            ));
        }

        let target_expr = &expressions[0];
        let value_exprs = &expressions[1..];

        if matches!(target_expr, Expr::Symbol(_, _) | Expr::ResolvedSymbol(_)) {
            if value_exprs.len() != 1 {
                return Err(error_at(
                    pos,
                    format!(
                        "define expected 1 value expression, got {}",
                        value_exprs.len()
                    ),
                ));
            }

            self.frames.push(MachineFrame::DefineValue {
                name: binding_name(target_expr, "define")?,
                env: Rc::clone(&env),
            });
            self.control = MachineControl::Expr(value_exprs[0].clone(), env);
            return Ok(());
        }

        let signature = match target_expr {
            Expr::List(elements, _) if !elements.is_empty() => elements,
            _ => {
                return Err(error_at(
                    target_expr.pos(),
                    "define expected a symbol or function signature",
                ))
            }
        };

        let name = expect_symbol_expr(&signature[0], "define")?;
        let binding_key = binding_name(&signature[0], "define")?;
        let formals = parse_formal_parameter_list(&signature[1..], "define")?;

        if value_exprs.is_empty() {
            return Err(error_at(
                pos,
                "define expected at least one function body expression",
            ));
        }

        let closure = Value::Closure(Rc::new(Closure {
            params: formals.params,
            rest_param: formals.rest_param,
            body: value_exprs.to_vec(),
            env: Rc::clone(&env),
            name: Some(name),
        }));

        env.define(binding_key, closure);
        self.control = MachineControl::Value(Value::Void);
        Ok(())
    }

    fn start_set(
        &mut self,
        expressions: &[Expr],
        env: Rc<Environment>,
        pos: Position,
    ) -> EvalResult<()> {
        if expressions.len() != 2 {
            return Err(error_at(
                pos,
                format!("set! expected 2 argument(s), got {}", expressions.len()),
            ));
        }

        self.frames.push(MachineFrame::SetValue {
            target: expressions[0].clone(),
            env: Rc::clone(&env),
        });
        self.control = MachineControl::Expr(expressions[1].clone(), env);
        Ok(())
    }

    fn start_cond(&mut self, clauses: Vec<Expr>, env: Rc<Environment>) -> EvalResult<()> {
        let Some(first_clause) = clauses.first() else {
            self.control = MachineControl::Value(Value::Void);
            return Ok(());
        };

        let elements = match first_clause {
            Expr::List(elements, _) if !elements.is_empty() => elements.clone(),
            _ => return Err(error_at(first_clause.pos(), "cond expected a non-empty clause")),
        };

        let test_expr = elements[0].clone();
        let body = elements[1..].to_vec();
        let remaining_clauses = clauses[1..].to_vec();
        let is_else = matches!(symbol_name(&test_expr), Some("else"));

        if is_else {
            if !remaining_clauses.is_empty() {
                return Err(error_at(first_clause.pos(), "cond else clause must be last"));
            }

            self.schedule_sequence(body, env);
            return Ok(());
        }

        self.frames.push(MachineFrame::CondTest {
            body,
            remaining_clauses,
            env: Rc::clone(&env),
        });
        self.control = MachineControl::Expr(test_expr, env);
        Ok(())
    }

    fn start_let(
        &mut self,
        expressions: &[Expr],
        env: Rc<Environment>,
        pos: Position,
    ) -> EvalResult<()> {
        if expressions.len() < 2 {
            return Err(error_at(
                pos,
                format!(
                    "let expected at least 2 argument(s), got {}",
                    expressions.len()
                ),
            ));
        }

        let (bindings_expr, body_exprs, named) = if matches!(
            &expressions[0],
            Expr::Symbol(_, _) | Expr::ResolvedSymbol(_)
        ) {
            if expressions.len() < 3 {
                return Err(error_at(pos, "let expected bindings and a body"));
            }

            (
                &expressions[1],
                expressions[2..].to_vec(),
                Some(NamedLetState {
                    loop_name: expect_symbol_expr(&expressions[0], "let")?,
                    loop_key: binding_name(&expressions[0], "let")?,
                    pos: expressions[0].pos(),
                }),
            )
        } else {
            (&expressions[0], expressions[1..].to_vec(), None)
        };

        let bindings = parse_bindings(bindings_expr, "let")?;

        if bindings.is_empty() {
            self.finish_let(bindings, Vec::new(), body_exprs, env, named);
            return Ok(());
        }

        let first = bindings[0].value_expr.clone();
        self.frames.push(MachineFrame::LetBinding {
            bindings,
            next_index: 1,
            values: Vec::new(),
            body: body_exprs,
            env: Rc::clone(&env),
            named,
        });
        self.control = MachineControl::Expr(first, env);
        Ok(())
    }

    fn finish_let(
        &mut self,
        bindings: Vec<Binding>,
        values: Vec<Value>,
        body: Vec<Expr>,
        env: Rc<Environment>,
        named: Option<NamedLetState>,
    ) {
        if let Some(named) = named {
            let let_env = Rc::new(Environment::new(Some(Rc::clone(&env))));
            let closure = Value::Closure(Rc::new(Closure {
                params: bindings
                    .iter()
                    .map(|binding| binding.name.clone())
                    .collect(),
                rest_param: None,
                body,
                env: Rc::clone(&let_env),
                name: Some(named.loop_name),
            }));

            let_env.define(named.loop_key, closure.clone());
            self.control = MachineControl::Apply(closure, values, named.pos);
            return;
        }

        let let_env = Rc::new(Environment::new(Some(env)));
        for (binding, value) in bindings.into_iter().zip(values.into_iter()) {
            let_env.define(binding.name, value);
        }
        self.schedule_sequence(body, let_env);
    }

    fn start_map(&mut self, args: &[Value], pos: Position) -> EvalResult<()> {
        expect_at_least_arity("map", args, 2, pos)?;

        let procedure = args[0].clone();
        let lists = args[1..]
            .iter()
            .map(|arg| list_to_vec(arg, "map", pos))
            .collect::<EvalResult<Vec<_>>>()?;

        let expected_len = lists[0].len();
        if lists
            .iter()
            .skip(1)
            .any(|list| list.len() != expected_len)
        {
            return Err(error_at(pos, "map expected lists of equal length"));
        }

        if expected_len == 0 {
            self.control = MachineControl::Value(Value::EmptyList);
            return Ok(());
        }

        let call_args = lists
            .iter()
            .map(|list| list[0].clone())
            .collect::<Vec<_>>();
        self.frames.push(MachineFrame::MapCall {
            procedure: procedure.clone(),
            lists,
            index: 1,
            results: Vec::new(),
            pos,
        });
        self.control = MachineControl::Apply(procedure, call_args, pos);
        Ok(())
    }

    fn start_continuation_transfer(
        &mut self,
        target: Rc<CapturedContinuation>,
        value: Value,
        pos: Position,
    ) {
        let target = self.resolve_continuation_target(target);
        let common_prefix = common_wind_prefix(&self.winds, &target.winds);
        self.continue_continuation_transfer_out(target, value, common_prefix, pos);
    }

    fn continue_continuation_transfer_out(
        &mut self,
        target: Rc<CapturedContinuation>,
        value: Value,
        common_prefix: usize,
        pos: Position,
    ) {
        if self.winds.len() > common_prefix {
            let wind = self
                .winds
                .pop()
                .expect("wind stack length checked before pop");
            self.frames.push(MachineFrame::ContinuationTransferOut {
                target,
                value,
                common_prefix,
                pos,
            });
            self.control = MachineControl::Apply(wind.out_thunk, Vec::new(), pos);
            return;
        }

        self.continue_continuation_transfer_in(target, value, common_prefix, pos);
    }

    fn continue_continuation_transfer_in(
        &mut self,
        target: Rc<CapturedContinuation>,
        value: Value,
        next_index: usize,
        pos: Position,
    ) {
        if next_index < target.winds.len() {
            let wind = target.winds[next_index].clone();
            self.frames.push(MachineFrame::ContinuationTransferIn {
                target,
                value,
                next_index,
                pos,
            });
            self.control = MachineControl::Apply(wind.in_thunk, Vec::new(), pos);
            return;
        }

        self.frames = target.frames.clone();
        self.winds = target.winds.clone();
        self.current_invocation_id = target.invocation_id;
        self.control = MachineControl::Value(value);
    }

    fn schedule_sequence(&mut self, expressions: Vec<Expr>, env: Rc<Environment>) {
        if expressions.is_empty() {
            self.control = MachineControl::Value(Value::Void);
            return;
        }

        if expressions.len() > 1 {
            self.frames.push(MachineFrame::Sequence {
                remaining: expressions[1..].to_vec(),
                env: Rc::clone(&env),
            });
        }
        self.control = MachineControl::Expr(expressions[0].clone(), env);
    }

    fn deactivate_wind(&mut self, wind_id: usize) {
        if let Some(index) = self.winds.iter().rposition(|wind| wind.id == wind_id) {
            self.winds.remove(index);
        }
    }

    fn resolve_continuation_target(
        &self,
        target: Rc<CapturedContinuation>,
    ) -> Rc<CapturedContinuation> {
        target
            .invocation_id
            .and_then(|invocation_id| {
                self.state
                    .latest_continuations
                    .get(&invocation_id)
                    .cloned()
            })
            .unwrap_or(target)
    }
}

fn common_wind_prefix(left: &[DynamicWindFrame], right: &[DynamicWindFrame]) -> usize {
    left.iter()
        .zip(right.iter())
        .take_while(|(left, right)| left.id == right.id)
        .count()
}
