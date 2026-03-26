use super::EvalError;

use std::cell::{Cell, RefCell};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::panic::{self, AssertUnwindSafe};
use std::rc::Rc;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut interpreter = Interpreter::new(false);
    let result = interpreter.eval_program(input)?;
    Ok(interpreter.render_top_level(result)?)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut interpreter = Interpreter::new(true);
    let result = interpreter.eval_program(input)?;
    Ok((interpreter.render_top_level(result)?, interpreter.take_output()))
}

struct Interpreter {
    global_env: Rc<Environment>,
    output: Option<String>,
    eval_depth: usize,
    guard_stack: Vec<ActiveGuard>,
    synthetic_counter: u64,
}

impl Interpreter {
    fn new(capture_output: bool) -> Self {
        let global_env = Self::create_global_env();
        Self {
            global_env,
            output: capture_output.then(String::new),
            eval_depth: 0,
            guard_stack: Vec::new(),
            synthetic_counter: 0,
        }
    }

    fn create_global_env() -> Rc<Environment> {
        let env = Environment::new(None);

        for (name, action) in [
            ("+", Self::builtin_add as BuiltinAction),
            ("-", Self::builtin_subtract),
            ("*", Self::builtin_multiply),
            ("/", Self::builtin_divide),
            ("=", Self::builtin_num_eq),
            ("<", Self::builtin_less),
            (">", Self::builtin_greater),
            ("<=", Self::builtin_less_equal),
            (">=", Self::builtin_greater_equal),
            ("abs", Self::builtin_abs),
            ("modulo", Self::builtin_modulo),
            ("remainder", Self::builtin_remainder),
            ("quotient", Self::builtin_quotient),
            ("min", Self::builtin_min),
            ("max", Self::builtin_max),
            ("expt", Self::builtin_expt),
            ("not", Self::builtin_not),
            ("zero?", Self::builtin_zero_predicate),
            ("positive?", Self::builtin_positive_predicate),
            ("negative?", Self::builtin_negative_predicate),
            ("odd?", Self::builtin_odd_predicate),
            ("even?", Self::builtin_even_predicate),
            ("cons", Self::builtin_cons),
            ("car", Self::builtin_car),
            ("cdr", Self::builtin_cdr),
            ("caar", Self::builtin_caar),
            ("cadr", Self::builtin_cadr),
            ("cdar", Self::builtin_cdar),
            ("cddr", Self::builtin_cddr),
            ("set-car!", Self::builtin_set_car),
            ("set-cdr!", Self::builtin_set_cdr),
            ("null?", Self::builtin_null_predicate),
            ("pair?", Self::builtin_pair_predicate),
            ("list?", Self::builtin_list_predicate),
            ("list", Self::builtin_list),
            ("length", Self::builtin_length),
            ("append", Self::builtin_append),
            ("reverse", Self::builtin_reverse),
            ("list-ref", Self::builtin_list_ref),
            ("list-tail", Self::builtin_list_tail),
            ("apply", Self::builtin_apply),
            ("map", Self::builtin_map),
            ("for-each", Self::builtin_for_each),
            ("eq?", Self::builtin_eq),
            ("eqv?", Self::builtin_eqv),
            ("equal?", Self::builtin_equal),
            ("assoc", Self::builtin_assoc),
            ("assq", Self::builtin_assq),
            ("assv", Self::builtin_assv),
            ("member", Self::builtin_member),
            ("memq", Self::builtin_memq),
            ("memv", Self::builtin_memv),
            ("string?", Self::builtin_string_predicate),
            ("symbol?", Self::builtin_symbol_predicate),
            ("number?", Self::builtin_number_predicate),
            ("integer?", Self::builtin_integer_predicate),
            ("boolean?", Self::builtin_boolean_predicate),
            ("procedure?", Self::builtin_procedure_predicate),
            ("char?", Self::builtin_char_predicate),
            ("string-append", Self::builtin_string_append),
            ("string-length", Self::builtin_string_length),
            ("substring", Self::builtin_substring),
            ("string->number", Self::builtin_string_to_number),
            ("number->string", Self::builtin_number_to_string),
            ("symbol->string", Self::builtin_symbol_to_string),
            ("string->symbol", Self::builtin_string_to_symbol),
            ("string-ref", Self::builtin_string_ref),
            ("string-copy", Self::builtin_string_copy),
            ("string-set!", Self::builtin_string_set),
            ("make-string", Self::builtin_make_string),
            ("string", Self::builtin_string),
            ("string<?", Self::builtin_string_less),
            ("string<=?", Self::builtin_string_less_equal),
            ("string=?", Self::builtin_string_equal),
            ("string>=?", Self::builtin_string_greater_equal),
            ("string>?", Self::builtin_string_greater),
            ("display", Self::builtin_display),
            ("write", Self::builtin_write),
            ("newline", Self::builtin_newline),
            ("vector", Self::builtin_vector),
            ("make-vector", Self::builtin_make_vector),
            ("vector?", Self::builtin_vector_predicate),
            ("vector-ref", Self::builtin_vector_ref),
            ("vector-set!", Self::builtin_vector_set),
            ("vector-length", Self::builtin_vector_length),
            ("vector->list", Self::builtin_vector_to_list),
            ("list->vector", Self::builtin_list_to_vector),
            ("round", Self::builtin_round),
            ("truncate", Self::builtin_truncate),
            ("gcd", Self::builtin_gcd),
            ("lcm", Self::builtin_lcm),
            ("values", Self::builtin_values),
            ("call-with-values", Self::builtin_call_with_values),
            ("dynamic-wind", Self::builtin_dynamic_wind),
            ("call/cc", Self::builtin_call_cc),
            (
                "call-with-current-continuation",
                Self::builtin_call_with_current_continuation,
            ),
            ("raise", Self::builtin_raise),
            (
                "with-exception-handler",
                Self::builtin_with_exception_handler,
            ),
            ("error", Self::builtin_error),
        ] {
            env.define_value(name.to_owned(), builtin_value(name, action));
        }

        env
    }

    fn eval_program(&mut self, input: &str) -> Result<Value, EvalError> {
        let mut parser = Parser::new(input);
        let mut last = None;

        while parser.has_more() {
            let expr = parser.parse_expr()?;
            last = Some(self.eval_top_level_expr(expr, self.global_env.clone())?);
        }

        last.ok_or_else(|| EvalError::new("empty input"))
    }

    fn render_top_level(&self, value: Value) -> Result<String, EvalError> {
        match value {
            Value::Multi(values) => Err(EvalError::new(format!(
                "top-level expression returned {} values",
                values.len()
            ))),
            other => Ok(other.render()),
        }
    }

    fn eval_top_level_expr(&mut self, expr: Expr, env: Rc<Environment>) -> Result<Value, EvalError> {
        match catch_signal(|| self.eval_expr(expr, env)) {
            Ok(result) => result,
            Err(ControlSignal::Raised(exception)) => Err(EvalError::new(format!(
                "uncaught exception: {}",
                exception.render()
            ))),
            Err(signal) => resume_signal(signal),
        }
    }

    fn eval_expr(&mut self, mut expr: Expr, mut env: Rc<Environment>) -> Result<Value, EvalError> {
        self.eval_depth += 1;
        let depth = self.eval_depth;

        let result = catch_signal(|| loop {
            let position = expr.position();
            match catch_signal(|| self.eval_step(expr.clone(), env.clone())) {
                Ok(Ok(Step::Value(value))) => {
                    self.pop_guards_for_depth(depth);
                    break Ok(value);
                }
                Ok(Ok(Step::Tail(next_expr, next_env))) => {
                    expr = next_expr;
                    env = next_env;
                }
                Ok(Err(error)) => {
                    self.pop_guards_for_depth(depth);
                    break Err(error.with_position(position.line, position.column));
                }
                Err(ControlSignal::Raised(exception)) => {
                    match self.handle_guard_signal(depth, exception)? {
                        GuardResolution::Tail(next_expr, next_env) => {
                            expr = next_expr;
                            env = next_env;
                        }
                        GuardResolution::Value(value) => {
                            self.pop_guards_for_depth(depth);
                            break Ok(value);
                        }
                        GuardResolution::Rethrow(exception) => {
                            self.pop_guards_for_depth(depth);
                            signal_exception(exception);
                        }
                    }
                }
                Err(signal) => {
                    self.pop_guards_for_depth(depth);
                    resume_signal(signal);
                }
            }
        });

        self.eval_depth -= 1;
        match result {
            Ok(result) => result,
            Err(signal) => {
                self.pop_guards_for_depth(depth);
                resume_signal(signal);
            }
        }
    }

    fn eval_step(&mut self, expr: Expr, env: Rc<Environment>) -> Result<Step, EvalError> {
        match expr.kind() {
            ExprKind::Int(value) => Ok(Step::Value(Value::Int(*value))),
            ExprKind::Rational(value) => Ok(Step::Value(Value::Rational(*value))),
            ExprKind::Bool(value) => Ok(Step::Value(Value::Bool(*value))),
            ExprKind::String(value) => Ok(Step::Value(Value::String(SchemeString::new(value)))),
            ExprKind::Char(value) => Ok(Step::Value(Value::Char(*value))),
            ExprKind::Symbol(name) => Ok(Step::Value(env.lookup(name)?)),
            ExprKind::DottedList(_, _) => Err(EvalError::new("cannot evaluate dotted list")),
            ExprKind::List(elements) => self.eval_list(elements, env),
        }
    }

    fn eval_list(&mut self, elements: &[Expr], env: Rc<Environment>) -> Result<Step, EvalError> {
        let Some((head, args)) = elements.split_first() else {
            return Err(EvalError::new("cannot evaluate empty list"));
        };

        if let Some(name) = head.symbol_name() {
            return match name {
                "define" => self.eval_define(args, env),
                "define-syntax" => self.eval_define_syntax(args, env),
                "define-record-type" => self.eval_define_record_type(args, env),
                "if" => self.eval_if(args, env),
                "quote" => self.eval_quote(args),
                "lambda" => self.eval_lambda(args, env),
                "begin" => self.eval_begin(args, env),
                "let" => self.eval_let(args, env),
                "let*" => self.eval_let_star(args, env),
                "letrec" => self.eval_letrec(args, env, false),
                "letrec*" => self.eval_letrec(args, env, true),
                "cond" => self.eval_cond(args, env),
                "case" => self.eval_case(args, env),
                "and" => self.eval_and(args, env),
                "or" => self.eval_or(args, env),
                "do" => self.eval_do(head.position(), args, env),
                "guard" => self.eval_guard(head.position(), args, env),
                "set!" => self.eval_set(args, env),
                _ => {
                    if let Some(macro_binding) = env.lookup_syntax(name) {
                        let expanded = macro_binding.expand(head.position(), elements)?;
                        self.eval_step(expanded, env)
                    } else {
                        self.eval_application(head.clone(), args, env)
                    }
                }
            };
        }

        self.eval_application(head.clone(), args, env)
    }

    fn eval_application(
        &mut self,
        head: Expr,
        args: &[Expr],
        env: Rc<Environment>,
    ) -> Result<Step, EvalError> {
        let procedure = self.eval_expr(head, env.clone())?;
        let arguments = self.eval_args(args, env)?;
        self.tail_apply_value(procedure, arguments)
    }

    fn eval_define(&mut self, args: &[Expr], env: Rc<Environment>) -> Result<Step, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::new("define requires a name and a value"));
        }

        match args[0].kind() {
            ExprKind::Symbol(name) => {
                Self::require_arity("define", args.len(), 2)?;
                let value = self.eval_expr(args[1].clone(), env.clone())?;
                env.define_value(name.clone(), value);
                Ok(Step::Value(Value::Void))
            }
            ExprKind::List(_) | ExprKind::DottedList(_, _) => {
                let (name, params) = Self::parse_define_signature(&args[0])?;
                let body = Self::parse_body("define", &args[1..])?;
                let procedure = Value::Procedure(Rc::new(Procedure::User(UserProcedure {
                    name: Some(name.clone()),
                    params,
                    body,
                    closure_env: env.clone(),
                })));
                env.define_value(name, procedure);
                Ok(Step::Value(Value::Void))
            }
            _ => Err(EvalError::new("invalid define")),
        }
    }

    fn eval_define_syntax(
        &mut self,
        args: &[Expr],
        env: Rc<Environment>,
    ) -> Result<Step, EvalError> {
        Self::require_arity("define-syntax", args.len(), 2)?;
        let Some(name) = args[0].symbol_name() else {
            return Err(EvalError::new("define-syntax name must be a symbol"));
        };

        let macro_binding = SyntaxRulesMacro::compile(name, &args[1])?;
        env.define_syntax(name.to_owned(), macro_binding);
        Ok(Step::Value(Value::Void))
    }

    fn eval_define_record_type(
        &mut self,
        args: &[Expr],
        env: Rc<Environment>,
    ) -> Result<Step, EvalError> {
        if args.len() < 3 {
            return Err(EvalError::new(
                "define-record-type requires a type name, constructor, and predicate",
            ));
        }

        let Some(type_name) = args[0].symbol_name() else {
            return Err(EvalError::new("record type name must be a symbol"));
        };
        let constructor = Self::parse_record_constructor_spec(&args[1])?;
        let Some(predicate_name) = args[2].symbol_name() else {
            return Err(EvalError::new("record predicate name must be a symbol"));
        };
        let fields = Self::parse_record_field_specs(&args[3..])?;
        let record_type = Rc::new(RecordType::new(type_name.to_owned(), &fields)?);

        let constructor_indexes =
            Self::resolve_record_constructor_fields(&constructor.field_names, &record_type)?;

        env.define_value(
            constructor.name.clone(),
            Value::Procedure(Rc::new(Procedure::RecordConstructor(
                RecordConstructorProcedure {
                    name: constructor.name.clone(),
                    record_type: record_type.clone(),
                    field_indexes: constructor_indexes,
                },
            ))),
        );
        env.define_value(
            predicate_name.to_owned(),
            Value::Procedure(Rc::new(Procedure::RecordPredicate(
                RecordPredicateProcedure {
                    name: predicate_name.to_owned(),
                    record_type: record_type.clone(),
                },
            ))),
        );

        for (index, field) in fields.iter().enumerate() {
            env.define_value(
                field.accessor_name.clone(),
                Value::Procedure(Rc::new(Procedure::RecordAccessor(
                    RecordAccessorProcedure {
                        name: field.accessor_name.clone(),
                        record_type: record_type.clone(),
                        field_index: index,
                    },
                ))),
            );

            if let Some(mutator_name) = &field.mutator_name {
                env.define_value(
                    mutator_name.clone(),
                    Value::Procedure(Rc::new(Procedure::RecordMutator(
                        RecordMutatorProcedure {
                            name: mutator_name.clone(),
                            record_type: record_type.clone(),
                            field_index: index,
                        },
                    ))),
                );
            }
        }

        Ok(Step::Value(Value::Void))
    }

    fn eval_do(
        &mut self,
        position: SourcePos,
        args: &[Expr],
        env: Rc<Environment>,
    ) -> Result<Step, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::new("do requires bindings and a termination clause"));
        }

        let bindings = Self::parse_do_bindings(&args[0])?;
        let termination_parts = Self::list_items(&args[1], "do termination clause must be a list")?;
        if termination_parts.is_empty() {
            return Err(EvalError::new("do termination clause requires a test"));
        }

        let loop_name = self.fresh_symbol("do");
        let mut let_bindings = Vec::with_capacity(bindings.len());
        for binding in &bindings {
            let_bindings.push(Self::list_expr(
                position,
                vec![Self::symbol_expr(position, &binding.name), binding.init_expr.clone()],
            ));
        }

        let mut loop_args = Vec::with_capacity(bindings.len() + 1);
        loop_args.push(Self::symbol_expr(position, &loop_name));
        for binding in &bindings {
            let step_expr = binding
                .step_expr
                .clone()
                .unwrap_or_else(|| Self::symbol_expr(position, &binding.name));
            loop_args.push(step_expr);
        }

        let mut else_body = args[2..].to_vec();
        else_body.push(Self::list_expr(position, loop_args));

        let then_expr = if termination_parts.len() == 1 {
            Self::list_expr(position, vec![Self::symbol_expr(position, "begin")])
        } else {
            Self::list_expr(
                position,
                std::iter::once(Self::symbol_expr(position, "begin"))
                    .chain(termination_parts[1..].iter().cloned())
                    .collect(),
            )
        };
        let else_expr = Self::list_expr(
            position,
            std::iter::once(Self::symbol_expr(position, "begin"))
                .chain(else_body.into_iter())
                .collect(),
        );
        let if_expr = Self::list_expr(
            position,
            vec![
                Self::symbol_expr(position, "if"),
                termination_parts[0].clone(),
                then_expr,
                else_expr,
            ],
        );

        let named_let = Self::list_expr(
            position,
            vec![
                Self::symbol_expr(position, "let"),
                Self::symbol_expr(position, &loop_name),
                Expr::new(ExprKind::List(let_bindings), position),
                if_expr,
            ],
        );
        Ok(Step::Tail(named_let, env))
    }

    fn eval_guard(
        &mut self,
        position: SourcePos,
        args: &[Expr],
        env: Rc<Environment>,
    ) -> Result<Step, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::new("guard requires a clause list and a body"));
        }

        let guard_spec = Self::list_items(&args[0], "guard requires a clause list")?;
        let Some(variable_expr) = guard_spec.first() else {
            return Err(EvalError::new("guard clause list cannot be empty"));
        };
        let Some(variable_name) = variable_expr.symbol_name() else {
            return Err(EvalError::new("guard variable must be a symbol"));
        };

        self.guard_stack.push(ActiveGuard {
            depth: self.eval_depth,
            position,
            variable_name: variable_name.to_owned(),
            clauses: guard_spec[1..].to_vec(),
            env: env.clone(),
        });

        Ok(Step::Tail(
            Self::list_expr(
                position,
                std::iter::once(Self::symbol_expr(position, "begin"))
                    .chain(args[1..].iter().cloned())
                    .collect(),
            ),
            env,
        ))
    }

    fn eval_if(&mut self, args: &[Expr], env: Rc<Environment>) -> Result<Step, EvalError> {
        if !(2..=3).contains(&args.len()) {
            return Err(EvalError::new("if requires two or three expressions"));
        }

        let condition = self.eval_expr(args[0].clone(), env.clone())?;
        if Self::is_truthy(&condition) {
            Ok(Step::Tail(args[1].clone(), env))
        } else if args.len() == 3 {
            Ok(Step::Tail(args[2].clone(), env))
        } else {
            Ok(Step::Value(Value::Void))
        }
    }

    fn eval_quote(&mut self, args: &[Expr]) -> Result<Step, EvalError> {
        Self::require_arity("quote", args.len(), 1)?;
        Ok(Step::Value(Self::quote_to_value(&args[0])?))
    }

    fn eval_lambda(&mut self, args: &[Expr], env: Rc<Environment>) -> Result<Step, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::new("lambda requires parameters and a body"));
        }

        let params = Self::parse_parameter_spec(&args[0])?;
        let body = Self::parse_body("lambda", &args[1..])?;
        Ok(Step::Value(Value::Procedure(Rc::new(Procedure::User(
            UserProcedure {
                name: None,
                params,
                body,
                closure_env: env,
            },
        )))))
    }

    fn eval_begin(&mut self, args: &[Expr], env: Rc<Environment>) -> Result<Step, EvalError> {
        self.tail_from_sequence(args, env)
    }

    fn eval_let(&mut self, args: &[Expr], env: Rc<Environment>) -> Result<Step, EvalError> {
        let Some(first) = args.first() else {
            return Err(EvalError::new("let requires bindings and a body"));
        };

        match first.kind() {
            ExprKind::Symbol(name) => {
                if args.len() < 3 {
                    return Err(EvalError::new("let requires bindings and a body"));
                }
                let bindings = Self::parse_bindings(&args[1])?;
                let body = Self::parse_body("let", &args[2..])?;
                self.eval_named_let(name, &bindings, &body, env)
            }
            ExprKind::List(_) => {
                let bindings = Self::parse_bindings(first)?;
                let body = Self::parse_body("let", &args[1..])?;
                let let_env = Environment::new(Some(env.clone()));
                for binding in bindings {
                    let value = self.eval_expr(binding.value_expr, env.clone())?;
                    let_env.define_value(binding.name, value);
                }
                self.tail_from_sequence(&body, let_env)
            }
            _ => Err(EvalError::new("let bindings must be a list")),
        }
    }

    fn eval_named_let(
        &mut self,
        name: &str,
        bindings: &[LetBinding],
        body: &[Expr],
        env: Rc<Environment>,
    ) -> Result<Step, EvalError> {
        let mut param_names = Vec::with_capacity(bindings.len());
        let mut values = Vec::with_capacity(bindings.len());
        for binding in bindings {
            param_names.push(binding.name.clone());
            values.push(self.eval_expr(binding.value_expr.clone(), env.clone())?);
        }

        let proc_env = Environment::new(Some(env));
        let procedure = Value::Procedure(Rc::new(Procedure::User(UserProcedure {
            name: Some(name.to_owned()),
            params: ParameterSpec {
                required_names: param_names,
                rest_name: None,
            },
            body: body.to_vec(),
            closure_env: proc_env.clone(),
        })));
        proc_env.define_value(name.to_owned(), procedure.clone());
        self.tail_apply_value(procedure, values)
    }

    fn eval_let_star(&mut self, args: &[Expr], env: Rc<Environment>) -> Result<Step, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::new("let* requires bindings and a body"));
        }

        let bindings = Self::parse_bindings(&args[0])?;
        let body = Self::parse_body("let*", &args[1..])?;
        let current_env = Environment::new(Some(env));

        for binding in bindings {
            let value = self.eval_expr(binding.value_expr, current_env.clone())?;
            current_env.define_value(binding.name, value);
        }

        self.tail_from_sequence(&body, current_env)
    }

    fn eval_letrec(
        &mut self,
        args: &[Expr],
        env: Rc<Environment>,
        sequential: bool,
    ) -> Result<Step, EvalError> {
        let form_name = if sequential { "letrec*" } else { "letrec" };
        if args.len() < 2 {
            return Err(EvalError::new(format!(
                "{form_name} requires bindings and a body"
            )));
        }

        let bindings = Self::parse_bindings(&args[0])?;
        let body = Self::parse_body(form_name, &args[1..])?;
        let letrec_env = Environment::new(Some(env));
        let mut cells = Vec::with_capacity(bindings.len());

        for binding in &bindings {
            cells.push((
                binding.name.clone(),
                letrec_env.define_placeholder(binding.name.clone()),
            ));
        }

        if sequential {
            for (binding, (_, cell)) in bindings.iter().zip(cells.iter()) {
                let value = self.eval_expr(binding.value_expr.clone(), letrec_env.clone())?;
                *cell.borrow_mut() = value;
            }
        } else {
            let mut values = Vec::with_capacity(bindings.len());
            for binding in &bindings {
                values.push(self.eval_expr(binding.value_expr.clone(), letrec_env.clone())?);
            }
            for ((_, cell), value) in cells.into_iter().zip(values) {
                *cell.borrow_mut() = value;
            }
        }

        self.tail_from_sequence(&body, letrec_env)
    }

    fn eval_cond(&mut self, args: &[Expr], env: Rc<Environment>) -> Result<Step, EvalError> {
        for (index, clause_expr) in args.iter().enumerate() {
            let clause = Self::list_items(clause_expr, "cond clause must be a list")?;
            let Some((test_expr, body)) = clause.split_first() else {
                return Err(EvalError::new("cond clause cannot be empty"));
            };

            if test_expr.symbol_name() == Some("else") {
                if index != args.len() - 1 {
                    return Err(EvalError::new("cond else clause must be last"));
                }
                return self.tail_from_clause_body("cond", body, env.clone(), None);
            }

            let test_value = self.eval_expr(test_expr.clone(), env.clone())?;
            if !Self::is_truthy(&test_value) {
                continue;
            }

            if body.is_empty() {
                return Ok(Step::Value(test_value));
            }

            if body.len() == 2 && body[0].symbol_name() == Some("=>") {
                let procedure = self.eval_expr(body[1].clone(), env.clone())?;
                return self.tail_apply_value(procedure, vec![test_value]);
            }

            return self.tail_from_clause_body("cond", body, env.clone(), Some(test_value));
        }

        Ok(Step::Value(Value::Void))
    }

    fn eval_case(&mut self, args: &[Expr], env: Rc<Environment>) -> Result<Step, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::new(
                "case requires a key and at least one clause",
            ));
        }

        let key = self.eval_expr(args[0].clone(), env.clone())?;

        for (index, clause_expr) in args[1..].iter().enumerate() {
            let clause = Self::list_items(clause_expr, "case clause must be a list")?;
            let Some((datum_expr, body)) = clause.split_first() else {
                return Err(EvalError::new("case clause cannot be empty"));
            };

            if datum_expr.symbol_name() == Some("else") {
                if index != args.len() - 2 {
                    return Err(EvalError::new("case else clause must be last"));
                }
                return self.tail_from_clause_body("case", body, env.clone(), None);
            }

            let datums = Self::list_items(datum_expr, "case datums must be a list")?;
            let matched = datums.iter().any(|datum| {
                let Ok(value) = Self::quote_to_value(datum) else {
                    return false;
                };
                Self::eqv_values(&key, &value)
            });

            if matched {
                return self.tail_from_clause_body("case", body, env.clone(), None);
            }
        }

        Ok(Step::Value(Value::Void))
    }

    fn eval_and(&mut self, args: &[Expr], env: Rc<Environment>) -> Result<Step, EvalError> {
        if args.is_empty() {
            return Ok(Step::Value(Value::Bool(true)));
        }

        for expr in &args[..args.len() - 1] {
            let value = self.eval_expr(expr.clone(), env.clone())?;
            if !Self::is_truthy(&value) {
                return Ok(Step::Value(value));
            }
        }

        Ok(Step::Tail(args[args.len() - 1].clone(), env))
    }

    fn eval_or(&mut self, args: &[Expr], env: Rc<Environment>) -> Result<Step, EvalError> {
        if args.is_empty() {
            return Ok(Step::Value(Value::Bool(false)));
        }

        for expr in &args[..args.len() - 1] {
            let value = self.eval_expr(expr.clone(), env.clone())?;
            if Self::is_truthy(&value) {
                return Ok(Step::Value(value));
            }
        }

        Ok(Step::Tail(args[args.len() - 1].clone(), env))
    }

    fn eval_set(&mut self, args: &[Expr], env: Rc<Environment>) -> Result<Step, EvalError> {
        Self::require_arity("set!", args.len(), 2)?;
        let Some(name) = args[0].symbol_name() else {
            return Err(EvalError::new("set! requires a symbol"));
        };

        let value = self.eval_expr(args[1].clone(), env.clone())?;
        env.set(name, value)?;
        Ok(Step::Value(Value::Void))
    }

    fn tail_from_clause_body(
        &mut self,
        form_name: &str,
        body: &[Expr],
        env: Rc<Environment>,
        default_value: Option<Value>,
    ) -> Result<Step, EvalError> {
        if body.is_empty() {
            return default_value
                .map(Step::Value)
                .ok_or_else(|| EvalError::new(format!("{form_name} clause requires a body")));
        }

        self.tail_from_sequence(body, env)
    }

    fn tail_from_sequence(
        &mut self,
        exprs: &[Expr],
        env: Rc<Environment>,
    ) -> Result<Step, EvalError> {
        if exprs.is_empty() {
            return Ok(Step::Value(Value::Void));
        }

        for expr in &exprs[..exprs.len() - 1] {
            self.eval_expr(expr.clone(), env.clone())?;
        }

        Ok(Step::Tail(exprs[exprs.len() - 1].clone(), env))
    }

    fn eval_body(&mut self, body: &[Expr], env: Rc<Environment>) -> Result<Value, EvalError> {
        match self.tail_from_sequence(body, env)? {
            Step::Value(value) => Ok(value),
            Step::Tail(expr, env) => self.eval_expr(expr, env),
        }
    }

    fn fresh_symbol(&mut self, prefix: &str) -> String {
        self.synthetic_counter += 1;
        format!("__ming${prefix}${}", self.synthetic_counter)
    }

    fn symbol_expr(position: SourcePos, name: &str) -> Expr {
        Expr::new(ExprKind::Symbol(name.to_owned()), position)
    }

    fn list_expr(position: SourcePos, elements: Vec<Expr>) -> Expr {
        Expr::new(ExprKind::List(elements), position)
    }

    fn pop_guards_for_depth(&mut self, depth: usize) {
        while self
            .guard_stack
            .last()
            .is_some_and(|guard| guard.depth == depth)
        {
            self.guard_stack.pop();
        }
    }

    fn handle_guard_signal(
        &mut self,
        depth: usize,
        exception: Value,
    ) -> Result<GuardResolution, EvalError> {
        while let Some(guard) = self.guard_stack.last() {
            if guard.depth != depth {
                break;
            }

            let guard = self.guard_stack.pop().expect("guard stack underflow");
            if let Some(resolution) = self.try_guard_clauses(&guard, exception.clone())? {
                return Ok(resolution);
            }
        }

        Ok(GuardResolution::Rethrow(exception))
    }

    fn try_guard_clauses(
        &mut self,
        guard: &ActiveGuard,
        exception: Value,
    ) -> Result<Option<GuardResolution>, EvalError> {
        let guard_env = Environment::new(Some(guard.env.clone()));
        guard_env.define_value(guard.variable_name.clone(), exception);

        for (index, clause_expr) in guard.clauses.iter().enumerate() {
            let clause = Self::list_items(clause_expr, "guard clause must be a list")?;
            let Some((test_expr, body)) = clause.split_first() else {
                return Err(EvalError::new("guard clause cannot be empty"));
            };

            if test_expr.symbol_name() == Some("else") {
                if index != guard.clauses.len() - 1 {
                    return Err(EvalError::new("guard else clause must be last"));
                }
                return Ok(Some(Self::guard_resolution_from_step(self.tail_from_clause_body(
                    "guard",
                    body,
                    guard_env.clone(),
                    None,
                )?)));
            }

            let test_value = self.eval_expr(test_expr.clone(), guard_env.clone())?;
            if Self::is_truthy(&test_value) {
                return Ok(Some(Self::guard_resolution_from_step(self.tail_from_clause_body(
                    "guard",
                    body,
                    guard_env.clone(),
                    Some(test_value),
                )?)));
            }
        }

        Ok(None)
    }

    fn guard_resolution_from_step(step: Step) -> GuardResolution {
        match step {
            Step::Value(value) => GuardResolution::Value(value),
            Step::Tail(expr, env) => GuardResolution::Tail(expr, env),
        }
    }

    fn eval_args(&mut self, args: &[Expr], env: Rc<Environment>) -> Result<Vec<Value>, EvalError> {
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            values.push(self.eval_expr(arg.clone(), env.clone())?);
        }
        Ok(values)
    }

    fn tail_apply_value(
        &mut self,
        procedure_value: Value,
        args: Vec<Value>,
    ) -> Result<Step, EvalError> {
        let Value::Procedure(procedure) = procedure_value else {
            return Err(EvalError::new("not a procedure"));
        };

        match &*procedure {
            Procedure::Builtin(builtin) => Ok(Step::Value((builtin.action)(self, &args)?)),
            Procedure::User(user) => {
                let call_env = Self::prepare_user_call(user, &args)?;
                self.tail_from_sequence(&user.body, call_env)
            }
            Procedure::Continuation(continuation) => {
                signal_continuation_jump(continuation.id, args);
            }
            Procedure::RecordConstructor(constructor) => {
                Ok(Step::Value(constructor.apply(&args)?))
            }
            Procedure::RecordPredicate(predicate) => Ok(Step::Value(predicate.apply(&args)?)),
            Procedure::RecordAccessor(accessor) => Ok(Step::Value(accessor.apply(&args)?)),
            Procedure::RecordMutator(mutator) => Ok(Step::Value(mutator.apply(&args)?)),
        }
    }

    fn call_value(&mut self, procedure_value: Value, args: Vec<Value>) -> Result<Value, EvalError> {
        let Value::Procedure(procedure) = procedure_value else {
            return Err(EvalError::new("not a procedure"));
        };

        match &*procedure {
            Procedure::Builtin(builtin) => (builtin.action)(self, &args),
            Procedure::User(user) => {
                let call_env = Self::prepare_user_call(user, &args)?;
                self.eval_body(&user.body, call_env)
            }
            Procedure::Continuation(continuation) => {
                signal_continuation_jump(continuation.id, args);
            }
            Procedure::RecordConstructor(constructor) => constructor.apply(&args),
            Procedure::RecordPredicate(predicate) => predicate.apply(&args),
            Procedure::RecordAccessor(accessor) => accessor.apply(&args),
            Procedure::RecordMutator(mutator) => mutator.apply(&args),
        }
    }

    fn prepare_user_call(
        procedure: &UserProcedure,
        args: &[Value],
    ) -> Result<Rc<Environment>, EvalError> {
        procedure
            .params
            .validate_arity(procedure.display_name(), args.len())?;

        let call_env = Environment::new(Some(procedure.closure_env.clone()));
        for (name, value) in procedure.params.required_names.iter().zip(args.iter()) {
            call_env.define_value(name.clone(), value.clone());
        }
        if let Some(rest_name) = &procedure.params.rest_name {
            call_env.define_value(
                rest_name.clone(),
                Self::make_list(&args[procedure.params.required_names.len()..]),
            );
        }

        Ok(call_env)
    }

    fn parse_body(form_name: &str, body: &[Expr]) -> Result<Vec<Expr>, EvalError> {
        if body.is_empty() {
            Err(EvalError::new(format!("{form_name} requires a body")))
        } else {
            Ok(body.to_vec())
        }
    }

    fn parse_define_signature(expr: &Expr) -> Result<(String, ParameterSpec), EvalError> {
        match expr.kind() {
            ExprKind::List(items) => {
                let Some((name_expr, params)) = items.split_first() else {
                    return Err(EvalError::new("define requires a function name"));
                };
                let Some(name) = name_expr.symbol_name() else {
                    return Err(EvalError::new("function name must be a symbol"));
                };
                Ok((
                    name.to_owned(),
                    Self::parse_parameter_list_items(params, None)?,
                ))
            }
            ExprKind::DottedList(items, tail) => {
                let Some((name_expr, params)) = items.split_first() else {
                    return Err(EvalError::new("define requires a function name"));
                };
                let Some(name) = name_expr.symbol_name() else {
                    return Err(EvalError::new("function name must be a symbol"));
                };
                Ok((
                    name.to_owned(),
                    Self::parse_parameter_list_items(params, Some(tail.clone()))?,
                ))
            }
            _ => Err(EvalError::new("invalid function signature")),
        }
    }

    fn parse_parameter_spec(expr: &Expr) -> Result<ParameterSpec, EvalError> {
        match expr.kind() {
            ExprKind::Symbol(name) => Ok(ParameterSpec {
                required_names: Vec::new(),
                rest_name: Some(name.clone()),
            }),
            ExprKind::List(items) => Self::parse_parameter_list_items(items, None),
            ExprKind::DottedList(items, tail) => {
                Self::parse_parameter_list_items(items, Some(tail.clone()))
            }
            _ => Err(EvalError::new("lambda parameters must be a list or symbol")),
        }
    }

    fn parse_parameter_list_items(
        items: &[Expr],
        tail: Option<Expr>,
    ) -> Result<ParameterSpec, EvalError> {
        let mut required_names = Vec::with_capacity(items.len());
        for item in items {
            let Some(name) = item.symbol_name() else {
                return Err(EvalError::new("parameter must be a symbol"));
            };
            if name == "." {
                return Err(EvalError::new("invalid parameter list"));
            }
            required_names.push(name.to_owned());
        }

        let rest_name = match tail {
            Some(expr) => {
                let Some(name) = expr.symbol_name() else {
                    return Err(EvalError::new("rest parameter must be a symbol"));
                };
                if name == "." {
                    return Err(EvalError::new("rest parameter must be a symbol"));
                }
                Some(name.to_owned())
            }
            None => None,
        };

        Ok(ParameterSpec {
            required_names,
            rest_name,
        })
    }

    fn parse_bindings(expr: &Expr) -> Result<Vec<LetBinding>, EvalError> {
        let binding_exprs = Self::list_items(expr, "let bindings must be a list")?;
        let mut bindings = Vec::with_capacity(binding_exprs.len());

        for binding_expr in binding_exprs {
            let binding = Self::list_items(binding_expr, "let binding must be a list")?;
            if binding.len() != 2 {
                return Err(EvalError::new("let binding must contain a name and value"));
            }
            let Some(name) = binding[0].symbol_name() else {
                return Err(EvalError::new("let binding name must be a symbol"));
            };
            bindings.push(LetBinding {
                name: name.to_owned(),
                value_expr: binding[1].clone(),
            });
        }

        Ok(bindings)
    }

    fn parse_do_bindings(expr: &Expr) -> Result<Vec<DoBinding>, EvalError> {
        let binding_exprs = Self::list_items(expr, "do bindings must be a list")?;
        let mut bindings = Vec::with_capacity(binding_exprs.len());

        for binding_expr in binding_exprs {
            let binding = Self::list_items(binding_expr, "do binding must be a list")?;
            if binding.len() < 2 || binding.len() > 3 {
                return Err(EvalError::new(
                    "do binding must contain a name, init, and optional step",
                ));
            }
            let Some(name) = binding[0].symbol_name() else {
                return Err(EvalError::new("do binding name must be a symbol"));
            };
            bindings.push(DoBinding {
                name: name.to_owned(),
                init_expr: binding[1].clone(),
                step_expr: binding.get(2).cloned(),
            });
        }

        Ok(bindings)
    }

    fn parse_record_constructor_spec(expr: &Expr) -> Result<RecordConstructorSpec, EvalError> {
        let parts = Self::list_items(expr, "record constructor spec must be a list")?;
        let Some(name_expr) = parts.first() else {
            return Err(EvalError::new("record constructor spec cannot be empty"));
        };
        let Some(name) = name_expr.symbol_name() else {
            return Err(EvalError::new("record constructor name must be a symbol"));
        };

        let mut field_names = Vec::with_capacity(parts.len().saturating_sub(1));
        for field_expr in &parts[1..] {
            let Some(field_name) = field_expr.symbol_name() else {
                return Err(EvalError::new("record constructor field must be a symbol"));
            };
            field_names.push(field_name.to_owned());
        }

        Ok(RecordConstructorSpec {
            name: name.to_owned(),
            field_names,
        })
    }

    fn parse_record_field_specs(field_exprs: &[Expr]) -> Result<Vec<RecordFieldSpec>, EvalError> {
        let mut fields = Vec::with_capacity(field_exprs.len());
        for field_expr in field_exprs {
            let parts = Self::list_items(field_expr, "record field spec must be a list")?;
            if parts.len() < 2 || parts.len() > 3 {
                return Err(EvalError::new(
                    "record field spec must contain a field name, accessor, and optional mutator",
                ));
            }

            let Some(field_name) = parts[0].symbol_name() else {
                return Err(EvalError::new("record field name must be a symbol"));
            };
            let Some(accessor_name) = parts[1].symbol_name() else {
                return Err(EvalError::new("record accessor name must be a symbol"));
            };
            let mutator_name = if parts.len() == 3 {
                let Some(mutator_name) = parts[2].symbol_name() else {
                    return Err(EvalError::new("record mutator name must be a symbol"));
                };
                Some(mutator_name.to_owned())
            } else {
                None
            };

            fields.push(RecordFieldSpec {
                field_name: field_name.to_owned(),
                accessor_name: accessor_name.to_owned(),
                mutator_name,
            });
        }

        Ok(fields)
    }

    fn resolve_record_constructor_fields(
        constructor_fields: &[String],
        record_type: &RecordType,
    ) -> Result<Vec<usize>, EvalError> {
        let mut indexes = Vec::with_capacity(constructor_fields.len());
        let mut used = vec![false; record_type.field_names.len()];

        for field_name in constructor_fields {
            let Some(index) = record_type.field_indexes.get(field_name).copied() else {
                return Err(EvalError::new(format!("unknown record field: {field_name}")));
            };
            if used[index] {
                return Err(EvalError::new(format!("duplicate record field: {field_name}")));
            }
            used[index] = true;
            indexes.push(index);
        }

        Ok(indexes)
    }

    fn list_items<'a>(expr: &'a Expr, error_message: &str) -> Result<&'a [Expr], EvalError> {
        match expr.kind() {
            ExprKind::List(items) => Ok(items),
            _ => Err(EvalError::new(error_message)),
        }
    }

    fn is_truthy(value: &Value) -> bool {
        !matches!(value, Value::Bool(false))
    }

    fn quote_to_value(expr: &Expr) -> Result<Value, EvalError> {
        match expr.kind() {
            ExprKind::Int(value) => Ok(Value::Int(*value)),
            ExprKind::Rational(value) => Ok(Value::Rational(*value)),
            ExprKind::Bool(value) => Ok(Value::Bool(*value)),
            ExprKind::String(value) => Ok(Value::String(SchemeString::new(value))),
            ExprKind::Char(value) => Ok(Value::Char(*value)),
            ExprKind::Symbol(name) => Ok(Value::Symbol(name.clone())),
            ExprKind::List(items) => {
                let mut result = Value::EmptyList;
                for item in items.iter().rev() {
                    result = Value::Pair(PairCell::new_rc(Self::quote_to_value(item)?, result));
                }
                Ok(result)
            }
            ExprKind::DottedList(items, tail) => {
                let mut result = Self::quote_to_value(tail)?;
                for item in items.iter().rev() {
                    result = Value::Pair(PairCell::new_rc(Self::quote_to_value(item)?, result));
                }
                Ok(result)
            }
        }
    }

    fn take_output(&mut self) -> String {
        self.output.take().unwrap_or_default()
    }

    fn append_output(&mut self, text: &str) {
        if let Some(output) = &mut self.output {
            output.push_str(text);
        }
    }

    fn render_for_display(value: &Value) -> String {
        match value {
            Value::String(string) => string.as_plain_string().to_owned(),
            Value::Char(ch) => ch.to_string(),
            _ => value.render(),
        }
    }

    fn builtin_add(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        let mut total = ExactNumber::from_int(0);
        for arg in args {
            total = total.add(Self::expect_number(arg)?)?;
        }
        Ok(total.to_value())
    }

    fn builtin_subtract(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_at_least("-", args.len(), 1)?;

        let mut result = Self::expect_number(&args[0])?;
        if args.len() == 1 {
            return Ok(ExactNumber::from_int(0).subtract(result)?.to_value());
        }

        for arg in &args[1..] {
            result = result.subtract(Self::expect_number(arg)?)?;
        }
        Ok(result.to_value())
    }

    fn builtin_multiply(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        let mut total = ExactNumber::from_int(1);
        for arg in args {
            total = total.multiply(Self::expect_number(arg)?)?;
        }
        Ok(total.to_value())
    }

    fn builtin_divide(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_at_least("/", args.len(), 2)?;
        let mut result = Self::expect_number(&args[0])?;
        for arg in &args[1..] {
            result = result.divide(Self::expect_number(arg)?)?;
        }
        Ok(result.to_value())
    }

    fn builtin_num_eq(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Ok(Value::Bool(Self::compare_numbers(args, |a, b| a == b)?))
    }

    fn builtin_less(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Ok(Value::Bool(Self::compare_numbers(args, |a, b| {
            a.cmp(b) == Ordering::Less
        })?))
    }

    fn builtin_greater(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Ok(Value::Bool(Self::compare_numbers(args, |a, b| {
            a.cmp(b) == Ordering::Greater
        })?))
    }

    fn builtin_less_equal(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Ok(Value::Bool(Self::compare_numbers(args, |a, b| {
            matches!(a.cmp(b), Ordering::Less | Ordering::Equal)
        })?))
    }

    fn builtin_greater_equal(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Ok(Value::Bool(Self::compare_numbers(args, |a, b| {
            matches!(a.cmp(b), Ordering::Greater | Ordering::Equal)
        })?))
    }

    fn builtin_abs(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("abs", args.len(), 1)?;
        Ok(Self::expect_number(&args[0])?.abs().to_value())
    }

    fn builtin_modulo(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("modulo", args.len(), 2)?;
        let dividend = Self::expect_int(&args[0])?;
        let divisor = Self::expect_int(&args[1])?;
        if divisor == 0 {
            return Err(EvalError::new("division by zero"));
        }
        let mut result = dividend % divisor;
        if result != 0 && (result > 0) != (divisor > 0) {
            result += divisor;
        }
        Ok(Value::Int(result))
    }

    fn builtin_remainder(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("remainder", args.len(), 2)?;
        let dividend = Self::expect_int(&args[0])?;
        let divisor = Self::expect_int(&args[1])?;
        if divisor == 0 {
            return Err(EvalError::new("division by zero"));
        }
        Ok(Value::Int(dividend % divisor))
    }

    fn builtin_quotient(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("quotient", args.len(), 2)?;
        let dividend = Self::expect_int(&args[0])?;
        let divisor = Self::expect_int(&args[1])?;
        if divisor == 0 {
            return Err(EvalError::new("division by zero"));
        }
        Ok(Value::Int(dividend / divisor))
    }

    fn builtin_min(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_at_least("min", args.len(), 1)?;
        let mut result = Self::expect_number(&args[0])?;
        for arg in &args[1..] {
            let current = Self::expect_number(arg)?;
            if current.cmp(result) == Ordering::Less {
                result = current;
            }
        }
        Ok(result.to_value())
    }

    fn builtin_max(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_at_least("max", args.len(), 1)?;
        let mut result = Self::expect_number(&args[0])?;
        for arg in &args[1..] {
            let current = Self::expect_number(arg)?;
            if current.cmp(result) == Ordering::Greater {
                result = current;
            }
        }
        Ok(result.to_value())
    }

    fn builtin_expt(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("expt", args.len(), 2)?;
        let base = Self::expect_int(&args[0])?;
        let exponent = Self::expect_int(&args[1])?;
        if exponent < 0 {
            return Err(EvalError::new("negative exponent not supported"));
        }
        Ok(Value::Int(base.pow(exponent as u32)))
    }

    fn builtin_not(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("not", args.len(), 1)?;
        Ok(Value::Bool(!Self::is_truthy(&args[0])))
    }

    fn builtin_zero_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::numeric_predicate("zero?", args, |n| n.numerator == 0)
    }

    fn builtin_positive_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::numeric_predicate("positive?", args, |n| n.numerator > 0)
    }

    fn builtin_negative_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::numeric_predicate("negative?", args, |n| n.numerator < 0)
    }

    fn builtin_odd_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("odd?", args.len(), 1)?;
        Ok(Value::Bool(Self::expect_int(&args[0])? % 2 != 0))
    }

    fn builtin_even_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("even?", args.len(), 1)?;
        Ok(Value::Bool(Self::expect_int(&args[0])? % 2 == 0))
    }

    fn builtin_cons(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("cons", args.len(), 2)?;
        Ok(Value::Pair(PairCell::new_rc(
            args[0].clone(),
            args[1].clone(),
        )))
    }

    fn builtin_car(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("car", args.len(), 1)?;
        Ok(Self::expect_pair(&args[0])?.car())
    }

    fn builtin_cdr(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("cdr", args.len(), 1)?;
        Ok(Self::expect_pair(&args[0])?.cdr())
    }

    fn builtin_caar(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("caar", args.len(), 1)?;
        let pair = Self::expect_pair(&args[0])?;
        Ok(Self::expect_pair(&pair.car())?.car())
    }

    fn builtin_cadr(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("cadr", args.len(), 1)?;
        let pair = Self::expect_pair(&args[0])?;
        Ok(Self::expect_pair(&pair.cdr())?.car())
    }

    fn builtin_cdar(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("cdar", args.len(), 1)?;
        let pair = Self::expect_pair(&args[0])?;
        Ok(Self::expect_pair(&pair.car())?.cdr())
    }

    fn builtin_cddr(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("cddr", args.len(), 1)?;
        let pair = Self::expect_pair(&args[0])?;
        Ok(Self::expect_pair(&pair.cdr())?.cdr())
    }

    fn builtin_set_car(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("set-car!", args.len(), 2)?;
        Self::expect_pair(&args[0])?.set_car(args[1].clone());
        Ok(Value::Void)
    }

    fn builtin_set_cdr(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("set-cdr!", args.len(), 2)?;
        Self::expect_pair(&args[0])?.set_cdr(args[1].clone());
        Ok(Value::Void)
    }

    fn builtin_null_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("null?", args.len(), 1)?;
        Ok(Value::Bool(matches!(args[0], Value::EmptyList)))
    }

    fn builtin_pair_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("pair?", args, |value| matches!(value, Value::Pair(_)))
    }

    fn builtin_list_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("list?", args.len(), 1)?;
        Ok(Value::Bool(Self::is_proper_list(&args[0])))
    }

    fn builtin_list(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Ok(Self::make_list(args))
    }

    fn builtin_length(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("length", args.len(), 1)?;
        Ok(Value::Int(Self::list_length(&args[0])? as i64))
    }

    fn builtin_append(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Ok(Value::EmptyList);
        }

        let mut result = args[args.len() - 1].clone();
        for value in args[..args.len() - 1].iter().rev() {
            let elements = Self::list_elements(value)?;
            for element in elements.into_iter().rev() {
                result = Value::Pair(PairCell::new_rc(element, result));
            }
        }
        Ok(result)
    }

    fn builtin_reverse(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("reverse", args.len(), 1)?;
        let mut result = Value::EmptyList;
        let mut current = args[0].clone();
        let mut seen = HashSet::new();

        loop {
            match current {
                Value::Pair(pair) => {
                    let id = Rc::as_ptr(&pair) as usize;
                    if !seen.insert(id) {
                        return Err(EvalError::new("expected list"));
                    }
                    result = Value::Pair(PairCell::new_rc(pair.car(), result));
                    current = pair.cdr();
                }
                Value::EmptyList => return Ok(result),
                _ => return Err(EvalError::new("expected list")),
            }
        }
    }

    fn builtin_list_ref(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("list-ref", args.len(), 2)?;
        let tail = Self::list_tail_value(&args[0], Self::expect_index(&args[1], "list-ref")?)?;
        Ok(Self::expect_pair(&tail)?.car())
    }

    fn builtin_list_tail(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("list-tail", args.len(), 2)?;
        Self::list_tail_value(&args[0], Self::expect_index(&args[1], "list-tail")?)
    }

    fn builtin_apply(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_at_least("apply", args.len(), 2)?;
        let mut expanded = Vec::new();
        expanded.extend(args[1..args.len() - 1].iter().cloned());
        expanded.extend(Self::list_elements(&args[args.len() - 1])?);
        interpreter.call_value(args[0].clone(), expanded)
    }

    fn builtin_map(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_at_least("map", args.len(), 2)?;
        let mut cursors = args[1..].to_vec();
        let mut results = Vec::new();

        loop {
            let mut call_args = Vec::with_capacity(cursors.len());
            for cursor in &mut cursors {
                match cursor.clone() {
                    Value::EmptyList => return Ok(Self::make_list(&results)),
                    Value::Pair(pair) => {
                        call_args.push(pair.car());
                        *cursor = pair.cdr();
                    }
                    _ => return Err(EvalError::new("expected list")),
                }
            }
            results.push(interpreter.call_value(args[0].clone(), call_args)?);
        }
    }

    fn builtin_for_each(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_at_least("for-each", args.len(), 2)?;
        let mut cursors = args[1..].to_vec();

        loop {
            let mut call_args = Vec::with_capacity(cursors.len());
            for cursor in &mut cursors {
                match cursor.clone() {
                    Value::EmptyList => return Ok(Value::Void),
                    Value::Pair(pair) => {
                        call_args.push(pair.car());
                        *cursor = pair.cdr();
                    }
                    _ => return Err(EvalError::new("expected list")),
                }
            }
            interpreter.call_value(args[0].clone(), call_args)?;
        }
    }

    fn builtin_eq(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("eq?", args.len(), 2)?;
        Ok(Value::Bool(Self::eq_values(&args[0], &args[1])))
    }

    fn builtin_eqv(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("eqv?", args.len(), 2)?;
        Ok(Value::Bool(Self::eqv_values(&args[0], &args[1])))
    }

    fn builtin_equal(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("equal?", args.len(), 2)?;
        let mut seen = HashSet::new();
        Ok(Value::Bool(Self::equal_values(
            &args[0], &args[1], &mut seen,
        )))
    }

    fn builtin_assoc(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::assoc_like(args, "assoc", |left, right| {
            let mut seen = HashSet::new();
            Self::equal_values(left, right, &mut seen)
        })
    }

    fn builtin_assq(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::assoc_like(args, "assq", Self::eq_values)
    }

    fn builtin_assv(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::assoc_like(args, "assv", Self::eqv_values)
    }

    fn builtin_member(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::member_like(args, "member", |left, right| {
            let mut seen = HashSet::new();
            Self::equal_values(left, right, &mut seen)
        })
    }

    fn builtin_memq(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::member_like(args, "memq", Self::eq_values)
    }

    fn builtin_memv(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::member_like(args, "memv", Self::eqv_values)
    }

    fn builtin_string_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("string?", args, |value| matches!(value, Value::String(_)))
    }

    fn builtin_symbol_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("symbol?", args, |value| matches!(value, Value::Symbol(_)))
    }

    fn builtin_number_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("number?", args, |value| {
            matches!(value, Value::Int(_) | Value::Rational(_))
        })
    }

    fn builtin_integer_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("integer?", args, |value| match value {
            Value::Int(_) => true,
            Value::Rational(number) => number.is_integer(),
            _ => false,
        })
    }

    fn builtin_boolean_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("boolean?", args, |value| matches!(value, Value::Bool(_)))
    }

    fn builtin_procedure_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("procedure?", args, |value| {
            matches!(value, Value::Procedure(_))
        })
    }

    fn builtin_char_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("char?", args, |value| matches!(value, Value::Char(_)))
    }

    fn builtin_string_append(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        let mut result = String::new();
        for arg in args {
            result.push_str(Self::expect_string(arg)?.as_plain_string());
        }
        Ok(Value::String(SchemeString::new_owned(result)))
    }

    fn builtin_string_length(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("string-length", args.len(), 1)?;
        Ok(Value::Int(Self::expect_string(&args[0])?.char_len() as i64))
    }

    fn builtin_substring(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("substring", args.len(), 3)?;
        let string = Self::expect_string(&args[0])?;
        let start = Self::expect_index(&args[1], "substring")?;
        let end = Self::expect_index(&args[2], "substring")?;
        let chars = string.characters();
        if start > end || end > chars.len() {
            return Err(EvalError::new("substring indices out of range"));
        }
        Ok(Value::String(SchemeString::new_owned(
            chars[start..end].iter().collect(),
        )))
    }

    fn builtin_string_to_number(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("string->number", args.len(), 1)?;
        let string = Self::expect_string(&args[0])?.as_plain_string().to_owned();
        if let Ok(number) = string.parse::<i64>() {
            return Ok(Value::Int(number));
        }
        if let Some(number) = parse_exact_number_literal(&string) {
            return Ok(number.to_value());
        }
        Ok(Value::Bool(false))
    }

    fn builtin_number_to_string(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("number->string", args.len(), 1)?;
        Ok(Value::String(SchemeString::new_owned(
            Self::expect_number(&args[0])?.render(),
        )))
    }

    fn builtin_symbol_to_string(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("symbol->string", args.len(), 1)?;
        Ok(Value::String(SchemeString::new_owned(
            Self::expect_symbol(&args[0])?.to_owned(),
        )))
    }

    fn builtin_string_to_symbol(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("string->symbol", args.len(), 1)?;
        Ok(Value::Symbol(
            Self::expect_string(&args[0])?.as_plain_string().to_owned(),
        ))
    }

    fn builtin_string_ref(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("string-ref", args.len(), 2)?;
        let string = Self::expect_string(&args[0])?;
        let index = Self::expect_index(&args[1], "string-ref")?;
        let Some(ch) = string.characters().get(index).copied() else {
            return Err(EvalError::new("string-ref index out of range"));
        };
        Ok(Value::Char(ch))
    }

    fn builtin_string_copy(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("string-copy", args.len(), 1)?;
        Ok(Value::String(Self::expect_string(&args[0])?.copy()))
    }

    fn builtin_string_set(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("string-set!", args.len(), 3)?;
        let _ = Self::expect_string(&args[0])?;
        let _ = Self::expect_index(&args[1], "string-set!")?;
        let _ = Self::expect_char(&args[2])?;
        Err(EvalError::new("string-set! on immutable string"))
    }

    fn builtin_make_string(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        if !(1..=2).contains(&args.len()) {
            return Err(EvalError::new("wrong number of arguments for make-string"));
        }
        let length = Self::expect_index(&args[0], "make-string")?;
        let fill = if args.len() == 2 {
            Self::expect_char(&args[1])?
        } else {
            ' '
        };
        Ok(Value::String(SchemeString::new_owned(
            std::iter::repeat_n(fill, length).collect(),
        )))
    }

    fn builtin_string(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        let mut result = String::with_capacity(args.len());
        for arg in args {
            result.push(Self::expect_char(arg)?);
        }
        Ok(Value::String(SchemeString::new_owned(result)))
    }

    fn builtin_string_less(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::string_compare(args, "string<?", |left, right| left < right)
    }

    fn builtin_string_less_equal(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::string_compare(args, "string<=?", |left, right| left <= right)
    }

    fn builtin_string_equal(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::string_compare(args, "string=?", |left, right| left == right)
    }

    fn builtin_string_greater_equal(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::string_compare(args, "string>=?", |left, right| left >= right)
    }

    fn builtin_string_greater(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::string_compare(args, "string>?", |left, right| left > right)
    }

    fn builtin_display(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("display", args.len(), 1)?;
        interpreter.append_output(&Self::render_for_display(&args[0]));
        Ok(Value::Void)
    }

    fn builtin_write(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("write", args.len(), 1)?;
        interpreter.append_output(&args[0].render());
        Ok(Value::Void)
    }

    fn builtin_newline(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("newline", args.len(), 0)?;
        interpreter.append_output("\n");
        Ok(Value::Void)
    }

    fn builtin_vector(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
    }

    fn builtin_make_vector(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        if !(1..=2).contains(&args.len()) {
            return Err(EvalError::new("wrong number of arguments for make-vector"));
        }
        let length = Self::expect_index(&args[0], "make-vector")?;
        let fill = args.get(1).cloned().unwrap_or(Value::Void);
        Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; length]))))
    }

    fn builtin_vector_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("vector?", args, |value| matches!(value, Value::Vector(_)))
    }

    fn builtin_vector_ref(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("vector-ref", args.len(), 2)?;
        let vector = Self::expect_vector(&args[0])?;
        let index = Self::expect_index(&args[1], "vector-ref")?;
        let value = vector
            .borrow()
            .get(index)
            .cloned()
            .ok_or_else(|| EvalError::new("vector-ref index out of range"))?;
        Ok(value)
    }

    fn builtin_vector_set(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("vector-set!", args.len(), 3)?;
        let vector = Self::expect_vector(&args[0])?;
        let index = Self::expect_index(&args[1], "vector-set!")?;
        let mut values = vector.borrow_mut();
        if index >= values.len() {
            return Err(EvalError::new("vector-set! index out of range"));
        }
        values[index] = args[2].clone();
        Ok(Value::Void)
    }

    fn builtin_vector_length(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("vector-length", args.len(), 1)?;
        Ok(Value::Int(
            Self::expect_vector(&args[0])?.borrow().len() as i64
        ))
    }

    fn builtin_vector_to_list(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("vector->list", args.len(), 1)?;
        let elements = Self::expect_vector(&args[0])?.borrow().clone();
        Ok(Self::make_list(&elements))
    }

    fn builtin_list_to_vector(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("list->vector", args.len(), 1)?;
        Ok(Value::Vector(Rc::new(RefCell::new(Self::list_elements(
            &args[0],
        )?))))
    }

    fn builtin_round(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("round", args.len(), 1)?;
        Ok(Self::expect_number(&args[0])?.to_value())
    }

    fn builtin_truncate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("truncate", args.len(), 1)?;
        Ok(Self::expect_number(&args[0])?.to_value())
    }

    fn builtin_gcd(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        let mut result = 0_i64;
        for arg in args {
            result = gcd(result, Self::expect_int(arg)?);
        }
        Ok(Value::Int(result.abs()))
    }

    fn builtin_lcm(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        let mut result = 1_i64;
        if args.is_empty() {
            return Ok(Value::Int(1));
        }
        for arg in args {
            result = lcm(result, Self::expect_int(arg)?);
        }
        Ok(Value::Int(result.abs()))
    }

    fn builtin_error(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::new("error"));
        }

        let mut message = Self::render_for_display(&args[0]);
        for arg in &args[1..] {
            if !message.is_empty() {
                message.push(' ');
            }
            message.push_str(&arg.render());
        }

        Err(EvalError::new(message))
    }

    fn builtin_values(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Ok(Self::pack_values(args.to_vec()))
    }

    fn builtin_call_with_values(
        interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("call-with-values", args.len(), 2)?;
        let produced = interpreter.call_value(args[0].clone(), Vec::new())?;
        interpreter.call_value(args[1].clone(), Self::unpack_values(produced))
    }

    fn builtin_dynamic_wind(
        interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("dynamic-wind", args.len(), 3)?;
        interpreter.call_value(args[0].clone(), Vec::new())?;

        match catch_signal(|| interpreter.call_value(args[1].clone(), Vec::new())) {
            Ok(result) => {
                let value = result?;
                interpreter.call_value(args[2].clone(), Vec::new())?;
                Ok(value)
            }
            Err(signal) => {
                interpreter.call_value(args[2].clone(), Vec::new())?;
                resume_signal(signal);
            }
        }
    }

    fn builtin_call_cc(
        interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("call/cc", args.len(), 1)?;

        let continuation_id = next_signal_id();
        let continuation = Value::Procedure(Rc::new(Procedure::Continuation(
            ContinuationProcedure {
                id: continuation_id,
            },
        )));

        match catch_signal(|| interpreter.call_value(args[0].clone(), vec![continuation])) {
            Ok(result) => result,
            Err(ControlSignal::ContinuationJump { id, values }) if id == continuation_id => {
                Ok(Self::pack_values(values))
            }
            Err(signal) => resume_signal(signal),
        }
    }

    fn builtin_call_with_current_continuation(
        interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::builtin_call_cc(interpreter, args)
    }

    fn builtin_raise(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("raise", args.len(), 1)?;
        signal_exception(args[0].clone());
    }

    fn builtin_with_exception_handler(
        interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("with-exception-handler", args.len(), 2)?;

        match catch_signal(|| interpreter.call_value(args[1].clone(), Vec::new())) {
            Ok(result) => result,
            Err(ControlSignal::Raised(exception)) => {
                interpreter.call_value(args[0].clone(), vec![exception.clone()])?;
                signal_exception(exception);
            }
            Err(signal) => resume_signal(signal),
        }
    }

    fn compare_numbers<F>(args: &[Value], predicate: F) -> Result<bool, EvalError>
    where
        F: Fn(ExactNumber, ExactNumber) -> bool,
    {
        Self::require_at_least("comparison", args.len(), 2)?;
        let mut previous = Self::expect_number(&args[0])?;
        for arg in &args[1..] {
            let current = Self::expect_number(arg)?;
            if !predicate(previous, current) {
                return Ok(false);
            }
            previous = current;
        }
        Ok(true)
    }

    fn numeric_predicate<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
    where
        F: Fn(ExactNumber) -> bool,
    {
        Self::require_arity(name, args.len(), 1)?;
        Ok(Value::Bool(predicate(Self::expect_number(&args[0])?)))
    }

    fn type_predicate<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
    where
        F: Fn(&Value) -> bool,
    {
        Self::require_arity(name, args.len(), 1)?;
        Ok(Value::Bool(predicate(&args[0])))
    }

    fn string_compare<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
    where
        F: Fn(&str, &str) -> bool,
    {
        Self::require_arity(name, args.len(), 2)?;
        Ok(Value::Bool(predicate(
            Self::expect_string(&args[0])?.as_plain_string(),
            Self::expect_string(&args[1])?.as_plain_string(),
        )))
    }

    fn assoc_like<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
    where
        F: Fn(&Value, &Value) -> bool,
    {
        Self::require_arity(name, args.len(), 2)?;
        let mut current = args[1].clone();
        let mut seen = HashSet::new();

        loop {
            match current {
                Value::Pair(pair) => {
                    let id = Rc::as_ptr(&pair) as usize;
                    if !seen.insert(id) {
                        return Err(EvalError::new("expected list"));
                    }
                    let entry = pair.car();
                    let entry_pair = Self::expect_pair(&entry)?;
                    if predicate(&args[0], &entry_pair.car()) {
                        return Ok(entry);
                    }
                    current = pair.cdr();
                }
                Value::EmptyList => return Ok(Value::Bool(false)),
                _ => return Err(EvalError::new("expected list")),
            }
        }
    }

    fn member_like<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
    where
        F: Fn(&Value, &Value) -> bool,
    {
        Self::require_arity(name, args.len(), 2)?;
        let mut current = args[1].clone();
        let mut seen = HashSet::new();

        loop {
            match current.clone() {
                Value::Pair(pair) => {
                    let id = Rc::as_ptr(&pair) as usize;
                    if !seen.insert(id) {
                        return Err(EvalError::new("expected list"));
                    }
                    if predicate(&args[0], &pair.car()) {
                        return Ok(current);
                    }
                    current = pair.cdr();
                }
                Value::EmptyList => return Ok(Value::Bool(false)),
                _ => return Err(EvalError::new("expected list")),
            }
        }
    }

    fn pack_values(values: Vec<Value>) -> Value {
        if values.len() == 1 {
            values.into_iter().next().expect("single value missing")
        } else {
            Value::Multi(values)
        }
    }

    fn unpack_values(value: Value) -> Vec<Value> {
        match value {
            Value::Multi(values) => values,
            other => vec![other],
        }
    }

    fn expect_int(value: &Value) -> Result<i64, EvalError> {
        match value {
            Value::Int(number) => Ok(*number),
            Value::Rational(number) if number.is_integer() => Ok(number.numerator),
            _ => Err(EvalError::new("expected number")),
        }
    }

    fn expect_number(value: &Value) -> Result<ExactNumber, EvalError> {
        match value {
            Value::Int(number) => Ok(ExactNumber::from_int(*number)),
            Value::Rational(number) => Ok(*number),
            _ => Err(EvalError::new("expected number")),
        }
    }

    fn expect_index(value: &Value, operation_name: &str) -> Result<usize, EvalError> {
        let index = Self::expect_int(value)?;
        if index < 0 {
            return Err(EvalError::new(format!(
                "{operation_name} index out of range"
            )));
        }
        Ok(index as usize)
    }

    fn expect_string(value: &Value) -> Result<SchemeString, EvalError> {
        match value {
            Value::String(string) => Ok(string.clone()),
            _ => Err(EvalError::new("expected string")),
        }
    }

    fn expect_symbol(value: &Value) -> Result<&str, EvalError> {
        match value {
            Value::Symbol(symbol) => Ok(symbol),
            _ => Err(EvalError::new("expected symbol")),
        }
    }

    fn expect_char(value: &Value) -> Result<char, EvalError> {
        match value {
            Value::Char(ch) => Ok(*ch),
            _ => Err(EvalError::new("expected character")),
        }
    }

    fn expect_pair(value: &Value) -> Result<Rc<PairCell>, EvalError> {
        match value {
            Value::Pair(pair) => Ok(pair.clone()),
            _ => Err(EvalError::new("expected pair")),
        }
    }

    fn expect_record(
        value: &Value,
        expected_type: &Rc<RecordType>,
    ) -> Result<Rc<RecordInstance>, EvalError> {
        match value {
            Value::Record(record) if Rc::ptr_eq(&record.record_type, expected_type) => {
                Ok(record.clone())
            }
            _ => Err(EvalError::new("expected record")),
        }
    }

    fn expect_vector(value: &Value) -> Result<Rc<RefCell<Vec<Value>>>, EvalError> {
        match value {
            Value::Vector(vector) => Ok(vector.clone()),
            _ => Err(EvalError::new("expected vector")),
        }
    }

    fn make_list(values: &[Value]) -> Value {
        let mut result = Value::EmptyList;
        for value in values.iter().rev() {
            result = Value::Pair(PairCell::new_rc(value.clone(), result));
        }
        result
    }

    fn list_length(value: &Value) -> Result<usize, EvalError> {
        let mut length = 0;
        let mut current = value.clone();
        let mut seen = HashSet::new();

        loop {
            match current {
                Value::Pair(pair) => {
                    let id = Rc::as_ptr(&pair) as usize;
                    if !seen.insert(id) {
                        return Err(EvalError::new("expected list"));
                    }
                    length += 1;
                    current = pair.cdr();
                }
                Value::EmptyList => return Ok(length),
                _ => return Err(EvalError::new("expected list")),
            }
        }
    }

    fn list_elements(value: &Value) -> Result<Vec<Value>, EvalError> {
        let mut elements = Vec::new();
        let mut current = value.clone();
        let mut seen = HashSet::new();

        loop {
            match current {
                Value::Pair(pair) => {
                    let id = Rc::as_ptr(&pair) as usize;
                    if !seen.insert(id) {
                        return Err(EvalError::new("expected list"));
                    }
                    elements.push(pair.car());
                    current = pair.cdr();
                }
                Value::EmptyList => return Ok(elements),
                _ => return Err(EvalError::new("expected list")),
            }
        }
    }

    fn list_tail_value(value: &Value, index: usize) -> Result<Value, EvalError> {
        let mut current = value.clone();
        let mut seen = HashSet::new();

        for _ in 0..index {
            match current {
                Value::Pair(pair) => {
                    let id = Rc::as_ptr(&pair) as usize;
                    if !seen.insert(id) {
                        return Err(EvalError::new("expected list"));
                    }
                    current = pair.cdr();
                }
                _ => return Err(EvalError::new("expected list")),
            }
        }

        Ok(current)
    }

    fn is_proper_list(value: &Value) -> bool {
        let mut slow = value.clone();
        let mut fast = value.clone();

        loop {
            fast = match fast {
                Value::EmptyList => return true,
                Value::Pair(pair) => pair.cdr(),
                _ => return false,
            };

            fast = match fast {
                Value::EmptyList => return true,
                Value::Pair(pair) => pair.cdr(),
                _ => return false,
            };

            slow = match slow {
                Value::EmptyList => return true,
                Value::Pair(pair) => pair.cdr(),
                _ => return false,
            };

            if let (Value::Pair(slow_pair), Value::Pair(fast_pair)) = (&slow, &fast) {
                if Rc::ptr_eq(slow_pair, fast_pair) {
                    return false;
                }
            }
        }
    }

    fn eq_values(left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Int(left), Value::Int(right)) => left == right,
            (Value::Rational(left), Value::Rational(right)) => left == right,
            (Value::Int(left), Value::Rational(right))
            | (Value::Rational(right), Value::Int(left)) => {
                ExactNumber::from_int(*left) == *right
            }
            (Value::Bool(left), Value::Bool(right)) => left == right,
            (Value::Char(left), Value::Char(right)) => left == right,
            (Value::Symbol(left), Value::Symbol(right)) => left == right,
            (Value::EmptyList, Value::EmptyList) => true,
            (Value::String(left), Value::String(right)) => Rc::ptr_eq(&left.0, &right.0),
            (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
            (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(left, right),
            (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
            (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
            (Value::Void, Value::Void) => true,
            (Value::Uninitialized, Value::Uninitialized) => true,
            _ => false,
        }
    }

    fn eqv_values(left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::String(left), Value::String(right)) => Rc::ptr_eq(&left.0, &right.0),
            _ => Self::eq_values(left, right),
        }
    }

    fn equal_values(left: &Value, right: &Value, seen: &mut HashSet<(usize, usize)>) -> bool {
        match (left, right) {
            (Value::Int(left), Value::Int(right)) => left == right,
            (Value::Rational(left), Value::Rational(right)) => left == right,
            (Value::Int(left), Value::Rational(right))
            | (Value::Rational(right), Value::Int(left)) => {
                ExactNumber::from_int(*left) == *right
            }
            (Value::Bool(left), Value::Bool(right)) => left == right,
            (Value::String(left), Value::String(right)) => {
                left.as_plain_string() == right.as_plain_string()
            }
            (Value::Char(left), Value::Char(right)) => left == right,
            (Value::Symbol(left), Value::Symbol(right)) => left == right,
            (Value::EmptyList, Value::EmptyList) => true,
            (Value::Pair(left), Value::Pair(right)) => {
                let key = (Rc::as_ptr(left) as usize, Rc::as_ptr(right) as usize);
                if !seen.insert(key) {
                    return true;
                }
                Self::equal_values(&left.car(), &right.car(), seen)
                    && Self::equal_values(&left.cdr(), &right.cdr(), seen)
            }
            (Value::Vector(left), Value::Vector(right)) => {
                let key = (Rc::as_ptr(left) as usize, Rc::as_ptr(right) as usize);
                if !seen.insert(key) {
                    return true;
                }
                let left_values = left.borrow();
                let right_values = right.borrow();
                left_values.len() == right_values.len()
                    && left_values
                        .iter()
                        .zip(right_values.iter())
                        .all(|(left, right)| Self::equal_values(left, right, seen))
            }
            (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
            (Value::Multi(left), Value::Multi(right)) => {
                left.len() == right.len()
                    && left
                        .iter()
                        .zip(right.iter())
                        .all(|(left, right)| Self::equal_values(left, right, seen))
            }
            (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
            (Value::Void, Value::Void) => true,
            (Value::Uninitialized, Value::Uninitialized) => true,
            _ => false,
        }
    }

    fn require_arity(name: &str, actual: usize, expected: usize) -> Result<(), EvalError> {
        if actual == expected {
            Ok(())
        } else {
            Err(EvalError::new(format!(
                "wrong number of arguments for {name}: expected {expected}, got {actual}"
            )))
        }
    }

    fn require_at_least(name: &str, actual: usize, minimum: usize) -> Result<(), EvalError> {
        if actual >= minimum {
            Ok(())
        } else {
            Err(EvalError::new(format!(
                "wrong number of arguments for {name}: expected at least {minimum}, got {actual}"
            )))
        }
    }
}

enum Step {
    Value(Value),
    Tail(Expr, Rc<Environment>),
}

struct ActiveGuard {
    depth: usize,
    position: SourcePos,
    variable_name: String,
    clauses: Vec<Expr>,
    env: Rc<Environment>,
}

enum GuardResolution {
    Value(Value),
    Tail(Expr, Rc<Environment>),
    Rethrow(Value),
}

#[derive(Clone)]
struct LetBinding {
    name: String,
    value_expr: Expr,
}

#[derive(Clone)]
struct DoBinding {
    name: String,
    init_expr: Expr,
    step_expr: Option<Expr>,
}

#[derive(Clone)]
struct ParameterSpec {
    required_names: Vec<String>,
    rest_name: Option<String>,
}

impl ParameterSpec {
    fn validate_arity(&self, name: &str, actual: usize) -> Result<(), EvalError> {
        if self.rest_name.is_some() {
            Interpreter::require_at_least(name, actual, self.required_names.len())
        } else {
            Interpreter::require_arity(name, actual, self.required_names.len())
        }
    }
}

#[derive(Copy, Clone)]
struct SourcePos {
    line: usize,
    column: usize,
}

#[derive(Clone)]
struct Expr(Rc<ExprNode>);

impl Expr {
    fn new(kind: ExprKind, position: SourcePos) -> Self {
        Self(Rc::new(ExprNode { kind, position }))
    }

    fn position(&self) -> SourcePos {
        self.0.position
    }

    fn kind(&self) -> &ExprKind {
        &self.0.kind
    }

    fn symbol_name(&self) -> Option<&str> {
        match &self.0.kind {
            ExprKind::Symbol(name) => Some(name),
            _ => None,
        }
    }
}

struct ExprNode {
    kind: ExprKind,
    position: SourcePos,
}

#[derive(Clone)]
enum ExprKind {
    Int(i64),
    Rational(ExactNumber),
    Bool(bool),
    String(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
    DottedList(Vec<Expr>, Expr),
}

#[derive(Clone)]
enum Value {
    Int(i64),
    Rational(ExactNumber),
    Bool(bool),
    String(SchemeString),
    Char(char),
    Symbol(String),
    Pair(Rc<PairCell>),
    EmptyList,
    Vector(Rc<RefCell<Vec<Value>>>),
    Record(Rc<RecordInstance>),
    Multi(Vec<Value>),
    Void,
    Procedure(Rc<Procedure>),
    Uninitialized,
}

impl Value {
    fn render(&self) -> String {
        let mut seen_pairs = HashSet::new();
        let mut seen_vectors = HashSet::new();
        self.render_with_seen(&mut seen_pairs, &mut seen_vectors)
    }

    fn render_with_seen(
        &self,
        seen_pairs: &mut HashSet<usize>,
        seen_vectors: &mut HashSet<usize>,
    ) -> String {
        match self {
            Value::Int(value) => value.to_string(),
            Value::Rational(value) => value.render(),
            Value::Bool(true) => "#t".to_owned(),
            Value::Bool(false) => "#f".to_owned(),
            Value::String(value) => format!("\"{}\"", escape_string(value.as_plain_string())),
            Value::Char(value) => render_char(*value),
            Value::Symbol(name) => name.clone(),
            Value::Pair(pair) => render_pair(pair, seen_pairs, seen_vectors),
            Value::EmptyList => "()".to_owned(),
            Value::Vector(vector) => {
                let id = Rc::as_ptr(vector) as usize;
                if !seen_vectors.insert(id) {
                    return "#<cycle>".to_owned();
                }
                let rendered = vector
                    .borrow()
                    .iter()
                    .map(|value| value.render_with_seen(seen_pairs, seen_vectors))
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("#({rendered})")
            }
            Value::Record(record) => format!("#<record:{}>", record.record_type.name),
            Value::Multi(_) => "#<values>".to_owned(),
            Value::Void => "#<void>".to_owned(),
            Value::Procedure(procedure) => procedure.render(),
            Value::Uninitialized => "#<uninitialized>".to_owned(),
        }
    }
}

#[derive(Clone)]
struct SchemeString(Rc<String>);

impl SchemeString {
    fn new(value: &str) -> Self {
        Self(Rc::new(value.to_owned()))
    }

    fn new_owned(value: String) -> Self {
        Self(Rc::new(value))
    }

    fn as_plain_string(&self) -> &str {
        &self.0
    }

    fn char_len(&self) -> usize {
        self.0.chars().count()
    }

    fn characters(&self) -> Vec<char> {
        self.0.chars().collect()
    }

    fn copy(&self) -> Self {
        Self::new_owned(self.as_plain_string().to_owned())
    }
}

struct PairCell {
    car: RefCell<Value>,
    cdr: RefCell<Value>,
}

impl PairCell {
    fn new_rc(car: Value, cdr: Value) -> Rc<Self> {
        Rc::new(Self {
            car: RefCell::new(car),
            cdr: RefCell::new(cdr),
        })
    }

    fn car(&self) -> Value {
        self.car.borrow().clone()
    }

    fn cdr(&self) -> Value {
        self.cdr.borrow().clone()
    }

    fn set_car(&self, value: Value) {
        *self.car.borrow_mut() = value;
    }

    fn set_cdr(&self, value: Value) {
        *self.cdr.borrow_mut() = value;
    }
}

type BuiltinAction = fn(&mut Interpreter, &[Value]) -> Result<Value, EvalError>;

#[derive(Clone)]
enum Procedure {
    Builtin(BuiltinProcedure),
    User(UserProcedure),
    Continuation(ContinuationProcedure),
    RecordConstructor(RecordConstructorProcedure),
    RecordPredicate(RecordPredicateProcedure),
    RecordAccessor(RecordAccessorProcedure),
    RecordMutator(RecordMutatorProcedure),
}

impl Procedure {
    fn render(&self) -> String {
        match self {
            Procedure::Builtin(builtin) => format!("#<procedure:{}>", builtin.name),
            Procedure::User(_) => "#<procedure>".to_owned(),
            Procedure::Continuation(_) => "#<procedure>".to_owned(),
            Procedure::RecordConstructor(procedure) => format!("#<procedure:{}>", procedure.name),
            Procedure::RecordPredicate(procedure) => format!("#<procedure:{}>", procedure.name),
            Procedure::RecordAccessor(procedure) => format!("#<procedure:{}>", procedure.name),
            Procedure::RecordMutator(procedure) => format!("#<procedure:{}>", procedure.name),
        }
    }
}

#[derive(Clone)]
struct BuiltinProcedure {
    name: &'static str,
    action: BuiltinAction,
}

#[derive(Clone)]
struct UserProcedure {
    name: Option<String>,
    params: ParameterSpec,
    body: Vec<Expr>,
    closure_env: Rc<Environment>,
}

impl UserProcedure {
    fn display_name(&self) -> &str {
        self.name.as_deref().unwrap_or("lambda")
    }
}

#[derive(Clone)]
struct ContinuationProcedure {
    id: u64,
}

#[derive(Clone)]
struct RecordType {
    name: String,
    field_names: Vec<String>,
    field_indexes: HashMap<String, usize>,
}

impl RecordType {
    fn new(name: String, fields: &[RecordFieldSpec]) -> Result<Self, EvalError> {
        let mut field_names = Vec::with_capacity(fields.len());
        let mut field_indexes = HashMap::with_capacity(fields.len());

        for (index, field) in fields.iter().enumerate() {
            if field_indexes
                .insert(field.field_name.clone(), index)
                .is_some()
            {
                return Err(EvalError::new(format!(
                    "duplicate record field: {}",
                    field.field_name
                )));
            }
            field_names.push(field.field_name.clone());
        }

        Ok(Self {
            name,
            field_names,
            field_indexes,
        })
    }
}

struct RecordInstance {
    record_type: Rc<RecordType>,
    fields: RefCell<Vec<Value>>,
}

impl RecordInstance {
    fn new(record_type: Rc<RecordType>, fields: Vec<Value>) -> Rc<Self> {
        Rc::new(Self {
            record_type,
            fields: RefCell::new(fields),
        })
    }

    fn field(&self, index: usize) -> Value {
        self.fields.borrow()[index].clone()
    }

    fn set_field(&self, index: usize, value: Value) {
        self.fields.borrow_mut()[index] = value;
    }
}

#[derive(Clone)]
struct RecordConstructorProcedure {
    name: String,
    record_type: Rc<RecordType>,
    field_indexes: Vec<usize>,
}

impl RecordConstructorProcedure {
    fn apply(&self, args: &[Value]) -> Result<Value, EvalError> {
        Interpreter::require_arity(&self.name, args.len(), self.field_indexes.len())?;

        let mut fields = vec![Value::Void; self.record_type.field_names.len()];
        for (index, field_index) in self.field_indexes.iter().copied().enumerate() {
            fields[field_index] = args[index].clone();
        }

        Ok(Value::Record(RecordInstance::new(
            self.record_type.clone(),
            fields,
        )))
    }
}

#[derive(Clone)]
struct RecordPredicateProcedure {
    name: String,
    record_type: Rc<RecordType>,
}

impl RecordPredicateProcedure {
    fn apply(&self, args: &[Value]) -> Result<Value, EvalError> {
        Interpreter::require_arity(&self.name, args.len(), 1)?;
        Ok(Value::Bool(matches!(
            &args[0],
            Value::Record(record) if Rc::ptr_eq(&record.record_type, &self.record_type)
        )))
    }
}

#[derive(Clone)]
struct RecordAccessorProcedure {
    name: String,
    record_type: Rc<RecordType>,
    field_index: usize,
}

impl RecordAccessorProcedure {
    fn apply(&self, args: &[Value]) -> Result<Value, EvalError> {
        Interpreter::require_arity(&self.name, args.len(), 1)?;
        let record = Interpreter::expect_record(&args[0], &self.record_type)?;
        Ok(record.field(self.field_index))
    }
}

#[derive(Clone)]
struct RecordMutatorProcedure {
    name: String,
    record_type: Rc<RecordType>,
    field_index: usize,
}

impl RecordMutatorProcedure {
    fn apply(&self, args: &[Value]) -> Result<Value, EvalError> {
        Interpreter::require_arity(&self.name, args.len(), 2)?;
        let record = Interpreter::expect_record(&args[0], &self.record_type)?;
        record.set_field(self.field_index, args[1].clone());
        Ok(Value::Void)
    }
}

#[derive(Clone)]
struct RecordConstructorSpec {
    name: String,
    field_names: Vec<String>,
}

#[derive(Clone)]
struct RecordFieldSpec {
    field_name: String,
    accessor_name: String,
    mutator_name: Option<String>,
}

#[derive(Clone)]
enum MacroBinding {
    SyntaxRules(SyntaxRulesMacro),
}

impl MacroBinding {
    fn expand(&self, position: SourcePos, elements: &[Expr]) -> Result<Expr, EvalError> {
        match self {
            Self::SyntaxRules(macro_binding) => macro_binding.expand(position, elements),
        }
    }
}

struct Environment {
    parent: Option<Rc<Environment>>,
    bindings: RefCell<HashMap<String, Rc<RefCell<Value>>>>,
    syntax_bindings: RefCell<HashMap<String, Rc<MacroBinding>>>,
}

impl Environment {
    fn new(parent: Option<Rc<Environment>>) -> Rc<Self> {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
            syntax_bindings: RefCell::new(HashMap::new()),
        })
    }

    fn define_value(&self, name: String, value: Value) {
        if let Some(cell) = self.bindings.borrow().get(&name) {
            *cell.borrow_mut() = value;
            return;
        }
        self.bindings
            .borrow_mut()
            .insert(name, Rc::new(RefCell::new(value)));
    }

    fn define_placeholder(&self, name: String) -> Rc<RefCell<Value>> {
        let cell = Rc::new(RefCell::new(Value::Uninitialized));
        self.bindings.borrow_mut().insert(name, cell.clone());
        cell
    }

    fn lookup(&self, name: &str) -> Result<Value, EvalError> {
        let cell = self
            .lookup_cell(name)
            .ok_or_else(|| EvalError::new(format!("unbound variable: {name}")))?;
        let value = cell.borrow().clone();
        if matches!(value, Value::Uninitialized) {
            Err(EvalError::new(format!("uninitialized variable: {name}")))
        } else {
            Ok(value)
        }
    }

    fn set(&self, name: &str, value: Value) -> Result<(), EvalError> {
        let cell = self
            .lookup_cell(name)
            .ok_or_else(|| EvalError::new(format!("unbound variable: {name}")))?;
        *cell.borrow_mut() = value;
        Ok(())
    }

    fn define_syntax(&self, name: String, macro_binding: SyntaxRulesMacro) {
        self.syntax_bindings.borrow_mut().insert(
            name,
            Rc::new(MacroBinding::SyntaxRules(macro_binding)),
        );
    }

    fn lookup_syntax(&self, name: &str) -> Option<Rc<MacroBinding>> {
        if let Some(binding) = self.syntax_bindings.borrow().get(name) {
            return Some(binding.clone());
        }
        self.parent.as_ref()?.lookup_syntax(name)
    }

    fn lookup_cell(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        if let Some(value) = self.bindings.borrow().get(name) {
            return Some(value.clone());
        }
        self.parent.as_ref()?.lookup_cell(name)
    }
}

fn builtin_value(name: &'static str, action: BuiltinAction) -> Value {
    Value::Procedure(Rc::new(Procedure::Builtin(BuiltinProcedure {
        name,
        action,
    })))
}

#[derive(Copy, Clone, PartialEq, Eq)]
struct ExactNumber {
    numerator: i64,
    denominator: i64,
}

impl ExactNumber {
    fn new(numerator: i64, denominator: i64) -> Result<Self, EvalError> {
        if denominator == 0 {
            return Err(EvalError::new("division by zero"));
        }

        if numerator == 0 {
            return Ok(Self {
                numerator: 0,
                denominator: 1,
            });
        }

        let mut numerator = numerator;
        let mut denominator = denominator;
        if denominator < 0 {
            numerator = -numerator;
            denominator = -denominator;
        }

        let divisor = gcd(numerator, denominator);
        Ok(Self {
            numerator: numerator / divisor,
            denominator: denominator / divisor,
        })
    }

    fn from_int(value: i64) -> Self {
        Self {
            numerator: value,
            denominator: 1,
        }
    }

    fn add(self, other: Self) -> Result<Self, EvalError> {
        Self::new(
            self.numerator * other.denominator + other.numerator * self.denominator,
            self.denominator * other.denominator,
        )
    }

    fn subtract(self, other: Self) -> Result<Self, EvalError> {
        Self::new(
            self.numerator * other.denominator - other.numerator * self.denominator,
            self.denominator * other.denominator,
        )
    }

    fn multiply(self, other: Self) -> Result<Self, EvalError> {
        Self::new(
            self.numerator * other.numerator,
            self.denominator * other.denominator,
        )
    }

    fn divide(self, other: Self) -> Result<Self, EvalError> {
        if other.numerator == 0 {
            return Err(EvalError::new("division by zero"));
        }
        Self::new(
            self.numerator * other.denominator,
            self.denominator * other.numerator,
        )
    }

    fn abs(self) -> Self {
        Self {
            numerator: self.numerator.abs(),
            denominator: self.denominator,
        }
    }

    fn is_integer(self) -> bool {
        self.denominator == 1
    }

    fn cmp(self, other: Self) -> Ordering {
        (self.numerator * other.denominator).cmp(&(other.numerator * self.denominator))
    }

    fn render(self) -> String {
        if self.denominator == 1 {
            self.numerator.to_string()
        } else {
            format!("{}/{}", self.numerator, self.denominator)
        }
    }

    fn to_value(self) -> Value {
        if self.denominator == 1 {
            Value::Int(self.numerator)
        } else {
            Value::Rational(self)
        }
    }
}

#[derive(Clone)]
enum ControlSignal {
    Raised(Value),
    ContinuationJump { id: u64, values: Vec<Value> },
}

thread_local! {
    static NEXT_SIGNAL_ID: Cell<u64> = const { Cell::new(1) };
    static SIGNALS: RefCell<HashMap<u64, ControlSignal>> = RefCell::new(HashMap::new());
}

fn next_signal_id() -> u64 {
    NEXT_SIGNAL_ID.with(|next| {
        let id = next.get();
        next.set(id + 1);
        id
    })
}

fn catch_signal<F, T>(operation: F) -> Result<T, ControlSignal>
where
    F: FnOnce() -> T,
{
    match panic::catch_unwind(AssertUnwindSafe(operation)) {
        Ok(result) => Ok(result),
        Err(payload) => match payload.downcast::<u64>() {
            Ok(id) => {
                let Some(signal) = SIGNALS.with(|signals| signals.borrow_mut().remove(&*id)) else {
                    panic::resume_unwind(Box::new(*id));
                };
                Err(signal)
            }
            Err(payload) => panic::resume_unwind(payload),
        },
    }
}

fn signal_with(signal: ControlSignal) -> ! {
    let id = next_signal_id();
    SIGNALS.with(|signals| {
        signals.borrow_mut().insert(id, signal);
    });
    panic::panic_any(id);
}

fn signal_exception(exception: Value) -> ! {
    signal_with(ControlSignal::Raised(exception))
}

fn signal_continuation_jump(id: u64, values: Vec<Value>) -> ! {
    signal_with(ControlSignal::ContinuationJump { id, values })
}

fn resume_signal(signal: ControlSignal) -> ! {
    signal_with(signal)
}

#[derive(Clone)]
struct SyntaxRulesMacro {
    name: String,
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
}

#[derive(Clone)]
struct MacroRule {
    pattern: Expr,
    template: Expr,
}

impl SyntaxRulesMacro {
    fn compile(name: &str, transformer_expr: &Expr) -> Result<Self, EvalError> {
        let parts = match transformer_expr.kind() {
            ExprKind::List(parts) => parts,
            _ => {
                return Err(EvalError::new(
                    "define-syntax transformer must be a syntax-rules form",
                ))
            }
        };

        if parts.len() < 3 || parts[0].symbol_name() != Some("syntax-rules") {
            return Err(EvalError::new(
                "define-syntax transformer must be a syntax-rules form",
            ));
        }

        let literal_exprs = Self::expect_list(
            &parts[1],
            "syntax-rules literals must be a list",
        )?;
        let mut literals = HashSet::with_capacity(literal_exprs.len());
        for literal_expr in literal_exprs {
            let Some(literal) = literal_expr.symbol_name() else {
                return Err(EvalError::new("syntax-rules literal must be a symbol"));
            };
            literals.insert(literal.to_owned());
        }

        let mut rules = Vec::with_capacity(parts.len() - 2);
        for rule_expr in &parts[2..] {
            let rule_parts =
                Self::expect_list(rule_expr, "syntax-rules rule must be a list")?;
            if rule_parts.len() != 2 {
                return Err(EvalError::new(
                    "syntax-rules rule must contain a pattern and template",
                ));
            }
            rules.push(MacroRule {
                pattern: rule_parts[0].clone(),
                template: rule_parts[1].clone(),
            });
        }

        Ok(Self {
            name: name.to_owned(),
            literals,
            rules,
        })
    }

    fn expand(&self, position: SourcePos, elements: &[Expr]) -> Result<Expr, EvalError> {
        let invocation = Expr::new(ExprKind::List(elements.to_vec()), position);

        for rule in &self.rules {
            let mut bindings = HashMap::new();
            if self.match_pattern(&rule.pattern, &invocation, &mut bindings)? {
                return Ok(self.expand_template(&rule.template, &bindings));
            }
        }

        Err(EvalError::new(format!(
            "no matching rule for macro: {}",
            self.name
        )))
    }

    fn expect_list<'a>(expr: &'a Expr, error_message: &str) -> Result<&'a [Expr], EvalError> {
        match expr.kind() {
            ExprKind::List(items) => Ok(items),
            _ => Err(EvalError::new(error_message)),
        }
    }

    fn match_pattern(
        &self,
        pattern: &Expr,
        input: &Expr,
        bindings: &mut HashMap<String, Expr>,
    ) -> Result<bool, EvalError> {
        match (pattern.kind(), input.kind()) {
            (ExprKind::Int(left), ExprKind::Int(right)) => Ok(left == right),
            (ExprKind::Rational(left), ExprKind::Rational(right)) => Ok(left == right),
            (ExprKind::Bool(left), ExprKind::Bool(right)) => Ok(left == right),
            (ExprKind::String(left), ExprKind::String(right)) => Ok(left == right),
            (ExprKind::Char(left), ExprKind::Char(right)) => Ok(left == right),
            (ExprKind::Symbol(symbol), _) => self.match_pattern_symbol(symbol, input, bindings),
            (ExprKind::List(pattern_items), ExprKind::List(input_items)) => {
                if pattern_items.len() != input_items.len() {
                    return Ok(false);
                }

                for (pattern_item, input_item) in pattern_items.iter().zip(input_items) {
                    if !self.match_pattern(pattern_item, input_item, bindings)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (ExprKind::DottedList(pattern_items, pattern_tail), ExprKind::DottedList(input_items, input_tail)) => {
                if pattern_items.len() != input_items.len() {
                    return Ok(false);
                }
                for (pattern_item, input_item) in pattern_items.iter().zip(input_items) {
                    if !self.match_pattern(pattern_item, input_item, bindings)? {
                        return Ok(false);
                    }
                }
                self.match_pattern(pattern_tail, input_tail, bindings)
            }
            _ => Ok(false),
        }
    }

    fn match_pattern_symbol(
        &self,
        symbol: &str,
        input: &Expr,
        bindings: &mut HashMap<String, Expr>,
    ) -> Result<bool, EvalError> {
        if symbol == "_" {
            return Ok(true);
        }
        if self.literals.contains(symbol) {
            return Ok(input.symbol_name() == Some(symbol));
        }
        if let Some(existing) = bindings.get(symbol) {
            return Ok(expr_equal(existing, input));
        }

        bindings.insert(symbol.to_owned(), input.clone());
        Ok(true)
    }

    fn expand_template(&self, template: &Expr, bindings: &HashMap<String, Expr>) -> Expr {
        match template.kind() {
            ExprKind::Symbol(symbol) => bindings
                .get(symbol)
                .cloned()
                .unwrap_or_else(|| template.clone()),
            ExprKind::List(items) => Expr::new(
                ExprKind::List(
                    items.iter()
                        .map(|item| self.expand_template(item, bindings))
                        .collect(),
                ),
                template.position(),
            ),
            ExprKind::DottedList(items, tail) => Expr::new(
                ExprKind::DottedList(
                    items.iter()
                        .map(|item| self.expand_template(item, bindings))
                        .collect(),
                    self.expand_template(tail, bindings),
                ),
                template.position(),
            ),
            _ => template.clone(),
        }
    }
}

fn expr_equal(left: &Expr, right: &Expr) -> bool {
    match (left.kind(), right.kind()) {
        (ExprKind::Int(left), ExprKind::Int(right)) => left == right,
        (ExprKind::Rational(left), ExprKind::Rational(right)) => left == right,
        (ExprKind::Bool(left), ExprKind::Bool(right)) => left == right,
        (ExprKind::String(left), ExprKind::String(right)) => left == right,
        (ExprKind::Char(left), ExprKind::Char(right)) => left == right,
        (ExprKind::Symbol(left), ExprKind::Symbol(right)) => left == right,
        (ExprKind::List(left), ExprKind::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| expr_equal(left, right))
        }
        (ExprKind::DottedList(left_items, left_tail), ExprKind::DottedList(right_items, right_tail)) => {
            left_items.len() == right_items.len()
                && left_items
                    .iter()
                    .zip(right_items.iter())
                    .all(|(left, right)| expr_equal(left, right))
                && expr_equal(left_tail, right_tail)
        }
        _ => false,
    }
}

fn render_pair(
    pair: &Rc<PairCell>,
    seen_pairs: &mut HashSet<usize>,
    seen_vectors: &mut HashSet<usize>,
) -> String {
    let mut result = String::from("(");
    let mut current = Value::Pair(pair.clone());
    let mut first = true;

    loop {
        match current {
            Value::Pair(next_pair) => {
                let id = Rc::as_ptr(&next_pair) as usize;
                if !seen_pairs.insert(id) {
                    if !first {
                        result.push(' ');
                    }
                    result.push_str("#<cycle>");
                    break;
                }

                if !first {
                    result.push(' ');
                }
                result.push_str(&next_pair.car().render_with_seen(seen_pairs, seen_vectors));
                current = next_pair.cdr();
                first = false;
            }
            Value::EmptyList => break,
            other => {
                if !first {
                    result.push_str(" . ");
                }
                result.push_str(&other.render_with_seen(seen_pairs, seen_vectors));
                break;
            }
        }
    }

    result.push(')');
    result
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".to_owned(),
        '\n' => "#\\newline".to_owned(),
        _ => format!("#\\{value}"),
    }
}

fn escape_string(value: &str) -> String {
    let mut builder = String::with_capacity(value.len());
    for ch in value.chars() {
        match ch {
            '\\' => builder.push_str("\\\\"),
            '"' => builder.push_str("\\\""),
            '\n' => builder.push_str("\\n"),
            '\t' => builder.push_str("\\t"),
            '\r' => builder.push_str("\\r"),
            _ => builder.push(ch),
        }
    }
    builder
}

fn gcd(left: i64, right: i64) -> i64 {
    let mut a = left.abs();
    let mut b = right.abs();
    while b != 0 {
        let next = a % b;
        a = b;
        b = next;
    }
    a
}

fn lcm(left: i64, right: i64) -> i64 {
    if left == 0 || right == 0 {
        0
    } else {
        (left / gcd(left, right)) * right
    }
}

struct Parser<'a> {
    input: &'a str,
    index: usize,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            index: 0,
            line: 1,
            column: 1,
        }
    }

    fn has_more(&mut self) -> bool {
        self.skip_whitespace();
        self.index < self.input.len()
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace();
        if self.index >= self.input.len() {
            return Err(self.error_at_current("unexpected end of input"));
        }

        let position = self.current_position();
        match self.current_char() {
            Some('(') => self.parse_list(position),
            Some('\'') => self.parse_quote(position),
            Some('"') => self.parse_string(position),
            Some(')') => Err(self.error_at(position, "unexpected ')'")),
            Some(_) => self.parse_atom(position),
            None => Err(self.error_at_current("unexpected end of input")),
        }
    }

    fn parse_list(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.advance();
        let mut elements = Vec::new();

        loop {
            self.skip_whitespace();
            if self.index >= self.input.len() {
                return Err(self.error_at(position, "unterminated list"));
            }

            match self.current_char() {
                Some(')') => {
                    self.advance();
                    return Ok(Expr::new(ExprKind::List(elements), position));
                }
                Some('.') if self.is_dotted_tail_marker() => {
                    if elements.is_empty() {
                        return Err(self.error_at(position, "invalid dotted list"));
                    }
                    self.advance();
                    let tail = self.parse_expr()?;
                    self.skip_whitespace();
                    if self.current_char() != Some(')') {
                        return Err(self.error_at(position, "invalid dotted list"));
                    }
                    self.advance();
                    return Ok(Expr::new(ExprKind::DottedList(elements, tail), position));
                }
                _ => elements.push(self.parse_expr()?),
            }
        }
    }

    fn parse_quote(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.advance();
        Ok(Expr::new(
            ExprKind::List(vec![
                Expr::new(ExprKind::Symbol("quote".to_owned()), position),
                self.parse_expr()?,
            ]),
            position,
        ))
    }

    fn parse_string(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.advance();
        let mut value = String::new();

        while let Some(ch) = self.current_char() {
            self.advance();
            match ch {
                '"' => return Ok(Expr::new(ExprKind::String(value), position)),
                '\\' => value.push(self.parse_escape(position)?),
                other => value.push(other),
            }
        }

        Err(self.error_at(position, "unterminated string"))
    }

    fn parse_escape(&mut self, position: SourcePos) -> Result<char, EvalError> {
        let Some(ch) = self.current_char() else {
            return Err(self.error_at(position, "unterminated string escape"));
        };
        self.advance();
        Ok(match ch {
            '\\' => '\\',
            '"' => '"',
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            other => other,
        })
    }

    fn parse_atom(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        let start = self.index;
        while let Some(ch) = self.current_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.advance();
        }

        let token = &self.input[start..self.index];
        if token == "#t" {
            return Ok(Expr::new(ExprKind::Bool(true), position));
        }
        if token == "#f" {
            return Ok(Expr::new(ExprKind::Bool(false), position));
        }
        if let Some(ch) = parse_char_literal(token) {
            return Ok(Expr::new(ExprKind::Char(ch), position));
        }
        if token.starts_with("#\\") {
            return Err(self.error_at(position, "invalid character literal"));
        }
        if let Ok(value) = token.parse::<i64>() {
            return Ok(Expr::new(ExprKind::Int(value), position));
        }
        if let Some(value) = parse_exact_number_literal(token) {
            return Ok(Expr::new(ExprKind::Rational(value), position));
        }
        Ok(Expr::new(ExprKind::Symbol(token.to_owned()), position))
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char() {
            if ch.is_whitespace() {
                self.advance();
                continue;
            }

            if ch == ';' {
                self.advance();
                while let Some(comment_ch) = self.current_char() {
                    if comment_ch == '\n' {
                        break;
                    }
                    self.advance();
                }
                continue;
            }

            break;
        }
    }

    fn is_dotted_tail_marker(&self) -> bool {
        if self.current_char() != Some('.') {
            return false;
        }
        match self.peek_char() {
            None => true,
            Some(ch) => ch.is_whitespace() || matches!(ch, ')' | ';'),
        }
    }

    fn current_position(&self) -> SourcePos {
        SourcePos {
            line: self.line,
            column: self.column,
        }
    }

    fn error_at_current(&self, message: impl Into<String>) -> EvalError {
        let position = self.current_position();
        self.error_at(position, message)
    }

    fn error_at(&self, position: SourcePos, message: impl Into<String>) -> EvalError {
        EvalError::new(message).with_position(position.line, position.column)
    }

    fn current_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }

    fn peek_char(&self) -> Option<char> {
        let mut chars = self.input[self.index..].chars();
        chars.next()?;
        chars.next()
    }

    fn advance(&mut self) {
        let ch = self
            .current_char()
            .expect("advance called at end of parser input");
        self.index += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }
}

fn parse_char_literal(token: &str) -> Option<char> {
    let rest = token.strip_prefix("#\\")?;
    match rest {
        "space" => Some(' '),
        "newline" => Some('\n'),
        _ => {
            let mut chars = rest.chars();
            let ch = chars.next()?;
            if chars.next().is_none() {
                Some(ch)
            } else {
                None
            }
        }
    }
}

fn parse_exact_number_literal(token: &str) -> Option<ExactNumber> {
    let (numerator, denominator) = token.split_once('/')?;
    let numerator = numerator.parse::<i64>().ok()?;
    let denominator = denominator.parse::<i64>().ok()?;
    ExactNumber::new(numerator, denominator).ok()
}
