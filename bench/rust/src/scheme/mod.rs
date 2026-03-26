use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

pub mod error;

pub use error::EvalError;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    Evaluator::new().eval_str(input)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

struct Evaluator {
    global_env: Rc<Env>,
}

impl Evaluator {
    fn new() -> Self {
        Self {
            global_env: create_global_env(),
        }
    }

    fn eval_str(&self, input: &str) -> Result<String, EvalError> {
        let expressions = Parser::new(input).parse_program()?;
        if expressions.is_empty() {
            return Err(EvalError::with_position(
                "input did not contain any expressions",
                1,
                1,
            ));
        }

        let mut result = Value::Void;
        for expression in &expressions {
            result = self.eval(expression, self.global_env.clone())?;
        }
        Ok(render(&result))
    }

    fn eval(&self, expr: &Expr, env: Rc<Env>) -> Result<Value, EvalError> {
        let result = match &expr.kind {
            ExprKind::Int(value) => Ok(Value::Int(*value)),
            ExprKind::Rational(numerator, denominator) => {
                Ok(exact_value(ExactNumber::new(*numerator, *denominator)))
            }
            ExprKind::Inexact(value) => Ok(Value::Inexact(*value)),
            ExprKind::Bool(value) => Ok(Value::Bool(*value)),
            ExprKind::String(value) => Ok(Value::String(value.clone())),
            ExprKind::Symbol(name) => env.lookup(name),
            ExprKind::Vector(elements) => {
                Ok(vector_value(elements.iter().map(quote_to_value).collect()))
            }
            ExprKind::List(elements) => self.eval_list(elements, env),
        };

        result.map_err(|error| ensure_position(error, expr.position))
    }

    fn eval_list(&self, elements: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        if elements.is_empty() {
            return Err(EvalError::message("cannot evaluate an empty list"));
        }

        let operator_expr = &elements[0];
        let arguments = &elements[1..];

        if let ExprKind::Symbol(name) = &operator_expr.kind {
            return match name.as_str() {
                "define" => self.eval_define(arguments, env),
                "define-record-type" => self.eval_define_record_type(arguments, env),
                "set!" => self.eval_set(arguments, env),
                "if" => self.eval_if(arguments, env),
                "quote" => self.eval_quote(arguments),
                "lambda" => self.eval_lambda(arguments, env),
                "and" => self.eval_and(arguments, env),
                "or" => self.eval_or(arguments, env),
                "begin" => self.eval_begin(arguments, env),
                "let" => self.eval_let(arguments, env),
                "letrec" => self.eval_letrec(arguments, env, false),
                "letrec*" => self.eval_letrec(arguments, env, true),
                "cond" => self.eval_cond(arguments, env),
                "case" => self.eval_case(arguments, env),
                "do" => self.eval_do(arguments, env),
                _ => {
                    let operator = self.eval(operator_expr, env.clone())?;
                    let values = self.eval_all(arguments, env)?;
                    self.apply_procedure(operator, values)
                }
            };
        }

        let operator = self.eval(operator_expr, env.clone())?;
        let values = self.eval_all(arguments, env)?;
        self.apply_procedure(operator, values)
    }

    fn eval_define(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(EvalError::message("define expects a name and a value"));
        }

        match &arguments[0].kind {
            ExprKind::Symbol(name) => {
                if arguments.len() != 2 {
                    return Err(EvalError::message(
                        "define expects exactly one value expression",
                    ));
                }

                let binding = env.define_placeholder(name.clone());
                let value = self.eval(&arguments[1], env)?;
                *binding.borrow_mut() = value;
                Ok(Value::Void)
            }
            ExprKind::List(signature) => {
                if signature.is_empty() {
                    return Err(EvalError::message("define function name must be a symbol"));
                }

                let ExprKind::Symbol(name) = &signature[0].kind else {
                    return Err(EvalError::message("define function name must be a symbol"));
                };

                if arguments.len() < 2 {
                    return Err(EvalError::message("define requires a function body"));
                }

                let binding = env.define_placeholder(name.clone());
                let closure = Value::Closure(Rc::new(ClosureValue {
                    parameters: parse_parameter_names_list(&signature[1..])?,
                    body: arguments[1..].to_vec(),
                    env: env.clone(),
                }));
                *binding.borrow_mut() = closure;
                Ok(Value::Void)
            }
            _ => Err(EvalError::message(
                "define target must be a symbol or parameter list",
            )),
        }
    }

    fn eval_set(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        require_exact_args("set!", arguments.len(), 2)?;

        let ExprKind::Symbol(name) = &arguments[0].kind else {
            return Err(EvalError::message("set! target must be a symbol"));
        };

        let value = self.eval(&arguments[1], env.clone())?;
        env.set(name, value)?;
        Ok(Value::Void)
    }

    fn eval_if(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        if arguments.len() != 2 && arguments.len() != 3 {
            return Err(EvalError::message("if expected 2 or 3 argument(s)"));
        }
        let condition = self.eval(&arguments[0], env.clone())?;
        if is_truthy(&condition) {
            self.eval(&arguments[1], env)
        } else if arguments.len() == 3 {
            self.eval(&arguments[2], env)
        } else {
            Ok(Value::Void)
        }
    }

    fn eval_quote(&self, arguments: &[Expr]) -> Result<Value, EvalError> {
        require_exact_args("quote", arguments.len(), 1)?;
        Ok(quote_to_value(&arguments[0]))
    }

    fn eval_lambda(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(EvalError::message("lambda requires parameters and a body"));
        }

        Ok(Value::Closure(Rc::new(ClosureValue {
            parameters: parse_parameter_names(&arguments[0])?,
            body: arguments[1..].to_vec(),
            env,
        })))
    }

    fn eval_define_record_type(
        &self,
        arguments: &[Expr],
        env: Rc<Env>,
    ) -> Result<Value, EvalError> {
        if arguments.len() < 3 {
            return Err(EvalError::message(
                "define-record-type expects a name, constructor, predicate, and fields",
            ));
        }

        let type_name =
            parse_symbol_name(&arguments[0], "define-record-type name must be a symbol")?;

        let constructor = parse_record_constructor(&arguments[1])?;
        let predicate_name = parse_symbol_name(
            &arguments[2],
            "define-record-type predicate must be a symbol",
        )?;
        let fields = parse_record_fields(&arguments[3..])?;

        if constructor.field_names.len() != fields.len() {
            return Err(EvalError::message(
                "define-record-type constructor arity must match field count",
            ));
        }

        for (constructor_field, field) in constructor.field_names.iter().zip(fields.iter()) {
            if constructor_field != &field.name {
                return Err(EvalError::message(
                    "define-record-type constructor fields must match accessor fields",
                ));
            }
        }

        let record_type = Rc::new(RecordType {
            name: type_name,
            field_count: fields.len(),
        });

        env.define(
            constructor.name,
            Value::RecordProcedure(RecordProcedure::Constructor(record_type.clone())),
        );
        env.define(
            predicate_name,
            Value::RecordProcedure(RecordProcedure::Predicate(record_type.clone())),
        );

        for (field_index, field) in fields.iter().enumerate() {
            env.define(
                field.accessor.clone(),
                Value::RecordProcedure(RecordProcedure::Accessor {
                    record_type: record_type.clone(),
                    field_index,
                }),
            );
            if let Some(mutator) = &field.mutator {
                env.define(
                    mutator.clone(),
                    Value::RecordProcedure(RecordProcedure::Mutator {
                        record_type: record_type.clone(),
                        field_index,
                    }),
                );
            }
        }

        Ok(Value::Void)
    }

    fn eval_all(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Vec<Value>, EvalError> {
        let mut values = Vec::with_capacity(arguments.len());
        for argument in arguments {
            values.push(self.eval(argument, env.clone())?);
        }
        Ok(values)
    }

    fn eval_and(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        let mut last = Value::Bool(true);
        for argument in arguments {
            last = self.eval(argument, env.clone())?;
            if !is_truthy(&last) {
                return Ok(last);
            }
        }
        Ok(last)
    }

    fn eval_or(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        let mut last = Value::Bool(false);
        for argument in arguments {
            last = self.eval(argument, env.clone())?;
            if is_truthy(&last) {
                return Ok(last);
            }
        }
        Ok(last)
    }

    fn eval_begin(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        self.eval_sequence(arguments, env)
    }

    fn eval_let(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        if arguments.is_empty() {
            return Err(EvalError::message("let requires bindings and a body"));
        }

        if let ExprKind::Symbol(name) = &arguments[0].kind {
            if arguments.len() < 3 {
                return Err(EvalError::message("named let requires bindings and a body"));
            }

            return self.eval_named_let(name, &arguments[1], &arguments[2..], env);
        }

        if arguments.len() < 2 {
            return Err(EvalError::message("let requires bindings and a body"));
        }

        let bindings = parse_bindings(&arguments[0])?;
        let let_env = Env::new(Some(env.clone()));
        let values = self.eval_binding_values(&bindings, env)?;
        bind_values(&let_env, &bindings, values);
        self.eval_sequence(&arguments[1..], let_env)
    }

    fn eval_letrec(
        &self,
        arguments: &[Expr],
        env: Rc<Env>,
        sequential: bool,
    ) -> Result<Value, EvalError> {
        let form_name = if sequential { "letrec*" } else { "letrec" };
        if arguments.len() < 2 {
            return Err(EvalError::message(format!(
                "{form_name} requires bindings and a body"
            )));
        }

        let bindings = parse_bindings(&arguments[0])?;
        let letrec_env = Env::new(Some(env));
        let mut cells = Vec::with_capacity(bindings.len());

        for binding in &bindings {
            cells.push(letrec_env.define_placeholder(binding.name.clone()));
        }

        if sequential {
            for (binding, cell) in bindings.iter().zip(cells.iter()) {
                let value = self.eval(&binding.init_expr, letrec_env.clone())?;
                *cell.borrow_mut() = value;
            }
        } else {
            let mut values = Vec::with_capacity(bindings.len());
            for binding in &bindings {
                values.push(self.eval(&binding.init_expr, letrec_env.clone())?);
            }
            for (cell, value) in cells.into_iter().zip(values) {
                *cell.borrow_mut() = value;
            }
        }

        self.eval_sequence(&arguments[1..], letrec_env)
    }

    fn eval_named_let(
        &self,
        name: &str,
        binding_expr: &Expr,
        body: &[Expr],
        env: Rc<Env>,
    ) -> Result<Value, EvalError> {
        let bindings = parse_bindings(binding_expr)?;
        let let_env = Env::new(Some(env.clone()));
        let binding = let_env.define_placeholder(name.to_string());
        let closure = Value::Closure(Rc::new(ClosureValue {
            parameters: ParameterSpec {
                required: binding_names(&bindings),
                rest: None,
            },
            body: body.to_vec(),
            env: let_env.clone(),
        }));
        *binding.borrow_mut() = closure.clone();
        let arguments = self.eval_binding_values(&bindings, env)?;
        self.apply_procedure(closure, arguments)
    }

    fn eval_binding_values(
        &self,
        bindings: &[BindingSpec],
        env: Rc<Env>,
    ) -> Result<Vec<Value>, EvalError> {
        let mut values = Vec::with_capacity(bindings.len());
        for binding in bindings {
            values.push(self.eval(&binding.init_expr, env.clone())?);
        }
        Ok(values)
    }

    fn eval_cond(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        for (index, clause_expr) in arguments.iter().enumerate() {
            let ExprKind::List(elements) = &clause_expr.kind else {
                return Err(EvalError::message("cond clause must be a non-empty list"));
            };

            if elements.is_empty() {
                return Err(EvalError::message("cond clause must be a non-empty list"));
            }

            let test_expr = &elements[0];
            if let ExprKind::Symbol(name) = &test_expr.kind {
                if name == "else" {
                    if index != arguments.len() - 1 {
                        return Err(EvalError::message("cond else clause must be last"));
                    }
                    if elements.len() == 1 {
                        return Ok(Value::Bool(true));
                    }
                    return self.eval_sequence(&elements[1..], env);
                }
            }

            let test_value = self.eval(test_expr, env.clone())?;
            if is_truthy(&test_value) {
                if elements.len() == 1 {
                    return Ok(test_value);
                }
                return self.eval_sequence(&elements[1..], env);
            }
        }

        Ok(Value::Void)
    }

    fn eval_case(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(EvalError::message(
                "case requires a key and at least one clause",
            ));
        }

        let key = self.eval(&arguments[0], env.clone())?;
        for (index, clause_expr) in arguments[1..].iter().enumerate() {
            let ExprKind::List(elements) = &clause_expr.kind else {
                return Err(EvalError::message("case clause must be a non-empty list"));
            };
            if elements.is_empty() {
                return Err(EvalError::message("case clause must be a non-empty list"));
            }

            if let ExprKind::Symbol(name) = &elements[0].kind {
                if name == "else" {
                    if index != arguments.len() - 2 {
                        return Err(EvalError::message("case else clause must be last"));
                    }
                    if elements.len() == 1 {
                        return Ok(Value::Bool(true));
                    }
                    return self.eval_sequence(&elements[1..], env);
                }
            }

            let ExprKind::List(datums) = &elements[0].kind else {
                return Err(EvalError::message("case clause datums must be a list"));
            };

            if datums
                .iter()
                .any(|datum| eqv_values(&key, &quote_to_value(datum)))
            {
                if elements.len() == 1 {
                    return Ok(Value::Void);
                }
                return self.eval_sequence(&elements[1..], env);
            }
        }

        Ok(Value::Void)
    }

    fn eval_do(&self, arguments: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(EvalError::message(
                "do requires bindings and a termination clause",
            ));
        }

        let bindings = parse_do_bindings(&arguments[0])?;
        let ExprKind::List(termination_clause) = &arguments[1].kind else {
            return Err(EvalError::message(
                "do termination clause must be a non-empty list",
            ));
        };
        if termination_clause.is_empty() {
            return Err(EvalError::message(
                "do termination clause must be a non-empty list",
            ));
        }

        let loop_env = Env::new(Some(env.clone()));
        let mut initial_values = Vec::with_capacity(bindings.len());
        for binding in &bindings {
            initial_values.push(self.eval(&binding.init_expr, env.clone())?);
        }
        for (binding, value) in bindings.iter().zip(initial_values) {
            loop_env.define(binding.name.clone(), value);
        }

        let body = &arguments[2..];
        loop {
            let test_result = self.eval(&termination_clause[0], loop_env.clone())?;
            if is_truthy(&test_result) {
                if termination_clause.len() == 1 {
                    return Ok(Value::Void);
                }
                return self.eval_sequence(&termination_clause[1..], loop_env);
            }

            self.eval_sequence(body, loop_env.clone())?;

            let mut next_values = Vec::with_capacity(bindings.len());
            for binding in &bindings {
                let next_value = match &binding.step_expr {
                    Some(step_expr) => self.eval(step_expr, loop_env.clone())?,
                    None => loop_env.lookup(&binding.name)?,
                };
                next_values.push(next_value);
            }

            for (binding, value) in bindings.iter().zip(next_values) {
                loop_env.set(&binding.name, value)?;
            }
        }
    }

    fn apply_procedure(&self, operator: Value, arguments: Vec<Value>) -> Result<Value, EvalError> {
        match operator {
            Value::Builtin(builtin) => (builtin.implementation)(self, &arguments),
            Value::Closure(closure) => self.apply_closure(&closure, arguments),
            Value::RecordProcedure(procedure) => self.apply_record_procedure(procedure, arguments),
            _ => Err(EvalError::message("attempted to call a non-procedure")),
        }
    }

    fn apply_closure(
        &self,
        closure: &ClosureValue,
        arguments: Vec<Value>,
    ) -> Result<Value, EvalError> {
        let required = closure.parameters.required.len();
        if closure.parameters.rest.is_some() {
            require_min_args("lambda", arguments.len(), required)?;
        } else {
            require_exact_args("lambda", arguments.len(), required)?;
        }

        let call_env = Env::new(Some(closure.env.clone()));
        for (name, value) in closure
            .parameters
            .required
            .iter()
            .zip(arguments.iter().take(required).cloned())
        {
            call_env.define(name.clone(), value);
        }
        if let Some(rest_name) = &closure.parameters.rest {
            call_env.define(
                rest_name.clone(),
                list_value(arguments[required..].to_vec()),
            );
        }
        self.eval_sequence(&closure.body, call_env)
    }

    fn apply_record_procedure(
        &self,
        procedure: RecordProcedure,
        arguments: Vec<Value>,
    ) -> Result<Value, EvalError> {
        match procedure {
            RecordProcedure::Constructor(record_type) => {
                require_exact_args(
                    "record constructor",
                    arguments.len(),
                    record_type.field_count,
                )?;
                Ok(Value::Record(Rc::new(RecordValue {
                    record_type,
                    fields: arguments
                        .into_iter()
                        .map(|value| Rc::new(RefCell::new(value)))
                        .collect(),
                })))
            }
            RecordProcedure::Predicate(record_type) => {
                require_exact_args("record predicate", arguments.len(), 1)?;
                let result = match &arguments[0] {
                    Value::Record(record) => Rc::ptr_eq(&record.record_type, &record_type),
                    _ => false,
                };
                Ok(Value::Bool(result))
            }
            RecordProcedure::Accessor {
                record_type,
                field_index,
            } => {
                require_exact_args("record accessor", arguments.len(), 1)?;
                Ok(require_record_field(
                    &arguments[0],
                    &record_type,
                    field_index,
                    "record accessor",
                )?
                .borrow()
                .clone())
            }
            RecordProcedure::Mutator {
                record_type,
                field_index,
            } => {
                require_exact_args("record mutator", arguments.len(), 2)?;
                let field = require_record_field(
                    &arguments[0],
                    &record_type,
                    field_index,
                    "record mutator",
                )?;
                *field.borrow_mut() = arguments[1].clone();
                Ok(Value::Void)
            }
        }
    }

    fn eval_sequence(&self, expressions: &[Expr], env: Rc<Env>) -> Result<Value, EvalError> {
        let mut result = Value::Void;
        for expression in expressions {
            result = self.eval(expression, env.clone())?;
        }
        Ok(result)
    }
}

fn create_global_env() -> Rc<Env> {
    let env = Env::new(None);
    env.define_builtin("+", |_, arguments| builtin_add(arguments));
    env.define_builtin("-", |_, arguments| builtin_subtract(arguments));
    env.define_builtin("*", |_, arguments| builtin_multiply(arguments));
    env.define_builtin("/", |_, arguments| builtin_divide(arguments));
    env.define_builtin("zero?", |_, arguments| builtin_is_zero(arguments));
    env.define_builtin("remainder", |_, arguments| builtin_remainder(arguments));
    env.define_builtin("quotient", |_, arguments| builtin_quotient(arguments));
    env.define_builtin("<", |_, arguments| builtin_less_than(arguments));
    env.define_builtin(">", |_, arguments| builtin_greater_than(arguments));
    env.define_builtin("=", |_, arguments| builtin_equal(arguments));
    env.define_builtin("<=", |_, arguments| builtin_less_equal(arguments));
    env.define_builtin("not", |_, arguments| builtin_not(arguments));
    env.define_builtin("eq?", |_, arguments| builtin_eq(arguments));
    env.define_builtin("eqv?", |_, arguments| builtin_eqv(arguments));
    env.define_builtin("equal?", |_, arguments| builtin_equal_value(arguments));
    env.define_builtin("exact?", |_, arguments| builtin_is_exact(arguments));
    env.define_builtin("inexact?", |_, arguments| builtin_is_inexact(arguments));
    env.define_builtin("integer?", |_, arguments| builtin_is_integer(arguments));
    env.define_builtin("rational?", |_, arguments| builtin_is_rational(arguments));
    env.define_builtin("exact->inexact", |_, arguments| {
        builtin_exact_to_inexact(arguments)
    });
    env.define_builtin("inexact->exact", |_, arguments| {
        builtin_inexact_to_exact(arguments)
    });
    env.define_builtin("numerator", |_, arguments| builtin_numerator(arguments));
    env.define_builtin("denominator", |_, arguments| builtin_denominator(arguments));
    env.define_builtin("cons", |_, arguments| builtin_cons(arguments));
    env.define_builtin("car", |_, arguments| builtin_car(arguments));
    env.define_builtin("cdr", |_, arguments| builtin_cdr(arguments));
    env.define_builtin("null?", |_, arguments| builtin_is_null(arguments));
    env.define_builtin("list", |_, arguments| builtin_list(arguments));
    env.define_builtin("length", |_, arguments| builtin_length(arguments));
    env.define_builtin("append", |_, arguments| builtin_append(arguments));
    env.define_builtin("string?", |_, arguments| builtin_is_string(arguments));
    env.define_builtin("number?", |_, arguments| builtin_is_number(arguments));
    env.define_builtin("boolean?", |_, arguments| builtin_is_boolean(arguments));
    env.define_builtin("pair?", |_, arguments| builtin_is_pair(arguments));
    env.define_builtin("symbol?", |_, arguments| builtin_is_symbol(arguments));
    env.define_builtin("vector", |_, arguments| builtin_vector(arguments));
    env.define_builtin("make-vector", |_, arguments| builtin_make_vector(arguments));
    env.define_builtin("vector-ref", |_, arguments| builtin_vector_ref(arguments));
    env.define_builtin("vector-set!", |_, arguments| builtin_vector_set(arguments));
    env.define_builtin("vector-length", |_, arguments| {
        builtin_vector_length(arguments)
    });
    env.define_builtin("vector?", |_, arguments| builtin_is_vector(arguments));
    env.define_builtin("vector->list", |_, arguments| {
        builtin_vector_to_list(arguments)
    });
    env.define_builtin("list->vector", |_, arguments| {
        builtin_list_to_vector(arguments)
    });
    env.define_builtin("apply", builtin_apply);
    env.define_builtin("map", builtin_map);
    env
}

fn bind_values(env: &Rc<Env>, bindings: &[BindingSpec], values: Vec<Value>) {
    for (binding, value) in bindings.iter().zip(values) {
        env.define(binding.name.clone(), value);
    }
}

fn binding_names(bindings: &[BindingSpec]) -> Vec<String> {
    bindings
        .iter()
        .map(|binding| binding.name.clone())
        .collect()
}

fn parse_bindings(binding_expr: &Expr) -> Result<Vec<BindingSpec>, EvalError> {
    let ExprKind::List(binding_list) = &binding_expr.kind else {
        return Err(EvalError::message("let bindings must be a list"));
    };

    let mut bindings = Vec::with_capacity(binding_list.len());
    for entry_expr in binding_list {
        let ExprKind::List(entry) = &entry_expr.kind else {
            return Err(EvalError::message(
                "let binding must contain a name and value",
            ));
        };

        if entry.len() != 2 {
            return Err(EvalError::message(
                "let binding must contain a name and value",
            ));
        }

        let ExprKind::Symbol(name) = &entry[0].kind else {
            return Err(EvalError::message("let binding name must be a symbol"));
        };

        bindings.push(BindingSpec {
            name: name.clone(),
            init_expr: entry[1].clone(),
        });
    }
    Ok(bindings)
}

fn parse_do_bindings(binding_expr: &Expr) -> Result<Vec<DoBindingSpec>, EvalError> {
    let ExprKind::List(binding_list) = &binding_expr.kind else {
        return Err(EvalError::message("do bindings must be a list"));
    };

    let mut bindings = Vec::with_capacity(binding_list.len());
    for entry_expr in binding_list {
        let ExprKind::List(entry) = &entry_expr.kind else {
            return Err(EvalError::message("do binding must have 2 or 3 elements"));
        };

        if entry.len() != 2 && entry.len() != 3 {
            return Err(EvalError::message("do binding must have 2 or 3 elements"));
        }

        let ExprKind::Symbol(name) = &entry[0].kind else {
            return Err(EvalError::message("do binding name must be a symbol"));
        };

        bindings.push(DoBindingSpec {
            name: name.clone(),
            init_expr: entry[1].clone(),
            step_expr: entry.get(2).cloned(),
        });
    }
    Ok(bindings)
}

fn parse_symbol_name(expr: &Expr, error_message: &str) -> Result<String, EvalError> {
    match &expr.kind {
        ExprKind::Symbol(name) => Ok(name.clone()),
        _ => Err(EvalError::message(error_message)),
    }
}

fn parse_record_constructor(expr: &Expr) -> Result<RecordConstructorSpec, EvalError> {
    let ExprKind::List(elements) = &expr.kind else {
        return Err(EvalError::message(
            "define-record-type constructor must be a list",
        ));
    };

    if elements.is_empty() {
        return Err(EvalError::message(
            "define-record-type constructor must not be empty",
        ));
    }

    let name = parse_symbol_name(
        &elements[0],
        "define-record-type constructor name must be a symbol",
    )?;
    let mut field_names = Vec::with_capacity(elements.len().saturating_sub(1));
    for field in &elements[1..] {
        field_names.push(parse_symbol_name(
            field,
            "define-record-type constructor field must be a symbol",
        )?);
    }

    Ok(RecordConstructorSpec { name, field_names })
}

fn parse_record_fields(field_exprs: &[Expr]) -> Result<Vec<RecordFieldSpec>, EvalError> {
    if field_exprs.is_empty() {
        return Err(EvalError::message(
            "define-record-type requires at least one field",
        ));
    }

    let mut fields = Vec::with_capacity(field_exprs.len());
    for field_expr in field_exprs {
        let ExprKind::List(elements) = &field_expr.kind else {
            return Err(EvalError::message(
                "define-record-type field specification must be a list",
            ));
        };

        if elements.len() != 2 && elements.len() != 3 {
            return Err(EvalError::message(
                "define-record-type field specification must have 2 or 3 elements",
            ));
        }

        let name = parse_symbol_name(
            &elements[0],
            "define-record-type field name must be a symbol",
        )?;
        let accessor = parse_symbol_name(
            &elements[1],
            "define-record-type accessor name must be a symbol",
        )?;
        let mutator = if elements.len() == 3 {
            Some(parse_symbol_name(
                &elements[2],
                "define-record-type mutator name must be a symbol",
            )?)
        } else {
            None
        };

        fields.push(RecordFieldSpec {
            name,
            accessor,
            mutator,
        });
    }

    Ok(fields)
}

fn parse_parameter_names(parameter_expr: &Expr) -> Result<ParameterSpec, EvalError> {
    match &parameter_expr.kind {
        ExprKind::List(parameter_list) => parse_parameter_names_list(parameter_list),
        ExprKind::Symbol(name) => Ok(ParameterSpec {
            required: Vec::new(),
            rest: Some(name.clone()),
        }),
        _ => Err(EvalError::message("lambda parameters must be a list")),
    }
}

fn parse_parameter_names_list(parameter_exprs: &[Expr]) -> Result<ParameterSpec, EvalError> {
    if let Some(dot_index) = parameter_exprs.iter().position(|parameter_expr| {
        matches!(
            &parameter_expr.kind,
            ExprKind::Symbol(name) if name == "."
        )
    }) {
        if dot_index + 2 != parameter_exprs.len() {
            return Err(EvalError::message("lambda parameter list is malformed"));
        }

        let mut required = Vec::with_capacity(dot_index);
        for parameter_expr in &parameter_exprs[..dot_index] {
            let ExprKind::Symbol(name) = &parameter_expr.kind else {
                return Err(EvalError::message("lambda parameter must be a symbol"));
            };
            required.push(name.clone());
        }

        let ExprKind::Symbol(rest) = &parameter_exprs[dot_index + 1].kind else {
            return Err(EvalError::message("lambda parameter must be a symbol"));
        };

        return Ok(ParameterSpec {
            required,
            rest: Some(rest.clone()),
        });
    }

    let mut required = Vec::with_capacity(parameter_exprs.len());
    for parameter_expr in parameter_exprs {
        let ExprKind::Symbol(name) = &parameter_expr.kind else {
            return Err(EvalError::message("lambda parameter must be a symbol"));
        };
        required.push(name.clone());
    }

    Ok(ParameterSpec {
        required,
        rest: None,
    })
}

fn quote_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Int(value) => Value::Int(*value),
        ExprKind::Rational(numerator, denominator) => {
            exact_value(ExactNumber::new(*numerator, *denominator))
        }
        ExprKind::Inexact(value) => Value::Inexact(*value),
        ExprKind::Bool(value) => Value::Bool(*value),
        ExprKind::String(value) => Value::String(value.clone()),
        ExprKind::Symbol(name) => Value::Symbol(name.clone()),
        ExprKind::Vector(elements) => vector_value(elements.iter().map(quote_to_value).collect()),
        ExprKind::List(elements) => list_value(elements.iter().map(quote_to_value).collect()),
    }
}

fn list_value(values: Vec<Value>) -> Value {
    let mut result = Value::EmptyList;
    for value in values.into_iter().rev() {
        result = Value::Pair(Rc::new(PairValue {
            car: value,
            cdr: result,
        }));
    }
    result
}

fn vector_value(values: Vec<Value>) -> Value {
    Value::Vector(Rc::new(VectorValue {
        elements: RefCell::new(values),
    }))
}

fn exact_value(number: ExactNumber) -> Value {
    if number.denominator == 1 {
        Value::Int(number.numerator)
    } else {
        Value::Rational(number)
    }
}

fn ensure_position(error: EvalError, position: SourceLoc) -> EvalError {
    match error {
        EvalError::WithPosition { .. } => error,
        EvalError::Message { message } => {
            EvalError::with_position(message, position.line, position.column)
        }
    }
}

fn require_min_args(name: &str, actual: usize, min: usize) -> Result<(), EvalError> {
    if actual < min {
        return Err(EvalError::message(format!(
            "{name} expected at least {min} argument(s)"
        )));
    }
    Ok(())
}

fn require_exact_args(name: &str, actual: usize, exact: usize) -> Result<(), EvalError> {
    if actual != exact {
        return Err(EvalError::message(format!(
            "{name} expected exactly {exact} argument(s)"
        )));
    }
    Ok(())
}

fn require_number<'a>(value: &'a Value, operator: &str) -> Result<&'a Value, EvalError> {
    if is_number(value) {
        Ok(value)
    } else {
        Err(EvalError::message(format!(
            "{operator} expects numeric arguments"
        )))
    }
}

fn require_exact_number(value: &Value, operator: &str) -> Result<ExactNumber, EvalError> {
    match value {
        Value::Int(number) => Ok(ExactNumber::integer(*number)),
        Value::Rational(number) => Ok(*number),
        _ => Err(EvalError::message(format!(
            "{operator} expects exact numeric arguments"
        ))),
    }
}

fn require_numeric_f64(value: &Value, operator: &str) -> Result<f64, EvalError> {
    match value {
        Value::Int(number) => Ok(*number as f64),
        Value::Rational(number) => Ok(number.as_f64()),
        Value::Inexact(number) => Ok(*number),
        _ => Err(EvalError::message(format!(
            "{operator} expects numeric arguments"
        ))),
    }
}

fn contains_inexact(arguments: &[Value]) -> bool {
    arguments
        .iter()
        .any(|argument| matches!(argument, Value::Inexact(_)))
}

fn is_number(value: &Value) -> bool {
    matches!(
        value,
        Value::Int(_) | Value::Rational(_) | Value::Inexact(_)
    )
}

fn is_exact_number(value: &Value) -> bool {
    matches!(value, Value::Int(_) | Value::Rational(_))
}

fn require_int(value: &Value, operator: &str) -> Result<i64, EvalError> {
    match value {
        Value::Int(number) => Ok(*number),
        _ => Err(EvalError::message(format!(
            "{operator} expects numeric arguments"
        ))),
    }
}

fn require_pair<'a>(value: &'a Value, operator: &str) -> Result<&'a PairValue, EvalError> {
    match value {
        Value::Pair(pair) => Ok(pair.as_ref()),
        _ => Err(EvalError::message(format!("{operator} expects a pair"))),
    }
}

fn require_vector<'a>(value: &'a Value, operator: &str) -> Result<&'a Rc<VectorValue>, EvalError> {
    match value {
        Value::Vector(vector) => Ok(vector),
        _ => Err(EvalError::message(format!("{operator} expects a vector"))),
    }
}

fn require_index(value: &Value, operator: &str) -> Result<usize, EvalError> {
    let index = require_int(value, operator)?;
    usize::try_from(index)
        .map_err(|_| EvalError::message(format!("{operator} expects a valid index")))
}

fn require_record_field(
    value: &Value,
    expected_type: &Rc<RecordType>,
    field_index: usize,
    operator: &str,
) -> Result<Cell, EvalError> {
    match value {
        Value::Record(record) if Rc::ptr_eq(&record.record_type, expected_type) => record
            .fields
            .get(field_index)
            .cloned()
            .ok_or_else(|| EvalError::message(format!("{operator} field out of range"))),
        Value::Record(_) => Err(EvalError::message(format!(
            "{operator} expects a record of the correct type"
        ))),
        _ => Err(EvalError::message(format!("{operator} expects a record"))),
    }
}

fn require_proper_list(value: &Value, operator: &str) -> Result<Vec<Value>, EvalError> {
    let mut elements = Vec::new();
    let mut current = value.clone();

    loop {
        match current {
            Value::Pair(pair) => {
                elements.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            Value::EmptyList => return Ok(elements),
            _ => {
                return Err(EvalError::message(format!(
                    "{operator} expects a proper list"
                )))
            }
        }
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn eqv_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => left == right,
        (Value::Rational(left), Value::Rational(right)) => left == right,
        (Value::Inexact(left), Value::Inexact(right)) => left == right,
        (Value::Bool(left), Value::Bool(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(left, right),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Builtin(left), Value::Builtin(right)) => left.name == right.name,
        (Value::Closure(left), Value::Closure(right)) => Rc::ptr_eq(left, right),
        (Value::RecordProcedure(left), Value::RecordProcedure(right)) => {
            record_procedure_eqv(left, right)
        }
        (Value::Void, Value::Void) => true,
        (Value::Uninitialized, Value::Uninitialized) => true,
        _ => false,
    }
}

fn record_procedure_eqv(left: &RecordProcedure, right: &RecordProcedure) -> bool {
    match (left, right) {
        (RecordProcedure::Constructor(left), RecordProcedure::Constructor(right)) => {
            Rc::ptr_eq(left, right)
        }
        (RecordProcedure::Predicate(left), RecordProcedure::Predicate(right)) => {
            Rc::ptr_eq(left, right)
        }
        (
            RecordProcedure::Accessor {
                record_type: left_type,
                field_index: left_index,
            },
            RecordProcedure::Accessor {
                record_type: right_type,
                field_index: right_index,
            },
        ) => left_index == right_index && Rc::ptr_eq(left_type, right_type),
        (
            RecordProcedure::Mutator {
                record_type: left_type,
                field_index: left_index,
            },
            RecordProcedure::Mutator {
                record_type: right_type,
                field_index: right_index,
            },
        ) => left_index == right_index && Rc::ptr_eq(left_type, right_type),
        _ => false,
    }
}

fn equal_values(left: &Value, right: &Value) -> bool {
    let mut visited = Vec::new();
    equal_values_inner(left, right, &mut visited)
}

fn equal_values_inner(left: &Value, right: &Value, visited: &mut Vec<(usize, usize)>) -> bool {
    if numeric_values_equal(left, right) || eqv_values(left, right) {
        return true;
    }

    match (left, right) {
        (Value::String(left), Value::String(right)) => left == right,
        (Value::Pair(left), Value::Pair(right)) => {
            let key = (Rc::as_ptr(left) as usize, Rc::as_ptr(right) as usize);
            if visited.contains(&key) {
                return true;
            }
            visited.push(key);

            equal_values_inner(&left.car, &right.car, visited)
                && equal_values_inner(&left.cdr, &right.cdr, visited)
        }
        (Value::Vector(left), Value::Vector(right)) => {
            let key = (Rc::as_ptr(left) as usize, Rc::as_ptr(right) as usize);
            if visited.contains(&key) {
                return true;
            }
            visited.push(key);

            let left = left.elements.borrow();
            let right = right.elements.borrow();
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| equal_values_inner(left, right, visited))
        }
        _ => false,
    }
}

fn numeric_values_equal(left: &Value, right: &Value) -> bool {
    if !is_number(left) || !is_number(right) {
        return false;
    }

    if matches!(left, Value::Inexact(_)) || matches!(right, Value::Inexact(_)) {
        return numeric_value_as_f64(left) == numeric_value_as_f64(right);
    }

    exact_number_value(left).compare(exact_number_value(right)) == Ordering::Equal
}

fn numeric_value_as_f64(value: &Value) -> f64 {
    match value {
        Value::Int(number) => *number as f64,
        Value::Rational(number) => number.as_f64(),
        Value::Inexact(number) => *number,
        _ => unreachable!("validated numeric value"),
    }
}

fn exact_number_value(value: &Value) -> ExactNumber {
    match value {
        Value::Int(number) => ExactNumber::integer(*number),
        Value::Rational(number) => *number,
        _ => unreachable!("validated exact numeric value"),
    }
}

fn render(value: &Value) -> String {
    match value {
        Value::Int(number) => number.to_string(),
        Value::Rational(number) => format!("{}/{}", number.numerator, number.denominator),
        Value::Inexact(number) => render_inexact(*number),
        Value::Bool(true) => "#t".to_string(),
        Value::Bool(false) => "#f".to_string(),
        Value::String(text) => quote_string(text),
        Value::Symbol(name) => name.clone(),
        Value::EmptyList => "()".to_string(),
        Value::Pair(pair) => render_pair(pair.as_ref()),
        Value::Vector(vector) => render_vector(vector.as_ref()),
        Value::Record(record) => format!("#<record {}>", record.record_type.name),
        Value::Builtin(_) | Value::Closure(_) | Value::RecordProcedure(_) => {
            "#<procedure>".to_string()
        }
        Value::Void => "#<void>".to_string(),
        Value::Uninitialized => "#<uninitialized>".to_string(),
    }
}

fn render_inexact(value: f64) -> String {
    format!("{value:?}")
}

fn inexact_to_exact_value(value: f64, operator: &str) -> Result<Value, EvalError> {
    if !value.is_finite() {
        return Err(EvalError::message(format!(
            "{operator} expects numeric arguments"
        )));
    }

    let rendered = render_inexact(value);
    if rendered.contains('e') || rendered.contains('E') {
        return Err(EvalError::message(format!(
            "{operator} produced a value out of range"
        )));
    }

    if let Some((whole, fractional)) = rendered.split_once('.') {
        let negative = whole.starts_with('-');
        let whole = whole.strip_prefix('-').unwrap_or(whole);
        let digits = format!("{whole}{fractional}");
        let magnitude = digits
            .parse::<i64>()
            .map_err(|_| EvalError::message(format!("{operator} produced a value out of range")))?;
        let numerator = if negative { -magnitude } else { magnitude };
        let denominator = 10_i64.checked_pow(fractional.len() as u32).ok_or_else(|| {
            EvalError::message(format!("{operator} produced a value out of range"))
        })?;
        return Ok(exact_value(ExactNumber::new(numerator, denominator)));
    }

    rendered
        .parse::<i64>()
        .map(|number| Value::Int(number))
        .map_err(|_| EvalError::message(format!("{operator} produced a value out of range")))
}

fn render_pair(pair: &PairValue) -> String {
    let mut result = String::from("(");
    let mut current = Value::Pair(Rc::new(pair.clone()));
    let mut first = true;

    loop {
        match current {
            Value::Pair(pair) => {
                if !first {
                    result.push(' ');
                }
                result.push_str(&render(&pair.car));
                current = pair.cdr.clone();
                first = false;
            }
            Value::EmptyList => {
                result.push(')');
                return result;
            }
            other => {
                result.push_str(" . ");
                result.push_str(&render(&other));
                result.push(')');
                return result;
            }
        }
    }
}

fn render_vector(vector: &VectorValue) -> String {
    let elements = vector.elements.borrow();
    let mut result = String::from("#(");
    for (index, element) in elements.iter().enumerate() {
        if index > 0 {
            result.push(' ');
        }
        result.push_str(&render(element));
    }
    result.push(')');
    result
}

fn quote_string(value: &str) -> String {
    let mut result = String::from("\"");
    for ch in value.chars() {
        match ch {
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            _ => result.push(ch),
        }
    }
    result.push('"');
    result
}

fn gcd(mut left: i64, mut right: i64) -> i64 {
    left = left.abs();
    right = right.abs();
    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }
    if left == 0 {
        1
    } else {
        left
    }
}

fn builtin_add(arguments: &[Value]) -> Result<Value, EvalError> {
    if contains_inexact(arguments) {
        let mut total = 0.0_f64;
        for argument in arguments {
            total += require_numeric_f64(argument, "+")?;
        }
        return Ok(Value::Inexact(total));
    }

    let mut total = ExactNumber::integer(0);
    for argument in arguments {
        total = total.add(require_exact_number(argument, "+")?);
    }
    Ok(exact_value(total))
}

fn builtin_subtract(arguments: &[Value]) -> Result<Value, EvalError> {
    require_min_args("-", arguments.len(), 1)?;
    if contains_inexact(arguments) {
        let mut result = require_numeric_f64(&arguments[0], "-")?;
        if arguments.len() == 1 {
            return Ok(Value::Inexact(-result));
        }

        for argument in &arguments[1..] {
            result -= require_numeric_f64(argument, "-")?;
        }
        return Ok(Value::Inexact(result));
    }

    let mut result = require_exact_number(&arguments[0], "-")?;
    if arguments.len() == 1 {
        return Ok(exact_value(result.negate()));
    }

    for argument in &arguments[1..] {
        result = result.subtract(require_exact_number(argument, "-")?);
    }
    Ok(exact_value(result))
}

fn builtin_multiply(arguments: &[Value]) -> Result<Value, EvalError> {
    if contains_inexact(arguments) {
        let mut total = 1.0_f64;
        for argument in arguments {
            total *= require_numeric_f64(argument, "*")?;
        }
        return Ok(Value::Inexact(total));
    }

    let mut total = ExactNumber::integer(1);
    for argument in arguments {
        total = total.multiply(require_exact_number(argument, "*")?);
    }
    Ok(exact_value(total))
}

fn builtin_divide(arguments: &[Value]) -> Result<Value, EvalError> {
    require_min_args("/", arguments.len(), 2)?;
    if contains_inexact(arguments) {
        let mut result = require_numeric_f64(&arguments[0], "/")?;
        for argument in &arguments[1..] {
            let divisor = require_numeric_f64(argument, "/")?;
            if divisor == 0.0 {
                return Err(EvalError::message("division by zero"));
            }
            result /= divisor;
        }
        return Ok(Value::Inexact(result));
    }

    let mut result = require_exact_number(&arguments[0], "/")?;
    for argument in &arguments[1..] {
        let divisor = require_exact_number(argument, "/")?;
        if divisor.numerator == 0 {
            return Err(EvalError::message("division by zero"));
        }
        result = result.divide(divisor);
    }
    Ok(exact_value(result))
}

fn builtin_less_than(arguments: &[Value]) -> Result<Value, EvalError> {
    compare(arguments, "<")
}

fn builtin_greater_than(arguments: &[Value]) -> Result<Value, EvalError> {
    compare(arguments, ">")
}

fn builtin_equal(arguments: &[Value]) -> Result<Value, EvalError> {
    compare(arguments, "=")
}

fn builtin_less_equal(arguments: &[Value]) -> Result<Value, EvalError> {
    compare(arguments, "<=")
}

fn builtin_is_zero(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("zero?", arguments.len(), 1)?;
    let result = match require_number(&arguments[0], "zero?")? {
        Value::Int(number) => *number == 0,
        Value::Rational(number) => number.numerator == 0,
        Value::Inexact(number) => *number == 0.0,
        _ => unreachable!("validated numeric value"),
    };
    Ok(Value::Bool(result))
}

fn builtin_remainder(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("remainder", arguments.len(), 2)?;
    let dividend = require_int(&arguments[0], "remainder")?;
    let divisor = require_int(&arguments[1], "remainder")?;
    if divisor == 0 {
        return Err(EvalError::message("division by zero"));
    }
    Ok(Value::Int(dividend % divisor))
}

fn builtin_quotient(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("quotient", arguments.len(), 2)?;
    let dividend = require_int(&arguments[0], "quotient")?;
    let divisor = require_int(&arguments[1], "quotient")?;
    if divisor == 0 {
        return Err(EvalError::message("division by zero"));
    }
    Ok(Value::Int(dividend / divisor))
}

fn compare(arguments: &[Value], operator: &str) -> Result<Value, EvalError> {
    require_min_args(operator, arguments.len(), 2)?;
    for pair in arguments.windows(2) {
        let passes = if contains_inexact(pair) {
            let left = require_numeric_f64(&pair[0], operator)?;
            let right = require_numeric_f64(&pair[1], operator)?;
            match operator {
                "<" => left < right,
                ">" => left > right,
                "=" => left == right,
                "<=" => left <= right,
                _ => unreachable!("unexpected comparison operator"),
            }
        } else {
            let left = require_exact_number(&pair[0], operator)?;
            let right = require_exact_number(&pair[1], operator)?;
            match operator {
                "<" => left.compare(right) == Ordering::Less,
                ">" => left.compare(right) == Ordering::Greater,
                "=" => left.compare(right) == Ordering::Equal,
                "<=" => matches!(left.compare(right), Ordering::Less | Ordering::Equal),
                _ => unreachable!("unexpected comparison operator"),
            }
        };
        if !passes {
            return Ok(Value::Bool(false));
        }
    }
    Ok(Value::Bool(true))
}

fn builtin_not(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("not", arguments.len(), 1)?;
    Ok(Value::Bool(!is_truthy(&arguments[0])))
}

fn builtin_eq(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("eq?", arguments.len(), 2)?;
    Ok(Value::Bool(eqv_values(&arguments[0], &arguments[1])))
}

fn builtin_eqv(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("eqv?", arguments.len(), 2)?;
    Ok(Value::Bool(eqv_values(&arguments[0], &arguments[1])))
}

fn builtin_equal_value(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("equal?", arguments.len(), 2)?;
    Ok(Value::Bool(equal_values(&arguments[0], &arguments[1])))
}

fn builtin_is_exact(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("exact?", arguments.len(), 1)?;
    Ok(Value::Bool(is_exact_number(&arguments[0])))
}

fn builtin_is_inexact(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("inexact?", arguments.len(), 1)?;
    Ok(Value::Bool(matches!(arguments[0], Value::Inexact(_))))
}

fn builtin_is_integer(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("integer?", arguments.len(), 1)?;
    let result = match &arguments[0] {
        Value::Int(_) => true,
        Value::Rational(number) => number.denominator == 1,
        Value::Inexact(number) => number.is_finite() && number.fract() == 0.0,
        _ => false,
    };
    Ok(Value::Bool(result))
}

fn builtin_is_rational(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("rational?", arguments.len(), 1)?;
    let result = match &arguments[0] {
        Value::Int(_) | Value::Rational(_) => true,
        Value::Inexact(number) => number.is_finite(),
        _ => false,
    };
    Ok(Value::Bool(result))
}

fn builtin_exact_to_inexact(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("exact->inexact", arguments.len(), 1)?;
    Ok(Value::Inexact(require_numeric_f64(
        require_number(&arguments[0], "exact->inexact")?,
        "exact->inexact",
    )?))
}

fn builtin_inexact_to_exact(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("inexact->exact", arguments.len(), 1)?;
    let value = require_number(&arguments[0], "inexact->exact")?;
    match value {
        Value::Int(_) | Value::Rational(_) => Ok(value.clone()),
        Value::Inexact(number) => inexact_to_exact_value(*number, "inexact->exact"),
        _ => unreachable!("validated numeric value"),
    }
}

fn builtin_numerator(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("numerator", arguments.len(), 1)?;
    Ok(Value::Int(
        require_exact_number(&arguments[0], "numerator")?.numerator,
    ))
}

fn builtin_denominator(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("denominator", arguments.len(), 1)?;
    Ok(Value::Int(
        require_exact_number(&arguments[0], "denominator")?.denominator,
    ))
}

fn builtin_cons(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("cons", arguments.len(), 2)?;
    Ok(Value::Pair(Rc::new(PairValue {
        car: arguments[0].clone(),
        cdr: arguments[1].clone(),
    })))
}

fn builtin_car(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("car", arguments.len(), 1)?;
    Ok(require_pair(&arguments[0], "car")?.car.clone())
}

fn builtin_cdr(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("cdr", arguments.len(), 1)?;
    Ok(require_pair(&arguments[0], "cdr")?.cdr.clone())
}

fn builtin_is_null(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("null?", arguments.len(), 1)?;
    Ok(Value::Bool(matches!(arguments[0], Value::EmptyList)))
}

fn builtin_list(arguments: &[Value]) -> Result<Value, EvalError> {
    Ok(list_value(arguments.to_vec()))
}

fn builtin_length(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("length", arguments.len(), 1)?;
    Ok(Value::Int(
        require_proper_list(&arguments[0], "length")?.len() as i64,
    ))
}

fn builtin_append(arguments: &[Value]) -> Result<Value, EvalError> {
    if arguments.is_empty() {
        return Ok(Value::EmptyList);
    }

    let mut result = arguments[arguments.len() - 1].clone();
    for prefix in arguments[..arguments.len() - 1].iter().rev() {
        let items = require_proper_list(prefix, "append")?;
        for item in items.into_iter().rev() {
            result = Value::Pair(Rc::new(PairValue {
                car: item,
                cdr: result,
            }));
        }
    }
    Ok(result)
}

fn builtin_apply(evaluator: &Evaluator, arguments: &[Value]) -> Result<Value, EvalError> {
    require_min_args("apply", arguments.len(), 2)?;

    let operator = arguments[0].clone();
    let mut applied_arguments = arguments[1..arguments.len() - 1].to_vec();
    applied_arguments.extend(require_proper_list(
        &arguments[arguments.len() - 1],
        "apply",
    )?);

    evaluator.apply_procedure(operator, applied_arguments)
}

fn builtin_map(evaluator: &Evaluator, arguments: &[Value]) -> Result<Value, EvalError> {
    require_min_args("map", arguments.len(), 2)?;

    let operator = arguments[0].clone();
    let lists = arguments[1..]
        .iter()
        .map(|value| require_proper_list(value, "map"))
        .collect::<Result<Vec<_>, _>>()?;

    let result_len = lists.iter().map(Vec::len).min().unwrap_or(0);
    let mut results = Vec::with_capacity(result_len);

    for index in 0..result_len {
        let call_arguments = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        results.push(evaluator.apply_procedure(operator.clone(), call_arguments)?);
    }

    Ok(list_value(results))
}

fn builtin_is_string(arguments: &[Value]) -> Result<Value, EvalError> {
    type_predicate("string?", arguments, |value| {
        matches!(value, Value::String(_))
    })
}

fn builtin_is_number(arguments: &[Value]) -> Result<Value, EvalError> {
    type_predicate("number?", arguments, is_number)
}

fn builtin_is_boolean(arguments: &[Value]) -> Result<Value, EvalError> {
    type_predicate("boolean?", arguments, |value| {
        matches!(value, Value::Bool(_))
    })
}

fn builtin_is_pair(arguments: &[Value]) -> Result<Value, EvalError> {
    type_predicate("pair?", arguments, |value| matches!(value, Value::Pair(_)))
}

fn builtin_is_symbol(arguments: &[Value]) -> Result<Value, EvalError> {
    type_predicate("symbol?", arguments, |value| {
        matches!(value, Value::Symbol(_))
    })
}

fn builtin_vector(arguments: &[Value]) -> Result<Value, EvalError> {
    Ok(vector_value(arguments.to_vec()))
}

fn builtin_make_vector(arguments: &[Value]) -> Result<Value, EvalError> {
    if arguments.len() != 1 && arguments.len() != 2 {
        return Err(EvalError::message(
            "make-vector expected 1 or 2 argument(s)",
        ));
    }

    let length = require_index(&arguments[0], "make-vector")?;
    let fill = arguments.get(1).cloned().unwrap_or(Value::Void);
    Ok(vector_value(vec![fill; length]))
}

fn builtin_vector_ref(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("vector-ref", arguments.len(), 2)?;
    let vector = require_vector(&arguments[0], "vector-ref")?;
    let index = require_index(&arguments[1], "vector-ref")?;
    vector
        .elements
        .borrow()
        .get(index)
        .cloned()
        .ok_or_else(|| EvalError::message("vector-ref index out of bounds"))
}

fn builtin_vector_set(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("vector-set!", arguments.len(), 3)?;
    let vector = require_vector(&arguments[0], "vector-set!")?;
    let index = require_index(&arguments[1], "vector-set!")?;
    let mut elements = vector.elements.borrow_mut();
    let Some(slot) = elements.get_mut(index) else {
        return Err(EvalError::message("vector-set! index out of bounds"));
    };
    *slot = arguments[2].clone();
    Ok(Value::Void)
}

fn builtin_vector_length(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("vector-length", arguments.len(), 1)?;
    let vector = require_vector(&arguments[0], "vector-length")?;
    Ok(Value::Int(vector.elements.borrow().len() as i64))
}

fn builtin_is_vector(arguments: &[Value]) -> Result<Value, EvalError> {
    type_predicate("vector?", arguments, |value| {
        matches!(value, Value::Vector(_))
    })
}

fn builtin_vector_to_list(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("vector->list", arguments.len(), 1)?;
    let vector = require_vector(&arguments[0], "vector->list")?;
    Ok(list_value(vector.elements.borrow().clone()))
}

fn builtin_list_to_vector(arguments: &[Value]) -> Result<Value, EvalError> {
    require_exact_args("list->vector", arguments.len(), 1)?;
    Ok(vector_value(require_proper_list(
        &arguments[0],
        "list->vector",
    )?))
}

fn type_predicate<F>(name: &str, arguments: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    require_exact_args(name, arguments.len(), 1)?;
    Ok(Value::Bool(predicate(&arguments[0])))
}

type BuiltinFn = fn(&Evaluator, &[Value]) -> Result<Value, EvalError>;
type Cell = Rc<RefCell<Value>>;

#[derive(Clone, Debug)]
struct Expr {
    kind: ExprKind,
    position: SourceLoc,
}

#[derive(Clone, Debug)]
enum ExprKind {
    Int(i64),
    Rational(i64, i64),
    Inexact(f64),
    Bool(bool),
    String(String),
    Symbol(String),
    Vector(Vec<Expr>),
    List(Vec<Expr>),
}

#[derive(Clone, Copy, Debug)]
struct SourceLoc {
    line: usize,
    column: usize,
}

#[derive(Clone)]
enum Value {
    Int(i64),
    Rational(ExactNumber),
    Inexact(f64),
    Bool(bool),
    String(String),
    Symbol(String),
    EmptyList,
    Pair(Rc<PairValue>),
    Vector(Rc<VectorValue>),
    Record(Rc<RecordValue>),
    Builtin(Builtin),
    Closure(Rc<ClosureValue>),
    RecordProcedure(RecordProcedure),
    Void,
    Uninitialized,
}

#[derive(Clone)]
struct PairValue {
    car: Value,
    cdr: Value,
}

#[derive(Clone)]
struct RecordValue {
    record_type: Rc<RecordType>,
    fields: Vec<Cell>,
}

struct VectorValue {
    elements: RefCell<Vec<Value>>,
}

#[derive(Clone, Copy)]
struct Builtin {
    name: &'static str,
    implementation: BuiltinFn,
}

#[derive(Clone)]
enum RecordProcedure {
    Constructor(Rc<RecordType>),
    Predicate(Rc<RecordType>),
    Accessor {
        record_type: Rc<RecordType>,
        field_index: usize,
    },
    Mutator {
        record_type: Rc<RecordType>,
        field_index: usize,
    },
}

#[derive(Clone)]
struct ClosureValue {
    parameters: ParameterSpec,
    body: Vec<Expr>,
    env: Rc<Env>,
}

#[derive(Clone)]
struct BindingSpec {
    name: String,
    init_expr: Expr,
}

#[derive(Clone)]
struct DoBindingSpec {
    name: String,
    init_expr: Expr,
    step_expr: Option<Expr>,
}

#[derive(Clone)]
struct ParameterSpec {
    required: Vec<String>,
    rest: Option<String>,
}

struct RecordType {
    name: String,
    field_count: usize,
}

struct RecordConstructorSpec {
    name: String,
    field_names: Vec<String>,
}

struct RecordFieldSpec {
    name: String,
    accessor: String,
    mutator: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ExactNumber {
    numerator: i64,
    denominator: i64,
}

impl ExactNumber {
    fn new(numerator: i64, denominator: i64) -> Self {
        debug_assert_ne!(denominator, 0);
        if numerator == 0 {
            return Self {
                numerator: 0,
                denominator: 1,
            };
        }

        let mut numerator = numerator;
        let mut denominator = denominator;
        if denominator < 0 {
            numerator = -numerator;
            denominator = -denominator;
        }

        let divisor = gcd(numerator, denominator);
        Self {
            numerator: numerator / divisor,
            denominator: denominator / divisor,
        }
    }

    fn integer(value: i64) -> Self {
        Self {
            numerator: value,
            denominator: 1,
        }
    }

    fn add(self, other: Self) -> Self {
        Self::new(
            self.numerator * other.denominator + other.numerator * self.denominator,
            self.denominator * other.denominator,
        )
    }

    fn subtract(self, other: Self) -> Self {
        Self::new(
            self.numerator * other.denominator - other.numerator * self.denominator,
            self.denominator * other.denominator,
        )
    }

    fn multiply(self, other: Self) -> Self {
        Self::new(
            self.numerator * other.numerator,
            self.denominator * other.denominator,
        )
    }

    fn divide(self, other: Self) -> Self {
        Self::new(
            self.numerator * other.denominator,
            self.denominator * other.numerator,
        )
    }

    fn negate(self) -> Self {
        Self::new(-self.numerator, self.denominator)
    }

    fn compare(self, other: Self) -> Ordering {
        let left = i128::from(self.numerator) * i128::from(other.denominator);
        let right = i128::from(other.numerator) * i128::from(self.denominator);
        left.cmp(&right)
    }

    fn as_f64(self) -> f64 {
        self.numerator as f64 / self.denominator as f64
    }
}

struct Env {
    parent: Option<Rc<Env>>,
    bindings: RefCell<HashMap<String, Cell>>,
}

impl Env {
    fn new(parent: Option<Rc<Env>>) -> Rc<Self> {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
        })
    }

    fn define(&self, name: String, value: Value) {
        self.bindings
            .borrow_mut()
            .insert(name, Rc::new(RefCell::new(value)));
    }

    fn define_builtin(&self, name: &'static str, implementation: BuiltinFn) {
        self.define(
            name.to_string(),
            Value::Builtin(Builtin {
                name,
                implementation,
            }),
        );
    }

    fn define_placeholder(&self, name: String) -> Cell {
        let cell = Rc::new(RefCell::new(Value::Uninitialized));
        self.bindings.borrow_mut().insert(name, cell.clone());
        cell
    }

    fn lookup(&self, name: &str) -> Result<Value, EvalError> {
        let Some(cell) = self.lookup_cell(name) else {
            return Err(EvalError::message(format!("unbound variable: {name}")));
        };

        let value = cell.borrow().clone();
        if matches!(value, Value::Uninitialized) {
            return Err(EvalError::message(format!("unbound variable: {name}")));
        }

        Ok(value)
    }

    fn set(&self, name: &str, value: Value) -> Result<(), EvalError> {
        let Some(cell) = self.lookup_cell(name) else {
            return Err(EvalError::message(format!("unbound variable: {name}")));
        };

        if matches!(*cell.borrow(), Value::Uninitialized) {
            return Err(EvalError::message(format!("unbound variable: {name}")));
        }

        *cell.borrow_mut() = value;
        Ok(())
    }

    fn lookup_cell(&self, name: &str) -> Option<Cell> {
        if let Some(cell) = self.bindings.borrow().get(name).cloned() {
            return Some(cell);
        }

        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup_cell(name))
    }
}

struct Parser<'input> {
    input: &'input str,
    index: usize,
    line: usize,
    column: usize,
}

impl<'input> Parser<'input> {
    fn new(input: &'input str) -> Self {
        Self {
            input,
            index: 0,
            line: 1,
            column: 1,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_trivia();
        while !self.is_at_end() {
            expressions.push(self.parse_expression()?);
            self.skip_trivia();
        }
        Ok(expressions)
    }

    fn parse_expression(&mut self) -> Result<Expr, EvalError> {
        self.skip_trivia();
        if self.is_at_end() {
            return Err(self.error("unexpected end of input"));
        }

        match self.current_char().unwrap_or_default() {
            '(' => self.parse_list(),
            '\'' => {
                let position = self.current_loc();
                self.advance();
                Ok(Expr {
                    kind: ExprKind::List(vec![
                        Expr {
                            kind: ExprKind::Symbol("quote".to_string()),
                            position,
                        },
                        self.parse_expression()?,
                    ]),
                    position,
                })
            }
            '#' if self.peek_char() == Some('(') => self.parse_vector(),
            '"' => self.parse_string(),
            ')' => Err(self.error("unexpected ')'")),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        let position = self.current_loc();
        self.consume('(')?;
        let mut elements = Vec::new();
        self.skip_trivia();
        while !self.is_at_end() && self.current_char() != Some(')') {
            elements.push(self.parse_expression()?);
            self.skip_trivia();
        }

        if self.is_at_end() {
            return Err(self.error("unterminated list"));
        }

        self.consume(')')?;
        Ok(Expr {
            kind: ExprKind::List(elements),
            position,
        })
    }

    fn parse_vector(&mut self) -> Result<Expr, EvalError> {
        let position = self.current_loc();
        self.consume('#')?;
        self.consume('(')?;

        let mut elements = Vec::new();
        self.skip_trivia();
        while !self.is_at_end() && self.current_char() != Some(')') {
            elements.push(self.parse_expression()?);
            self.skip_trivia();
        }

        if self.is_at_end() {
            return Err(self.error("unterminated vector"));
        }

        self.consume(')')?;
        Ok(Expr {
            kind: ExprKind::Vector(elements),
            position,
        })
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        let position = self.current_loc();
        self.consume('"')?;
        let mut result = String::new();

        while !self.is_at_end() {
            let current = self.advance().unwrap_or_default();
            if current == '"' {
                return Ok(Expr {
                    kind: ExprKind::String(result),
                    position,
                });
            }

            if current == '\\' {
                if self.is_at_end() {
                    return Err(self.error("unterminated string escape"));
                }
                result.push(match self.advance().unwrap_or_default() {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '"' => '"',
                    '\\' => '\\',
                    other => other,
                });
                continue;
            }

            result.push(current);
        }

        Err(self.error("unterminated string literal"))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let position = self.current_loc();
        let start = self.index;
        while !self.is_at_end() && !self.is_delimiter(self.current_char().unwrap_or_default()) {
            self.advance();
        }

        let token = &self.input[start..self.index];
        let kind = match token {
            "#t" => ExprKind::Bool(true),
            "#f" => ExprKind::Bool(false),
            _ => self.parse_number_or_symbol(token, position)?,
        };

        Ok(Expr { kind, position })
    }

    fn parse_number_or_symbol(
        &self,
        token: &str,
        position: SourceLoc,
    ) -> Result<ExprKind, EvalError> {
        if is_integer_token(token) {
            if let Ok(number) = token.parse::<i64>() {
                return Ok(ExprKind::Int(number));
            }
        }

        if is_rational_token(token) {
            let Some((numerator, denominator)) = token.split_once('/') else {
                unreachable!("validated rational token");
            };
            let numerator = numerator.parse::<i64>().map_err(|_| {
                EvalError::with_position("invalid rational literal", position.line, position.column)
            })?;
            let denominator = denominator.parse::<i64>().map_err(|_| {
                EvalError::with_position("invalid rational literal", position.line, position.column)
            })?;
            if denominator == 0 {
                return Err(EvalError::with_position(
                    "invalid rational literal",
                    position.line,
                    position.column,
                ));
            }
            return Ok(ExprKind::Rational(numerator, denominator));
        }

        if is_decimal_token(token) {
            if let Ok(number) = token.parse::<f64>() {
                return Ok(ExprKind::Inexact(number));
            }
        }

        Ok(ExprKind::Symbol(token.to_string()))
    }

    fn skip_trivia(&mut self) {
        while !self.is_at_end() {
            match self.current_char().unwrap_or_default() {
                ch if ch.is_whitespace() => {
                    self.advance();
                }
                ';' => self.skip_comment(),
                _ => return,
            }
        }
    }

    fn skip_comment(&mut self) {
        while !self.is_at_end() && self.current_char() != Some('\n') {
            self.advance();
        }
    }

    fn is_delimiter(&self, ch: char) -> bool {
        ch.is_whitespace() || matches!(ch, '(' | ')' | ';' | '\'')
    }

    fn consume(&mut self, expected: char) -> Result<(), EvalError> {
        if self.current_char() != Some(expected) {
            return Err(self.error(&format!("expected '{expected}'")));
        }
        self.advance();
        Ok(())
    }

    fn current_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }

    fn peek_char(&self) -> Option<char> {
        let mut chars = self.input[self.index..].chars();
        chars.next()?;
        chars.next()
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.input.len()
    }

    fn current_loc(&self) -> SourceLoc {
        SourceLoc {
            line: self.line,
            column: self.column,
        }
    }

    fn advance(&mut self) -> Option<char> {
        let current = self.current_char()?;
        self.index += current.len_utf8();
        if current == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(current)
    }

    fn error(&self, message: &str) -> EvalError {
        EvalError::with_position(message, self.line, self.column)
    }
}

fn is_integer_token(token: &str) -> bool {
    if token.is_empty() {
        return false;
    }

    let mut chars = token.chars();
    let first = chars.next().unwrap_or_default();
    if matches!(first, '+' | '-') {
        return !chars.as_str().is_empty() && chars.all(|ch| ch.is_ascii_digit());
    }

    token.chars().all(|ch| ch.is_ascii_digit())
}

fn is_rational_token(token: &str) -> bool {
    let Some((numerator, denominator)) = token.split_once('/') else {
        return false;
    };

    !denominator.contains('/') && is_integer_token(numerator) && is_integer_token(denominator)
}

fn is_decimal_token(token: &str) -> bool {
    if token.is_empty() {
        return false;
    }

    let token = token
        .strip_prefix('+')
        .or_else(|| token.strip_prefix('-'))
        .unwrap_or(token);

    let Some((whole, fractional)) = token.split_once('.') else {
        return false;
    };

    !whole.is_empty()
        && !fractional.is_empty()
        && !fractional.contains('.')
        && whole.chars().all(|ch| ch.is_ascii_digit())
        && fractional.chars().all(|ch| ch.is_ascii_digit())
}

#[cfg(test)]
mod tests;
