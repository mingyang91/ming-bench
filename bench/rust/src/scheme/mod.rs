pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut interpreter = Interpreter::new(false);
    Ok(interpreter.eval_program(input)?.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut interpreter = Interpreter::new(true);
    let result = interpreter.eval_program(input)?;
    Ok((result.render(), interpreter.take_output()))
}

struct Interpreter {
    global_env: Rc<Environment>,
    output: Option<String>,
}

impl Interpreter {
    fn new(capture_output: bool) -> Self {
        let global_env = Self::create_global_env();
        Self {
            global_env,
            output: capture_output.then(String::new),
        }
    }

    fn create_global_env() -> Rc<Environment> {
        let env = Environment::new(None);

        env.define("+", builtin_value("+", Self::builtin_add));
        env.define("-", builtin_value("-", Self::builtin_subtract));
        env.define("*", builtin_value("*", Self::builtin_multiply));
        env.define("/", builtin_value("/", Self::builtin_divide));
        env.define("<", builtin_value("<", Self::builtin_less));
        env.define(">", builtin_value(">", Self::builtin_greater));
        env.define("=", builtin_value("=", Self::builtin_equal));
        env.define("<=", builtin_value("<=", Self::builtin_less_equal));
        env.define("not", builtin_value("not", Self::builtin_not));
        env.define("cons", builtin_value("cons", Self::builtin_cons));
        env.define("car", builtin_value("car", Self::builtin_car));
        env.define("cdr", builtin_value("cdr", Self::builtin_cdr));
        env.define("null?", builtin_value("null?", Self::builtin_null));
        env.define("list", builtin_value("list", Self::builtin_list));
        env.define("length", builtin_value("length", Self::builtin_length));
        env.define("append", builtin_value("append", Self::builtin_append));
        env.define("string?", builtin_value("string?", Self::builtin_string_predicate));
        env.define("number?", builtin_value("number?", Self::builtin_number_predicate));
        env.define(
            "boolean?",
            builtin_value("boolean?", Self::builtin_boolean_predicate),
        );
        env.define("pair?", builtin_value("pair?", Self::builtin_pair_predicate));
        env.define("symbol?", builtin_value("symbol?", Self::builtin_symbol_predicate));
        env.define("display", builtin_value("display", Self::builtin_display));
        env.define("write", builtin_value("write", Self::builtin_write));
        env.define("newline", builtin_value("newline", Self::builtin_newline));
        env.define(
            "string-append",
            builtin_value("string-append", Self::builtin_string_append),
        );
        env.define(
            "string-length",
            builtin_value("string-length", Self::builtin_string_length),
        );
        env.define("substring", builtin_value("substring", Self::builtin_substring));
        env.define(
            "string->number",
            builtin_value("string->number", Self::builtin_string_to_number),
        );
        env.define(
            "number->string",
            builtin_value("number->string", Self::builtin_number_to_string),
        );
        env.define(
            "symbol->string",
            builtin_value("symbol->string", Self::builtin_symbol_to_string),
        );
        env.define(
            "string->symbol",
            builtin_value("string->symbol", Self::builtin_string_to_symbol),
        );
        env.define("string-ref", builtin_value("string-ref", Self::builtin_string_ref));
        env.define("char?", builtin_value("char?", Self::builtin_char_predicate));
        env.define("string-copy", builtin_value("string-copy", Self::builtin_string_copy));
        env.define("string-set!", builtin_value("string-set!", Self::builtin_string_set));

        env
    }

    fn eval_program(&mut self, input: &str) -> Result<Value, EvalError> {
        let mut parser = Parser::new(input);
        let mut last_value = None;

        while parser.has_more() {
            let expr = parser.parse_expr()?;
            last_value = Some(self.eval(&expr, self.global_env.clone())?);
        }

        last_value.ok_or_else(|| EvalError::new("empty input"))
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
            Value::String(string) => string.as_plain_string(),
            Value::Char(ch) => ch.to_string(),
            _ => value.render(),
        }
    }

    fn eval(&mut self, expr: &Expr, env: Rc<Environment>) -> Result<Value, EvalError> {
        self.eval_inner(expr, env)
            .map_err(|error| error.with_position(expr.position().line, expr.position().column))
    }

    fn eval_inner(&mut self, expr: &Expr, env: Rc<Environment>) -> Result<Value, EvalError> {
        match expr {
            Expr::Int { value, .. } => Ok(Value::Int(*value)),
            Expr::Bool { value, .. } => Ok(Value::Bool(*value)),
            Expr::String { value, .. } => Ok(Value::String(SchemeString::new(value))),
            Expr::Char { value, .. } => Ok(Value::Char(*value)),
            Expr::Symbol { name, .. } => env.lookup(name),
            Expr::List { elements, .. } => self.eval_list(elements, env),
        }
    }

    fn eval_list(&mut self, elements: &[Expr], env: Rc<Environment>) -> Result<Value, EvalError> {
        let Some((head, arg_exprs)) = elements.split_first() else {
            return Err(EvalError::new("cannot evaluate empty list"));
        };

        if let Expr::Symbol { name, .. } = head {
            return match name.as_str() {
                "define" => self.eval_define(arg_exprs, env),
                "if" => self.eval_if(arg_exprs, env),
                "quote" => self.eval_quote(arg_exprs),
                "lambda" => self.eval_lambda(arg_exprs, env),
                "begin" => self.eval_begin(arg_exprs, env),
                "let" => self.eval_let(arg_exprs, env),
                "cond" => self.eval_cond(arg_exprs, env),
                "and" => self.eval_and(arg_exprs, env),
                "or" => self.eval_or(arg_exprs, env),
                _ => {
                    let procedure = self.eval(head, env.clone())?;
                    let arguments = self.eval_args(arg_exprs, env)?;
                    self.apply_procedure(procedure, arguments)
                }
            };
        }

        let procedure = self.eval(head, env.clone())?;
        let arguments = self.eval_args(arg_exprs, env)?;
        self.apply_procedure(procedure, arguments)
    }

    fn eval_define(&mut self, arg_exprs: &[Expr], env: Rc<Environment>) -> Result<Value, EvalError> {
        if arg_exprs.len() < 2 {
            return Err(EvalError::new("define requires a name and a value"));
        }

        match &arg_exprs[0] {
            Expr::Symbol { name, .. } => {
                Self::require_arity("define", arg_exprs.len(), 2)?;
                let value = self.eval(&arg_exprs[1], env.clone())?;
                env.define(name.clone(), value);
                Ok(Value::Void)
            }
            Expr::List { elements, .. } => {
                let Some((name_expr, parameter_exprs)) = elements.split_first() else {
                    return Err(EvalError::new("define requires a function name"));
                };
                let Expr::Symbol { name, .. } = name_expr else {
                    return Err(EvalError::new("function name must be a symbol"));
                };

                let parameter_names = Self::parse_parameter_names(parameter_exprs)?;
                let body = Self::parse_body("define", &arg_exprs[1..])?;
                let procedure = Value::Procedure(Rc::new(Procedure::User(UserProcedure {
                    name: Some(name.clone()),
                    parameter_names,
                    body,
                    closure_env: env.clone(),
                })));
                env.define(name.clone(), procedure);
                Ok(Value::Void)
            }
            _ => Err(EvalError::new("invalid define")),
        }
    }

    fn eval_if(&mut self, arg_exprs: &[Expr], env: Rc<Environment>) -> Result<Value, EvalError> {
        Self::require_arity("if", arg_exprs.len(), 3)?;
        let condition = self.eval(&arg_exprs[0], env.clone())?;
        if Self::is_truthy(&condition) {
            self.eval(&arg_exprs[1], env)
        } else {
            self.eval(&arg_exprs[2], env)
        }
    }

    fn eval_quote(&mut self, arg_exprs: &[Expr]) -> Result<Value, EvalError> {
        Self::require_arity("quote", arg_exprs.len(), 1)?;
        Self::quote_to_value(&arg_exprs[0])
    }

    fn eval_lambda(
        &mut self,
        arg_exprs: &[Expr],
        env: Rc<Environment>,
    ) -> Result<Value, EvalError> {
        if arg_exprs.len() < 2 {
            return Err(EvalError::new("lambda requires parameters and a body"));
        }

        let Expr::List { elements, .. } = &arg_exprs[0] else {
            return Err(EvalError::new("lambda parameters must be a list"));
        };

        let parameter_names = Self::parse_parameter_names(elements)?;
        let body = Self::parse_body("lambda", &arg_exprs[1..])?;

        Ok(Value::Procedure(Rc::new(Procedure::User(UserProcedure {
            name: None,
            parameter_names,
            body,
            closure_env: env,
        }))))
    }

    fn eval_begin(
        &mut self,
        arg_exprs: &[Expr],
        env: Rc<Environment>,
    ) -> Result<Value, EvalError> {
        self.eval_sequence(arg_exprs, env)
    }

    fn eval_let(&mut self, arg_exprs: &[Expr], env: Rc<Environment>) -> Result<Value, EvalError> {
        let Some(first_arg) = arg_exprs.first() else {
            return Err(EvalError::new("let requires bindings and a body"));
        };

        match first_arg {
            Expr::Symbol { name, .. } => {
                if arg_exprs.len() < 2 {
                    return Err(EvalError::new("let requires bindings and a body"));
                }

                let Expr::List { elements, .. } = &arg_exprs[1] else {
                    return Err(EvalError::new("let bindings must be a list"));
                };

                let bindings = Self::parse_bindings(elements)?;
                let body = Self::parse_body("let", &arg_exprs[2..])?;
                self.eval_named_let(name, &bindings, &body, env)
            }
            Expr::List { elements, .. } => {
                let bindings = Self::parse_bindings(elements)?;
                let body = Self::parse_body("let", &arg_exprs[1..])?;
                self.eval_simple_let(&bindings, &body, env)
            }
            _ => Err(EvalError::new("let bindings must be a list")),
        }
    }

    fn eval_simple_let(
        &mut self,
        bindings: &[LetBinding],
        body: &[Expr],
        env: Rc<Environment>,
    ) -> Result<Value, EvalError> {
        let let_env = Environment::new(Some(env.clone()));
        for binding in bindings {
            let value = self.eval(&binding.value_expr, env.clone())?;
            let_env.define(binding.name.clone(), value);
        }
        self.eval_sequence(body, let_env)
    }

    fn eval_named_let(
        &mut self,
        name: &str,
        bindings: &[LetBinding],
        body: &[Expr],
        env: Rc<Environment>,
    ) -> Result<Value, EvalError> {
        let mut parameter_names = Vec::with_capacity(bindings.len());
        let mut arguments = Vec::with_capacity(bindings.len());

        for binding in bindings {
            parameter_names.push(binding.name.clone());
            arguments.push(self.eval(&binding.value_expr, env.clone())?);
        }

        let let_env = Environment::new(Some(env));
        let procedure = Value::Procedure(Rc::new(Procedure::User(UserProcedure {
            name: Some(name.to_owned()),
            parameter_names,
            body: body.to_vec(),
            closure_env: let_env.clone(),
        })));
        let_env.define(name.to_owned(), procedure.clone());

        self.apply_procedure(procedure, arguments)
    }

    fn eval_cond(&mut self, arg_exprs: &[Expr], env: Rc<Environment>) -> Result<Value, EvalError> {
        for (index, clause_expr) in arg_exprs.iter().enumerate() {
            let Expr::List { elements: clause, .. } = clause_expr else {
                return Err(EvalError::new("cond clause must be a list"));
            };
            let Some((test_expr, body)) = clause.split_first() else {
                return Err(EvalError::new("cond clause cannot be empty"));
            };

            if let Expr::Symbol { name, .. } = test_expr {
                if name == "else" {
                    if index != arg_exprs.len() - 1 {
                        return Err(EvalError::new("cond else clause must be last"));
                    }
                    return self.eval_clause_body("cond", body, env.clone(), None);
                }
            }

            let test_value = self.eval(test_expr, env.clone())?;
            if Self::is_truthy(&test_value) {
                return self.eval_clause_body("cond", body, env.clone(), Some(test_value));
            }
        }

        Ok(Value::Void)
    }

    fn eval_clause_body(
        &mut self,
        form_name: &str,
        body: &[Expr],
        env: Rc<Environment>,
        default_value: Option<Value>,
    ) -> Result<Value, EvalError> {
        if body.is_empty() {
            return match default_value {
                Some(value) => Ok(value),
                None => Err(EvalError::new(format!("{form_name} clause requires a body"))),
            };
        }

        self.eval_sequence(body, env)
    }

    fn parse_parameter_names(params: &[Expr]) -> Result<Vec<String>, EvalError> {
        let mut parameter_names = Vec::with_capacity(params.len());
        for param in params {
            let Expr::Symbol { name, .. } = param else {
                return Err(EvalError::new("parameter must be a symbol"));
            };
            parameter_names.push(name.clone());
        }
        Ok(parameter_names)
    }

    fn parse_bindings(binding_exprs: &[Expr]) -> Result<Vec<LetBinding>, EvalError> {
        let mut bindings = Vec::with_capacity(binding_exprs.len());
        for binding_expr in binding_exprs {
            let Expr::List { elements, .. } = binding_expr else {
                return Err(EvalError::new("let binding must be a list"));
            };

            if elements.len() != 2 {
                return Err(EvalError::new("let binding must contain a name and value"));
            }

            let Expr::Symbol { name, .. } = &elements[0] else {
                return Err(EvalError::new("let binding name must be a symbol"));
            };

            bindings.push(LetBinding {
                name: name.clone(),
                value_expr: elements[1].clone(),
            });
        }

        Ok(bindings)
    }

    fn parse_body(form_name: &str, body: &[Expr]) -> Result<Vec<Expr>, EvalError> {
        if body.is_empty() {
            return Err(EvalError::new(format!("{form_name} requires a body")));
        }
        Ok(body.to_vec())
    }

    fn eval_args(&mut self, arg_exprs: &[Expr], env: Rc<Environment>) -> Result<Vec<Value>, EvalError> {
        let mut values = Vec::with_capacity(arg_exprs.len());
        for arg_expr in arg_exprs {
            values.push(self.eval(arg_expr, env.clone())?);
        }
        Ok(values)
    }

    fn eval_and(&mut self, arg_exprs: &[Expr], env: Rc<Environment>) -> Result<Value, EvalError> {
        let mut result = Value::Bool(true);
        for arg_expr in arg_exprs {
            result = self.eval(arg_expr, env.clone())?;
            if !Self::is_truthy(&result) {
                return Ok(result);
            }
        }
        Ok(result)
    }

    fn eval_or(&mut self, arg_exprs: &[Expr], env: Rc<Environment>) -> Result<Value, EvalError> {
        let mut last_value = Value::Bool(false);
        for arg_expr in arg_exprs {
            let value = self.eval(arg_expr, env.clone())?;
            if Self::is_truthy(&value) {
                return Ok(value);
            }
            last_value = value;
        }
        Ok(last_value)
    }

    fn eval_sequence(&mut self, exprs: &[Expr], env: Rc<Environment>) -> Result<Value, EvalError> {
        let mut result = Value::Void;
        for expr in exprs {
            result = self.eval(expr, env.clone())?;
        }
        Ok(result)
    }

    fn apply_procedure(
        &mut self,
        procedure_value: Value,
        argument_values: Vec<Value>,
    ) -> Result<Value, EvalError> {
        let Value::Procedure(procedure) = procedure_value else {
            return Err(EvalError::new("not a procedure"));
        };
        procedure.apply(self, argument_values)
    }

    fn quote_to_value(expr: &Expr) -> Result<Value, EvalError> {
        match expr {
            Expr::Int { value, .. } => Ok(Value::Int(*value)),
            Expr::Bool { value, .. } => Ok(Value::Bool(*value)),
            Expr::String { value, .. } => Ok(Value::String(SchemeString::new(value))),
            Expr::Char { value, .. } => Ok(Value::Char(*value)),
            Expr::Symbol { name, .. } => Ok(Value::Symbol(name.clone())),
            Expr::List { elements, .. } => Self::quote_list_to_value(elements),
        }
    }

    fn quote_list_to_value(elements: &[Expr]) -> Result<Value, EvalError> {
        let mut result = Value::EmptyList;
        for expr in elements.iter().rev() {
            result = Value::Pair(Rc::new(PairValue {
                car: Self::quote_to_value(expr)?,
                cdr: result,
            }));
        }
        Ok(result)
    }

    fn builtin_add(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Ok(Value::Int(Self::sum(args)?))
    }

    fn builtin_subtract(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Ok(Value::Int(Self::subtract(args)?))
    }

    fn builtin_multiply(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Ok(Value::Int(Self::multiply(args)?))
    }

    fn builtin_divide(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Ok(Value::Int(Self::divide(args)?))
    }

    fn builtin_less(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Ok(Value::Bool(Self::compare_increasing(
            args,
            Comparison::StrictlyLess,
        )?))
    }

    fn builtin_greater(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Ok(Value::Bool(Self::compare_increasing(
            args,
            Comparison::StrictlyGreater,
        )?))
    }

    fn builtin_equal(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Ok(Value::Bool(Self::compare_increasing(args, Comparison::Equal)?))
    }

    fn builtin_less_equal(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Ok(Value::Bool(Self::compare_increasing(
            args,
            Comparison::LessOrEqual,
        )?))
    }

    fn builtin_not(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("not", args.len(), 1)?;
        Ok(Value::Bool(!Self::is_truthy(&args[0])))
    }

    fn builtin_cons(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("cons", args.len(), 2)?;
        Ok(Value::Pair(Rc::new(PairValue {
            car: args[0].clone(),
            cdr: args[1].clone(),
        })))
    }

    fn builtin_car(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("car", args.len(), 1)?;
        Ok(Self::expect_pair(&args[0])?.car.clone())
    }

    fn builtin_cdr(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("cdr", args.len(), 1)?;
        Ok(Self::expect_pair(&args[0])?.cdr.clone())
    }

    fn builtin_null(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("null?", args.len(), 1)?;
        Ok(Value::Bool(matches!(args[0], Value::EmptyList)))
    }

    fn builtin_list(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Ok(Self::make_list(args))
    }

    fn builtin_length(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("length", args.len(), 1)?;
        Ok(Value::Int(Self::length_of_list(&args[0])? as i64))
    }

    fn builtin_append(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::append_lists(args)
    }

    fn builtin_string_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("string?", args, |value| matches!(value, Value::String(_)))
    }

    fn builtin_number_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("number?", args, |value| matches!(value, Value::Int(_)))
    }

    fn builtin_boolean_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("boolean?", args, |value| matches!(value, Value::Bool(_)))
    }

    fn builtin_pair_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("pair?", args, |value| matches!(value, Value::Pair(_)))
    }

    fn builtin_symbol_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("symbol?", args, |value| matches!(value, Value::Symbol(_)))
    }

    fn builtin_display(
        interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("display", args.len(), 1)?;
        interpreter.append_output(&Self::render_for_display(&args[0]));
        Ok(Value::Void)
    }

    fn builtin_write(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, EvalError> {
        Self::require_arity("write", args.len(), 1)?;
        interpreter.append_output(&args[0].render());
        Ok(Value::Void)
    }

    fn builtin_newline(
        interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("newline", args.len(), 0)?;
        interpreter.append_output("\n");
        Ok(Value::Void)
    }

    fn builtin_string_append(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Ok(Value::String(SchemeString::new_owned(Self::string_append(args)?)))
    }

    fn builtin_string_length(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("string-length", args.len(), 1)?;
        Ok(Value::Int(Self::expect_string(&args[0])?.len() as i64))
    }

    fn builtin_substring(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("substring", args.len(), 3)?;
        let string = Self::expect_string(&args[0])?;
        let start = Self::expect_index(&args[1], "substring")?;
        let end = Self::expect_index(&args[2], "substring")?;
        let characters = string.characters();
        if start > end || end > characters.len() {
            return Err(EvalError::new("substring indices out of range"));
        }
        Ok(Value::String(SchemeString::from_chars(
            characters[start..end].to_vec(),
        )))
    }

    fn builtin_string_to_number(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("string->number", args.len(), 1)?;
        let value = Self::expect_string(&args[0])?.as_plain_string();
        match value.parse::<i64>() {
            Ok(number) => Ok(Value::Int(number)),
            Err(_) => Ok(Value::Bool(false)),
        }
    }

    fn builtin_number_to_string(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("number->string", args.len(), 1)?;
        Ok(Value::String(SchemeString::new_owned(
            Self::expect_int(&args[0])?.to_string(),
        )))
    }

    fn builtin_symbol_to_string(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("symbol->string", args.len(), 1)?;
        Ok(Value::String(SchemeString::new_owned(Self::expect_symbol(
            &args[0],
        )?)))
    }

    fn builtin_string_to_symbol(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("string->symbol", args.len(), 1)?;
        Ok(Value::Symbol(Self::expect_string(&args[0])?.as_plain_string()))
    }

    fn builtin_string_ref(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::require_arity("string-ref", args.len(), 2)?;
        let string = Self::expect_string(&args[0])?;
        let index = Self::expect_index(&args[1], "string-ref")?;
        let Some(ch) = string.char_at(index) else {
            return Err(EvalError::new("string-ref index out of range"));
        };
        Ok(Value::Char(ch))
    }

    fn builtin_char_predicate(
        _interpreter: &mut Interpreter,
        args: &[Value],
    ) -> Result<Value, EvalError> {
        Self::type_predicate("char?", args, |value| matches!(value, Value::Char(_)))
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
        let string = Self::expect_string(&args[0])?;
        let index = Self::expect_index(&args[1], "string-set!")?;
        let ch = Self::expect_char(&args[2])?;
        if !string.set_char(index, ch) {
            return Err(EvalError::new("string-set! index out of range"));
        }
        Ok(Value::Void)
    }

    fn sum(args: &[Value]) -> Result<i64, EvalError> {
        let mut total = 0;
        for arg in args {
            total += Self::expect_int(arg)?;
        }
        Ok(total)
    }

    fn subtract(args: &[Value]) -> Result<i64, EvalError> {
        Self::require_at_least("-", args.len(), 1)?;

        if args.len() == 1 {
            return Ok(-Self::expect_int(&args[0])?);
        }

        let mut result = Self::expect_int(&args[0])?;
        for arg in &args[1..] {
            result -= Self::expect_int(arg)?;
        }
        Ok(result)
    }

    fn multiply(args: &[Value]) -> Result<i64, EvalError> {
        let mut total = 1;
        for arg in args {
            total *= Self::expect_int(arg)?;
        }
        Ok(total)
    }

    fn divide(args: &[Value]) -> Result<i64, EvalError> {
        Self::require_at_least("/", args.len(), 2)?;

        let mut result = Self::expect_int(&args[0])?;
        for arg in &args[1..] {
            let divisor = Self::expect_int(arg)?;
            if divisor == 0 {
                return Err(EvalError::new("division by zero"));
            }
            result /= divisor;
        }

        Ok(result)
    }

    fn compare_increasing(args: &[Value], comparison: Comparison) -> Result<bool, EvalError> {
        Self::require_at_least(comparison.symbol(), args.len(), 2)?;

        let mut previous = Self::expect_int(&args[0])?;
        for arg in &args[1..] {
            let current = Self::expect_int(arg)?;
            if !comparison.matches(previous, current) {
                return Ok(false);
            }
            previous = current;
        }

        Ok(true)
    }

    fn expect_int(value: &Value) -> Result<i64, EvalError> {
        let Value::Int(number) = value else {
            return Err(EvalError::new("expected number"));
        };
        Ok(*number)
    }

    fn expect_index(value: &Value, operation_name: &str) -> Result<usize, EvalError> {
        let index = Self::expect_int(value)?;
        if index < 0 {
            return Err(EvalError::new(format!("{operation_name} index out of range")));
        }
        Ok(index as usize)
    }

    fn expect_string(value: &Value) -> Result<SchemeString, EvalError> {
        let Value::String(string) = value else {
            return Err(EvalError::new("expected string"));
        };
        Ok(string.clone())
    }

    fn expect_symbol(value: &Value) -> Result<String, EvalError> {
        let Value::Symbol(symbol) = value else {
            return Err(EvalError::new("expected symbol"));
        };
        Ok(symbol.clone())
    }

    fn expect_char(value: &Value) -> Result<char, EvalError> {
        let Value::Char(ch) = value else {
            return Err(EvalError::new("expected character"));
        };
        Ok(*ch)
    }

    fn expect_pair(value: &Value) -> Result<Rc<PairValue>, EvalError> {
        let Value::Pair(pair) = value else {
            return Err(EvalError::new("expected pair"));
        };
        Ok(pair.clone())
    }

    fn length_of_list(value: &Value) -> Result<usize, EvalError> {
        let mut length = 0;
        let mut current = value.clone();
        loop {
            match current {
                Value::Pair(pair) => {
                    length += 1;
                    current = pair.cdr.clone();
                }
                Value::EmptyList => return Ok(length),
                _ => return Err(EvalError::new("expected list")),
            }
        }
    }

    fn list_elements(value: &Value) -> Result<Vec<Value>, EvalError> {
        let mut elements = Vec::new();
        let mut current = value.clone();

        loop {
            match current {
                Value::Pair(pair) => {
                    elements.push(pair.car.clone());
                    current = pair.cdr.clone();
                }
                Value::EmptyList => return Ok(elements),
                _ => return Err(EvalError::new("expected list")),
            }
        }
    }

    fn append_lists(args: &[Value]) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Ok(Value::EmptyList);
        }

        let mut result = args[args.len() - 1].clone();
        for value in args[..args.len() - 1].iter().rev() {
            let elements = Self::list_elements(value)?;
            for element in elements.into_iter().rev() {
                result = Value::Pair(Rc::new(PairValue {
                    car: element,
                    cdr: result,
                }));
            }
        }

        Ok(result)
    }

    fn string_append(args: &[Value]) -> Result<String, EvalError> {
        let mut result = String::new();
        for arg in args {
            result.push_str(&Self::expect_string(arg)?.as_plain_string());
        }
        Ok(result)
    }

    fn make_list(args: &[Value]) -> Value {
        let mut result = Value::EmptyList;
        for arg in args.iter().rev() {
            result = Value::Pair(Rc::new(PairValue {
                car: arg.clone(),
                cdr: result,
            }));
        }
        result
    }

    fn type_predicate<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
    where
        F: Fn(&Value) -> bool,
    {
        Self::require_arity(name, args.len(), 1)?;
        Ok(Value::Bool(predicate(&args[0])))
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

    fn is_truthy(value: &Value) -> bool {
        !matches!(value, Value::Bool(false))
    }
}

#[derive(Clone)]
enum Expr {
    Int { value: i64, position: SourcePos },
    Bool { value: bool, position: SourcePos },
    String { value: String, position: SourcePos },
    Char { value: char, position: SourcePos },
    Symbol { name: String, position: SourcePos },
    List { elements: Vec<Expr>, position: SourcePos },
}

impl Expr {
    fn position(&self) -> SourcePos {
        match self {
            Expr::Int { position, .. }
            | Expr::Bool { position, .. }
            | Expr::String { position, .. }
            | Expr::Char { position, .. }
            | Expr::Symbol { position, .. }
            | Expr::List { position, .. } => *position,
        }
    }
}

#[derive(Clone)]
struct LetBinding {
    name: String,
    value_expr: Expr,
}

#[derive(Copy, Clone)]
struct SourcePos {
    line: usize,
    column: usize,
}

#[derive(Clone)]
enum Value {
    Int(i64),
    Bool(bool),
    String(SchemeString),
    Char(char),
    Symbol(String),
    Pair(Rc<PairValue>),
    EmptyList,
    Void,
    Procedure(Rc<Procedure>),
}

impl Value {
    fn render(&self) -> String {
        match self {
            Value::Int(value) => value.to_string(),
            Value::Bool(true) => "#t".to_owned(),
            Value::Bool(false) => "#f".to_owned(),
            Value::String(value) => format!("\"{}\"", escape_string(&value.as_plain_string())),
            Value::Char(value) => render_char(*value),
            Value::Symbol(name) => name.clone(),
            Value::Pair(_) => {
                let mut builder = String::from("(");
                append_list_contents(&mut builder, self);
                builder.push(')');
                builder
            }
            Value::EmptyList => "()".to_owned(),
            Value::Void => "#<void>".to_owned(),
            Value::Procedure(procedure) => procedure.render(),
        }
    }
}

#[derive(Clone)]
struct SchemeString(Rc<RefCell<Vec<char>>>);

impl SchemeString {
    fn new(value: &str) -> Self {
        Self::new_owned(value.to_owned())
    }

    fn new_owned(value: String) -> Self {
        Self::from_chars(value.chars().collect())
    }

    fn from_chars(chars: Vec<char>) -> Self {
        Self(Rc::new(RefCell::new(chars)))
    }

    fn len(&self) -> usize {
        self.0.borrow().len()
    }

    fn as_plain_string(&self) -> String {
        self.0.borrow().iter().collect()
    }

    fn characters(&self) -> Vec<char> {
        self.0.borrow().clone()
    }

    fn char_at(&self, index: usize) -> Option<char> {
        self.0.borrow().get(index).copied()
    }

    fn set_char(&self, index: usize, value: char) -> bool {
        let mut characters = self.0.borrow_mut();
        if index >= characters.len() {
            return false;
        }
        characters[index] = value;
        true
    }

    fn copy(&self) -> Self {
        Self::from_chars(self.characters())
    }
}

#[derive(Clone)]
struct PairValue {
    car: Value,
    cdr: Value,
}

type BuiltinAction = fn(&mut Interpreter, &[Value]) -> Result<Value, EvalError>;

#[derive(Clone)]
enum Procedure {
    Builtin(BuiltinProcedure),
    User(UserProcedure),
}

impl Procedure {
    fn apply(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, EvalError> {
        match self {
            Procedure::Builtin(procedure) => (procedure.action)(interpreter, &args),
            Procedure::User(procedure) => procedure.apply(interpreter, args),
        }
    }

    fn render(&self) -> String {
        match self {
            Procedure::Builtin(procedure) => format!("#<procedure:{}>", procedure.name),
            Procedure::User(_) => "#<procedure>".to_owned(),
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
    parameter_names: Vec<String>,
    body: Vec<Expr>,
    closure_env: Rc<Environment>,
}

impl UserProcedure {
    fn apply(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, EvalError> {
        Interpreter::require_arity(self.display_name(), args.len(), self.parameter_names.len())?;

        let call_env = Environment::new(Some(self.closure_env.clone()));
        for (parameter_name, value) in self.parameter_names.iter().zip(args) {
            call_env.define(parameter_name.clone(), value);
        }

        interpreter.eval_sequence(&self.body, call_env)
    }

    fn display_name(&self) -> &str {
        self.name.as_deref().unwrap_or("lambda")
    }
}

struct Environment {
    parent: Option<Rc<Environment>>,
    bindings: RefCell<HashMap<String, Value>>,
}

impl Environment {
    fn new(parent: Option<Rc<Environment>>) -> Rc<Self> {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.bindings.borrow_mut().insert(name.into(), value);
    }

    fn lookup(&self, name: &str) -> Result<Value, EvalError> {
        if let Some(value) = self.bindings.borrow().get(name) {
            return Ok(value.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.lookup(name);
        }
        Err(EvalError::new(format!("unbound variable: {name}")))
    }
}

#[derive(Copy, Clone)]
enum Comparison {
    StrictlyLess,
    StrictlyGreater,
    Equal,
    LessOrEqual,
}

impl Comparison {
    fn symbol(self) -> &'static str {
        match self {
            Comparison::StrictlyLess => "<",
            Comparison::StrictlyGreater => ">",
            Comparison::Equal => "=",
            Comparison::LessOrEqual => "<=",
        }
    }

    fn matches(self, left: i64, right: i64) -> bool {
        match self {
            Comparison::StrictlyLess => left < right,
            Comparison::StrictlyGreater => left > right,
            Comparison::Equal => left == right,
            Comparison::LessOrEqual => left <= right,
        }
    }
}

fn builtin_value(name: &'static str, action: BuiltinAction) -> Value {
    Value::Procedure(Rc::new(Procedure::Builtin(BuiltinProcedure { name, action })))
}

fn append_list_contents(builder: &mut String, value: &Value) {
    let mut current = value.clone();
    let mut first = true;

    loop {
        match current {
            Value::Pair(pair) => {
                if !first {
                    builder.push(' ');
                }
                builder.push_str(&pair.car.render());
                current = pair.cdr.clone();
                first = false;
            }
            Value::EmptyList => return,
            _ => break,
        }
    }

    if !first {
        builder.push_str(" . ");
    }
    builder.push_str(&current.render());
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
            Some('\'') => self.parse_quoted(position),
            Some('"') => self.parse_string(position),
            Some(')') => Err(self.error_at(position, "unexpected ')'")),
            Some(_) => self.parse_atom(position),
            None => Err(self.error_at_current("unexpected end of input")),
        }
    }

    fn parse_quoted(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.advance();
        Ok(Expr::List {
            elements: vec![
                Expr::Symbol {
                    name: "quote".to_owned(),
                    position,
                },
                self.parse_expr()?,
            ],
            position,
        })
    }

    fn parse_list(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.advance();
        let mut elements = Vec::new();

        loop {
            self.skip_whitespace();
            if self.index >= self.input.len() {
                return Err(self.error_at(position, "unterminated list"));
            }

            if self.current_char() == Some(')') {
                self.advance();
                return Ok(Expr::List { elements, position });
            }

            elements.push(self.parse_expr()?);
        }
    }

    fn parse_string(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.advance();
        let mut value = String::new();

        while let Some(ch) = self.current_char() {
            let ch = {
                self.advance();
                ch
            };

            if ch == '"' {
                return Ok(Expr::String { value, position });
            }

            if ch == '\\' {
                value.push(self.parse_escape(position)?);
            } else {
                value.push(ch);
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
            return Ok(Expr::Bool {
                value: true,
                position,
            });
        }
        if token == "#f" {
            return Ok(Expr::Bool {
                value: false,
                position,
            });
        }
        if let Some(character) = parse_char_literal(token) {
            return Ok(Expr::Char {
                value: character,
                position,
            });
        }
        if token.starts_with("#\\") {
            return Err(self.error_at(position, "invalid character literal"));
        }
        if let Ok(value) = token.parse::<i64>() {
            return Ok(Expr::Int { value, position });
        }

        Ok(Expr::Symbol {
            name: token.to_owned(),
            position,
        })
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char() {
            if ch.is_whitespace() {
                self.advance();
                continue;
            }

            if ch == ';' {
                self.advance();
                while let Some(comment_char) = self.current_char() {
                    if comment_char == '\n' {
                        break;
                    }
                    self.advance();
                }
                continue;
            }

            break;
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

#[cfg(test)]
mod tests;
