use std::rc::Rc;

use super::environment::{EnvRef, Environment};
use super::error::EvalError;
use super::expr::Expr;
use super::position::SourcePos;
use super::value::{Builtin, Closure, SchemeString, Value};

pub(crate) struct Interpreter {
    global: EnvRef,
    output: String,
}

#[derive(Clone)]
struct Binding {
    name: String,
    value_expr: Expr,
}

#[derive(Clone)]
struct EvaluatedArgument {
    value: Value,
    pos: SourcePos,
}

impl Interpreter {
    pub(crate) fn new() -> Self {
        let global = Environment::new(None);
        for builtin in Builtin::ALL {
            global.define(builtin.name(), Value::Builtin(builtin));
        }
        Self {
            global,
            output: String::new(),
        }
    }

    pub(crate) fn eval_program(&mut self, program: &[Expr]) -> Result<Value, EvalError> {
        let global = Rc::clone(&self.global);
        let mut last = None;
        for expr in program {
            last = Some(self.eval(expr, &global)?);
        }
        last.ok_or_else(|| EvalError::syntax(SourcePos::new(1, 1), "empty program"))
    }

    pub(crate) fn captured_output(&self) -> String {
        self.output.clone()
    }

    fn eval(&mut self, expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
        match expr {
            Expr::Integer(value, _) => Ok(Value::Integer(*value)),
            Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
            Expr::String(value, _) => Ok(Value::String(SchemeString::new(value))),
            Expr::Character(value, _) => Ok(Value::Character(*value)),
            Expr::Symbol(name, pos) => self.lookup_symbol(name, *pos, env),
            Expr::List(elements, pos) => self.eval_list(elements, *pos, env),
        }
    }

    fn eval_list(
        &mut self,
        elements: &[Expr],
        pos: SourcePos,
        env: &EnvRef,
    ) -> Result<Value, EvalError> {
        if elements.is_empty() {
            return Err(EvalError::syntax(pos, "cannot evaluate empty list"));
        }

        let operator = &elements[0];
        if let Expr::Symbol(name, operator_pos) = operator {
            let arguments = &elements[1..];
            return match name.as_str() {
                "define" => self.eval_define(arguments, env, *operator_pos),
                "if" => self.eval_if(arguments, env, *operator_pos),
                "quote" => self.eval_quote(arguments, *operator_pos),
                "lambda" => self.eval_lambda(arguments, env, *operator_pos),
                "begin" => self.eval_begin(arguments, env),
                "let" => self.eval_let(arguments, env, *operator_pos),
                "cond" => self.eval_cond(arguments, env, *operator_pos),
                "and" => self.eval_and(arguments, env),
                "or" => self.eval_or(arguments, env),
                _ => {
                    let operator_value = self.eval(operator, env)?;
                    self.apply(operator_value, operator.pos(), arguments, env)
                }
            };
        }

        let operator_value = self.eval(operator, env)?;
        self.apply(operator_value, operator.pos(), &elements[1..], env)
    }

    fn eval_define(
        &mut self,
        arguments: &[Expr],
        env: &EnvRef,
        pos: SourcePos,
    ) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(EvalError::syntax(pos, "define expects a target and value"));
        }

        match &arguments[0] {
            Expr::Symbol(name, _) => {
                if arguments.len() != 2 {
                    return Err(EvalError::syntax(
                        pos,
                        "define variable form expects exactly one value expression",
                    ));
                }
                let value = self.eval(&arguments[1], env)?;
                env.define(name.clone(), value);
                Ok(Value::Void)
            }
            Expr::List(signature, signature_pos) => {
                if signature.is_empty() {
                    return Err(EvalError::syntax(
                        *signature_pos,
                        "define function form requires a function name",
                    ));
                }

                let Expr::Symbol(name, _) = &signature[0] else {
                    return Err(EvalError::syntax(
                        signature[0].pos(),
                        "function name must be a symbol",
                    ));
                };

                let parameters = self.parse_parameters(&signature[1..])?;
                let body = arguments[1..].to_vec();
                if body.is_empty() {
                    return Err(EvalError::syntax(pos, "define function form requires a body"));
                }

                let closure = Value::Closure(Closure {
                    parameters,
                    body,
                    env: Rc::clone(env),
                });
                env.define(name.clone(), closure);
                Ok(Value::Void)
            }
            target => Err(EvalError::syntax(target.pos(), "invalid define target")),
        }
    }

    fn eval_if(
        &mut self,
        arguments: &[Expr],
        env: &EnvRef,
        pos: SourcePos,
    ) -> Result<Value, EvalError> {
        if arguments.len() != 3 {
            return Err(EvalError::syntax(pos, "if expects exactly 3 arguments"));
        }

        if self.eval(&arguments[0], env)?.is_truthy() {
            self.eval(&arguments[1], env)
        } else {
            self.eval(&arguments[2], env)
        }
    }

    fn eval_quote(&mut self, arguments: &[Expr], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::syntax(pos, "quote expects exactly 1 argument"));
        }
        self.quote(&arguments[0])
    }

    fn eval_lambda(
        &mut self,
        arguments: &[Expr],
        env: &EnvRef,
        pos: SourcePos,
    ) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(EvalError::syntax(
                pos,
                "lambda expects a parameter list and body",
            ));
        }

        let Expr::List(parameters, _) = &arguments[0] else {
            return Err(EvalError::syntax(
                arguments[0].pos(),
                "lambda parameters must be a list",
            ));
        };

        Ok(Value::Closure(Closure {
            parameters: self.parse_parameters(parameters)?,
            body: arguments[1..].to_vec(),
            env: Rc::clone(env),
        }))
    }

    fn eval_begin(&mut self, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        self.eval_sequence(arguments, env)
    }

    fn eval_let(
        &mut self,
        arguments: &[Expr],
        env: &EnvRef,
        pos: SourcePos,
    ) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(EvalError::syntax(pos, "let expects bindings and a body"));
        }

        if let Expr::Symbol(name, _) = &arguments[0] {
            if arguments.len() < 3 {
                return Err(EvalError::syntax(pos, "named let expects bindings and a body"));
            }
            let Expr::List(bindings, _) = &arguments[1] else {
                return Err(EvalError::syntax(
                    arguments[1].pos(),
                    "named let bindings must be a list",
                ));
            };
            return self.eval_named_let(name, bindings, &arguments[2..], env, pos);
        }

        let Expr::List(bindings, _) = &arguments[0] else {
            return Err(EvalError::syntax(
                arguments[0].pos(),
                "let bindings must be a list",
            ));
        };

        let bindings = self.parse_bindings(bindings)?;
        let values = self.eval_binding_values(&bindings, env)?;
        let let_env = Environment::new(Some(Rc::clone(env)));
        for (binding, value) in bindings.iter().zip(values) {
            let_env.define(binding.name.clone(), value);
        }
        self.eval_sequence(&arguments[1..], &let_env)
    }

    fn eval_named_let(
        &mut self,
        name: &str,
        binding_exprs: &[Expr],
        body: &[Expr],
        env: &EnvRef,
        pos: SourcePos,
    ) -> Result<Value, EvalError> {
        let bindings = self.parse_bindings(binding_exprs)?;
        let values = self.eval_binding_values(&bindings, env)?;
        let parameters = bindings.iter().map(|binding| binding.name.clone()).collect();

        let named_env = Environment::new(Some(Rc::clone(env)));
        let closure = Closure {
            parameters,
            body: body.to_vec(),
            env: Rc::clone(&named_env),
        };
        named_env.define(name.to_owned(), Value::Closure(closure.clone()));
        self.apply_closure_values(&closure, values, pos)
    }

    fn parse_bindings(&self, binding_exprs: &[Expr]) -> Result<Vec<Binding>, EvalError> {
        let mut bindings = Vec::with_capacity(binding_exprs.len());
        for binding_expr in binding_exprs {
            let Expr::List(binding_list, binding_pos) = binding_expr else {
                return Err(EvalError::syntax(
                    binding_expr.pos(),
                    "let bindings must be lists",
                ));
            };
            if binding_list.len() != 2 {
                return Err(EvalError::syntax(
                    *binding_pos,
                    "let binding must have exactly 2 elements",
                ));
            }
            let Expr::Symbol(name, _) = &binding_list[0] else {
                return Err(EvalError::syntax(
                    binding_list[0].pos(),
                    "let binding name must be a symbol",
                ));
            };
            bindings.push(Binding {
                name: name.clone(),
                value_expr: binding_list[1].clone(),
            });
        }
        Ok(bindings)
    }

    fn eval_binding_values(
        &mut self,
        bindings: &[Binding],
        env: &EnvRef,
    ) -> Result<Vec<Value>, EvalError> {
        let mut values = Vec::with_capacity(bindings.len());
        for binding in bindings {
            values.push(self.eval(&binding.value_expr, env)?);
        }
        Ok(values)
    }

    fn eval_cond(
        &mut self,
        arguments: &[Expr],
        env: &EnvRef,
        pos: SourcePos,
    ) -> Result<Value, EvalError> {
        if arguments.is_empty() {
            return Err(EvalError::syntax(pos, "cond expects at least one clause"));
        }

        for (index, clause_expr) in arguments.iter().enumerate() {
            let Expr::List(clause, clause_pos) = clause_expr else {
                return Err(EvalError::syntax(
                    clause_expr.pos(),
                    "cond clauses must be lists",
                ));
            };
            if clause.is_empty() {
                return Err(EvalError::syntax(*clause_pos, "cond clause cannot be empty"));
            }

            let test_expr = &clause[0];
            let body = &clause[1..];
            if let Expr::Symbol(name, test_pos) = test_expr {
                if name == "else" {
                    if index != arguments.len() - 1 {
                        return Err(EvalError::syntax(*test_pos, "else clause must be last"));
                    }
                    return self.eval_sequence(body, env);
                }
            }

            let test_value = self.eval(test_expr, env)?;
            if test_value.is_truthy() {
                if body.is_empty() {
                    return Ok(test_value);
                }
                return self.eval_sequence(body, env);
            }
        }

        Ok(Value::Void)
    }

    fn parse_parameters(&self, parameter_exprs: &[Expr]) -> Result<Vec<String>, EvalError> {
        let mut parameters = Vec::with_capacity(parameter_exprs.len());
        for parameter_expr in parameter_exprs {
            let Expr::Symbol(name, _) = parameter_expr else {
                return Err(EvalError::syntax(
                    parameter_expr.pos(),
                    "parameters must be symbols",
                ));
            };
            parameters.push(name.clone());
        }
        Ok(parameters)
    }

    fn quote(&self, expr: &Expr) -> Result<Value, EvalError> {
        match expr {
            Expr::Integer(value, _) => Ok(Value::Integer(*value)),
            Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
            Expr::String(value, _) => Ok(Value::String(SchemeString::new(value))),
            Expr::Character(value, _) => Ok(Value::Character(*value)),
            Expr::Symbol(name, _) => Ok(Value::Symbol(name.clone())),
            Expr::List(elements, _) => {
                let mut values = Vec::with_capacity(elements.len());
                for element in elements {
                    values.push(self.quote(element)?);
                }
                Ok(Value::List(values))
            }
        }
    }

    fn eval_and(&mut self, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        let mut result = Value::Boolean(true);
        for argument in arguments {
            result = self.eval(argument, env)?;
            if !result.is_truthy() {
                return Ok(result);
            }
        }
        Ok(result)
    }

    fn eval_or(&mut self, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        let mut result = Value::Boolean(false);
        for argument in arguments {
            result = self.eval(argument, env)?;
            if result.is_truthy() {
                return Ok(result);
            }
        }
        Ok(result)
    }

    fn lookup_symbol(
        &self,
        name: &str,
        pos: SourcePos,
        env: &EnvRef,
    ) -> Result<Value, EvalError> {
        env.lookup(name)
            .ok_or_else(|| EvalError::at(pos, format!("unbound variable: {name}")))
    }

    fn apply(
        &mut self,
        operator: Value,
        operator_pos: SourcePos,
        argument_exprs: &[Expr],
        env: &EnvRef,
    ) -> Result<Value, EvalError> {
        match operator {
            Value::Builtin(name) => self.apply_builtin(name, argument_exprs, env, operator_pos),
            Value::Closure(closure) => self.apply_closure_exprs(&closure, argument_exprs, env, operator_pos),
            _ => Err(EvalError::at(operator_pos, "attempted to call non-procedure")),
        }
    }

    fn apply_closure_exprs(
        &mut self,
        closure: &Closure,
        argument_exprs: &[Expr],
        env: &EnvRef,
        pos: SourcePos,
    ) -> Result<Value, EvalError> {
        let arguments = self.eval_arguments(argument_exprs, env)?;
        self.apply_closure_values(closure, arguments, pos)
    }

    fn apply_closure_values(
        &mut self,
        closure: &Closure,
        arguments: Vec<Value>,
        pos: SourcePos,
    ) -> Result<Value, EvalError> {
        if arguments.len() != closure.parameters.len() {
            return Err(EvalError::arity(
                pos,
                "lambda",
                format!("expected {} arguments", closure.parameters.len()),
            ));
        }

        let call_env = Environment::new(Some(Rc::clone(&closure.env)));
        for (parameter, argument) in closure.parameters.iter().zip(arguments) {
            call_env.define(parameter.clone(), argument);
        }
        self.eval_sequence(&closure.body, &call_env)
    }

    fn eval_sequence(&mut self, expressions: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        let mut last = Value::Void;
        for expression in expressions {
            last = self.eval(expression, env)?;
        }
        Ok(last)
    }

    fn eval_arguments(
        &mut self,
        argument_exprs: &[Expr],
        env: &EnvRef,
    ) -> Result<Vec<Value>, EvalError> {
        let mut arguments = Vec::with_capacity(argument_exprs.len());
        for argument_expr in argument_exprs {
            arguments.push(self.eval(argument_expr, env)?);
        }
        Ok(arguments)
    }

    fn eval_arguments_with_positions(
        &mut self,
        argument_exprs: &[Expr],
        env: &EnvRef,
    ) -> Result<Vec<EvaluatedArgument>, EvalError> {
        let mut arguments = Vec::with_capacity(argument_exprs.len());
        for argument_expr in argument_exprs {
            arguments.push(EvaluatedArgument {
                value: self.eval(argument_expr, env)?,
                pos: argument_expr.pos(),
            });
        }
        Ok(arguments)
    }

    fn apply_builtin(
        &mut self,
        name: Builtin,
        argument_exprs: &[Expr],
        env: &EnvRef,
        pos: SourcePos,
    ) -> Result<Value, EvalError> {
        let arguments = self.eval_arguments_with_positions(argument_exprs, env)?;

        match name {
            Builtin::Add => self.add(&arguments),
            Builtin::Subtract => self.subtract(&arguments, pos),
            Builtin::Multiply => self.multiply(&arguments),
            Builtin::Divide => self.divide(&arguments, pos),
            Builtin::LessThan => self.compare(&arguments, "<", |left, right| left < right),
            Builtin::GreaterThan => self.compare(&arguments, ">", |left, right| left > right),
            Builtin::Equal => self.compare(&arguments, "=", |left, right| left == right),
            Builtin::LessEqual => self.compare(&arguments, "<=", |left, right| left <= right),
            Builtin::Not => self.not(&arguments, pos),
            Builtin::Cons => self.cons(&arguments, pos),
            Builtin::Car => self.car(&arguments, pos),
            Builtin::Cdr => self.cdr(&arguments, pos),
            Builtin::NullPredicate => self.null_predicate(&arguments, pos),
            Builtin::List => Ok(self.list(&arguments)),
            Builtin::Length => self.length(&arguments, pos),
            Builtin::Append => self.append(&arguments, pos),
            Builtin::StringPredicate => self.predicate("string?", &arguments, pos, |value| matches!(value, Value::String(_))),
            Builtin::NumberPredicate => self.predicate("number?", &arguments, pos, |value| matches!(value, Value::Integer(_))),
            Builtin::BooleanPredicate => self.predicate("boolean?", &arguments, pos, |value| matches!(value, Value::Boolean(_))),
            Builtin::PairPredicate => self.predicate("pair?", &arguments, pos, |value| matches!(value, Value::List(elements) if !elements.is_empty())),
            Builtin::SymbolPredicate => self.predicate("symbol?", &arguments, pos, |value| matches!(value, Value::Symbol(_))),
            Builtin::Display => self.display(&arguments, pos),
            Builtin::Write => self.write(&arguments, pos),
            Builtin::Newline => self.newline(&arguments, pos),
            Builtin::StringAppend => self.string_append(&arguments),
            Builtin::StringLength => self.string_length(&arguments, pos),
            Builtin::Substring => self.substring(&arguments, pos),
            Builtin::StringToNumber => self.string_to_number(&arguments, pos),
            Builtin::NumberToString => self.number_to_string(&arguments, pos),
            Builtin::SymbolToString => self.symbol_to_string(&arguments, pos),
            Builtin::StringToSymbol => self.string_to_symbol(&arguments, pos),
            Builtin::StringRef => self.string_ref(&arguments, pos),
            Builtin::CharPredicate => self.predicate("char?", &arguments, pos, |value| matches!(value, Value::Character(_))),
            Builtin::StringSet => self.string_set(&arguments, pos),
            Builtin::StringCopy => self.string_copy(&arguments, pos),
        }
    }

    fn display(&mut self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::arity(pos, "display", "expected exactly 1 argument"));
        }
        self.output.push_str(&arguments[0].value.render_for_display());
        Ok(Value::Void)
    }

    fn write(&mut self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::arity(pos, "write", "expected exactly 1 argument"));
        }
        self.output.push_str(&arguments[0].value.render());
        Ok(Value::Void)
    }

    fn newline(&mut self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if !arguments.is_empty() {
            return Err(EvalError::arity(pos, "newline", "expected exactly 0 arguments"));
        }
        self.output.push('\n');
        Ok(Value::Void)
    }

    fn add(&self, arguments: &[EvaluatedArgument]) -> Result<Value, EvalError> {
        let mut total = 0_i64;
        for argument in arguments {
            total += self.expect_integer(argument, "+")?;
        }
        Ok(Value::Integer(total))
    }

    fn subtract(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.is_empty() {
            return Err(EvalError::arity(pos, "-", "expected at least 1 argument"));
        }

        let mut result = self.expect_integer(&arguments[0], "-")?;
        if arguments.len() == 1 {
            return Ok(Value::Integer(-result));
        }

        for argument in &arguments[1..] {
            result -= self.expect_integer(argument, "-")?;
        }
        Ok(Value::Integer(result))
    }

    fn multiply(&self, arguments: &[EvaluatedArgument]) -> Result<Value, EvalError> {
        let mut product = 1_i64;
        for argument in arguments {
            product *= self.expect_integer(argument, "*")?;
        }
        Ok(Value::Integer(product))
    }

    fn divide(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(EvalError::arity(pos, "/", "expected at least 2 arguments"));
        }

        let mut result = self.expect_integer(&arguments[0], "/")?;
        for argument in &arguments[1..] {
            let divisor = self.expect_integer(argument, "/")?;
            if divisor == 0 {
                return Err(EvalError::at(argument.pos, "division by zero"));
            }
            result /= divisor;
        }
        Ok(Value::Integer(result))
    }

    fn compare<F>(
        &self,
        arguments: &[EvaluatedArgument],
        name: &str,
        comparison: F,
    ) -> Result<Value, EvalError>
    where
        F: Fn(i64, i64) -> bool,
    {
        if arguments.len() <= 1 {
            return Ok(Value::Boolean(true));
        }

        let mut previous = self.expect_integer(&arguments[0], name)?;
        for argument in &arguments[1..] {
            let current = self.expect_integer(argument, name)?;
            if !comparison(previous, current) {
                return Ok(Value::Boolean(false));
            }
            previous = current;
        }
        Ok(Value::Boolean(true))
    }

    fn not(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::arity(pos, "not", "expected exactly 1 argument"));
        }
        Ok(Value::Boolean(!arguments[0].value.is_truthy()))
    }

    fn cons(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 2 {
            return Err(EvalError::arity(pos, "cons", "expected exactly 2 arguments"));
        }

        let mut elements = Vec::new();
        elements.push(arguments[0].value.clone());
        elements.extend(self.expect_list(&arguments[1], "cons")?);
        Ok(Value::List(elements))
    }

    fn car(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::arity(pos, "car", "expected exactly 1 argument"));
        }

        let list = self.expect_non_empty_list(&arguments[0], "car")?;
        Ok(list[0].clone())
    }

    fn cdr(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::arity(pos, "cdr", "expected exactly 1 argument"));
        }

        let list = self.expect_non_empty_list(&arguments[0], "cdr")?;
        Ok(Value::List(list[1..].to_vec()))
    }

    fn null_predicate(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::arity(pos, "null?", "expected exactly 1 argument"));
        }

        Ok(Value::Boolean(matches!(
            &arguments[0].value,
            Value::List(elements) if elements.is_empty()
        )))
    }

    fn list(&self, arguments: &[EvaluatedArgument]) -> Value {
        Value::List(arguments.iter().map(|argument| argument.value.clone()).collect())
    }

    fn length(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::arity(pos, "length", "expected exactly 1 argument"));
        }
        Ok(Value::Integer(self.expect_list(&arguments[0], "length")?.len() as i64))
    }

    fn append(&self, arguments: &[EvaluatedArgument], _pos: SourcePos) -> Result<Value, EvalError> {
        let mut combined = Vec::new();
        for argument in arguments {
            combined.extend(self.expect_list(argument, "append")?);
        }
        Ok(Value::List(combined))
    }

    fn string_append(&self, arguments: &[EvaluatedArgument]) -> Result<Value, EvalError> {
        let mut builder = String::new();
        for argument in arguments {
            builder.push_str(&self.expect_string(argument, "string-append")?.as_plain_string());
        }
        Ok(Value::String(SchemeString::new(builder)))
    }

    fn string_length(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::arity(
                pos,
                "string-length",
                "expected exactly 1 argument",
            ));
        }
        Ok(Value::Integer(self.expect_string(&arguments[0], "string-length")?.len() as i64))
    }

    fn substring(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 3 {
            return Err(EvalError::arity(pos, "substring", "expected exactly 3 arguments"));
        }

        let value = self.expect_string(&arguments[0], "substring")?;
        let start = self.expect_integer(&arguments[1], "substring")?;
        let end = self.expect_integer(&arguments[2], "substring")?;
        if start < 0 || start as usize > value.len() {
            return Err(EvalError::at(arguments[1].pos, "substring: start index out of range"));
        }
        if end < start || end as usize > value.len() {
            return Err(EvalError::at(arguments[2].pos, "substring: end index out of range"));
        }

        Ok(Value::String(value.substring(start as usize, end as usize)))
    }

    fn string_to_number(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::arity(
                pos,
                "string->number",
                "expected exactly 1 argument",
            ));
        }

        match self
            .expect_string(&arguments[0], "string->number")?
            .as_plain_string()
            .parse::<i64>()
        {
            Ok(value) => Ok(Value::Integer(value)),
            Err(_) => Ok(Value::Boolean(false)),
        }
    }

    fn number_to_string(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::arity(
                pos,
                "number->string",
                "expected exactly 1 argument",
            ));
        }
        Ok(Value::String(SchemeString::new(
            self.expect_integer(&arguments[0], "number->string")?.to_string(),
        )))
    }

    fn symbol_to_string(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::arity(
                pos,
                "symbol->string",
                "expected exactly 1 argument",
            ));
        }
        Ok(Value::String(SchemeString::new(
            self.expect_symbol(&arguments[0], "symbol->string")?,
        )))
    }

    fn string_to_symbol(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::arity(
                pos,
                "string->symbol",
                "expected exactly 1 argument",
            ));
        }
        Ok(Value::Symbol(
            self.expect_string(&arguments[0], "string->symbol")?
                .as_plain_string(),
        ))
    }

    fn string_ref(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 2 {
            return Err(EvalError::arity(pos, "string-ref", "expected exactly 2 arguments"));
        }

        let value = self.expect_string(&arguments[0], "string-ref")?;
        let index = self.expect_integer(&arguments[1], "string-ref")?;
        if index < 0 || index as usize >= value.len() {
            return Err(EvalError::at(arguments[1].pos, "string-ref: index out of range"));
        }

        Ok(Value::Character(value.char_at(index as usize).unwrap()))
    }

    fn string_set(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 3 {
            return Err(EvalError::arity(pos, "string-set!", "expected exactly 3 arguments"));
        }

        let value = self.expect_string(&arguments[0], "string-set!")?;
        let index = self.expect_integer(&arguments[1], "string-set!")?;
        let character = self.expect_character(&arguments[2], "string-set!")?;
        if index < 0 || !value.set_char(index as usize, character) {
            return Err(EvalError::at(arguments[1].pos, "string-set!: index out of range"));
        }

        Ok(Value::Void)
    }

    fn string_copy(&self, arguments: &[EvaluatedArgument], pos: SourcePos) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::arity(pos, "string-copy", "expected exactly 1 argument"));
        }
        Ok(Value::String(
            self.expect_string(&arguments[0], "string-copy")?.copy(),
        ))
    }

    fn predicate<F>(
        &self,
        name: &str,
        arguments: &[EvaluatedArgument],
        pos: SourcePos,
        predicate: F,
    ) -> Result<Value, EvalError>
    where
        F: Fn(&Value) -> bool,
    {
        if arguments.len() != 1 {
            return Err(EvalError::arity(pos, name, "expected exactly 1 argument"));
        }
        Ok(Value::Boolean(predicate(&arguments[0].value)))
    }

    fn expect_list(&self, argument: &EvaluatedArgument, name: &str) -> Result<Vec<Value>, EvalError> {
        let Value::List(elements) = &argument.value else {
            return Err(EvalError::type_error(
                argument.pos,
                format!("{name} expects list arguments"),
            ));
        };
        Ok(elements.clone())
    }

    fn expect_non_empty_list(
        &self,
        argument: &EvaluatedArgument,
        name: &str,
    ) -> Result<Vec<Value>, EvalError> {
        let list = self.expect_list(argument, name)?;
        if list.is_empty() {
            return Err(EvalError::type_error(
                argument.pos,
                format!("{name} expects a non-empty list"),
            ));
        }
        Ok(list)
    }

    fn expect_string(&self, argument: &EvaluatedArgument, name: &str) -> Result<SchemeString, EvalError> {
        let Value::String(value) = &argument.value else {
            return Err(EvalError::type_error(
                argument.pos,
                format!("{name} expects string arguments"),
            ));
        };
        Ok(value.clone())
    }

    fn expect_symbol(&self, argument: &EvaluatedArgument, name: &str) -> Result<String, EvalError> {
        let Value::Symbol(value) = &argument.value else {
            return Err(EvalError::type_error(
                argument.pos,
                format!("{name} expects symbol arguments"),
            ));
        };
        Ok(value.clone())
    }

    fn expect_integer(&self, argument: &EvaluatedArgument, name: &str) -> Result<i64, EvalError> {
        let Value::Integer(value) = &argument.value else {
            return Err(EvalError::type_error(
                argument.pos,
                format!("{name} expects integer arguments"),
            ));
        };
        Ok(*value)
    }

    fn expect_character(&self, argument: &EvaluatedArgument, name: &str) -> Result<char, EvalError> {
        let Value::Character(value) = &argument.value else {
            return Err(EvalError::type_error(
                argument.pos,
                format!("{name} expects character arguments"),
            ));
        };
        Ok(*value)
    }
}
