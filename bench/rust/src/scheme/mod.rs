pub mod error;

pub use error::EvalError;

use std::cmp::Ordering;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
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
    let mut interpreter = Interpreter::default();
    interpreter.eval_str(input)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut interpreter = Interpreter::default();
    interpreter.eval_str_with_output(input)
}

#[derive(Default)]
struct Interpreter {
    output: String,
    macros: HashMap<String, MacroDefinition>,
    next_hygiene_id: usize,
}

type EnvRef = Rc<RefCell<Environment>>;
type StringCell = Rc<RefCell<Vec<char>>>;

impl Interpreter {
    fn eval_str(&mut self, input: &str) -> Result<String, EvalError> {
        Ok(self.evaluate_with_output(input)?.0.render())
    }

    fn eval_str_with_output(&mut self, input: &str) -> Result<(String, String), EvalError> {
        let (result, output) = self.evaluate_with_output(input)?;
        Ok((result.render(), output))
    }

    fn evaluate_with_output(&mut self, input: &str) -> Result<(Value, String), EvalError> {
        let previous_output = std::mem::take(&mut self.output);
        let evaluation = self
            .evaluate_program(input)
            .map(|result| (result, self.output.clone()));
        self.output = previous_output;
        evaluation
    }

    fn evaluate_program(&mut self, input: &str) -> Result<Value, EvalError> {
        let expressions = Parser::new(input).parse_program()?;
        if expressions.is_empty() {
            return Err(EvalError::new("empty input"));
        }

        let global = self.create_global_environment();
        let mut result = Value::Void;
        for expression in &expressions {
            result = self.eval(expression, &global)?;
        }
        Ok(result)
    }

    fn create_global_environment(&self) -> EnvRef {
        let env = Environment::new(None);

        self.install_builtin(&env, "+", Builtin::Add);
        self.install_builtin(&env, "-", Builtin::Subtract);
        self.install_builtin(&env, "*", Builtin::Multiply);
        self.install_builtin(&env, "/", Builtin::Divide);
        self.install_builtin(&env, "<", Builtin::LessThan);
        self.install_builtin(&env, ">", Builtin::GreaterThan);
        self.install_builtin(&env, "=", Builtin::EqualNumber);
        self.install_builtin(&env, "<=", Builtin::LessEqual);
        self.install_builtin(&env, "cons", Builtin::Cons);
        self.install_builtin(&env, "car", Builtin::Car);
        self.install_builtin(&env, "cdr", Builtin::Cdr);
        self.install_builtin(&env, "null?", Builtin::NullPredicate);
        self.install_builtin(&env, "list", Builtin::List);
        self.install_builtin(&env, "length", Builtin::Length);
        self.install_builtin(&env, "append", Builtin::Append);
        self.install_builtin(&env, "display", Builtin::Display);
        self.install_builtin(&env, "write", Builtin::Write);
        self.install_builtin(&env, "newline", Builtin::Newline);
        self.install_builtin(&env, "string?", Builtin::StringPredicate);
        self.install_builtin(&env, "string-append", Builtin::StringAppend);
        self.install_builtin(&env, "string-length", Builtin::StringLength);
        self.install_builtin(&env, "substring", Builtin::Substring);
        self.install_builtin(&env, "string-copy", Builtin::StringCopy);
        self.install_builtin(&env, "string-set!", Builtin::StringSet);
        self.install_builtin(&env, "string->number", Builtin::StringToNumber);
        self.install_builtin(&env, "number->string", Builtin::NumberToString);
        self.install_builtin(&env, "symbol->string", Builtin::SymbolToString);
        self.install_builtin(&env, "string->symbol", Builtin::StringToSymbol);
        self.install_builtin(&env, "string-ref", Builtin::StringRef);
        self.install_builtin(&env, "number?", Builtin::NumberPredicate);
        self.install_builtin(&env, "exact?", Builtin::ExactPredicate);
        self.install_builtin(&env, "inexact?", Builtin::InexactPredicate);
        self.install_builtin(&env, "integer?", Builtin::IntegerPredicate);
        self.install_builtin(&env, "rational?", Builtin::RationalPredicate);
        self.install_builtin(&env, "exact->inexact", Builtin::ExactToInexact);
        self.install_builtin(&env, "inexact->exact", Builtin::InexactToExact);
        self.install_builtin(&env, "numerator", Builtin::Numerator);
        self.install_builtin(&env, "denominator", Builtin::Denominator);
        self.install_builtin(&env, "boolean?", Builtin::BooleanPredicate);
        self.install_builtin(&env, "char?", Builtin::CharPredicate);
        self.install_builtin(&env, "pair?", Builtin::PairPredicate);
        self.install_builtin(&env, "symbol?", Builtin::SymbolPredicate);
        self.install_builtin(&env, "eq?", Builtin::Eq);
        self.install_builtin(&env, "equal?", Builtin::Equal);
        self.install_builtin(&env, "abs", Builtin::Abs);
        self.install_builtin(&env, "modulo", Builtin::Modulo);
        self.install_builtin(&env, "remainder", Builtin::Remainder);
        self.install_builtin(&env, "quotient", Builtin::Quotient);
        self.install_builtin(&env, "min", Builtin::Min);
        self.install_builtin(&env, "max", Builtin::Max);
        self.install_builtin(&env, "expt", Builtin::Expt);
        self.install_builtin(&env, "zero?", Builtin::ZeroPredicate);
        self.install_builtin(&env, "positive?", Builtin::PositivePredicate);
        self.install_builtin(&env, "negative?", Builtin::NegativePredicate);
        self.install_builtin(&env, "odd?", Builtin::OddPredicate);
        self.install_builtin(&env, "even?", Builtin::EvenPredicate);
        self.install_builtin(&env, "list-ref", Builtin::ListRef);
        self.install_builtin(&env, "list-tail", Builtin::ListTail);
        self.install_builtin(&env, "list?", Builtin::ListPredicate);
        self.install_builtin(&env, "assoc", Builtin::Assoc);
        self.install_builtin(&env, "map", Builtin::Map);
        self.install_builtin(&env, "char-alphabetic?", Builtin::CharAlphabeticPredicate);
        self.install_builtin(&env, "char-numeric?", Builtin::CharNumericPredicate);
        self.install_builtin(&env, "char-upcase", Builtin::CharUpcase);
        self.install_builtin(&env, "char-downcase", Builtin::CharDowncase);
        self.install_builtin(&env, "char=?", Builtin::CharEquals);
        self.install_builtin(&env, "char<?", Builtin::CharLessThan);
        self.install_builtin(&env, "string=?", Builtin::StringEquals);
        self.install_builtin(&env, "string<?", Builtin::StringLessThan);
        self.install_builtin(&env, "string-ci=?", Builtin::StringCaseInsensitiveEquals);
        self.install_builtin(&env, "string-upcase", Builtin::StringUpcase);
        self.install_builtin(&env, "string-downcase", Builtin::StringDowncase);
        self.install_builtin(&env, "not", Builtin::Not);
        self.install_builtin(&env, "apply", Builtin::Apply);

        env
    }

    fn install_builtin(&self, env: &EnvRef, name: &str, builtin: Builtin) {
        Environment::define(env, name.to_owned(), Value::Builtin(builtin));
    }

    fn eval(&mut self, expression: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
        let value = match expression {
            Expr::Number(number, _) => Ok(Value::Number(*number)),
            Expr::Bool(boolean, _) => Ok(Value::Bool(*boolean)),
            Expr::String(text, _) => Ok(Value::String(string_cell(text))),
            Expr::Char(ch, _) => Ok(Value::Char(*ch)),
            Expr::Symbol(name, _) => Environment::lookup(env, name),
            Expr::List(elements, pos) => self.eval_list(elements, *pos, env),
        };
        value.map_err(|error| self.with_position(error, expression.pos()))
    }

    fn eval_list(
        &mut self,
        elements: &[Expr],
        list_pos: SourcePos,
        env: &EnvRef,
    ) -> Result<Value, EvalError> {
        if elements.is_empty() {
            return Err(EvalError::new("cannot evaluate empty list"));
        }

        let operator_expr = &elements[0];
        let arguments = &elements[1..];

        if let Expr::Symbol(name, _) = operator_expr {
            return match name.as_str() {
                "and" => self.eval_and(arguments, env),
                "begin" => self.eval_begin(arguments, env),
                "cond" => self.eval_cond(arguments, env),
                "or" => self.eval_or(arguments, env),
                "define" => self.eval_define(arguments, env),
                "define-syntax" => self.eval_define_syntax(arguments, env),
                "if" => self.eval_if(arguments, env),
                "let" => self.eval_let(arguments, env),
                "lambda" => self.eval_lambda(arguments, env),
                "quote" => self.eval_quote(arguments),
                "set!" => self.eval_set(arguments, env),
                _ => {
                    if let Some(macro_definition) = self.macros.get(name).cloned() {
                        return self.eval_macro_invocation(
                            &macro_definition,
                            elements,
                            list_pos,
                            env,
                        );
                    }
                    let operator = self.eval(operator_expr, env)?;
                    let evaluated_arguments = self.eval_arguments(arguments, env)?;
                    self.apply(operator, operator_expr.pos(), evaluated_arguments, list_pos)
                }
            };
        }

        let operator = self.eval(operator_expr, env)?;
        let evaluated_arguments = self.eval_arguments(arguments, env)?;
        self.apply(operator, operator_expr.pos(), evaluated_arguments, list_pos)
    }

    fn eval_arguments(
        &mut self,
        arguments: &[Expr],
        env: &EnvRef,
    ) -> Result<Vec<LocatedValue>, EvalError> {
        let mut values = Vec::with_capacity(arguments.len());
        for argument in arguments {
            values.push(LocatedValue {
                value: self.eval(argument, env)?,
                pos: argument.pos(),
            });
        }
        Ok(values)
    }

    fn eval_and(&mut self, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        let mut result = Value::Bool(true);
        for argument in arguments {
            result = self.eval(argument, env)?;
            if !self.is_truthy(&result) {
                return Ok(result);
            }
        }
        Ok(result)
    }

    fn eval_or(&mut self, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        let mut result = Value::Bool(false);
        for argument in arguments {
            result = self.eval(argument, env)?;
            if self.is_truthy(&result) {
                return Ok(result);
            }
        }
        Ok(result)
    }

    fn eval_begin(&mut self, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        self.eval_sequence(arguments, env)
    }

    fn eval_cond(&mut self, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        for (index, clause_expr) in arguments.iter().enumerate() {
            let Expr::List(clause, _) = clause_expr else {
                return Err(EvalError::new("invalid cond"));
            };
            if clause.is_empty() {
                return Err(EvalError::new("invalid cond"));
            }

            let test_expr = &clause[0];
            let is_else_clause = matches!(test_expr, Expr::Symbol(name, _) if name == "else");
            if is_else_clause {
                if index + 1 != arguments.len() {
                    return Err(EvalError::new("invalid cond"));
                }
                return if clause.len() == 1 {
                    Ok(Value::Bool(true))
                } else {
                    self.eval_sequence(&clause[1..], env)
                };
            }

            let test_value = self.eval(test_expr, env)?;
            if self.is_truthy(&test_value) {
                return if clause.len() == 1 {
                    Ok(test_value)
                } else {
                    self.eval_sequence(&clause[1..], env)
                };
            }
        }
        Ok(Value::Void)
    }

    fn eval_define(&mut self, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        if arguments.is_empty() {
            return Err(EvalError::new("invalid define"));
        }

        match &arguments[0] {
            Expr::Symbol(name, _) => {
                if arguments.len() != 2 {
                    return Err(EvalError::new("invalid define"));
                }
                let value = self.eval(&arguments[1], env)?;
                Environment::define(env, name.clone(), value);
                Ok(Value::Void)
            }
            Expr::List(signature, _) => {
                if signature.is_empty() {
                    return Err(EvalError::new("invalid define"));
                }
                let Expr::Symbol(name, _) = &signature[0] else {
                    return Err(EvalError::new("invalid define"));
                };
                if arguments.len() < 2 {
                    return Err(EvalError::new("invalid define"));
                }

                let parameters = self.parse_parameters(&signature[1..])?;
                let lambda = Value::Lambda(Rc::new(LambdaProcedure {
                    name: Some(name.clone()),
                    parameters,
                    body: arguments[1..].to_vec(),
                    closure: Rc::clone(env),
                }));
                Environment::define(env, name.clone(), lambda);
                Ok(Value::Void)
            }
            _ => Err(EvalError::new("invalid define")),
        }
    }

    fn eval_define_syntax(&mut self, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        if arguments.len() != 2 {
            return Err(EvalError::new("invalid define-syntax"));
        }

        let Expr::Symbol(name, _) = &arguments[0] else {
            return Err(EvalError::new("invalid define-syntax"));
        };

        let macro_definition = self.parse_macro_definition(name, &arguments[1], env)?;
        self.macros.insert(name.clone(), macro_definition);
        Ok(Value::Void)
    }

    fn eval_macro_invocation(
        &mut self,
        macro_definition: &MacroDefinition,
        elements: &[Expr],
        list_pos: SourcePos,
        env: &EnvRef,
    ) -> Result<Value, EvalError> {
        let expansion =
            self.expand_macro(macro_definition, &Expr::List(elements.to_vec(), list_pos))?;
        let macro_env = Environment::new(Some(Rc::clone(env)));
        for (name, value) in expansion.capture_bindings {
            Environment::define(&macro_env, name, value);
        }
        self.eval(&expansion.expr, &macro_env)
    }

    fn parse_macro_definition(
        &self,
        name: &str,
        rules_expr: &Expr,
        env: &EnvRef,
    ) -> Result<MacroDefinition, EvalError> {
        let Expr::List(parts, _) = rules_expr else {
            return Err(EvalError::new("invalid define-syntax"));
        };
        if parts.len() < 3 {
            return Err(EvalError::new("invalid define-syntax"));
        }

        let Expr::Symbol(keyword, _) = &parts[0] else {
            return Err(EvalError::new("invalid define-syntax"));
        };
        if keyword != "syntax-rules" {
            return Err(EvalError::new("invalid define-syntax"));
        }

        let Expr::List(literal_exprs, _) = &parts[1] else {
            return Err(EvalError::new("invalid define-syntax"));
        };
        let mut literals = HashSet::new();
        for literal_expr in literal_exprs {
            let Expr::Symbol(literal, _) = literal_expr else {
                return Err(EvalError::new("invalid define-syntax"));
            };
            literals.insert(literal.clone());
        }

        let mut rules = Vec::with_capacity(parts.len() - 2);
        for rule_expr in &parts[2..] {
            let Expr::List(rule_parts, _) = rule_expr else {
                return Err(EvalError::new("invalid define-syntax"));
            };
            if rule_parts.len() != 2 {
                return Err(EvalError::new("invalid define-syntax"));
            }
            rules.push(MacroRule {
                pattern: rule_parts[0].clone(),
                template: rule_parts[1].clone(),
            });
        }

        let mut definition_macros = self.macros.keys().cloned().collect::<HashSet<_>>();
        definition_macros.insert(name.to_owned());

        Ok(MacroDefinition {
            name: name.to_owned(),
            literals,
            rules,
            definition_env: Rc::clone(env),
            definition_macros,
        })
    }

    fn expand_macro(
        &mut self,
        macro_definition: &MacroDefinition,
        call_expr: &Expr,
    ) -> Result<MacroExpansion, EvalError> {
        for rule in &macro_definition.rules {
            if let Some(bindings) = match_macro_pattern(
                &rule.pattern,
                call_expr,
                &macro_definition.name,
                &macro_definition.literals,
            ) {
                let mut state = MacroExpansionState::default();
                let expanded = self.expand_macro_template(
                    &rule.template,
                    macro_definition,
                    &bindings,
                    &HashMap::new(),
                    &mut state,
                    None,
                )?;
                return Ok(MacroExpansion {
                    expr: expanded,
                    capture_bindings: state.capture_bindings,
                });
            }
        }

        Err(EvalError::new(format!(
            "no matching syntax-rules pattern for {}",
            macro_definition.name
        )))
    }

    fn expand_macro_template(
        &mut self,
        template: &Expr,
        macro_definition: &MacroDefinition,
        bindings: &MacroBindings,
        rename_env: &HashMap<String, String>,
        state: &mut MacroExpansionState,
        repeat_index: Option<usize>,
    ) -> Result<Expr, EvalError> {
        match template {
            Expr::Number(_, _) | Expr::Bool(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {
                Ok(template.clone())
            }
            Expr::Symbol(name, pos) => {
                if name == "..." {
                    return Err(EvalError::new("invalid syntax-rules template"));
                }

                if let Some(renamed) = rename_env.get(name) {
                    return Ok(Expr::Symbol(renamed.clone(), *pos));
                }

                if let Some(bound) = bindings.lookup(name, repeat_index) {
                    return Ok(bound.clone());
                }

                if is_special_form(name) || macro_definition.definition_macros.contains(name) {
                    return Ok(Expr::Symbol(name.clone(), *pos));
                }

                if let Some(alias) = state.capture_aliases.get(name) {
                    return Ok(Expr::Symbol(alias.clone(), *pos));
                }

                if let Ok(value) = Environment::lookup(&macro_definition.definition_env, name) {
                    let alias = self.fresh_hygienic_name(name);
                    state.capture_aliases.insert(name.clone(), alias.clone());
                    state.capture_bindings.push((alias.clone(), value));
                    return Ok(Expr::Symbol(alias, *pos));
                }

                Ok(Expr::Symbol(name.clone(), *pos))
            }
            Expr::List(elements, pos) => {
                if let Some(expanded) = self.expand_let_template(
                    elements,
                    *pos,
                    macro_definition,
                    bindings,
                    rename_env,
                    state,
                    repeat_index,
                )? {
                    return Ok(expanded);
                }

                let mut expanded_elements = Vec::new();
                let mut index = 0;
                while index < elements.len() {
                    if index + 1 < elements.len() && is_ellipsis_expr(&elements[index + 1]) {
                        let repeat_count =
                            repetition_count_for_template(&elements[index], bindings)?;
                        for nested_index in 0..repeat_count {
                            expanded_elements.push(self.expand_macro_template(
                                &elements[index],
                                macro_definition,
                                bindings,
                                rename_env,
                                state,
                                Some(nested_index),
                            )?);
                        }
                        index += 2;
                        continue;
                    }

                    expanded_elements.push(self.expand_macro_template(
                        &elements[index],
                        macro_definition,
                        bindings,
                        rename_env,
                        state,
                        repeat_index,
                    )?);
                    index += 1;
                }

                Ok(Expr::List(expanded_elements, *pos))
            }
        }
    }

    fn expand_let_template(
        &mut self,
        elements: &[Expr],
        pos: SourcePos,
        macro_definition: &MacroDefinition,
        bindings: &MacroBindings,
        rename_env: &HashMap<String, String>,
        state: &mut MacroExpansionState,
        repeat_index: Option<usize>,
    ) -> Result<Option<Expr>, EvalError> {
        let Some(Expr::Symbol(keyword, keyword_pos)) = elements.first() else {
            return Ok(None);
        };
        if keyword != "let" {
            return Ok(None);
        }
        if elements.len() < 3 {
            return Err(EvalError::new("invalid syntax-rules template"));
        }

        let Expr::List(binding_exprs, bindings_pos) = &elements[1] else {
            return Ok(None);
        };

        let mut body_renames = rename_env.clone();
        let mut expanded_bindings = Vec::with_capacity(binding_exprs.len());
        for binding_expr in binding_exprs {
            let Expr::List(binding_parts, binding_pos) = binding_expr else {
                return Err(EvalError::new("invalid syntax-rules template"));
            };
            if binding_parts.len() != 2 {
                return Err(EvalError::new("invalid syntax-rules template"));
            }

            let Expr::Symbol(binding_name, binding_name_pos) = &binding_parts[0] else {
                return Err(EvalError::new("invalid syntax-rules template"));
            };

            let expanded_name =
                if let Some(bound_name) = bindings.lookup(binding_name, repeat_index) {
                    let Expr::Symbol(_, _) = bound_name else {
                        return Err(EvalError::new("invalid syntax-rules template"));
                    };
                    bound_name.clone()
                } else {
                    let renamed = self.fresh_hygienic_name(binding_name);
                    body_renames.insert(binding_name.clone(), renamed.clone());
                    Expr::Symbol(renamed, *binding_name_pos)
                };

            let expanded_value = self.expand_macro_template(
                &binding_parts[1],
                macro_definition,
                bindings,
                rename_env,
                state,
                repeat_index,
            )?;

            expanded_bindings.push(Expr::List(
                vec![expanded_name, expanded_value],
                *binding_pos,
            ));
        }

        let mut expanded_elements = Vec::with_capacity(elements.len());
        expanded_elements.push(Expr::Symbol(keyword.clone(), *keyword_pos));
        expanded_elements.push(Expr::List(expanded_bindings, *bindings_pos));
        for body_expr in &elements[2..] {
            expanded_elements.push(self.expand_macro_template(
                body_expr,
                macro_definition,
                bindings,
                &body_renames,
                state,
                repeat_index,
            )?);
        }

        Ok(Some(Expr::List(expanded_elements, pos)))
    }

    fn fresh_hygienic_name(&mut self, base: &str) -> String {
        let name = format!("__macro_{}_{}", base, self.next_hygiene_id);
        self.next_hygiene_id += 1;
        name
    }

    fn eval_if(&mut self, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        if arguments.len() < 2 || arguments.len() > 3 {
            return Err(EvalError::new("wrong argument count for if"));
        }

        let condition = self.eval(&arguments[0], env)?;
        if self.is_truthy(&condition) {
            return self.eval(&arguments[1], env);
        }
        if arguments.len() == 3 {
            return self.eval(&arguments[2], env);
        }
        Ok(Value::Void)
    }

    fn eval_lambda(&mut self, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(EvalError::new("invalid lambda"));
        }

        let parameters = self.parse_parameters_from_expr(&arguments[0])?;
        Ok(Value::Lambda(Rc::new(LambdaProcedure {
            name: None,
            parameters,
            body: arguments[1..].to_vec(),
            closure: Rc::clone(env),
        })))
    }

    fn eval_let(&mut self, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(EvalError::new("invalid let"));
        }

        if let Expr::Symbol(name, _) = &arguments[0] {
            if arguments.len() < 3 {
                return Err(EvalError::new("invalid let"));
            }

            let bindings = self.parse_bindings(&arguments[1])?;
            let values = self.eval_binding_values(&bindings, env)?;
            let parameters = bindings
                .iter()
                .map(|binding| binding.name.clone())
                .collect();

            let loop_env = Environment::new(Some(Rc::clone(env)));
            let procedure = Rc::new(LambdaProcedure {
                name: Some(name.clone()),
                parameters: ParameterSpec::fixed(parameters),
                body: arguments[2..].to_vec(),
                closure: Rc::clone(&loop_env),
            });
            Environment::define(
                &loop_env,
                name.clone(),
                Value::Lambda(Rc::clone(&procedure)),
            );

            let located_values = values
                .into_iter()
                .zip(bindings.iter())
                .map(|(value, binding)| LocatedValue {
                    value,
                    pos: binding.value_expr.pos(),
                })
                .collect::<Vec<_>>();

            return self.apply_lambda(&procedure, located_values, arguments[1].pos());
        }

        let bindings = self.parse_bindings(&arguments[0])?;
        let values = self.eval_binding_values(&bindings, env)?;
        let let_env = Environment::new(Some(Rc::clone(env)));
        for (binding, value) in bindings.iter().zip(values.into_iter()) {
            Environment::define(&let_env, binding.name.clone(), value);
        }
        self.eval_sequence(&arguments[1..], &let_env)
    }

    fn eval_quote(&self, arguments: &[Expr]) -> Result<Value, EvalError> {
        if arguments.len() != 1 {
            return Err(EvalError::new("wrong argument count for quote"));
        }
        Ok(self.quote(&arguments[0]))
    }

    fn eval_set(&mut self, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        if arguments.len() != 2 {
            return Err(EvalError::new("invalid set!"));
        }
        let Expr::Symbol(name, _) = &arguments[0] else {
            return Err(EvalError::new("invalid set!"));
        };
        let value = self.eval(&arguments[1], env)?;
        Environment::set(env, name, value)?;
        Ok(Value::Void)
    }

    fn eval_sequence(&mut self, expressions: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
        let mut result = Value::Void;
        for expression in expressions {
            result = self.eval(expression, env)?;
        }
        Ok(result)
    }

    fn parse_bindings(&self, bindings_expr: &Expr) -> Result<Vec<Binding>, EvalError> {
        let Expr::List(bindings, _) = bindings_expr else {
            return Err(EvalError::new("invalid let"));
        };

        let mut parsed = Vec::with_capacity(bindings.len());
        for binding_expr in bindings {
            let Expr::List(binding, _) = binding_expr else {
                return Err(EvalError::new("invalid let"));
            };
            if binding.len() != 2 {
                return Err(EvalError::new("invalid let"));
            }
            let Expr::Symbol(name, _) = &binding[0] else {
                return Err(EvalError::new("invalid let"));
            };
            parsed.push(Binding {
                name: name.clone(),
                value_expr: binding[1].clone(),
            });
        }
        Ok(parsed)
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

    fn parse_parameters_from_expr(
        &self,
        parameters_expr: &Expr,
    ) -> Result<ParameterSpec, EvalError> {
        match parameters_expr {
            Expr::Symbol(name, _) => {
                if name == "." {
                    Err(EvalError::new("invalid parameter list"))
                } else {
                    Ok(ParameterSpec::rest_only(name.clone()))
                }
            }
            Expr::List(parameters, _) => self.parse_parameters(parameters),
            _ => Err(EvalError::new("invalid parameter list")),
        }
    }

    fn parse_parameters(&self, parameters: &[Expr]) -> Result<ParameterSpec, EvalError> {
        let mut names = Vec::with_capacity(parameters.len());
        let mut index = 0;
        while index < parameters.len() {
            let Expr::Symbol(name, _) = &parameters[index] else {
                return Err(EvalError::new("invalid parameter list"));
            };
            if name == "." {
                if index + 2 != parameters.len() {
                    return Err(EvalError::new("invalid parameter list"));
                }
                let Expr::Symbol(rest, _) = &parameters[index + 1] else {
                    return Err(EvalError::new("invalid parameter list"));
                };
                if rest == "." {
                    return Err(EvalError::new("invalid parameter list"));
                }
                return Ok(ParameterSpec::new(names, Some(rest.clone())));
            }
            names.push(name.clone());
            index += 1;
        }
        Ok(ParameterSpec::fixed(names))
    }

    fn quote(&self, expression: &Expr) -> Value {
        match expression {
            Expr::Number(number, _) => Value::Number(*number),
            Expr::Bool(boolean, _) => Value::Bool(*boolean),
            Expr::String(text, _) => Value::String(string_cell(text)),
            Expr::Char(ch, _) => Value::Char(*ch),
            Expr::Symbol(name, _) => Value::Symbol(name.clone()),
            Expr::List(elements, _) => self.quote_list(elements),
        }
    }

    fn quote_list(&self, elements: &[Expr]) -> Value {
        let mut value = Value::EmptyList;
        for element in elements.iter().rev() {
            value = Value::Pair(Rc::new(PairValue {
                car: self.quote(element),
                cdr: value,
            }));
        }
        value
    }

    fn apply(
        &mut self,
        operator: Value,
        operator_pos: SourcePos,
        arguments: Vec<LocatedValue>,
        call_pos: SourcePos,
    ) -> Result<Value, EvalError> {
        match operator {
            Value::Builtin(builtin) => builtin.apply(self, call_pos, &arguments),
            Value::Lambda(lambda) => self.apply_lambda(&lambda, arguments, call_pos),
            _ => Err(self.error_at(operator_pos, "attempted to call non-procedure")),
        }
    }

    fn apply_lambda(
        &mut self,
        lambda: &Rc<LambdaProcedure>,
        arguments: Vec<LocatedValue>,
        call_pos: SourcePos,
    ) -> Result<Value, EvalError> {
        if !lambda.parameters.accepts(arguments.len()) {
            return Err(self.error_at(
                call_pos,
                format!("wrong argument count for {}", lambda.display_name()),
            ));
        }

        let call_env = Environment::new(Some(Rc::clone(&lambda.closure)));
        for (name, argument) in lambda.parameters.required.iter().zip(arguments.iter()) {
            Environment::define(&call_env, name.clone(), argument.value.clone());
        }
        if let Some(rest) = &lambda.parameters.rest {
            Environment::define(
                &call_env,
                rest.clone(),
                self.build_list(&arguments, lambda.parameters.required.len()),
            );
        }

        let mut result = Value::Void;
        for expression in &lambda.body {
            result = self.eval(expression, &call_env)?;
        }
        Ok(result)
    }

    fn builtin_not(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "not", call_pos)?;
        Ok(Value::Bool(!self.is_truthy(&arguments[0].value)))
    }

    fn builtin_cons(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 2, "cons", call_pos)?;
        Ok(Value::Pair(Rc::new(PairValue {
            car: arguments[0].value.clone(),
            cdr: arguments[1].value.clone(),
        })))
    }

    fn builtin_car(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "car", call_pos)?;
        Ok(self.require_pair(&arguments[0], "car")?.car.clone())
    }

    fn builtin_cdr(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "cdr", call_pos)?;
        Ok(self.require_pair(&arguments[0], "cdr")?.cdr.clone())
    }

    fn builtin_null(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "null?", call_pos)?;
        Ok(Value::Bool(matches!(arguments[0].value, Value::EmptyList)))
    }

    fn builtin_list(
        &mut self,
        _call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        Ok(self.list_from_values(arguments.iter().map(|argument| argument.value.clone())))
    }

    fn builtin_length(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "length", call_pos)?;
        Ok(Value::Number(Number::int(
            self.require_proper_list_length(&arguments[0], "length")?,
        )))
    }

    fn builtin_append(
        &mut self,
        _call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        if arguments.is_empty() {
            return Ok(Value::EmptyList);
        }

        let mut result = arguments.last().unwrap().value.clone();
        for argument in arguments[..arguments.len() - 1].iter().rev() {
            result = self.append_list_onto(argument, result)?;
        }
        Ok(result)
    }

    fn builtin_display(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "display", call_pos)?;
        self.output
            .push_str(&render_value(&arguments[0].value, true));
        Ok(Value::Void)
    }

    fn builtin_write(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "write", call_pos)?;
        self.output.push_str(&arguments[0].value.render());
        Ok(Value::Void)
    }

    fn builtin_newline(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 0, "newline", call_pos)?;
        self.output.push('\n');
        Ok(Value::Void)
    }

    fn builtin_string_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "string?", call_pos)?;
        Ok(Value::Bool(matches!(arguments[0].value, Value::String(_))))
    }

    fn builtin_string_append(
        &mut self,
        _call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        let mut chars = Vec::new();
        for argument in arguments {
            chars.extend(self.require_string_chars(argument, "string-append")?);
        }
        Ok(Value::String(Rc::new(RefCell::new(chars))))
    }

    fn builtin_string_length(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "string-length", call_pos)?;
        Ok(Value::Number(Number::int(
            self.require_string_chars(&arguments[0], "string-length")?
                .len() as i64,
        )))
    }

    fn builtin_substring(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 3, "substring", call_pos)?;
        let chars = self.require_string_chars(&arguments[0], "substring")?;
        let start = self.require_string_index(&arguments[1], "substring", chars.len())?;
        let end = self.require_string_index(&arguments[2], "substring", chars.len())?;
        if start > end {
            return Err(self.error_at(arguments[1].pos, "invalid substring range"));
        }
        Ok(Value::String(Rc::new(RefCell::new(
            chars[start..end].to_vec(),
        ))))
    }

    fn builtin_string_copy(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "string-copy", call_pos)?;
        Ok(Value::String(Rc::new(RefCell::new(
            self.require_string_chars(&arguments[0], "string-copy")?,
        ))))
    }

    fn builtin_string_set(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 3, "string-set!", call_pos)?;
        let string = self.require_string_cell(&arguments[0], "string-set!")?;
        let len = string.borrow().len();
        let index =
            self.require_string_index(&arguments[1], "string-set!", len.saturating_sub(1))?;
        let ch = self.require_char(&arguments[2], "string-set!")?;
        string.borrow_mut()[index] = ch;
        Ok(Value::Void)
    }

    fn builtin_string_to_number(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "string->number", call_pos)?;
        let value = chars_to_string(&self.require_string_chars(&arguments[0], "string->number")?);
        match parse_number_token(&value) {
            Ok(Some(number)) => Ok(Value::Number(number)),
            Ok(None) | Err(_) => Ok(Value::Bool(false)),
        }
    }

    fn builtin_number_to_string(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "number->string", call_pos)?;
        Ok(Value::String(string_cell(
            &self.require_number(&arguments[0], "number->string")?.render(),
        )))
    }

    fn builtin_symbol_to_string(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "symbol->string", call_pos)?;
        Ok(Value::String(string_cell(
            &self.require_symbol(&arguments[0], "symbol->string")?,
        )))
    }

    fn builtin_string_to_symbol(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "string->symbol", call_pos)?;
        Ok(Value::Symbol(chars_to_string(
            &self.require_string_chars(&arguments[0], "string->symbol")?,
        )))
    }

    fn builtin_string_ref(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 2, "string-ref", call_pos)?;
        let chars = self.require_string_chars(&arguments[0], "string-ref")?;
        let index =
            self.require_string_index(&arguments[1], "string-ref", chars.len().saturating_sub(1))?;
        Ok(Value::Char(chars[index]))
    }

    fn builtin_number_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "number?", call_pos)?;
        Ok(Value::Bool(matches!(arguments[0].value, Value::Number(_))))
    }

    fn builtin_exact_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "exact?", call_pos)?;
        Ok(Value::Bool(matches!(arguments[0].value, Value::Number(number) if number.is_exact())))
    }

    fn builtin_inexact_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "inexact?", call_pos)?;
        Ok(Value::Bool(matches!(arguments[0].value, Value::Number(number) if number.is_inexact())))
    }

    fn builtin_integer_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "integer?", call_pos)?;
        Ok(Value::Bool(matches!(arguments[0].value, Value::Number(number) if number.is_integer())))
    }

    fn builtin_rational_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "rational?", call_pos)?;
        Ok(Value::Bool(matches!(arguments[0].value, Value::Number(number) if number.is_rational())))
    }

    fn builtin_exact_to_inexact(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "exact->inexact", call_pos)?;
        let number = self.require_number(&arguments[0], "exact->inexact")?;
        Ok(Value::Number(Number::Inexact(number.to_f64())))
    }

    fn builtin_inexact_to_exact(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "inexact->exact", call_pos)?;
        let number = self.require_number(&arguments[0], "inexact->exact")?;
        let exact = match number {
            Number::Exact(_, _) => number,
            Number::Inexact(value) => inexact_to_exact_number(value),
        };
        Ok(Value::Number(exact))
    }

    fn builtin_numerator(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "numerator", call_pos)?;
        let (numerator, _) = self.require_exact_number(&arguments[0], "numerator")?;
        Ok(Value::Number(Number::int(numerator)))
    }

    fn builtin_denominator(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "denominator", call_pos)?;
        let (_, denominator) = self.require_exact_number(&arguments[0], "denominator")?;
        Ok(Value::Number(Number::int(denominator)))
    }

    fn builtin_boolean_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "boolean?", call_pos)?;
        Ok(Value::Bool(matches!(arguments[0].value, Value::Bool(_))))
    }

    fn builtin_char_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "char?", call_pos)?;
        Ok(Value::Bool(matches!(arguments[0].value, Value::Char(_))))
    }

    fn builtin_pair_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "pair?", call_pos)?;
        Ok(Value::Bool(matches!(arguments[0].value, Value::Pair(_))))
    }

    fn builtin_symbol_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "symbol?", call_pos)?;
        Ok(Value::Bool(matches!(arguments[0].value, Value::Symbol(_))))
    }

    fn builtin_eq(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 2, "eq?", call_pos)?;
        Ok(Value::Bool(
            self.eq_values(&arguments[0].value, &arguments[1].value),
        ))
    }

    fn builtin_equal(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 2, "equal?", call_pos)?;
        Ok(Value::Bool(
            self.equal_values(&arguments[0].value, &arguments[1].value),
        ))
    }

    fn builtin_abs(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "abs", call_pos)?;
        let number = self.require_number(&arguments[0], "abs")?;
        let result = match number {
            Number::Exact(numerator, denominator) => Number::rational(numerator.abs(), denominator),
            Number::Inexact(value) => Number::Inexact(value.abs()),
        };
        Ok(Value::Number(result))
    }

    fn builtin_modulo(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 2, "modulo", call_pos)?;
        let dividend = self.require_int(&arguments[0], "modulo")?;
        let divisor = self.require_non_zero_divisor(&arguments[1], "modulo")?;
        let mut result = dividend % divisor;
        if result != 0 && ((result > 0) != (divisor > 0)) {
            result += divisor;
        }
        Ok(Value::Number(Number::int(result)))
    }

    fn builtin_remainder(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 2, "remainder", call_pos)?;
        let dividend = self.require_int(&arguments[0], "remainder")?;
        let divisor = self.require_non_zero_divisor(&arguments[1], "remainder")?;
        Ok(Value::Number(Number::int(dividend % divisor)))
    }

    fn builtin_quotient(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 2, "quotient", call_pos)?;
        let dividend = self.require_int(&arguments[0], "quotient")?;
        let divisor = self.require_non_zero_divisor(&arguments[1], "quotient")?;
        Ok(Value::Number(Number::int(dividend / divisor)))
    }

    fn builtin_min(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_minimum_argument_count(arguments, 1, "min", call_pos)?;
        let mut result = self.require_number(&arguments[0], "min")?;
        for argument in &arguments[1..] {
            let next = self.require_number(argument, "min")?;
            if compare_numbers(next, result) == Ordering::Less {
                result = next;
            }
        }
        Ok(Value::Number(result))
    }

    fn builtin_max(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_minimum_argument_count(arguments, 1, "max", call_pos)?;
        let mut result = self.require_number(&arguments[0], "max")?;
        for argument in &arguments[1..] {
            let next = self.require_number(argument, "max")?;
            if compare_numbers(next, result) == Ordering::Greater {
                result = next;
            }
        }
        Ok(Value::Number(result))
    }

    fn builtin_expt(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 2, "expt", call_pos)?;
        let base = self.require_int(&arguments[0], "expt")?;
        let exponent = self.require_int(&arguments[1], "expt")?;
        if exponent < 0 {
            return Err(self.error_at(arguments[1].pos, "expected non-negative integer for expt"));
        }

        let mut result = 1_i64;
        let mut factor = base;
        let mut remaining = exponent as u64;
        while remaining > 0 {
            if remaining & 1 == 1 {
                result *= factor;
            }
            factor *= factor;
            remaining >>= 1;
        }
        Ok(Value::Number(Number::int(result)))
    }

    fn builtin_zero_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "zero?", call_pos)?;
        Ok(Value::Bool(
            compare_numbers(self.require_number(&arguments[0], "zero?")?, Number::int(0))
                == Ordering::Equal,
        ))
    }

    fn builtin_positive_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "positive?", call_pos)?;
        Ok(Value::Bool(
            compare_numbers(self.require_number(&arguments[0], "positive?")?, Number::int(0))
                == Ordering::Greater,
        ))
    }

    fn builtin_negative_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "negative?", call_pos)?;
        Ok(Value::Bool(
            compare_numbers(self.require_number(&arguments[0], "negative?")?, Number::int(0))
                == Ordering::Less,
        ))
    }

    fn builtin_odd_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "odd?", call_pos)?;
        Ok(Value::Bool(
            self.require_int(&arguments[0], "odd?")? & 1 != 0,
        ))
    }

    fn builtin_even_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "even?", call_pos)?;
        Ok(Value::Bool(
            self.require_int(&arguments[0], "even?")? & 1 == 0,
        ))
    }

    fn builtin_list_ref(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 2, "list-ref", call_pos)?;
        let index = self.require_non_negative_index(&arguments[1], "list-ref")?;
        let mut current = arguments[0].value.clone();

        for _ in 0..index {
            if let Some((_, cdr)) = pair_parts(&current) {
                current = cdr;
            } else {
                return Err(self.error_at(arguments[0].pos, "expected list for list-ref"));
            }
        }

        if let Some((car, _)) = pair_parts(&current) {
            Ok(car)
        } else {
            Err(self.error_at(arguments[0].pos, "expected list for list-ref"))
        }
    }

    fn builtin_list_tail(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 2, "list-tail", call_pos)?;
        let index = self.require_non_negative_index(&arguments[1], "list-tail")?;
        let mut current = arguments[0].value.clone();

        for _ in 0..index {
            if let Some((_, cdr)) = pair_parts(&current) {
                current = cdr;
            } else {
                return Err(self.error_at(arguments[0].pos, "expected list for list-tail"));
            }
        }

        if matches!(current, Value::Pair(_) | Value::EmptyList) {
            Ok(current)
        } else {
            Err(self.error_at(arguments[0].pos, "expected list for list-tail"))
        }
    }

    fn builtin_list_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "list?", call_pos)?;
        Ok(Value::Bool(self.is_proper_list(&arguments[0].value)))
    }

    fn builtin_assoc(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 2, "assoc", call_pos)?;
        let key = &arguments[0].value;
        let mut current = arguments[1].value.clone();

        while let Some((entry, cdr)) = pair_parts(&current) {
            let Some((entry_key, _)) = pair_parts(&entry) else {
                return Err(self.error_at(arguments[1].pos, "expected pair for assoc"));
            };
            if self.equal_values(key, &entry_key) {
                return Ok(entry);
            }
            current = cdr;
        }

        if matches!(current, Value::EmptyList) {
            Ok(Value::Bool(false))
        } else {
            Err(self.error_at(arguments[1].pos, "expected list for assoc"))
        }
    }

    fn builtin_map(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_minimum_argument_count(arguments, 2, "map", call_pos)?;

        let procedure = arguments[0].clone();
        let list_arguments = &arguments[1..];
        let mut cursors = list_arguments
            .iter()
            .map(|argument| argument.value.clone())
            .collect::<Vec<_>>();
        let mut results = Vec::new();

        loop {
            let mut mapped_arguments = Vec::with_capacity(list_arguments.len());
            let mut reached_end = false;

            for (cursor, list_argument) in cursors.iter().zip(list_arguments.iter()) {
                if matches!(cursor, Value::EmptyList) {
                    reached_end = true;
                    break;
                }
                let Some((car, _)) = pair_parts(cursor) else {
                    return Err(self.error_at(list_argument.pos, "expected list for map"));
                };
                mapped_arguments.push(LocatedValue {
                    value: car,
                    pos: list_argument.pos,
                });
            }

            if reached_end {
                break;
            }

            for cursor in &mut cursors {
                let (_, cdr) = pair_parts(cursor).unwrap();
                *cursor = cdr;
            }
            results.push(self.apply(
                procedure.value.clone(),
                procedure.pos,
                mapped_arguments,
                call_pos,
            )?);
        }

        Ok(self.list_from_values(results))
    }

    fn builtin_char_alphabetic_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "char-alphabetic?", call_pos)?;
        Ok(Value::Bool(
            self.require_char(&arguments[0], "char-alphabetic?")?
                .is_alphabetic(),
        ))
    }

    fn builtin_char_numeric_predicate(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "char-numeric?", call_pos)?;
        Ok(Value::Bool(
            self.require_char(&arguments[0], "char-numeric?")?
                .is_ascii_digit(),
        ))
    }

    fn builtin_char_upcase(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "char-upcase", call_pos)?;
        Ok(Value::Char(
            self.require_char(&arguments[0], "char-upcase")?
                .to_ascii_uppercase(),
        ))
    }

    fn builtin_char_downcase(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "char-downcase", call_pos)?;
        Ok(Value::Char(
            self.require_char(&arguments[0], "char-downcase")?
                .to_ascii_lowercase(),
        ))
    }

    fn builtin_char_equals(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.compare_chars(call_pos, arguments, "char=?", |left, right| left == right)
    }

    fn builtin_char_less_than(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.compare_chars(call_pos, arguments, "char<?", |left, right| left < right)
    }

    fn builtin_string_equals(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.compare_strings(call_pos, arguments, "string=?", |left, right| left == right)
    }

    fn builtin_string_less_than(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.compare_strings(call_pos, arguments, "string<?", |left, right| left < right)
    }

    fn builtin_string_case_insensitive_equals(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.compare_strings(call_pos, arguments, "string-ci=?", |left, right| {
            left.eq_ignore_ascii_case(&right)
        })
    }

    fn builtin_string_upcase(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "string-upcase", call_pos)?;
        let chars = self
            .require_string_chars(&arguments[0], "string-upcase")?
            .into_iter()
            .map(|ch| ch.to_ascii_uppercase())
            .collect::<Vec<_>>();
        Ok(Value::String(Rc::new(RefCell::new(chars))))
    }

    fn builtin_string_downcase(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        self.expect_argument_count(arguments, 1, "string-downcase", call_pos)?;
        let chars = self
            .require_string_chars(&arguments[0], "string-downcase")?
            .into_iter()
            .map(|ch| ch.to_ascii_lowercase())
            .collect::<Vec<_>>();
        Ok(Value::String(Rc::new(RefCell::new(chars))))
    }

    fn builtin_apply(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(self.error_at(call_pos, "wrong argument count for apply"));
        }

        let operator = &arguments[0];
        let mut expanded_arguments = arguments[1..arguments.len() - 1].to_vec();
        expanded_arguments.extend(self.expand_apply_arguments(arguments.last().unwrap())?);
        self.apply(
            operator.value.clone(),
            operator.pos,
            expanded_arguments,
            call_pos,
        )
    }

    fn builtin_add(
        &mut self,
        _call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        let any_inexact = arguments.iter().try_fold(false, |seen, argument| {
            Ok::<_, EvalError>(seen || matches!(self.require_number(argument, "+")?, Number::Inexact(_)))
        })?;

        if any_inexact {
            let mut total = 0.0_f64;
            for argument in arguments {
                total += self.require_number(argument, "+")?.to_f64();
            }
            return Ok(Value::Number(Number::Inexact(total)));
        }

        let mut total = Number::int(0);
        for argument in arguments {
            total = add_exact_numbers(total, self.require_number(argument, "+")?);
        }
        Ok(Value::Number(total))
    }

    fn builtin_subtract(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        if arguments.is_empty() {
            return Err(self.error_at(call_pos, "wrong argument count for -"));
        }

        let first = self.require_number(&arguments[0], "-")?;
        if arguments.len() == 1 {
            return Ok(Value::Number(match first {
                Number::Exact(numerator, denominator) => Number::rational(-numerator, denominator),
                Number::Inexact(value) => Number::Inexact(-value),
            }));
        }

        let any_inexact = matches!(first, Number::Inexact(_))
            || arguments[1..]
                .iter()
                .try_fold(false, |seen, argument| {
                    Ok::<_, EvalError>(seen || matches!(self.require_number(argument, "-")?, Number::Inexact(_)))
                })?;

        if any_inexact {
            let mut result = first.to_f64();
            for argument in &arguments[1..] {
                result -= self.require_number(argument, "-")?.to_f64();
            }
            return Ok(Value::Number(Number::Inexact(result)));
        }

        let mut result = first;
        for argument in &arguments[1..] {
            result = subtract_exact_numbers(result, self.require_number(argument, "-")?);
        }
        Ok(Value::Number(result))
    }

    fn builtin_multiply(
        &mut self,
        _call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        let any_inexact = arguments.iter().try_fold(false, |seen, argument| {
            Ok::<_, EvalError>(seen || matches!(self.require_number(argument, "*")?, Number::Inexact(_)))
        })?;

        if any_inexact {
            let mut total = 1.0_f64;
            for argument in arguments {
                total *= self.require_number(argument, "*")?.to_f64();
            }
            return Ok(Value::Number(Number::Inexact(total)));
        }

        let mut total = Number::int(1);
        for argument in arguments {
            total = multiply_exact_numbers(total, self.require_number(argument, "*")?);
        }
        Ok(Value::Number(total))
    }

    fn builtin_divide(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(self.error_at(call_pos, "wrong argument count for /"));
        }

        let first = self.require_number(&arguments[0], "/")?;
        let any_inexact = matches!(first, Number::Inexact(_))
            || arguments[1..]
                .iter()
                .try_fold(false, |seen, argument| {
                    Ok::<_, EvalError>(seen || matches!(self.require_number(argument, "/")?, Number::Inexact(_)))
                })?;

        if any_inexact {
            let mut result = first.to_f64();
            for argument in &arguments[1..] {
                let divisor = self.require_number(argument, "/")?.to_f64();
                if divisor == 0.0 {
                    return Err(self.error_at(argument.pos, "division by zero"));
                }
                result /= divisor;
            }
            return Ok(Value::Number(Number::Inexact(result)));
        }

        let mut result = first;
        for argument in &arguments[1..] {
            let divisor = self.require_number(argument, "/")?;
            if matches!(divisor, Number::Exact(0, _)) {
                return Err(self.error_at(argument.pos, "division by zero"));
            }
            result = divide_exact_numbers(result, divisor);
        }
        Ok(Value::Number(result))
    }

    fn builtin_numeric_comparison(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
        name: &str,
        comparison: impl Fn(Ordering) -> bool,
    ) -> Result<Value, EvalError> {
        if arguments.len() < 2 {
            return Err(self.error_at(call_pos, format!("wrong argument count for {name}")));
        }

        let mut left = self.require_number(&arguments[0], name)?;
        for argument in &arguments[1..] {
            let right = self.require_number(argument, name)?;
            if !comparison(compare_numbers(left, right)) {
                return Ok(Value::Bool(false));
            }
            left = right;
        }
        Ok(Value::Bool(true))
    }

    fn expect_argument_count(
        &self,
        arguments: &[LocatedValue],
        expected: usize,
        name: &str,
        pos: SourcePos,
    ) -> Result<(), EvalError> {
        if arguments.len() != expected {
            return Err(self.error_at(pos, format!("wrong argument count for {name}")));
        }
        Ok(())
    }

    fn expect_minimum_argument_count(
        &self,
        arguments: &[LocatedValue],
        minimum: usize,
        name: &str,
        pos: SourcePos,
    ) -> Result<(), EvalError> {
        if arguments.len() < minimum {
            return Err(self.error_at(pos, format!("wrong argument count for {name}")));
        }
        Ok(())
    }

    fn require_int(&self, value: &LocatedValue, name: &str) -> Result<i64, EvalError> {
        match value.value {
            Value::Number(Number::Exact(number, 1)) => Ok(number),
            Value::Number(_) => Err(self.error_at(value.pos, format!("expected integer for {name}"))),
            _ => Err(self.error_at(value.pos, format!("expected number for {name}"))),
        }
    }

    fn require_number(&self, value: &LocatedValue, name: &str) -> Result<Number, EvalError> {
        match value.value {
            Value::Number(number) => Ok(number),
            _ => Err(self.error_at(value.pos, format!("expected number for {name}"))),
        }
    }

    fn require_exact_number(
        &self,
        value: &LocatedValue,
        name: &str,
    ) -> Result<(i64, i64), EvalError> {
        match self.require_number(value, name)? {
            Number::Exact(numerator, denominator) => Ok((numerator, denominator)),
            Number::Inexact(_) => Err(self.error_at(value.pos, format!("expected exact number for {name}"))),
        }
    }

    fn require_string_cell(
        &self,
        value: &LocatedValue,
        name: &str,
    ) -> Result<StringCell, EvalError> {
        match &value.value {
            Value::String(text) => Ok(Rc::clone(text)),
            _ => Err(self.error_at(value.pos, format!("expected string for {name}"))),
        }
    }

    fn require_string_chars(
        &self,
        value: &LocatedValue,
        name: &str,
    ) -> Result<Vec<char>, EvalError> {
        Ok(self.require_string_cell(value, name)?.borrow().clone())
    }

    fn require_symbol(&self, value: &LocatedValue, name: &str) -> Result<String, EvalError> {
        match &value.value {
            Value::Symbol(symbol) => Ok(symbol.clone()),
            _ => Err(self.error_at(value.pos, format!("expected symbol for {name}"))),
        }
    }

    fn require_char(&self, value: &LocatedValue, name: &str) -> Result<char, EvalError> {
        match value.value {
            Value::Char(ch) => Ok(ch),
            _ => Err(self.error_at(value.pos, format!("expected char for {name}"))),
        }
    }

    fn require_non_zero_divisor(&self, value: &LocatedValue, name: &str) -> Result<i64, EvalError> {
        let divisor = self.require_int(value, name)?;
        if divisor == 0 {
            return Err(self.error_at(value.pos, "division by zero"));
        }
        Ok(divisor)
    }

    fn require_string_index(
        &self,
        value: &LocatedValue,
        name: &str,
        upper_bound: usize,
    ) -> Result<usize, EvalError> {
        let index = self.require_int(value, name)?;
        if index < 0 || index as usize > upper_bound {
            return Err(self.error_at(value.pos, format!("index out of range for {name}")));
        }
        Ok(index as usize)
    }

    fn require_non_negative_index(
        &self,
        value: &LocatedValue,
        name: &str,
    ) -> Result<usize, EvalError> {
        let index = self.require_int(value, name)?;
        if index < 0 {
            return Err(self.error_at(value.pos, format!("index out of range for {name}")));
        }
        Ok(index as usize)
    }

    fn require_pair<'a>(
        &self,
        value: &'a LocatedValue,
        name: &str,
    ) -> Result<&'a PairValue, EvalError> {
        match &value.value {
            Value::Pair(pair) => Ok(pair),
            _ => Err(self.error_at(value.pos, format!("expected pair for {name}"))),
        }
    }

    fn require_proper_list_length(
        &self,
        value: &LocatedValue,
        name: &str,
    ) -> Result<i64, EvalError> {
        let mut length = 0_i64;
        let mut current = value.value.clone();
        while let Some((_, cdr)) = pair_parts(&current) {
            length += 1;
            current = cdr;
        }
        if matches!(current, Value::EmptyList) {
            Ok(length)
        } else {
            Err(self.error_at(value.pos, format!("expected list for {name}")))
        }
    }

    fn append_list_onto(&self, list: &LocatedValue, tail: Value) -> Result<Value, EvalError> {
        let mut elements = Vec::new();
        let mut current = list.value.clone();
        while let Some((car, cdr)) = pair_parts(&current) {
            elements.push(car);
            current = cdr;
        }
        if !matches!(current, Value::EmptyList) {
            return Err(self.error_at(list.pos, "expected list for append"));
        }

        let mut result = tail;
        for element in elements.into_iter().rev() {
            result = Value::Pair(Rc::new(PairValue {
                car: element,
                cdr: result,
            }));
        }
        Ok(result)
    }

    fn build_list(&self, values: &[LocatedValue], start_index: usize) -> Value {
        let mut result = Value::EmptyList;
        for value in values[start_index..].iter().rev() {
            result = Value::Pair(Rc::new(PairValue {
                car: value.value.clone(),
                cdr: result,
            }));
        }
        result
    }

    fn list_from_values(&self, values: impl IntoIterator<Item = Value>) -> Value {
        let mut collected = values.into_iter().collect::<Vec<_>>();
        let mut result = Value::EmptyList;
        for value in collected.drain(..).rev() {
            result = Value::Pair(Rc::new(PairValue {
                car: value,
                cdr: result,
            }));
        }
        result
    }

    fn expand_apply_arguments(&self, list: &LocatedValue) -> Result<Vec<LocatedValue>, EvalError> {
        let mut values = Vec::new();
        let mut current = list.value.clone();
        while let Some((car, cdr)) = pair_parts(&current) {
            values.push(LocatedValue {
                value: car,
                pos: list.pos,
            });
            current = cdr;
        }
        if matches!(current, Value::EmptyList) {
            Ok(values)
        } else {
            Err(self.error_at(list.pos, "expected list for apply"))
        }
    }

    fn compare_chars(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
        name: &str,
        comparison: impl Fn(char, char) -> bool,
    ) -> Result<Value, EvalError> {
        self.expect_minimum_argument_count(arguments, 2, name, call_pos)?;

        let mut left = self.require_char(&arguments[0], name)?;
        for argument in &arguments[1..] {
            let right = self.require_char(argument, name)?;
            if !comparison(left, right) {
                return Ok(Value::Bool(false));
            }
            left = right;
        }
        Ok(Value::Bool(true))
    }

    fn compare_strings(
        &mut self,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
        name: &str,
        comparison: impl Fn(String, String) -> bool,
    ) -> Result<Value, EvalError> {
        self.expect_minimum_argument_count(arguments, 2, name, call_pos)?;

        let mut left = chars_to_string(&self.require_string_chars(&arguments[0], name)?);
        for argument in &arguments[1..] {
            let right = chars_to_string(&self.require_string_chars(argument, name)?);
            if !comparison(left, right.clone()) {
                return Ok(Value::Bool(false));
            }
            left = right;
        }
        Ok(Value::Bool(true))
    }

    fn is_proper_list(&self, value: &Value) -> bool {
        let mut current = value.clone();
        while let Some((_, cdr)) = pair_parts(&current) {
            current = cdr;
        }
        matches!(current, Value::EmptyList)
    }

    fn eq_values(&self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => numbers_equal(*left, *right),
            (Value::Bool(left), Value::Bool(right)) => left == right,
            (Value::Symbol(left), Value::Symbol(right)) => left == right,
            (Value::Char(left), Value::Char(right)) => left == right,
            (Value::String(left), Value::String(right)) => Rc::ptr_eq(left, right),
            (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
            (Value::Builtin(left), Value::Builtin(right)) => left == right,
            (Value::Lambda(left), Value::Lambda(right)) => Rc::ptr_eq(left, right),
            (Value::EmptyList, Value::EmptyList) => true,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }

    fn equal_values(&self, left: &Value, right: &Value) -> bool {
        if self.eq_values(left, right) {
            return true;
        }

        match (left, right) {
            (Value::String(left), Value::String(right)) => {
                let left_chars = left.borrow();
                let right_chars = right.borrow();
                *left_chars == *right_chars
            }
            (Value::Pair(left), Value::Pair(right)) => {
                self.equal_values(&left.car, &right.car) && self.equal_values(&left.cdr, &right.cdr)
            }
            _ => false,
        }
    }

    fn is_truthy(&self, value: &Value) -> bool {
        !matches!(value, Value::Bool(false))
    }

    fn error_at(&self, pos: SourcePos, message: impl Into<String>) -> EvalError {
        EvalError::at(message, pos.line, pos.column)
    }

    fn with_position(&self, error: EvalError, pos: SourcePos) -> EvalError {
        if error.has_position() {
            error
        } else {
            error.with_position(pos.line, pos.column)
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct SourcePos {
    line: usize,
    column: usize,
}

#[derive(Clone, Debug)]
enum Expr {
    Number(Number, SourcePos),
    Bool(bool, SourcePos),
    String(String, SourcePos),
    Char(char, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    fn pos(&self) -> SourcePos {
        match self {
            Self::Number(_, pos)
            | Self::Bool(_, pos)
            | Self::String(_, pos)
            | Self::Char(_, pos)
            | Self::Symbol(_, pos)
            | Self::List(_, pos) => *pos,
        }
    }
}

#[derive(Clone, Debug)]
struct LocatedValue {
    value: Value,
    pos: SourcePos,
}

#[derive(Clone, Debug)]
enum Value {
    Number(Number),
    Bool(bool),
    String(StringCell),
    Symbol(String),
    Char(char),
    Pair(Rc<PairValue>),
    EmptyList,
    Builtin(Builtin),
    Lambda(Rc<LambdaProcedure>),
    Void,
}

impl Value {
    fn render(&self) -> String {
        render_value(self, false)
    }
}

#[derive(Clone, Copy, Debug)]
enum Number {
    Exact(i64, i64),
    Inexact(f64),
}

impl Number {
    fn int(value: i64) -> Self {
        Self::Exact(value, 1)
    }

    fn rational(numerator: i64, denominator: i64) -> Self {
        let (numerator, denominator) = normalize_fraction(numerator as i128, denominator as i128);
        Self::Exact(numerator, denominator)
    }

    fn is_exact(self) -> bool {
        matches!(self, Self::Exact(_, _))
    }

    fn is_inexact(self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    fn is_integer(self) -> bool {
        match self {
            Self::Exact(_, denominator) => denominator == 1,
            Self::Inexact(number) => number.is_finite() && number.fract() == 0.0,
        }
    }

    fn is_rational(self) -> bool {
        self.is_exact()
    }

    fn to_f64(self) -> f64 {
        match self {
            Self::Exact(numerator, denominator) => numerator as f64 / denominator as f64,
            Self::Inexact(number) => number,
        }
    }

    fn as_exact_parts(self) -> Option<(i64, i64)> {
        match self {
            Self::Exact(numerator, denominator) => Some((numerator, denominator)),
            Self::Inexact(_) => None,
        }
    }

    fn render(self) -> String {
        match self {
            Self::Exact(numerator, denominator) if denominator == 1 => numerator.to_string(),
            Self::Exact(numerator, denominator) => format!("{numerator}/{denominator}"),
            Self::Inexact(number) => render_inexact(number),
        }
    }
}

fn normalize_fraction(numerator: i128, denominator: i128) -> (i64, i64) {
    debug_assert_ne!(denominator, 0);
    if numerator == 0 {
        return (0, 1);
    }

    let mut numerator = numerator;
    let mut denominator = denominator;
    if denominator < 0 {
        numerator = -numerator;
        denominator = -denominator;
    }

    let divisor = gcd_i128(numerator.abs(), denominator);
    (
        i64::try_from(numerator / divisor).unwrap(),
        i64::try_from(denominator / divisor).unwrap(),
    )
}

fn gcd_i128(mut left: i128, mut right: i128) -> i128 {
    while right != 0 {
        let next = left % right;
        left = right;
        right = next;
    }
    left.abs().max(1)
}

fn compare_numbers(left: Number, right: Number) -> Ordering {
    match (left, right) {
        (Number::Exact(left_num, left_den), Number::Exact(right_num, right_den)) => {
            let left_scaled = left_num as i128 * right_den as i128;
            let right_scaled = right_num as i128 * left_den as i128;
            left_scaled.cmp(&right_scaled)
        }
        _ => left.to_f64().partial_cmp(&right.to_f64()).unwrap_or(Ordering::Equal),
    }
}

fn numbers_equal(left: Number, right: Number) -> bool {
    compare_numbers(left, right) == Ordering::Equal
}

#[derive(Clone, Copy, Debug)]
enum NumberParseError {
    InvalidInteger,
    InvalidRational,
    InvalidInexact,
}

impl NumberParseError {
    fn message(self, token: &str) -> String {
        match self {
            Self::InvalidInteger => format!("invalid integer literal: {token}"),
            Self::InvalidRational => format!("invalid rational literal: {token}"),
            Self::InvalidInexact => format!("invalid inexact literal: {token}"),
        }
    }
}

fn add_exact_numbers(left: Number, right: Number) -> Number {
    let (left_num, left_den) = left.as_exact_parts().unwrap();
    let (right_num, right_den) = right.as_exact_parts().unwrap();
    Number::rational(
        i64::try_from(left_num as i128 * right_den as i128 + right_num as i128 * left_den as i128)
            .unwrap(),
        i64::try_from(left_den as i128 * right_den as i128).unwrap(),
    )
}

fn subtract_exact_numbers(left: Number, right: Number) -> Number {
    let (left_num, left_den) = left.as_exact_parts().unwrap();
    let (right_num, right_den) = right.as_exact_parts().unwrap();
    Number::rational(
        i64::try_from(left_num as i128 * right_den as i128 - right_num as i128 * left_den as i128)
            .unwrap(),
        i64::try_from(left_den as i128 * right_den as i128).unwrap(),
    )
}

fn multiply_exact_numbers(left: Number, right: Number) -> Number {
    let (left_num, left_den) = left.as_exact_parts().unwrap();
    let (right_num, right_den) = right.as_exact_parts().unwrap();
    Number::rational(
        i64::try_from(left_num as i128 * right_num as i128).unwrap(),
        i64::try_from(left_den as i128 * right_den as i128).unwrap(),
    )
}

fn divide_exact_numbers(left: Number, right: Number) -> Number {
    let (left_num, left_den) = left.as_exact_parts().unwrap();
    let (right_num, right_den) = right.as_exact_parts().unwrap();
    Number::rational(
        i64::try_from(left_num as i128 * right_den as i128).unwrap(),
        i64::try_from(left_den as i128 * right_num as i128).unwrap(),
    )
}

fn render_inexact(number: f64) -> String {
    if number.is_finite() && number.fract() == 0.0 {
        format!("{number:.1}")
    } else {
        number.to_string()
    }
}

#[derive(Clone, Debug)]
struct PairValue {
    car: Value,
    cdr: Value,
}

#[derive(Clone, Debug)]
struct LambdaProcedure {
    name: Option<String>,
    parameters: ParameterSpec,
    body: Vec<Expr>,
    closure: EnvRef,
}

impl LambdaProcedure {
    fn display_name(&self) -> &str {
        self.name.as_deref().unwrap_or("lambda")
    }
}

#[derive(Clone, Debug)]
struct ParameterSpec {
    required: Vec<String>,
    rest: Option<String>,
}

impl ParameterSpec {
    fn new(required: Vec<String>, rest: Option<String>) -> Self {
        Self { required, rest }
    }

    fn fixed(required: Vec<String>) -> Self {
        Self {
            required,
            rest: None,
        }
    }

    fn rest_only(rest: String) -> Self {
        Self {
            required: Vec::new(),
            rest: Some(rest),
        }
    }

    fn accepts(&self, argument_count: usize) -> bool {
        if self.rest.is_some() {
            argument_count >= self.required.len()
        } else {
            argument_count == self.required.len()
        }
    }
}

#[derive(Clone, Debug)]
struct Binding {
    name: String,
    value_expr: Expr,
}

#[derive(Clone, Debug)]
struct MacroDefinition {
    name: String,
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
    definition_env: EnvRef,
    definition_macros: HashSet<String>,
}

#[derive(Clone, Debug)]
struct MacroRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Clone, Debug, Default)]
struct MacroBindings {
    single: HashMap<String, Expr>,
    repeated: HashMap<String, Vec<Expr>>,
}

impl MacroBindings {
    fn bind_single(&mut self, name: &str, value: &Expr) -> bool {
        if let Some(existing) = self.single.get(name) {
            return expr_syntax_eq(existing, value);
        }
        self.single.insert(name.to_owned(), value.clone());
        true
    }

    fn push_repeated(&mut self, name: &str, value: &Expr) {
        self.repeated
            .entry(name.to_owned())
            .or_default()
            .push(value.clone());
    }

    fn ensure_repeated(&mut self, name: &str) {
        self.repeated.entry(name.to_owned()).or_default();
    }

    fn lookup(&self, name: &str, repeat_index: Option<usize>) -> Option<&Expr> {
        if let Some(values) = self.repeated.get(name) {
            return repeat_index.and_then(|index| values.get(index));
        }
        self.single.get(name)
    }

    fn repeated_len(&self, name: &str) -> Option<usize> {
        self.repeated.get(name).map(Vec::len)
    }
}

#[derive(Debug)]
struct MacroExpansion {
    expr: Expr,
    capture_bindings: Vec<(String, Value)>,
}

#[derive(Debug, Default)]
struct MacroExpansionState {
    capture_aliases: HashMap<String, String>,
    capture_bindings: Vec<(String, Value)>,
}

#[derive(Clone, Debug)]
struct Environment {
    parent: Option<EnvRef>,
    bindings: HashMap<String, Value>,
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent,
            bindings: HashMap::new(),
        }))
    }

    fn define(env: &EnvRef, name: String, value: Value) {
        env.borrow_mut().bindings.insert(name, value);
    }

    fn lookup(env: &EnvRef, name: &str) -> Result<Value, EvalError> {
        if let Some(value) = env.borrow().bindings.get(name).cloned() {
            return Ok(value);
        }
        if let Some(parent) = env.borrow().parent.clone() {
            return Self::lookup(&parent, name);
        }
        Err(EvalError::new(format!("unbound symbol: {name}")))
    }

    fn set(env: &EnvRef, name: &str, value: Value) -> Result<(), EvalError> {
        if env.borrow().bindings.contains_key(name) {
            env.borrow_mut().bindings.insert(name.to_owned(), value);
            return Ok(());
        }
        if let Some(parent) = env.borrow().parent.clone() {
            return Self::set(&parent, name, value);
        }
        Err(EvalError::new(format!("unbound symbol: {name}")))
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Builtin {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    EqualNumber,
    LessEqual,
    Cons,
    Car,
    Cdr,
    NullPredicate,
    List,
    Length,
    Append,
    Display,
    Write,
    Newline,
    StringPredicate,
    StringAppend,
    StringLength,
    Substring,
    StringCopy,
    StringSet,
    StringToNumber,
    NumberToString,
    SymbolToString,
    StringToSymbol,
    StringRef,
    NumberPredicate,
    ExactPredicate,
    InexactPredicate,
    IntegerPredicate,
    RationalPredicate,
    ExactToInexact,
    InexactToExact,
    Numerator,
    Denominator,
    BooleanPredicate,
    CharPredicate,
    PairPredicate,
    SymbolPredicate,
    Eq,
    Equal,
    Abs,
    Modulo,
    Remainder,
    Quotient,
    Min,
    Max,
    Expt,
    ZeroPredicate,
    PositivePredicate,
    NegativePredicate,
    OddPredicate,
    EvenPredicate,
    ListRef,
    ListTail,
    ListPredicate,
    Assoc,
    Map,
    CharAlphabeticPredicate,
    CharNumericPredicate,
    CharUpcase,
    CharDowncase,
    CharEquals,
    CharLessThan,
    StringEquals,
    StringLessThan,
    StringCaseInsensitiveEquals,
    StringUpcase,
    StringDowncase,
    Not,
    Apply,
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
            Self::EqualNumber => "=",
            Self::LessEqual => "<=",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::NullPredicate => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::Append => "append",
            Self::Display => "display",
            Self::Write => "write",
            Self::Newline => "newline",
            Self::StringPredicate => "string?",
            Self::StringAppend => "string-append",
            Self::StringLength => "string-length",
            Self::Substring => "substring",
            Self::StringCopy => "string-copy",
            Self::StringSet => "string-set!",
            Self::StringToNumber => "string->number",
            Self::NumberToString => "number->string",
            Self::SymbolToString => "symbol->string",
            Self::StringToSymbol => "string->symbol",
            Self::StringRef => "string-ref",
            Self::NumberPredicate => "number?",
            Self::ExactPredicate => "exact?",
            Self::InexactPredicate => "inexact?",
            Self::IntegerPredicate => "integer?",
            Self::RationalPredicate => "rational?",
            Self::ExactToInexact => "exact->inexact",
            Self::InexactToExact => "inexact->exact",
            Self::Numerator => "numerator",
            Self::Denominator => "denominator",
            Self::BooleanPredicate => "boolean?",
            Self::CharPredicate => "char?",
            Self::PairPredicate => "pair?",
            Self::SymbolPredicate => "symbol?",
            Self::Eq => "eq?",
            Self::Equal => "equal?",
            Self::Abs => "abs",
            Self::Modulo => "modulo",
            Self::Remainder => "remainder",
            Self::Quotient => "quotient",
            Self::Min => "min",
            Self::Max => "max",
            Self::Expt => "expt",
            Self::ZeroPredicate => "zero?",
            Self::PositivePredicate => "positive?",
            Self::NegativePredicate => "negative?",
            Self::OddPredicate => "odd?",
            Self::EvenPredicate => "even?",
            Self::ListRef => "list-ref",
            Self::ListTail => "list-tail",
            Self::ListPredicate => "list?",
            Self::Assoc => "assoc",
            Self::Map => "map",
            Self::CharAlphabeticPredicate => "char-alphabetic?",
            Self::CharNumericPredicate => "char-numeric?",
            Self::CharUpcase => "char-upcase",
            Self::CharDowncase => "char-downcase",
            Self::CharEquals => "char=?",
            Self::CharLessThan => "char<?",
            Self::StringEquals => "string=?",
            Self::StringLessThan => "string<?",
            Self::StringCaseInsensitiveEquals => "string-ci=?",
            Self::StringUpcase => "string-upcase",
            Self::StringDowncase => "string-downcase",
            Self::Not => "not",
            Self::Apply => "apply",
        }
    }

    fn apply(
        self,
        interpreter: &mut Interpreter,
        call_pos: SourcePos,
        arguments: &[LocatedValue],
    ) -> Result<Value, EvalError> {
        match self {
            Self::Add => interpreter.builtin_add(call_pos, arguments),
            Self::Subtract => interpreter.builtin_subtract(call_pos, arguments),
            Self::Multiply => interpreter.builtin_multiply(call_pos, arguments),
            Self::Divide => interpreter.builtin_divide(call_pos, arguments),
            Self::LessThan => {
                interpreter
                    .builtin_numeric_comparison(call_pos, arguments, "<", |ordering| {
                        matches!(ordering, Ordering::Less)
                    })
            }
            Self::GreaterThan => {
                interpreter
                    .builtin_numeric_comparison(call_pos, arguments, ">", |ordering| {
                        matches!(ordering, Ordering::Greater)
                    })
            }
            Self::EqualNumber => {
                interpreter
                    .builtin_numeric_comparison(call_pos, arguments, "=", |ordering| {
                        matches!(ordering, Ordering::Equal)
                    })
            }
            Self::LessEqual => {
                interpreter
                    .builtin_numeric_comparison(call_pos, arguments, "<=", |ordering| {
                        matches!(ordering, Ordering::Less | Ordering::Equal)
                    })
            }
            Self::Cons => interpreter.builtin_cons(call_pos, arguments),
            Self::Car => interpreter.builtin_car(call_pos, arguments),
            Self::Cdr => interpreter.builtin_cdr(call_pos, arguments),
            Self::NullPredicate => interpreter.builtin_null(call_pos, arguments),
            Self::List => interpreter.builtin_list(call_pos, arguments),
            Self::Length => interpreter.builtin_length(call_pos, arguments),
            Self::Append => interpreter.builtin_append(call_pos, arguments),
            Self::Display => interpreter.builtin_display(call_pos, arguments),
            Self::Write => interpreter.builtin_write(call_pos, arguments),
            Self::Newline => interpreter.builtin_newline(call_pos, arguments),
            Self::StringPredicate => interpreter.builtin_string_predicate(call_pos, arguments),
            Self::StringAppend => interpreter.builtin_string_append(call_pos, arguments),
            Self::StringLength => interpreter.builtin_string_length(call_pos, arguments),
            Self::Substring => interpreter.builtin_substring(call_pos, arguments),
            Self::StringCopy => interpreter.builtin_string_copy(call_pos, arguments),
            Self::StringSet => interpreter.builtin_string_set(call_pos, arguments),
            Self::StringToNumber => interpreter.builtin_string_to_number(call_pos, arguments),
            Self::NumberToString => interpreter.builtin_number_to_string(call_pos, arguments),
            Self::SymbolToString => interpreter.builtin_symbol_to_string(call_pos, arguments),
            Self::StringToSymbol => interpreter.builtin_string_to_symbol(call_pos, arguments),
            Self::StringRef => interpreter.builtin_string_ref(call_pos, arguments),
            Self::NumberPredicate => interpreter.builtin_number_predicate(call_pos, arguments),
            Self::ExactPredicate => interpreter.builtin_exact_predicate(call_pos, arguments),
            Self::InexactPredicate => interpreter.builtin_inexact_predicate(call_pos, arguments),
            Self::IntegerPredicate => interpreter.builtin_integer_predicate(call_pos, arguments),
            Self::RationalPredicate => interpreter.builtin_rational_predicate(call_pos, arguments),
            Self::ExactToInexact => interpreter.builtin_exact_to_inexact(call_pos, arguments),
            Self::InexactToExact => interpreter.builtin_inexact_to_exact(call_pos, arguments),
            Self::Numerator => interpreter.builtin_numerator(call_pos, arguments),
            Self::Denominator => interpreter.builtin_denominator(call_pos, arguments),
            Self::BooleanPredicate => interpreter.builtin_boolean_predicate(call_pos, arguments),
            Self::CharPredicate => interpreter.builtin_char_predicate(call_pos, arguments),
            Self::PairPredicate => interpreter.builtin_pair_predicate(call_pos, arguments),
            Self::SymbolPredicate => interpreter.builtin_symbol_predicate(call_pos, arguments),
            Self::Eq => interpreter.builtin_eq(call_pos, arguments),
            Self::Equal => interpreter.builtin_equal(call_pos, arguments),
            Self::Abs => interpreter.builtin_abs(call_pos, arguments),
            Self::Modulo => interpreter.builtin_modulo(call_pos, arguments),
            Self::Remainder => interpreter.builtin_remainder(call_pos, arguments),
            Self::Quotient => interpreter.builtin_quotient(call_pos, arguments),
            Self::Min => interpreter.builtin_min(call_pos, arguments),
            Self::Max => interpreter.builtin_max(call_pos, arguments),
            Self::Expt => interpreter.builtin_expt(call_pos, arguments),
            Self::ZeroPredicate => interpreter.builtin_zero_predicate(call_pos, arguments),
            Self::PositivePredicate => interpreter.builtin_positive_predicate(call_pos, arguments),
            Self::NegativePredicate => interpreter.builtin_negative_predicate(call_pos, arguments),
            Self::OddPredicate => interpreter.builtin_odd_predicate(call_pos, arguments),
            Self::EvenPredicate => interpreter.builtin_even_predicate(call_pos, arguments),
            Self::ListRef => interpreter.builtin_list_ref(call_pos, arguments),
            Self::ListTail => interpreter.builtin_list_tail(call_pos, arguments),
            Self::ListPredicate => interpreter.builtin_list_predicate(call_pos, arguments),
            Self::Assoc => interpreter.builtin_assoc(call_pos, arguments),
            Self::Map => interpreter.builtin_map(call_pos, arguments),
            Self::CharAlphabeticPredicate => {
                interpreter.builtin_char_alphabetic_predicate(call_pos, arguments)
            }
            Self::CharNumericPredicate => {
                interpreter.builtin_char_numeric_predicate(call_pos, arguments)
            }
            Self::CharUpcase => interpreter.builtin_char_upcase(call_pos, arguments),
            Self::CharDowncase => interpreter.builtin_char_downcase(call_pos, arguments),
            Self::CharEquals => interpreter.builtin_char_equals(call_pos, arguments),
            Self::CharLessThan => interpreter.builtin_char_less_than(call_pos, arguments),
            Self::StringEquals => interpreter.builtin_string_equals(call_pos, arguments),
            Self::StringLessThan => interpreter.builtin_string_less_than(call_pos, arguments),
            Self::StringCaseInsensitiveEquals => {
                interpreter.builtin_string_case_insensitive_equals(call_pos, arguments)
            }
            Self::StringUpcase => interpreter.builtin_string_upcase(call_pos, arguments),
            Self::StringDowncase => interpreter.builtin_string_downcase(call_pos, arguments),
            Self::Not => interpreter.builtin_not(call_pos, arguments),
            Self::Apply => interpreter.builtin_apply(call_pos, arguments),
        }
    }
}

fn is_special_form(name: &str) -> bool {
    matches!(
        name,
        "and"
            | "begin"
            | "cond"
            | "define"
            | "define-syntax"
            | "else"
            | "if"
            | "lambda"
            | "let"
            | "or"
            | "quote"
            | "set!"
            | "syntax-rules"
    )
}

fn is_ellipsis_expr(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(name, _) if name == "...")
}

fn is_pattern_variable(name: &str, macro_name: &str, literals: &HashSet<String>) -> bool {
    name != "..." && name != macro_name && !literals.contains(name)
}

fn match_macro_pattern(
    pattern: &Expr,
    input: &Expr,
    macro_name: &str,
    literals: &HashSet<String>,
) -> Option<MacroBindings> {
    let mut bindings = MacroBindings::default();
    if match_pattern(pattern, input, macro_name, literals, &mut bindings, false) {
        Some(bindings)
    } else {
        None
    }
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    macro_name: &str,
    literals: &HashSet<String>,
    bindings: &mut MacroBindings,
    repeated: bool,
) -> bool {
    match pattern {
        Expr::Symbol(name, _) => {
            if is_pattern_variable(name, macro_name, literals) {
                if repeated {
                    bindings.push_repeated(name, input);
                    true
                } else {
                    bindings.bind_single(name, input)
                }
            } else {
                matches!(input, Expr::Symbol(candidate, _) if candidate == name)
            }
        }
        Expr::Number(_, _) | Expr::Bool(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {
            expr_syntax_eq(pattern, input)
        }
        Expr::List(patterns, _) => match input {
            Expr::List(values, _) => {
                match_list_pattern(patterns, values, macro_name, literals, bindings)
            }
            _ => false,
        },
    }
}

fn match_list_pattern(
    patterns: &[Expr],
    inputs: &[Expr],
    macro_name: &str,
    literals: &HashSet<String>,
    bindings: &mut MacroBindings,
) -> bool {
    let mut pattern_index = 0;
    let mut input_index = 0;

    while pattern_index < patterns.len() {
        if pattern_index + 1 < patterns.len() && is_ellipsis_expr(&patterns[pattern_index + 1]) {
            let suffix_min = min_pattern_input_count(&patterns[pattern_index + 2..]);
            if inputs.len() < input_index + suffix_min {
                return false;
            }

            let repeat_count = inputs.len() - input_index - suffix_min;
            initialize_repeated_bindings(&patterns[pattern_index], macro_name, literals, bindings);
            for input in &inputs[input_index..input_index + repeat_count] {
                if !match_pattern(
                    &patterns[pattern_index],
                    input,
                    macro_name,
                    literals,
                    bindings,
                    true,
                ) {
                    return false;
                }
            }

            input_index += repeat_count;
            pattern_index += 2;
            continue;
        }

        if input_index >= inputs.len() {
            return false;
        }

        if !match_pattern(
            &patterns[pattern_index],
            &inputs[input_index],
            macro_name,
            literals,
            bindings,
            false,
        ) {
            return false;
        }

        pattern_index += 1;
        input_index += 1;
    }

    input_index == inputs.len()
}

fn min_pattern_input_count(patterns: &[Expr]) -> usize {
    let mut count = 0;
    let mut index = 0;
    while index < patterns.len() {
        if index + 1 < patterns.len() && is_ellipsis_expr(&patterns[index + 1]) {
            index += 2;
        } else {
            count += 1;
            index += 1;
        }
    }
    count
}

fn initialize_repeated_bindings(
    pattern: &Expr,
    macro_name: &str,
    literals: &HashSet<String>,
    bindings: &mut MacroBindings,
) {
    match pattern {
        Expr::Symbol(name, _) => {
            if is_pattern_variable(name, macro_name, literals) {
                bindings.ensure_repeated(name);
            }
        }
        Expr::List(elements, _) => {
            for element in elements {
                initialize_repeated_bindings(element, macro_name, literals, bindings);
            }
        }
        Expr::Number(_, _) | Expr::Bool(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {}
    }
}

fn repetition_count_for_template(
    template: &Expr,
    bindings: &MacroBindings,
) -> Result<usize, EvalError> {
    let mut count = None;
    collect_repetition_count(template, bindings, &mut count)?;
    count.ok_or_else(|| EvalError::new("invalid syntax-rules template"))
}

fn collect_repetition_count(
    template: &Expr,
    bindings: &MacroBindings,
    count: &mut Option<usize>,
) -> Result<(), EvalError> {
    match template {
        Expr::Symbol(name, _) => {
            if let Some(len) = bindings.repeated_len(name) {
                if let Some(existing) = count {
                    if *existing != len {
                        return Err(EvalError::new("invalid syntax-rules template"));
                    }
                } else {
                    *count = Some(len);
                }
            }
        }
        Expr::List(elements, _) => {
            for element in elements {
                collect_repetition_count(element, bindings, count)?;
            }
        }
        Expr::Number(_, _) | Expr::Bool(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {}
    }
    Ok(())
}

fn expr_syntax_eq(left: &Expr, right: &Expr) -> bool {
    match (left, right) {
        (Expr::Number(left, _), Expr::Number(right, _)) => numbers_equal(*left, *right),
        (Expr::Bool(left, _), Expr::Bool(right, _)) => left == right,
        (Expr::String(left, _), Expr::String(right, _)) => left == right,
        (Expr::Char(left, _), Expr::Char(right, _)) => left == right,
        (Expr::Symbol(left, _), Expr::Symbol(right, _)) => left == right,
        (Expr::List(left, _), Expr::List(right, _)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| expr_syntax_eq(left, right))
        }
        _ => false,
    }
}

struct Parser<'a> {
    input: &'a str,
    line_starts: Vec<usize>,
    index: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            line_starts: compute_line_starts(input),
            index: 0,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_whitespace();
        while !self.is_at_end() {
            expressions.push(self.parse_expr()?);
            self.skip_whitespace();
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace();
        if self.is_at_end() {
            return Err(self.error_at_current("unexpected end of input"));
        }

        let pos = self.position_at(self.index);
        match self.peek_char() {
            '\'' => self.parse_quoted(pos),
            '(' => self.parse_list(pos),
            ')' => Err(self.error_at_current("unexpected )")),
            '"' => self.parse_string(pos),
            '#' => self.parse_boolean_or_character(pos),
            _ => self.parse_atom(pos),
        }
    }

    fn parse_quoted(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.index += 1;
        Ok(Expr::List(
            vec![Expr::Symbol("quote".to_owned(), pos), self.parse_expr()?],
            pos,
        ))
    }

    fn parse_list(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.index += 1;
        let mut elements = Vec::new();
        self.skip_whitespace();

        loop {
            if self.is_at_end() {
                return Err(self.error_at(pos, "unterminated list"));
            }
            if self.peek_char() == ')' {
                self.index += 1;
                return Ok(Expr::List(elements, pos));
            }
            elements.push(self.parse_expr()?);
            self.skip_whitespace();
        }
    }

    fn parse_string(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.index += 1;
        let mut value = String::new();
        while !self.is_at_end() {
            let ch = self.next_char();
            if ch == '"' {
                return Ok(Expr::String(value, pos));
            }
            if ch == '\\' {
                if self.is_at_end() {
                    return Err(self.error_at(pos, "unterminated string"));
                }
                let escaped = self.next_char();
                value.push(match escaped {
                    'n' => '\n',
                    't' => '\t',
                    '"' => '"',
                    '\\' => '\\',
                    other => other,
                });
            } else {
                value.push(ch);
            }
        }
        Err(self.error_at(pos, "unterminated string"))
    }

    fn parse_boolean_or_character(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        if self.matches_token("#t") {
            self.index += 2;
            return Ok(Expr::Bool(true, pos));
        }
        if self.matches_token("#f") {
            self.index += 2;
            return Ok(Expr::Bool(false, pos));
        }
        if self.input[self.index..].starts_with("#\\") {
            return self.parse_character(pos);
        }
        Err(self.error_at(pos, "invalid boolean literal"))
    }

    fn parse_character(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.index += 2;
        let start = self.index;
        while !self.is_at_end() && !is_delimiter(self.peek_char()) {
            self.index += 1;
        }

        let token = &self.input[start..self.index];
        if token.is_empty() {
            return Err(self.error_at(pos, "invalid character literal"));
        }
        if token.len() == 1 {
            return Ok(Expr::Char(token.chars().next().unwrap(), pos));
        }

        match token {
            "space" => Ok(Expr::Char(' ', pos)),
            "newline" => Ok(Expr::Char('\n', pos)),
            _ => Err(self.error_at(pos, "invalid character literal")),
        }
    }

    fn parse_atom(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        let start = self.index;
        while !self.is_at_end() && !is_delimiter(self.peek_char()) {
            self.index += 1;
        }

        let token = &self.input[start..self.index];
        if token.is_empty() {
            return Err(self.error_at(pos, "unexpected token"));
        }
        match parse_number_token(token) {
            Ok(Some(number)) => return Ok(Expr::Number(number, pos)),
            Ok(None) => {}
            Err(error) => return Err(self.error_at(pos, error.message(token))),
        }
        Ok(Expr::Symbol(token.to_owned(), pos))
    }

    fn matches_token(&self, token: &str) -> bool {
        let end = self.index + token.len();
        if end > self.input.len() {
            return false;
        }
        if !self.input[self.index..].starts_with(token) {
            return false;
        }
        end == self.input.len() || is_delimiter(self.input.as_bytes()[end] as char)
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            let ch = self.peek_char();
            if ch.is_ascii_whitespace() {
                self.index += 1;
                continue;
            }
            if ch == ';' {
                while !self.is_at_end() && self.peek_char() != '\n' {
                    self.index += 1;
                }
                continue;
            }
            return;
        }
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.input.len()
    }

    fn peek_char(&self) -> char {
        self.input.as_bytes()[self.index] as char
    }

    fn next_char(&mut self) -> char {
        let ch = self.peek_char();
        self.index += 1;
        ch
    }

    fn error_at_current(&self, message: impl Into<String>) -> EvalError {
        self.error_at(self.position_at(self.index), message)
    }

    fn error_at(&self, pos: SourcePos, message: impl Into<String>) -> EvalError {
        EvalError::at(message, pos.line, pos.column)
    }

    fn position_at(&self, absolute_index: usize) -> SourcePos {
        let capped_index = absolute_index.min(self.input.len());
        let mut low = 0_usize;
        let mut high = self.line_starts.len() - 1;
        let mut line_index = 0_usize;

        while low <= high {
            let mid = (low + high) >> 1;
            let line_start = self.line_starts[mid];
            if line_start <= capped_index {
                line_index = mid;
                low = mid + 1;
            } else if mid == 0 {
                break;
            } else {
                high = mid - 1;
            }
        }

        let line_start = self.line_starts[line_index];
        SourcePos {
            line: line_index + 1,
            column: capped_index - line_start + 1,
        }
    }
}

fn compute_line_starts(source: &str) -> Vec<usize> {
    let mut starts = vec![0];
    for (index, byte) in source.bytes().enumerate() {
        if byte == b'\n' {
            starts.push(index + 1);
        }
    }
    starts
}

fn is_integer_token(token: &str) -> bool {
    if token.is_empty() {
        return false;
    }

    let bytes = token.as_bytes();
    let mut start = 0;
    if matches!(bytes[0], b'+' | b'-') {
        if bytes.len() == 1 {
            return false;
        }
        start = 1;
    }

    bytes[start..].iter().all(u8::is_ascii_digit)
}

fn parse_number_token(token: &str) -> Result<Option<Number>, NumberParseError> {
    if is_integer_token(token) {
        return token
            .parse::<i64>()
            .map(Number::int)
            .map(Some)
            .map_err(|_| NumberParseError::InvalidInteger);
    }

    if is_rational_token(token) {
        let slash_index = token.find('/').unwrap();
        let numerator = token[..slash_index]
            .parse::<i64>()
            .map_err(|_| NumberParseError::InvalidRational)?;
        let denominator = token[slash_index + 1..]
            .parse::<i64>()
            .map_err(|_| NumberParseError::InvalidRational)?;
        if denominator == 0 {
            return Err(NumberParseError::InvalidRational);
        }
        return Ok(Some(Number::rational(numerator, denominator)));
    }

    if is_inexact_token(token) {
        return token
            .parse::<f64>()
            .map(Number::Inexact)
            .map(Some)
            .map_err(|_| NumberParseError::InvalidInexact);
    }

    Ok(None)
}

fn is_rational_token(token: &str) -> bool {
    let Some(slash_index) = token.find('/') else {
        return false;
    };
    if token[slash_index + 1..].contains('/') || slash_index == 0 {
        return false;
    }
    is_integer_token(&token[..slash_index]) && is_integer_token(&token[slash_index + 1..])
}

fn is_inexact_token(token: &str) -> bool {
    if token.is_empty() {
        return false;
    }

    let bytes = token.as_bytes();
    let mut start = 0;
    if matches!(bytes[0], b'+' | b'-') {
        if bytes.len() == 1 {
            return false;
        }
        start = 1;
    }

    let body = &token[start..];
    let Some(dot_index) = body.find('.') else {
        return false;
    };
    if body[dot_index + 1..].contains('.') {
        return false;
    }

    let whole = &body[..dot_index];
    let fractional = &body[dot_index + 1..];
    if whole.is_empty() && fractional.is_empty() {
        return false;
    }

    digits_only(whole) && digits_only(fractional)
}

fn digits_only(text: &str) -> bool {
    text.bytes().all(|byte| byte.is_ascii_digit())
}

fn inexact_to_exact_number(number: f64) -> Number {
    decimal_text_to_exact(&render_inexact(number))
}

fn decimal_text_to_exact(text: &str) -> Number {
    let sign = if text.starts_with('-') { -1_i128 } else { 1_i128 };
    let digits = text.trim_start_matches(['+', '-']);
    let (whole, fractional) = digits.split_once('.').unwrap_or((digits, ""));
    let whole_value = if whole.is_empty() {
        0_i128
    } else {
        whole.parse::<i128>().unwrap()
    };
    let fractional_value = if fractional.is_empty() {
        0_i128
    } else {
        fractional.parse::<i128>().unwrap()
    };

    let denominator = 10_i128.pow(fractional.len() as u32);
    let numerator = sign * (whole_value * denominator + fractional_value);
    let (numerator, denominator) = normalize_fraction(numerator, denominator);
    Number::Exact(numerator, denominator)
}

fn is_delimiter(ch: char) -> bool {
    ch.is_ascii_whitespace() || matches!(ch, '(' | ')' | ';')
}

fn string_cell(text: &str) -> StringCell {
    Rc::new(RefCell::new(text.chars().collect()))
}

fn chars_to_string(chars: &[char]) -> String {
    chars.iter().collect()
}

fn pair_parts(value: &Value) -> Option<(Value, Value)> {
    match value {
        Value::Pair(pair) => Some((pair.car.clone(), pair.cdr.clone())),
        _ => None,
    }
}

fn render_value(value: &Value, display_mode: bool) -> String {
    match value {
        Value::Number(number) => number.render(),
        Value::Bool(true) => "#t".to_owned(),
        Value::Bool(false) => "#f".to_owned(),
        Value::String(text) => {
            let text = chars_to_string(&text.borrow());
            if display_mode {
                text
            } else {
                render_string_literal(&text)
            }
        }
        Value::Symbol(symbol) => symbol.clone(),
        Value::Char(ch) => {
            if display_mode {
                ch.to_string()
            } else {
                render_character_literal(*ch)
            }
        }
        Value::Pair(pair) => render_pair(pair, display_mode),
        Value::EmptyList => "()".to_owned(),
        Value::Builtin(builtin) => format!("#<procedure {}>", builtin.name()),
        Value::Lambda(lambda) => format!("#<procedure {}>", lambda.display_name()),
        Value::Void => String::new(),
    }
}

fn render_pair(pair: &PairValue, display_mode: bool) -> String {
    let mut builder = String::from("(");
    let mut current = Value::Pair(Rc::new(pair.clone()));
    let mut first = true;

    while let Some((car, cdr)) = pair_parts(&current) {
        if !first {
            builder.push(' ');
        }
        builder.push_str(&render_value(&car, display_mode));
        current = cdr;
        first = false;
    }

    if matches!(current, Value::EmptyList) {
        builder.push(')');
        return builder;
    }

    builder.push_str(" . ");
    builder.push_str(&render_value(&current, display_mode));
    builder.push(')');
    builder
}

fn render_string_literal(value: &str) -> String {
    let mut escaped = String::new();
    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\t' => escaped.push_str("\\t"),
            other => escaped.push(other),
        }
    }
    format!("\"{escaped}\"")
}

fn render_character_literal(value: char) -> String {
    match value {
        ' ' => "#\\space".to_owned(),
        '\n' => "#\\newline".to_owned(),
        other => format!("#\\{other}"),
    }
}

#[cfg(test)]
mod tests;
