use std::collections::{HashMap, HashSet};
use std::rc::Rc;

mod builtins;
pub mod error;
mod model;
mod number;
mod parser;

use builtins::apply_builtin;
pub use error::EvalError;
use error::SourcePos;
use model::{
    expr_datum_eq, fresh_identifier, is_core_syntax, is_ellipsis, Builtin, Env, EnvRef,
    ExpansionState, Expr, MacroExpansion, MacroRef, MacroTransformer, Params, PatternBindings,
    Procedure, SchemeString, SyntaxRule, Value,
};
use parser::Parser;

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
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let env = initial_env();
    let mut output = String::new();
    let value = eval_sequence(&exprs, &env, &mut output)?;
    Ok((value, output))
}

fn initial_env() -> EnvRef {
    let env = Env::new(None);

    for builtin in [
        Builtin::Add,
        Builtin::Sub,
        Builtin::Mul,
        Builtin::Div,
        Builtin::Abs,
        Builtin::Modulo,
        Builtin::Remainder,
        Builtin::Quotient,
        Builtin::Min,
        Builtin::Max,
        Builtin::Expt,
        Builtin::ZeroPred,
        Builtin::PositivePred,
        Builtin::NegativePred,
        Builtin::OddPred,
        Builtin::EvenPred,
        Builtin::ExactPred,
        Builtin::InexactPred,
        Builtin::IntegerPred,
        Builtin::RationalPred,
        Builtin::ExactToInexact,
        Builtin::InexactToExact,
        Builtin::Numerator,
        Builtin::Denominator,
        Builtin::Less,
        Builtin::Greater,
        Builtin::Equal,
        Builtin::LessEqual,
        Builtin::EqPred,
        Builtin::EqualPred,
        Builtin::Not,
        Builtin::Display,
        Builtin::Write,
        Builtin::Newline,
        Builtin::Cons,
        Builtin::Car,
        Builtin::Cdr,
        Builtin::Append,
        Builtin::List,
        Builtin::Length,
        Builtin::ListRef,
        Builtin::ListTail,
        Builtin::ListPred,
        Builtin::Assoc,
        Builtin::Map,
        Builtin::StringAppend,
        Builtin::StringLength,
        Builtin::Substring,
        Builtin::StringToNumber,
        Builtin::NumberToString,
        Builtin::SymbolToString,
        Builtin::StringToSymbol,
        Builtin::StringRef,
        Builtin::StringSet,
        Builtin::StringCopy,
        Builtin::NullPred,
        Builtin::NumberPred,
        Builtin::StringPred,
        Builtin::BooleanPred,
        Builtin::PairPred,
        Builtin::SymbolPred,
        Builtin::CharPred,
        Builtin::CharAlphabeticPred,
        Builtin::CharNumericPred,
        Builtin::CharUpcase,
        Builtin::CharDowncase,
        Builtin::CharEqual,
        Builtin::CharLess,
        Builtin::StringEqual,
        Builtin::StringLess,
        Builtin::StringCiEqual,
        Builtin::StringUpcase,
        Builtin::StringDowncase,
        Builtin::Apply,
    ] {
        env.define(builtin.name().into(), Value::Builtin(builtin));
    }

    env
}

fn eval_sequence(exprs: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Void;

    for expr in exprs {
        result = eval(expr, env, output)?;
    }

    Ok(result)
}

fn eval(expr: &Expr, env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let pos = expr.pos();

    match expr {
        Expr::Number(value, _) => Ok(Value::Number(*value)),
        Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
        Expr::String(value, _) => Ok(Value::String(SchemeString::literal(value))),
        Expr::Char(value, _) => Ok(Value::Char(*value)),
        Expr::Symbol(name, _) => env
            .lookup(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() })
            .map_err(|error| error.with_position(pos)),
        Expr::List(items, _) => {
            eval_list(items, env, output).map_err(|error| error.with_position(pos))
        }
    }
}

fn eval_list(items: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "cannot evaluate empty list".into(),
        });
    };

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => return eval_define(tail, env, output),
            "define-syntax" => return eval_define_syntax(tail, env),
            "set!" => return eval_set(tail, env, output),
            "if" => return eval_if(tail, env, output),
            "quote" => return eval_quote(tail),
            "lambda" => return build_lambda(tail, env, None),
            "and" => return eval_and(tail, env, output),
            "or" => return eval_or(tail, env, output),
            "begin" => return eval_begin(tail, env, output),
            "cond" => return eval_cond(tail, env, output),
            "let" => return eval_let(tail, env, output),
            _ => {}
        }

        if let Some(transformer) = env.lookup_macro(name) {
            let expansion = expand_macro_call(items, &transformer)?;
            let expanded_env = env_with_expansion_aliases(env, &expansion);
            return eval(&expansion.expr, &expanded_env, output);
        }
    }

    let callable = eval(head, env, output)?;
    let args = eval_args(tail, env, output)?;
    apply(callable, &args, output)
}

fn eval_define(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), value_expr] => {
            let value = if let Some(parts) = lambda_parts(value_expr) {
                build_lambda(parts, env, Some(name.clone()))?
            } else {
                eval(value_expr, env, output)?
            };
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List(signature, _), body @ ..] => {
            let Some((Expr::Symbol(name, _), params)) = signature.split_first() else {
                return Err(EvalError::Syntax {
                    message: "define: expected function name".into(),
                });
            };
            if body.is_empty() {
                return Err(EvalError::Syntax {
                    message: "define: expected function body".into(),
                });
            }

            let value = new_procedure(Some(name.clone()), parse_param_list(params)?, body, env);
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Syntax {
            message: "define: invalid syntax".into(),
        }),
    }
}

fn eval_define_syntax(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), transformer_expr] => {
            let transformer = parse_syntax_rules(transformer_expr, env)?;
            env.define_macro(name.clone(), transformer);
            Ok(Value::Void)
        }
        [_, _] => Err(EvalError::Syntax {
            message: "define-syntax: expected transformer name".into(),
        }),
        _ => Err(wrong_arg_count("define-syntax", "2", args.len())),
    }
}

fn eval_set(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), value_expr] => {
            let value = eval(value_expr, env, output)?;
            if env.set(name, value) {
                Ok(Value::Void)
            } else {
                Err(EvalError::UnboundVariable { name: name.clone() })
            }
        }
        [_, _] => Err(EvalError::Syntax {
            message: "set!: expected variable name".into(),
        }),
        _ => Err(wrong_arg_count("set!", "2", args.len())),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [condition, then_branch, else_branch] => {
            if eval(condition, env, output)?.is_truthy() {
                eval(then_branch, env, output)
            } else {
                eval(else_branch, env, output)
            }
        }
        _ => Err(wrong_arg_count("if", "3", args.len())),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    match args {
        [expr] => Ok(quote_expr(expr)),
        _ => Err(wrong_arg_count("quote", "1", args.len())),
    }
}

fn eval_args(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::with_capacity(args.len());
    for expr in args {
        values.push(eval(expr, env, output)?);
    }
    Ok(values)
}

fn eval_and(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for arg in args {
        let value = eval(arg, env, output)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    for arg in args {
        let value = eval(arg, env, output)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_begin(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    eval_sequence(args, env, output)
}

fn eval_cond(clauses: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::Syntax {
                message: "cond: expected clause".into(),
            });
        };
        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::Syntax {
                message: "cond: expected clause".into(),
            });
        };

        if matches!(test, Expr::Symbol(name, _) if name == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::Syntax {
                    message: "cond: else must be last".into(),
                });
            }
            return eval_sequence(body, env, output);
        }

        let value = eval(test, env, output)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(value)
            } else {
                eval_sequence(body, env, output)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), bindings, body @ ..] => {
            eval_named_let(name, bindings, body, env, output)
        }
        [bindings, body @ ..] => eval_plain_let(bindings, body, env, output),
        _ => Err(EvalError::Syntax {
            message: "let: invalid syntax".into(),
        }),
    }
}

fn eval_plain_let(
    bindings_expr: &Expr,
    body: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let mut values = Vec::with_capacity(bindings.len());
    for (_, value_expr) in &bindings {
        values.push(eval(value_expr, env, output)?);
    }

    let let_env = Env::new(Some(env.clone()));
    for ((name, _), value) in bindings.into_iter().zip(values) {
        let_env.define(name, value);
    }

    eval_sequence(body, &let_env, output)
}

fn eval_named_let(
    name: &str,
    bindings_expr: &Expr,
    body: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let mut args = Vec::with_capacity(bindings.len());
    for (_, value_expr) in &bindings {
        args.push(eval(value_expr, env, output)?);
    }
    let params = bindings.iter().map(|(param, _)| param.clone()).collect();

    let let_env = Env::new(Some(env.clone()));
    let procedure = new_procedure(Some(name.into()), Params::fixed(params), body, &let_env);
    let_env.define(name.into(), procedure.clone());
    apply(procedure, &args, output)
}

fn parse_let_bindings(bindings_expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings, _) = bindings_expr else {
        return Err(EvalError::Syntax {
            message: "let: expected bindings".into(),
        });
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        match binding {
            Expr::List(parts, _) => match parts.as_slice() {
                [Expr::Symbol(name, _), value_expr] => {
                    parsed.push((name.clone(), value_expr.clone()));
                }
                _ => {
                    return Err(EvalError::Syntax {
                        message: "let: expected binding pair".into(),
                    });
                }
            },
            _ => {
                return Err(EvalError::Syntax {
                    message: "let: expected binding pair".into(),
                });
            }
        }
    }

    Ok(parsed)
}

fn parse_syntax_rules(expr: &Expr, env: &EnvRef) -> Result<MacroRef, EvalError> {
    let Expr::List(items, _) = expr else {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected syntax-rules form".into(),
        });
    };

    let [Expr::Symbol(keyword, _), Expr::List(literal_exprs, _), rules @ ..] = items.as_slice()
    else {
        return Err(EvalError::Syntax {
            message: "define-syntax: invalid syntax-rules form".into(),
        });
    };

    if keyword != "syntax-rules" {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected syntax-rules".into(),
        });
    }

    if rules.is_empty() {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected at least one syntax-rules clause".into(),
        });
    }

    let mut literals = HashSet::new();
    for literal in literal_exprs {
        match literal {
            Expr::Symbol(name, _) if name != "..." => {
                literals.insert(name.clone());
            }
            _ => {
                return Err(EvalError::Syntax {
                    message: "syntax-rules: expected literal identifier".into(),
                });
            }
        }
    }

    let mut parsed_rules = Vec::with_capacity(rules.len());
    for rule in rules {
        let Expr::List(parts, _) = rule else {
            return Err(EvalError::Syntax {
                message: "syntax-rules: expected rule".into(),
            });
        };

        match parts.as_slice() {
            [pattern, template] => parsed_rules.push(SyntaxRule {
                pattern: pattern.clone(),
                template: template.clone(),
            }),
            _ => {
                return Err(EvalError::Syntax {
                    message: "syntax-rules: expected (pattern template)".into(),
                });
            }
        }
    }

    Ok(Rc::new(MacroTransformer {
        literals,
        rules: parsed_rules,
        env: env.clone(),
    }))
}

fn env_with_expansion_aliases(env: &EnvRef, expansion: &MacroExpansion) -> EnvRef {
    if expansion.value_aliases.is_empty() && expansion.macro_aliases.is_empty() {
        return env.clone();
    }

    let expanded_env = Env::new(Some(env.clone()));
    for (name, cell) in &expansion.value_aliases {
        expanded_env.define_alias(name.clone(), cell.clone());
    }
    for (name, transformer) in &expansion.macro_aliases {
        expanded_env.define_macro(name.clone(), transformer.clone());
    }
    expanded_env
}

fn expand_macro_call(items: &[Expr], transformer: &MacroRef) -> Result<MacroExpansion, EvalError> {
    let call_expr = Expr::List(items.to_vec(), items[0].pos());

    for rule in &transformer.rules {
        let mut bindings = PatternBindings::default();
        if match_macro_rule(rule, &call_expr, &transformer.literals, &mut bindings)? {
            let mut state = ExpansionState::new(bindings, &transformer.env);
            let expr = expand_template_expr(&rule.template, &mut state, &HashMap::new(), None)?;
            return Ok(MacroExpansion {
                expr,
                value_aliases: state.value_aliases,
                macro_aliases: state.macro_aliases,
            });
        }
    }

    Err(EvalError::Syntax {
        message: "syntax-rules: no matching clause".into(),
    })
}

fn match_macro_rule(
    rule: &SyntaxRule,
    call_expr: &Expr,
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
) -> Result<bool, EvalError> {
    let Expr::List(pattern_items, _) = &rule.pattern else {
        return Err(EvalError::Syntax {
            message: "syntax-rules: expected list pattern".into(),
        });
    };
    let Expr::List(call_items, _) = call_expr else {
        return Ok(false);
    };

    if pattern_items.is_empty() {
        return Err(EvalError::Syntax {
            message: "syntax-rules: expected macro name in pattern".into(),
        });
    }

    if call_items.is_empty() {
        return Ok(false);
    }

    match_list_pattern(
        &pattern_items[1..],
        &call_items[1..],
        literals,
        bindings,
        false,
    )
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
    repeated: bool,
) -> Result<bool, EvalError> {
    match pattern {
        Expr::Number(_, _) | Expr::Boolean(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {
            Ok(expr_datum_eq(pattern, input))
        }
        Expr::Symbol(name, _) => {
            if name == "..." {
                return Err(EvalError::Syntax {
                    message: "syntax-rules: invalid ellipsis pattern".into(),
                });
            }

            if literals.contains(name) {
                return Ok(matches!(input, Expr::Symbol(other, _) if other == name));
            }

            Ok(if repeated {
                bindings.bind_repeated(name, input)
            } else {
                bindings.bind_single(name, input)
            })
        }
        Expr::List(pattern_items, _) => match input {
            Expr::List(input_items, _) => {
                match_list_pattern(pattern_items, input_items, literals, bindings, repeated)
            }
            _ => Ok(false),
        },
    }
}

fn match_list_pattern(
    pattern_items: &[Expr],
    input_items: &[Expr],
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
    repeated: bool,
) -> Result<bool, EvalError> {
    let Some(ellipsis_index) = find_ellipsis_index(pattern_items)? else {
        if pattern_items.len() != input_items.len() {
            return Ok(false);
        }

        for (pattern, input) in pattern_items.iter().zip(input_items.iter()) {
            if !match_pattern(pattern, input, literals, bindings, repeated)? {
                return Ok(false);
            }
        }

        return Ok(true);
    };

    let repeated_pattern = &pattern_items[ellipsis_index - 1];
    let prefix = &pattern_items[..ellipsis_index - 1];
    let suffix = &pattern_items[ellipsis_index + 1..];

    if input_items.len() < prefix.len() + suffix.len() {
        return Ok(false);
    }

    for (pattern, input) in prefix.iter().zip(input_items.iter()) {
        if !match_pattern(pattern, input, literals, bindings, repeated)? {
            return Ok(false);
        }
    }

    let repeat_count = input_items.len() - prefix.len() - suffix.len();
    seed_repeated_bindings(repeated_pattern, literals, bindings)?;
    for input in &input_items[prefix.len()..prefix.len() + repeat_count] {
        if !match_pattern(repeated_pattern, input, literals, bindings, true)? {
            return Ok(false);
        }
    }

    for (pattern, input) in suffix
        .iter()
        .zip(input_items[prefix.len() + repeat_count..].iter())
    {
        if !match_pattern(pattern, input, literals, bindings, repeated)? {
            return Ok(false);
        }
    }

    Ok(true)
}

fn seed_repeated_bindings(
    pattern: &Expr,
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
) -> Result<(), EvalError> {
    match pattern {
        Expr::Symbol(name, _) => {
            if name == "..." {
                return Err(EvalError::Syntax {
                    message: "syntax-rules: invalid ellipsis pattern".into(),
                });
            }

            if !literals.contains(name) {
                bindings.seed_repeated(name);
            }
        }
        Expr::List(items, _) => {
            for item in items {
                if !is_ellipsis(item) {
                    seed_repeated_bindings(item, literals, bindings)?;
                }
            }
        }
        Expr::Number(_, _) | Expr::Boolean(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {}
    }

    Ok(())
}

fn find_ellipsis_index(items: &[Expr]) -> Result<Option<usize>, EvalError> {
    let mut ellipsis_index = None;

    for (index, item) in items.iter().enumerate() {
        if !is_ellipsis(item) {
            continue;
        }

        if index == 0 {
            return Err(EvalError::Syntax {
                message: "syntax-rules: ellipsis must follow a pattern".into(),
            });
        }

        if ellipsis_index.is_some() {
            return Err(EvalError::Syntax {
                message: "syntax-rules: multiple ellipses at one list level are unsupported".into(),
            });
        }

        ellipsis_index = Some(index);
    }

    Ok(ellipsis_index)
}

fn expand_template_expr(
    template: &Expr,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match template {
        Expr::Number(_, _) | Expr::Boolean(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {
            Ok(template.clone())
        }
        Expr::Symbol(name, pos) => expand_template_symbol(name, *pos, state, scope, repeat_index),
        Expr::List(items, pos) => {
            if let Some(Expr::Symbol(name, _)) = items.first() {
                match name.as_str() {
                    "let" => {
                        if let Some(expanded) =
                            expand_let_template(items, *pos, state, scope, repeat_index)?
                        {
                            return Ok(expanded);
                        }
                    }
                    "lambda" => {
                        if let Some(expanded) =
                            expand_lambda_template(items, *pos, state, scope, repeat_index)?
                        {
                            return Ok(expanded);
                        }
                    }
                    _ => {}
                }
            }

            Ok(Expr::List(
                expand_template_items(items, state, scope, repeat_index)?,
                *pos,
            ))
        }
    }
}

fn expand_template_items(
    items: &[Expr],
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Vec<Expr>, EvalError> {
    let mut expanded = Vec::with_capacity(items.len());
    let mut index = 0;

    while index < items.len() {
        if is_ellipsis(&items[index]) {
            return Err(EvalError::Syntax {
                message: "template: unexpected ellipsis".into(),
            });
        }

        if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
            let repeat_count = repetition_len_for_template(&items[index], &state.bindings, scope)?;
            for repeated_index in 0..repeat_count {
                expanded.push(expand_template_expr(
                    &items[index],
                    state,
                    scope,
                    Some(repeated_index),
                )?);
            }
            index += 2;
            continue;
        }

        expanded.push(expand_template_expr(
            &items[index],
            state,
            scope,
            repeat_index,
        )?);
        index += 1;
    }

    Ok(expanded)
}

fn expand_template_symbol(
    name: &str,
    pos: SourcePos,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if let Some(expr) = state.bindings.substitute(name, repeat_index)? {
        return Ok(expr);
    }

    if let Some(renamed) = scope.get(name) {
        return Ok(Expr::Symbol(renamed.clone(), pos));
    }

    if name == "..." || is_core_syntax(name) {
        return Ok(Expr::Symbol(name.into(), pos));
    }

    if let Some(alias) = state.alias_names.get(name) {
        return Ok(Expr::Symbol(alias.clone(), pos));
    }

    let mut captured_any = false;
    let alias = fresh_identifier(name);

    if let Some(cell) = state.definition_env.lookup_cell(name) {
        state.value_aliases.push((alias.clone(), cell));
        captured_any = true;
    }

    if let Some(transformer) = state.definition_env.lookup_macro(name) {
        state.macro_aliases.push((alias.clone(), transformer));
        captured_any = true;
    }

    if captured_any {
        state.alias_names.insert(name.into(), alias.clone());
        Ok(Expr::Symbol(alias, pos))
    } else {
        Ok(Expr::Symbol(name.into(), pos))
    }
}

fn expand_let_template(
    items: &[Expr],
    pos: SourcePos,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Option<Expr>, EvalError> {
    let [head, bindings_expr, body @ ..] = items else {
        return Ok(None);
    };

    let Expr::List(bindings, bindings_pos) = bindings_expr else {
        return Ok(None);
    };

    let mut introduced = HashMap::new();
    let mut expanded_bindings = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let Expr::List(parts, binding_pos) = binding else {
            return Ok(None);
        };

        let [name_expr, value_expr] = parts.as_slice() else {
            return Ok(None);
        };

        let expanded_name =
            expand_binding_target(name_expr, state, scope, repeat_index, &mut introduced)?;
        let expanded_value = expand_template_expr(value_expr, state, scope, repeat_index)?;
        expanded_bindings.push(Expr::List(
            vec![expanded_name, expanded_value],
            *binding_pos,
        ));
    }

    let mut body_scope = scope.clone();
    body_scope.extend(introduced);

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head.clone());
    expanded_items.push(Expr::List(expanded_bindings, *bindings_pos));
    for expr in body {
        expanded_items.push(expand_template_expr(
            expr,
            state,
            &body_scope,
            repeat_index,
        )?);
    }

    Ok(Some(Expr::List(expanded_items, pos)))
}

fn expand_lambda_template(
    items: &[Expr],
    pos: SourcePos,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Option<Expr>, EvalError> {
    let [head, params_expr, body @ ..] = items else {
        return Ok(None);
    };

    let (expanded_params, introduced) =
        expand_parameter_list(params_expr, state, scope, repeat_index)?;
    let mut body_scope = scope.clone();
    body_scope.extend(introduced);

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head.clone());
    expanded_items.push(expanded_params);
    for expr in body {
        expanded_items.push(expand_template_expr(
            expr,
            state,
            &body_scope,
            repeat_index,
        )?);
    }

    Ok(Some(Expr::List(expanded_items, pos)))
}

fn expand_parameter_list(
    params_expr: &Expr,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<(Expr, HashMap<String, String>), EvalError> {
    let Expr::List(params, pos) = params_expr else {
        return Ok((
            expand_template_expr(params_expr, state, scope, repeat_index)?,
            HashMap::new(),
        ));
    };

    let mut introduced = HashMap::new();
    let mut expanded = Vec::with_capacity(params.len());
    let mut index = 0;

    while index < params.len() {
        match &params[index] {
            Expr::Symbol(name, _) if name == "." => {
                expanded.push(params[index].clone());
                index += 1;
            }
            param => {
                expanded.push(expand_binding_target(
                    param,
                    state,
                    scope,
                    repeat_index,
                    &mut introduced,
                )?);
                index += 1;
            }
        }
    }

    Ok((Expr::List(expanded, *pos), introduced))
}

fn expand_binding_target(
    target: &Expr,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
    introduced: &mut HashMap<String, String>,
) -> Result<Expr, EvalError> {
    match target {
        Expr::Symbol(name, pos) => {
            if let Some(expr) = state.bindings.substitute(name, repeat_index)? {
                return Ok(expr);
            }

            let fresh = fresh_identifier(name);
            introduced.insert(name.clone(), fresh.clone());
            Ok(Expr::Symbol(fresh, *pos))
        }
        _ => expand_template_expr(target, state, scope, repeat_index),
    }
}

fn repetition_len_for_template(
    template: &Expr,
    bindings: &PatternBindings,
    scope: &HashMap<String, String>,
) -> Result<usize, EvalError> {
    let mut repetition_len = None;
    collect_repetition_lens(template, bindings, scope, &mut repetition_len)?;

    repetition_len.ok_or_else(|| EvalError::Syntax {
        message: "template: ellipsis without repeated pattern variable".into(),
    })
}

fn collect_repetition_lens(
    expr: &Expr,
    bindings: &PatternBindings,
    scope: &HashMap<String, String>,
    repetition_len: &mut Option<usize>,
) -> Result<(), EvalError> {
    match expr {
        Expr::Symbol(name, _) => {
            if scope.contains_key(name) {
                return Ok(());
            }

            if let Some(current_len) = bindings.repetition_len(name) {
                match repetition_len {
                    Some(existing_len) if *existing_len != current_len => {
                        return Err(EvalError::Syntax {
                            message: "template: inconsistent ellipsis lengths".into(),
                        });
                    }
                    Some(_) => {}
                    None => *repetition_len = Some(current_len),
                }
            }
        }
        Expr::List(items, _) => {
            for item in items {
                if !is_ellipsis(item) {
                    collect_repetition_lens(item, bindings, scope, repetition_len)?;
                }
            }
        }
        Expr::Number(_, _) | Expr::Boolean(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {}
    }

    Ok(())
}

fn build_lambda(parts: &[Expr], env: &EnvRef, name: Option<String>) -> Result<Value, EvalError> {
    let [params_expr, body @ ..] = parts else {
        return Err(EvalError::Syntax {
            message: "lambda: expected parameters and body".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "lambda: expected body".into(),
        });
    }

    Ok(new_procedure(
        name,
        parse_params_expr(params_expr)?,
        body,
        env,
    ))
}

fn new_procedure(name: Option<String>, params: Params, body: &[Expr], env: &EnvRef) -> Value {
    Value::Procedure(Rc::new(Procedure {
        name,
        params,
        body: body.to_vec(),
        env: env.clone(),
    }))
}

fn parse_params_expr(params: &Expr) -> Result<Params, EvalError> {
    let Expr::List(items, _) = params else {
        return Err(EvalError::Syntax {
            message: "lambda: expected parameter list".into(),
        });
    };

    parse_param_list(items)
}

fn parse_param_list(params: &[Expr]) -> Result<Params, EvalError> {
    let mut required = Vec::with_capacity(params.len());
    let mut index = 0;

    while index < params.len() {
        match &params[index] {
            Expr::Symbol(name, _) if name == "." => {
                if index + 2 != params.len() {
                    return Err(EvalError::Syntax {
                        message: "lambda: expected parameter name".into(),
                    });
                }

                return match &params[index + 1] {
                    Expr::Symbol(name, _) if name != "." => Ok(Params {
                        required,
                        rest: Some(name.clone()),
                    }),
                    _ => Err(EvalError::Syntax {
                        message: "lambda: expected parameter name".into(),
                    }),
                };
            }
            Expr::Symbol(name, _) => required.push(name.clone()),
            _ => {
                return Err(EvalError::Syntax {
                    message: "lambda: expected parameter name".into(),
                });
            }
        }

        index += 1;
    }

    Ok(Params {
        required,
        rest: None,
    })
}

fn lambda_parts(expr: &Expr) -> Option<&[Expr]> {
    let Expr::List(items, _) = expr else {
        return None;
    };
    let (Expr::Symbol(name, _), tail) = items.split_first()? else {
        return None;
    };

    if name == "lambda" {
        Some(tail)
    } else {
        None
    }
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Number(value, _) => Value::Number(*value),
        Expr::Boolean(value, _) => Value::Boolean(*value),
        Expr::String(value, _) => Value::String(SchemeString::literal(value)),
        Expr::Char(value, _) => Value::Char(*value),
        Expr::Symbol(value, _) => Value::Symbol(value.clone()),
        Expr::List(items, _) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn apply(callable: Value, args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match callable {
        Value::Builtin(builtin) => apply_builtin(builtin, args, output),
        Value::Procedure(procedure) => apply_procedure(&procedure, args, output),
        value => Err(EvalError::NotAProcedure {
            got: value.type_name().into(),
        }),
    }
}

fn apply_procedure(
    procedure: &Procedure,
    args: &[Value],
    output: &mut String,
) -> Result<Value, EvalError> {
    if !procedure.params.matches_arity(args.len()) {
        let name = procedure.name.as_deref().unwrap_or("lambda");
        let expected = procedure.params.expected_args();
        return Err(wrong_arg_count(name, &expected, args.len()));
    }

    let call_env = Env::new(Some(procedure.env.clone()));
    for (param, arg) in procedure.params.required.iter().zip(args.iter()) {
        call_env.define(param.clone(), arg.clone());
    }
    if let Some(rest) = &procedure.params.rest {
        call_env.define(
            rest.clone(),
            Value::List(args[procedure.params.required.len()..].to_vec()),
        );
    }

    eval_sequence(&procedure.body, &call_env, output)
}

fn wrong_arg_count(name: &str, expected: &str, got: usize) -> EvalError {
    EvalError::WrongArgCount {
        name: name.into(),
        expected: expected.into(),
        got,
    }
}

#[cfg(test)]
mod tests;
