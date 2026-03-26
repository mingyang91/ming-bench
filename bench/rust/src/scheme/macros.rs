use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::error::{EvalError, SourcePos};
use super::evaluator::{apply, quote_expr};
use super::model::{
    dotted_list_parts, expr_datum_eq, fresh_identifier, is_core_syntax, is_ellipsis, Env,
    EnvRef, ExpansionState, Expr, MacroExpansion, MacroRef, MacroTransformer, PatternBindings,
    SchemeString, SyntaxCaseClause, SyntaxRule, Value,
};

pub(super) fn parse_macro_transformer(expr: &Expr, env: &EnvRef) -> Result<MacroRef, EvalError> {
    let Expr::List(items, _) = expr else {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected syntax-rules or transformer lambda".into(),
        });
    };

    let Some(Expr::Symbol(keyword, _)) = items.first() else {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected syntax-rules or transformer lambda".into(),
        });
    };

    match keyword.as_str() {
        "syntax-rules" => parse_syntax_rules(expr, env),
        "lambda" => parse_syntax_case_transformer(expr, env),
        _ => Err(EvalError::Syntax {
            message: "define-syntax: expected syntax-rules or transformer lambda".into(),
        }),
    }
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

    let literals = parse_literal_identifiers(literal_exprs, "syntax-rules")?;

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

    Ok(Rc::new(MacroTransformer::SyntaxRules {
        literals,
        rules: parsed_rules,
        env: env.clone(),
    }))
}

fn parse_syntax_case_transformer(expr: &Expr, env: &EnvRef) -> Result<MacroRef, EvalError> {
    let Expr::List(items, _) = expr else {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected transformer lambda".into(),
        });
    };

    let [Expr::Symbol(keyword, _), params_expr, body_expr] = items.as_slice() else {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected transformer lambda".into(),
        });
    };

    if keyword != "lambda" {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected transformer lambda".into(),
        });
    }

    let Expr::List(params, _) = params_expr else {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected transformer parameter list".into(),
        });
    };

    let [Expr::Symbol(input_name, _)] = params.as_slice() else {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected exactly one transformer parameter".into(),
        });
    };

    let Expr::List(body_items, _) = body_expr else {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected syntax-case body".into(),
        });
    };

    let [Expr::Symbol(body_keyword, _), Expr::Symbol(target_name, _), Expr::List(literal_exprs, _), clauses @ ..] =
        body_items.as_slice()
    else {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected syntax-case body".into(),
        });
    };

    if body_keyword != "syntax-case" {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected syntax-case body".into(),
        });
    }

    if target_name != input_name {
        return Err(EvalError::Syntax {
            message: "define-syntax: syntax-case must match the transformer parameter".into(),
        });
    }

    if clauses.is_empty() {
        return Err(EvalError::Syntax {
            message: "syntax-case: expected at least one clause".into(),
        });
    }

    let literals = parse_literal_identifiers(literal_exprs, "syntax-case")?;
    let mut parsed_clauses = Vec::with_capacity(clauses.len());
    for clause in clauses {
        let Expr::List(parts, _) = clause else {
            return Err(EvalError::Syntax {
                message: "syntax-case: expected clause".into(),
            });
        };

        match parts.as_slice() {
            [pattern, result] => parsed_clauses.push(SyntaxCaseClause {
                pattern: pattern.clone(),
                fender: None,
                result: result.clone(),
            }),
            [pattern, fender, result] => parsed_clauses.push(SyntaxCaseClause {
                pattern: pattern.clone(),
                fender: Some(fender.clone()),
                result: result.clone(),
            }),
            _ => {
                return Err(EvalError::Syntax {
                    message: "syntax-case: expected (pattern result) or (pattern fender result)"
                        .into(),
                });
            }
        }
    }

    Ok(Rc::new(MacroTransformer::SyntaxCase {
        literals,
        clauses: parsed_clauses,
        env: env.clone(),
    }))
}

fn parse_literal_identifiers(
    literal_exprs: &[Expr],
    form_name: &str,
) -> Result<HashSet<String>, EvalError> {
    let mut literals = HashSet::new();
    for literal in literal_exprs {
        match literal {
            Expr::Symbol(name, _) if name != "..." => {
                literals.insert(name.clone());
            }
            _ => {
                return Err(EvalError::Syntax {
                    message: format!("{form_name}: expected literal identifier"),
                });
            }
        }
    }
    Ok(literals)
}

pub(super) fn env_with_expansion_aliases(env: &EnvRef, expansion: &MacroExpansion) -> EnvRef {
    if expansion.value_aliases.is_empty() && expansion.macro_aliases.is_empty() {
        return env.clone();
    }

    let expanded_env = Env::new_transparent(Some(env.clone()));
    for (name, cell) in &expansion.value_aliases {
        expanded_env.define_alias(name.clone(), cell.clone());
    }
    for (name, transformer) in &expansion.macro_aliases {
        expanded_env.define_macro(name.clone(), transformer.clone());
    }
    expanded_env
}

pub(super) fn expand_macro_call(
    items: &[Expr],
    transformer: &MacroRef,
) -> Result<MacroExpansion, EvalError> {
    let call_expr = Expr::List(items.to_vec(), items[0].pos());

    match transformer.as_ref() {
        MacroTransformer::SyntaxRules {
            literals,
            rules,
            env,
        } => expand_syntax_rules_call(&call_expr, literals, rules, env),
        MacroTransformer::SyntaxCase {
            literals,
            clauses,
            env,
        } => expand_syntax_case_call(&call_expr, literals, clauses, env),
    }
}

fn expand_syntax_rules_call(
    call_expr: &Expr,
    literals: &HashSet<String>,
    rules: &[SyntaxRule],
    env: &EnvRef,
) -> Result<MacroExpansion, EvalError> {
    for rule in rules {
        let mut bindings = PatternBindings::default();
        if match_call_pattern(
            &rule.pattern,
            call_expr,
            literals,
            &mut bindings,
            "syntax-rules",
        )? {
            return expand_syntax_template(&rule.template, bindings, env);
        }
    }

    Err(EvalError::Syntax {
        message: "syntax-rules: no matching clause".into(),
    })
}

fn expand_syntax_case_call(
    call_expr: &Expr,
    literals: &HashSet<String>,
    clauses: &[SyntaxCaseClause],
    env: &EnvRef,
) -> Result<MacroExpansion, EvalError> {
    for clause in clauses {
        let mut bindings = PatternBindings::default();
        if !match_call_pattern(
            &clause.pattern,
            call_expr,
            literals,
            &mut bindings,
            "syntax-case",
        )? {
            continue;
        }

        if let Some(fender) = &clause.fender {
            let fender_value = expect_transformer_datum(
                "syntax-case fender",
                eval_transformer_expr(fender, &bindings, env)?,
            )?;
            if !fender_value.is_truthy() {
                continue;
            }
        }

        return expect_transformer_syntax(
            "syntax-case",
            eval_transformer_expr(&clause.result, &bindings, env)?,
        );
    }

    Err(EvalError::Syntax {
        message: "syntax-case: no matching clause".into(),
    })
}

enum TransformerValue {
    Datum(Value),
    Syntax(MacroExpansion),
}

fn eval_transformer_expr(
    expr: &Expr,
    bindings: &PatternBindings,
    env: &EnvRef,
) -> Result<TransformerValue, EvalError> {
    match expr {
        Expr::Number(value, _) => Ok(TransformerValue::Datum(Value::Number(*value))),
        Expr::Boolean(value, _) => Ok(TransformerValue::Datum(Value::Boolean(*value))),
        Expr::String(value, _) => Ok(TransformerValue::Datum(Value::String(
            SchemeString::literal(value),
        ))),
        Expr::Char(value, _) => Ok(TransformerValue::Datum(Value::Char(*value))),
        Expr::Symbol(name, pos) => {
            if let Some(bound) = bindings.substitute(name, None)? {
                return Ok(TransformerValue::Syntax(simple_macro_expansion(bound)));
            }

            env.lookup(name)
                .map(TransformerValue::Datum)
                .ok_or_else(|| {
                    EvalError::UnboundVariable { name: name.clone() }.with_position(*pos)
                })
        }
        Expr::List(items, pos) => {
            let Some((head, tail)) = items.split_first() else {
                return Err(EvalError::Syntax {
                    message: "cannot evaluate empty list".into(),
                }
                .with_position(*pos));
            };

            if let Expr::Symbol(name, _) = head {
                match name.as_str() {
                    "quote" => {
                        let [datum] = tail else {
                            return Err(EvalError::Syntax {
                                message: "quote: expected 1 datum".into(),
                            }
                            .with_position(*pos));
                        };
                        return Ok(TransformerValue::Datum(quote_expr(datum)));
                    }
                    "syntax" => {
                        let [template] = tail else {
                            return Err(EvalError::Syntax {
                                message: "syntax: expected 1 template".into(),
                            }
                            .with_position(*pos));
                        };
                        return expand_syntax_template(template, bindings.clone(), env)
                            .map(TransformerValue::Syntax);
                    }
                    "with-syntax" => {
                        return eval_transformer_with_syntax(tail, bindings, env)
                            .map_err(|error| error.with_position(*pos));
                    }
                    "syntax->datum" => {
                        return eval_transformer_syntax_to_datum(tail, bindings, env)
                            .map_err(|error| error.with_position(*pos));
                    }
                    "datum->syntax" => {
                        return eval_transformer_datum_to_syntax(tail, bindings, env)
                            .map_err(|error| error.with_position(*pos));
                    }
                    _ => {}
                }
            }

            eval_transformer_application(items, bindings, env)
                .map_err(|error| error.with_position(*pos))
        }
    }
}

fn eval_transformer_sequence(
    exprs: &[Expr],
    bindings: &PatternBindings,
    env: &EnvRef,
) -> Result<TransformerValue, EvalError> {
    let mut result = TransformerValue::Datum(Value::Void);
    for expr in exprs {
        result = eval_transformer_expr(expr, bindings, env)?;
    }
    Ok(result)
}

fn eval_transformer_with_syntax(
    args: &[Expr],
    bindings: &PatternBindings,
    env: &EnvRef,
) -> Result<TransformerValue, EvalError> {
    let [Expr::List(binding_specs, _), body @ ..] = args else {
        return Err(EvalError::Syntax {
            message: "with-syntax: invalid syntax".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "with-syntax: expected body".into(),
        });
    }

    let mut local_bindings = bindings.clone();
    let mut value_aliases = Vec::new();
    let mut macro_aliases = Vec::new();

    for binding in binding_specs {
        let Expr::List(parts, _) = binding else {
            return Err(EvalError::Syntax {
                message: "with-syntax: expected binding".into(),
            });
        };

        let [Expr::Symbol(name, _), value_expr] = parts.as_slice() else {
            return Err(EvalError::Syntax {
                message: "with-syntax: expected (identifier expr) binding".into(),
            });
        };

        let expansion = expect_transformer_syntax(
            "with-syntax",
            eval_transformer_expr(value_expr, &local_bindings, env)?,
        )?;
        local_bindings.insert_single(name.clone(), expansion.expr);
        value_aliases.extend(expansion.value_aliases);
        macro_aliases.extend(expansion.macro_aliases);
    }

    match eval_transformer_sequence(body, &local_bindings, env)? {
        TransformerValue::Datum(value) => Ok(TransformerValue::Datum(value)),
        TransformerValue::Syntax(mut expansion) => {
            expansion.value_aliases.extend(value_aliases);
            expansion.macro_aliases.extend(macro_aliases);
            Ok(TransformerValue::Syntax(expansion))
        }
    }
}

fn eval_transformer_syntax_to_datum(
    args: &[Expr],
    bindings: &PatternBindings,
    env: &EnvRef,
) -> Result<TransformerValue, EvalError> {
    let [value_expr] = args else {
        return Err(EvalError::Syntax {
            message: "syntax->datum: expected 1 argument".into(),
        });
    };

    let expansion = expect_transformer_syntax(
        "syntax->datum",
        eval_transformer_expr(value_expr, bindings, env)?,
    )?;
    Ok(TransformerValue::Datum(quote_expr(&expansion.expr)))
}

fn eval_transformer_datum_to_syntax(
    args: &[Expr],
    bindings: &PatternBindings,
    env: &EnvRef,
) -> Result<TransformerValue, EvalError> {
    let [context_expr, datum_expr] = args else {
        return Err(EvalError::Syntax {
            message: "datum->syntax: expected 2 arguments".into(),
        });
    };

    let context = expect_transformer_syntax(
        "datum->syntax",
        eval_transformer_expr(context_expr, bindings, env)?,
    )?;
    let datum = expect_transformer_datum(
        "datum->syntax",
        eval_transformer_expr(datum_expr, bindings, env)?,
    )?;

    Ok(TransformerValue::Syntax(simple_macro_expansion(
        value_to_datum_expr("datum->syntax", &datum, context.expr.pos())?,
    )))
}

fn eval_transformer_application(
    items: &[Expr],
    bindings: &PatternBindings,
    env: &EnvRef,
) -> Result<TransformerValue, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "cannot evaluate empty list".into(),
        });
    };

    let callable = expect_transformer_datum(
        "transformer application",
        eval_transformer_expr(head, bindings, env)?,
    )?;
    let mut args = Vec::with_capacity(tail.len());
    for expr in tail {
        args.push(expect_transformer_datum(
            "transformer application",
            eval_transformer_expr(expr, bindings, env)?,
        )?);
    }

    let mut output = String::new();
    apply(callable, &args, &mut output).map(TransformerValue::Datum)
}

fn expect_transformer_datum(name: &str, value: TransformerValue) -> Result<Value, EvalError> {
    match value {
        TransformerValue::Datum(value) => Ok(value),
        TransformerValue::Syntax(_) => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "datum".into(),
            got: "syntax object".into(),
        }),
    }
}

fn expect_transformer_syntax(
    name: &str,
    value: TransformerValue,
) -> Result<MacroExpansion, EvalError> {
    match value {
        TransformerValue::Syntax(expansion) => Ok(expansion),
        TransformerValue::Datum(value) => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "syntax object".into(),
            got: value.type_name().into(),
        }),
    }
}

fn expand_syntax_template(
    template: &Expr,
    bindings: PatternBindings,
    definition_env: &EnvRef,
) -> Result<MacroExpansion, EvalError> {
    let mut state = ExpansionState::new(bindings, definition_env);
    let expr = expand_template_expr(template, &mut state, &HashMap::new(), None)?;
    Ok(MacroExpansion {
        expr,
        value_aliases: state.value_aliases,
        macro_aliases: state.macro_aliases,
    })
}

fn simple_macro_expansion(expr: Expr) -> MacroExpansion {
    MacroExpansion {
        expr,
        value_aliases: Vec::new(),
        macro_aliases: Vec::new(),
    }
}

fn value_to_datum_expr(name: &str, value: &Value, pos: SourcePos) -> Result<Expr, EvalError> {
    let mut seen_pairs = HashSet::new();
    value_to_datum_expr_with_state(name, value, pos, &mut seen_pairs)
}

fn value_to_datum_expr_with_state(
    name: &str,
    value: &Value,
    pos: SourcePos,
    seen_pairs: &mut HashSet<usize>,
) -> Result<Expr, EvalError> {
    match value {
        Value::Number(number) => Ok(Expr::Number(*number, pos)),
        Value::Boolean(boolean) => Ok(Expr::Boolean(*boolean, pos)),
        Value::String(string) => Ok(Expr::String(string.to_plain_string(), pos)),
        Value::Symbol(symbol) => Ok(Expr::Symbol(symbol.clone(), pos)),
        Value::Char(ch) => Ok(Expr::Char(*ch, pos)),
        Value::EmptyList => Ok(Expr::List(Vec::new(), pos)),
        Value::Pair(pair) => {
            let mut items = Vec::new();
            let mut current = Value::Pair(pair.clone());

            loop {
                match current {
                    Value::EmptyList => return Ok(Expr::List(items, pos)),
                    Value::Pair(pair) => {
                        if !seen_pairs.insert(pair.id()) {
                            return Err(EvalError::CircularList { name: name.into() });
                        }

                        items.push(value_to_datum_expr_with_state(
                            name,
                            &pair.car(),
                            pos,
                            seen_pairs,
                        )?);
                        current = pair.cdr();
                    }
                    other => {
                        items.push(Expr::Symbol(".".into(), pos));
                        items.push(value_to_datum_expr_with_state(
                            name, &other, pos, seen_pairs,
                        )?);
                        return Ok(Expr::List(items, pos));
                    }
                }
            }
        }
        other => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "datum".into(),
            got: other.type_name().into(),
        }),
    }
}

fn match_call_pattern(
    pattern: &Expr,
    call_expr: &Expr,
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
    form_name: &str,
) -> Result<bool, EvalError> {
    let Expr::List(pattern_items, _) = pattern else {
        return Err(EvalError::Syntax {
            message: format!("{form_name}: expected list pattern"),
        });
    };
    let Expr::List(call_items, _) = call_expr else {
        return Ok(false);
    };

    if pattern_items.is_empty() {
        return Err(EvalError::Syntax {
            message: format!("{form_name}: expected macro name in pattern"),
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
                if let Some((prefix_patterns, tail_pattern)) = dotted_list_parts(pattern_items) {
                    return match_dotted_list_pattern(
                        prefix_patterns,
                        tail_pattern,
                        input_items,
                        input.pos(),
                        literals,
                        bindings,
                        repeated,
                    );
                }
                match_list_pattern(pattern_items, input_items, literals, bindings, repeated)
            }
            _ => Ok(false),
        },
    }
}

fn match_dotted_list_pattern(
    prefix_patterns: &[Expr],
    tail_pattern: &Expr,
    input_items: &[Expr],
    input_pos: SourcePos,
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
    repeated: bool,
) -> Result<bool, EvalError> {
    if find_ellipsis_index(prefix_patterns)?.is_some() {
        return Err(EvalError::Syntax {
            message: "syntax-rules: dotted patterns with ellipsis are unsupported".into(),
        });
    }

    let input_prefix = if let Some((prefix, _)) = dotted_list_parts(input_items) {
        prefix
    } else {
        input_items
    };

    if input_prefix.len() < prefix_patterns.len() {
        return Ok(false);
    }

    for (pattern, input) in prefix_patterns.iter().zip(input_prefix.iter()) {
        if !match_pattern(pattern, input, literals, bindings, repeated)? {
            return Ok(false);
        }
    }

    let Some(tail_input) = expr_list_tail(input_items, prefix_patterns.len(), input_pos) else {
        return Ok(false);
    };

    match_pattern(tail_pattern, &tail_input, literals, bindings, repeated)
}

fn expr_list_tail(items: &[Expr], consumed: usize, pos: SourcePos) -> Option<Expr> {
    if let Some((prefix, tail)) = dotted_list_parts(items) {
        if consumed > prefix.len() {
            return None;
        }

        if consumed == prefix.len() {
            return Some(tail.clone());
        }

        let mut remaining = prefix[consumed..].to_vec();
        remaining.push(Expr::Symbol(".".into(), pos));
        remaining.push(tail.clone());
        return Some(Expr::List(remaining, pos));
    }

    if consumed > items.len() {
        return None;
    }

    Some(Expr::List(items[consumed..].to_vec(), pos))
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
                    "letrec" | "letrec*" => {
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
                    "case-lambda" => {
                        if let Some(expanded) =
                            expand_case_lambda_template(items, *pos, state, scope, repeat_index)?
                        {
                            return Ok(expanded);
                        }
                    }
                    _ => {}
                }
            }

            if let Some((prefix, tail)) = dotted_list_parts(items) {
                return expand_dotted_template(prefix, tail, *pos, state, scope, repeat_index);
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

fn expand_dotted_template(
    prefix: &[Expr],
    tail: &Expr,
    pos: SourcePos,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let mut expanded = expand_template_items(prefix, state, scope, repeat_index)?;
    let expanded_tail = expand_template_expr(tail, state, scope, repeat_index)?;

    match expanded_tail {
        Expr::List(mut tail_items, _) => {
            expanded.append(&mut tail_items);
            Ok(Expr::List(expanded, pos))
        }
        other => {
            expanded.push(Expr::Symbol(".".into(), pos));
            expanded.push(other);
            Ok(Expr::List(expanded, pos))
        }
    }
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

fn expand_case_lambda_template(
    items: &[Expr],
    pos: SourcePos,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Option<Expr>, EvalError> {
    let [head, clauses @ ..] = items else {
        return Ok(None);
    };

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head.clone());

    for clause in clauses {
        let Expr::List(parts, clause_pos) = clause else {
            return Ok(None);
        };
        let [params_expr, body @ ..] = parts.as_slice() else {
            return Ok(None);
        };

        let (expanded_params, introduced) =
            expand_parameter_list(params_expr, state, scope, repeat_index)?;
        let mut body_scope = scope.clone();
        body_scope.extend(introduced);

        let mut expanded_clause = Vec::with_capacity(parts.len());
        expanded_clause.push(expanded_params);
        for expr in body {
            expanded_clause.push(expand_template_expr(
                expr,
                state,
                &body_scope,
                repeat_index,
            )?);
        }
        expanded_items.push(Expr::List(expanded_clause, *clause_pos));
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
