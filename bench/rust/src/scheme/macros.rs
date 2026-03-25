use std::collections::{HashMap, HashSet};

use super::core::{list_to_vec, quote_expr, EnvRef, Environment, Expr, Position, Runtime, Value};
use super::error::EvalError;
use super::eval::apply_procedure;

const ELLIPSIS: &str = "...";
const PROTECTED_IDENTIFIERS: &[&str] = &[
    ".",
    "and",
    "begin",
    "cond",
    "define",
    "define-syntax",
    "else",
    "if",
    "lambda",
    "let",
    "or",
    "quote",
    "set!",
    "syntax",
    "syntax-case",
    "syntax-rules",
    "with-syntax",
];

#[derive(Clone)]
pub(crate) enum MacroTransformer {
    SyntaxRules(SyntaxRulesTransformer),
    Procedure(ProcedureTransformer),
}

#[derive(Clone)]
pub(crate) struct SyntaxRulesTransformer {
    name: String,
    literals: HashSet<String>,
    rules: Vec<SyntaxRule>,
    definition_env: EnvRef,
}

#[derive(Clone)]
pub(crate) struct ProcedureTransformer {
    param: String,
    body: Vec<Expr>,
    definition_env: EnvRef,
}

#[derive(Clone)]
struct SyntaxRule {
    pattern: Expr,
    template: Expr,
    pattern_vars: HashSet<String>,
}

#[derive(Clone)]
enum BindingMatch {
    Scalar(Expr),
    Repeated(Vec<Expr>),
}

type Bindings = HashMap<String, BindingMatch>;
type TransformerEnv = HashMap<String, TransformerBinding>;

#[derive(Clone)]
enum TransformerBinding {
    Syntax(BindingMatch),
}

#[derive(Clone)]
enum TransformerValue {
    Scheme(Value),
    Syntax(Expr),
}

struct ListMatchContext<'a> {
    patterns: &'a [Expr],
    pattern_index: usize,
    inputs: &'a [Expr],
    input_index: usize,
    literals: &'a HashSet<String>,
    ellipsis_depth: usize,
    bindings: &'a Bindings,
}

struct ExpansionState<'a> {
    bindings: &'a Bindings,
    pattern_vars: &'a HashSet<String>,
    definition_env: &'a EnvRef,
    expansion_env: EnvRef,
    captured_aliases: &'a mut HashMap<String, String>,
    protected_identifiers: HashSet<String>,
    runtime: &'a mut Runtime,
}

struct ProcedureExpansionState<'a> {
    definition_env: &'a EnvRef,
    expansion_env: EnvRef,
    captured_aliases: HashMap<String, String>,
    runtime: &'a mut Runtime,
}

pub(crate) fn parse_macro_definition(
    name: &str,
    spec: &Expr,
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<MacroTransformer, EvalError> {
    let Expr::List(items, _) = spec else {
        return Err(positioned_syntax_error(
            spec,
            "define-syntax requires a syntax-rules or lambda transformer",
        ));
    };

    let Some((head, rest)) = items.split_first() else {
        return Err(positioned_syntax_error(
            spec,
            "define-syntax requires a syntax-rules or lambda transformer",
        ));
    };

    match head {
        Expr::Symbol(symbol, _) if symbol == "syntax-rules" => {
            parse_syntax_rules_definition(name, rest, spec, env).map(MacroTransformer::SyntaxRules)
        }
        Expr::Symbol(symbol, _) if symbol == "lambda" => {
            parse_procedure_macro_definition(name, rest, spec, env).map(MacroTransformer::Procedure)
        }
        Expr::Symbol(symbol, _) => {
            let Some(transformer) = runtime.lookup_macro(symbol) else {
                return Err(positioned_syntax_error(
                    head,
                    "define-syntax requires a syntax-rules or lambda transformer",
                ));
            };

            let (expanded, _) = expand_macro_call(&transformer, items, env, runtime)?;
            parse_macro_definition(name, &expanded, env, runtime)
        }
        _ => Err(positioned_syntax_error(
            head,
            "define-syntax requires a syntax-rules or lambda transformer",
        )),
    }
}

fn parse_syntax_rules_definition(
    name: &str,
    rest: &[Expr],
    spec: &Expr,
    env: &EnvRef,
) -> Result<SyntaxRulesTransformer, EvalError> {
    let Some((literals_expr, rules)) = rest.split_first() else {
        return Err(positioned_syntax_error(
            spec,
            "syntax-rules requires a literals list and at least one rule",
        ));
    };

    if rules.is_empty() {
        return Err(positioned_syntax_error(
            spec,
            "syntax-rules requires at least one rule",
        ));
    }

    let literals = parse_literal_identifiers(literals_expr)?;
    let parsed_rules = rules
        .iter()
        .map(|rule| parse_syntax_rule(rule, name, &literals))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(SyntaxRulesTransformer {
        name: name.to_string(),
        literals,
        rules: parsed_rules,
        definition_env: env.clone(),
    })
}

fn parse_procedure_macro_definition(
    _name: &str,
    rest: &[Expr],
    spec: &Expr,
    env: &EnvRef,
) -> Result<ProcedureTransformer, EvalError> {
    let Some((params_expr, body)) = rest.split_first() else {
        return Err(positioned_syntax_error(
            spec,
            "macro transformer lambda requires a parameter list and body",
        ));
    };

    if body.is_empty() {
        return Err(positioned_syntax_error(
            spec,
            "macro transformer lambda requires a body",
        ));
    }

    let Expr::List(params, _) = params_expr else {
        return Err(positioned_syntax_error(
            params_expr,
            "macro transformer lambda must have exactly one parameter",
        ));
    };

    let [param_expr] = params.as_slice() else {
        return Err(positioned_syntax_error(
            params_expr,
            "macro transformer lambda must have exactly one parameter",
        ));
    };

    let Expr::Symbol(param, _) = param_expr else {
        return Err(positioned_syntax_error(
            param_expr,
            "macro transformer parameter must be an identifier",
        ));
    };

    Ok(ProcedureTransformer {
        param: param.clone(),
        body: body.to_vec(),
        definition_env: env.clone(),
    })
}

pub(crate) fn expand_macro_call(
    transformer: &MacroTransformer,
    call: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<(Expr, EnvRef), EvalError> {
    match transformer {
        MacroTransformer::SyntaxRules(transformer) => {
            expand_syntax_rules_call(transformer, call, env, runtime)
        }
        MacroTransformer::Procedure(transformer) => {
            expand_procedure_macro_call(transformer, call, env, runtime)
        }
    }
}

fn expand_syntax_rules_call(
    transformer: &SyntaxRulesTransformer,
    call: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<(Expr, EnvRef), EvalError> {
    let Some(first) = call.first() else {
        return Err(syntax_error("macro call cannot be empty"));
    };
    let call_expr = Expr::List(call.to_vec(), first.pos());
    let match_literals = transformer.match_literals();

    for rule in &transformer.rules {
        let Some(bindings) = match_pattern(
            &rule.pattern,
            &call_expr,
            &match_literals,
            0,
            &Bindings::new(),
        )?
        else {
            continue;
        };

        let protected_identifiers = build_protected_identifiers(runtime);
        let expansion_env = Environment::new_transparent(Some(env.clone()));
        let mut captured_aliases = HashMap::new();
        let mut state = ExpansionState {
            bindings: &bindings,
            pattern_vars: &rule.pattern_vars,
            definition_env: &transformer.definition_env,
            expansion_env: expansion_env.clone(),
            captured_aliases: &mut captured_aliases,
            protected_identifiers,
            runtime,
        };
        let expanded = expand_template(&rule.template, &mut state, None, &HashMap::new())?;
        return Ok((expanded, expansion_env));
    }

    Err(first.pos().attach(syntax_error(format!(
        "no syntax-rules clause matched macro '{}'",
        transformer.name
    ))))
}

fn expand_procedure_macro_call(
    transformer: &ProcedureTransformer,
    call: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<(Expr, EnvRef), EvalError> {
    let Some(first) = call.first() else {
        return Err(syntax_error("macro call cannot be empty"));
    };
    let call_expr = Expr::List(call.to_vec(), first.pos());
    let expansion_env = Environment::new_transparent(Some(env.clone()));
    let mut transformer_env = TransformerEnv::new();
    transformer_env.insert(
        transformer.param.clone(),
        TransformerBinding::Syntax(BindingMatch::Scalar(call_expr)),
    );

    let mut state = ProcedureExpansionState {
        definition_env: &transformer.definition_env,
        expansion_env: expansion_env.clone(),
        captured_aliases: HashMap::new(),
        runtime,
    };
    let result = eval_transformer_sequence(&transformer.body, &transformer_env, &mut state)?;
    match result {
        TransformerValue::Syntax(expanded) => Ok((expanded, expansion_env)),
        TransformerValue::Scheme(value) => Err(first.pos().attach(EvalError::TypeMismatch {
            expected: "syntax object".into(),
            found: value.render_for_error(),
        })),
    }
}

impl SyntaxRulesTransformer {
    fn match_literals(&self) -> HashSet<String> {
        let mut literals = self.literals.clone();
        literals.insert(self.name.clone());
        literals
    }
}

fn parse_literal_identifiers(expr: &Expr) -> Result<HashSet<String>, EvalError> {
    let Expr::List(items, _) = expr else {
        return Err(positioned_syntax_error(
            expr,
            "syntax-rules literals must be a list",
        ));
    };

    let mut literals = HashSet::new();
    for item in items {
        match item {
            Expr::Symbol(name, _) if name != ELLIPSIS => {
                literals.insert(name.clone());
            }
            Expr::Symbol(_, _) => {
                return Err(positioned_syntax_error(
                    item,
                    "ellipsis cannot be used as a literal identifier",
                ));
            }
            _ => {
                return Err(positioned_syntax_error(
                    item,
                    "syntax-rules literals must be identifiers",
                ));
            }
        }
    }

    Ok(literals)
}

fn parse_syntax_rule(
    expr: &Expr,
    macro_name: &str,
    literals: &HashSet<String>,
) -> Result<SyntaxRule, EvalError> {
    let Expr::List(items, _) = expr else {
        return Err(positioned_syntax_error(
            expr,
            "syntax-rules clauses must be (pattern template) pairs",
        ));
    };

    let [pattern, template] = items.as_slice() else {
        return Err(positioned_syntax_error(
            expr,
            "syntax-rules clauses must be (pattern template) pairs",
        ));
    };

    let Expr::List(pattern_items, _) = pattern else {
        return Err(positioned_syntax_error(
            pattern,
            "syntax-rules patterns must be lists",
        ));
    };

    let Some(head) = pattern_items.first() else {
        return Err(positioned_syntax_error(
            pattern,
            "syntax-rules patterns cannot be empty",
        ));
    };

    let normalized_pattern = match head {
        // The pattern head is a placeholder for the macro keyword. Accept `_`,
        // the macro name itself, or any other identifier, but normalize it so
        // matching always checks the actual macro keyword.
        Expr::Symbol(_, pos) => {
            let mut normalized_items = pattern_items.clone();
            normalized_items[0] = Expr::Symbol(macro_name.to_string(), *pos);
            Expr::List(normalized_items, pattern.pos())
        }
        _ => {
            return Err(positioned_syntax_error(
                head,
                "syntax-rules pattern must start with an identifier",
            ));
        }
    };

    let mut pattern_vars = HashSet::new();
    let mut match_literals = literals.clone();
    match_literals.insert(macro_name.to_string());
    collect_pattern_variables(&normalized_pattern, &match_literals, &mut pattern_vars);

    Ok(SyntaxRule {
        pattern: normalized_pattern,
        template: template.clone(),
        pattern_vars,
    })
}

fn eval_transformer_sequence(
    body: &[Expr],
    env: &TransformerEnv,
    state: &mut ProcedureExpansionState<'_>,
) -> Result<TransformerValue, EvalError> {
    let Some((last, prefix)) = body.split_last() else {
        return Err(syntax_error("macro transformer body cannot be empty"));
    };

    for expr in prefix {
        let _ = eval_transformer_expr(expr, env, state)?;
    }

    eval_transformer_expr(last, env, state)
}

fn eval_transformer_expr(
    expr: &Expr,
    env: &TransformerEnv,
    state: &mut ProcedureExpansionState<'_>,
) -> Result<TransformerValue, EvalError> {
    match expr {
        Expr::Bool(value, _) => Ok(TransformerValue::Scheme(Value::Bool(*value))),
        Expr::Number(value, _) => Ok(TransformerValue::Scheme(Value::Number(*value))),
        Expr::String(value, _) => Ok(TransformerValue::Scheme(Value::String(std::rc::Rc::new(
            super::core::SchemeString::new(value.clone(), false),
        )))),
        Expr::Char(value, _) => Ok(TransformerValue::Scheme(Value::Char(*value))),
        Expr::Symbol(name, pos) => eval_transformer_symbol(name, *pos, env, state),
        Expr::Vector(_, _) => Ok(TransformerValue::Scheme(quote_expr(expr))),
        Expr::List(items, pos) => eval_transformer_list(items, *pos, env, state),
    }
}

fn eval_transformer_symbol(
    name: &str,
    pos: Position,
    env: &TransformerEnv,
    state: &ProcedureExpansionState<'_>,
) -> Result<TransformerValue, EvalError> {
    match env.get(name) {
        Some(TransformerBinding::Syntax(BindingMatch::Scalar(expr))) => {
            Ok(TransformerValue::Syntax(expr.clone()))
        }
        Some(TransformerBinding::Syntax(BindingMatch::Repeated(_))) => {
            Err(pos.attach(syntax_error(format!(
                "pattern variable '{name}' requires ellipsis in a syntax template"
            ))))
        }
        None => Environment::lookup(state.definition_env, name)
            .map(TransformerValue::Scheme)
            .ok_or_else(|| pos.attach(EvalError::UnboundSymbol { name: name.into() })),
    }
}

fn eval_transformer_list(
    items: &[Expr],
    pos: Position,
    env: &TransformerEnv,
    state: &mut ProcedureExpansionState<'_>,
) -> Result<TransformerValue, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(pos.attach(syntax_error("empty application")));
    };

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "quote" => return eval_transformer_quote(args),
            "if" => return eval_transformer_if(args, env, state),
            "begin" => return eval_transformer_sequence(args, env, state),
            "syntax" => return eval_transformer_syntax(args, env, state),
            "with-syntax" => return eval_transformer_with_syntax(args, env, state),
            "syntax-case" => return eval_transformer_syntax_case(args, env, state),
            "syntax->datum" => return eval_transformer_syntax_to_datum(args, env, state),
            "datum->syntax" => return eval_transformer_datum_to_syntax(args, env, state),
            _ => {}
        }
    }

    let operator = expect_scheme_value(eval_transformer_expr(head, env, state)?, "procedure")?;
    let args = args
        .iter()
        .map(|arg| eval_transformer_expr(arg, env, state))
        .map(|result| result.and_then(|value| expect_scheme_value(value, "value")))
        .collect::<Result<Vec<_>, _>>()?;
    apply_procedure(operator, &args, state.runtime).map(TransformerValue::Scheme)
}

fn eval_transformer_quote(args: &[Expr]) -> Result<TransformerValue, EvalError> {
    let [expr] = args else {
        return Err(wrong_arg_count("quote", "exactly 1", args.len()));
    };
    Ok(TransformerValue::Scheme(quote_expr(expr)))
}

fn eval_transformer_if(
    args: &[Expr],
    env: &TransformerEnv,
    state: &mut ProcedureExpansionState<'_>,
) -> Result<TransformerValue, EvalError> {
    let [test, consequent] = args else {
        if let [test, consequent, alternate] = args {
            let test_value = eval_transformer_expr(test, env, state)?;
            if transformer_value_is_truthy(&test_value) {
                return eval_transformer_expr(consequent, env, state);
            }
            return eval_transformer_expr(alternate, env, state);
        }
        return Err(wrong_arg_count("if", "exactly 2 or 3", args.len()));
    };

    let test_value = eval_transformer_expr(test, env, state)?;
    if transformer_value_is_truthy(&test_value) {
        eval_transformer_expr(consequent, env, state)
    } else {
        Ok(TransformerValue::Scheme(Value::Void))
    }
}

fn eval_transformer_syntax(
    args: &[Expr],
    env: &TransformerEnv,
    state: &mut ProcedureExpansionState<'_>,
) -> Result<TransformerValue, EvalError> {
    let [template] = args else {
        return Err(wrong_arg_count("syntax", "exactly 1", args.len()));
    };

    let (bindings, pattern_vars) = collect_transformer_syntax_bindings(env);
    let protected_identifiers = build_protected_identifiers(state.runtime);
    let mut expansion_state = ExpansionState {
        bindings: &bindings,
        pattern_vars: &pattern_vars,
        definition_env: state.definition_env,
        expansion_env: state.expansion_env.clone(),
        captured_aliases: &mut state.captured_aliases,
        protected_identifiers,
        runtime: state.runtime,
    };
    expand_template(template, &mut expansion_state, None, &HashMap::new())
        .map(TransformerValue::Syntax)
}

fn eval_transformer_with_syntax(
    args: &[Expr],
    env: &TransformerEnv,
    state: &mut ProcedureExpansionState<'_>,
) -> Result<TransformerValue, EvalError> {
    let Some((bindings_expr, body)) = args.split_first() else {
        return Err(wrong_arg_count("with-syntax", "at least 2", 0));
    };
    if body.is_empty() {
        return Err(wrong_arg_count("with-syntax", "at least 2", 1));
    }

    let Expr::List(bindings, _) = bindings_expr else {
        return Err(positioned_syntax_error(
            bindings_expr,
            "with-syntax bindings must be a list",
        ));
    };

    let mut additions = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let Expr::List(parts, _) = binding else {
            return Err(positioned_syntax_error(
                binding,
                "with-syntax bindings must be (name expr) pairs",
            ));
        };
        let [name_expr, value_expr] = parts.as_slice() else {
            return Err(positioned_syntax_error(
                binding,
                "with-syntax bindings must be (name expr) pairs",
            ));
        };
        let Expr::Symbol(name, _) = name_expr else {
            return Err(positioned_syntax_error(
                name_expr,
                "with-syntax binding names must be identifiers",
            ));
        };
        let value = eval_transformer_expr(value_expr, env, state)?;
        let syntax = expect_syntax_value(value)?;
        additions.push((name.clone(), syntax));
    }

    let mut local_env = env.clone();
    for (name, syntax) in additions {
        local_env.insert(
            name,
            TransformerBinding::Syntax(BindingMatch::Scalar(syntax)),
        );
    }

    eval_transformer_sequence(body, &local_env, state)
}

fn eval_transformer_syntax_case(
    args: &[Expr],
    env: &TransformerEnv,
    state: &mut ProcedureExpansionState<'_>,
) -> Result<TransformerValue, EvalError> {
    let Some((target_expr, rest)) = args.split_first() else {
        return Err(wrong_arg_count("syntax-case", "at least 3", 0));
    };
    let Some((literals_expr, clauses)) = rest.split_first() else {
        return Err(wrong_arg_count("syntax-case", "at least 3", 1));
    };
    if clauses.is_empty() {
        return Err(wrong_arg_count("syntax-case", "at least 3", 2));
    }

    let target = expect_syntax_value(eval_transformer_expr(target_expr, env, state)?)?;
    let literals = parse_literal_identifiers(literals_expr)?;

    for clause in clauses {
        let Expr::List(parts, _) = clause else {
            return Err(positioned_syntax_error(
                clause,
                "syntax-case clauses must be (pattern expr) or (pattern fender expr)",
            ));
        };

        let (pattern, fender, template) = match parts.as_slice() {
            [pattern, template] => (pattern, None, template),
            [pattern, fender, template] => (pattern, Some(fender), template),
            _ => {
                return Err(positioned_syntax_error(
                    clause,
                    "syntax-case clauses must be (pattern expr) or (pattern fender expr)",
                ))
            }
        };

        let Some(bindings) = match_pattern(pattern, &target, &literals, 0, &Bindings::new())?
        else {
            continue;
        };

        let mut local_env = env.clone();
        for (name, binding) in &bindings {
            local_env.insert(name.clone(), TransformerBinding::Syntax(binding.clone()));
        }

        if let Some(fender_expr) = fender {
            let fender_value = eval_transformer_expr(fender_expr, &local_env, state)?;
            if !transformer_value_is_truthy(&fender_value) {
                continue;
            }
        }

        return eval_transformer_expr(template, &local_env, state);
    }

    Err(target_expr
        .pos()
        .attach(syntax_error("no syntax-case clause matched")))
}

fn eval_transformer_syntax_to_datum(
    args: &[Expr],
    env: &TransformerEnv,
    state: &mut ProcedureExpansionState<'_>,
) -> Result<TransformerValue, EvalError> {
    let [expr] = args else {
        return Err(wrong_arg_count("syntax->datum", "exactly 1", args.len()));
    };
    let syntax = expect_syntax_value(eval_transformer_expr(expr, env, state)?)?;
    Ok(TransformerValue::Scheme(quote_expr(&syntax)))
}

fn eval_transformer_datum_to_syntax(
    args: &[Expr],
    env: &TransformerEnv,
    state: &mut ProcedureExpansionState<'_>,
) -> Result<TransformerValue, EvalError> {
    let [context_expr, datum_expr] = args else {
        return Err(wrong_arg_count("datum->syntax", "exactly 2", args.len()));
    };
    let context = expect_syntax_value(eval_transformer_expr(context_expr, env, state)?)?;
    let datum = expect_scheme_value(eval_transformer_expr(datum_expr, env, state)?, "datum")?;
    datum_value_to_expr(&datum, context.pos()).map(TransformerValue::Syntax)
}

fn collect_transformer_syntax_bindings(env: &TransformerEnv) -> (Bindings, HashSet<String>) {
    let mut bindings = Bindings::new();
    let mut pattern_vars = HashSet::new();

    for (name, binding) in env {
        let TransformerBinding::Syntax(binding) = binding;
        bindings.insert(name.clone(), binding.clone());
        if name != "_" {
            pattern_vars.insert(name.clone());
        }
    }

    (bindings, pattern_vars)
}

fn expect_scheme_value(value: TransformerValue, expected: &str) -> Result<Value, EvalError> {
    match value {
        TransformerValue::Scheme(value) => Ok(value),
        TransformerValue::Syntax(_) => Err(EvalError::TypeMismatch {
            expected: expected.into(),
            found: "syntax object".into(),
        }),
    }
}

fn expect_syntax_value(value: TransformerValue) -> Result<Expr, EvalError> {
    match value {
        TransformerValue::Syntax(expr) => Ok(expr),
        TransformerValue::Scheme(value) => Err(EvalError::TypeMismatch {
            expected: "syntax object".into(),
            found: value.render_for_error(),
        }),
    }
}

fn transformer_value_is_truthy(value: &TransformerValue) -> bool {
    match value {
        TransformerValue::Scheme(value) => value.is_truthy(),
        TransformerValue::Syntax(_) => true,
    }
}

fn datum_value_to_expr(value: &Value, pos: Position) -> Result<Expr, EvalError> {
    match value {
        Value::Bool(value) => Ok(Expr::Bool(*value, pos)),
        Value::Number(value) => Ok(Expr::Number(*value, pos)),
        Value::String(value) => Ok(Expr::String(value.borrow().clone(), pos)),
        Value::Symbol(value) => Ok(Expr::Symbol(value.clone(), pos)),
        Value::Char(value) => Ok(Expr::Char(*value, pos)),
        Value::List(_) | Value::Pair(_) => list_to_vec(value)
            .ok_or_else(|| EvalError::TypeMismatch {
                expected: "datum".into(),
                found: value.render_for_error(),
            })
            .and_then(|items| {
                items
                    .into_iter()
                    .map(|item| datum_value_to_expr(&item, pos))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|items| Expr::List(items, pos))
            }),
        Value::Vector(_)
        | Value::Record(_)
        | Value::Procedure(_)
        | Value::Uninitialized
        | Value::Void => Err(EvalError::TypeMismatch {
            expected: "datum".into(),
            found: value.render_for_error(),
        }),
    }
}

fn collect_pattern_variables(
    expr: &Expr,
    literals: &HashSet<String>,
    pattern_vars: &mut HashSet<String>,
) {
    match expr {
        Expr::Symbol(name, _)
            if name != ELLIPSIS && name != "_" && name != "." && !literals.contains(name) =>
        {
            pattern_vars.insert(name.clone());
        }
        Expr::Vector(items, _) | Expr::List(items, _) => {
            for item in items {
                collect_pattern_variables(item, literals, pattern_vars);
            }
        }
        _ => {}
    }
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    match pattern {
        Expr::Bool(value, _) => Ok(match input {
            Expr::Bool(other, _) if value == other => Some(bindings.clone()),
            _ => None,
        }),
        Expr::Number(value, _) => Ok(match input {
            Expr::Number(other, _) if value == other => Some(bindings.clone()),
            _ => None,
        }),
        Expr::String(value, _) => Ok(match input {
            Expr::String(other, _) if value == other => Some(bindings.clone()),
            _ => None,
        }),
        Expr::Char(value, _) => Ok(match input {
            Expr::Char(other, _) if value == other => Some(bindings.clone()),
            _ => None,
        }),
        Expr::Symbol(name, _) if name == "." => Ok(match input {
            Expr::Symbol(other, _) if name == other => Some(bindings.clone()),
            _ => None,
        }),
        Expr::Symbol(name, _) if name == ELLIPSIS => {
            Err(syntax_error("misplaced ellipsis in macro pattern"))
        }
        Expr::Symbol(name, _) if name == "_" => Ok(Some(bindings.clone())),
        Expr::Symbol(name, _) if literals.contains(name) => Ok(match input {
            Expr::Symbol(other, _) if name == other => Some(bindings.clone()),
            _ => None,
        }),
        Expr::Symbol(name, _) => bind_pattern_variable(name, input, ellipsis_depth, bindings),
        Expr::Vector(patterns, _) => match input {
            Expr::Vector(inputs, _) => {
                match_list(patterns, inputs, literals, ellipsis_depth, bindings)
            }
            _ => Ok(None),
        },
        Expr::List(patterns, _) => match input {
            Expr::List(inputs, pos) => {
                match_list_syntax(patterns, inputs, *pos, literals, ellipsis_depth, bindings)
            }
            _ => Ok(None),
        },
    }
}

struct InputListSyntax<'a> {
    head: &'a [Expr],
    tail: Option<&'a Expr>,
    pos: Position,
}

enum ParsedListSyntax<'a> {
    Proper(&'a [Expr]),
    Improper { head: &'a [Expr], tail: &'a Expr },
}

struct ImproperListMatchContext<'a> {
    patterns: &'a [Expr],
    tail_pattern: &'a Expr,
    inputs: InputListSyntax<'a>,
    literals: &'a HashSet<String>,
    ellipsis_depth: usize,
}

fn match_list_syntax(
    patterns: &[Expr],
    inputs: &[Expr],
    pos: Position,
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    match split_list_syntax(patterns) {
        ParsedListSyntax::Proper(patterns) => match split_list_syntax(inputs) {
            ParsedListSyntax::Proper(inputs) => {
                match_list(patterns, inputs, literals, ellipsis_depth, bindings)
            }
            ParsedListSyntax::Improper { .. } => Ok(None),
        },
        ParsedListSyntax::Improper {
            head: pattern_head,
            tail: pattern_tail,
        } => match_improper_list(
            pattern_head,
            pattern_tail,
            parse_input_list_syntax(inputs, pos),
            literals,
            ellipsis_depth,
            bindings,
        ),
    }
}

fn split_list_syntax(items: &[Expr]) -> ParsedListSyntax<'_> {
    let dot_index = items.iter().position(is_dot_symbol);
    match dot_index {
        Some(index) if index > 0 && index + 2 == items.len() => ParsedListSyntax::Improper {
            head: &items[..index],
            tail: &items[index + 1],
        },
        Some(_) | None => ParsedListSyntax::Proper(items),
    }
}

fn parse_input_list_syntax(items: &[Expr], pos: Position) -> InputListSyntax<'_> {
    match split_list_syntax(items) {
        ParsedListSyntax::Proper(head) => InputListSyntax {
            head,
            tail: None,
            pos,
        },
        ParsedListSyntax::Improper { head, tail } => InputListSyntax {
            head,
            tail: Some(tail),
            pos,
        },
    }
}

fn match_improper_list(
    patterns: &[Expr],
    tail_pattern: &Expr,
    inputs: InputListSyntax<'_>,
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    let context = ImproperListMatchContext {
        patterns,
        tail_pattern,
        inputs,
        literals,
        ellipsis_depth,
    };
    match_improper_list_from(&context, 0, 0, bindings)
}

fn match_improper_list_from(
    context: &ImproperListMatchContext<'_>,
    pattern_index: usize,
    input_index: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    if pattern_index == context.patterns.len() {
        let remainder = rebuild_list_remainder(&context.inputs, input_index);
        return match_pattern(
            context.tail_pattern,
            &remainder,
            context.literals,
            context.ellipsis_depth,
            bindings,
        );
    }

    if followed_by_ellipsis(context.patterns, pattern_index) {
        return match_repeated_improper_pattern(context, pattern_index, input_index, bindings);
    }

    let Some(input) = context.inputs.head.get(input_index) else {
        return Ok(None);
    };
    let Some(next) = match_pattern(
        &context.patterns[pattern_index],
        input,
        context.literals,
        context.ellipsis_depth,
        bindings,
    )?
    else {
        return Ok(None);
    };

    match_improper_list_from(context, pattern_index + 1, input_index + 1, &next)
}

fn match_repeated_improper_pattern(
    context: &ImproperListMatchContext<'_>,
    pattern_index: usize,
    input_index: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    let min_remaining = minimum_inputs_required(&context.patterns[pattern_index + 2..]);
    if context.inputs.head.len() < input_index + min_remaining {
        return Ok(None);
    }

    let repeated = &context.patterns[pattern_index];
    let max_repeat = context.inputs.head.len() - input_index - min_remaining;
    for repeat in (0..=max_repeat).rev() {
        let mut branch = bindings.clone();
        initialize_repeated_list_branch(
            repeated,
            repeat,
            context.literals,
            context.ellipsis_depth,
            &mut branch,
        )?;
        if !match_repeated_inputs(
            repeated,
            &context.inputs.head[input_index..input_index + repeat],
            context.literals,
            context.ellipsis_depth,
            &mut branch,
        )? {
            continue;
        }

        if let Some(done) =
            match_improper_list_from(context, pattern_index + 2, input_index + repeat, &branch)?
        {
            return Ok(Some(done));
        }
    }

    Ok(None)
}

fn rebuild_list_remainder(inputs: &InputListSyntax<'_>, index: usize) -> Expr {
    if index == inputs.head.len() {
        return match inputs.tail {
            Some(tail) => tail.clone(),
            None => Expr::List(Vec::new(), inputs.pos),
        };
    }

    build_list_expr(&inputs.head[index..], inputs.tail, inputs.pos)
}

fn build_list_expr(head: &[Expr], tail: Option<&Expr>, pos: Position) -> Expr {
    let mut items = head.to_vec();
    if let Some(tail) = tail {
        items.push(Expr::Symbol(".".into(), pos));
        items.push(tail.clone());
    }
    Expr::List(items, pos)
}

fn is_dot_symbol(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(name, _) if name == ".")
}

fn match_list(
    patterns: &[Expr],
    inputs: &[Expr],
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    match_list_from(patterns, 0, inputs, 0, literals, ellipsis_depth, bindings)
}

fn followed_by_ellipsis(items: &[Expr], index: usize) -> bool {
    items.get(index + 1).is_some_and(is_ellipsis)
}

fn match_list_from(
    patterns: &[Expr],
    pattern_index: usize,
    inputs: &[Expr],
    input_index: usize,
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    if pattern_index == patterns.len() {
        return Ok((input_index == inputs.len()).then(|| bindings.clone()));
    }

    if followed_by_ellipsis(patterns, pattern_index) {
        return match_repeated_list_pattern(
            patterns,
            pattern_index,
            inputs,
            input_index,
            literals,
            ellipsis_depth,
            bindings,
        );
    }

    match_single_list_pattern(
        patterns,
        pattern_index,
        inputs,
        input_index,
        literals,
        ellipsis_depth,
        bindings,
    )
}

fn match_repeated_list_pattern(
    patterns: &[Expr],
    pattern_index: usize,
    inputs: &[Expr],
    input_index: usize,
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    let context = ListMatchContext {
        patterns,
        pattern_index,
        inputs,
        input_index,
        literals,
        ellipsis_depth,
        bindings,
    };
    let min_remaining = minimum_inputs_required(&patterns[pattern_index + 2..]);
    if inputs.len() < input_index + min_remaining {
        return Ok(None);
    }

    let max_repeat = inputs.len() - input_index - min_remaining;
    for repeat in (0..=max_repeat).rev() {
        if let Some(done) = match_repeated_list_branch(&context, repeat)? {
            return Ok(Some(done));
        }
    }

    Ok(None)
}

fn match_repeated_list_branch(
    context: &ListMatchContext<'_>,
    repeat: usize,
) -> Result<Option<Bindings>, EvalError> {
    let repeated = &context.patterns[context.pattern_index];
    let mut branch = context.bindings.clone();
    initialize_repeated_list_branch(
        repeated,
        repeat,
        context.literals,
        context.ellipsis_depth,
        &mut branch,
    )?;
    if !match_repeated_inputs(
        repeated,
        &context.inputs[context.input_index..context.input_index + repeat],
        context.literals,
        context.ellipsis_depth,
        &mut branch,
    )? {
        return Ok(None);
    }

    match_list_from(
        context.patterns,
        context.pattern_index + 2,
        context.inputs,
        context.input_index + repeat,
        context.literals,
        context.ellipsis_depth,
        &branch,
    )
}

fn initialize_repeated_list_branch(
    repeated: &Expr,
    repeat: usize,
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    branch: &mut Bindings,
) -> Result<(), EvalError> {
    if repeat != 0 {
        return Ok(());
    }

    ensure_empty_repeated_bindings(repeated, literals, ellipsis_depth + 1, branch)
}

fn match_repeated_inputs(
    repeated: &Expr,
    inputs: &[Expr],
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    branch: &mut Bindings,
) -> Result<bool, EvalError> {
    for input in inputs {
        let Some(next) = match_pattern(repeated, input, literals, ellipsis_depth + 1, branch)?
        else {
            return Ok(false);
        };
        *branch = next;
    }

    Ok(true)
}

fn match_single_list_pattern(
    patterns: &[Expr],
    pattern_index: usize,
    inputs: &[Expr],
    input_index: usize,
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    let Some(input) = inputs.get(input_index) else {
        return Ok(None);
    };
    let Some(next) = match_pattern(
        &patterns[pattern_index],
        input,
        literals,
        ellipsis_depth,
        bindings,
    )?
    else {
        return Ok(None);
    };

    match_list_from(
        patterns,
        pattern_index + 1,
        inputs,
        input_index + 1,
        literals,
        ellipsis_depth,
        &next,
    )
}

fn bind_pattern_variable(
    name: &str,
    input: &Expr,
    ellipsis_depth: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    if ellipsis_depth > 1 {
        return Err(syntax_error("nested ellipsis is not supported"));
    }

    let mut next = bindings.clone();
    match ellipsis_depth {
        0 => match next.get(name) {
            None => {
                next.insert(name.to_string(), BindingMatch::Scalar(input.clone()));
                Ok(Some(next))
            }
            Some(BindingMatch::Scalar(existing)) if existing == input => Ok(Some(next)),
            Some(BindingMatch::Scalar(_)) | Some(BindingMatch::Repeated(_)) => Ok(None),
        },
        1 => {
            match next.entry(name.to_string()) {
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(BindingMatch::Repeated(vec![input.clone()]));
                }
                std::collections::hash_map::Entry::Occupied(mut entry) => match entry.get_mut() {
                    BindingMatch::Repeated(values) => values.push(input.clone()),
                    BindingMatch::Scalar(_) => return Ok(None),
                },
            }
            Ok(Some(next))
        }
        _ => unreachable!("nested ellipsis handled above"),
    }
}

fn ensure_empty_repeated_bindings(
    pattern: &Expr,
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    bindings: &mut Bindings,
) -> Result<(), EvalError> {
    if ellipsis_depth > 1 {
        return Err(syntax_error("nested ellipsis is not supported"));
    }

    match pattern {
        Expr::Symbol(name, _)
            if name != ELLIPSIS && name != "_" && name != "." && !literals.contains(name) =>
        {
            if ellipsis_depth == 1 {
                bindings
                    .entry(name.clone())
                    .or_insert_with(|| BindingMatch::Repeated(Vec::new()));
            }
            Ok(())
        }
        Expr::List(items, _) => {
            let mut index = 0;
            while index < items.len() {
                let (item, next_index, next_depth) =
                    repeated_binding_step(items, index, ellipsis_depth);
                ensure_empty_repeated_bindings(item, literals, next_depth, bindings)?;
                index = next_index;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn repeated_binding_step(
    items: &[Expr],
    index: usize,
    ellipsis_depth: usize,
) -> (&Expr, usize, usize) {
    if followed_by_ellipsis(items, index) {
        (&items[index], index + 2, ellipsis_depth + 1)
    } else {
        (&items[index], index + 1, ellipsis_depth)
    }
}

fn minimum_inputs_required(patterns: &[Expr]) -> usize {
    let mut required = 0;
    let mut index = 0;
    while index < patterns.len() {
        if index + 1 < patterns.len() && is_ellipsis(&patterns[index + 1]) {
            index += 2;
        } else {
            required += 1;
            index += 1;
        }
    }
    required
}

fn expand_template(
    template: &Expr,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    match template {
        Expr::Bool(_, _) | Expr::Number(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {
            Ok(template.clone())
        }
        Expr::Symbol(name, pos) => {
            expand_template_symbol(name, *pos, state, repetition_index, local_renames)
        }
        Expr::Vector(items, pos) => {
            expand_plain_vector(items, *pos, state, repetition_index, local_renames)
        }
        Expr::List(items, pos) if is_quote_form(items, state.pattern_vars) => {
            let datum = expand_quoted_template(&items[1], state, repetition_index)?;
            Ok(Expr::List(
                vec![Expr::Symbol("quote".into(), items[0].pos()), datum],
                *pos,
            ))
        }
        Expr::List(items, pos) if is_template_head(items, state.pattern_vars, "let") => {
            expand_template_let(items, *pos, state, repetition_index, local_renames)
        }
        Expr::List(items, pos) if is_template_head(items, state.pattern_vars, "lambda") => {
            expand_template_lambda(items, *pos, state, repetition_index, local_renames)
        }
        Expr::List(items, pos) if is_template_head(items, state.pattern_vars, "define") => {
            expand_template_define(items, *pos, state, repetition_index, local_renames)
        }
        Expr::List(items, pos) => {
            expand_plain_list(items, *pos, state, repetition_index, local_renames)
        }
    }
}

fn expand_plain_vector(
    items: &[Expr],
    pos: Position,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    let Expr::List(expanded, _) =
        expand_plain_list(items, pos, state, repetition_index, local_renames)?
    else {
        return Err(pos.attach(syntax_error(
            "internal error: vector expansion did not produce a list",
        )));
    };

    Ok(Expr::Vector(expanded, pos))
}

fn expand_template_symbol(
    name: &str,
    pos: Position,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    if name == ELLIPSIS {
        return Err(pos.attach(syntax_error("unexpected ellipsis in macro template")));
    }

    if state.pattern_vars.contains(name) {
        return expand_pattern_variable(name, pos, state, repetition_index);
    }

    if let Some(rename) = local_renames.get(name) {
        return Ok(Expr::Symbol(rename.clone(), pos));
    }

    if state.protected_identifiers.contains(name) {
        return Ok(Expr::Symbol(name.to_string(), pos));
    }

    Ok(capture_free_identifier(name, pos, state))
}

fn expand_pattern_variable(
    name: &str,
    pos: Position,
    state: &ExpansionState<'_>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match state.bindings.get(name) {
        Some(BindingMatch::Scalar(expr)) => Ok(expr.clone()),
        Some(BindingMatch::Repeated(values)) => {
            let Some(index) = repetition_index else {
                return Err(pos.attach(syntax_error(format!(
                    "pattern variable '{name}' requires ellipsis in the template"
                ))));
            };

            values.get(index).cloned().ok_or_else(|| {
                pos.attach(syntax_error(format!(
                    "pattern variable '{name}' is missing repetition {index}"
                )))
            })
        }
        None => Err(pos.attach(syntax_error(format!(
            "unbound pattern variable '{name}' in macro template"
        )))),
    }
}

fn expand_plain_list(
    items: &[Expr],
    pos: Position,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    let mut expanded = Vec::new();
    let mut index = 0;

    while index < items.len() {
        if followed_by_ellipsis(items, index) {
            expand_repeated_template_items(
                &mut expanded,
                items,
                index,
                state,
                repetition_index,
                local_renames,
            )?;
            index += 2;
            continue;
        }

        expanded.push(expand_template(
            &items[index],
            state,
            repetition_index,
            local_renames,
        )?);
        index += 1;
    }

    Ok(Expr::List(expanded, pos))
}

fn expand_repeated_template_items(
    expanded: &mut Vec<Expr>,
    items: &[Expr],
    index: usize,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<(), EvalError> {
    reject_nested_ellipsis(&items[index + 1], repetition_index)?;

    let repeat_count = template_repeat_count(&items[index], state)?;
    for repeat in 0..repeat_count {
        expanded.push(expand_template(
            &items[index],
            state,
            Some(repeat),
            local_renames,
        )?);
    }

    Ok(())
}

fn reject_nested_ellipsis(
    ellipsis: &Expr,
    repetition_index: Option<usize>,
) -> Result<(), EvalError> {
    if repetition_index.is_none() {
        return Ok(());
    }

    Err(ellipsis
        .pos()
        .attach(syntax_error("nested ellipsis is not supported")))
}

fn expand_template_let(
    items: &[Expr],
    pos: Position,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return expand_plain_list(items, pos, state, repetition_index, local_renames);
    }

    let Expr::List(bindings, bindings_pos) = &items[1] else {
        return expand_plain_list(items, pos, state, repetition_index, local_renames);
    };

    let mut body_renames = local_renames.clone();
    let mut expanded_bindings = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let Expr::List(parts, binding_pos) = binding else {
            return Err(positioned_syntax_error(
                binding,
                "macro-generated let bindings must be lists",
            ));
        };
        let [name_expr, value_expr] = parts.as_slice() else {
            return Err(positioned_syntax_error(
                binding,
                "macro-generated let bindings must contain a name and value",
            ));
        };

        let expanded_name = expand_binding_identifier(
            name_expr,
            state,
            repetition_index,
            local_renames,
            &mut body_renames,
        )?;
        let expanded_value = expand_template(value_expr, state, repetition_index, local_renames)?;
        expanded_bindings.push(Expr::List(
            vec![expanded_name, expanded_value],
            *binding_pos,
        ));
    }

    let mut expanded = vec![
        Expr::Symbol("let".into(), items[0].pos()),
        Expr::List(expanded_bindings, *bindings_pos),
    ];
    expanded.extend(expand_template_body_forms(
        &items[2..],
        state,
        repetition_index,
        &mut body_renames,
    )?);

    Ok(Expr::List(expanded, pos))
}

fn expand_template_lambda(
    items: &[Expr],
    pos: Position,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return expand_plain_list(items, pos, state, repetition_index, local_renames);
    }

    let mut body_renames = local_renames.clone();
    let params = expand_formals(
        &items[1],
        state,
        repetition_index,
        local_renames,
        &mut body_renames,
    )?;

    let mut expanded = vec![Expr::Symbol("lambda".into(), items[0].pos()), params];
    expanded.extend(expand_template_body_forms(
        &items[2..],
        state,
        repetition_index,
        &mut body_renames,
    )?);

    Ok(Expr::List(expanded, pos))
}

fn expand_template_define(
    items: &[Expr],
    pos: Position,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    let mut body_renames = local_renames.clone();
    expand_template_define_with_scope(
        items,
        pos,
        state,
        repetition_index,
        local_renames,
        &mut body_renames,
    )
}

fn expand_template_define_with_scope(
    items: &[Expr],
    pos: Position,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
    body_renames: &mut HashMap<String, String>,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return expand_plain_list(items, pos, state, repetition_index, local_renames);
    }

    let mut expanded = vec![Expr::Symbol("define".into(), items[0].pos())];
    match &items[1] {
        Expr::Symbol(_, _) => expanded.push(expand_binding_identifier(
            &items[1],
            state,
            repetition_index,
            local_renames,
            body_renames,
        )?),
        Expr::List(signature, signature_pos) if !signature.is_empty() => {
            let name = expand_binding_identifier(
                &signature[0],
                state,
                repetition_index,
                local_renames,
                body_renames,
            )?;
            let mut define_body_renames = body_renames.clone();
            let mut expanded_signature = vec![name];
            for param in &signature[1..] {
                expanded_signature.push(expand_binding_identifier(
                    param,
                    state,
                    repetition_index,
                    local_renames,
                    &mut define_body_renames,
                )?);
            }
            expanded.push(Expr::List(expanded_signature, *signature_pos));
            expanded.extend(expand_template_body_forms(
                &items[2..],
                state,
                repetition_index,
                &mut define_body_renames,
            )?);
            return Ok(Expr::List(expanded, pos));
        }
        _ => return expand_plain_list(items, pos, state, repetition_index, local_renames),
    }

    for value in &items[2..] {
        expanded.push(expand_template(
            value,
            state,
            repetition_index,
            body_renames,
        )?);
    }

    Ok(Expr::List(expanded, pos))
}

fn expand_template_body_forms(
    body: &[Expr],
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    body_renames: &mut HashMap<String, String>,
) -> Result<Vec<Expr>, EvalError> {
    let mut expanded = Vec::with_capacity(body.len());

    for expr in body {
        match expr {
            Expr::List(items, pos) if is_template_head(items, state.pattern_vars, "define") => {
                let local_renames = body_renames.clone();
                expanded.push(expand_template_define_with_scope(
                    items,
                    *pos,
                    state,
                    repetition_index,
                    &local_renames,
                    body_renames,
                )?);
            }
            _ => expanded.push(expand_template(
                expr,
                state,
                repetition_index,
                body_renames,
            )?),
        }
    }

    Ok(expanded)
}

fn expand_formals(
    formals: &Expr,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
    body_renames: &mut HashMap<String, String>,
) -> Result<Expr, EvalError> {
    match formals {
        Expr::List(params, pos) => expand_formal_list(
            params,
            *pos,
            state,
            repetition_index,
            local_renames,
            body_renames,
        ),
        Expr::Symbol(name, pos) if name != "." => expand_binding_identifier(
            formals,
            state,
            repetition_index,
            local_renames,
            body_renames,
        ),
        _ => expand_template(formals, state, repetition_index, local_renames),
    }
}

fn expand_formal_list(
    params: &[Expr],
    pos: Position,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
    body_renames: &mut HashMap<String, String>,
) -> Result<Expr, EvalError> {
    let expanded = params
        .iter()
        .map(|param| {
            expand_formal_param(param, state, repetition_index, local_renames, body_renames)
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Expr::List(expanded, pos))
}

fn expand_formal_param(
    param: &Expr,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
    body_renames: &mut HashMap<String, String>,
) -> Result<Expr, EvalError> {
    match param {
        Expr::Symbol(name, pos) if name == "." => Ok(Expr::Symbol(name.clone(), *pos)),
        _ => expand_binding_identifier(param, state, repetition_index, local_renames, body_renames),
    }
}

fn expand_binding_identifier(
    template: &Expr,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
    body_renames: &mut HashMap<String, String>,
) -> Result<Expr, EvalError> {
    match template {
        Expr::Symbol(name, pos) if !state.pattern_vars.contains(name) && name != "." => {
            let fresh = state.runtime.fresh_symbol(name);
            body_renames.insert(name.clone(), fresh.clone());
            Ok(Expr::Symbol(fresh, *pos))
        }
        _ => expand_template(template, state, repetition_index, local_renames),
    }
}

fn expand_quoted_template(
    template: &Expr,
    state: &ExpansionState<'_>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match template {
        Expr::Bool(_, _) | Expr::Number(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {
            Ok(template.clone())
        }
        Expr::Symbol(name, pos) if name == ELLIPSIS => {
            Err(pos.attach(syntax_error("unexpected ellipsis in quoted macro template")))
        }
        Expr::Symbol(name, pos) if state.pattern_vars.contains(name) => {
            expand_pattern_variable(name, *pos, state, repetition_index)
        }
        Expr::Symbol(_, _) => Ok(template.clone()),
        Expr::Vector(items, pos) => expand_quoted_vector(items, *pos, state, repetition_index),
        Expr::List(items, pos) => expand_quoted_list(items, *pos, state, repetition_index),
    }
}

fn expand_quoted_vector(
    items: &[Expr],
    pos: Position,
    state: &ExpansionState<'_>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let Expr::List(expanded, _) = expand_quoted_list(items, pos, state, repetition_index)? else {
        return Err(pos.attach(syntax_error(
            "internal error: quoted vector expansion did not produce a list",
        )));
    };

    Ok(Expr::Vector(expanded, pos))
}

fn expand_quoted_list(
    items: &[Expr],
    pos: Position,
    state: &ExpansionState<'_>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let mut expanded = Vec::new();
    let mut index = 0;

    while index < items.len() {
        if followed_by_ellipsis(items, index) {
            expand_repeated_quoted_items(&mut expanded, items, index, state, repetition_index)?;
            index += 2;
            continue;
        }

        expanded.push(expand_quoted_template(
            &items[index],
            state,
            repetition_index,
        )?);
        index += 1;
    }

    Ok(Expr::List(expanded, pos))
}

fn expand_repeated_quoted_items(
    expanded: &mut Vec<Expr>,
    items: &[Expr],
    index: usize,
    state: &ExpansionState<'_>,
    repetition_index: Option<usize>,
) -> Result<(), EvalError> {
    reject_nested_ellipsis(&items[index + 1], repetition_index)?;

    let repeat_count = template_repeat_count(&items[index], state)?;
    for repeat in 0..repeat_count {
        expanded.push(expand_quoted_template(&items[index], state, Some(repeat))?);
    }

    Ok(())
}

fn template_repeat_count(template: &Expr, state: &ExpansionState<'_>) -> Result<usize, EvalError> {
    let mut counts = Vec::new();
    collect_repeat_counts(template, state.pattern_vars, state.bindings, &mut counts);
    let Some(first) = counts.first().copied() else {
        return Err(positioned_syntax_error(
            template,
            "ellipsis in macro template requires a repeated pattern variable",
        ));
    };

    if counts.iter().any(|count| *count != first) {
        return Err(positioned_syntax_error(
            template,
            "all repeated pattern variables in a template must have the same length",
        ));
    }

    Ok(first)
}

fn collect_repeat_counts(
    template: &Expr,
    pattern_vars: &HashSet<String>,
    bindings: &Bindings,
    counts: &mut Vec<usize>,
) {
    match template {
        Expr::Symbol(name, _) if pattern_vars.contains(name) => {
            if let Some(BindingMatch::Repeated(values)) = bindings.get(name) {
                counts.push(values.len());
            }
        }
        Expr::Vector(items, _) | Expr::List(items, _) => {
            for item in items.iter().filter(|item| !is_ellipsis(item)) {
                collect_repeat_counts(item, pattern_vars, bindings, counts);
            }
        }
        _ => {}
    }
}

fn capture_free_identifier(name: &str, pos: Position, state: &mut ExpansionState<'_>) -> Expr {
    if let Some(alias) = state.captured_aliases.get(name) {
        return Expr::Symbol(alias.clone(), pos);
    }

    let Some(binding) = Environment::lookup_cell(state.definition_env, name) else {
        return Expr::Symbol(name.to_string(), pos);
    };

    let alias = state.runtime.fresh_symbol(name);
    Environment::define_cell(&state.expansion_env, alias.clone(), binding);
    state
        .captured_aliases
        .insert(name.to_string(), alias.clone());
    Expr::Symbol(alias, pos)
}

fn build_protected_identifiers(runtime: &Runtime) -> HashSet<String> {
    let mut identifiers = PROTECTED_IDENTIFIERS
        .iter()
        .map(|name| (*name).to_string())
        .collect::<HashSet<_>>();
    identifiers.extend(runtime.macro_names());
    identifiers
}

fn is_template_head(items: &[Expr], pattern_vars: &HashSet<String>, name: &str) -> bool {
    matches!(items.first(), Some(Expr::Symbol(symbol, _)) if symbol == name && !pattern_vars.contains(symbol))
}

fn is_quote_form(items: &[Expr], pattern_vars: &HashSet<String>) -> bool {
    items.len() == 2 && is_template_head(items, pattern_vars, "quote")
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(name, _) if name == ELLIPSIS)
}

fn syntax_error(message: impl Into<String>) -> EvalError {
    EvalError::SyntaxError {
        message: message.into(),
    }
}

fn positioned_syntax_error(expr: &Expr, message: impl Into<String>) -> EvalError {
    expr.pos().attach(syntax_error(message))
}

fn wrong_arg_count(name: &str, expected: impl Into<String>, actual: usize) -> EvalError {
    EvalError::WrongArgCount {
        name: name.into(),
        expected: expected.into(),
        actual,
    }
}
