use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::{Env, EvalError, Value};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let id = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("\0{base}_{id}")
}

/// Parse `(syntax-rules (literals...) (pattern template) ...)` into a `SyntaxRules` value.
pub(super) fn parse_syntax_rules(
    sr_args: &[Value],
    name: &str,
    def_env: &Env,
) -> Result<Value, EvalError> {
    let [literals_val, rules @ ..] = sr_args else {
        return Err(EvalError::TypeError {
            expected: "syntax-rules form".to_string(),
            got: format!("{} args", sr_args.len()),
        });
    };

    let literals: Vec<String> = literals_val
        .to_list_vec()
        .ok_or_else(|| EvalError::TypeError {
            expected: "literal list".to_string(),
            got: literals_val.display(),
        })?
        .iter()
        .filter_map(|v| match v {
            Value::Symbol(s) => Some(s.clone()),
            _ => None,
        })
        .collect();

    let parsed_rules: Vec<(Value, Value)> = rules
        .iter()
        .map(|rule| {
            let items = rule.to_list_vec().ok_or_else(|| EvalError::TypeError {
                expected: "(pattern template)".to_string(),
                got: rule.display(),
            })?;
            match items.as_slice() {
                [pattern, template] => Ok((pattern.clone(), template.clone())),
                _ => Err(EvalError::TypeError {
                    expected: "(pattern template)".to_string(),
                    got: rule.display(),
                }),
            }
        })
        .collect::<Result<_, _>>()?;

    Ok(Value::SyntaxRules {
        name: name.to_string(),
        literals,
        rules: parsed_rules,
        def_env: def_env.clone(),
    })
}

/// Expand a macro call. `input_args` are the unevaluated arguments (without the macro name).
/// Injects hygiene bindings from the definition-site environment into `env`.
pub(super) fn expand_macro(
    macro_name: &str,
    literals: &[String],
    rules: &[(Value, Value)],
    def_env: &Env,
    input_args: &[Value],
    env: &mut Env,
) -> Result<Value, EvalError> {
    for (pattern, template) in rules {
        let pattern_items = pattern.to_list_vec().ok_or_else(|| EvalError::TypeError {
            expected: "pattern list".to_string(),
            got: pattern.display(),
        })?;

        // Skip the macro name (first element of pattern)
        let pattern_args = &pattern_items[1..];

        let mut bindings = HashMap::new();
        if !match_pattern(pattern_args, input_args, literals, &mut bindings) {
            continue;
        }

        let rename_map =
            build_rename_map(template, pattern_args, macro_name, def_env, env);

        return Ok(expand_template_inner(
            template,
            &bindings,
            &rename_map,
            macro_name,
        ));
    }

    Err(EvalError::TypeError {
        expected: format!("matching pattern for macro {macro_name}"),
        got: format!("({macro_name} ...)"),
    })
}

fn build_rename_map(
    template: &Value,
    pattern_args: &[Value],
    macro_name: &str,
    def_env: &Env,
    env: &mut Env,
) -> HashMap<String, String> {
    let mut pattern_vars = Vec::new();
    collect_pattern_vars(pattern_args, &mut pattern_vars);

    let mut rename_map = HashMap::new();
    collect_template_free_vars(template, &pattern_vars, macro_name, &mut rename_map);

    for (orig, renamed) in &rename_map {
        if let Some(val_rc) = def_env.get(orig) {
            env.insert(renamed.clone(), val_rc.clone());
        }
    }

    rename_map
}

// --- Pattern variable collection ---

fn collect_pattern_vars(pattern: &[Value], vars: &mut Vec<String>) {
    for item in pattern {
        match item {
            Value::Symbol(s) if s != "..." => vars.push(s.clone()),
            Value::Pair(..) => collect_pattern_vars_from_pair(item, vars),
            _ => {}
        }
    }
}

fn collect_pattern_vars_from_pair(item: &Value, vars: &mut Vec<String>) {
    if let Some(items) = item.to_list_vec() {
        collect_pattern_vars(&items, vars);
    }
}

// --- Pattern matching ---

#[derive(Debug, Clone)]
enum Binding {
    Single(Value),
    List(Vec<Value>),
}

fn match_pattern(
    pattern: &[Value],
    input: &[Value],
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    let mut pi = 0;
    let mut ii = 0;

    while pi < pattern.len() {
        // Check if current element is followed by ellipsis
        if pi + 1 < pattern.len()
            && matches!(&pattern[pi + 1], Value::Symbol(s) if s == "...")
        {
            return match_ellipsis(&pattern[pi], &input[ii..], bindings);
        }

        if ii >= input.len() {
            return false;
        }

        if !match_element(&pattern[pi], &input[ii], literals, bindings) {
            return false;
        }

        pi += 1;
        ii += 1;
    }

    ii == input.len()
}

fn match_ellipsis(
    pattern_elem: &Value,
    remaining_input: &[Value],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    let Value::Symbol(var) = pattern_elem else {
        return false;
    };
    bindings.insert(var.clone(), Binding::List(remaining_input.to_vec()));
    true
}

fn match_element(
    pat: &Value,
    inp: &Value,
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    match pat {
        Value::Symbol(s) if s == "..." => false,
        Value::Symbol(s) if literals.contains(s) => {
            matches!(inp, Value::Symbol(is) if is == s)
        }
        Value::Symbol(s) => {
            bindings.insert(s.clone(), Binding::Single(inp.clone()));
            true
        }
        Value::Pair(..) => match_pair_element(pat, inp, literals, bindings),
        _ => pat == inp,
    }
}

fn match_pair_element(
    pat: &Value,
    inp: &Value,
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    let Some(pat_items) = pat.to_list_vec() else {
        return false;
    };
    let Some(inp_items) = inp.to_list_vec() else {
        return false;
    };
    match_pattern(&pat_items, &inp_items, literals, bindings)
}

// --- Template expansion ---

fn is_special_or_builtin(name: &str) -> bool {
    matches!(
        name,
        "define" | "if" | "quote" | "lambda" | "begin" | "let" | "cond" | "and" | "or"
            | "set!" | "string-set!" | "call/cc" | "call-with-current-continuation"
            | "define-syntax" | "syntax-rules" | "else"
            | "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
            | "cons" | "car" | "cdr" | "null?" | "list" | "length"
            | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
            | "display" | "write" | "newline"
            | "string-append" | "string-length" | "substring"
            | "string->number" | "number->string"
            | "symbol->string" | "string->symbol"
            | "string-ref" | "string-copy"
            | "string->list" | "list->string"
            | "char->integer" | "integer->char"
            | "map" | "apply"
    )
}

fn collect_template_free_vars(
    template: &Value,
    pattern_vars: &[String],
    macro_name: &str,
    rename_map: &mut HashMap<String, String>,
) {
    match template {
        Value::Symbol(s)
            if s != "..."
                && s != macro_name
                && !pattern_vars.contains(s)
                && !is_special_or_builtin(s)
                && !rename_map.contains_key(s) =>
        {
            rename_map.insert(s.clone(), gensym(s));
        }
        Value::Pair(car, cdr) => {
            collect_template_free_vars(car, pattern_vars, macro_name, rename_map);
            collect_template_free_vars(cdr, pattern_vars, macro_name, rename_map);
        }
        _ => {}
    }
}

fn expand_template_inner(
    template: &Value,
    bindings: &HashMap<String, Binding>,
    rename_map: &HashMap<String, String>,
    macro_name: &str,
) -> Value {
    match template {
        Value::Symbol(s) => {
            if let Some(binding) = bindings.get(s) {
                match binding {
                    Binding::Single(v) => v.clone(),
                    Binding::List(vs) => vs.first().cloned().unwrap_or(Value::Nil),
                }
            } else if let Some(renamed) = rename_map.get(s) {
                Value::Symbol(renamed.clone())
            } else {
                template.clone()
            }
        }
        Value::Pair(..) => expand_list_template(template, bindings, rename_map, macro_name),
        _ => template.clone(),
    }
}

fn expand_list_template(
    template: &Value,
    bindings: &HashMap<String, Binding>,
    rename_map: &HashMap<String, String>,
    macro_name: &str,
) -> Value {
    let Some(items) = template.to_list_vec() else {
        // Improper list — expand car and cdr separately
        let Value::Pair(car, cdr) = template else {
            unreachable!()
        };
        let new_car = expand_template_inner(car, bindings, rename_map, macro_name);
        let new_cdr = expand_template_inner(cdr, bindings, rename_map, macro_name);
        return Value::Pair(Box::new(new_car), Box::new(new_cdr));
    };

    let mut result = Vec::new();
    let mut i = 0;
    while i < items.len() {
        if i + 1 < items.len()
            && matches!(&items[i + 1], Value::Symbol(s) if s == "...")
        {
            expand_ellipsis_splice(&items[i], bindings, rename_map, macro_name, &mut result);
            i += 2; // skip element + ellipsis
        } else {
            result.push(expand_template_inner(
                &items[i], bindings, rename_map, macro_name,
            ));
            i += 1;
        }
    }

    result
        .into_iter()
        .rev()
        .fold(Value::Nil, |acc, v| Value::Pair(Box::new(v), Box::new(acc)))
}

fn expand_ellipsis_splice(
    template_elem: &Value,
    bindings: &HashMap<String, Binding>,
    rename_map: &HashMap<String, String>,
    macro_name: &str,
    result: &mut Vec<Value>,
) {
    let Some(var_name) = find_ellipsis_var(template_elem, bindings) else {
        return;
    };
    let Some(Binding::List(vs)) = bindings.get(&var_name) else {
        return;
    };
    for v in vs {
        let mut sub_bindings = bindings.clone();
        sub_bindings.insert(var_name.clone(), Binding::Single(v.clone()));
        result.push(expand_template_inner(
            template_elem,
            &sub_bindings,
            rename_map,
            macro_name,
        ));
    }
}

/// Find which ellipsis-bound variable is referenced in a template element.
fn find_ellipsis_var(template: &Value, bindings: &HashMap<String, Binding>) -> Option<String> {
    match template {
        Value::Symbol(s) if matches!(bindings.get(s), Some(Binding::List(_))) => Some(s.clone()),
        Value::Pair(car, cdr) => {
            find_ellipsis_var(car, bindings).or_else(|| find_ellipsis_var(cdr, bindings))
        }
        _ => None,
    }
}
