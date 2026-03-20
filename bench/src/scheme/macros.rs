use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Binding produced by pattern matching: single value or list (from ellipsis).
#[derive(Debug, Clone)]
enum MacroBinding {
    Single(Value),
    List(Vec<Value>),
}

/// Parse a `(define-syntax name (syntax-rules ...))` form and bind the macro.
pub(crate) fn eval_define_syntax(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [Value::Symbol(name), syntax_rules_form] = args else {
        return Err(EvalError::BadSyntax {
            form: "define-syntax".into(),
        });
    };
    let Value::List(sr_elems) = syntax_rules_form else {
        return Err(EvalError::BadSyntax {
            form: "define-syntax".into(),
        });
    };
    let [Value::Symbol(kw), Value::List(lit_list), rules @ ..] = sr_elems.as_slice() else {
        return Err(EvalError::BadSyntax {
            form: "syntax-rules".into(),
        });
    };
    if kw != "syntax-rules" || rules.is_empty() {
        return Err(EvalError::BadSyntax {
            form: "syntax-rules".into(),
        });
    }
    let literals: Vec<String> = lit_list
        .iter()
        .map(|v| match v {
            Value::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::BadSyntax {
                form: "syntax-rules".into(),
            }),
        })
        .collect::<Result<_, _>>()?;
    let parsed_rules: Vec<(Value, Value)> = rules
        .iter()
        .map(|rule| {
            let Value::List(pair) = rule else {
                return Err(EvalError::BadSyntax {
                    form: "syntax-rules".into(),
                });
            };
            let [pattern, template] = pair.as_slice() else {
                return Err(EvalError::BadSyntax {
                    form: "syntax-rules".into(),
                });
            };
            Ok((pattern.clone(), template.clone()))
        })
        .collect::<Result<_, _>>()?;
    env.set(
        name.clone(),
        Value::Macro {
            literals,
            rules: parsed_rules,
            env: Rc::clone(env),
        },
    );
    Ok(Value::Void)
}

/// Try to expand a macro invocation. Returns `Some((expanded, def_env))` if
/// the operator resolves to a macro, `None` otherwise.
pub(crate) fn try_expand(
    elements: &[Value],
    env: &Rc<Env>,
) -> Option<Result<(Value, Rc<Env>), EvalError>> {
    let Value::Symbol(op) = elements.first()? else {
        return None;
    };
    let macro_val = env.get(op)?;
    let Value::Macro {
        literals,
        rules,
        env: def_env,
    } = &macro_val
    else {
        return None;
    };
    Some(expand(elements, literals, rules, def_env))
}

/// Expand a macro invocation against its rules.
fn expand(
    input: &[Value],
    literals: &[String],
    rules: &[(Value, Value)],
    def_env: &Rc<Env>,
) -> Result<(Value, Rc<Env>), EvalError> {
    for (pattern, template) in rules {
        let Value::List(pat_elems) = pattern else {
            continue;
        };
        if let Some(bindings) = match_pattern(pat_elems, input, literals) {
            let expanded = instantiate(template, &bindings);
            return Ok((expanded, Rc::clone(def_env)));
        }
    }
    Err(EvalError::BadSyntax {
        form: "macro expansion".into(),
    })
}

// ── Pattern matching ────────────────────────────────────────────────

/// Match input against a pattern (both include the macro keyword as first element).
fn match_pattern(
    pattern: &[Value],
    input: &[Value],
    literals: &[String],
) -> Option<HashMap<String, MacroBinding>> {
    let mut bindings = HashMap::new();
    // Skip macro keyword (first element of both pattern and input).
    match_elements(&pattern[1..], &input[1..], literals, &mut bindings)?;
    Some(bindings)
}

fn match_elements(
    pattern: &[Value],
    input: &[Value],
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> Option<()> {
    let ellipsis_pos = pattern
        .iter()
        .position(|p| matches!(p, Value::Symbol(s) if s == "..."));

    match ellipsis_pos {
        Some(0) => None,
        Some(pos) => {
            let before = &pattern[..pos - 1];
            let ellipsis_var = &pattern[pos - 1];
            let after = &pattern[pos + 1..];
            let min_len = before.len() + after.len();
            if input.len() < min_len {
                return None;
            }
            for (p, i) in before.iter().zip(input.iter()) {
                match_single(p, i, literals, bindings)?;
            }
            let after_start = input.len() - after.len();
            for (p, i) in after.iter().zip(input[after_start..].iter()) {
                match_single(p, i, literals, bindings)?;
            }
            let middle = &input[before.len()..after_start];
            let Value::Symbol(name) = ellipsis_var else {
                return None;
            };
            if literals.contains(name) {
                return None;
            }
            bindings.insert(name.clone(), MacroBinding::List(middle.to_vec()));
            Some(())
        }
        None => {
            if pattern.len() != input.len() {
                return None;
            }
            for (p, i) in pattern.iter().zip(input.iter()) {
                match_single(p, i, literals, bindings)?;
            }
            Some(())
        }
    }
}

fn match_single(
    pattern: &Value,
    input: &Value,
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> Option<()> {
    match pattern {
        Value::Symbol(name) if literals.contains(name) => {
            matches!(input, Value::Symbol(s) if s == name).then_some(())
        }
        Value::Symbol(name) if name == "_" => Some(()),
        Value::Symbol(name) => {
            bindings.insert(name.clone(), MacroBinding::Single(input.clone()));
            Some(())
        }
        Value::List(pat_elems) => {
            let Value::List(inp_elems) = input else {
                return None;
            };
            match_elements(pat_elems, inp_elems, literals, bindings)
        }
        _ => (pattern == input).then_some(()),
    }
}

// ── Template instantiation ──────────────────────────────────────────

/// Instantiate a template by substituting pattern variable bindings.
fn instantiate(template: &Value, bindings: &HashMap<String, MacroBinding>) -> Value {
    match template {
        Value::Symbol(name) => match bindings.get(name) {
            Some(MacroBinding::Single(val)) => val.clone(),
            _ => template.clone(),
        },
        Value::List(elems) => Value::List(instantiate_list(elems, bindings)),
        _ => template.clone(),
    }
}

fn instantiate_list(
    elems: &[Value],
    bindings: &HashMap<String, MacroBinding>,
) -> Vec<Value> {
    let mut result = Vec::new();
    let mut i = 0;
    while i < elems.len() {
        let has_ellipsis =
            i + 1 < elems.len() && matches!(&elems[i + 1], Value::Symbol(s) if s == "...");

        if has_ellipsis {
            expand_ellipsis(&elems[i], bindings, &mut result);
            i += 2;
        } else {
            result.push(instantiate(&elems[i], bindings));
            i += 1;
        }
    }
    result
}

/// Expand a template element followed by `...`.
fn expand_ellipsis(
    elem: &Value,
    bindings: &HashMap<String, MacroBinding>,
    result: &mut Vec<Value>,
) {
    // Simple case: the element is a symbol bound to a list.
    if let Value::Symbol(name) = elem {
        if let Some(MacroBinding::List(vals)) = bindings.get(name) {
            result.extend(vals.iter().cloned());
            return;
        }
    }
    // Complex case: sub-template containing list-bound variables.
    // Find the first list-bound var and repeat the template for each element.
    let Some((var_name, count)) = find_list_var(elem, bindings) else {
        return;
    };
    for idx in 0..count {
        let mut sub = bindings.clone();
        if let Some(MacroBinding::List(vals)) = bindings.get(&var_name) {
            sub.insert(var_name.clone(), MacroBinding::Single(vals[idx].clone()));
        }
        result.push(instantiate(elem, &sub));
    }
}

/// Find the first list-bound pattern variable inside a template form.
fn find_list_var(
    template: &Value,
    bindings: &HashMap<String, MacroBinding>,
) -> Option<(String, usize)> {
    match template {
        Value::Symbol(name) => {
            if let Some(MacroBinding::List(vals)) = bindings.get(name) {
                Some((name.clone(), vals.len()))
            } else {
                None
            }
        }
        Value::List(elems) => elems.iter().find_map(|e| find_list_var(e, bindings)),
        _ => None,
    }
}
