use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::value::Value;

/// A pattern variable binding.
#[derive(Debug, Clone)]
enum Binding {
    /// Single matched value.
    One(Value),
    /// Multiple matched values (from `...` ellipsis).
    Many(Vec<Value>),
}

/// Attempt to expand a macro invocation. Returns the expanded (unevaluated) form.
pub fn expand_macro(
    macro_name: &str,
    keywords: &[String],
    rules: &[(Value, Value)],
    def_env: &Rc<RefCell<Env>>,
    input: &[Value],
    span: Span,
    gensym_counter: &Cell<u64>,
) -> Result<Value, EvalError> {
    for (pattern, template) in rules {
        let Value::List(pat_elems) = pattern else {
            continue;
        };
        let pat_args = &pat_elems[1..];
        let input_args = &input[1..];

        let mut bindings = HashMap::new();
        if match_elements(pat_args, input_args, keywords, &mut bindings) {
            let mut gensym_map = HashMap::new();
            return Ok(expand_template(
                template,
                &bindings,
                macro_name,
                def_env,
                gensym_counter,
                &mut gensym_map,
            ));
        }
    }

    Err(EvalError::TypeError {
        message: format!("{macro_name}: no matching pattern"),
        span,
    })
}

// --- Pattern matching ---

fn match_elements(
    pattern: &[Value],
    input: &[Value],
    keywords: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    let ellipsis_pos = pattern
        .iter()
        .position(|v| matches!(v, Value::Symbol(s) if s == "..."));

    let Some(pos) = ellipsis_pos else {
        return match_elements_exact(pattern, input, keywords, bindings);
    };

    {
        if pos == 0 {
            return false;
        }

        let before = &pattern[..pos - 1];
        let repeated = &pattern[pos - 1];
        let after = &pattern[pos + 1..];
        let min_required = before.len() + after.len();

        if input.len() < min_required {
            return false;
        }

        let all_before_match = before
            .iter()
            .zip(input.iter())
            .all(|(p, i)| match_single(p, i, keywords, bindings));
        if !all_before_match {
            return false;
        }

        let after_start = input.len() - after.len();
        let all_after_match = after
            .iter()
            .zip(input[after_start..].iter())
            .all(|(p, i)| match_single(p, i, keywords, bindings));
        if !all_after_match {
            return false;
        }

        let repeated_input = &input[before.len()..after_start];
        match repeated {
            Value::Symbol(var) if !keywords.contains(var) && var != "_" => {
                bindings.insert(var.clone(), Binding::Many(repeated_input.to_vec()));
                true
            }
            _ => false,
        }
    }
}

fn match_elements_exact(
    pattern: &[Value],
    input: &[Value],
    keywords: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    {
        if pattern.len() != input.len() {
            return false;
        }
        pattern
            .iter()
            .zip(input.iter())
            .all(|(p, i)| match_single(p, i, keywords, bindings))
    }
}

fn match_single(
    pattern: &Value,
    input: &Value,
    keywords: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    match pattern {
        Value::Symbol(name) if keywords.contains(name) => {
            matches!(input, Value::Symbol(s) if s == name)
        }
        Value::Symbol(name) if name == "_" => true,
        Value::Symbol(name) => {
            bindings.insert(name.clone(), Binding::One(input.clone()));
            true
        }
        Value::List(p_elems) => {
            let Value::List(i_elems) = input else {
                return false;
            };
            match_elements(p_elems, i_elems, keywords, bindings)
        }
        Value::Boolean(b) => matches!(input, Value::Boolean(b2) if b == b2),
        Value::Integer(n) => matches!(input, Value::Integer(n2) if n == n2),
        Value::String(s) => matches!(input, Value::String(s2) if s == s2),
        _ => false,
    }
}

// --- Template expansion ---

fn is_special_form(name: &str) -> bool {
    matches!(
        name,
        "if" | "begin"
            | "let"
            | "set!"
            | "define"
            | "define-syntax"
            | "quote"
            | "lambda"
            | "cond"
            | "and"
            | "or"
            | "syntax-rules"
            | "string-set!"
    )
}

fn is_builtin_name(name: &str) -> bool {
    matches!(
        name,
        "+" | "-"
            | "*"
            | "/"
            | "<"
            | ">"
            | "="
            | "<="
            | ">="
            | "not"
            | "cons"
            | "car"
            | "cdr"
            | "null?"
            | "list"
            | "length"
            | "string?"
            | "number?"
            | "boolean?"
            | "pair?"
            | "symbol?"
            | "char?"
            | "apply"
            | "map"
            | "display"
            | "write"
            | "newline"
            | "string-append"
            | "string-length"
            | "substring"
            | "string->number"
            | "number->string"
            | "symbol->string"
            | "string->symbol"
            | "string-ref"
            | "string-copy"
            | "string->list"
            | "list->string"
            | "char->integer"
            | "integer->char"
            | "call/cc"
            | "call-with-current-continuation"
            | "set-car!"
            | "set-cdr!"
            | "caar"
            | "cadr"
            | "cdar"
            | "cddr"
    )
}

fn expand_template(
    template: &Value,
    bindings: &HashMap<String, Binding>,
    macro_name: &str,
    def_env: &Rc<RefCell<Env>>,
    counter: &Cell<u64>,
    gensym_map: &mut HashMap<String, String>,
) -> Value {
    match template {
        Value::Symbol(name) => {
            if let Some(binding) = bindings.get(name) {
                return match binding {
                    Binding::One(v) => v.clone(),
                    Binding::Many(_) => Value::Symbol(name.clone()),
                };
            }

            if is_special_form(name) || is_builtin_name(name) || name == macro_name {
                return Value::Symbol(name.clone());
            }

            // Free variable — check definition env for hygiene.
            // Skip Macro values: they must be resolved by symbol name at eval time
            // because eval_list_tco dispatches macros via symbol-based env lookup.
            if let Some(val) = def_env.borrow().get(name).filter(|v| !matches!(v, Value::Macro { .. })) {
                return val;
            }

            // Introduced identifier — leave as-is for runtime resolution
            Value::Symbol(name.clone())
        }
        Value::List(elems) => expand_template_list(elems, bindings, macro_name, def_env, counter, gensym_map),
        other => other.clone(),
    }
}

fn expand_template_list(
    elems: &[Value],
    bindings: &HashMap<String, Binding>,
    macro_name: &str,
    def_env: &Rc<RefCell<Env>>,
    counter: &Cell<u64>,
    gensym_map: &mut HashMap<String, String>,
) -> Value {
    let mut result = Vec::new();
    let mut i = 0;
    while i < elems.len() {
        if i + 1 < elems.len() && matches!(&elems[i + 1], Value::Symbol(s) if s == "...") {
            expand_ellipsis(&elems[i], bindings, macro_name, def_env, counter, gensym_map, &mut result);
            i += 2;
        } else {
            result.push(expand_template(&elems[i], bindings, macro_name, def_env, counter, gensym_map));
            i += 1;
        }
    }
    Value::List(result)
}

/// Collect names of ellipsis-bound variables reachable from a template fragment.
fn collect_ellipsis_vars(template: &Value, bindings: &HashMap<String, Binding>) -> Vec<String> {
    match template {
        Value::Symbol(name) => {
            if matches!(bindings.get(name), Some(Binding::Many(_))) {
                vec![name.clone()]
            } else {
                vec![]
            }
        }
        Value::List(elems) => elems
            .iter()
            .flat_map(|e| collect_ellipsis_vars(e, bindings))
            .collect(),
        _ => vec![],
    }
}

fn bind_ellipsis_iteration(
    vars: &[String],
    bindings: &HashMap<String, Binding>,
    idx: usize,
    local_bindings: &mut HashMap<String, Binding>,
) {
    for var in vars {
        let val = bindings.get(var).and_then(|b| match b {
            Binding::Many(vals) => vals.get(idx).cloned(),
            Binding::One(_) => None,
        });
        if let Some(v) = val {
            local_bindings.insert(var.clone(), Binding::One(v));
        }
    }
}

fn expand_ellipsis(
    template_elem: &Value,
    bindings: &HashMap<String, Binding>,
    macro_name: &str,
    def_env: &Rc<RefCell<Env>>,
    counter: &Cell<u64>,
    gensym_map: &mut HashMap<String, String>,
    result: &mut Vec<Value>,
) {
    let vars = collect_ellipsis_vars(template_elem, bindings);
    let Some(primary_var) = vars.first() else {
        return;
    };
    let Some(Binding::Many(values)) = bindings.get(primary_var) else {
        return;
    };

    for idx in 0..values.len() {
        let mut local_bindings = bindings.clone();
        bind_ellipsis_iteration(&vars, bindings, idx, &mut local_bindings);
        result.push(expand_template(
            template_elem, &local_bindings, macro_name, def_env, counter, gensym_map,
        ));
    }
}
