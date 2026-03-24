use std::collections::HashMap;

use super::error::Span;
use super::macros;
use super::{
    apply, env_get, env_set, eval, gensym, new_env, Bounce, Env, EvalError, Output, Spanned,
    Value, SYNTAX_CASE_BINDINGS, SYNTAX_RENAME_SINK,
};

const SYNTAX_SPECIAL_FORMS: &[&str] = &[
    "quote", "if", "define", "lambda", "let", "let*", "letrec", "begin", "set!",
    "cond", "and", "or", "not", "string-set!", "define-syntax", "syntax-rules",
    "syntax-case", "syntax", "with-syntax", "case", "when", "unless", "do",
    "case-lambda", "define-record-type", "guard", "quasiquote",
];

/// Expand a syntax-case macro transformer: call the transformer procedure
/// with the input form as a syntax object, then inject hygiene renames.
pub(super) fn expand_syntax_case_macro(
    items: &[Spanned], transformer: &Value, env: &Env, out: &Output, span: Span,
) -> Result<Spanned, EvalError> {
    let form = Value::List(items.to_vec());
    SYNTAX_RENAME_SINK.with(|s| s.borrow_mut().clear());
    let expanded_val = apply(transformer, &[form], out, span)?;
    let renames = SYNTAX_RENAME_SINK.with(|s| {
        let mut sink = s.borrow_mut();
        std::mem::take(&mut *sink)
    });
    for (gs, val) in renames {
        env_set(env, gs, val);
    }
    Ok(Spanned::new(expanded_val, span))
}

/// Evaluate (syntax-case expr (literals) clause ...)
pub(super) fn eval_syntax_case(
    items: &[Spanned], env: &Env, out: &Output, span: Span,
) -> Result<Bounce, EvalError> {
    if items.len() < 4 {
        return Err(EvalError::Arity("syntax-case requires expr, literals, and clauses".into(), span));
    }
    let scrutinee_val = eval(&items[1], env, out)?;
    let scrutinee = Spanned::new(scrutinee_val, items[1].span);

    let Value::List(lit_list) = &items[2].val else {
        return Err(EvalError::Type("syntax-case: expected literals list".into(), span));
    };
    let literals: Vec<String> = lit_list.iter().filter_map(|l| {
        if let Value::Symbol(s) = &l.val { Some(s.clone()) } else { None }
    }).collect();

    for clause in &items[3..] {
        let Value::List(parts) = &clause.val else {
            return Err(EvalError::Type("syntax-case: expected clause".into(), span));
        };
        if parts.len() < 2 || parts.len() > 3 {
            return Err(EvalError::Arity("syntax-case: clause needs pattern and body".into(), span));
        }
        let pattern = &parts[0];
        let (fender, body) = if parts.len() == 3 {
            (Some(&parts[1]), &parts[2])
        } else {
            (None, &parts[1])
        };

        let mut bindings = macros::Bindings::new();
        if macros::match_single(pattern, &scrutinee, &literals, &mut bindings) {
            // Check fender if present
            if let Some(fender_expr) = fender {
                let fender_env = new_env(Some(env.clone()));
                for (name, binding) in &bindings {
                    match binding {
                        macros::PatternBinding::Single(v) => env_set(&fender_env, name.clone(), v.val.clone()),
                        macros::PatternBinding::List(vs) => {
                            let list_val = Value::List(vs.clone());
                            env_set(&fender_env, name.clone(), list_val);
                        }
                    }
                }
                let fender_result = eval(fender_expr, &fender_env, out)?;
                if !fender_result.is_truthy() {
                    continue;
                }
            }

            // Push bindings and evaluate body
            SYNTAX_CASE_BINDINGS.with(|b| b.borrow_mut().push(bindings));
            let result = eval(body, env, out);
            SYNTAX_CASE_BINDINGS.with(|b| b.borrow_mut().pop());
            return result.map(Bounce::Done);
        }
    }

    Err(EvalError::Type("syntax-case: no matching clause".into(), span))
}

/// Evaluate (syntax template) — #'template
pub(super) fn eval_syntax_template(
    items: &[Spanned], env: &Env, span: Span,
) -> Result<Bounce, EvalError> {
    if items.len() != 2 {
        return Err(EvalError::Arity("syntax requires 1 argument".into(), span));
    }
    let template = &items[1];

    // Merge all syntax-case binding frames
    let bindings = SYNTAX_CASE_BINDINGS.with(|b| {
        let stack = b.borrow();
        let mut merged = macros::Bindings::new();
        for frame in stack.iter() {
            for (k, v) in frame {
                merged.insert(k.clone(), v.clone());
            }
        }
        merged
    });

    // Simple case: template is a single symbol that's a pattern variable
    if let Value::Symbol(name) = &template.val {
        if let Some(binding) = bindings.get(name) {
            match binding {
                macros::PatternBinding::Single(val) => return Ok(Bounce::Done(val.val.clone())),
                macros::PatternBinding::List(_) => {
                    return Err(EvalError::Type("syntax: ellipsis variable used without ellipsis".into(), span));
                }
            }
        }
    }

    // Compute hygiene renames
    let pattern_vars: std::collections::HashSet<String> = bindings.keys().cloned().collect();
    let free_vars = macros::collect_free_vars(template, &pattern_vars);
    let mut renames = HashMap::new();
    for var in &free_vars {
        if !SYNTAX_SPECIAL_FORMS.contains(&var.as_str()) {
            renames.insert(var.clone(), gensym(var));
        }
    }

    // Store renames for hygiene injection into use-site env
    for (orig, gs) in &renames {
        if let Some(val) = env_get(env, orig) {
            SYNTAX_RENAME_SINK.with(|sink| {
                sink.borrow_mut().push((gs.clone(), val));
            });
        }
    }

    let expanded = macros::instantiate_template(template, &bindings, &renames);
    Ok(Bounce::Done(expanded.val.clone()))
}

/// Evaluate (with-syntax ((pattern expr) ...) body ...)
pub(super) fn eval_with_syntax(
    items: &[Spanned], env: &Env, out: &Output, span: Span,
) -> Result<Bounce, EvalError> {
    if items.len() < 3 {
        return Err(EvalError::Arity("with-syntax requires bindings and body".into(), span));
    }
    let Value::List(binding_list) = &items[1].val else {
        return Err(EvalError::Type("with-syntax: expected binding list".into(), span));
    };

    let mut bindings = macros::Bindings::new();
    for binding in binding_list {
        let Value::List(bparts) = &binding.val else {
            return Err(EvalError::Type("with-syntax: expected (pattern expr)".into(), span));
        };
        if bparts.len() != 2 {
            return Err(EvalError::Arity("with-syntax: binding needs pattern and expression".into(), span));
        }
        let pattern = &bparts[0];
        let val = eval(&bparts[1], env, out)?;
        let val_spanned = Spanned::new(val, bparts[1].span);
        macros::match_single(pattern, &val_spanned, &[], &mut bindings);
    }

    SYNTAX_CASE_BINDINGS.with(|b| b.borrow_mut().push(bindings));
    // Must fully evaluate body before popping bindings (not tail-deferred)
    let result = if items.len() == 3 {
        eval(&items[2], env, out)
    } else {
        for expr in &items[2..items.len()-1] {
            eval(expr, env, out)?;
        }
        eval(&items[items.len()-1], env, out)
    };
    SYNTAX_CASE_BINDINGS.with(|b| b.borrow_mut().pop());
    result.map(Bounce::Done)
}
