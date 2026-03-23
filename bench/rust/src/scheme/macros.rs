use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

#[derive(Debug, Clone)]
pub enum Binding {
    Single(Value),
    List(Vec<Value>),
}

/// Expand a syntax-rules macro given the input form.
/// Returns the expanded AST ready for evaluation.
pub fn expand_syntax_rules(
    literals: &[String],
    rules: &[(Value, Value)],
    input: &[Value],
    def_env: &Rc<RefCell<Env>>,
    use_env: &Rc<RefCell<Env>>,
    gensym_counter: &mut usize,
) -> Result<Value, EvalError> {
    let input_args = &input[1..]; // skip macro name
    for (pattern, template) in rules {
        let Value::List(pat_elems, _) = pattern else {
            continue;
        };
        let pat_args = &pat_elems[1..]; // skip macro keyword in pattern
        let mut bindings = HashMap::new();
        if match_pattern(pat_args, input_args, literals, &mut bindings) {
            let mut gensyms: HashMap<String, String> = HashMap::new();
            return expand_template(
                template, &bindings, literals, def_env, use_env,
                gensym_counter, &mut gensyms,
            );
        }
    }
    Err(EvalError::Parse { msg: "no matching syntax-rules pattern".into() })
}

// ---------------------------------------------------------------------------
// Pattern matching
// ---------------------------------------------------------------------------

pub fn match_pattern(
    pattern: &[Value],
    input: &[Value],
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    let mut pi = 0;
    let mut ii = 0;

    while pi < pattern.len() {
        if pi + 1 < pattern.len() && is_ellipsis(&pattern[pi + 1]) {
            // Current element repeats zero or more times
            if let Value::Symbol(name, _) = &pattern[pi] {
                if !literals.contains(name) {
                    bindings.insert(name.clone(), Binding::List(input[ii..].to_vec()));
                }
            }
            pi += 2;
            ii = input.len();
        } else {
            if ii >= input.len() {
                return false;
            }
            if !match_single(&pattern[pi], &input[ii], literals, bindings) {
                return false;
            }
            pi += 1;
            ii += 1;
        }
    }
    ii == input.len()
}

fn match_single(
    pattern: &Value,
    input: &Value,
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    match pattern {
        Value::Symbol(name, _) => {
            if literals.contains(name) {
                matches!(input, Value::Symbol(s, _) if s == name)
            } else if name == "_" {
                true
            } else {
                bindings.insert(name.clone(), Binding::Single(input.clone()));
                true
            }
        }
        Value::List(pat_elems, _) => {
            let Value::List(inp_elems, _) = input else {
                return false;
            };
            match_pattern(pat_elems, inp_elems, literals, bindings)
        }
        Value::Bool(a) => matches!(input, Value::Bool(b) if a == b),
        Value::Int(a) => matches!(input, Value::Int(b) if a == b),
        Value::Float(a) => matches!(input, Value::Float(b) if a == b),
        Value::Rational(an, ad) => matches!(input, Value::Rational(bn, bd) if an == bn && ad == bd),
        Value::String(a) => matches!(input, Value::String(b) if a == b),
        Value::Char(a) => matches!(input, Value::Char(b) if a == b),
        Value::Vector(pat_elems) => {
            let Value::Vector(inp_elems) = input else {
                return false;
            };
            let pat_vec = pat_elems.borrow();
            let inp_vec = inp_elems.borrow();
            match_pattern(&pat_vec, &inp_vec, literals, bindings)
        }
        Value::Pair(_) | Value::Builtin(_) | Value::Closure { .. }
        | Value::Continuation(_) | Value::SyntaxRules { .. }
        | Value::Values(_) | Value::Record { .. }
        | Value::MacroTransformer { .. } | Value::CaseLambda { .. } | Value::Void => false,
    }
}

fn is_ellipsis(val: &Value) -> bool {
    matches!(val, Value::Symbol(s, _) if s == "...")
}

// ---------------------------------------------------------------------------
// Template expansion with hygiene
// ---------------------------------------------------------------------------

pub const SPECIAL_FORMS: &[&str] = &[
    "if", "define", "lambda", "quote", "let", "begin", "cond", "set!",
    "and", "or", "define-syntax", "syntax-rules", "string-set!",
    "let*", "letrec", "letrec*", "case", "do",
    "syntax-case", "syntax", "with-syntax",
    "guard", "define-record-type", "when", "unless",
    "raise", "values", "call-with-values", "call/cc", "call-with-current-continuation",
    "dynamic-wind",
];

pub fn expand_template(
    template: &Value,
    bindings: &HashMap<String, Binding>,
    literals: &[String],
    def_env: &Rc<RefCell<Env>>,
    use_env: &Rc<RefCell<Env>>,
    counter: &mut usize,
    gensyms: &mut HashMap<String, String>,
) -> Result<Value, EvalError> {
    match template {
        Value::Symbol(name, span) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    Binding::Single(val) => Ok(val.clone()),
                    Binding::List(_) => Err(EvalError::Parse {
                        msg: format!("ellipsis variable {name} used outside ellipsis context"),
                    }),
                }
            } else if SPECIAL_FORMS.contains(&name.as_str()) || literals.contains(name) {
                Ok(template.clone())
            } else {
                // Template-introduced identifier — apply hygiene via gensym
                let gensym_name = gensyms.entry(name.clone()).or_insert_with(|| {
                    let g = format!("{name}__{counter}");
                    *counter += 1;
                    g
                });
                // Inject definition-site binding into use-site env
                // Separate borrows to avoid RefCell conflict when def_env == use_env
                let def_val = def_env.borrow().get(name).ok();
                if let Some(val) = def_val {
                    use_env.borrow_mut().define(gensym_name.clone(), val);
                }
                Ok(Value::Symbol(gensym_name.clone(), *span))
            }
        }
        Value::List(elems, span) => {
            // Don't expand inside (quote ...)
            if let Some(Value::Symbol(s, _)) = elems.first() {
                if s == "quote" {
                    return Ok(template.clone());
                }
            }
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() && is_ellipsis(&elems[i + 1]) {
                    let sub = &elems[i];
                    let items = find_ellipsis_var(sub, bindings)
                        .and_then(|var| match bindings.get(&var) {
                            Some(Binding::List(items)) => Some((var, items.clone())),
                            _ => None,
                        });
                    if let Some((var_name, items)) = items {
                        for item in &items {
                            let mut sub_bindings = bindings.clone();
                            sub_bindings.insert(var_name.clone(), Binding::Single(item.clone()));
                            result.push(expand_template(
                                sub, &sub_bindings, literals,
                                def_env, use_env, counter, gensyms,
                            )?);
                        }
                    }
                    i += 2;
                } else {
                    result.push(expand_template(
                        &elems[i], bindings, literals,
                        def_env, use_env, counter, gensyms,
                    )?);
                    i += 1;
                }
            }
            Ok(Value::List(result, *span))
        }
        Value::Vector(elems_cell) => {
            let elems = elems_cell.borrow();
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() && is_ellipsis(&elems[i + 1]) {
                    let sub = &elems[i];
                    let items = find_ellipsis_var(sub, bindings)
                        .and_then(|var| match bindings.get(&var) {
                            Some(Binding::List(items)) => Some((var, items.clone())),
                            _ => None,
                        });
                    if let Some((var_name, items)) = items {
                        for item in &items {
                            let mut sub_bindings = bindings.clone();
                            sub_bindings.insert(var_name.clone(), Binding::Single(item.clone()));
                            result.push(expand_template(
                                sub, &sub_bindings, literals,
                                def_env, use_env, counter, gensyms,
                            )?);
                        }
                    }
                    i += 2;
                } else {
                    result.push(expand_template(
                        &elems[i], bindings, literals,
                        def_env, use_env, counter, gensyms,
                    )?);
                    i += 1;
                }
            }
            Ok(Value::Vector(Rc::new(RefCell::new(result))))
        }
        Value::Int(_) | Value::Float(_) | Value::Rational(_, _)
        | Value::Bool(_) | Value::String(_) | Value::Char(_)
        | Value::Pair(_) | Value::Builtin(_) | Value::Closure { .. }
        | Value::Continuation(_) | Value::SyntaxRules { .. }
        | Value::Values(_) | Value::Record { .. }
        | Value::MacroTransformer { .. } | Value::CaseLambda { .. } | Value::Void => Ok(template.clone()),
    }
}

pub fn find_ellipsis_var(template: &Value, bindings: &HashMap<String, Binding>) -> Option<String> {
    match template {
        Value::Symbol(name, _) => {
            if matches!(bindings.get(name), Some(Binding::List(_))) {
                Some(name.clone())
            } else {
                None
            }
        }
        Value::List(elems, _) => {
            for elem in elems {
                if let Some(var) = find_ellipsis_var(elem, bindings) {
                    return Some(var);
                }
            }
            None
        }
        Value::Vector(elems_cell) => {
            let elems = elems_cell.borrow();
            for elem in elems.iter() {
                if let Some(var) = find_ellipsis_var(elem, bindings) {
                    return Some(var);
                }
            }
            None
        }
        Value::Int(_) | Value::Float(_) | Value::Rational(_, _)
        | Value::Bool(_) | Value::String(_) | Value::Char(_)
        | Value::Pair(_) | Value::Builtin(_) | Value::Closure { .. }
        | Value::Continuation(_) | Value::SyntaxRules { .. }
        | Value::Values(_) | Value::Record { .. }
        | Value::MacroTransformer { .. } | Value::CaseLambda { .. } | Value::Void => None,
    }
}
