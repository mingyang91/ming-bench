use std::collections::{HashMap, HashSet};

use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::parser::Expr;
use crate::scheme::value::Value;

/// A binding from pattern matching.
enum PatternBinding {
    Single(Expr),
    Repeated(Vec<Expr>),
}

/// Result of macro expansion.
pub struct Expansion {
    pub expr: Expr,
    pub hygiene_bindings: Vec<(String, Value)>,
}

/// Top-level macro expansion: match call args against rules, expand template.
pub fn expand_macro(
    literals: &[String],
    rules: &[(Vec<Expr>, Expr)],
    def_env: &Env,
    call_args: &[Expr],
    span: Span,
    use_env: &Env,
) -> Result<Expansion, EvalError> {
    for (pattern_elems, template) in rules {
        let pattern_args = &pattern_elems[1..]; // skip macro name in pattern
        if let Some(bindings) = match_pattern(pattern_args, call_args, literals) {
            let pattern_vars: HashSet<String> = bindings.keys().cloned().collect();
            return expand_with_hygiene(template, &bindings, &pattern_vars, def_env, span, use_env);
        }
    }
    Err(EvalError::Parse("no matching syntax-rules pattern".into()).at(span))
}

fn match_pattern(
    pattern: &[Expr],
    args: &[Expr],
    literals: &[String],
) -> Option<HashMap<String, PatternBinding>> {
    let mut bindings = HashMap::new();
    match_elements(pattern, args, literals, &mut bindings).map(|()| bindings)
}

fn match_elements(
    pattern: &[Expr],
    args: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, PatternBinding>,
) -> Option<()> {
    let mut pi = 0;
    let mut ai = 0;

    while pi < pattern.len() {
        let has_ellipsis = pi + 1 < pattern.len()
            && matches!(&pattern[pi + 1], Expr::Symbol(s, _) if s == "...");

        if has_ellipsis {
            let remaining_fixed = pattern.len() - pi - 2;
            let available = args.len().checked_sub(ai + remaining_fixed)?;
            match_ellipsis(&pattern[pi], &args[ai..ai + available], literals, bindings)?;
            ai += available;
            pi += 2;
        } else if ai >= args.len() {
            return None;
        } else {
            match_single(&pattern[pi], &args[ai], literals, bindings)?;
            ai += 1;
            pi += 1;
        }
    }

    (ai == args.len()).then_some(())
}

fn match_ellipsis(
    pattern: &Expr,
    args: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, PatternBinding>,
) -> Option<()> {
    match pattern {
        Expr::Symbol(name, _) if !literals.contains(name) && name != "_" => {
            bindings.insert(name.clone(), PatternBinding::Repeated(args.to_vec()));
        }
        _ => {
            for arg in args {
                let mut dummy = HashMap::new();
                match_single(pattern, arg, literals, &mut dummy)?;
            }
        }
    }
    Some(())
}

fn match_single(
    pattern: &Expr,
    arg: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, PatternBinding>,
) -> Option<()> {
    match pattern {
        Expr::Symbol(name, _) if name == "_" => Some(()),
        Expr::Symbol(name, _) if literals.contains(name) => {
            matches!(arg, Expr::Symbol(s, _) if s == name).then_some(())
        }
        Expr::Symbol(name, _) => {
            bindings.insert(name.clone(), PatternBinding::Single(arg.clone()));
            Some(())
        }
        Expr::List(sub_pat, _) => {
            let Expr::List(arg_elems, _) = arg else {
                return None;
            };
            match_elements(sub_pat, arg_elems, literals, bindings)
        }
        Expr::Integer(n, _) => matches!(arg, Expr::Integer(m, _) if m == n).then_some(()),
        Expr::Rational(n, d, _) => matches!(arg, Expr::Rational(m, e, _) if m == n && e == d).then_some(()),
        Expr::Float(x, _) => matches!(arg, Expr::Float(y, _) if y == x).then_some(()),
        Expr::Boolean(b, _) => matches!(arg, Expr::Boolean(c, _) if c == b).then_some(()),
        Expr::String(s, _) => matches!(arg, Expr::String(t, _) if t == s).then_some(()),
        Expr::Char(c, _) => matches!(arg, Expr::Char(d, _) if d == c).then_some(()),
    }
}

/// Symbols that should not be renamed during hygiene.
fn is_reserved(name: &str) -> bool {
    matches!(
        name,
        // Special forms
        "if" | "let" | "let*" | "letrec" | "letrec*"
            | "begin" | "set!" | "define" | "quote"
            | "cond" | "case" | "and" | "or"
            | "lambda" | "when" | "unless" | "do"
            | "define-syntax" | "syntax-rules" | "syntax-case"
            | "syntax" | "with-syntax"
            | "string-set!" | "else" | "=>"
            | "dynamic-wind" | "raise" | "guard"
            | "with-exception-handler" | "define-record-type"
            | "let-values" | "receive"
            | "values" | "call-with-values"
            // Builtins
            | "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
            | "not" | "cons" | "car" | "cdr"
            | "caar" | "cadr" | "cdar" | "cddr"
            | "null?" | "list" | "length"
            | "set-car!" | "set-cdr!"
            | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
            | "display" | "write" | "newline"
            | "string-append" | "string-length" | "substring"
            | "string->number" | "number->string"
            | "string-ref" | "symbol->string" | "string->symbol"
            | "string-copy"
            | "string->list" | "list->string"
            | "char->integer" | "integer->char"
            | "eq?" | "eqv?" | "equal?"
            | "abs" | "modulo" | "remainder" | "quotient"
            | "min" | "max" | "expt"
            | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
            | "list-ref" | "list-tail" | "list?" | "assoc" | "map" | "for-each" | "reverse"
            | "char-alphabetic?" | "char-numeric?"
            | "char-upcase" | "char-downcase"
            | "char=?" | "char<?"
            | "string=?" | "string<?" | "string-ci=?"
            | "apply"
            | "call/cc"
            | "call-with-current-continuation"
            | "procedure?" | "vector?" | "integer?" | "rational?"
            | "make-vector" | "vector" | "vector-ref" | "vector-set!" | "vector-length"
            | "vector->list" | "list->vector" | "vector-fill!"
            | "append" | "member" | "filter"
            | "numerator" | "denominator" | "exact?" | "inexact?"
            | "exact->inexact" | "inexact->exact"
            | "floor" | "ceiling" | "truncate" | "round"
            | "gcd" | "lcm"
    )
}

fn expand_with_hygiene(
    template: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    pattern_vars: &HashSet<String>,
    def_env: &Env,
    span: Span,
    use_env: &Env,
) -> Result<Expansion, EvalError> {
    let mut renames: HashMap<String, String> = HashMap::new();
    collect_macro_symbols(template, pattern_vars, &mut renames, use_env);

    let hygiene_bindings: Vec<(String, Value)> = renames
        .iter()
        .filter_map(|(original, gensym)| {
            def_env.get(original).map(|val| (gensym.clone(), val))
        })
        .collect();

    let expr = expand_expr(template, bindings, &renames, span);
    Ok(Expansion {
        expr,
        hygiene_bindings,
    })
}

fn collect_macro_symbols(
    expr: &Expr,
    pattern_vars: &HashSet<String>,
    renames: &mut HashMap<String, String>,
    env: &Env,
) {
    match expr {
        Expr::Symbol(name, _) => {
            if !pattern_vars.contains(name)
                && !is_reserved(name)
                && name != "..."
                && !renames.contains_key(name)
            {
                let id = env.next_gensym();
                renames.insert(name.clone(), format!("__{name}_{id}"));
            }
        }
        Expr::List(elems, _) => {
            for elem in elems {
                collect_macro_symbols(elem, pattern_vars, renames, env);
            }
        }
        _ => {}
    }
}

fn expand_symbol(
    name: &str,
    s: Span,
    template: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    renames: &HashMap<String, String>,
) -> Expr {
    match bindings.get(name) {
        Some(PatternBinding::Single(expr)) => expr.clone(),
        Some(PatternBinding::Repeated(exprs)) => {
            exprs.first().cloned().unwrap_or(Expr::List(vec![], s))
        }
        None if renames.contains_key(name) => {
            Expr::Symbol(renames[name].clone(), s)
        }
        None => template.clone(),
    }
}

fn expand_expr(
    template: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    renames: &HashMap<String, String>,
    span: Span,
) -> Expr {
    match template {
        Expr::Symbol(name, s) => expand_symbol(name, *s, template, bindings, renames),
        Expr::List(elems, s) => {
            Expr::List(expand_list_elements(elems, bindings, renames, span), *s)
        }
        _ => template.clone(),
    }
}

fn expand_list_elements(
    elems: &[Expr],
    bindings: &HashMap<String, PatternBinding>,
    renames: &HashMap<String, String>,
    span: Span,
) -> Vec<Expr> {
    let mut result = Vec::new();
    let mut i = 0;

    while i < elems.len() {
        let has_ellipsis = i + 1 < elems.len()
            && matches!(&elems[i + 1], Expr::Symbol(s, _) if s == "...");

        if has_ellipsis {
            expand_ellipsis_element(&elems[i], bindings, renames, span, &mut result);
            i += 2;
        } else {
            result.push(expand_expr(&elems[i], bindings, renames, span));
            i += 1;
        }
    }

    result
}

fn expand_ellipsis_element(
    elem: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    renames: &HashMap<String, String>,
    span: Span,
    result: &mut Vec<Expr>,
) {
    // Simple case: element is a pattern variable with repeated bindings
    if let Expr::Symbol(name, _) = elem {
        if let Some(PatternBinding::Repeated(exprs)) = bindings.get(name) {
            result.extend(exprs.iter().cloned());
            return;
        }
    }

    // Complex case: sub-template containing repeated variables
    let repeated_vars = find_repeated_vars(elem, bindings);
    let Some(first_var) = repeated_vars.first() else {
        return;
    };
    let Some(PatternBinding::Repeated(exprs)) = bindings.get(first_var) else {
        return;
    };
    let count = exprs.len();
    for idx in 0..count {
        let indexed = create_indexed_bindings(bindings, &repeated_vars, idx);
        result.push(expand_expr(elem, &indexed, renames, span));
    }
}

fn find_repeated_vars(
    expr: &Expr,
    bindings: &HashMap<String, PatternBinding>,
) -> Vec<String> {
    let mut vars = Vec::new();
    find_repeated_vars_inner(expr, bindings, &mut vars);
    vars
}

fn find_repeated_vars_inner(
    expr: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    vars: &mut Vec<String>,
) {
    match expr {
        Expr::Symbol(name, _) => {
            if matches!(bindings.get(name), Some(PatternBinding::Repeated(_))) && !vars.contains(name)
            {
                vars.push(name.clone());
            }
        }
        Expr::List(elems, _) => {
            for elem in elems {
                find_repeated_vars_inner(elem, bindings, vars);
            }
        }
        _ => {}
    }
}

fn create_indexed_bindings(
    bindings: &HashMap<String, PatternBinding>,
    repeated_vars: &[String],
    idx: usize,
) -> HashMap<String, PatternBinding> {
    bindings
        .iter()
        .map(|(name, binding)| {
            let indexed = match binding {
                PatternBinding::Repeated(exprs)
                    if repeated_vars.contains(name) =>
                {
                    exprs
                        .get(idx)
                        .map(|expr| PatternBinding::Single(expr.clone()))
                        .unwrap_or_else(|| PatternBinding::Repeated(exprs.clone()))
                }
                PatternBinding::Single(e) => PatternBinding::Single(e.clone()),
                PatternBinding::Repeated(es) => PatternBinding::Repeated(es.clone()),
            };
            (name.clone(), indexed)
        })
        .collect()
}
