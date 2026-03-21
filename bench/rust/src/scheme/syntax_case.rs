use std::collections::{HashMap, HashSet};

use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::parser::Expr;
use crate::scheme::value::Value;

/// A binding from syntax-case pattern matching.
enum SyntaxBinding {
    Single(Value),
    Repeated(Vec<Value>),
}

/// Convert a parsed Expr to a Value (syntax object).
pub fn expr_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Symbol(s, _) => Value::Symbol(s.clone()),
        Expr::Integer(n, _) => Value::Integer(*n),
        Expr::Rational(n, d, _) => Value::Rational(*n, *d),
        Expr::Float(x, _) => Value::Float(*x),
        Expr::Boolean(b, _) => Value::Boolean(*b),
        Expr::String(s, _) => Value::String(s.clone()),
        Expr::Char(c, _) => Value::Char(*c),
        Expr::List(elems, _) => Value::make_list(elems.iter().map(expr_to_value).collect()),
    }
}

/// Convert a Value back to an Expr for evaluation.
pub fn value_to_expr(val: &Value, span: Span) -> Result<Expr, EvalError> {
    match val {
        Value::Symbol(s) => Ok(Expr::Symbol(s.clone(), span)),
        Value::Integer(n) => Ok(Expr::Integer(*n, span)),
        Value::Rational(n, d) => Ok(Expr::Rational(*n, *d, span)),
        Value::Float(x) => Ok(Expr::Float(*x, span)),
        Value::Boolean(b) => Ok(Expr::Boolean(*b, span)),
        Value::String(s) => Ok(Expr::String(s.clone(), span)),
        Value::Char(c) => Ok(Expr::Char(*c, span)),
        Value::List(items) => {
            let exprs: Vec<Expr> = items
                .iter()
                .map(|v| value_to_expr(v, span))
                .collect::<Result<_, _>>()?;
            Ok(Expr::List(exprs, span))
        }
        Value::Pair(_) => {
            let items = val.collect_list().ok_or_else(|| {
                EvalError::TypeError {
                    expected: "proper list".into(),
                    got: format!("{val}"),
                }
            })?;
            let exprs: Vec<Expr> = items
                .iter()
                .map(|v| value_to_expr(v, span))
                .collect::<Result<_, _>>()?;
            Ok(Expr::List(exprs, span))
        }
        _ => Err(EvalError::TypeError {
            expected: "syntax object".into(),
            got: format!("{val}"),
        }),
    }
}

/// Evaluate a syntax-case form: (syntax-case stx (literals) clause ...)
pub fn eval_syntax_case(
    args: &[Expr],
    span: Span,
    env: &Env,
    eval_fn: fn(&Expr, &Env) -> Result<Value, EvalError>,
) -> Result<Value, EvalError> {
    let [ref stx_expr, Expr::List(ref lit_exprs, _), ref clauses @ ..] = args else {
        return Err(EvalError::Parse("invalid syntax-case form".into()).at(span));
    };

    let stx_val = eval_fn(stx_expr, env)?;

    let literals: Vec<String> = lit_exprs
        .iter()
        .map(|e| match e {
            Expr::Symbol(s, _) => Ok(s.clone()),
            _ => Err(EvalError::Parse("literal must be a symbol".into()).at(span)),
        })
        .collect::<Result<_, _>>()?;

    for clause in clauses {
        let Expr::List(ref parts, _) = clause else {
            return Err(
                EvalError::Parse("syntax-case clause must be a list".into()).at(span),
            );
        };
        let (pattern, fender, template) = match parts.as_slice() {
            [Expr::List(ref pat, _), ref tmpl] => (pat, None, tmpl),
            [Expr::List(ref pat, _), ref fender, ref tmpl] => (pat, Some(fender), tmpl),
            _ => {
                return Err(
                    EvalError::Parse("invalid syntax-case clause".into()).at(span),
                )
            }
        };

        let Some(bindings) = match_value_pattern(pattern, &stx_val, &literals) else {
            continue;
        };
        let child_env = Env::extend(env);
        let (pattern_vars, repeated_vars) =
            bind_syntax_vars(&bindings, &child_env);
        store_syntax_meta(&child_env, &pattern_vars, &repeated_vars);

        let fender_failed = fender
            .map(|f| eval_fn(f, &child_env))
            .transpose()?
            .is_some_and(|v| !v.is_truthy());
        if fender_failed {
            continue;
        }

        return eval_fn(template, &child_env);
    }

    Err(EvalError::Parse("no matching syntax-case pattern".into()).at(span))
}

/// Evaluate a (syntax template) form — construct expansion with hygiene.
pub fn eval_syntax(
    args: &[Expr],
    span: Span,
    env: &Env,
) -> Result<Value, EvalError> {
    let [ref template] = args else {
        return Err(EvalError::Parse("syntax requires one argument".into()).at(span));
    };

    let pattern_vars = get_pattern_vars(env);
    let repeated_vars = get_repeated_vars(env);

    let mut renames: HashMap<String, String> = HashMap::new();
    collect_syntax_renames(template, &pattern_vars, &mut renames, env);

    for (original, gensym) in &renames {
        if let Some(val) = lookup_for_hygiene(original, env) {
            env.add_syntax_hygiene(gensym.clone(), val);
        }
    }

    expand_syntax_template(template, &pattern_vars, &repeated_vars, &renames, env, span)
}

/// Evaluate (with-syntax ((pat expr) ...) body ...)
pub fn eval_with_syntax(
    args: &[Expr],
    span: Span,
    env: &Env,
    eval_fn: fn(&Expr, &Env) -> Result<Value, EvalError>,
) -> Result<Value, EvalError> {
    let [Expr::List(ref binding_list, _), ref body @ ..] = args else {
        return Err(EvalError::Parse("invalid with-syntax form".into()).at(span));
    };

    let child_env = Env::extend(env);
    let mut pattern_vars = get_pattern_vars(env);
    let repeated_vars = get_repeated_vars(env);

    for binding in binding_list {
        let Expr::List(ref parts, _) = binding else {
            return Err(
                EvalError::Parse("with-syntax binding must be a list".into()).at(span),
            );
        };
        let [ref pat, ref expr] = parts.as_slice() else {
            return Err(
                EvalError::Parse("with-syntax binding must be (pat expr)".into()).at(span),
            );
        };
        let val = eval_fn(expr, &child_env)?;
        if let Expr::Symbol(ref name, _) = pat {
            child_env.define(name.clone(), val);
            pattern_vars.insert(name.clone());
        }
    }

    store_syntax_meta(&child_env, &pattern_vars, &repeated_vars);

    let mut result = Value::Void;
    for expr in body {
        result = eval_fn(expr, &child_env)?;
    }
    Ok(result)
}

// ─── Pattern matching on Values ────────────────────────────────────

fn match_value_pattern(
    pattern: &[Expr],
    value: &Value,
    literals: &[String],
) -> Option<HashMap<String, SyntaxBinding>> {
    let items = value.collect_list()?;
    let mut bindings = HashMap::new();
    match_value_elements(pattern, &items, literals, &mut bindings)?;
    Some(bindings)
}

fn match_value_elements(
    pattern: &[Expr],
    items: &[Value],
    literals: &[String],
    bindings: &mut HashMap<String, SyntaxBinding>,
) -> Option<()> {
    let mut pi = 0;
    let mut vi = 0;

    while pi < pattern.len() {
        let has_ellipsis = pi + 1 < pattern.len()
            && matches!(&pattern[pi + 1], Expr::Symbol(s, _) if s == "...");

        if has_ellipsis {
            let remaining = pattern.len() - pi - 2;
            let available = items.len().checked_sub(vi + remaining)?;
            match_value_ellipsis(&pattern[pi], &items[vi..vi + available], literals, bindings)?;
            vi += available;
            pi += 2;
        } else if vi >= items.len() {
            return None;
        } else {
            match_value_single(&pattern[pi], &items[vi], literals, bindings)?;
            vi += 1;
            pi += 1;
        }
    }

    (vi == items.len()).then_some(())
}

fn match_value_single(
    pattern: &Expr,
    value: &Value,
    literals: &[String],
    bindings: &mut HashMap<String, SyntaxBinding>,
) -> Option<()> {
    match pattern {
        Expr::Symbol(name, _) if name == "_" => Some(()),
        Expr::Symbol(name, _) if literals.contains(name) => {
            matches!(value, Value::Symbol(s) if s == name).then_some(())
        }
        Expr::Symbol(name, _) => {
            bindings.insert(name.clone(), SyntaxBinding::Single(value.clone()));
            Some(())
        }
        Expr::List(sub_pat, _) => {
            let items = value.collect_list()?;
            match_value_elements(sub_pat, &items, literals, bindings)
        }
        Expr::Integer(n, _) => matches!(value, Value::Integer(m) if m == n).then_some(()),
        Expr::Boolean(b, _) => matches!(value, Value::Boolean(c) if c == b).then_some(()),
        Expr::String(s, _) => matches!(value, Value::String(t) if t == s).then_some(()),
        _ => None,
    }
}

fn match_value_ellipsis(
    pattern: &Expr,
    values: &[Value],
    literals: &[String],
    bindings: &mut HashMap<String, SyntaxBinding>,
) -> Option<()> {
    match pattern {
        Expr::Symbol(name, _) if !literals.contains(name) && name != "_" => {
            bindings.insert(name.clone(), SyntaxBinding::Repeated(values.to_vec()));
            Some(())
        }
        _ => {
            for val in values {
                let mut dummy = HashMap::new();
                match_value_single(pattern, val, literals, &mut dummy)?;
            }
            Some(())
        }
    }
}

// ─── Binding helpers ───────────────────────────────────────────────

fn bind_syntax_vars(
    bindings: &HashMap<String, SyntaxBinding>,
    env: &Env,
) -> (HashSet<String>, HashSet<String>) {
    let mut pattern_vars = HashSet::new();
    let mut repeated_vars = HashSet::new();
    for (name, binding) in bindings {
        pattern_vars.insert(name.clone());
        match binding {
            SyntaxBinding::Single(val) => env.define(name.clone(), val.clone()),
            SyntaxBinding::Repeated(vals) => {
                repeated_vars.insert(name.clone());
                env.define(name.clone(), Value::make_list(vals.clone()));
            }
        }
    }
    (pattern_vars, repeated_vars)
}

fn store_syntax_meta(env: &Env, pattern_vars: &HashSet<String>, repeated_vars: &HashSet<String>) {
    env.define(
        "__syntax_pattern_vars__".to_string(),
        Value::List(pattern_vars.iter().map(|n| Value::Symbol(n.clone())).collect()),
    );
    env.define(
        "__syntax_repeated_vars__".to_string(),
        Value::List(repeated_vars.iter().map(|n| Value::Symbol(n.clone())).collect()),
    );
}

fn get_pattern_vars(env: &Env) -> HashSet<String> {
    extract_symbol_set(env, "__syntax_pattern_vars__")
}

fn get_repeated_vars(env: &Env) -> HashSet<String> {
    extract_symbol_set(env, "__syntax_repeated_vars__")
}

fn extract_symbol_set(env: &Env, key: &str) -> HashSet<String> {
    env.get(key)
        .and_then(|v| match v {
            Value::List(items) => Some(
                items
                    .iter()
                    .filter_map(|item| match item {
                        Value::Symbol(s) => Some(s.clone()),
                        _ => None,
                    })
                    .collect(),
            ),
            _ => None,
        })
        .unwrap_or_default()
}

// ─── Template expansion with hygiene ───────────────────────────────

fn collect_syntax_renames(
    template: &Expr,
    pattern_vars: &HashSet<String>,
    renames: &mut HashMap<String, String>,
    env: &Env,
) {
    match template {
        Expr::Symbol(name, _)
            if !pattern_vars.contains(name)
                && !is_reserved_syntax(name)
                && name != "..."
                && !renames.contains_key(name) =>
        {
            let id = env.next_gensym();
            renames.insert(name.clone(), format!("__{name}_{id}"));
        }
        Expr::List(elems, _) => {
            // Don't rename inside quote
            if matches!(elems.first(), Some(Expr::Symbol(s, _)) if s == "quote") {
                return;
            }
            elems
                .iter()
                .filter(|e| !matches!(e, Expr::Symbol(s, _) if s == "..."))
                .for_each(|e| collect_syntax_renames(e, pattern_vars, renames, env));
        }
        _ => {}
    }
}

fn expand_syntax_template(
    template: &Expr,
    pattern_vars: &HashSet<String>,
    repeated_vars: &HashSet<String>,
    renames: &HashMap<String, String>,
    env: &Env,
    span: Span,
) -> Result<Value, EvalError> {
    match template {
        Expr::Symbol(name, _) if pattern_vars.contains(name) => env
            .get(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }.at(span)),
        Expr::Symbol(name, _) if renames.contains_key(name) => {
            Ok(Value::Symbol(renames[name].clone()))
        }
        Expr::Symbol(name, _) => Ok(Value::Symbol(name.clone())),
        Expr::List(elems, _) => {
            if is_quote_form(elems) {
                let datum = expand_syntax_quoted(&elems[1], pattern_vars, env);
                return Ok(Value::make_list(vec![
                    Value::Symbol("quote".to_string()),
                    datum,
                ]));
            }
            let items =
                expand_syntax_list(elems, pattern_vars, repeated_vars, renames, env, span)?;
            Ok(Value::make_list(items))
        }
        Expr::Integer(n, _) => Ok(Value::Integer(*n)),
        Expr::Rational(n, d, _) => Ok(Value::Rational(*n, *d)),
        Expr::Float(x, _) => Ok(Value::Float(*x)),
        Expr::Boolean(b, _) => Ok(Value::Boolean(*b)),
        Expr::String(s, _) => Ok(Value::String(s.clone())),
        Expr::Char(c, _) => Ok(Value::Char(*c)),
    }
}

fn is_quote_form(elems: &[Expr]) -> bool {
    elems.len() == 2 && matches!(&elems[0], Expr::Symbol(s, _) if s == "quote")
}

fn expand_syntax_quoted(
    expr: &Expr,
    pattern_vars: &HashSet<String>,
    env: &Env,
) -> Value {
    match expr {
        Expr::Symbol(name, _) if pattern_vars.contains(name) => {
            env.get(name).unwrap_or_else(|| Value::Symbol(name.clone()))
        }
        Expr::List(elems, _) => Value::make_list(
            elems
                .iter()
                .map(|e| expand_syntax_quoted(e, pattern_vars, env))
                .collect(),
        ),
        other => expr_to_value(other),
    }
}

fn expand_syntax_list(
    elems: &[Expr],
    pattern_vars: &HashSet<String>,
    repeated_vars: &HashSet<String>,
    renames: &HashMap<String, String>,
    env: &Env,
    span: Span,
) -> Result<Vec<Value>, EvalError> {
    let mut result = Vec::new();
    let mut i = 0;

    while i < elems.len() {
        let has_ellipsis = i + 1 < elems.len()
            && matches!(&elems[i + 1], Expr::Symbol(s, _) if s == "...");

        if has_ellipsis {
            expand_syntax_ellipsis(
                &elems[i],
                pattern_vars,
                repeated_vars,
                renames,
                env,
                span,
                &mut result,
            )?;
            i += 2;
        } else {
            result.push(expand_syntax_template(
                &elems[i],
                pattern_vars,
                repeated_vars,
                renames,
                env,
                span,
            )?);
            i += 1;
        }
    }

    Ok(result)
}

fn expand_syntax_ellipsis(
    elem: &Expr,
    pattern_vars: &HashSet<String>,
    repeated_vars: &HashSet<String>,
    renames: &HashMap<String, String>,
    env: &Env,
    span: Span,
    result: &mut Vec<Value>,
) -> Result<(), EvalError> {
    let Some((var_name, values)) =
        find_repeated_var_in_template(elem, pattern_vars, repeated_vars, env)
    else {
        return Ok(());
    };

    for val in &values {
        let child_env = Env::extend(env);
        child_env.define(var_name.clone(), val.clone());
        result.push(expand_syntax_template(
            elem,
            pattern_vars,
            repeated_vars,
            renames,
            &child_env,
            span,
        )?);
    }
    Ok(())
}

fn find_repeated_var_in_template(
    elem: &Expr,
    pattern_vars: &HashSet<String>,
    repeated_vars: &HashSet<String>,
    env: &Env,
) -> Option<(String, Vec<Value>)> {
    match elem {
        Expr::Symbol(name, _)
            if pattern_vars.contains(name) && repeated_vars.contains(name) =>
        {
            let val = env.get(name)?;
            let list = val.collect_list()?;
            Some((name.clone(), list))
        }
        Expr::List(sub_elems, _) => sub_elems
            .iter()
            .find_map(|sub| find_repeated_var_in_template(sub, pattern_vars, repeated_vars, env)),
        _ => None,
    }
}

// ─── Hygiene: look up symbol for binding ───────────────────────────

fn lookup_for_hygiene(name: &str, env: &Env) -> Option<Value> {
    if let Some(val) = env.get(name) {
        return Some(val);
    }
    if is_builtin_name(name) {
        return Some(Value::Builtin(name.to_string()));
    }
    None
}

// ─── Reserved identifiers (not renamed during hygiene) ─────────────

fn is_reserved_syntax(name: &str) -> bool {
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
            | "string-upcase" | "string-downcase"
            | "vector" | "make-vector" | "vector-ref" | "vector-set!"
            | "vector-length" | "vector?" | "vector->list" | "list->vector"
            | "procedure?" | "integer?" | "rational?"
            | "exact?" | "inexact?" | "exact->inexact" | "inexact->exact"
            | "numerator" | "denominator"
            | "values" | "call-with-values"
            | "apply" | "call/cc" | "call-with-current-continuation"
            | "syntax->datum" | "datum->syntax"
    )
}

fn is_builtin_name(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
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
            | "string-upcase" | "string-downcase"
            | "vector" | "make-vector" | "vector-ref" | "vector-set!"
            | "vector-length" | "vector?" | "vector->list" | "list->vector"
            | "procedure?" | "integer?" | "rational?"
            | "exact?" | "inexact?" | "exact->inexact" | "inexact->exact"
            | "numerator" | "denominator"
            | "values" | "call-with-values"
            | "apply" | "call/cc" | "call-with-current-continuation"
            | "syntax->datum" | "datum->syntax"
    )
}
