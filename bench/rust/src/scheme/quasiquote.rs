use super::{Ast, AstKind};

pub(crate) fn qq_sym(name: &str) -> Ast {
    Ast { kind: AstKind::Symbol(name.into()), line: 0, col: 0 }
}

fn qq_wrap_quote(ast: &Ast) -> Ast {
    Ast {
        kind: AstKind::List(vec![qq_sym("quote"), ast.clone()]),
        line: 0, col: 0,
    }
}

/// Expand a quasiquote template into an expression using cons/list/append/quote.
pub(crate) fn qq_expand(tmpl: &Ast) -> Ast {
    match &tmpl.kind {
        AstKind::List(items) if items.len() == 2
            && matches!(&items[0].kind, AstKind::Symbol(s) if s == "unquote") =>
        {
            // ,expr → expr (evaluate it)
            items[1].clone()
        }
        AstKind::List(items) => {
            // Check for dotted pair notation: items contains "." as second-to-last
            let dot_pos = items.iter().position(|it| matches!(&it.kind, AstKind::Symbol(s) if s == "."));
            if let Some(dp) = dot_pos {
                if dp + 2 == items.len() && dp > 0 {
                    // (a b . c) — expand items before dot, then use tail as cdr
                    let tail = qq_expand(&items[dp + 1]);
                    let mut result = tail;
                    for item in items[..dp].iter().rev() {
                        let expanded_item = qq_expand_list_element(item);
                        // If it's a splicing form, use append; otherwise cons
                        if is_unquote_splicing(item) {
                            result = Ast {
                                kind: AstKind::List(vec![qq_sym("append"), expanded_item, result]),
                                line: 0, col: 0,
                            };
                        } else {
                            result = Ast {
                                kind: AstKind::List(vec![qq_sym("cons"), expanded_item, result]),
                                line: 0, col: 0,
                            };
                        }
                    }
                    return result;
                }
            }
            // Regular list — build using append of list segments and splices
            qq_expand_list(items)
        }
        _ => qq_wrap_quote(tmpl),
    }
}

fn is_unquote_splicing(ast: &Ast) -> bool {
    matches!(&ast.kind, AstKind::List(items) if items.len() == 2
        && matches!(&items[0].kind, AstKind::Symbol(s) if s == "unquote-splicing"))
}

fn qq_expand_list_element(item: &Ast) -> Ast {
    if is_unquote_splicing(item) {
        if let AstKind::List(items) = &item.kind {
            return items[1].clone();
        }
    }
    qq_expand(item)
}

fn qq_expand_list(items: &[Ast]) -> Ast {
    // Group consecutive non-splice items into (list ...) calls,
    // splice items become direct arguments to append
    let mut segments: Vec<Ast> = Vec::new();
    let mut current_list: Vec<Ast> = Vec::new();

    for item in items {
        if is_unquote_splicing(item) {
            if !current_list.is_empty() {
                let mut list_call = vec![qq_sym("list")];
                list_call.append(&mut current_list);
                segments.push(Ast { kind: AstKind::List(list_call), line: 0, col: 0 });
            }
            if let AstKind::List(sub) = &item.kind {
                segments.push(sub[1].clone());
            }
        } else {
            current_list.push(qq_expand(item));
        }
    }
    if !current_list.is_empty() {
        let mut list_call = vec![qq_sym("list")];
        list_call.append(&mut current_list);
        segments.push(Ast { kind: AstKind::List(list_call), line: 0, col: 0 });
    }

    if segments.is_empty() {
        Ast { kind: AstKind::List(vec![qq_sym("quote"), Ast { kind: AstKind::List(vec![]), line: 0, col: 0 }]), line: 0, col: 0 }
    } else if segments.len() == 1 && !items.iter().any(is_unquote_splicing) {
        segments.into_iter().next().expect("segments is non-empty")
    } else {
        let mut append_call = vec![qq_sym("append")];
        append_call.extend(segments);
        Ast { kind: AstKind::List(append_call), line: 0, col: 0 }
    }
}
