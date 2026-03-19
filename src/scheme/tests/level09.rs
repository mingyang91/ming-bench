use crate::scheme::eval_str;

// ===== Level 9: Strings & Type Predicates =====

#[test]
fn test_l09_string_pred() {
    assert_eq!(eval_str("(string? \"hello\")"), Ok("#t".into()));
}

#[test]
fn test_l09_number_pred() {
    assert_eq!(eval_str("(number? 42)"), Ok("#t".into()));
}

#[test]
fn test_l09_boolean_pred() {
    assert_eq!(eval_str("(boolean? #t)"), Ok("#t".into()));
}

#[test]
fn test_l09_pair_pred() {
    assert_eq!(eval_str("(pair? '(1 2))"), Ok("#t".into()));
}

#[test]
fn test_l09_symbol_pred() {
    assert_eq!(eval_str("(symbol? 'foo)"), Ok("#t".into()));
}
