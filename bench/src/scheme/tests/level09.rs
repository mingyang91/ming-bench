use crate::scheme::eval_str;

// ===== Level 9: Strings & Type Predicates =====

#[test]
fn test_l09_string_pred() {
    assert_eq!(
        eval_str(include_str!("fixtures/l09_string_pred.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l09_number_pred() {
    assert_eq!(
        eval_str(include_str!("fixtures/l09_number_pred.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l09_boolean_pred() {
    assert_eq!(
        eval_str(include_str!("fixtures/l09_boolean_pred.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l09_pair_pred() {
    assert_eq!(
        eval_str(include_str!("fixtures/l09_pair_pred.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l09_symbol_pred() {
    assert_eq!(
        eval_str(include_str!("fixtures/l09_symbol_pred.scm").trim()),
        Ok("#t".into())
    );
}
