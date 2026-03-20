use crate::scheme::eval_str;

// ===== Level 28: List Utilities =====

#[test]
fn test_l28_list_ref() {
    assert_eq!(
        eval_str(include_str!("fixtures/l28_list_ref.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l28_list_tail() {
    assert_eq!(
        eval_str(include_str!("fixtures/l28_list_tail.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l28_list_pred() {
    assert_eq!(
        eval_str(include_str!("fixtures/l28_list_pred.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l28_assoc() {
    assert_eq!(
        eval_str(include_str!("fixtures/l28_assoc.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l28_map_multi() {
    assert_eq!(
        eval_str(include_str!("fixtures/l28_map_multi.scm").trim()),
        Ok("#t".into())
    );
}
