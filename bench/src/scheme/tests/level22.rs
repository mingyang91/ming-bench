use crate::scheme::eval_str;

// ===== Level 22: Deep Equality =====

#[test]
fn test_l22_equal_numbers() {
    assert_eq!(
        eval_str(include_str!("fixtures/l22_equal_numbers.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l22_equal_strings() {
    assert_eq!(
        eval_str(include_str!("fixtures/l22_equal_strings.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l22_equal_nested_lists() {
    assert_eq!(
        eval_str(include_str!("fixtures/l22_equal_nested_lists.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l22_equal_different_types() {
    assert_eq!(
        eval_str(include_str!("fixtures/l22_equal_different_types.scm").trim()),
        Ok("#f".into())
    );
}
