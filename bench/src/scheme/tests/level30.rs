use crate::scheme::eval_str;

// ===== Level 30: String Comparison and Case Operations =====

#[test]
fn test_l30_string_equal() {
    assert_eq!(
        eval_str(include_str!("fixtures/l30_string_equal.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l30_string_less() {
    assert_eq!(
        eval_str(include_str!("fixtures/l30_string_less.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l30_string_upcase() {
    assert_eq!(
        eval_str(include_str!("fixtures/l30_string_upcase.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l30_string_downcase() {
    assert_eq!(
        eval_str(include_str!("fixtures/l30_string_downcase.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l30_string_ci() {
    assert_eq!(
        eval_str(include_str!("fixtures/l30_string_ci.scm").trim()),
        Ok("#t".into())
    );
}
