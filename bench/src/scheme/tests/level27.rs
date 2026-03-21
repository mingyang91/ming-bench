use crate::scheme::eval_str;

// ===== Level 27: Numeric Predicates =====

#[test]
fn test_l27_zero() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_zero.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l27_positive_negative() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_positive_negative.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l27_odd_even() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_odd_even.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l27_combined() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_combined.scm").trim()),
        Ok("#t".into())
    );
}
