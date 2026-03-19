use crate::scheme::eval_str;

// ===== Level 3: Comparisons & Boolean Ops =====

#[test]
fn test_l03_less_than() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_less_than.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l03_greater_than() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_greater_than.scm").trim()),
        Ok("#f".into())
    );
}

#[test]
fn test_l03_equal() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_equal.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l03_less_equal() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_less_equal.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l03_not() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_not.scm").trim()),
        Ok("#f".into())
    );
}

#[test]
fn test_l03_and() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_and.scm").trim()),
        Ok("#f".into())
    );
}

#[test]
fn test_l03_or() {
    assert_eq!(
        eval_str(include_str!("fixtures/l03_or.scm").trim()),
        Ok("5".into())
    );
}
