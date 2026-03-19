use crate::scheme::eval_str;

// ===== Level 3: Comparisons & Boolean Ops =====

#[test]
fn test_l03_less_than() {
    assert_eq!(eval_str("(< 1 2)"), Ok("#t".into()));
}

#[test]
fn test_l03_greater_than() {
    assert_eq!(eval_str("(> 1 2)"), Ok("#f".into()));
}

#[test]
fn test_l03_equal() {
    assert_eq!(eval_str("(= 3 3)"), Ok("#t".into()));
}

#[test]
fn test_l03_less_equal() {
    assert_eq!(eval_str("(<= 2 2)"), Ok("#t".into()));
}

#[test]
fn test_l03_not() {
    assert_eq!(eval_str("(not #t)"), Ok("#f".into()));
}

#[test]
fn test_l03_and() {
    assert_eq!(eval_str("(and #t #t #f)"), Ok("#f".into()));
}

#[test]
fn test_l03_or() {
    assert_eq!(eval_str("(or #f #f 5)"), Ok("5".into()));
}
