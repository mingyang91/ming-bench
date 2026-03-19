use crate::scheme::eval_str;

// ===== Level 1: Atoms =====

#[test]
fn test_l01_integer() {
    assert_eq!(eval_str("42"), Ok("42".into()));
}

#[test]
fn test_l01_negative_integer() {
    assert_eq!(eval_str("-7"), Ok("-7".into()));
}

#[test]
fn test_l01_true() {
    assert_eq!(eval_str("#t"), Ok("#t".into()));
}

#[test]
fn test_l01_false() {
    assert_eq!(eval_str("#f"), Ok("#f".into()));
}

#[test]
fn test_l01_string() {
    assert_eq!(eval_str("\"hello\""), Ok("\"hello\"".into()));
}
