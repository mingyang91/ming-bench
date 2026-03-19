use crate::scheme::eval_str;

// ===== Level 1: Atoms =====

#[test]
fn test_l01_integer() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_integer.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l01_negative_integer() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_negative_integer.scm").trim()),
        Ok("-7".into())
    );
}

#[test]
fn test_l01_true() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_true.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l01_false() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_false.scm").trim()),
        Ok("#f".into())
    );
}

#[test]
fn test_l01_string() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_string.scm").trim()),
        Ok("\"hello\"".into())
    );
}
