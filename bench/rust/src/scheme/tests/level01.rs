use crate::scheme::eval_str;

// ===== Level 1: Atoms, Arithmetic & Comparisons =====

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

#[test]
fn test_l01_add() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_add.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l01_sub() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_sub.scm").trim()),
        Ok("7".into())
    );
}

#[test]
fn test_l01_mul() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_mul.scm").trim()),
        Ok("20".into())
    );
}

#[test]
fn test_l01_div() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_div.scm").trim()),
        Ok("5".into())
    );
}

#[test]
fn test_l01_variadic_add() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_variadic_add.scm").trim()),
        Ok("10".into())
    );
}

#[test]
fn test_l01_unary_minus() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_unary_minus.scm").trim()),
        Ok("-10".into())
    );
}

#[test]
fn test_l01_nested_arith() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_nested_arith.scm").trim()),
        Ok("12".into())
    );
}

#[test]
fn test_l01_less_than() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_less_than.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l01_greater_than() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_greater_than.scm").trim()),
        Ok("#f".into())
    );
}

#[test]
fn test_l01_equal() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_equal.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l01_less_equal() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_less_equal.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l01_not() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_not.scm").trim()),
        Ok("#f".into())
    );
}

#[test]
fn test_l01_and() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_and.scm").trim()),
        Ok("#f".into())
    );
}

#[test]
fn test_l01_or() {
    assert_eq!(
        eval_str(include_str!("fixtures/l01_or.scm").trim()),
        Ok("5".into())
    );
}
