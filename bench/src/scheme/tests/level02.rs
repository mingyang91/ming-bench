use crate::scheme::eval_str;

// ===== Level 2: Arithmetic =====

#[test]
fn test_l02_add() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_add.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l02_sub() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_sub.scm").trim()),
        Ok("7".into())
    );
}

#[test]
fn test_l02_mul() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_mul.scm").trim()),
        Ok("20".into())
    );
}

#[test]
fn test_l02_div() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_div.scm").trim()),
        Ok("5".into())
    );
}

#[test]
fn test_l02_variadic_add() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_variadic_add.scm").trim()),
        Ok("10".into())
    );
}

#[test]
fn test_l02_unary_minus() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_unary_minus.scm").trim()),
        Ok("-10".into())
    );
}

#[test]
fn test_l02_nested_arith() {
    assert_eq!(
        eval_str(include_str!("fixtures/l02_nested_arith.scm").trim()),
        Ok("12".into())
    );
}
