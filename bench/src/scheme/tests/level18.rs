use crate::scheme::eval_str;

// ===== Level 18: values & call-with-values =====

#[test]
fn test_l18_values_basic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l18_values_basic.scm").trim()),
        Ok("6".into())
    );
}

#[test]
fn test_l18_values_single() {
    assert_eq!(
        eval_str(include_str!("fixtures/l18_values_single.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l18_values_receive() {
    assert_eq!(
        eval_str(include_str!("fixtures/l18_values_receive.scm").trim()),
        Ok("(30 20 10)".into())
    );
}

#[test]
fn test_l18_values_let() {
    assert_eq!(
        eval_str(include_str!("fixtures/l18_values_let.scm").trim()),
        Ok("600".into())
    );
}

#[test]
fn test_l18_values_zero() {
    assert_eq!(
        eval_str(include_str!("fixtures/l18_values_zero.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l18_values_compose() {
    assert_eq!(
        eval_str(include_str!("fixtures/l18_values_compose.scm").trim()),
        Ok("25".into())
    );
}
