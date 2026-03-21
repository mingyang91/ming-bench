use crate::scheme::eval_str;

// ===== Level 17: guard, raise & with-exception-handler =====

#[test]
fn test_l17_raise_basic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_raise_basic.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l17_guard_string() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_guard_string.scm").trim()),
        Ok("\"caught: boom\"".into())
    );
}

#[test]
fn test_l17_guard_else() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_guard_else.scm").trim()),
        Ok("other".into())
    );
}

#[test]
fn test_l17_guard_no_raise() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_guard_no_raise.scm").trim()),
        Ok("30".into())
    );
}

#[test]
fn test_l17_with_exception_handler() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_with_exception_handler.scm").trim()),
        Ok("142".into())
    );
}

#[test]
fn test_l17_guard_with_dynamic_wind() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_guard_with_dynamic_wind.scm").trim()),
        Ok("(caught fail (in body out))".into())
    );
}
