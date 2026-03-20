use crate::scheme::eval_str;

// ===== Level 17: First-Class Continuations — call/cc =====

#[test]
fn test_l17_callcc_nonlocal_exit() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_callcc_nonlocal_exit.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l17_callcc_no_escape() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_callcc_no_escape.scm").trim()),
        Ok("7".into())
    );
}

#[test]
fn test_l17_callcc_early_return() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_callcc_early_return.scm").trim()),
        Ok("-2".into())
    );
}

#[test]
fn test_l17_callcc_saved_continuation() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_callcc_saved_continuation.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l17_callcc_as_value() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_callcc_as_value.scm").trim()),
        Ok("11".into())
    );
}

#[test]
fn test_l17_callcc_reentrant() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_callcc_reentrant.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l17_callcc_exception_handler() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_callcc_exception_handler.scm").trim()),
        Ok("(error 42)".into())
    );
}

#[test]
fn test_l17_callcc_is_first_class() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_callcc_is_first_class.scm").trim()),
        Ok("7".into())
    );
}

#[test]
fn test_l17_callcc_resumes_lambda_body() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_callcc_resumes_lambda_body.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l17_callcc_resumes_pending_application() {
    assert_eq!(
        eval_str(include_str!("fixtures/l17_callcc_resumes_pending_application.scm").trim()),
        Ok("43".into())
    );
}
