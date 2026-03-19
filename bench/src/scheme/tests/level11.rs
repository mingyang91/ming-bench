use crate::scheme::eval_str;

// ===== Level 11: set! and Mutation =====

#[test]
fn test_l11_set_basic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l11_set_basic.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l11_set_unbound_error() {
    assert!(eval_str(include_str!("fixtures/l11_set_unbound_error.scm").trim()).is_err());
}

#[test]
fn test_l11_counter() {
    assert_eq!(
        eval_str(include_str!("fixtures/l11_counter.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l11_shared_state() {
    assert_eq!(
        eval_str(include_str!("fixtures/l11_shared_state.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l11_set_in_loop() {
    assert_eq!(
        eval_str(include_str!("fixtures/l11_set_in_loop.scm").trim()),
        Ok("55".into())
    );
}
