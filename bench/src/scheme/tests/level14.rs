use crate::scheme::eval_str;

// ===== Level 14: set! and Mutation =====

#[test]
fn test_l14_set_basic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_set_basic.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l14_set_unbound_error() {
    assert!(eval_str(include_str!("fixtures/l14_set_unbound_error.scm").trim()).is_err());
}

#[test]
fn test_l14_counter() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_counter.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l14_shared_state() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_shared_state.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l14_set_in_loop() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_set_in_loop.scm").trim()),
        Ok("55".into())
    );
}
