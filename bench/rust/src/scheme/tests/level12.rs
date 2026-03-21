use crate::scheme::eval_str;

// ===== Level 12: Integration =====

#[test]
fn test_l12_callcc_with_mutation() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_callcc_with_mutation.scm").trim()),
        Ok("4".into())
    );
}

#[test]
fn test_l12_macro_tco_loop() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_macro_tco_loop.scm").trim()),
        Ok("1000000".into())
    );
}

#[test]
fn test_l12_callcc_try_catch() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_callcc_try_catch.scm").trim()),
        Ok("(caught 42)".into())
    );
}

#[test]
fn test_l12_coroutine_scheduler() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_coroutine_scheduler.scm").trim()),
        Ok("4".into())
    );
}

#[test]
fn test_l12_church_booleans_with_callcc() {
    assert_eq!(
        eval_str(include_str!("fixtures/l12_church_booleans_with_callcc.scm").trim()),
        Ok("yes".into())
    );
}
