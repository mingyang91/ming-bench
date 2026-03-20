use crate::scheme::eval_str;

// ===== Level 19: Comprehensive Integration =====

#[test]
fn test_l19_callcc_with_mutation() {
    assert_eq!(
        eval_str(include_str!("fixtures/l19_callcc_with_mutation.scm").trim()),
        Ok("4".into())
    );
}

#[test]
fn test_l19_macro_tco_loop() {
    assert_eq!(
        eval_str(include_str!("fixtures/l19_macro_tco_loop.scm").trim()),
        Ok("1000000".into())
    );
}

#[test]
fn test_l19_callcc_try_catch() {
    assert_eq!(
        eval_str(include_str!("fixtures/l19_callcc_try_catch.scm").trim()),
        Ok("(caught 42)".into())
    );
}

#[test]
fn test_l19_coroutine_scheduler() {
    assert_eq!(
        eval_str(include_str!("fixtures/l19_coroutine_scheduler.scm").trim()),
        Ok("4".into())
    );
}

#[test]
fn test_l19_church_booleans_with_callcc() {
    assert_eq!(
        eval_str(include_str!("fixtures/l19_church_booleans_with_callcc.scm").trim()),
        Ok("yes".into())
    );
}
