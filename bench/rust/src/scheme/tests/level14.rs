use crate::scheme::eval_str;

// ===== Level 14: Immutable Strings (R7RS) =====

#[test]
fn test_l14_string_set_error() {
    let err = eval_str(include_str!("fixtures/l14_string_set_error.scm").trim());
    assert!(err.is_err(), "string-set! should error on immutable strings");
}

#[test]
fn test_l14_string_to_list() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_string_to_list.scm").trim()),
        Ok("(#\\h #\\e #\\l #\\l #\\o)".into())
    );
}

#[test]
fn test_l14_list_to_string() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_list_to_string.scm").trim()),
        Ok("\"hello\"".into())
    );
}

#[test]
fn test_l14_string_transform_via_list() {
    assert_eq!(
        eval_str(include_str!("fixtures/l14_string_transform_via_list.scm").trim()),
        Ok("\"HELLO\"".into())
    );
}
