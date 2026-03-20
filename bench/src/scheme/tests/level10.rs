use crate::scheme::eval_str;

// ===== Level 10: Error Quality =====
// All error messages must include source position info (line:col).

/// Check that an error message contains position info (digit followed by colon).
fn has_position_info(msg: &str) -> bool {
    msg.as_bytes()
        .windows(2)
        .any(|w| w[0].is_ascii_digit() && w[1] == b':')
}

#[test]
fn test_l10_error_undefined_var() {
    let err = eval_str(include_str!("fixtures/l10_error_undefined_var.scm").trim()).unwrap_err();
    assert!(
        has_position_info(&err.to_string()),
        "error should contain position info: {err}"
    );
}

#[test]
fn test_l10_error_wrong_arg_count() {
    let err = eval_str(include_str!("fixtures/l10_error_wrong_arg_count.scm").trim()).unwrap_err();
    assert!(
        has_position_info(&err.to_string()),
        "error should contain position info: {err}"
    );
}

#[test]
fn test_l10_error_type_mismatch() {
    let err = eval_str(include_str!("fixtures/l10_error_type_mismatch.scm").trim()).unwrap_err();
    assert!(
        has_position_info(&err.to_string()),
        "error should contain position info: {err}"
    );
}

#[test]
fn test_l10_error_syntax() {
    let err = eval_str(include_str!("fixtures/l10_error_syntax.scm").trim()).unwrap_err();
    assert!(
        has_position_info(&err.to_string()),
        "error should contain position info: {err}"
    );
}

#[test]
fn test_l10_error_division_by_zero() {
    let err = eval_str(include_str!("fixtures/l10_error_division_by_zero.scm").trim()).unwrap_err();
    assert!(
        has_position_info(&err.to_string()),
        "error should contain position info: {err}"
    );
}

#[test]
fn test_l10_error_not_a_procedure() {
    let err = eval_str(include_str!("fixtures/l10_error_not_a_procedure.scm").trim()).unwrap_err();
    assert!(
        has_position_info(&err.to_string()),
        "error should contain position info: {err}"
    );
}
