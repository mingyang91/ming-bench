use crate::scheme::eval_str;

// ===== Level 22: syntax-case =====

#[test]
fn test_l22_syntax_case_basic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l22_syntax_case_basic.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l22_syntax_case_with_guard() {
    assert_eq!(
        eval_str(include_str!("fixtures/l22_syntax_case_with_guard.scm").trim()),
        Ok("(5 division-by-zero)".into())
    );
}

#[test]
fn test_l22_syntax_case_hygiene() {
    assert_eq!(
        eval_str(include_str!("fixtures/l22_syntax_case_hygiene.scm").trim()),
        Ok("(2 1 999)".into())
    );
}

#[test]
fn test_l22_syntax_case_datum() {
    assert_eq!(
        eval_str(include_str!("fixtures/l22_syntax_case_datum.scm").trim()),
        Ok("15".into())
    );
}

#[test]
fn test_l22_syntax_case_ellipsis() {
    assert_eq!(
        eval_str(include_str!("fixtures/l22_syntax_case_ellipsis.scm").trim()),
        Ok("(1 2 3)".into())
    );
}
