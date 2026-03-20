use crate::scheme::eval_str;

// ===== Level 24: Case Expression =====

#[test]
fn test_l24_case_symbol() {
    assert_eq!(
        eval_str(include_str!("fixtures/l24_case_symbol.scm").trim()),
        Ok("2".into())
    );
}

#[test]
fn test_l24_case_number() {
    assert_eq!(
        eval_str(include_str!("fixtures/l24_case_number.scm").trim()),
        Ok("\"two\"".into())
    );
}

#[test]
fn test_l24_case_else() {
    assert_eq!(
        eval_str(include_str!("fixtures/l24_case_else.scm").trim()),
        Ok("0".into())
    );
}

#[test]
fn test_l24_case_no_match() {
    assert_eq!(
        eval_str(include_str!("fixtures/l24_case_no_match.scm").trim()),
        Ok("#t".into())
    );
}
