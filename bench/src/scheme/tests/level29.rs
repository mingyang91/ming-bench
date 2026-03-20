use crate::scheme::eval_str;

// ===== Level 29: Character Operations =====

#[test]
fn test_l29_char_alpha() {
    assert_eq!(
        eval_str(include_str!("fixtures/l29_char_alpha.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l29_char_numeric() {
    assert_eq!(
        eval_str(include_str!("fixtures/l29_char_numeric.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l29_char_case() {
    assert_eq!(
        eval_str(include_str!("fixtures/l29_char_case.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l29_char_compare() {
    assert_eq!(
        eval_str(include_str!("fixtures/l29_char_compare.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l29_char_literal() {
    assert_eq!(
        eval_str(include_str!("fixtures/l29_char_literal.scm").trim()),
        Ok("#t".into())
    );
}
