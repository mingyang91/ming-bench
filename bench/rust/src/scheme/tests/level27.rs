use crate::scheme::eval_str;

// ===== Level 27: parameterize + make-parameter =====

#[test]
fn test_l27_make_parameter() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_make_parameter.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l27_parameterize_basic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_parameterize_basic.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l27_parameterize_nested() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_parameterize_nested.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l27_parameterize_callcc() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_parameterize_callcc.scm").trim()),
        Ok("42".into())
    );
}

#[test]
fn test_l27_parameterize_converter() {
    assert_eq!(
        eval_str(include_str!("fixtures/l27_parameterize_converter.scm").trim()),
        Ok("#t".into())
    );
}
