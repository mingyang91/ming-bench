use crate::scheme::eval_str;

// ===== Level 26: procedure? on all callable types =====

#[test]
fn test_l26_procedure_lambda() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_procedure_lambda.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l26_procedure_case_lambda() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_procedure_case_lambda.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l26_procedure_builtin() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_procedure_builtin.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l26_procedure_continuation() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_procedure_continuation.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l26_procedure_non_callable() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_procedure_non_callable.scm").trim()),
        Ok("#t".into())
    );
}
