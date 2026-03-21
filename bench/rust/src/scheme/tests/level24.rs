use crate::scheme::eval_str;

// ===== Level 24: case-lambda =====

#[test]
fn test_l24_case_lambda_basic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l24_case_lambda_basic.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l24_case_lambda_rest() {
    assert_eq!(
        eval_str(include_str!("fixtures/l24_case_lambda_rest.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l24_case_lambda_recursive() {
    assert_eq!(
        eval_str(include_str!("fixtures/l24_case_lambda_recursive.scm").trim()),
        Ok("3".into())
    );
}

#[test]
fn test_l24_case_lambda_higher_order() {
    assert_eq!(
        eval_str(include_str!("fixtures/l24_case_lambda_higher_order.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l24_case_lambda_procedure() {
    assert_eq!(
        eval_str(include_str!("fixtures/l24_case_lambda_procedure.scm").trim()),
        Ok("#t".into())
    );
}
