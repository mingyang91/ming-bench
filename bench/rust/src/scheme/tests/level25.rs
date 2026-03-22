use crate::scheme::eval_str;

// ===== Level 25: procedure-name =====

#[test]
fn test_l25_procedure_name_define() {
    assert_eq!(
        eval_str(include_str!("fixtures/l25_procedure_name_define.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l25_procedure_name_anonymous() {
    assert_eq!(
        eval_str(include_str!("fixtures/l25_procedure_name_anonymous.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l25_procedure_name_let() {
    assert_eq!(
        eval_str(include_str!("fixtures/l25_procedure_name_let.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l25_procedure_name_case_lambda() {
    assert_eq!(
        eval_str(include_str!("fixtures/l25_procedure_name_case_lambda.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l25_procedure_name_builtin() {
    assert_eq!(
        eval_str(include_str!("fixtures/l25_procedure_name_builtin.scm").trim()),
        Ok("#t".into())
    );
}
