use crate::scheme::eval_str;

// ===== Level 26: let-values / receive =====

#[test]
fn test_l26_let_values_basic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_let_values_basic.scm").trim()),
        Ok("6".into())
    );
}

#[test]
fn test_l26_let_values_multi() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_let_values_multi.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l26_receive_basic() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_receive_basic.scm").trim()),
        Ok("(1 2 3)".into())
    );
}

#[test]
fn test_l26_receive_rest() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_receive_rest.scm").trim()),
        Ok("(2 3)".into())
    );
}

#[test]
fn test_l26_let_values_nested() {
    assert_eq!(
        eval_str(include_str!("fixtures/l26_let_values_nested.scm").trim()),
        Ok("#t".into())
    );
}
