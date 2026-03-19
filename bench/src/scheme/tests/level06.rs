use crate::scheme::eval_str;

// ===== Level 6: List Operations =====

#[test]
fn test_l06_cons() {
    assert_eq!(
        eval_str(include_str!("fixtures/l06_cons.scm").trim()),
        Ok("(1)".into())
    );
}

#[test]
fn test_l06_cons_chain() {
    assert_eq!(
        eval_str(include_str!("fixtures/l06_cons_chain.scm").trim()),
        Ok("(1 2 3)".into())
    );
}

#[test]
fn test_l06_car() {
    assert_eq!(
        eval_str(include_str!("fixtures/l06_car.scm").trim()),
        Ok("1".into())
    );
}

#[test]
fn test_l06_cdr() {
    assert_eq!(
        eval_str(include_str!("fixtures/l06_cdr.scm").trim()),
        Ok("(2 3)".into())
    );
}

#[test]
fn test_l06_null_true() {
    assert_eq!(
        eval_str(include_str!("fixtures/l06_null_true.scm").trim()),
        Ok("#t".into())
    );
}

#[test]
fn test_l06_null_false() {
    assert_eq!(
        eval_str(include_str!("fixtures/l06_null_false.scm").trim()),
        Ok("#f".into())
    );
}

#[test]
fn test_l06_list() {
    assert_eq!(
        eval_str(include_str!("fixtures/l06_list.scm").trim()),
        Ok("(1 2 3)".into())
    );
}

#[test]
fn test_l06_length() {
    assert_eq!(
        eval_str(include_str!("fixtures/l06_length.scm").trim()),
        Ok("3".into())
    );
}
