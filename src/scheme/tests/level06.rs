use crate::scheme::eval_str;

// ===== Level 6: List Operations =====

#[test]
fn test_l06_cons() {
    assert_eq!(eval_str("(cons 1 '())"), Ok("(1)".into()));
}

#[test]
fn test_l06_cons_chain() {
    assert_eq!(
        eval_str("(cons 1 (cons 2 (cons 3 '())))"),
        Ok("(1 2 3)".into())
    );
}

#[test]
fn test_l06_car() {
    assert_eq!(eval_str("(car '(1 2 3))"), Ok("1".into()));
}

#[test]
fn test_l06_cdr() {
    assert_eq!(eval_str("(cdr '(1 2 3))"), Ok("(2 3)".into()));
}

#[test]
fn test_l06_null_true() {
    assert_eq!(eval_str("(null? '())"), Ok("#t".into()));
}

#[test]
fn test_l06_null_false() {
    assert_eq!(eval_str("(null? '(1))"), Ok("#f".into()));
}

#[test]
fn test_l06_list() {
    assert_eq!(eval_str("(list 1 2 3)"), Ok("(1 2 3)".into()));
}

#[test]
fn test_l06_length() {
    assert_eq!(eval_str("(length '(1 2 3))"), Ok("3".into()));
}
