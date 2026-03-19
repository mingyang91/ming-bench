use crate::scheme::eval_str;

// ===== Level 8: Let, Begin, Cond =====

#[test]
fn test_l08_let() {
    assert_eq!(eval_str("(let ((x 1) (y 2)) (+ x y))"), Ok("3".into()));
}

#[test]
fn test_l08_nested_let() {
    assert_eq!(
        eval_str("(let ((x 5)) (let ((y (+ x 1))) y))"),
        Ok("6".into())
    );
}

#[test]
fn test_l08_begin() {
    assert_eq!(eval_str("(begin 1 2 3)"), Ok("3".into()));
}

#[test]
fn test_l08_cond() {
    assert_eq!(
        eval_str("(cond ((= 1 2) 10) ((= 1 1) 20) (else 30))"),
        Ok("20".into())
    );
}

#[test]
fn test_l08_cond_else() {
    assert_eq!(eval_str("(cond (#f 1) (else 2))"), Ok("2".into()));
}

#[test]
fn test_l08_begin_define() {
    assert_eq!(
        eval_str("(define x 0) (begin (define x 1) (define x 2) x)"),
        Ok("2".into())
    );
}
