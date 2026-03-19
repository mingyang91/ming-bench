use crate::scheme::eval_str;

// ===== Level 4: Define & If =====

#[test]
fn test_l04_if_true() {
    assert_eq!(eval_str("(if #t 1 2)"), Ok("1".into()));
}

#[test]
fn test_l04_if_false() {
    assert_eq!(eval_str("(if #f 1 2)"), Ok("2".into()));
}

#[test]
fn test_l04_if_expr() {
    assert_eq!(eval_str("(if (< 1 2) 10 20)"), Ok("10".into()));
}

#[test]
fn test_l04_define_var() {
    assert_eq!(eval_str("(define x 5) x"), Ok("5".into()));
}

#[test]
fn test_l04_define_use() {
    assert_eq!(eval_str("(define x 3) (+ x 1)"), Ok("4".into()));
}

#[test]
fn test_l04_define_multi() {
    assert_eq!(
        eval_str("(define x 10) (define y 20) (+ x y)"),
        Ok("30".into())
    );
}

#[test]
fn test_l04_quote() {
    assert_eq!(eval_str("(quote (1 2 3))"), Ok("(1 2 3)".into()));
}
