use crate::scheme::eval_str;

// ===== Level 11: set! and Mutation =====

#[test]
fn test_l11_set_basic() {
    assert_eq!(eval_str("(define x 1) (set! x 2) x"), Ok("2".into()));
}

#[test]
fn test_l11_set_unbound_error() {
    assert!(eval_str("(set! unbound 5)").is_err());
}

#[test]
fn test_l11_counter() {
    assert_eq!(
        eval_str(
            "(define (make-counter) (let ((n 0)) (lambda () (set! n (+ n 1)) n))) (define c (make-counter)) (c) (c) (c)"
        ),
        Ok("3".into())
    );
}

#[test]
fn test_l11_shared_state() {
    assert_eq!(
        eval_str(
            "(define (make-pair) (let ((val 0)) (define (getter) val) (define (setter v) (set! val v)) (list getter setter))) (define p (make-pair)) (define get (car p)) (define set-val (car (cdr p))) (set-val 42) (get)"
        ),
        Ok("42".into())
    );
}

#[test]
fn test_l11_set_in_loop() {
    assert_eq!(
        eval_str(
            "(define sum 0) (define (add-up n) (if (= n 0) sum (begin (set! sum (+ sum n)) (add-up (- n 1))))) (add-up 10)"
        ),
        Ok("55".into())
    );
}
