use crate::scheme::eval_str;

// ===== Level 5: Lambda & Closures =====

#[test]
fn test_l05_lambda_call() {
    assert_eq!(eval_str("((lambda (x) (+ x 1)) 5)"), Ok("6".into()));
}

#[test]
fn test_l05_lambda_multi_param() {
    assert_eq!(eval_str("((lambda (x y) (+ x y)) 3 4)"), Ok("7".into()));
}

#[test]
fn test_l05_define_fn() {
    assert_eq!(
        eval_str("(define (square x) (* x x)) (square 5)"),
        Ok("25".into())
    );
}

#[test]
fn test_l05_closure() {
    assert_eq!(
        eval_str("(define (make-adder n) (lambda (x) (+ x n))) ((make-adder 3) 4)"),
        Ok("7".into())
    );
}

#[test]
fn test_l05_higher_order() {
    assert_eq!(
        eval_str("(define (apply-twice f x) (f (f x))) (apply-twice (lambda (x) (+ x 1)) 0)"),
        Ok("2".into())
    );
}

#[test]
fn test_l05_factorial() {
    assert_eq!(
        eval_str("(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)"),
        Ok("120".into())
    );
}

#[test]
fn test_l05_fibonacci() {
    assert_eq!(
        eval_str("(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)"),
        Ok("55".into())
    );
}
