use crate::scheme::eval_str;

// ===== Level 14: First-Class Continuations — call/cc =====

#[test]
fn test_l14_callcc_nonlocal_exit() {
    assert_eq!(
        eval_str("(call/cc (lambda (k) (k 42) 99))"),
        Ok("42".into())
    );
}

#[test]
fn test_l14_callcc_no_escape() {
    assert_eq!(
        eval_str("(call/cc (lambda (k) 7))"),
        Ok("7".into())
    );
}

#[test]
fn test_l14_callcc_early_return() {
    assert_eq!(
        eval_str(
            "(define (find-negative lst) (call/cc (lambda (return) (define (loop l) (cond ((null? l) #f) ((< (car l) 0) (return (car l))) (else (loop (cdr l))))) (loop lst)))) (find-negative '(3 7 -2 5))"
        ),
        Ok("-2".into())
    );
}

#[test]
fn test_l14_callcc_saved_continuation() {
    assert_eq!(
        eval_str(
            "(define saved #f) (define (get-cont) (call/cc (lambda (k) (set! saved k) 10))) (define val (get-cont)) (if (= val 10) (saved 42) val)"
        ),
        Ok("42".into())
    );
}

#[test]
fn test_l14_callcc_as_value() {
    assert_eq!(
        eval_str(
            "(define (call-with-escape f) (call/cc (lambda (k) (f k)))) (+ 1 (call-with-escape (lambda (exit) (exit 10) 999)))"
        ),
        Ok("11".into())
    );
}

#[test]
fn test_l14_callcc_reentrant() {
    assert_eq!(
        eval_str(
            "(define k-save #f) (define count 0) (define (run) (set! count (+ count (call/cc (lambda (k) (set! k-save k) 1))))) (run) (if (< count 3) (k-save 1) count)"
        ),
        Ok("3".into())
    );
}

#[test]
fn test_l14_callcc_exception_handler() {
    assert_eq!(
        eval_str(
            "(define (with-handler handler thunk) (call/cc (lambda (exit) (define (raise msg) (exit (handler msg))) (thunk raise)))) (with-handler (lambda (msg) (list (quote error) msg)) (lambda (raise) (raise 42) (quote unreachable)))"
        ),
        Ok("(error 42)".into())
    );
}

#[test]
fn test_l14_callcc_is_first_class() {
    assert_eq!(
        eval_str("((lambda (cc) (cc (lambda (k) (k 7)))) call/cc)"),
        Ok("7".into())
    );
}

#[test]
fn test_l14_callcc_resumes_lambda_body() {
    assert_eq!(
        eval_str(
            "(define saved #f) (define first? #t) (define result ((lambda () (call/cc (lambda (k) (set! saved k) 1)) (if first? (begin (set! first? #f) 2) 3)))) (if (= result 2) (saved 99) result)"
        ),
        Ok("3".into())
    );
}

#[test]
fn test_l14_callcc_resumes_pending_application() {
    assert_eq!(
        eval_str(
            "(define saved #f) (define first? #t) (define result ((lambda (a b) (+ b 1)) 1 (call/cc (lambda (k) (set! saved k) 2)))) (if first? (begin (set! first? #f) (saved 42)) result)"
        ),
        Ok("43".into())
    );
}
