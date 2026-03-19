use crate::scheme::eval_str;

// ===== Level 16: Comprehensive Integration =====

#[test]
fn test_l16_callcc_with_mutation() {
    assert_eq!(
        eval_str(
            "(define result '()) (define saved #f) (define (run) (let ((v (call/cc (lambda (k) (set! saved k) 0)))) (set! result (cons v result)) v)) (run) (if (< (car result) 3) (saved (+ (car result) 1)) (length result))"
        ),
        Ok("4".into())
    );
}

#[test]
fn test_l16_macro_tco_loop() {
    assert_eq!(
        eval_str(
            "(define-syntax while (syntax-rules () ((while test body ...) (let loop () (when test body ... (loop)))))) (define-syntax when (syntax-rules () ((when test body ...) (if test (begin body ...) #f)))) (define n 1000000) (define i 0) (while (< i n) (set! i (+ i 1))) i"
        ),
        Ok("1000000".into())
    );
}

#[test]
fn test_l16_callcc_try_catch() {
    assert_eq!(
        eval_str(
            "(define-syntax try (syntax-rules (catch) ((try body catch handler) (call/cc (lambda (exit) (define (throw v) (exit (handler v))) (body throw)))))) (try (lambda (throw) (+ 1 (throw 42) 999)) catch (lambda (v) (list (quote caught) v)))"
        ),
        Ok("(caught 42)".into())
    );
}

#[test]
fn test_l16_coroutine_scheduler() {
    assert_eq!(
        eval_str(
            "(define tasks '()) (define results '()) (define (spawn thunk) (set! tasks (cons thunk tasks))) (define (yield-val v k) (set! results (cons v results)) (set! tasks (cons k tasks))) (define (run-all) (if (null? tasks) results (let ((t (car tasks))) (set! tasks (cdr tasks)) (t) (run-all)))) (spawn (lambda () (call/cc (lambda (k) (yield-val 1 (lambda () (k #f))))) (call/cc (lambda (k) (yield-val 2 (lambda () (k #f))))))) (spawn (lambda () (call/cc (lambda (k) (yield-val 10 (lambda () (k #f))))) (call/cc (lambda (k) (yield-val 20 (lambda () (k #f))))))) (run-all) (length results)"
        ),
        Ok("4".into())
    );
}

#[test]
fn test_l16_church_booleans_with_callcc() {
    assert_eq!(
        eval_str(
            "(define (church-true x y) x) (define (church-false x y) y) (define (church-if b t f) (b t f)) (define (church-not b) (lambda (x y) (b y x))) (define result (church-if (church-not church-false) (quote yes) (quote no))) result"
        ),
        Ok("yes".into())
    );
}
