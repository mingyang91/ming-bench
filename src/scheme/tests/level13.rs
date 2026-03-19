use crate::scheme::eval_str;

// ===== Level 13: Tail Position in All Forms =====

#[test]
fn test_l13_tco_cond() {
    assert_eq!(
        eval_str(
            "(define (loop n) (cond ((= n 0) (quote done)) (else (loop (- n 1))))) (loop 1000000)"
        ),
        Ok("done".into())
    );
}

#[test]
fn test_l13_tco_named_let() {
    assert_eq!(
        eval_str(
            "(let loop ((n 1000000)) (if (= n 0) (quote done) (loop (- n 1))))"
        ),
        Ok("done".into())
    );
}

#[test]
fn test_l13_tco_and_or() {
    assert_eq!(
        eval_str(
            "(define (loop n) (if (= n 0) #t (and #t (loop (- n 1))))) (loop 1000000)"
        ),
        Ok("#t".into())
    );
}

#[test]
fn test_l13_tco_begin() {
    assert_eq!(
        eval_str(
            "(define (loop n) (if (= n 0) (quote done) (begin 1 2 (loop (- n 1))))) (loop 1000000)"
        ),
        Ok("done".into())
    );
}

#[test]
fn test_l13_tco_let_body() {
    assert_eq!(
        eval_str(
            "(define (loop n) (if (= n 0) (quote done) (let ((m (- n 1))) (loop m)))) (loop 1000000)"
        ),
        Ok("done".into())
    );
}
