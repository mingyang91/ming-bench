use crate::scheme::eval_str;

// ===== Level 10: Tail Call Optimization =====

#[test]
fn test_l10_tco_loop() {
    assert_eq!(
        eval_str("(define (loop n) (if (= n 0) (quote done) (loop (- n 1)))) (loop 1000000)"),
        Ok("done".into())
    );
}

#[test]
fn test_l10_tco_fact_iter() {
    assert_eq!(
        eval_str(
            "(define (fact-iter n acc) (if (= n 0) acc (fact-iter (- n 1) (* n acc)))) (fact-iter 20 1)"
        ),
        Ok("2432902008176640000".into())
    );
}

#[test]
fn test_l10_tco_mutual_recursion() {
    assert_eq!(
        eval_str(
            "(define (even? n) (if (= n 0) #t (odd? (- n 1)))) (define (odd? n) (if (= n 0) #f (even? (- n 1)))) (even? 100000)"
        ),
        Ok("#t".into())
    );
}
