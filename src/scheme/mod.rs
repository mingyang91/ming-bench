/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use cs61a_bench::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, String> {
    todo!()
}

// ---------------------------------------------------------------------------
// Tests — organised by level (easy → hard).
//
// Run a single level:  cargo test test_l01
// Run all tests:       cargo test
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::eval_str;

    // ===== Level 1: Atoms =====

    #[test]
    fn test_l01_integer() {
        assert_eq!(eval_str("42"), Ok("42".into()));
    }

    #[test]
    fn test_l01_negative_integer() {
        assert_eq!(eval_str("-7"), Ok("-7".into()));
    }

    #[test]
    fn test_l01_true() {
        assert_eq!(eval_str("#t"), Ok("#t".into()));
    }

    #[test]
    fn test_l01_false() {
        assert_eq!(eval_str("#f"), Ok("#f".into()));
    }

    #[test]
    fn test_l01_string() {
        assert_eq!(eval_str("\"hello\""), Ok("\"hello\"".into()));
    }

    // ===== Level 2: Arithmetic =====

    #[test]
    fn test_l02_add() {
        assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
    }

    #[test]
    fn test_l02_sub() {
        assert_eq!(eval_str("(- 10 3)"), Ok("7".into()));
    }

    #[test]
    fn test_l02_mul() {
        assert_eq!(eval_str("(* 4 5)"), Ok("20".into()));
    }

    #[test]
    fn test_l02_div() {
        assert_eq!(eval_str("(/ 10 2)"), Ok("5".into()));
    }

    #[test]
    fn test_l02_variadic_add() {
        assert_eq!(eval_str("(+ 1 2 3 4)"), Ok("10".into()));
    }

    #[test]
    fn test_l02_unary_minus() {
        assert_eq!(eval_str("(- 10)"), Ok("-10".into()));
    }

    #[test]
    fn test_l02_nested_arith() {
        assert_eq!(eval_str("(+ (* 2 3) (- 10 4))"), Ok("12".into()));
    }

    // ===== Level 3: Comparisons & Boolean Ops =====

    #[test]
    fn test_l03_less_than() {
        assert_eq!(eval_str("(< 1 2)"), Ok("#t".into()));
    }

    #[test]
    fn test_l03_greater_than() {
        assert_eq!(eval_str("(> 1 2)"), Ok("#f".into()));
    }

    #[test]
    fn test_l03_equal() {
        assert_eq!(eval_str("(= 3 3)"), Ok("#t".into()));
    }

    #[test]
    fn test_l03_less_equal() {
        assert_eq!(eval_str("(<= 2 2)"), Ok("#t".into()));
    }

    #[test]
    fn test_l03_not() {
        assert_eq!(eval_str("(not #t)"), Ok("#f".into()));
    }

    #[test]
    fn test_l03_and() {
        assert_eq!(eval_str("(and #t #t #f)"), Ok("#f".into()));
    }

    #[test]
    fn test_l03_or() {
        assert_eq!(eval_str("(or #f #f 5)"), Ok("5".into()));
    }

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
        assert_eq!(eval_str("(define x 10) (define y 20) (+ x y)"), Ok("30".into()));
    }

    #[test]
    fn test_l04_quote() {
        assert_eq!(eval_str("(quote (1 2 3))"), Ok("(1 2 3)".into()));
    }

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
            eval_str(
                "(define (apply-twice f x) (f (f x))) (apply-twice (lambda (x) (+ x 1)) 0)"
            ),
            Ok("2".into())
        );
    }

    #[test]
    fn test_l05_factorial() {
        assert_eq!(
            eval_str(
                "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)"
            ),
            Ok("120".into())
        );
    }

    #[test]
    fn test_l05_fibonacci() {
        assert_eq!(
            eval_str(
                "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)"
            ),
            Ok("55".into())
        );
    }

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

    // ===== Level 7: Recursive List Programs =====

    #[test]
    fn test_l07_count() {
        assert_eq!(
            eval_str(
                "(define (count lst) (if (null? lst) 0 (+ 1 (count (cdr lst))))) (count '(a b c))"
            ),
            Ok("3".into())
        );
    }

    #[test]
    fn test_l07_append() {
        assert_eq!(
            eval_str(
                "(define (my-append a b) (if (null? a) b (cons (car a) (my-append (cdr a) b)))) (my-append '(1 2) '(3 4))"
            ),
            Ok("(1 2 3 4)".into())
        );
    }

    #[test]
    fn test_l07_reverse() {
        assert_eq!(
            eval_str(
                "(define (reverse lst) (define (rev-iter l acc) (if (null? l) acc (rev-iter (cdr l) (cons (car l) acc)))) (rev-iter lst '())) (reverse '(1 2 3))"
            ),
            Ok("(3 2 1)".into())
        );
    }

    #[test]
    fn test_l07_map() {
        assert_eq!(
            eval_str(
                "(define (map f lst) (if (null? lst) '() (cons (f (car lst)) (map f (cdr lst))))) (map (lambda (x) (* x x)) '(1 2 3 4))"
            ),
            Ok("(1 4 9 16)".into())
        );
    }

    #[test]
    fn test_l07_filter() {
        assert_eq!(
            eval_str(
                "(define (filter pred lst) (if (null? lst) '() (if (pred (car lst)) (cons (car lst) (filter pred (cdr lst))) (filter pred (cdr lst))))) (filter (lambda (x) (> x 2)) '(1 2 3 4 5))"
            ),
            Ok("(3 4 5)".into())
        );
    }

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

    // ===== Level 9: Strings & Type Predicates =====

    #[test]
    fn test_l09_string_pred() {
        assert_eq!(eval_str("(string? \"hello\")"), Ok("#t".into()));
    }

    #[test]
    fn test_l09_number_pred() {
        assert_eq!(eval_str("(number? 42)"), Ok("#t".into()));
    }

    #[test]
    fn test_l09_boolean_pred() {
        assert_eq!(eval_str("(boolean? #t)"), Ok("#t".into()));
    }

    #[test]
    fn test_l09_pair_pred() {
        assert_eq!(eval_str("(pair? '(1 2))"), Ok("#t".into()));
    }

    #[test]
    fn test_l09_symbol_pred() {
        assert_eq!(eval_str("(symbol? 'foo)"), Ok("#t".into()));
    }

    // ===== Level 10: Tail Call Optimization =====

    #[test]
    fn test_l10_tco_loop() {
        assert_eq!(
            eval_str(
                "(define (loop n) (if (= n 0) (quote done) (loop (- n 1)))) (loop 1000000)"
            ),
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
}
