/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use cs61a_bench::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(_input: &str) -> Result<String, String> {
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

    // ===== Level 12: Variadic & apply =====

    #[test]
    fn test_l12_rest_args() {
        assert_eq!(
            eval_str("(define (f x . rest) rest) (f 1 2 3)"),
            Ok("(2 3)".into())
        );
    }

    #[test]
    fn test_l12_rest_args_empty() {
        assert_eq!(
            eval_str("(define (f . all) all) (f)"),
            Ok("()".into())
        );
    }

    #[test]
    fn test_l12_apply_builtin() {
        assert_eq!(eval_str("(apply + '(1 2 3))"), Ok("6".into()));
    }

    #[test]
    fn test_l12_apply_prefix_args() {
        assert_eq!(eval_str("(apply + 1 2 '(3 4))"), Ok("10".into()));
    }

    #[test]
    fn test_l12_apply_user_fn() {
        assert_eq!(
            eval_str(
                "(define (sum . args) (if (null? args) 0 (+ (car args) (apply sum (cdr args))))) (sum 1 2 3 4 5)"
            ),
            Ok("15".into())
        );
    }

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

    // ===== Level 15: define-syntax / syntax-rules =====

    #[test]
    fn test_l15_simple_macro() {
        assert_eq!(
            eval_str(
                "(define-syntax my-if (syntax-rules () ((my-if c t f) (if c t f)))) (my-if #t 1 2)"
            ),
            Ok("1".into())
        );
    }

    #[test]
    fn test_l15_my_and_macro() {
        assert_eq!(
            eval_str(
                "(define-syntax my-and (syntax-rules () ((my-and) #t) ((my-and x) x) ((my-and x rest ...) (if x (my-and rest ...) #f)))) (my-and 1 2 3)"
            ),
            Ok("3".into())
        );
    }

    #[test]
    fn test_l15_swap_hygiene() {
        assert_eq!(
            eval_str(
                "(define-syntax swap! (syntax-rules () ((swap! a b) (let ((tmp a)) (set! a b) (set! b tmp))))) (define x 1) (define y 2) (swap! x y) (list x y)"
            ),
            Ok("(2 1)".into())
        );
    }

    #[test]
    fn test_l15_variadic_pattern() {
        assert_eq!(
            eval_str(
                "(define-syntax my-list (syntax-rules () ((my-list) '()) ((my-list x rest ...) (cons x (my-list rest ...))))) (my-list 1 2 3)"
            ),
            Ok("(1 2 3)".into())
        );
    }

    #[test]
    fn test_l15_nested_macro() {
        assert_eq!(
            eval_str(
                "(define-syntax when (syntax-rules () ((when test body ...) (if test (begin body ...) #f)))) (define x 0) (when #t (set! x 1) (set! x (+ x 1))) x"
            ),
            Ok("2".into())
        );
    }

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
}
