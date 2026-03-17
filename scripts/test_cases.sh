#!/usr/bin/env bash
# Ground-truth verification: run each test expression through Guile Scheme
# and compare output to the expected values from our Rust tests.
#
# Usage: ./scripts/test_cases.sh
#
# Requires: guile (apt install guile-3.0)

set -euo pipefail

PASS=0
FAIL=0
ERRORS=""

check() {
    local label="$1"
    local expr="$2"
    local expected="$3"

    # Wrap in (display ...) so Guile prints the value without Scheme formatting quirks
    # For multi-expression inputs, evaluate all but display the last
    local actual
    actual=$(guile --no-auto-compile -c "$expr" 2>&1) || {
        ERRORS="${ERRORS}\n  FAIL ${label}: guile error on: ${expr}"
        FAIL=$((FAIL + 1))
        return
    }

    if [ "$actual" = "$expected" ]; then
        PASS=$((PASS + 1))
    else
        ERRORS="${ERRORS}\n  FAIL ${label}: expected '${expected}', got '${actual}'"
        FAIL=$((FAIL + 1))
    fi
}

# Helper: evaluate expr(s) and display the last result
d() {
    # Takes N scheme expressions; evaluates all, displays the last
    # Usage: d '(define x 5)' 'x'  =>  (define x 5) (display x)
    local last="${!#}"
    local all_but_last=""
    local i=0
    for arg in "$@"; do
        i=$((i + 1))
        if [ $i -lt $# ]; then
            all_but_last="${all_but_last} ${arg}"
        fi
    done
    echo "${all_but_last} (display ${last}) (newline)"
}

echo "=== Level 1: Atoms ==="
check "l01_integer"          '(display 42) (newline)'           "42"
check "l01_negative"         '(display -7) (newline)'           "-7"
check "l01_true"             '(display #t) (newline)'           "#t"
check "l01_false"            '(display #f) (newline)'           "#f"
check "l01_string"           '(write "hello") (newline)'         "\"hello\""

echo "=== Level 2: Arithmetic ==="
check "l02_add"              '(display (+ 1 2)) (newline)'              "3"
check "l02_sub"              '(display (- 10 3)) (newline)'             "7"
check "l02_mul"              '(display (* 4 5)) (newline)'              "20"
check "l02_div"              '(display (/ 10 2)) (newline)'             "5"
check "l02_variadic"         '(display (+ 1 2 3 4)) (newline)'         "10"
check "l02_unary_minus"      '(display (- 10)) (newline)'              "-10"
check "l02_nested"           '(display (+ (* 2 3) (- 10 4))) (newline)' "12"

echo "=== Level 3: Comparisons ==="
check "l03_lt"               '(display (< 1 2)) (newline)'             "#t"
check "l03_gt"               '(display (> 1 2)) (newline)'             "#f"
check "l03_eq"               '(display (= 3 3)) (newline)'             "#t"
check "l03_le"               '(display (<= 2 2)) (newline)'            "#t"
check "l03_not"              '(display (not #t)) (newline)'             "#f"
check "l03_and"              '(display (and #t #t #f)) (newline)'       "#f"
check "l03_or"               '(display (or #f #f 5)) (newline)'        "5"

echo "=== Level 4: Define & If ==="
check "l04_if_true"          '(display (if #t 1 2)) (newline)'         "1"
check "l04_if_false"         '(display (if #f 1 2)) (newline)'         "2"
check "l04_if_expr"          '(display (if (< 1 2) 10 20)) (newline)'  "10"
check "l04_define_var"       '(define x 5) (display x) (newline)'      "5"
check "l04_define_use"       '(define x 3) (display (+ x 1)) (newline)' "4"
check "l04_define_multi"     '(define x 10) (define y 20) (display (+ x y)) (newline)' "30"
check "l04_quote"            '(display (quote (1 2 3))) (newline)'      "(1 2 3)"

echo "=== Level 5: Lambda & Closures ==="
check "l05_lambda"           '(display ((lambda (x) (+ x 1)) 5)) (newline)' "6"
check "l05_multi_param"      '(display ((lambda (x y) (+ x y)) 3 4)) (newline)' "7"
check "l05_define_fn"        '(define (square x) (* x x)) (display (square 5)) (newline)' "25"
check "l05_closure"          '(define (make-adder n) (lambda (x) (+ x n))) (display ((make-adder 3) 4)) (newline)' "7"
check "l05_higher_order"     '(define (apply-twice f x) (f (f x))) (display (apply-twice (lambda (x) (+ x 1)) 0)) (newline)' "2"
check "l05_factorial"        '(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (display (fact 5)) (newline)' "120"
check "l05_fibonacci"        '(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (display (fib 10)) (newline)' "55"

echo "=== Level 6: List Operations ==="
check "l06_cons"             "(display (cons 1 '())) (newline)"          "(1)"
check "l06_cons_chain"       "(display (cons 1 (cons 2 (cons 3 '())))) (newline)" "(1 2 3)"
check "l06_car"              "(display (car '(1 2 3))) (newline)"        "1"
check "l06_cdr"              "(display (cdr '(1 2 3))) (newline)"        "(2 3)"
check "l06_null_true"        "(display (null? '())) (newline)"           "#t"
check "l06_null_false"       "(display (null? '(1))) (newline)"          "#f"
check "l06_list"             '(display (list 1 2 3)) (newline)'          "(1 2 3)"
check "l06_length"           "(display (length '(1 2 3))) (newline)"     "3"

echo "=== Level 7: Recursive List Programs ==="
check "l07_count"            "(define (count lst) (if (null? lst) 0 (+ 1 (count (cdr lst))))) (display (count '(a b c))) (newline)" "3"
check "l07_append"           "(define (my-append a b) (if (null? a) b (cons (car a) (my-append (cdr a) b)))) (display (my-append '(1 2) '(3 4))) (newline)" "(1 2 3 4)"
check "l07_reverse"          "(define (my-reverse lst) (define (rev-iter l acc) (if (null? l) acc (rev-iter (cdr l) (cons (car l) acc)))) (rev-iter lst '())) (display (my-reverse '(1 2 3))) (newline)" "(3 2 1)"
check "l07_map"              "(define (my-map f lst) (if (null? lst) '() (cons (f (car lst)) (my-map f (cdr lst))))) (display (my-map (lambda (x) (* x x)) '(1 2 3 4))) (newline)" "(1 4 9 16)"
check "l07_filter"           "(define (my-filter pred lst) (if (null? lst) '() (if (pred (car lst)) (cons (car lst) (my-filter pred (cdr lst))) (my-filter pred (cdr lst))))) (display (my-filter (lambda (x) (> x 2)) '(1 2 3 4 5))) (newline)" "(3 4 5)"

echo "=== Level 8: Let, Begin, Cond ==="
check "l08_let"              '(display (let ((x 1) (y 2)) (+ x y))) (newline)' "3"
check "l08_nested_let"       '(display (let ((x 5)) (let ((y (+ x 1))) y))) (newline)' "6"
check "l08_begin"            '(display (begin 1 2 3)) (newline)'        "3"
check "l08_cond"             '(display (cond ((= 1 2) 10) ((= 1 1) 20) (else 30))) (newline)' "20"
check "l08_cond_else"        '(display (cond (#f 1) (else 2))) (newline)' "2"
check "l08_begin_define"     '(define x 0) (begin (define x 1) (define x 2) (display x)) (newline)' "2"

echo "=== Level 9: Type Predicates ==="
check "l09_string"           '(display (string? "hello")) (newline)'    "#t"
check "l09_number"           '(display (number? 42)) (newline)'         "#t"
check "l09_boolean"          '(display (boolean? #t)) (newline)'        "#t"
check "l09_pair"             "(display (pair? '(1 2))) (newline)"        "#t"
check "l09_symbol"           "(display (symbol? 'foo)) (newline)"        "#t"

echo "=== Level 10: Tail Call Optimization ==="
check "l10_loop"             "(define (loop n) (if (= n 0) (quote done) (loop (- n 1)))) (display (loop 1000000)) (newline)" "done"
check "l10_fact_iter"        '(define (fact-iter n acc) (if (= n 0) acc (fact-iter (- n 1) (* n acc)))) (display (fact-iter 20 1)) (newline)' "2432902008176640000"
check "l10_mutual"           '(define (my-even? n) (if (= n 0) #t (my-odd? (- n 1)))) (define (my-odd? n) (if (= n 0) #f (my-even? (- n 1)))) (display (my-even? 100000)) (newline)' "#t"

echo ""
echo "=== Results ==="
echo "  Passed: ${PASS}"
echo "  Failed: ${FAIL}"
if [ $FAIL -gt 0 ]; then
    echo -e "${ERRORS}"
    exit 1
else
    echo "  All tests match ground truth!"
fi
