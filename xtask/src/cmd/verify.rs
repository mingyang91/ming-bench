use crate::model::{command_exists, run_cmd_capture, Error, Result};

/// (label, guile_expression, expected_output)
const TEST_CASES: &[(&str, &str, &str)] = &[
    // Level 1: Atoms
    ("l01_integer",     "(display 42) (newline)",          "42"),
    ("l01_negative",    "(display -7) (newline)",          "-7"),
    ("l01_true",        "(display #t) (newline)",          "#t"),
    ("l01_false",       "(display #f) (newline)",          "#f"),
    ("l01_string",      "(write \"hello\") (newline)",     "\"hello\""),

    // Level 2: Arithmetic
    ("l02_add",         "(display (+ 1 2)) (newline)",             "3"),
    ("l02_sub",         "(display (- 10 3)) (newline)",            "7"),
    ("l02_mul",         "(display (* 4 5)) (newline)",             "20"),
    ("l02_div",         "(display (/ 10 2)) (newline)",            "5"),
    ("l02_variadic",    "(display (+ 1 2 3 4)) (newline)",        "10"),
    ("l02_unary_minus", "(display (- 10)) (newline)",              "-10"),
    ("l02_nested",      "(display (+ (* 2 3) (- 10 4))) (newline)", "12"),

    // Level 3: Comparisons
    ("l03_lt",          "(display (< 1 2)) (newline)",             "#t"),
    ("l03_gt",          "(display (> 1 2)) (newline)",             "#f"),
    ("l03_eq",          "(display (= 3 3)) (newline)",             "#t"),
    ("l03_le",          "(display (<= 2 2)) (newline)",            "#t"),
    ("l03_not",         "(display (not #t)) (newline)",            "#f"),
    ("l03_and",         "(display (and #t #t #f)) (newline)",      "#f"),
    ("l03_or",          "(display (or #f #f 5)) (newline)",        "5"),

    // Level 4: Define & If
    ("l04_if_true",     "(display (if #t 1 2)) (newline)",        "1"),
    ("l04_if_false",    "(display (if #f 1 2)) (newline)",        "2"),
    ("l04_if_expr",     "(display (if (< 1 2) 10 20)) (newline)", "10"),
    ("l04_define_var",  "(define x 5) (display x) (newline)",     "5"),
    ("l04_define_use",  "(define x 3) (display (+ x 1)) (newline)", "4"),
    ("l04_define_multi","(define x 10) (define y 20) (display (+ x y)) (newline)", "30"),
    ("l04_quote",       "(display (quote (1 2 3))) (newline)",     "(1 2 3)"),

    // Level 5: Lambda & Closures
    ("l05_lambda",      "(display ((lambda (x) (+ x 1)) 5)) (newline)", "6"),
    ("l05_multi_param", "(display ((lambda (x y) (+ x y)) 3 4)) (newline)", "7"),
    ("l05_define_fn",   "(define (square x) (* x x)) (display (square 5)) (newline)", "25"),
    ("l05_closure",     "(define (make-adder n) (lambda (x) (+ x n))) (display ((make-adder 3) 4)) (newline)", "7"),
    ("l05_higher_order","(define (apply-twice f x) (f (f x))) (display (apply-twice (lambda (x) (+ x 1)) 0)) (newline)", "2"),
    ("l05_factorial",   "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (display (fact 5)) (newline)", "120"),
    ("l05_fibonacci",   "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (display (fib 10)) (newline)", "55"),

    // Level 6: List Operations
    ("l06_cons",        "(display (cons 1 '())) (newline)",        "(1)"),
    ("l06_cons_chain",  "(display (cons 1 (cons 2 (cons 3 '())))) (newline)", "(1 2 3)"),
    ("l06_car",         "(display (car '(1 2 3))) (newline)",      "1"),
    ("l06_cdr",         "(display (cdr '(1 2 3))) (newline)",      "(2 3)"),
    ("l06_null_true",   "(display (null? '())) (newline)",         "#t"),
    ("l06_null_false",  "(display (null? '(1))) (newline)",        "#f"),
    ("l06_list",        "(display (list 1 2 3)) (newline)",        "(1 2 3)"),
    ("l06_length",      "(display (length '(1 2 3))) (newline)",   "3"),

    // Level 7: Recursive List Programs
    ("l07_count",       "(define (count lst) (if (null? lst) 0 (+ 1 (count (cdr lst))))) (display (count '(a b c))) (newline)", "3"),
    ("l07_append",      "(define (my-append a b) (if (null? a) b (cons (car a) (my-append (cdr a) b)))) (display (my-append '(1 2) '(3 4))) (newline)", "(1 2 3 4)"),
    ("l07_reverse",     "(define (my-reverse lst) (define (rev-iter l acc) (if (null? l) acc (rev-iter (cdr l) (cons (car l) acc)))) (rev-iter lst '())) (display (my-reverse '(1 2 3))) (newline)", "(3 2 1)"),
    ("l07_map",         "(define (my-map f lst) (if (null? lst) '() (cons (f (car lst)) (my-map f (cdr lst))))) (display (my-map (lambda (x) (* x x)) '(1 2 3 4))) (newline)", "(1 4 9 16)"),
    ("l07_filter",      "(define (my-filter pred lst) (if (null? lst) '() (if (pred (car lst)) (cons (car lst) (my-filter pred (cdr lst))) (my-filter pred (cdr lst))))) (display (my-filter (lambda (x) (> x 2)) '(1 2 3 4 5))) (newline)", "(3 4 5)"),

    // Level 8: Let, Begin, Cond
    ("l08_let",         "(display (let ((x 1) (y 2)) (+ x y))) (newline)", "3"),
    ("l08_nested_let",  "(display (let ((x 5)) (let ((y (+ x 1))) y))) (newline)", "6"),
    ("l08_begin",       "(display (begin 1 2 3)) (newline)",       "3"),
    ("l08_cond",        "(display (cond ((= 1 2) 10) ((= 1 1) 20) (else 30))) (newline)", "20"),
    ("l08_cond_else",   "(display (cond (#f 1) (else 2))) (newline)", "2"),
    ("l08_begin_define","(define x 0) (begin (define x 1) (define x 2) (display x)) (newline)", "2"),

    // Level 9: Type Predicates
    ("l09_string",      "(display (string? \"hello\")) (newline)", "#t"),
    ("l09_number",      "(display (number? 42)) (newline)",        "#t"),
    ("l09_boolean",     "(display (boolean? #t)) (newline)",       "#t"),
    ("l09_pair",        "(display (pair? '(1 2))) (newline)",      "#t"),
    ("l09_symbol",      "(display (symbol? 'foo)) (newline)",      "#t"),

    // Level 10: Tail Call Optimization
    ("l10_loop",        "(define (loop n) (if (= n 0) (quote done) (loop (- n 1)))) (display (loop 1000000)) (newline)", "done"),
    ("l10_fact_iter",   "(define (fact-iter n acc) (if (= n 0) acc (fact-iter (- n 1) (* n acc)))) (display (fact-iter 20 1)) (newline)", "2432902008176640000"),
    ("l10_mutual",      "(define (my-even? n) (if (= n 0) #t (my-odd? (- n 1)))) (define (my-odd? n) (if (= n 0) #f (my-even? (- n 1)))) (display (my-even? 100000)) (newline)", "#t"),
];

pub fn run() -> Result<()> {
    if !command_exists("guile") {
        return Err(Error::BinaryNotFound {
            name: "guile".to_string(),
        });
    }

    let cwd = std::env::current_dir().expect("cannot read current directory");
    let mut passed = 0u32;
    let mut failed = 0u32;
    let mut errors: Vec<String> = Vec::new();

    let mut current_section = "";

    for (label, expr, expected) in TEST_CASES {
        // Print section headers
        let section = &label[..3];
        if section != current_section {
            current_section = section;
            let level_name = match section {
                "l01" => "Level 1: Atoms",
                "l02" => "Level 2: Arithmetic",
                "l03" => "Level 3: Comparisons",
                "l04" => "Level 4: Define & If",
                "l05" => "Level 5: Lambda & Closures",
                "l06" => "Level 6: List Operations",
                "l07" => "Level 7: Recursive List Programs",
                "l08" => "Level 8: Let, Begin, Cond",
                "l09" => "Level 9: Type Predicates",
                "l10" => "Level 10: Tail Call Optimization",
                _ => section,
            };
            println!("=== {level_name} ===");
        }

        let result = run_cmd_capture(
            "guile",
            &["--no-auto-compile", "-c", expr],
            &cwd,
        );

        match result {
            Ok((exit_code, actual)) => {
                let actual = actual.trim_end();
                if exit_code != 0 {
                    failed += 1;
                    errors.push(format!("  FAIL {label}: guile error on: {expr}"));
                } else if actual == *expected {
                    passed += 1;
                } else {
                    failed += 1;
                    errors.push(format!(
                        "  FAIL {label}: expected '{expected}', got '{actual}'"
                    ));
                }
            }
            Err(_) => {
                failed += 1;
                errors.push(format!("  FAIL {label}: failed to run guile"));
            }
        }
    }

    println!();
    println!("=== Results ===");
    println!("  Passed: {passed}");
    println!("  Failed: {failed}");

    if failed > 0 {
        for err in &errors {
            eprintln!("{err}");
        }
        Err(Error::CommandFailed {
            cmd: "verify".to_string(),
            exit_code: 1,
        })
    } else {
        println!("  All tests match ground truth!");
        Ok(())
    }
}
