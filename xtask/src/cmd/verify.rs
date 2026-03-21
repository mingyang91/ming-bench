use crate::model::{command_exists, run_cmd_capture, Error, Result};

/// (label, guile_expression, expected_output)
const TEST_CASES: &[(&str, &str, &str)] = &[
    // Level 1: Atoms, Arithmetic & Comparisons
    ("l01_integer",     "(display 42) (newline)",          "42"),
    ("l01_negative",    "(display -7) (newline)",          "-7"),
    ("l01_true",        "(display #t) (newline)",          "#t"),
    ("l01_false",       "(display #f) (newline)",          "#f"),
    ("l01_string",      "(write \"hello\") (newline)",     "\"hello\""),
    ("l01_add",         "(display (+ 1 2)) (newline)",             "3"),
    ("l01_sub",         "(display (- 10 3)) (newline)",            "7"),
    ("l01_mul",         "(display (* 4 5)) (newline)",             "20"),
    ("l01_div",         "(display (/ 10 2)) (newline)",            "5"),
    ("l01_variadic",    "(display (+ 1 2 3 4)) (newline)",        "10"),
    ("l01_unary_minus", "(display (- 10)) (newline)",              "-10"),
    ("l01_nested",      "(display (+ (* 2 3) (- 10 4))) (newline)", "12"),
    ("l01_lt",          "(display (< 1 2)) (newline)",             "#t"),
    ("l01_gt",          "(display (> 1 2)) (newline)",             "#f"),
    ("l01_eq",          "(display (= 3 3)) (newline)",             "#t"),
    ("l01_le",          "(display (<= 2 2)) (newline)",            "#t"),
    ("l01_not",         "(display (not #t)) (newline)",            "#f"),
    ("l01_and",         "(display (and #t #t #f)) (newline)",      "#f"),
    ("l01_or",          "(display (or #f #f 5)) (newline)",        "5"),

    // Level 2: Variables, Conditionals & Lambda
    ("l02_if_true",     "(display (if #t 1 2)) (newline)",        "1"),
    ("l02_if_false",    "(display (if #f 1 2)) (newline)",        "2"),
    ("l02_if_expr",     "(display (if (< 1 2) 10 20)) (newline)", "10"),
    ("l02_define_var",  "(define x 5) (display x) (newline)",     "5"),
    ("l02_define_use",  "(define x 3) (display (+ x 1)) (newline)", "4"),
    ("l02_define_multi","(define x 10) (define y 20) (display (+ x y)) (newline)", "30"),
    ("l02_quote",       "(display (quote (1 2 3))) (newline)",     "(1 2 3)"),
    ("l02_lambda",      "(display ((lambda (x) (+ x 1)) 5)) (newline)", "6"),
    ("l02_multi_param", "(display ((lambda (x y) (+ x y)) 3 4)) (newline)", "7"),
    ("l02_define_fn",   "(define (square x) (* x x)) (display (square 5)) (newline)", "25"),
    ("l02_closure",     "(define (make-adder n) (lambda (x) (+ x n))) (display ((make-adder 3) 4)) (newline)", "7"),
    ("l02_higher_order","(define (apply-twice f x) (f (f x))) (display (apply-twice (lambda (x) (+ x 1)) 0)) (newline)", "2"),
    ("l02_factorial",   "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (display (fact 5)) (newline)", "120"),
    ("l02_fibonacci",   "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (display (fib 10)) (newline)", "55"),

    // Level 3: Lists, Recursion, Let/Begin/Cond & Predicates
    ("l03_cons",        "(display (cons 1 '())) (newline)",        "(1)"),
    ("l03_cons_chain",  "(display (cons 1 (cons 2 (cons 3 '())))) (newline)", "(1 2 3)"),
    ("l03_car",         "(display (car '(1 2 3))) (newline)",      "1"),
    ("l03_cdr",         "(display (cdr '(1 2 3))) (newline)",      "(2 3)"),
    ("l03_null_true",   "(display (null? '())) (newline)",         "#t"),
    ("l03_null_false",  "(display (null? '(1))) (newline)",        "#f"),
    ("l03_list",        "(display (list 1 2 3)) (newline)",        "(1 2 3)"),
    ("l03_length",      "(display (length '(1 2 3))) (newline)",   "3"),
    ("l03_count",       "(define (count lst) (if (null? lst) 0 (+ 1 (count (cdr lst))))) (display (count '(a b c))) (newline)", "3"),
    ("l03_append",      "(define (my-append a b) (if (null? a) b (cons (car a) (my-append (cdr a) b)))) (display (my-append '(1 2) '(3 4))) (newline)", "(1 2 3 4)"),
    ("l03_reverse",     "(define (my-reverse lst) (define (rev-iter l acc) (if (null? l) acc (rev-iter (cdr l) (cons (car l) acc)))) (rev-iter lst '())) (display (my-reverse '(1 2 3))) (newline)", "(3 2 1)"),
    ("l03_map",         "(define (my-map f lst) (if (null? lst) '() (cons (f (car lst)) (my-map f (cdr lst))))) (display (my-map (lambda (x) (* x x)) '(1 2 3 4))) (newline)", "(1 4 9 16)"),
    ("l03_filter",      "(define (my-filter pred lst) (if (null? lst) '() (if (pred (car lst)) (cons (car lst) (my-filter pred (cdr lst))) (my-filter pred (cdr lst))))) (display (my-filter (lambda (x) (> x 2)) '(1 2 3 4 5))) (newline)", "(3 4 5)"),
    ("l03_let",         "(display (let ((x 1) (y 2)) (+ x y))) (newline)", "3"),
    ("l03_nested_let",  "(display (let ((x 5)) (let ((y (+ x 1))) y))) (newline)", "6"),
    ("l03_begin",       "(display (begin 1 2 3)) (newline)",       "3"),
    ("l03_cond",        "(display (cond ((= 1 2) 10) ((= 1 1) 20) (else 30))) (newline)", "20"),
    ("l03_cond_else",   "(display (cond (#f 1) (else 2))) (newline)", "2"),
    ("l03_begin_define","(define x 0) (begin (define x 1) (define x 2) (display x)) (newline)", "2"),
    ("l03_string_pred", "(display (string? \"hello\")) (newline)", "#t"),
    ("l03_number_pred", "(display (number? 42)) (newline)",        "#t"),
    ("l03_boolean_pred","(display (boolean? #t)) (newline)",       "#t"),
    ("l03_pair_pred",   "(display (pair? '(1 2))) (newline)",      "#t"),
    ("l03_symbol_pred", "(display (symbol? 'foo)) (newline)",      "#t"),

    // Level 8: Tail Call Optimization
    ("l08_loop",        "(define (loop n) (if (= n 0) (quote done) (loop (- n 1)))) (display (loop 1000000)) (newline)", "done"),
    ("l08_fact_iter",   "(define (fact-iter n acc) (if (= n 0) acc (fact-iter (- n 1) (* n acc)))) (display (fact-iter 20 1)) (newline)", "2432902008176640000"),
    ("l08_mutual",      "(define (my-even? n) (if (= n 0) #t (my-odd? (- n 1)))) (define (my-odd? n) (if (= n 0) #f (my-even? (- n 1)))) (display (my-even? 100000)) (newline)", "#t"),

    // Level 15: Numeric/Char/String Utilities
    ("l15_abs",         "(display (abs -5)) (newline)",               "5"),
    ("l15_modulo",      "(display (modulo 10 3)) (newline)",          "1"),
    ("l15_modulo_neg",  "(display (modulo -10 3)) (newline)",         "2"),
    ("l15_remainder",   "(display (remainder -10 3)) (newline)",      "-1"),
    ("l15_quotient",    "(display (quotient 10 3)) (newline)",        "3"),
    ("l15_min",         "(display (min 3 1 4 1 5)) (newline)",        "1"),
    ("l15_max",         "(display (max 3 1 4 1 5)) (newline)",        "5"),
    ("l15_expt",        "(display (expt 2 10)) (newline)",            "1024"),
    ("l15_zero_t",      "(display (zero? 0)) (newline)",              "#t"),
    ("l15_zero_f",      "(display (zero? 1)) (newline)",              "#f"),
    ("l15_positive",    "(display (positive? 5)) (newline)",          "#t"),
    ("l15_negative",    "(display (negative? -3)) (newline)",         "#t"),
    ("l15_odd",         "(display (odd? 3)) (newline)",               "#t"),
    ("l15_even",        "(display (even? 4)) (newline)",              "#t"),
    ("l15_list_ref",    "(display (list-ref '(a b c d) 2)) (newline)", "c"),
    ("l15_list_tail",   "(display (list-tail '(a b c d) 2)) (newline)", "(c d)"),
    ("l15_list_pred_t", "(display (list? '(1 2 3))) (newline)",       "#t"),
    ("l15_list_pred_f", "(display (list? (cons 1 2))) (newline)",     "#f"),
    ("l15_assoc",       "(display (assoc 'b '((a 1) (b 2) (c 3)))) (newline)", "(b 2)"),
    ("l15_map_multi",   "(display (map + '(1 2 3) '(10 20 30))) (newline)", "(11 22 33)"),
    ("l15_char_alpha",  "(display (char-alphabetic? #\\a)) (newline)", "#t"),
    ("l15_char_num",    "(display (char-numeric? #\\5)) (newline)",   "#t"),
    ("l15_char_up",     "(display (char-upcase #\\a)) (newline)",     "A"),
    ("l15_char_down",   "(display (char-downcase #\\A)) (newline)",   "a"),
    ("l15_char_eq",     "(display (char=? #\\a #\\a)) (newline)",     "#t"),
    ("l15_char_lt",     "(display (char<? #\\a #\\b)) (newline)",     "#t"),
    ("l15_str_eq",      "(display (string=? \"abc\" \"abc\")) (newline)", "#t"),
    ("l15_str_lt",      "(display (string<? \"abc\" \"abd\")) (newline)", "#t"),
    ("l15_str_ci",      "(display (string-ci=? \"ABC\" \"abc\")) (newline)", "#t"),
    ("l15_str_up",      "(write (string-upcase \"hello\")) (newline)", "\"HELLO\""),
    ("l15_str_down",    "(write (string-downcase \"HELLO\")) (newline)", "\"hello\""),
];

fn print_section_header<'a>(label: &'a str, current_section: &mut &'a str) {
    let section = &label[..3];
    if section == *current_section {
        return;
    }
    *current_section = section;
    let level_name = match section {
        "l01" => "Level 1: Atoms, Arithmetic & Comparisons",
        "l02" => "Level 2: Variables, Conditionals & Lambda",
        "l03" => "Level 3: Lists, Recursion, Let/Begin/Cond & Predicates",
        "l08" => "Level 8: Tail Call Optimization",
        "l15" => "Level 15: Numeric/Char/String Utilities",
        _ => section,
    };
    println!("=== {level_name} ===");
}

/// Returns Ok(()) on pass, Err(message) on failure.
fn check_result(
    label: &str, expr: &str, expected: &str, exit_code: i32, actual: &str,
) -> std::result::Result<(), String> {
    if exit_code != 0 {
        Err(format!("  FAIL {label}: guile error on: {expr}"))
    } else if actual == expected {
        Ok(())
    } else {
        Err(format!("  FAIL {label}: expected '{expected}', got '{actual}'"))
    }
}

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
        print_section_header(label, &mut current_section);
        let verdict = match run_cmd_capture("guile", &["--no-auto-compile", "-c", expr], &cwd) {
            Ok((exit_code, actual)) => check_result(label, expr, expected, exit_code, actual.trim_end()),
            Err(_) => Err(format!("  FAIL {label}: failed to run guile")),
        };
        match verdict {
            Ok(()) => passed += 1,
            Err(msg) => { failed += 1; errors.push(msg); }
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
