use crate::scheme::{eval_str, eval_str_with_limit};

fn bench_level() -> u32 {
    std::env::var("BENCH_LEVEL")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(0)
}

// ===== Level 27: Step-Limited Evaluation =====
// eval_str_with_limit(input, max_steps) enforces a step budget.
// Each eval dispatch counts as one step. Exceeding → error.

#[test]
fn test_l27_step_limit_normal() {
    if bench_level() > 0 && bench_level() < 27 { return; }
    let result = eval_str_with_limit("(+ 1 2)", 1000);
    assert_eq!(result, Ok("3".into()));
}

#[test]
fn test_l27_step_limit_loop_within_budget() {
    if bench_level() > 0 && bench_level() < 27 { return; }
    let result = eval_str_with_limit(
        "(let loop ((n 50)) (if (= n 0) 'done (loop (- n 1))))",
        10000,
    );
    assert_eq!(result, Ok("done".into()));
}

#[test]
fn test_l27_step_limit_infinite_loop() {
    if bench_level() > 0 && bench_level() < 27 { return; }
    let result = eval_str_with_limit("(let loop () (loop))", 1000);
    assert!(result.is_err(), "infinite loop should hit step limit");
}

#[test]
fn test_l27_step_limit_exceeded() {
    if bench_level() > 0 && bench_level() < 27 { return; }
    let result = eval_str_with_limit(
        "(let loop ((n 1000)) (if (= n 0) 'done (loop (- n 1))))",
        50,
    );
    assert!(result.is_err(), "loop of 1000 iters should exceed 50-step budget");
}

#[test]
fn test_l27_step_limit_factorial() {
    if bench_level() > 0 && bench_level() < 27 { return; }
    let result = eval_str_with_limit(
        "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 10)",
        10000,
    );
    assert_eq!(result, Ok("3628800".into()));
}

#[test]
fn test_l27_normal_eval_unaffected() {
    if bench_level() > 0 && bench_level() < 27 { return; }
    // Normal eval_str still works without limit
    let result = eval_str("(let loop ((n 100000)) (if (= n 0) 'done (loop (- n 1))))");
    assert_eq!(result, Ok("done".into()));
}
