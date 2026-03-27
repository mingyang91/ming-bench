use crate::scheme::{eval_str, eval_str_with_output};

fn bench_level() -> u32 {
    std::env::var("BENCH_LEVEL")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(0)
}

// ===== Level 28: Concurrent Evaluation =====
// eval_str must be safe for concurrent use from multiple threads.
// Each call gets its own environment; no global mutable state.

#[test]
fn test_l28_concurrent_independent_eval() {
    if bench_level() > 0 && bench_level() < 28 {
        return;
    }
    let handles: Vec<_> = (0..8)
        .map(|i| {
            std::thread::spawn(move || {
                let program = format!(
                    "(let loop ((n 1000) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc {}))))",
                    i
                );
                eval_str(&program)
            })
        })
        .collect();

    for (i, h) in handles.into_iter().enumerate() {
        let result = h.join().expect("thread panicked");
        assert_eq!(result, Ok(format!("{}", i * 1000)));
    }
}

#[test]
fn test_l28_concurrent_output_isolation() {
    if bench_level() > 0 && bench_level() < 28 {
        return;
    }
    let handles: Vec<_> = (0..4)
        .map(|i| {
            std::thread::spawn(move || {
                let program = format!(
                    "(begin (display \"thread{i}\") (display \" \") (display \"done{i}\") \"ok\")"
                );
                eval_str_with_output(&program)
            })
        })
        .collect();

    for (i, h) in handles.into_iter().enumerate() {
        let (result, output) = h.join().expect("thread panicked").expect("eval failed");
        assert_eq!(result, "ok");
        assert_eq!(output, format!("thread{i} done{i}"));
    }
}

#[test]
fn test_l28_concurrent_closures_and_mutation() {
    if bench_level() > 0 && bench_level() < 28 {
        return;
    }
    // Each thread creates its own closure with set! counter — must be isolated
    let handles: Vec<_> = (0..4)
        .map(|_| {
            std::thread::spawn(|| {
                eval_str(
                    "(let ((count 0))
                       (define (inc!) (set! count (+ count 1)) count)
                       (inc!) (inc!) (inc!)
                       count)",
                )
            })
        })
        .collect();

    for h in handles {
        let result = h.join().expect("thread panicked");
        assert_eq!(result, Ok("3".into()));
    }
}

#[test]
fn test_l28_concurrent_stress() {
    if bench_level() > 0 && bench_level() < 28 {
        return;
    }
    let handles: Vec<_> = (0..16)
        .map(|i| {
            std::thread::spawn(move || {
                let program =
                    format!("(let ((x {i})) (define (f n) (if (= n 0) x (f (- n 1)))) (f 100))");
                eval_str(&program)
            })
        })
        .collect();

    for (i, h) in handles.into_iter().enumerate() {
        let result = h.join().expect("thread panicked");
        assert_eq!(result, Ok(format!("{i}")));
    }
}

// ===== State isolation tests (sequential — no threading needed) =====

#[test]
fn test_l28_sequential_state_leak() {
    if bench_level() > 0 && bench_level() < 28 {
        return;
    }
    // First call defines x, second call must NOT see it
    let r1 = eval_str("(begin (define x 42) x)");
    assert_eq!(r1, Ok("42".into()));

    // x must not leak to the next eval_str call
    let r2 = eval_str("x");
    assert!(
        r2.is_err(),
        "variable 'x' leaked between independent eval_str calls"
    );
}

#[test]
fn test_l28_sequential_output_leak() {
    if bench_level() > 0 && bench_level() < 28 {
        return;
    }
    let (_, out1) = eval_str_with_output("(display \"aaa\")").expect("eval failed");
    let (_, out2) = eval_str_with_output("(display \"bbb\")").expect("eval failed");
    assert_eq!(out1, "aaa");
    assert_eq!(
        out2, "bbb",
        "output buffer leaked between eval_str_with_output calls"
    );
}

// ===== Concurrent continuation/macro collision tests =====

#[test]
fn test_l28_concurrent_callcc_collision() {
    if bench_level() > 0 && bench_level() < 28 {
        return;
    }
    // 8 threads all use call/cc simultaneously — if continuation IDs
    // are a global counter, results may corrupt across threads
    let handles: Vec<_> = (0..8)
        .map(|_| {
            std::thread::spawn(|| {
                eval_str(
                    "(let ((count 0))
                       (set! count (+ count (call/cc (lambda (k) (k 10)))))
                       count)",
                )
            })
        })
        .collect();

    for h in handles {
        let result = h.join().expect("thread panicked");
        assert_eq!(result, Ok("10".into()));
    }
}

#[test]
fn test_l28_concurrent_macro_hygiene() {
    if bench_level() > 0 && bench_level() < 28 {
        return;
    }
    // 4 threads expand macros with gensym — if gensym counter is global,
    // symbol collisions cause incorrect variable capture
    let handles: Vec<_> = (0..4)
        .map(|_| {
            std::thread::spawn(|| {
                eval_str(
                    "(begin
                       (define-syntax my-swap!
                         (syntax-rules ()
                           ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
                       (let ((x 1) (y 2))
                         (my-swap! x y)
                         (list x y)))",
                )
            })
        })
        .collect();

    for h in handles {
        let result = h.join().expect("thread panicked");
        assert_eq!(result, Ok("(2 1)".into()));
    }
}
