use crate::scheme::{eval_str, eval_str_with_output};

fn bench_level() -> u32 {
    std::env::var("BENCH_LEVEL")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(0)
}

// ===== Level 27: Concurrent Evaluation =====
// eval_str must be safe for concurrent use from multiple threads.
// Each call gets its own environment; no global mutable state.

#[test]
fn test_l27_concurrent_independent_eval() {
    if bench_level() > 0 && bench_level() < 27 {
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
fn test_l27_concurrent_output_isolation() {
    if bench_level() > 0 && bench_level() < 27 {
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
fn test_l27_concurrent_closures_and_mutation() {
    if bench_level() > 0 && bench_level() < 27 {
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
fn test_l27_concurrent_stress() {
    if bench_level() > 0 && bench_level() < 27 {
        return;
    }
    let handles: Vec<_> = (0..16)
        .map(|i| {
            std::thread::spawn(move || {
                let program = format!(
                    "(let ((x {i})) (define (f n) (if (= n 0) x (f (- n 1)))) (f 100))"
                );
                eval_str(&program)
            })
        })
        .collect();

    for (i, h) in handles.into_iter().enumerate() {
        let result = h.join().expect("thread panicked");
        assert_eq!(result, Ok(format!("{i}")));
    }
}
