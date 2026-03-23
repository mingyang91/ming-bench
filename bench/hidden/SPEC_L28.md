### Level 28 — Concurrent Evaluation & Performance Stress
Two requirements in one level:

**Thread safety:** `eval_str` must be safe for concurrent use from multiple threads. Independent evaluations must not interfere — each call gets its own environment, and output capture must be isolated per-call. No global mutable state. Tests spawn 4-16 threads calling `eval_str` in parallel. Sequential state isolation: consecutive `eval_str` calls must not share definitions or output buffers.

**Performance:** The interpreter must handle large-scale computation within the container's 1GB / 30s limits. Fixtures include: 1M cons cell allocation pressure, 100 nested call/cc captures, 50-argument macro expansion stress, 1M-iteration TCO in all tail contexts (if/cond/begin/let/and/or/case), 50 nested let bindings with deep lookup, and 10K-char string construction.
