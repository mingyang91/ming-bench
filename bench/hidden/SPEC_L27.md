### Level 27 — Step-Limited Evaluation
Implement `eval_str_with_limit(input, max_steps)` that evaluates Scheme code with a step budget. Each "step" is one eval dispatch (one expression evaluated). If the budget is exhausted, return an error. This tests whether your evaluator has a central dispatch loop — trampoline-based interpreters add one counter check; deeply recursive interpreters must thread the counter through every call site.

Tests:
- Normal evaluation within budget succeeds.
- Infinite loops (`(let loop () (loop))`) are caught within the step limit.
- Finite loops that exceed a small budget return a step-limit error.
- The step counter must be precise enough that the same program always exhausts at the same limit (±10% tolerance for non-deterministic dispatch).
