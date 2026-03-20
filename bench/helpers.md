# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### start_sequence (src/scheme/evaluator.rs:52) — Schedule left-to-right sequence evaluation while preserving tail position for the final expression
### continue_cond (src/scheme/evaluator.rs:322) — Advance through validated `cond` clauses with resumable predicate evaluation
### finish_apply_arguments (src/scheme/evaluator.rs:775) — Resume a pending callable application after one argument value has been produced
### finish_let_bindings (src/scheme/evaluator.rs:802) — Resume `let` binding evaluation and enter the body once all values are ready
### start_argument_evaluation (src/scheme/evaluator.rs:882) — Evaluate application arguments right-to-left into continuation frames that `call/cc` can capture

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
