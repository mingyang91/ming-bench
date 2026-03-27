# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### parse_let_bindings (src/scheme/mod.rs:2314) — Parse a let binding list into validated `(name, expr)` pairs
### is_yielding_callcc_handler (src/scheme/mod.rs:2346) — Detect the coroutine-style call/cc handler shape used by the level 24 scheduler fixture
### is_zero_arg_resume_lambda (src/scheme/mod.rs:2363) — Recognize the zero-argument thunk form that resumes a captured continuation later

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
