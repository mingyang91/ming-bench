# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### is_equal (src/scheme/equality.rs:27) — Perform recursive structural equality across lists, vectors, strings, and scalar values
### start_recursive_let (src/scheme/evaluator/runtime.rs:452) — Drive sequential initialization and body entry for `letrec` and `letrec*`
### find_matching_case_clause (src/scheme/evaluator/runtime.rs:1559) — Select the first `case` clause whose datums are `eqv?` to the evaluated key
### vector_index (src/scheme/builtins.rs:868) — Validate and convert Scheme vector indices with structured bounds errors

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
