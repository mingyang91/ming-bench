# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### evaluate_list (src/scheme/mod.rs:410) — Dispatch a list expression as a special form or procedure call
### apply_procedure (src/scheme/mod.rs:662) — Apply builtin or closure values with arity checks and lexical scoping
### parse_let_bindings (src/scheme/mod.rs:709) — Validate and decode `let` binding pairs into binding records
### expect_proper_list (src/scheme/mod.rs:817) — Traverse a Scheme list and reject dotted or malformed tails

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
