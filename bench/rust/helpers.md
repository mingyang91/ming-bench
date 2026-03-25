# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### builtin_min_max (src/scheme/mod.rs:1699) — Shared reducer for variadic `min` and `max`
### builtin_string_compare (src/scheme/mod.rs:1753) — Normalizes and compares variadic string arguments
### list_tail_at (src/scheme/mod.rs:1791) — Walks a list/pair chain to the requested tail with index errors
### equal_values (src/scheme/mod.rs:1816) — Structural equality helper used by `assoc`
### expect_two_numbers (src/scheme/mod.rs:1921) — Validates binary numeric operations and handles division by zero

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
