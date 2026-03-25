# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### eval_list (src/scheme/mod.rs:533) — Dispatch special forms or procedure applications for parsed list expressions
### apply_builtin (src/scheme/mod.rs:859) — Implement the level-14 builtin procedure surface, including vectors and output forms
### parse_bindings (src/scheme/mod.rs:1148) — Validate and extract `(name value)` binding lists for `let` and `letrec`
### collect_list (src/scheme/mod.rs:1311) — Walk a proper list into a Rust vector while preserving Scheme list validation
### format_value (src/scheme/mod.rs:1377) — Render runtime values into Scheme output syntax for `eval_str`, `write`, and vectors

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
