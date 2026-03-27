# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### eval_sequence (src/scheme/mod.rs:515) — Evaluate a sequence of expressions in one environment and return the last value
### proper_list_length (src/scheme/mod.rs:577) — Validate a proper list and count its elements for list primitives
### append_lists (src/scheme/mod.rs:593) — Implement Scheme `append` over zero or more list arguments
### parse_bindings (src/scheme/mod.rs:633) — Validate `let` binding syntax and extract name/expression pairs
### eval_binding_values (src/scheme/mod.rs:650) — Evaluate `let` binding expressions in the outer environment
### make_proper_list (src/scheme/mod.rs:667) — Build a Scheme list value from an iterator of already-evaluated items

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
