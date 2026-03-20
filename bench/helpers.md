# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### eval_list (src/scheme/interpreter.rs:517) — Expand macros, dispatch special forms, or start generic procedure application for a list expression
### resume (src/scheme/interpreter.rs:964) — Advance the explicit continuation machine after a value is produced
### expand_template (src/scheme/interpreter.rs:1588) — Expand a `syntax-rules` template with hygiene-aware identifier handling
### parse_formals (src/scheme/interpreter.rs:2387) — Parse lambda or function-definition formals, including dotted rest arguments
### proper_list_to_vec (src/scheme/interpreter.rs:2683) — Convert a proper Scheme list value into a Rust vector with validation
### procedure_return_cont (src/scheme/interpreter.rs:2810) — Find the nearest procedure boundary continuation for `call/cc`-driven suspension

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
