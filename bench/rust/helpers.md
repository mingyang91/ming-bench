# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### parse_expr (src/scheme/mod.rs:79) — Parse the next Scheme atom or list expression from the input stream
### eval_application (src/scheme/mod.rs:264) — Evaluate list forms by dispatching short-circuit special forms and builtins
### apply_builtin (src/scheme/mod.rs:309) — Execute level 01 builtin arithmetic, comparison, and boolean procedures
### extract_numbers (src/scheme/mod.rs:406) — Validate evaluated arguments as integers and report numeric type errors

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
