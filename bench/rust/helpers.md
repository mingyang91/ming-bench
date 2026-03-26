# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### eval_application (src/scheme/mod.rs:203) — Dispatch a parsed Scheme list to the supported level 01 builtins
### eval_number_args (src/scheme/mod.rs:339) — Evaluate argument expressions and coerce them into integer operands for numeric builtins
### parse_program (src/scheme/mod.rs:54) — Parse one or more top-level Scheme expressions while skipping whitespace and comments
### parse_string (src/scheme/mod.rs:102) — Decode a Scheme string literal with basic escape handling

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
