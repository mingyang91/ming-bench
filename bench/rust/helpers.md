# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### apply_procedure (src/scheme/mod.rs:705) — Invoke builtins and closures with fixed/rest arity checks and lexical environments
### tokenize (src/scheme/mod.rs:955) — Convert Scheme source into positioned tokens with comment, string, char, and quote handling
### parse_formal_parameter_list (src/scheme/mod.rs:1297) — Parse fixed and dotted rest-parameter lists for lambdas and function defines
### list_to_vec (src/scheme/mod.rs:1370) — Validate a proper list and collect its elements for builtins like length, append, and apply

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
