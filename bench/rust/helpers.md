# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### eval_list (src/scheme/mod.rs:153) — Dispatch a parsed list form to special-form evaluation or procedure application
### eval_define (src/scheme/mod.rs:182) — Handle both variable definitions and function-definition shorthand with shared closure environments
### parse_bindings (src/scheme/mod.rs:388) — Validate and normalize `let` bindings into reusable name/value pairs
### append_lists (src/scheme/mod.rs:898) — Implement Scheme `append` by copying all but the last argument list onto the final tail value
### append_list_contents (src/scheme/mod.rs:1206) — Render proper and dotted pair/list values into Scheme list syntax
### parse_expr (src/scheme/mod.rs:1276) — Parse a single Scheme expression with source-position tracking for syntax errors

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
