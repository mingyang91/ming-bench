# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### expand_guard_form (src/scheme/mod.rs:346) — Lower `guard` into `call/cc` plus `with-exception-handler`
### raise_cps (src/scheme/continuation_runtime.rs:251) — Unwind to the active exception handler and apply it
### apply_with_exception_handler_cps (src/scheme/continuation_runtime.rs:283) — Install a dynamic exception handler around a thunk in CPS

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
