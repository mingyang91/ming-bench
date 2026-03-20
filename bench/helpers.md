# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### parse_program (src/scheme/parser.rs:4) — Parse one input string into a vector of Scheme expressions.
### expand (src/scheme/macros.rs:71) — Recursively expand syntax-rules macros while preserving captured identifiers.
### continue_program (src/scheme/evaluator.rs:311) — Interleave top-level macro installation, expansion, and evaluation across a whole program.
### apply_builtin (src/scheme/evaluator.rs:684) — Dispatch and execute Scheme builtin procedures, including `apply` and `call/cc`.
### render_value (src/scheme/runtime.rs:221) — Convert runtime values back into the exact output format expected by the tests.

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
