# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### eval (src/scheme/mod.rs:166) — Evaluate a positioned Scheme AST node and attach source positions to runtime errors.
### eval_control (src/scheme/mod.rs:792) — Evaluate continuation-aware expressions with explicit continuations for `call/cc` and `dynamic-wind`.
### eval_and_control (src/scheme/mod.rs:882) — Evaluate `and` with short-circuit behavior inside the continuation-aware interpreter path.
### eval_or_control (src/scheme/mod.rs:933) — Evaluate `or` with short-circuit behavior inside the continuation-aware interpreter path.
### eval_cond_control (src/scheme/mod.rs:984) — Evaluate `cond` clauses within control mode while preserving test-result return semantics.
### eval_macro_invocation_control (src/scheme/mod.rs:1133) — Expand `syntax-rules` macros and evaluate the expansion under the continuation-aware evaluator.
### expand_let_control (src/scheme/mod.rs:1195) — Rewrite plain and named `let` forms into lambda-based expressions consumable by control evaluation.
### transfer_continuation_control (src/scheme/mod.rs:1755) — Reconcile dynamic-wind frames when invoking a captured continuation, including unwind and re-entry thunks.
### builtin_apply_control (src/scheme/mod.rs:1611) — Execute `apply` through continuation-aware procedure dispatch so expanded calls still support continuations.
### builtin_map_control (src/scheme/mod.rs:1662) — Run `map` using continuation-aware application for each element-wise invocation.
### parse_parameters_from_expr (src/scheme/mod.rs:1888) — Normalize lambda parameter syntax into fixed/rest argument metadata.
### apply (src/scheme/mod.rs:1952) — Dispatch builtin and lambda procedure calls with call-site position tracking.
### equal_values (src/scheme/mod.rs:3296) — Implement recursive Scheme equality for strings, pairs, and atomic values.
### parse_program (src/scheme/mod.rs:4304) — Parse a full Scheme source string into a sequence of positioned expressions.
### render_value (src/scheme/mod.rs:4670) — Render Scheme values for final results and display/write output modes.

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
