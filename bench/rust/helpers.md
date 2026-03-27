# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### eval_sequence (src/scheme/mod.rs:1495) — Evaluate a sequence of expressions in one environment and return the last value
### continue_raise_after_exit (src/scheme/mod.rs:2371) — Unwind active dynamic-wind contexts while propagating an exception to the nearest handler
### pack_values (src/scheme/mod.rs:2588) — Collapse zero, one, or many procedure results into the interpreter’s multi-value representation
### normalize_number (src/scheme/mod.rs:2704) — Reduce an exact rational to lowest terms and collapse denominator-1 values back to integers
### is_equal (src/scheme/mod.rs:2771) — Compare Scheme values structurally for the `equal?` builtin across pairs and vectors
### proper_list_length (src/scheme/mod.rs:2823) — Validate a proper list and count its elements for list primitives
### append_lists (src/scheme/mod.rs:2855) — Implement Scheme `append` over zero or more list arguments
### parse_bindings (src/scheme/mod.rs:2895) — Validate `let` and `letrec` binding syntax and extract name/expression pairs
### desugar_guard (src/scheme/mod.rs:2932) — Lower `guard` into an internal protected-thunk form that cooperates with the machine exception handler
### desugar_do (src/scheme/mod.rs:2987) — Lower `do` into a named `let` loop with parallel step expressions
### eval_binding_values (src/scheme/mod.rs:3082) — Evaluate `let` binding expressions in the outer environment
### make_proper_list (src/scheme/mod.rs:3101) — Build a Scheme list value from an iterator of already-evaluated items

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
