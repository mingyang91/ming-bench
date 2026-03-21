# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### eval_case_clauses (src/scheme/mod.rs:1305) — Walk `case` clauses, compare quoted datums with `eqv?`, and dispatch the matching body or void.
### values_eqv (src/scheme/mod.rs:2260) — Implement `eq?`/`eqv?` semantics across scalar, procedure, string, vector, and void values.
### values_equal (src/scheme/mod.rs:2279) — Perform deep structural equality across lists, vectors, strings, and scalar values.
### expand_recursive_bindings (src/scheme/expand.rs:611) — Pre-bind recursive identifiers so `letrec` initializers expand in an environment that can see all bindings.
### expand_template_letrec (src/scheme/expand.rs:1249) — Hygienically expand `letrec` and `letrec*` macro templates with the right recursive binding visibility.
### expand_template_case (src/scheme/expand.rs:1303) — Expand `case` templates without rewriting literal datum lists while still expanding clause bodies.

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
