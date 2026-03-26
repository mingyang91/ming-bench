# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### eval_list (src/scheme/mod.rs:67) — Dispatch list evaluation across special forms and procedure application.
### eval_define (src/scheme/mod.rs:99) — Handle variable and function definitions, including recursive placeholders.
### eval_let (src/scheme/mod.rs:206) — Evaluate regular and named `let` forms with the correct binding environment.
### eval_cond (src/scheme/mod.rs:264) — Walk `cond` clauses, including `else`, and return the selected branch value.
### eval_sequence (src/scheme/mod.rs:321) — Evaluate a sequence of expressions and return the last result.
### parse_bindings (src/scheme/mod.rs:366) — Validate `let` binding lists and convert them into internal binding specs.
### quote_to_value (src/scheme/mod.rs:415) — Convert quoted syntax trees into runtime Scheme values.
### require_proper_list (src/scheme/mod.rs:479) — Validate proper lists and collect their elements for list operations.
### render_pair (src/scheme/mod.rs:518) — Render pairs and dotted lists into Scheme surface syntax.

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
