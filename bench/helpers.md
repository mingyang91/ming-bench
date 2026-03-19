# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### fmt_string_contents (src/scheme/engine.rs:76) — Render Scheme string contents with escapes preserved in output
### drive (src/scheme/engine.rs:387) — Advance the evaluator by one explicit-machine step from either an expression or a value
### resume_cond (src/scheme/engine.rs:562) — Resume a `cond` continuation by either advancing to later clauses or running the chosen clause body
### schedule_sequence (src/scheme/engine.rs:840) — Evaluate a Scheme body left-to-right and preserve the outer continuation for the final expression
### schedule_cond (src/scheme/engine.rs:897) — Start or continue `cond` clause evaluation with explicit continuation frames
### schedule_argument_evaluation (src/scheme/engine.rs:935) — Evaluate application arguments from right to left so continuations can replay pending argument work
### continue_argument_evaluation (src/scheme/engine.rs:955) — Rebuild an application’s argument vector as resumed argument values arrive
### continue_call (src/scheme/engine.rs:984) — Dispatch a callable Scheme value across builtins, procedures, and captured continuations
### call_builtin (src/scheme/engine.rs:1002) — Handle builtins that need evaluator participation such as `apply` and `call/cc`
### parse_let_bindings (src/scheme/engine.rs:1080) — Split `let` bindings into parameter names and initializer expressions for lambda-style evaluation
### skip_ignored (src/scheme/engine.rs:1590) — Skip parser whitespace and line comments until the next Scheme token
### skip_whitespace (src/scheme/engine.rs:1594) — Consume a contiguous run of whitespace and report whether input advanced
### skip_line_comment (src/scheme/engine.rs:1604) — Consume one `;` line comment and report whether a comment was skipped

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
