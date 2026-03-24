# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### Parser::parse_program (src/scheme/parser.rs:22) — Parse a full Scheme program, skipping whitespace/comments and rejecting empty input.
### Parser::parse_hash_literal (src/scheme/parser.rs:115) — Decode `#t`/`#f` booleans and `#\` character literals with position-aware syntax errors.
### Environment::lookup (src/scheme/environment.rs:26) — Resolve lexical bindings through the parent chain while preserving shared mutable values.
### Value::render_with_mode (src/scheme/value.rs:206) — Centralize normal rendering versus display-mode rendering for every Scheme value type.
### Interpreter::eval (src/scheme/interpreter.rs:51) — Dispatch expression evaluation across atoms, symbol lookup, and list application.
### Interpreter::eval_define (src/scheme/interpreter.rs:96) — Handle variable definitions and function-definition sugar while preserving recursive closures.
### Interpreter::eval_let (src/scheme/interpreter.rs:206) — Evaluate both standard `let` bindings and named-let setup using fresh environments.
### Interpreter::apply_builtin (src/scheme/interpreter.rs:496) — Route evaluated arguments to arithmetic, list, I/O, string, and mutable-string builtins.
### Interpreter::expect_string (src/scheme/interpreter.rs:881) — Enforce string arguments and preserve shared mutable string storage for level 06 mutations.

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
