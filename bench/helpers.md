# Helpers Registry

Canonical registry of extracted helpers (>= 5 ops).

## Rules
- **Before writing any new helper:** search this file for an existing one that does what you need. Reuse it.
- **After creating any new helper:** add an entry here immediately. This is mandatory, not optional.
- **Format:** `### helper_name (file_path:line) — one-line purpose`
- **When to update:** If a helper's signature, location, or purpose changes, update its entry. If deleted, remove the entry.

## Rust Helpers (src/scheme/)

### fmt_string_contents (src/scheme/engine.rs:71) — Render Scheme string contents with escapes preserved in output
### skip_ignored (src/scheme/engine.rs:819) — Skip parser whitespace and line comments until the next Scheme token
### skip_whitespace (src/scheme/engine.rs:823) — Consume a contiguous run of whitespace and report whether input advanced
### skip_line_comment (src/scheme/engine.rs:833) — Consume one `;` line comment and report whether a comment was skipped

## Python Helpers (scripts/)

### load_session_metrics (session_metrics.py) — Deduplicate streamed session.jsonl assistant usage by request id and capture request-level tool/text/thinking metrics
### build_run_summary (analyze-session.py) — Aggregate one full-run or levels-run results directory into request-level cost and behavior metrics
