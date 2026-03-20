# MING — Ming Interpreter Nurture Gauntlet

This is the benchmark framework that orchestrates coding agent evaluation. The agent workspace lives in `bench/` — do NOT modify test cases or agent instructions without understanding the experimental design.

## Architecture

- **`bench/`** — Agent playground. Contains `SPEC.md` (task definition), `strategies/` (instruction variants), and the interpreter crate. Agents only see this directory.
- **`xtask/`** — CLI tooling for orchestration, scoring, token analysis, and monitoring.
- **`Dockerfile.bench`** — Container image for sandboxed test execution.
- **`results/`** — Run data (gitignored). Each run gets a timestamped directory.

## Build & Test

```bash
cargo xtask setup            # install podman, build container image
cargo xtask test 01          # run level 1 tests (containerized)
cargo xtask test 01 --gate   # run with quality gates (clippy/mod.rs)
cargo xtask test all         # run all levels
cargo xtask run-agent ...    # orchestrate a full agent run
cargo xtask results          # tabular summary of all runs
cargo xtask tokens --all     # token usage and cost estimates
cargo xtask analyze --all    # request-level cost analysis
cargo xtask watch            # live dashboard of running agents
cargo xtask verify           # ground-truth check against Guile
cargo xtask session-turns <run>  # per-level turn/time/token analysis
cargo xtask session-stats <run>  # quick overview of a run
cargo xtask session-dump <run>   # dump session content (--thinking, --text, --tools)
cargo xtask session-grep <run> <keywords...>  # keyword search across session
cargo xtask session-tools <run>  # tool call timeline (--summary)
cargo xtask compare <run1> <run2> # side-by-side run comparison
```

## Adding an xtask Command

1. Create `xtask/src/cmd/<name>.rs` with a `pub fn run(...) -> Result<()>`
2. Add `pub mod <name>;` to `xtask/src/cmd/mod.rs`
3. Add a variant to `Commands` enum in `xtask/src/main.rs`
4. Wire it in the `match` block in `main()`

Shared types and helpers live in `xtask/src/model.rs`.

## Strategy System

Strategies live in `bench/strategies/`. Each is a `.md` file that becomes the agent's `CLAUDE.md` at launch via symlink.

- **`default.md`** — Minimal: build the interpreter, test each level. No lint enforcement.
- **`quality-gate.md`** — Adds: code style rules (thiserror, immutable-first, thin mod.rs). Uses two-pass testing: coding pass (tests only), then cleanup pass (`--gate`, fresh session, 15 turns). The agent never sees clippy during coding.

`bench/SPEC.md` is the shared task spec (identical for all strategies). `bench/CLAUDE.md` and `bench/AGENTS.md` are gitignored — created as symlinks by `run-agent --strategy <name>`.

**Lint enforcement via xtask:** All clippy lint flags live in `GATE_LINT_FLAGS` in `xtask/src/cmd/test_level.rs`. The `--gate` flag on `cargo xtask test` activates clippy + mod.rs size checks. Without `--gate`, tests run without any lint enforcement. The agent's `cargo xtask test` is lint-free by default — lints only fire with `--gate`.

To add a new strategy: create `bench/strategies/<name>.md` and use `--strategy <name>`. To enable lint enforcement, also add a `<name>.clippy.toml` in `bench/strategies/`.

**Two-pass testing (quality-gate):** In levels mode, `run-agent` splits each quality-gate level into two agent invocations: (1) coding pass with plain `cargo xtask test` (no gate, same turn budget as default), (2) cleanup pass with `cargo xtask test --gate` (fresh session, 15 turns). The agent never sees clippy during coding — the orchestrator controls when quality checks run. Default strategy uses a single pass (no gate).

## Session Analysis

Session analysis tools parse Claude Code JSONL sessions from `results/`. The shared parser lives in `xtask/src/session.rs`. Run names support fuzzy matching (e.g., `cl-def-lvl` matches the full timestamped directory).

- **`session-turns`** — Per-level turns, time, output tokens, test runs, gate friction. Core analysis struct `LevelAnalysis`/`RunAnalysis` in `cmd/session_turns.rs` is reused by `compare`.
- **`compare`** — Side-by-side table of two runs. Uses `analyze_run()` from `session_turns.rs`.
- **`/compare` skill** — Guides narrative analysis: runs compare, identifies struggle levels, reads thinking blocks, produces verdict.
- **`/compliance` skill** — Analyzes whether an agent followed its strategy rules.

Turn limits: L01-L09 = 40 turns, L10+ = 60 turns. Quality-gate levels get an additional 15-turn cleanup pass.

## Key Conventions

- Tests NEVER run on the host — always containerized via `cargo xtask test`
- Agent worktrees are created at `<repo>/../workspace/<name>`
- Agent cwd is set to `<worktree>/bench/` so agents only see the playground
- `project_dir()` in `model.rs` finds the workspace root by walking up for `[workspace]` in Cargo.toml
