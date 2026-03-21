# MING — Ming Interpreter Nurture Gauntlet

This is the benchmark framework that orchestrates coding agent evaluation. The agent workspace lives in `bench/` — do NOT modify test cases or agent instructions without understanding the experimental design.

## Architecture

- **`bench/`** — Agent playground. Contains `SPEC.md` (task definition), `strategies/` (instruction variants), `fixtures/` (shared .scm test files), and `tests.json` (shared test manifest).
- **`bench/rust/`** — Rust interpreter crate (the original language).
- **`bench/go/`** — Go interpreter scaffold.
- **`bench/java/`** — Java interpreter scaffold (Gradle + JUnit 5).
- **`bench/ts/`** — TypeScript interpreter scaffold (vitest).
- **`bench/scala/`** — Scala interpreter scaffold (sbt + munit).
- **`xtask/`** — CLI tooling for orchestration, scoring, token analysis, and monitoring.
- **`Dockerfile.bench`** — Container image for Rust/Go (static binaries).
- **`Dockerfile.jvm`** — Container image for Java/Scala (amazoncorretto:26-headless).
- **`Dockerfile.node`** — Container image for TypeScript (node:latest).
- **`results/`** — Run data (gitignored). Each run gets a timestamped directory.

## Build & Test

```bash
cargo xtask setup              # install podman, build all container images
cargo xtask test 01             # run level 1 tests (Rust, default)
cargo xtask test 01 --lang go   # run level 1 tests in Go
cargo xtask test 01 --lang java # run level 1 tests in Java
cargo xtask test 01 --lang ts   # run level 1 tests in TypeScript
cargo xtask test 01 --lang scala # run level 1 tests in Scala
cargo xtask test 01 --gate      # run with quality gates (Rust only: clippy/mod.rs)
cargo xtask test all --lang go   # run all levels in Go
cargo xtask run-agent --lang go --strategy default --name my-run --mode levels
cargo xtask results              # tabular summary of all runs
cargo xtask tokens --all         # token usage and cost estimates
cargo xtask analyze --all        # request-level cost analysis
cargo xtask watch                # live dashboard of running agents
cargo xtask verify               # ground-truth check against Guile
cargo xtask session-turns <run>  # per-level turn/time/token analysis
cargo xtask session-stats <run>  # quick overview of a run
cargo xtask session-dump <run>   # dump session content (--thinking, --text, --tools)
cargo xtask session-grep <run> <keywords...>  # keyword search across session
cargo xtask session-tools <run>  # tool call timeline (--summary)
cargo xtask compare <run1> <run2> # side-by-side run comparison
```

## Multi-Language Support

Each language has a self-contained directory under `bench/`:

| Language | Directory | Build System | Test Framework | Container |
|----------|-----------|-------------|----------------|-----------|
| Rust | `bench/rust/` | Cargo | built-in `#[test]` | `ming` |
| Go | `bench/go/` | Go modules | `testing` | `ming` |
| Java | `bench/java/` | Gradle | JUnit 5 | `ming-jvm` |
| TypeScript | `bench/ts/` | npm/tsc | vitest | `ming-node` |
| Scala | `bench/scala/` | sbt | munit | `ming-jvm` |

**Shared resources:**
- `bench/SPEC.md` — Language-agnostic interpreter specification (27 levels)
- `bench/fixtures/*.scm` — Test fixture files (Scheme source code)
- `bench/tests.json` — Test manifest mapping test names to fixtures and expected values

**Per-language test harnesses** read `tests.json` + fixtures at test time. Rust keeps its original `include_str!()` pattern with a symlink to shared fixtures.

**Per-language scripts:** Each language has `build.sh` and `test.sh` in its directory. The xtask orchestrator calls these for non-Rust languages.

## Adding an xtask Command

1. Create `xtask/src/cmd/<name>.rs` with a `pub fn run(...) -> Result<()>`
2. Add `pub mod <name>;` to `xtask/src/cmd/mod.rs`
3. Add a variant to `Commands` enum in `xtask/src/main.rs`
4. Wire it in the `match` block in `main()`

Shared types and helpers live in `xtask/src/model.rs`. The `Lang` enum in model.rs handles language dispatch.

## Strategy System

Strategies live in `bench/strategies/`. Per-language strategies live in `bench/strategies/{lang}/`.

- **`default.md`** — Base strategy (Rust-specific for backwards compat).
- **`quality-gate.md`** — Adds: code style rules (Rust-specific).
- **`{lang}/default.md`** — Per-language default strategy (e.g., `go/default.md`, `java/default.md`).

The orchestrator picks `strategies/{lang}/{strategy}.md` if it exists, otherwise falls back to `strategies/{strategy}.md`.

`bench/SPEC.md` is the shared task spec (identical for all strategies and languages). `bench/{lang}/CLAUDE.md` and `bench/{lang}/AGENTS.md` are gitignored — created as symlinks by `run-agent --strategy <name> --lang <lang>`.

**Lint enforcement via xtask:** All clippy lint flags live in `GATE_LINT_FLAGS` in `xtask/src/cmd/test_level.rs`. The `--gate` flag on `cargo xtask test` activates clippy + mod.rs size checks (Rust only). For other languages, `--gate` is passed to the language's `test.sh`.

To add a new strategy: create `bench/strategies/<lang>/<name>.md` and use `--strategy <name> --lang <lang>`.

**Two-pass testing (quality-gate):** In levels mode, `run-agent` splits each quality-gate level into two agent invocations: (1) coding pass with plain `cargo xtask test` (no gate, same turn budget as default), (2) cleanup pass with `cargo xtask test --gate` (fresh session, 15 turns). The agent never sees clippy during coding — the orchestrator controls when quality checks run. Default strategy uses a single pass (no gate).

## Session Analysis

Session analysis tools parse Claude Code JSONL sessions from `results/`. The shared parser lives in `xtask/src/session.rs`. Run names support fuzzy matching (e.g., `cl-def-lvl` matches the full timestamped directory).

- **`session-turns`** — Per-level turns, time, output tokens, test runs, gate friction. Core analysis struct `LevelAnalysis`/`RunAnalysis` in `cmd/session_turns.rs` is reused by `compare`.
- **`compare`** — Side-by-side table of two runs. Uses `analyze_run()` from `session_turns.rs`.
- **`/compare` skill** — Guides narrative analysis: runs compare, identifies struggle levels, reads thinking blocks, produces verdict.
- **`/compliance` skill** — Analyzes whether an agent followed its strategy rules.

Turn limits: L01-L03 = 45, L04-L06 = 30, L07-L09 = 45, L10-L12 = 90, L13-L14 = 45, L15 = 60, L16-L18 = 60, L19-L20 = 75, L21-L23 = 90, L24-L27 = 60. Quality-gate levels get an additional 15-turn cleanup pass. Failed levels auto-retry up to 2 times if the failure was infrastructure (timeout/529/crash), not turns exhaustion.

## Key Conventions

- Tests NEVER run on the host — always containerized via `cargo xtask test`
- Agent worktrees are created at `<repo>/../workspace/<name>`
- Agent cwd is set to `<worktree>/bench/{lang}/` so agents only see their language's playground
- `project_dir()` in `model.rs` finds the workspace root by walking up for `[workspace]` in Cargo.toml
- `Lang` enum in `model.rs` handles language-specific paths, container images, and dispatch
