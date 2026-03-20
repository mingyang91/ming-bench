# CS 61A Benchmark Framework (Jug)

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
cargo xtask test all         # run all levels
cargo xtask run-agent ...    # orchestrate a full agent run
cargo xtask results          # tabular summary of all runs
cargo xtask tokens --all     # token usage and cost estimates
cargo xtask analyze --all    # request-level cost analysis
cargo xtask watch            # live dashboard of running agents
cargo xtask verify           # ground-truth check against Guile
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
- **`quality-gate.md`** — Adds: clippy enforcement, mod.rs size limits, code style rules. Activates the `quality-gate` Cargo feature.

`bench/SPEC.md` is the shared task spec (identical for all strategies). `bench/CLAUDE.md` and `bench/AGENTS.md` are gitignored — created as symlinks by `run-agent --strategy <name>`.

**Lint enforcement via Cargo feature:** `bench/src/lib.rs` uses `#![cfg_attr(feature = "quality-gate", ...)]` for all lint attributes. The `quality-gate` feature is off by default (`default = []` in Cargo.toml). When a strategy includes `clippy.toml`, `run-agent` patches the worktree's Cargo.toml to set `default = ["quality-gate"]`, enabling lints for all cargo commands the agent runs.

To add a new strategy: create `bench/strategies/<name>.md` and use `--strategy <name>`. To enable lint enforcement, also add a `clippy.toml` in the bench directory for that strategy.

## Key Conventions

- Tests NEVER run on the host — always containerized via `cargo xtask test`
- Agent worktrees are created at `<repo>/../workspace/<name>`
- Agent cwd is set to `<worktree>/bench/` so agents only see the playground
- `project_dir()` in `model.rs` finds the workspace root by walking up for `[workspace]` in Cargo.toml
