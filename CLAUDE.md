# CS 61A Benchmark Framework (Jug)

This is the benchmark framework that orchestrates coding agent evaluation. The agent workspace lives in `bench/` — do NOT modify test cases or agent instructions without understanding the experimental design.

## Architecture

- **`bench/`** — Agent playground. Contains the Scheme interpreter crate, test suite, and agent-facing `CLAUDE.md`. Agents only see this directory.
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

## Two-Branch Experiment

- **`main`** — Minimal agent instructions in `bench/CLAUDE.md`
- **`strategy`** — Enhanced instructions with quality gates and code style rules

Only `bench/CLAUDE.md` differs between branches. Framework code (xtask, Dockerfile) is identical. When making framework changes, commit to one branch and cherry-pick to the other.

## Key Conventions

- Tests NEVER run on the host — always containerized via `cargo xtask test`
- Agent worktrees are created at `<repo>/../workspace/<name>`
- Agent cwd is set to `<worktree>/bench/` so agents only see the playground
- `project_dir()` in `model.rs` finds the workspace root by walking up for `[workspace]` in Cargo.toml
