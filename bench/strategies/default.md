# MING Scheme Interpreter

Implement a Scheme interpreter in Rust. Read `SPEC.md` for the full specification.

## Contract

- Implement `eval_str` in `src/scheme/mod.rs`
- Add error variants to `EvalError` in `src/scheme/error.rs` as needed
- You may create any additional modules/files under `src/scheme/`
- Do NOT modify test functions
- Allowed external crates: `thiserror`, `log`, `env_logger` (already in Cargo.toml). Do NOT add any others.
- **NEVER run `cargo test` directly on the host.** Always use `cargo xtask test`. Bare `cargo test` risks infinite loops and OOM that crash the host. This rule has NO exceptions.

## Build & Test

`cargo xtask test` handles everything: release build and containerized test execution. Just run it.

```bash
cargo xtask test 01   # test level 1
cargo xtask test 05   # test level 5
cargo xtask test all  # test all levels (300s timeout)
```

- A level argument is required (e.g., `01`, `15`, or `all`)
- Tests run in a container with 1GB memory, 1 CPU
- Per-level timeout: 30s. Full suite (`all`): 300s. Exceeding these or OOM = failing
- Build the container image first if not already built: `cargo xtask setup`

## Development Strategy

- **Implement levels in order (L1 → L26).** Each level builds on the previous.
- **After implementing each level, run its tests before moving on.**
- **If a level's tests fail, fix them before proceeding.**
- **Code first, debug from test output.** Don't mentally simulate — let the test runner do that.
- **Maximum 10 turns of reading before first code change.** If you haven't written or edited a file by turn 10, your analysis is too deep — write a first attempt and iterate from test failures. Tests are the source of truth, not mental simulation.
