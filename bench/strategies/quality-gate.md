# CS 61A Scheme Interpreter

Implement a Scheme interpreter in Rust. Read `SPEC.md` for the full specification.

## Quality Gate

The `quality-gate` Cargo feature is enabled by default for this strategy, activating compile-time lint enforcement in `src/lib.rs`. `cargo xtask test` enforces code quality checks (clippy lints, mod.rs size limits, etc.) **before** running tests. Violations block testing. Write clean, modular, idiomatic Rust from the start to avoid rework. Read `src/lib.rs` for the active lint configuration.

## Contract

- Implement `eval_str` in `src/scheme/mod.rs`
- Add error variants to `EvalError` in `src/scheme/error.rs` as needed
- You may create any additional modules/files under `src/scheme/`
- Do NOT modify test functions
- Allowed external crates: `thiserror`, `log`, `env_logger` (already in Cargo.toml). Do NOT add any others.
- **NEVER run `cargo test` directly on the host.** Always use `cargo xtask test`. Bare `cargo test` risks infinite loops and OOM that crash the host. This rule has NO exceptions.

## Build & Test

`cargo xtask test` handles everything: clippy auto-fix, clippy verification, mod.rs size check, release build, and containerized test execution. Just run it.

```bash
cargo xtask test 01   # test level 1
cargo xtask test 05   # test level 5
cargo xtask test all  # test all levels (300s timeout)
```

- A level argument is required (e.g., `01`, `19`, or `all`)
- Tests run in a container with 1GB memory, 1 CPU
- Per-level timeout: 30s. Full suite (`all`): 300s. Exceeding these or OOM = failing
- Build the container image first if not already built: `cargo xtask setup`

## Development Strategy

- **Implement levels in order (L1 → L19).** Each level builds on the previous.
- **After implementing each level, run its tests before moving on.**
- **Do not skip ahead.** Later levels depend on earlier ones being correct.
- **If a level's tests fail, fix them before proceeding.**
- **Rely on the provided level tests as the source of truth.**

## Code Style

Write clean, idiomatic Rust. The quality gate enforces structure mechanically; these are additional expectations:

- **Use `thiserror` with structurally typed variants.** Each variant must carry domain-specific fields (e.g., `NotFound { key: String }`, `LimitExceeded { max: usize, actual: usize }`). Variants that wrap a formatted `String` message (e.g., `Other(String)`) are prohibited — they defeat pattern matching and are just `String` with extra steps.
- **Immutable-first.** Build new values from inputs instead of mutating temporaries.
- **Keep `mod.rs` thin.** Entry point and re-exports only; implementation goes in submodules.
- **When touching a file, fix violations in that file.** Do not defer. Do not rewrite unrelated files unprompted.
