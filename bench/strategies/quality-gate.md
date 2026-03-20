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

- A level argument is required (e.g., `01`, `25`, or `all`)
- Tests run in a container with 1GB memory, 1 CPU
- Per-level timeout: 30s. Full suite (`all`): 300s. Exceeding these or OOM = failing
- Build the container image first if not already built: `cargo xtask setup`

## Development Strategy

- **Implement levels in order (L1 → L25).** Each level builds on the previous.
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

## Lint Reference

These lints are enforced in a **separate cleanup pass** at the end of each level — not during coding. Focus on making tests pass first. You will get a dedicated session to fix lint violations afterward. This table is a quick-fix reference for that cleanup session.

### Denied (hard errors)

| Denied pattern | Use instead |
|---|---|
| `.unwrap()` | `.expect("reason")`, `?`, or `.ok_or()` |
| `Result<T, ()>` | A meaningful error type |
| `if !cond { panic!(...) }` | `assert!(cond, ...)` |

### Structural limits

| Constraint | L1-L5 | L6+ |
|---|---|---|
| Max function length | 80 lines | 60 lines |
| Max nesting depth | 3 | 3 |
| Max `mod.rs` size | 300 lines (L1-L3), 200 (L4-L6) | 100 lines |

### Style (auto-fixable)

| Flagged pattern | Use instead |
|---|---|
| `.filter().map()` | `.filter_map()` |
| `.find().map()` | `.find_map()` |
| `.map().flatten()` | `.flat_map()` |
| `for i in 0..v.len()` | `for item in &v` or `.enumerate()` |
| `for x in v.iter()` | `for x in &v` |
| `let mut v = vec![]; v.push(x)` | `vec![x]` |
| `.collect()` then `.iter()` | Chain iterators directly |
| `format!("{}", x)` | `format!("{x}")` |
| `let x = if let Some(v) = y { v } else { return }` | `let Some(x) = y else { return }` |
