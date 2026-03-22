# MING Scheme Interpreter

Implement a Scheme interpreter in Rust. Read `SPEC.md` for the full specification.

## Contract

- Implement `eval_str` in `src/scheme/mod.rs`
- Add error variants to `EvalError` in `src/scheme/error.rs` as needed
- You may create any additional modules/files under `src/scheme/`
- Do NOT modify test functions
- Allowed external crates: `thiserror`, `log`, `env_logger` (already in Cargo.toml). Do NOT add any others.
- **No `thread_local!` or `std::thread_local`.** All state must be passed explicitly through function parameters.
- **NEVER run `cargo test` directly on the host.** Always use `cargo xtask test`. This rule has NO exceptions.

## Build & Test

```bash
cargo xtask test 01   # test level 1
cargo xtask test all  # test all levels (300s timeout)
```

Tests run in a container (1GB memory, 1 CPU). Per-level: 30s. Full suite: 300s.

## Development Strategy

- **Implement levels in order (L1 → L27).** Each level builds on the previous.
- **After implementing each level, run its tests before moving on.**
- **If a level's tests fail, fix them before proceeding.**
- **Code first, debug from test output.** Don't mentally simulate — let the test runner do that.

## Type Safety

- **No `.unwrap()`.** Use `.expect("reason")` for trusted invariants, `?` for untrusted paths.
- **No silent error swallowing.** No `.unwrap_or_default()`, `.ok()` to discard errors.
- **Structured error types** via `thiserror`. Each variant carries domain-specific fields. No `Other(String)` catch-all variants.
- **Enums over booleans** for behavioral flags.
- **Exhaustive `match` — no `_ =>` catch-all.** When you add a new enum variant, the compiler must flag every match site. The blast radius of a type change IS the safety net.

## Refactoring Philosophy

- **Expanding blast radius to reduce tech debt is encouraged.** If adding a field to an enum touches 15 match sites, do it. The upfront cost is lower than compounding workarounds.
- **Fix violations in touched files.** When modifying a file, fix issues in that file — don't defer.
- **Proactive refactoring over workarounds.** If existing code doesn't accommodate a new feature cleanly, refactor the existing code rather than hacking around it.

## Structural Limits

| Constraint | Limit |
|---|---|
| Function length | 300 lines |
| Nesting depth | 6 levels |
| `mod.rs` impl lines | 500 |

## Lint Quick Reference

These are enforced mechanically during `cargo xtask test`. Hard errors:

| Denied | Use instead |
|---|---|
| `.unwrap()` | `.expect("reason")`, `?`, `.ok_or()` |
| `Result<T, ()>` | Meaningful error type |
| `if !cond { panic!() }` | `assert!(cond, ...)` |
| `thread_local!` | Pass state via parameters |
