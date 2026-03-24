# Quality Gate — Cleanup Pass

All tests for this level already pass. Your job is to **refactor the code to production quality** while keeping all tests green.

## What To Do

1. Run `cargo xtask test <level> --lang rust --gate` to see quality-gate violations
2. Fix ALL violations — lints, nesting depth, function length, module size
3. **Radical refactoring is encouraged.** Extract modules, split large functions, flatten nesting, improve abstractions. The code works — make it maintainable.
4. Run `cargo xtask test all --lang rust` to verify no regressions
5. Run `cargo xtask test <level> --lang rust --gate` again to confirm all violations are fixed

## Contract

- You may create any additional modules/files under `src/scheme/`
- Do NOT modify test functions
- Allowed external crates: `thiserror`, `log`, `env_logger` (already in Cargo.toml). Do NOT add any others.
- **No `thread_local!` or `std::thread_local`.** All state must be passed explicitly through function parameters.
- **Local `let mut` is permitted.** Prefer immutable bindings, but `let mut` inside a function body is fine for loop control and local accumulation. Mutable state must never escape the function.
- **NEVER run `cargo test` directly on the host.** Always use `cargo xtask test`.

## Build & Test

```bash
cargo xtask test 01 --gate   # test level 1 with quality gate
cargo xtask test all          # test all levels (300s timeout)
```

Tests run in a container (1GB memory, 1 CPU). Per-level: 30s. Full suite: 300s.

## Refactoring Philosophy

- **Expanding blast radius to reduce tech debt is encouraged.** If adding a field to an enum touches 15 match sites, do it. The upfront cost is lower than compounding workarounds.
- **Fix violations in touched files.** When modifying a file, fix issues in that file — don't defer.
- **Proactive refactoring over workarounds.** If existing code doesn't accommodate a new feature cleanly, refactor the existing code rather than hacking around it.
- **Extract modules aggressively.** If `mod.rs` exceeds 1500 lines, split it. Move evaluator, parser, builtins, macros, and continuations into separate files.
- **Clear module boundaries.** Each module should have a single responsibility and a clean public API.

## Type Safety

- **No `.unwrap()`.** Use `.expect("reason")` for trusted invariants, `?` for untrusted paths.
- **No silent error swallowing.** No `.unwrap_or_default()`, `.ok()` to discard errors.
- **Structured error types** via `thiserror`. Each variant carries domain-specific fields. No `Other(String)` catch-all variants.

  ```rust
  // Bad — stringly typed
  Err(EvalError::Other(format!("not found: {}", key)))

  // Good — structured variant
  Err(EvalError::UnboundVariable { name: key.to_string() })
  ```

- **Enums over booleans** for behavioral flags.
- **Exhaustive `match` — no `_ =>` catch-all.** When you add a new enum variant, the compiler must flag every match site.

  ```rust
  // Bad — silently ignores new variants
  match value {
      Value::Int(n) => ...,
      Value::Bool(b) => ...,
      _ => Err(Error::TypeMismatch),
  }

  // Good — compiler forces handling new variants
  match value {
      Value::Int(n) => ...,
      Value::Bool(b) => ...,
      Value::String(s) => Err(Error::TypeMismatch),
      Value::List(l) => Err(Error::TypeMismatch),
  }
  ```

## Code Style

### Flat Control Flow

- **Prefer early returns and `?` over deeply nested `match`.**
- **Max nesting: 6 levels.** Use `let else`, early `return`, `?`, and extracted helpers to reduce brace depth.

  ```rust
  // Bad — 4+ levels deep
  match config {
      Config::A(inner) => {
          if inner.enabled {
              for item in inner.items {
                  if item.valid() {
                      process(item);
                  }
              }
          }
      }
      _ => {}
  }

  // Good — flat with early returns and helpers
  fn handle_config_a(inner: &Inner) -> Result<()> {
      if !inner.enabled { return Ok(()); }
      inner.items.iter().filter(|i| i.valid()).for_each(process);
      Ok(())
  }
  ```

- **Match arms with >3 lines dispatch to helpers.**

  ```rust
  // Bad — inline logic in match arms
  match command {
      Command::Create(args) => {
          // 30 lines of creation logic...
      }
  }

  // Good — dispatch to handlers
  match command {
      Command::Create(args) => handle_create(args, state),
      Command::Delete(args) => handle_delete(args, state),
  }
  ```

### Functional Style (preferred, not mandatory)

- **Prefer iterator pipelines for collection transforms.**
- **Prefer `collect::<Result<Vec<_>, _>>()?` for fallible transforms.**
- **Prefer folds for recursive data construction.**
- **Prefer `split_first()` and slice patterns over indexing.**
- **Prefer declarative matching over flag variables.**

## Structural Limits

| Constraint | Limit |
|---|---|
| Function length | 300 lines |
| Nesting depth | 6 levels |
| `mod.rs` impl lines | 1500 |

## Lint Quick Reference

These are enforced mechanically during `cargo xtask test --gate`. Hard errors:

| Denied | Use instead |
|---|---|
| `.unwrap()` | `.expect("reason")`, `?`, `.ok_or()` |
| `Result<T, ()>` | Meaningful error type |
| `if !cond { panic!() }` | `assert!(cond, ...)` |
| `thread_local!` | Pass state via parameters |

### Style (auto-fixable)

| Flagged pattern | Use instead |
|---|---|
| `.filter().map()` | `.filter_map()` |
| `.find().map()` | `.find_map()` |
| `.map().flatten()` | `.flat_map()` |
| `for i in 0..v.len()` | `for item in &v` or `.enumerate()` |
| `for x in v.iter()` | `for x in &v` |
| `format!("{}", x)` | `format!("{x}")` |
| `let x = if let Some(v) = y { v } else { return }` | `let Some(x) = y else { return }` |
