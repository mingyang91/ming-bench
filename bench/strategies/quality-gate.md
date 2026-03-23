# MING Scheme Interpreter

Implement a Scheme interpreter in Rust. Read `SPEC.md` for the full specification.

## Contract

- Implement `eval_str` in `src/scheme/mod.rs`
- Add error variants to `EvalError` in `src/scheme/error.rs` as needed
- You may create any additional modules/files under `src/scheme/`
- Do NOT modify test functions
- Allowed external crates: `thiserror`, `log`, `env_logger` (already in Cargo.toml). Do NOT add any others.
- **No `thread_local!` or `std::thread_local`.** All state must be passed explicitly through function parameters.
- **Local `let mut` is permitted.** Prefer immutable bindings, but `let mut` inside a function body is fine for loop control and local accumulation. Mutable state must never escape the function — don't return `&mut`, don't store in struct fields, don't pass as `&mut` to other functions. If mutation needs to be shared, use explicit shared-ownership types.
- **NEVER run `cargo test` directly on the host.** Always use `cargo xtask test`. This rule has NO exceptions.

## Build & Test

```bash
cargo xtask test 01   # test level 1
cargo xtask test all  # test all levels (300s timeout)
```

Tests run in a container (1GB memory, 1 CPU). Per-level: 30s. Full suite: 300s.

## Development Strategy

- **Implement levels in order (L1 → L26).** Each level builds on the previous.
- **After implementing each level, run its tests before moving on.**
- **If a level's tests fail, fix them before proceeding.**
- **Code first, debug from test output.** Don't mentally simulate — let the test runner do that.
- **Maximum 10 turns of reading before first code change.** If you haven't written or edited a file by turn 10, your analysis is too deep — write a first attempt and iterate from test failures. Tests are the source of truth, not mental simulation.

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

- **Wrap values that cross subsystem boundaries.** If a `String` means different things in different contexts (identifier vs user text vs output), it needs a newtype. If a `bool` parameter controls behavior, it needs an enum.

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

Prefer functional patterns when they make the code clearer. Use imperative style when it's simpler.

- **Prefer iterator pipelines for collection transforms.**

  ```rust
  // Imperative — fine for complex logic
  let mut results = Vec::new();
  for item in items {
      results.push(process(item));
  }

  // Functional — preferred when straightforward
  let results: Vec<_> = items.iter().map(process).collect();
  ```

- **Prefer `collect::<Result<Vec<_>, _>>()?` for fallible transforms.**

  ```rust
  let parsed: Vec<_> = inputs.iter().map(parse).collect::<Result<_, _>>()?;
  ```

- **Prefer folds for recursive data construction.**

  ```rust
  let list = items.iter().rev().fold(Value::Nil, |acc, item| {
      Value::Pair(Box::new(item.clone()), Box::new(acc))
  });
  ```

- **Prefer `split_first()` and slice patterns over indexing.**

  ```rust
  // Bad
  let first = args[0];
  let rest = &args[1..];

  // Good
  let [first, rest @ ..] = args else {
      return Err(Error::NotEnoughArgs);
  };
  ```

- **Prefer declarative matching over flag variables.**

  ```rust
  // Bad
  let mut found = None;
  for entry in entries {
      if entry.matches(key) { found = Some(entry.value()); break; }
  }

  // Good
  let found = entries.iter().find_map(|e| e.matches(key).then(|| e.value()));
  ```

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
