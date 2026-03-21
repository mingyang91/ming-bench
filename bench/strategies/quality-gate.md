# MING Scheme Interpreter

Implement a Scheme interpreter in Rust. Read `SPEC.md` for the full specification.

## Contract

- Implement `eval_str` in `src/scheme/mod.rs`
- Add error variants to `EvalError` in `src/scheme/error.rs` as needed
- You may create any additional modules/files under `src/scheme/`
- Do NOT modify test functions
- Allowed external crates: `thiserror`, `log`, `env_logger` (already in Cargo.toml). Do NOT add any others.
- **No `thread_local!` or `std::thread_local`.** All state must be passed explicitly through function parameters. Thread-local storage hides state from signatures and defeats testability.
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

## Problem-Solving Approach

- **Code first, debug from test output.** Write a working first attempt based on your understanding, then iterate from test failures. Do not mentally simulate test cases before writing code — let the test runner do that work.
- **One failing test = one targeted fix.** When tests fail, read the error output and fix the specific failure. Do not re-analyze the entire design.
- **Budget your planning.** For any single feature, your plan should fit in a few paragraphs. If you're tracing through execution step-by-step in your head, stop and write code instead.

## Code Philosophy

The quality gate enforces structure mechanically; these rules are the design intent behind those checks. Follow them proactively — the gate is a safety net, not a substitute for judgment.

### Type Safety

- **Maximize type safety over minimal diffs.** The compiler is the last line of defense. If it compiles, it's correct. Prefer type-level refactors even if they touch many files.
- **Type precision is not over-engineering.** Newtypes, enums, `NonEmpty` wrappers — these remove runtime checks, not add complexity.
- **Newtypes for domain values** where it prevents confusion. Propagate constraints through signatures — don't downgrade and re-validate internally.
- **Wrap values that cross subsystem boundaries.** If a `String` means different things in different contexts (identifier vs user text vs output), it needs a newtype. If a `bool` parameter controls behavior, it needs an enum.
- **Introduce types when they earn their keep.** A newtype is worth it when it prevents a class of bugs across multiple call sites. Don't wrap values that are only used locally or have no ambiguity.
- **Enums over booleans for behavioral flags.** `enum Mode { Strict, Lenient }` communicates intent where `strict: bool` does not. The compiler can enforce exhaustive handling.

### Error Handling

- **No `.unwrap()`.** Use `.expect("reason")` for trusted invariants, `?` or typed errors for untrusted paths.
- **No silent error swallowing.** Forbidden: `.unwrap_or_default()`, `.ok()` to discard errors, `.unwrap_or(fallback)` hiding parse failures.
- **Trusted vs untrusted paths:**
  - **Trusted** (internal data, AST nodes, env lookups): bugs → `panic!` / `unreachable!`
  - **Untrusted** (user Scheme source code): errors → `Err(...)`

### Typed Error Model (`thiserror`)

Use `thiserror` for `enum` error types with named variants carrying structured context. Compose errors via wrapping, don't flatten to strings.

```rust
enum ParseError { UnexpectedToken(String), UnmatchedParen, ... }
enum EvalError { Parse(ParseError), UnboundVariable(String), WrongArgCount { expected: usize, got: usize }, ... }
impl From<ParseError> for EvalError { ... }  // enables ? propagation
```

- Each variant must carry domain-specific fields (e.g., `NotFound { key: String }`).
- Variants that wrap a formatted `String` message (e.g., `Other(String)`) are prohibited — they defeat pattern matching and are just `String` with extra steps.
- Exhaustive `match` — compiler enforces handling every variant.

### Effect Marking (visible signatures)

Make capabilities visible in function signatures — callers see exactly what a function does:
- Mutation: `&mut` parameters (not hidden behind `&self`)
- Fallibility: `-> Result<T, EvalError>` (not panic)
- Ownership: lifetime parameters where borrowing is non-obvious

No hidden side effects.

## Code Style

### Flat Control Flow

- **Flat control flow.** Prefer early returns and `?` over deeply nested `match`.
- **No premature helpers (<5 ops).** If logic is < 5 composed operators/steps, inline at call site.
- **Extract helpers at >= 5 ops.** Reuse existing helpers before writing new ones.
- **No premature abstractions.** Three similar lines > one abstraction used once.
- **Proactive naming review.** Fix misleading/stale names when modifying code.

### Functional Style

- **Prefer immutable-first data flow.** Build new values from inputs instead of mutating temporary state, unless mutation is required by semantics.

  ```rust
  // Bad
  let mut total = 0;
  for val in values {
      total += transform(val);
  }

  // Good
  let total: i64 = values.iter().map(transform).sum();
  ```

- **Prefer iterator pipelines for collection transforms.** Use `map`, `filter`, `fold`, `try_fold`, `collect` instead of manual `Vec::push` loops when the logic is a pure transformation.

  ```rust
  // Bad
  let mut results = Vec::new();
  for item in items {
      results.push(process(item));
  }

  // Good
  let results: Vec<_> = items.iter().map(process).collect();
  ```

- **Prefer `collect::<Result<Vec<_>, _>>()?` for fallible transforms.**

  ```rust
  // Bad
  let mut parsed = Vec::new();
  for raw in inputs {
      parsed.push(parse(raw)?);
  }

  // Good
  let parsed: Vec<_> = inputs.iter().map(parse).collect::<Result<_, _>>()?;
  ```

- **Prefer structural recursion or slice-pattern matching over index-driven loops.**

  ```rust
  // Bad
  let mut i = 0;
  while i < nodes.len() {
      match &nodes[i] { ... }
      i += 1;
  }

  // Good
  fn walk(nodes: &[Node]) -> Result<()> {
      match nodes {
          [] => Ok(()),
          [Node::Leaf(v), rest @ ..] => { handle(v); walk(rest) }
          [Node::Branch(children), rest @ ..] => { walk(children)?; walk(rest) }
      }
  }
  ```

- **Prefer `split_first()`, `split_last()`, and slice patterns over indexing.**

  ```rust
  // Bad
  let first = args[0];
  let rest = &args[1..];

  // Good
  let [first, rest @ ..] = args else {
      return Err(Error::NotEnoughArgs);
  };
  ```

- **Prefer folds for recursive data construction.**

  ```rust
  // Bad
  let mut list = Node::Empty;
  for item in items.iter().rev() {
      list = Node::Pair(Box::new(item.clone()), Box::new(list));
  }

  // Good
  let list = items.iter().rev().fold(Node::Empty, |acc, item| {
      Node::Pair(Box::new(item.clone()), Box::new(acc))
  });
  ```

- **Prefer declarative matching over flag variables.** Replace mutable `found = true` state with return-oriented control flow, `find_map`, or `try_fold`.

  ```rust
  // Bad
  let mut found = None;
  for entry in entries {
      if entry.matches(key) {
          found = Some(entry.value());
          break;
      }
  }

  // Good
  let found = entries.iter().find_map(|e| e.matches(key).then(|| e.value()));
  ```

- **Keep mutation at semantic boundaries only.** Accept local mutation when modeling runtime semantics (e.g., shared mutable state, in-place update), but avoid incidental mutation used only for bookkeeping.

## Structural Limits

- **`mod.rs` stays thin: exports + entry point.** Implementation logic goes in dedicated submodules. Hard cap: 300 lines.
- **Split by subsystem into dedicated submodules.** Each distinct responsibility gets its own file.
- **Function hard cap: 150 lines.** Exception: parser/state-machine code with inherently sequential logic. If a function needs scrolling, extract helpers.
- **Max nesting: 3 levels.** Use `let else`, early `return`, `?`, and extracted helpers to reduce brace depth.

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

- **Match arms with >3 lines dispatch to helpers.** A `match` arm should call a function, not inline multi-line logic.

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

## Logging

- Use `log` crate (`debug!`, `info!`, `warn!`, `error!`) with `env_logger`. Control verbosity via `RUST_LOG` env var.
- **Log at decision points** — special form dispatch, error paths. Helps trace failures.
- Default level: `info`. Set `RUST_LOG=debug` or `RUST_LOG=trace` when debugging.

## Runtime Assertion Checks

- **Use `debug_assert!` on invariants.** Catch broken assumptions before corrupt state propagates.
- **Where to assert:**
  - After environment operations — variable was actually bound
  - After list operations — structural invariants (`car`/`cdr` on non-pair)
  - Tail call trampoline — recursion depth doesn't silently overflow
- **Where NOT to assert:** User input validation (use typed errors), hot eval loops (use errors).

## Development Workflow

- **Fix code smells immediately.** Fix on the spot, don't track for later.
- **When touching a file, fix violations in that file.** Do not defer. Do not rewrite unrelated files unprompted.
- **Proactive refactoring is mandatory.** When implementing a new feature, if you notice surrounding code that violates nesting limits, function size caps, or functional style rules — fix it in the same pass.
- **Blast radius is not a concern — rule compliance is.** If existing code violates structural limits or functional style rules, refactor aggressively. Split oversized files into modules, extract bloated functions into helpers, rewrite imperative loops as pipelines.

## Lint Reference

These lints are enforced in a **separate cleanup pass** at the end of each level — not during coding. Focus on making tests pass first. You will get a dedicated session to fix lint violations afterward. This table is a quick-fix reference for that cleanup session.

### Denied (hard errors)

| Denied pattern | Use instead |
|---|---|
| `.unwrap()` | `.expect("reason")`, `?`, or `.ok_or()` |
| `Result<T, ()>` | A meaningful error type |
| `if !cond { panic!(...) }` | `assert!(cond, ...)` |
| `thread_local!` / `std::thread_local` | Pass state via function parameters |

### Structural limits

| Constraint | All levels |
|---|---|
| Max function length | 150 lines |
| Max nesting depth | 3 |
| Max `mod.rs` size | 300 lines |

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
