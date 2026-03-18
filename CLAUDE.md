# CS 61A Scheme Interpreter Benchmark

Implement a Scheme interpreter in Rust.

## Contract
- Implement `eval_str` in `src/scheme/mod.rs`
- You may create any additional modules/files under `src/scheme/`
- Do NOT modify test functions
- Do NOT add external dependencies to Cargo.toml (thiserror, log, env_logger are pre-included)
- **NEVER run `cargo test` directly on the host.** Always use `./scripts/test-level.sh`. Bare `cargo test` risks infinite loops and OOM that crash the host. This rule has NO exceptions.

## Build & Test

**Build on host, test in container.** This prevents infinite loops or memory leaks from crashing the host.

```bash
./scripts/test-level.sh 01   # test level 1
./scripts/test-level.sh 05   # test level 5
./scripts/test-level.sh all  # test all levels (300s timeout)
```

**IMPORTANT:**
- Always use `./scripts/test-level.sh <level>` — never bare `cargo test`
- A level argument is required (e.g., `01`, `16`, or `all`)
- Tests are built in release mode and run in a container with 1GB memory, 1 CPU
- Per-level timeout: 30s. Full suite (`all`): 300s. Exceeding these or OOM = failing
- Build the container image first if not already built: `sudo podman build -t cs61a-bench -f Dockerfile.bench .`

## Development Strategy
- **Implement levels in order (L1 → L16).** Each level builds on the previous.
- **After implementing each level, run its tests before moving on:**
  ```bash
  ./scripts/test-level.sh 01   # must pass before starting L2
  ./scripts/test-level.sh 02   # must pass before starting L3
  ```
- **Do not skip ahead.** Later levels depend on earlier ones being correct.
- **If a level's tests fail, fix them before proceeding.** Do not accumulate broken levels.
- **Rely on the provided level tests as the source of truth.** Do not write additional unit tests unless debugging a specific internal module. The level tests are comprehensive — passing them means the implementation is correct.

## Levels (implement in order)
1. **Atoms** — self-evaluating: integers, booleans, strings
2. **Arithmetic** — `+`, `-`, `*`, `/` (variadic, nested)
3. **Comparisons** — `<`, `>`, `=`, `<=`, `not`, `and`, `or`
4. **Define & If** — variable binding, conditionals, `quote`
5. **Lambda** — closures, define shorthand `(define (f x) ...)`, recursion
6. **Lists** — `cons`, `car`, `cdr`, `null?`, `list`, `length`
7. **Recursive list programs** — user-defined `map`, `filter`, `append`, `reverse`
8. **Let, begin, cond** — local bindings, sequencing, multi-branch conditionals
9. **Type predicates** — `string?`, `number?`, `boolean?`, `pair?`, `symbol?`
10. **Tail call optimization** — no stack overflow on deep tail recursion
11. **set! and mutation** — `set!`, mutable closures, shared state
12. **Variadic & apply** — rest args `(define (f x . rest) ...)`, `apply`
13. **Tail position in all forms** — TCO through `cond`, named `let`, `and`/`or`, `begin`
14. **First-class continuations** — `call/cc`, non-local exits, saved/reentrant continuations
15. **Hygienic macros** — `define-syntax`, `syntax-rules`, ellipsis patterns
16. **Comprehensive integration** — call/cc + macros + mutation + TCO combined

## Notes
- `eval_str` receives one or more expressions separated by spaces (e.g. `"(define x 5) x"`)
- It should return the string representation of the **last** expression's result
- Return `Err(...)` for evaluation errors (unbound variable, wrong arg count, etc.)
- Booleans print as `#t` / `#f`
- Lists print as `(1 2 3)` with spaces between elements
- The empty list prints as `()`
- Strings print with surrounding quotes: `"hello"`

## Code Philosophy

Rules merged from two production codebases (Rust + Scala), translated to idiomatic Rust for this project.

### Type Safety

- **Maximize type safety over minimal diffs.** The compiler is the last line of defense. If it compiles, it's correct. Prefer type-level refactors even if they touch many files.
- **Type precision is not over-engineering.** Newtypes, enums, `NonEmpty` wrappers — these remove runtime checks, not add complexity. "Avoid over-engineering" applies to architecture, not to type-level precision.
- **Write-cost is near zero.** AI writes 90%+ of code. Optimize for correctness, not minimal diff.
- **Newtypes for domain values** where it prevents confusion. Propagate constraints through signatures — don't downgrade and re-validate internally.

### Error Handling

- **Use `?` operator** or explicit error handling. No `.unwrap()` / `.expect()` in non-test code.
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

- Exhaustive `match` — compiler enforces handling every variant.

### Effect Marking (visible signatures)

Make capabilities visible in function signatures — callers see exactly what a function does:
- Mutation: `&mut Env` (not hidden behind `&self`)
- Fallibility: `-> Result<T, EvalError>` (not panic)
- Allocation: `&Arena` or lifetime params

No hidden side effects.

### Code Style

- **Flat control flow.** Prefer early returns and `?` over deeply nested `match`.
- **No premature helpers (<5 ops).** If logic is < 5 composed operators/steps, inline at call site.
- **Extract helpers at >= 5 ops.** Register in [helpers.md](helpers.md) — future agent sessions must check it before writing new helpers and must reuse existing ones. **After creating any new helper, immediately update helpers.md with its name, location, and purpose.** This is not optional.
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

- **Prefer `collect::<Result<Vec<_>, _>>()?` for fallible transforms.** Transform whole collections functionally instead of filling a mutable vector by hand.

  ```rust
  // Bad
  let mut parsed = Vec::new();
  for raw in inputs {
      parsed.push(parse(raw)?);
  }

  // Good
  let parsed: Vec<_> = inputs.iter().map(parse).collect::<Result<_, _>>()?;
  ```

- **Prefer structural recursion or slice-pattern matching over index-driven loops.** For tree/list traversal, match on structure instead of manually advancing indices.

  ```rust
  // Bad
  let mut i = 0;
  while i < nodes.len() {
      match &nodes[i] {
          Node::Leaf(v) => handle(v),
          Node::Branch(children) => { /* recurse somehow */ }
      }
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

- **Prefer `split_first()`, `split_last()`, and slice patterns over indexing.** Avoid `args[0]`, `v[v.len()-1]` when matching can encode the invariant directly.

  ```rust
  // Bad
  let first = args[0];
  let rest = &args[1..];

  // Good
  let [first, rest @ ..] = args else {
      return Err(Error::NotEnoughArgs);
  };
  ```

- **Prefer folds for recursive data construction.** Build recursive structures from slices with folds, destructure them with recursive helpers or `try_fold`.

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

- **Refactor repeated imperative patterns immediately.** Same hand-written loop or mutable accumulator in multiple places → extract a functional helper. Extract when duplicated across call sites, regardless of size.

### Structural Limits

- **`mod.rs` stays thin: exports + entry point.** Implementation logic goes in dedicated submodules. Soft cap: 200 lines for any `mod.rs`.

- **Split by subsystem into dedicated submodules.** Each distinct responsibility gets its own file. Don't pile unrelated logic into one file just because it's convenient.

- **Function soft cap: 40–60 lines. Hard cap: 100 lines.** Exception: parser/state-machine code with inherently sequential logic. If a function needs scrolling, extract helpers.

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

- **Match arms dispatch to helpers.** A `match` arm should call a function, not inline multi-line logic. If a branch needs scrolling, extract it.

  ```rust
  // Bad — inline logic in match arms
  match command {
      Command::Create(args) => {
          // 30 lines of creation logic...
      }
      Command::Delete(args) => {
          // 25 lines of deletion logic...
      }
  }

  // Good — dispatch to handlers
  match command {
      Command::Create(args) => handle_create(args, state),
      Command::Delete(args) => handle_delete(args, state),
  }
  ```

- **`cargo fmt --check` must pass.** Format before committing. `cargo clippy -- -D warnings` is already enforced by `test-level.sh`.

### Logging (agent debugging)

- Use `log` crate (`debug!`, `info!`, `warn!`, `error!`) with `env_logger`. Control verbosity via `RUST_LOG` env var.
- **Log at decision points** — special form dispatch, error paths. Helps agents trace failures.
- Default level: `info`. Agent sets `RUST_LOG=debug` or `RUST_LOG=trace` when debugging.

### Runtime Assertion Checks

- **Use `debug_assert!` on invariants.** Catch broken assumptions before corrupt state propagates.
- **Where to assert:**
  - After environment operations — variable was actually bound
  - After list operations — structural invariants (`car`/`cdr` on non-pair = boom)
  - Tail call trampoline — recursion depth doesn't silently overflow
- **Where NOT to assert:** User input validation (use typed errors), hot eval loops (use errors).

### Compiler Discipline

- `#![deny(warnings)]` and `#![deny(clippy::unwrap_used)]` are set in `src/lib.rs`. Do not remove them.
- `./scripts/test-level.sh` automatically runs clippy before testing — no need to run it separately.
- No `.unwrap()` in non-test code — use `?`, `.ok_or(...)`, or `match`.

### Development Workflow

- **Fix code smells immediately.** AI-agent-driven codebase — fix on the spot, don't track for later.
- **Resolve rule ambiguities immediately.** No human in the loop — make the best judgment call and move on.
- **Blast radius is not a concern — rule compliance is.** If existing code violates structural limits or functional style rules, refactor aggressively. Split oversized files into modules, extract bloated functions into helpers, rewrite imperative loops as pipelines. A 500-line diff that brings the codebase into compliance is better than a 5-line patch that leaves violations in place.
- **Never be conservative to minimize diff size.** AI writes code near-instantly — there is zero cost to rewriting an entire file if the result is cleaner, more modular, and rule-compliant. "Don't touch what isn't broken" does not apply here; if it violates the rules, it *is* broken.
- **Proactive refactoring is mandatory, not optional.** When implementing a new feature, if you notice surrounding code that violates nesting limits, function size caps, or functional style rules — fix it in the same pass. Do not defer. Do not leave TODOs.
