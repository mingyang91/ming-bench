# CS 61A Scheme Interpreter Benchmark

Implement a Scheme interpreter in Rust.

## MANDATORY — Quality Gate (enforced by `test-level.sh`)

These checks run automatically before every test. The script will **reject your code** if any check fails. You do not need to run them manually — but you should be aware of the limits so you don't waste a test cycle.

1. **`mod.rs` impl line count** (bash check in `test-level.sh`):
   - L01–L03: ≤ 300 lines (bootstrapping)
   - L04–L06: ≤ 200 lines (time to split into submodules)
   - L07+: ≤ 100 lines (mod.rs should only contain entry point + reexports)
2. **Function body length** (`clippy::too-many-lines` via leveled `clippy.toml`):
   - L01–L05: ≤ 80 lines
   - L06+: ≤ 60 lines
3. **Nesting depth** (`clippy::excessive-nesting` via `clippy.toml`):
   - All levels: ≤ 3 levels
4. **No `.unwrap()` or `.expect()` in non-test code** (`clippy::unwrap_used` — denied in `src/lib.rs`). Use `?`, `.ok_or(...)`, or `match`.
5. **Idiomatic Rust** — pedantic clippy lints are enabled in `src/lib.rs` (e.g. `manual_filter_map`, `needless_range_loop`, `explicit_counter_loop`, `vec_init_then_push`, `manual_let_else`). Combined with `#![deny(warnings)]`, these are errors. Many are auto-fixed by `clippy --fix`; the rest you must fix manually.
6. **Dead code allowance.** L01–L05: `dead_code` lint is suppressed (forward-declared types are OK). L06+: all dead code is denied.

## Contract

- Implement `eval_str` in `src/scheme/mod.rs`
- You may create any additional modules/files under `src/scheme/`
- Do NOT modify test functions
- Allowed external crates: `thiserror`, `log`, `env_logger` (already in Cargo.toml). Do NOT add any others.
- **NEVER run `cargo test` directly on the host.** Always use `./scripts/test-level.sh`. Bare `cargo test` risks infinite loops and OOM that crash the host. This rule has NO exceptions.

## Build & Test

`test-level.sh` handles everything: clippy auto-fix, clippy verification, mod.rs size check, release build, and containerized test execution. Just run it.

```bash
./scripts/test-level.sh 01   # test level 1
./scripts/test-level.sh 05   # test level 5
./scripts/test-level.sh all  # test all levels (300s timeout)
```

- A level argument is required (e.g., `01`, `16`, or `all`)
- Tests run in a container with 1GB memory, 1 CPU
- Per-level timeout: 30s. Full suite (`all`): 300s. Exceeding these or OOM = failing
- Build the container image first if not already built: `sudo podman build -t cs61a-bench -f Dockerfile.bench .`

## Development Strategy

- **Implement levels in order (L1 → L16).** Each level builds on the previous.
- **After implementing each level, run its tests before moving on.**
- **Do not skip ahead.** Later levels depend on earlier ones being correct.
- **If a level's tests fail, fix them before proceeding.**
- **Rely on the provided level tests as the source of truth.**

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
- Side-effect-only forms (`define`, `set!`) return a void/nil value — the tests only check the result of the final expression, so `"(define x 5) x"` → `"5"`
- Return `Err(...)` for evaluation errors (unbound variable, wrong arg count, etc.)
- Booleans print as `#t` / `#f`
- Lists print as `(1 2 3)` with spaces between elements
- The empty list prints as `()`
- Strings print with surrounding quotes: `"hello"`

## File Structure

`mod.rs` should only contain module declarations, re-exports, and the `eval_str` entry point. Split implementation logic into submodules organized by responsibility. The mod.rs line limit (see Quality Gate item 1) enforces this — when you hit the limit, extract a submodule.

How you organize submodules is up to you. Choose a structure that groups related logic and keeps each file focused on one responsibility.

## Code Rules

These rules are NOT enforced by tooling. They represent style expectations that clippy cannot check.

### Error Handling
- Use `?` operator for error propagation.
- Use `thiserror` for typed error enums. Do not flatten all errors to `String`.

### Functional Style (beyond what clippy enforces)
- **Immutable-first.** Build new values from inputs instead of mutating temporaries.
- **`collect::<Result<Vec<_>, _>>()?`** for fallible collection transforms.
- **Slice patterns** (`[first, rest @ ..]`) over indexing (`args[0]`, `&args[1..]`) when the length is statically known.
- **No `vec.insert(0, x)` or `vec.remove(0)`** — build new vecs instead.

### Observability
- **`log::debug!`** at key decision points (e.g. dispatch, error paths).
- **`debug_assert!`** on structural invariants after operations that establish them.

### Compiler Discipline
- `#![deny(warnings)]` and `#![deny(clippy::unwrap_used)]` in `src/lib.rs`. Do not remove.
- Pedantic clippy lints are also configured as `#![warn(...)]` in `src/lib.rs`. Do not remove them.
- `cargo fmt --check` must pass.

### Development Workflow
- **Fix violations immediately.** Do not defer to later levels.
- **When touching a file, fix violations in that file.** Do not leave rule violations in code you're editing. Do not rewrite unrelated files unprompted.
