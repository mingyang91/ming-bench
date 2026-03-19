# CS 61A Scheme Interpreter Benchmark

Implement a Scheme interpreter in Rust.

## MANDATORY — Quality Gate (run before EVERY test)

Before running `./scripts/test-level.sh`, you MUST verify ALL of these. If any check fails, fix it BEFORE testing.

1. **`mod.rs` impl lines are enforced by `test-level.sh`** (auto-checked, script will reject if over limit):
   - L01–L03: ≤ 300 lines (bootstrapping)
   - L04–L06: ≤ 200 lines (must split builtins/special_forms out)
   - L07+: ≤ 100 lines (mod.rs is thin: entry point + reexports only)
2. **Function body length enforced by `clippy::too-many-lines`** (via `clippy.toml`):
   - L01–L05: ≤ 80 lines
   - L06+: ≤ 60 lines
3. **Nesting depth enforced by `clippy::excessive-nesting`** (via `clippy.toml`):
   - All levels: ≤ 3 levels
3. **No duplicated logic.** If two functions do the same thing with a trivial wrapper (e.g. "evaluate args then call the other version"), delete the wrapper.
4. **`helpers.md` is up to date.** Every extracted helper function is registered with name, file, purpose.
5. **`log::debug!` at dispatch points.** At minimum: one in `eval_inner` (or equivalent), one in special-form dispatch, one in `call/cc` path.
6. **`debug_assert!` on invariants.** At minimum: after env define, after bind_params, in trampoline loop.
7. **No `.unwrap()` or `.expect()` in non-test code.** Use `?`, `.ok_or(...)`, or `match`.
8. **Immutable-first.** No `vec.insert(0, x)` or `vec.remove(0)` — build new collections instead.

**This gate is not optional.** Passing tests with style violations is a failure. Fix violations even if it means rewriting working code.

## Contract

- Implement `eval_str` in `src/scheme/mod.rs`
- You may create any additional modules/files under `src/scheme/`
- Do NOT modify test functions
- Do NOT add external dependencies to Cargo.toml (thiserror, log, env_logger are pre-included)
- **NEVER run `cargo test` directly on the host.** Always use `./scripts/test-level.sh`. Bare `cargo test` risks infinite loops and OOM that crash the host. This rule has NO exceptions.

## Build & Test

**Build on host, test in container.**

```bash
./scripts/test-level.sh 01   # test level 1
./scripts/test-level.sh 05   # test level 5
./scripts/test-level.sh all  # test all levels (300s timeout)
```

- Always use `./scripts/test-level.sh <level>` — never bare `cargo test`
- A level argument is required (e.g., `01`, `16`, or `all`)
- Tests are built in release mode and run in a container with 1GB memory, 1 CPU
- Per-level timeout: 30s. Full suite (`all`): 300s. Exceeding these or OOM = failing
- Build the container image first if not already built: `sudo podman build -t cs61a-bench -f Dockerfile.bench .`
- `test-level.sh` auto-runs `cargo clippy --fix --allow-dirty` before verification. Trivial lints (needless borrows, collapsible ifs, const initializers) are fixed automatically. You only need to fix structural clippy errors (dead code, wrong types).

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
- Return `Err(...)` for evaluation errors (unbound variable, wrong arg count, etc.)
- Booleans print as `#t` / `#f`
- Lists print as `(1 2 3)` with spaces between elements
- The empty list prints as `()`
- Strings print with surrounding quotes: `"hello"`

## File Structure (REQUIRED)

`mod.rs` is ONLY for: module declarations, re-exports, `eval_str` entry point, and the `Trampoline` enum. ALL implementation logic MUST go in submodules:

| File | Responsibility |
|------|---------------|
| `mod.rs` | Exports + `eval_str` + `Trampoline` (≤ 200 lines) |
| `types.rs` | `Value` enum, `Display`, `PartialEq` |
| `parser.rs` | Tokenizer, parser |
| `env.rs` | Environment / scoping |
| `eval.rs` | `eval`, `eval_inner`, `eval_list_tco`, apply, bind_params |
| `special_forms.rs` | `define`, `if`, `lambda`, `let`, `cond`, `and`, `or`, `set!`, `quote` |
| `builtins.rs` | Arithmetic, comparisons, list ops, type predicates |
| `continuations.rs` | `call/cc`, continuation capture/invoke, eval context |
| `macros.rs` | `syntax-rules`, macro expansion |

Create files as needed when you reach the relevant level. Do NOT put evaluator, builtins, or special forms in `mod.rs`.

## Code Rules (MANDATORY)

### Error Handling
- Use `?` operator. No `.unwrap()` / `.expect()` in non-test code.
- Use `thiserror` for typed error enums. Do not flatten all errors to `String`.

### Style
- **Flat control flow.** Early returns, `?`, max 3 levels of nesting.
- **Functions ≤ 60 lines** (hard cap: 100). Extract helpers if longer.
- **Match arms dispatch to helpers** — no multi-line inline logic in match arms.
- **Register helpers in `helpers.md`** after creating them. Check it before creating new ones.

### Functional Style
- **Immutable-first.** Build new values, don't mutate temporaries.
- **Iterator pipelines** (`map`, `filter`, `fold`, `collect`) over manual loops.
- **`collect::<Result<Vec<_>, _>>()?`** for fallible transforms.
- **Slice patterns** (`[first, rest @ ..]`) over indexing (`args[0]`, `&args[1..]`).
- **No `vec.insert(0, x)` or `vec.remove(0)`** — build new vecs instead.
- **No duplicated dispatch tables.** One builtin dispatch function, not two.

### Observability
- **`log::debug!`** at eval dispatch, special form dispatch, call/cc paths.
- **`debug_assert!`** after env operations, after param binding, on structural invariants.

### Compiler Discipline
- `#![deny(warnings)]` and `#![deny(clippy::unwrap_used)]` in `src/lib.rs`. Do not remove.
- `cargo fmt --check` must pass.

### Development Workflow
- **Fix violations immediately.** Do not defer to later levels.
- **Proactive refactoring is mandatory.** If surrounding code violates rules, fix it now.
- **Blast radius is not a concern — rule compliance is.** Rewrite entire files if needed.
