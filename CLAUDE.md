# CS 61A Scheme Interpreter Benchmark

Implement a Scheme interpreter in Rust.

## Contract
- Implement `eval_str` in `src/scheme/mod.rs`
- You may create any additional modules/files under `src/scheme/`
- Do NOT modify test functions
- Do NOT add external dependencies to Cargo.toml (thiserror, log, env_logger are pre-included)

## Build & Test
```
cargo test                    # run all tests
cargo test test_l01           # run Level 1 tests only
cargo test test_l05           # run Level 5 tests only
```

## Development Strategy
- **Implement levels in order (L1 → L16).** Each level builds on the previous.
- **After implementing each level, run its tests before moving on:**
  ```
  cargo test test_l01   # must pass before starting L2
  cargo test test_l02   # must pass before starting L3
  ```
- **Do not skip ahead.** Later levels depend on earlier ones being correct.
- **If a level's tests fail, fix them before proceeding.** Do not accumulate broken levels.

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
