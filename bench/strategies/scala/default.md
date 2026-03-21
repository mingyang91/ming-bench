# MING Scheme Interpreter

Implement a Scheme interpreter in Scala 3. Read `SPEC.md` for the full specification.

## Contract

- Implement `evalStr` in `src/main/scala/ming/Evaluator.scala`
- Implement `evalStrWithOutput` in `Evaluator.scala` (needed from Level 5)
- Extend `EvalError` in `src/main/scala/ming/EvalError.scala` as needed
- You may create any additional Scala files under `src/main/scala/ming/`
- Do NOT modify test files under `src/test/`
- No external runtime dependencies — standard library only. Test dependencies (munit, Gson) are already in `build.mill`.
- **NEVER run `./mill ming.test` directly.** Always use `cargo xtask test --lang scala`. Direct test runs risk infinite loops and OOM. This rule has NO exceptions.

## Build & Test

`cargo xtask test --lang scala` handles everything: build and containerized test execution.

```bash
cargo xtask test 01 --lang scala    # test level 1
cargo xtask test 05 --lang scala    # test level 5
cargo xtask test all --lang scala   # test all levels (300s timeout)
```

- A level argument is required (e.g., `01`, `15`, or `all`)
- Tests run in a container with 1GB memory, 1 CPU
- Per-level timeout: 30s. Full suite (`all`): 300s. Exceeding these or OOM = failing

## Development Strategy

- **Implement levels in order (L1 → L23).** Each level builds on the previous.
- **After implementing each level, run its tests before moving on.**
- **Do not skip ahead.** Later levels depend on earlier ones being correct.
- **If a level's tests fail, fix them before proceeding.**
- **Rely on the provided level tests as the source of truth.**

## Problem-Solving Approach

- **Code first, debug from test output.** Write a working first attempt based on your understanding, then iterate from test failures. Do not mentally simulate test cases before writing code — let the test runner do that work.
- **One failing test = one targeted fix.** When tests fail, read the error output and fix the specific failure. Do not re-analyze the entire design.
- **Budget your planning.** For any single feature, your plan should fit in a few paragraphs. If you're tracing through execution step-by-step in your head, stop and write code instead.
