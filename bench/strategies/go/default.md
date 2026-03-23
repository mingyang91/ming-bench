# MING Scheme Interpreter

Implement a Scheme interpreter in Go. Read `SPEC.md` for the full specification.

## Contract

- Implement `EvalStr` in `evaluator.go`
- Implement `EvalStrWithOutput` in `evaluator.go` (needed from Level 5)
- Extend `EvalError` in `eval_error.go` as needed
- You may create any additional `.go` files in this directory
- Do NOT modify test files (`scheme_test.go`)
- No external dependencies — standard library only
- **NEVER run `go test` directly.** Always use `cargo xtask test --lang go`. Direct `go test` risks infinite loops and OOM. This rule has NO exceptions.

## Build & Test

`cargo xtask test --lang go` handles everything: build and containerized test execution.

```bash
cargo xtask test 01 --lang go    # test level 1
cargo xtask test 05 --lang go    # test level 5
cargo xtask test all --lang go   # test all levels (300s timeout)
```

- A level argument is required (e.g., `01`, `15`, or `all`)
- Tests run in a container with 1GB memory, 1 CPU
- Per-level timeout: 30s. Full suite (`all`): 300s. Exceeding these or OOM = failing

## Development Strategy

- **Implement levels in order (L1 → L28).** Each level builds on the previous.
- **After implementing each level, run its tests before moving on.**
- **Do not skip ahead.** Later levels depend on earlier ones being correct.
- **If a level's tests fail, fix them before proceeding.**
- **Rely on the provided level tests as the source of truth.**

## Problem-Solving Approach

- **Code first, debug from test output.** Write a working first attempt based on your understanding, then iterate from test failures. Do not mentally simulate test cases before writing code — let the test runner do that work.
- **One failing test = one targeted fix.** When tests fail, read the error output and fix the specific failure. Do not re-analyze the entire design.
- **Budget your planning.** For any single feature, your plan should fit in a few paragraphs. If you're tracing through execution step-by-step in your head, stop and write code instead.
