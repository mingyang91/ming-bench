# CS 61A Scheme Interpreter Benchmark

A benchmark for evaluating coding agents by having them build a Scheme interpreter in Rust, inspired by UC Berkeley's CS 61A Project 4 and SICP.

## How It Works

The agent receives:
- A single function signature: `eval_str(input: &str) -> Result<String, String>`
- 92 test cases across 16 difficulty levels
- Instructions in `CLAUDE.md`

The agent's job: implement a working Scheme interpreter that passes the tests, level by level. The agent designs all internal architecture (lexer, parser, AST, evaluator) from scratch.

## Running Tests

```bash
cargo test                    # run all tests
cargo test test_l01           # run Level 1 only
cargo test test_l05           # run Level 5 only
```

## Test Levels

| Level | Topic | Tests | Key Concepts |
|-------|-------|-------|-------------|
| 1 | Atoms | 5 | Self-evaluating: integers, booleans, strings |
| 2 | Arithmetic | 7 | `+`, `-`, `*`, `/`, variadic, nested |
| 3 | Comparisons | 7 | `<`, `>`, `=`, `<=`, `not`, `and`, `or` |
| 4 | Define & If | 7 | Variable binding, conditionals, `quote` |
| 5 | Lambda | 7 | Closures, define sugar, recursion |
| 6 | Lists | 8 | `cons`, `car`, `cdr`, `null?`, `list`, `length` |
| 7 | Recursive programs | 5 | map, filter, append, reverse (user-defined) |
| 8 | Let/begin/cond | 6 | Local bindings, sequencing, multi-branch |
| 9 | Type predicates | 5 | `string?`, `number?`, `boolean?`, `pair?`, `symbol?` |
| 10 | Tail call opt. | 3 | Deep recursion without stack overflow |
| 11 | set! & mutation | 5 | Mutable bindings, shared state in closures |
| 12 | Variadic & apply | 5 | Rest args, `apply` with prefix args |
| 13 | Tail position (all) | 5 | TCO in `cond`, named `let`, `and`/`or`, `begin` |
| 14 | **call/cc** | 7 | First-class continuations, non-local exit, reentrant |
| 15 | **Macros** | 5 | `define-syntax`, `syntax-rules`, hygiene, ellipsis |
| 16 | **Integration** | 5 | call/cc + macros + mutation + TCO combined |

## Scoring

- **Raw score**: tests passed / 92
- **Watermark**: highest level where ALL tests pass
- **Weighted**:
  - L1–L10: 1–5 pts/test (foundational)
  - L11–L13: 3 pts/test (mutation, apply, advanced TCO)
  - L14: 8 pts/test (call/cc — architectural)
  - L15: 8 pts/test (macros — new subsystem)
  - L16: 10 pts/test (integration — everything together)

## Structure

```
src/
├── lib.rs
└── scheme/
    └── mod.rs       # eval_str() + all tests
```

The agent creates any additional files/modules they need under `src/scheme/`.

## Ground Truth

Test expected values are verified against Guile Scheme. Run `cargo xtask verify` to re-check.
