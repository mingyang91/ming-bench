# CS 61A Scheme Interpreter Benchmark

A benchmark for evaluating coding agents by having them build a Scheme interpreter in Rust, inspired by UC Berkeley's CS 61A Project 4 and SICP.

## How It Works

The agent receives:
- A single function signature: `eval_str(input: &str) -> Result<String, String>`
- 60 test cases across 10 difficulty levels
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

## Scoring

- **Raw score**: tests passed / 60
- **Watermark**: highest level where ALL tests pass
- **Weighted**: Level 1 tests = 1 pt each, Level 10 = 5 pts each

## Structure

```
src/
├── lib.rs
└── scheme/
    └── mod.rs       # eval_str() + all tests
```

The agent creates any additional files/modules they need under `src/scheme/`.

## Ground Truth

Test expected values are verified against Chez Scheme. Run `scripts/verify.sh` to re-check.
