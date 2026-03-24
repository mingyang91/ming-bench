# Quality Gate — Cleanup Pass

All tests for this level already pass. Your job is to **refactor the code to production quality** while keeping all tests green.

## What To Do

1. Run `cargo xtask test <level> --lang scala --gate` to see quality-gate violations
2. Fix ALL violations — scalafix rules, formatting, nesting depth, file/method length
3. **Radical refactoring is encouraged.** Extract files, split large methods, flatten nesting, improve abstractions. The code works — make it maintainable.
4. Run `cargo xtask test all --lang scala` to verify no regressions
5. Run `cargo xtask test <level> --lang scala --gate` again to confirm all violations are fixed

## Contract

- You may create any additional Scala files under `src/main/scala/ming/`
- Do NOT modify test files under `src/test/`
- No external runtime dependencies — standard library only.
- **No `var` at class/object/trait level.** `var` inside method bodies is permitted for loop control and local accumulation — but mutable state must never escape the function.
- **NEVER run `./mill ming.test` directly.** Always use `cargo xtask test --lang scala`.

## Build & Test

```bash
cargo xtask test 01 --lang scala --gate   # test level 1 with quality gate
cargo xtask test all --lang scala          # test all levels (300s timeout)
./mill ming.reformat                        # auto-fix formatting
./mill ming.fix.check                       # run scalafix lint rules
```

## Refactoring Philosophy

- **Prefer radical type-level refactors over conservative patches.** The compiler catches all downstream breakage. A 10-file signature change that the compiler verifies is safer than a 1-file patch with a runtime check.
- **Expanding blast radius to reduce tech debt is encouraged.** Don't minimize changes — maximize type safety and structural clarity.
- **Extract files aggressively.** If any file exceeds 1500 lines, split it. Each distinct responsibility gets its own file under `src/main/scala/ming/`.
- **Clear module boundaries.** Each file should have a single responsibility and a clean public API.
- **Fix violations in touched files.** When modifying a file, fix issues in that file — don't defer.

## Code Philosophy

### Immutable-First Programming

- All shared state flows through function parameters and return values.
- Prefer recursion (with `@tailrec`) or collection pipelines (`foldLeft`, `map`, `flatMap`) over mutable loops.
- `var` inside a method body is acceptable for loop control when the immutable alternative is significantly more complex.
- No `var` at class/object/trait level — ever.

### Pure State Machines

When processing a sequence of inputs that accumulates state, prefer **pure state machines**:

- Model accumulated state as a **single case class**, not scattered across multiple variables.
- Processing should be a **pure function** `(State, Input) => (State, Output)`.
- Prefer recursion over iteration. Express processing loops as `@tailrec` recursive functions.

### Type Safety

- **Maximize type safety over minimal diffs.** The compiler is the last line of defense.
- **Opaque types for domain values** where it prevents confusion.
- **Sealed traits / enums for variant data.** Exhaustive pattern matching.
- **Enums over booleans for behavioral flags.**

### Error Handling

- **No `.get` on `Option` without justification.** Use pattern matching, `.getOrElse(throw ...)`, or `.fold`.
- **No silent error swallowing.** Forbidden: `.getOrElse(defaultValue)` hiding failures, `.toOption` discarding errors.
- **Structured error types** via sealed enums with named variants carrying domain-specific fields.

## Code Style

### Flat Control Flow

- **Prefer early returns and for-comprehensions over deeply nested `match`/`if`.**
- **Max nesting: 6 levels.** Use early returns, pattern matching, and extracted helpers.
- **Match cases with >3 lines dispatch to helpers.**

### Functional Style

- **Prefer collection pipelines** — `map`, `filter`, `foldLeft`, `collect`, `flatMap`.
- **Prefer pattern matching on `List`** over index-driven loops.
- **Prefer folds** for recursive data construction.
- **Prefer `collectFirst` / `find`** over flag variables.
- **Prefer recursion** over mutable loop variables.

## Structural Limits

| Constraint | Limit | Enforced by |
|---|---|---|
| File length | 1500 lines | `FileTooLong` scalafix rule |
| Method length | 300 lines | `MethodTooLong` scalafix rule |
| Nesting depth | 6 levels | `NestingDepth` scalafix rule |

## Lint Reference

### Scalafix Rules (hard errors)

| Rule | Threshold | What it checks |
|---|---|---|
| `FileTooLong` | 1500 lines | Total lines in any `.scala` file |
| `MethodTooLong` | 300 lines | Body lines in any `def` |
| `NestingDepth` | 6 levels | `if`/`match`/`while`/`for`/`try` depth |

### Denied patterns

| Denied pattern | Use instead |
|---|---|
| `var` at class/object/trait level | `val` + recursion, `foldLeft`, parameter passing |
| `.get` on `Option` (unsafe) | Pattern match, `.getOrElse(throw ...)`, or `.fold` |
| `null` anywhere | `Option[T]`, `None` |
| `asInstanceOf[T]` | Pattern matching with typed cases |
| Bare `catch { case _: Exception => }` | Typed error handling |
| `mutable.*` at class/object level | `List`, `Vector`, `foldLeft` |

### Style (refactor targets)

| Flagged pattern | Use instead |
|---|---|
| `.filter(...).map(...)` | `.collect { case ... => ... }` |
| `.find(...).map(...)` | `.collectFirst { case ... => ... }` |
| `.map(...).flatten` | `.flatMap(...)` |
| `for (i <- 0 until v.length)` | `for item <- v` or `.zipWithIndex` |
| Nested `match` 3+ levels | Extract inner match to named method |
