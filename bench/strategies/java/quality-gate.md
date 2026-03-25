# Quality Gate — Cleanup Pass

All tests for this level already pass. Your job is to **refactor the code to production quality** while keeping all tests green.

## What To Do

1. Run `cargo xtask test <level> --lang java --gate` to see quality-gate violations
2. Fix ALL violations — Checkstyle structural limits, code organization
3. **Radical refactoring is encouraged.** Extract classes, split large methods, flatten nesting, improve abstractions. The code works — make it maintainable.
4. Run `cargo xtask test all --lang java` to verify no regressions
5. Run `cargo xtask test <level> --lang java --gate` again to confirm all violations are fixed

## Contract

- You may create any additional classes under `src/main/java/ming/`
- Do NOT modify test files under `src/test/`
- No external runtime dependencies — standard library only.
- **NEVER run `./gradlew test` directly.** Always use `cargo xtask test --lang java`.

## Build & Test

```bash
cargo xtask test 01 --lang java --gate   # test level 1 with quality gate
cargo xtask test all --lang java          # test all levels (300s timeout)
```

## Refactoring Philosophy

- **Prefer radical refactors over conservative patches.** The compiler catches all downstream breakage. A 10-class signature change that the compiler verifies is safer than a 1-class patch with a runtime check.
- **Expanding blast radius to reduce tech debt is encouraged.** Don't minimize changes — maximize structural clarity.
- **Extract classes aggressively.** If any file exceeds 1500 lines, split it. Each distinct responsibility gets its own class under `src/main/java/ming/`.
- **Clear class boundaries.** Each class should have a single responsibility and a clean public API.
- **Proper encapsulation.** Private fields, package-private helpers, thin public API.
- **Fix violations in touched files.** When modifying a file, fix issues in that file — don't defer.

## Recommended Design Patterns

| Pattern | When to use |
|---|---|
| **Sealed interface + exhaustive `switch`** | Type-safe dispatch without `instanceof` chains. Java 21 pattern matching makes this clean. |
| **Strategy / registry** | Replace large switch/if dispatching with `Map<String, Handler>`. Each handler registered once. |
| **Facade** | Thin public API class wrapping complex internals. |
| **SRP class extraction** | One class, one responsibility. Split when a class does multiple unrelated things. |
| **Composition over inheritance** | Delegate to collaborators, don't extend base classes. |

## Code Style

### Clean OOP Java

- **Use `sealed` interfaces and `record` types** for data hierarchies where appropriate.
- **Use `switch` expressions with pattern matching** (Java 21) instead of `instanceof` chains.
- **`Optional<T>` for nullable return types** where it makes the API clearer.
- **Specific exception types** instead of `catch (Exception e)` catch-all.
- **`final` fields where values don't change** — prefer immutability where natural, but mutable state is fine when it's the right tool.

### Flat Control Flow

- **Max nesting: 6 levels.** Use early returns, extracted helpers, and pattern matching to stay flat.
- **Switch arms with >3 lines should dispatch to helper methods.**
- **Prefer early return over deep nesting.**

## Structural Limits

| Constraint | Limit | Enforced by |
|---|---|---|
| File length | 1500 lines | Checkstyle `FileLength` |
| Method length | 300 lines | Checkstyle `MethodLength` |
| Nesting depth | 6 levels | Checkstyle `NestedIfDepth` / `NestedForDepth` / `NestedTryDepth` |

## Denied Patterns

| Denied pattern | Use instead |
|---|---|
| God class (>1500 lines) | Extract into multiple classes with clear responsibilities |
| Giant method (>300 lines) | Extract private helper methods |
| 3+ `instanceof` chain | `switch` with pattern matching |
| Deeply nested if/switch (>6) | Early return, extracted methods |
| `catch (Exception e)` catch-all | Specific exception types |
| Bare `null` return without clear contract | `Optional<T>` or document the null contract |
