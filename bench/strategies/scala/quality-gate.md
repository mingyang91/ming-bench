# MING Scheme Interpreter

Implement a Scheme interpreter in Scala 3. Read `SPEC.md` for the full specification.

## Contract

- Implement `evalStr` in `src/main/scala/ming/Evaluator.scala`
- Implement `evalStrWithOutput` in `Evaluator.scala` (needed from Level 5)
- Extend `EvalError` in `src/main/scala/ming/EvalError.scala` as needed
- You may create any additional Scala files under `src/main/scala/ming/`
- Do NOT modify test files under `src/test/`
- No external runtime dependencies — standard library only. Test dependencies (munit, Gson) are already in `build.mill`.
- **No `var` at class/object/trait level.** `var` inside method bodies is permitted for loop control and local accumulation — but mutable state must never escape the function. Don't return mutable references, don't store `var` in fields, don't pass mutable state to other functions. If you can solve it immutably, do so. `var` is a last resort for local control flow, not a design tool.
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

**Formatting & linting (locally):**

```bash
./mill ming.reformat        # format code with scalafmt
./mill ming.checkFormat     # check formatting without modifying
./mill ming.fix.check       # run scalafix lint rules (FileTooLong, MethodTooLong, NestingDepth)
```

## Development Strategy

- **Implement levels in order (L1 → L26).** Each level builds on the previous.
- **After implementing each level, run its tests before moving on.**
- **If a level's tests fail, fix them before proceeding.**
- **Code first, debug from test output.** Don't mentally simulate — let the test runner do that.
- **Maximum 10 turns of reading before first code change.** If you haven't written or edited a file by turn 10, your analysis is too deep — write a first attempt and iterate from test failures. Tests are the source of truth, not mental simulation.

## Refactoring Philosophy

**Prefer radical type-level refactors over conservative patches.** This is a statically-typed Scala 3 codebase — the compiler catches all downstream breakage. When fixing an issue, always choose the solution that encodes the constraint in the type system, even if it touches many files. A 10-file signature change that the compiler verifies is **safer** than a 1-file patch with a runtime check.

- **Don't minimize blast radius — maximize type safety.** Changing a method from `List[T]` to `(T, List[T])` across 6 files is not "risky" — the compiler finds every call site. A runtime `.head` hidden in one file is the real risk.
- **The compiler is the last line of defense.** If a refactor compiles, it's correct. Treat compilation as the acceptance test for type-level changes.
- **Type precision is not over-engineering.** Over-engineering means unnecessary abstractions, strategy patterns for one implementation. Using opaque types, sealed traits, or `NonEmptyList` is the opposite — it removes complexity (runtime checks) by shifting it to the compiler.

## Code Philosophy

The quality gate enforces structure mechanically; these rules are the design intent behind those checks. Follow them proactively — the gate is a safety net, not a substitute for judgment.

### Immutable-First Programming

**Prefer immutable state. Use `var` only as a local last resort.**

- All shared state flows through function parameters and return values.
- Prefer recursion (with `@tailrec`) or collection pipelines (`foldLeft`, `map`, `flatMap`) over mutable loops.
- `var` inside a method body is acceptable for loop control and local accumulation when the immutable alternative is significantly more complex. Mutable state must not escape the method.
- No `var` at class/object/trait level — ever.

```scala
// Preferred — pure pipeline
val count = items.length

// Acceptable — local var for complex loop control
var i = 0
while i < tokens.length && tokens(i) != delimiter do i += 1

// FORBIDDEN — var escaping method scope
class Foo:
  var state = 0  // NO: mutable field
```

### Pure State Machines

When processing a sequence of inputs that accumulates state (e.g., multi-step parsing, staged evaluation), prefer **pure state machines** over scattered mutable fields.

- Model accumulated state as a **single case class**, not scattered across multiple variables.
- Processing should be a **pure function** `(State, Input) => (State, Output)` — no side effects in the transition logic.
- This separates **what the state becomes** from **how the sequence is driven**.
- Pure transition functions are directly testable without wiring.
- **Prefer recursion over iteration.** Express processing loops as `@tailrec` recursive functions rather than mutable accumulators. Recursion makes termination conditions and state flow explicit in the type signatures.

```scala
// Bad — scattered mutable state
var pos = 0
var depth = 0
var tokens = List.empty[Token]
while pos < input.length do
  // mutate pos, depth, tokens...

// Good — single state case class + pure transition
case class ScanState(pos: Int, depth: Int, tokens: List[Token])

def scan(state: ScanState, input: String): ScanState = ...

@tailrec
def scanAll(state: ScanState, input: String): ScanState =
  if state.pos >= input.length then state
  else scanAll(scan(state, input), input)
```

**Scope:** Applies to any multi-step processing pattern. The key principle is: one immutable state value in, one immutable state value out.

### Type Safety

- **Maximize type safety over minimal diffs.** The compiler is the last line of defense. If it compiles, it's correct.
- **Opaque types for domain values** where it prevents confusion. Propagate constraints through signatures — don't downgrade and re-validate internally.

  ```scala
  // Good — type-safe wrappers prevent confusion
  opaque type Arity = Int
  object Arity:
    def apply(n: Int): Arity = n
    extension (a: Arity) def value: Int = a

  // Bad — raw Int everywhere
  def checkArgs(expected: Int, got: Int): Unit = ...
  ```

- **Sealed traits / enums for variant data.** Exhaustive pattern matching — compiler enforces handling every variant.
- **Enums over booleans for behavioral flags.** `enum Mode { case Strict, Lenient }` communicates intent where `strict: Boolean` does not. The compiler can enforce exhaustive handling.
- **Introduce types when they earn their keep.** A wrapper is worth it when it prevents a class of bugs across multiple call sites. Don't wrap values that are only used locally or have no ambiguity.

### Error Handling

- **No `.get` on `Option` without justification.** Use pattern matching, `.getOrElse(throw new EvalError(...))`, or `.fold`.
- **No silent error swallowing.** Forbidden: `.getOrElse(defaultValue)` hiding parse failures, `.toOption` discarding errors, `Try(x).toOption`.
- **Trusted vs untrusted paths:**
  - **Trusted** (internal data, AST nodes, lookups after bind): bugs → `throw` / `sys.error`
  - **Untrusted** (user source code): errors → `Left(...)` / `throw EvalError(...)`

### Typed Error Model (Sealed Enums)

Use `enum` error types with named variants carrying structured context. Compose errors via wrapping, don't flatten to strings.

```scala
// Example — generic domain error with structured variants
enum ServiceError:
  case NotFound(key: String)
  case InvalidInput(field: String, reason: String)
  case Upstream(cause: UpstreamError)

enum UpstreamError:
  case Timeout(endpoint: String)
  case MalformedResponse(body: String)
```

- Each variant must carry domain-specific fields (e.g., `NotFound(key)`).
- Variants that wrap a formatted `String` message (e.g., `Other(String)`) are a last resort — they defeat pattern matching.
- Exhaustive `match` — compiler enforces handling every variant.

### Effect Marking (Visible Signatures)

Make capabilities visible in function signatures — callers see exactly what a function does:
- Mutation: return new values (not hidden side effects)
- Fallibility: `Either[E, T]` or declared exceptions (not unchecked exceptions)
- Side-channel output: explicit accumulator parameter (not global/thread-local state)

No hidden side effects.

## Code Style

### Flat Control Flow

- **Flat control flow.** Prefer early returns, `match` at terminal positions, and `Either`-chaining over deeply nested `match`/`if`.

  ```scala
  // Bad — nested match in mid-chain
  val result = parse(input) match
    case Right(ast) =>
      eval(ast) match
        case Right(value) => Right(format(value))
        case Left(err) => Left(err)
    case Left(err) => Left(err)

  // Good — for-comprehension or flatMap chain
  for
    ast   <- parse(input)
    value <- eval(ast)
  yield format(value)
  ```

- **No premature helpers (<5 ops).** If logic is < 5 composed operators/steps, inline at call site.
- **Extract helpers at >= 5 ops.** Reuse existing helpers before writing new ones.
- **No premature abstractions.** Three similar lines > one abstraction used once.
- **Proactive naming review.** Fix misleading/stale names when modifying code.

### Functional Style

**Think FP first.** For each operation, consider the functional approach before reaching for mutation or imperative loops. Use the FP version when it's clear and concise. Fall back to imperative when the FP version obscures intent or adds significant complexity.

- **Prefer collection pipelines for transforms** — `map`, `filter`, `foldLeft`, `collect`, `flatMap` over accumulator patterns.
- **Prefer pattern matching on `List`** over index-driven loops.
- **Prefer folds** for recursive data construction.
- **Prefer `collectFirst` / `find`** over flag variables.
- **Prefer recursion** over mutable loop variables when termination logic is non-trivial.

### Match Expression Discipline

- **Match cases with >3 lines dispatch to helpers.** A `case` branch should call a method, not inline multi-line logic.

  ```scala
  // Bad — inline logic in match
  command match
    case Command.Create(args) =>
      // 20 lines of creation logic...

  // Good — dispatch to handlers
  command match
    case Command.Create(args) => handleCreate(args, state)
    case Command.Delete(args) => handleDelete(args, state)
    case Command.Update(args) => handleUpdate(args, state)
  ```

## Structural Limits

These limits are enforced by **scalafix** custom rules (`FileTooLong`, `MethodTooLong`, `NestingDepth`) during the quality gate cleanup pass.

- **Hard cap: 500 lines per file.** Split by subsystem into dedicated files. Each distinct responsibility gets its own file under `src/main/scala/ming/`.
- **Method hard cap: 300 lines.** If a method needs scrolling, extract helpers.
- **Max nesting: 6 levels.** Use early returns, pattern matching, and extracted helpers to reduce brace depth. Nesting-increasing constructs: `if`, `match`, `while`, `for`, `for/yield`, `try`.

  ```scala
  // Bad — 4+ levels deep
  config match
    case Config.A(inner) =>
      if inner.enabled then
        for item <- inner.items do
          if item.valid then
            process(item)

  // Good — flat with helpers
  def handleConfigA(inner: Inner): Unit =
    if !inner.enabled then return ()
    inner.items.filter(_.valid).foreach(process)
  ```

## Logging

- Use `System.err.println` for debug logging. Control verbosity via an environment variable.
- **Log at decision points** — dispatch branches, error paths. Helps trace failures.

## Runtime Assertion Checks

- **Use `assert()` / `require()` on invariants.** Catch broken assumptions before corrupt state propagates.
- **Where to assert:**
  - After lookup operations — key was actually bound
  - After structural operations — data invariants hold (e.g., non-empty after split)
  - Recursive loops — depth doesn't silently overflow
- **Where NOT to assert:** User input validation (use typed errors), hot loops (use errors).

## Development Workflow

- **Fix code smells immediately.** Fix on the spot, don't track for later.
- **When touching a file, fix violations in that file.** Do not defer. Do not rewrite unrelated files unprompted.
- **Proactive refactoring is mandatory.** When implementing a new feature, if you notice surrounding code that violates nesting limits, function size caps, or functional style rules — fix it in the same pass.
- **Blast radius is not a concern — rule compliance is.** If existing code violates structural limits or functional style rules, refactor aggressively. Split oversized files, extract bloated methods into helpers, rewrite imperative loops as pipelines.

## Lint Reference

These lints are enforced by **scalafix** and **scalafmt** in a **separate cleanup pass** at the end of each level — not during coding. Focus on making tests pass first. You will get a dedicated session to fix lint violations afterward. This table is a quick-fix reference for that cleanup session.

### Scalafix Rules (hard errors)

| Rule | Threshold | What it checks |
|---|---|---|
| `FileTooLong` | 500 lines | Total lines in any `.scala` file |
| `MethodTooLong` | 300 lines | Body lines in any `def` |
| `NestingDepth` | 6 levels | `if`/`match`/`while`/`for`/`try` depth in any `def` |

### Denied patterns (code review)

| Denied pattern | Use instead |
|---|---|
| `var` at class/object/trait level | `val` + recursion, `foldLeft`, parameter passing. Local `var` in methods is OK. |
| `.get` on `Option` (unsafe) | Pattern match, `.getOrElse(throw ...)`, or `.fold` |
| `null` anywhere | `Option[T]`, `None`, or sentinel values |
| `asInstanceOf[T]` | Pattern matching with typed cases |
| Bare `catch { case _: Exception => }` | Typed error handling, never swallow exceptions |
| `mutable.*` at class/object level | `List`, `Vector`, `foldLeft`. Local mutable collections in methods are OK. |

### Scalafmt (auto-fixable)

Run `./mill ming.reformat` to auto-fix formatting. The gate checks with `./mill ming.checkFormat`.

### Style (refactor targets)

| Flagged pattern | Use instead |
|---|---|
| `.filter(...).map(...)` | `.collect { case ... => ... }` |
| `.find(...).map(...)` | `.collectFirst { case ... => ... }` |
| `.map(...).flatten` | `.flatMap(...)` |
| `for (i <- 0 until v.length)` | `for item <- v` or `.zipWithIndex` |
| Nested `match` 3+ levels | Extract inner match to named method |
| `if (x != null)` | `Option(x).fold(...)` |
