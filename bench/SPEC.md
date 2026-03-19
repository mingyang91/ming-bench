# Scheme Interpreter Specification

## Interface

Implement `eval_str` in `src/scheme/mod.rs`. It evaluates one or more Scheme expressions and returns the string representation of the last result.

- **Input:** One or more Scheme expressions separated by whitespace
- **Output:** String representation of the **last** expression's result
- **Errors:** Return an error for invalid programs (unbound variable, wrong argument count, type mismatch, etc.)

Side-effect-only forms (`define`, `set!`) produce a void value. When a void form is followed by another expression, the void is discarded:

```
"(define x 5) x"  →  "5"
```

## Output Format

| Value | Format | Example |
|-------|--------|---------|
| Integer | Decimal, no leading zeros | `42`, `-7` |
| Boolean | `#t` or `#f` | `#t` |
| String | Surrounded by double quotes | `"hello"` |
| Symbol | Bare name | `foo` |
| Pair / List | Parenthesized, space-separated | `(1 2 3)` |
| Empty list | `()` | `()` |

## Levels

Each level builds on all previous levels. Implement in order.

### Level 1 — Atoms
Self-evaluating literals: integers, booleans, strings.

### Level 2 — Arithmetic
`+`, `-`, `*`, `/`. Variadic (`(+ 1 2 3 4)`). Unary minus (`(- 10)` → `-10`). Nested expressions.

### Level 3 — Comparisons & Logic
`<`, `>`, `=`, `<=` on numbers. `not`, `and`, `or` with short-circuit semantics. `and`/`or` return the deciding value, not just booleans.

### Level 4 — Define & If
`define` binds variables. `if` branches on a condition. `quote` returns its argument unevaluated. Falsy: only `#f`. Everything else (including `0`, `""`, `'()`) is truthy.

### Level 5 — Lambda & Closures
`lambda` creates closures that capture their definition environment. Shorthand: `(define (f x) body)` ≡ `(define f (lambda (x) body))`. Recursion through self-reference.

### Level 6 — Lists
`cons` constructs pairs. `car`/`cdr` destructure. `null?` tests for empty list. `list` creates a proper list. `length` counts elements. `'(1 2 3)` is sugar for nested cons.

### Level 7 — Recursive List Programs
User-defined `map`, `filter`, `append`, `reverse` using the primitives from Level 6. No new built-ins needed.

### Level 8 — Let, Begin, Cond
`let` introduces local bindings. `begin` sequences expressions, returns last. `cond` is multi-branch conditional with `else` clause.

### Level 9 — Type Predicates
`string?`, `number?`, `boolean?`, `pair?`, `symbol?` — each returns `#t` or `#f`.

### Level 10 — Tail Call Optimization
Tail-position calls must not grow the stack. `(loop 1000000)` with a tail-recursive body must not overflow.

### Level 11 — set! & Mutation
`set!` mutates an existing binding (error if unbound). Closures that share a binding observe each other's mutations.

### Level 12 — Variadic & Apply
Dot notation for rest parameters: `(define (f x . rest) rest)`. `apply` calls a function with an argument list. `apply` accepts prefix arguments: `(apply + 1 2 '(3 4))`.

### Level 13 — Tail Position in All Forms
TCO must work through `cond`, named `let`, `and`, `or`, `begin`, and `let` body — not just `if`.

### Level 14 — First-Class Continuations
`call/cc` (call-with-current-continuation) captures the current continuation as a first-class value. Supports: non-local exit, saving and resuming continuations, continuations as values passed to higher-order functions.

### Level 15 — Hygienic Macros
`define-syntax` + `syntax-rules`. Pattern matching with literals and ellipsis (`...`). Macro-introduced bindings do not capture user bindings (hygiene). Definition-site bindings are preserved.

### Level 16 — Integration
Combined use of continuations, macros, mutation, and tail calls.
