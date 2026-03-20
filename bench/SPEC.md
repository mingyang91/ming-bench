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

### Level 10 — Error Quality
All error messages must include source position info (line:col). Errors for undefined variables, wrong argument count, type mismatches, syntax errors, and division by zero must carry position information.

### Level 11 — Display, Write, Newline
`display` outputs a value without quotes. `write` outputs with quotes (for strings). `newline` outputs a newline. Implement `eval_str_with_output` to capture side-effect output alongside the expression result.

### Level 12 — String & Symbol Operations
`string-append`, `string-length`, `substring`, `string->number`, `number->string`. `symbol->string`, `string->symbol`. `string-ref` returns a character; `char?` predicate.

### Level 13 — Mutable Strings (R5RS)
`string-set!` mutates a character in a string by index. `string-copy` returns a mutable copy of a string. Strings created by `string-copy` are mutable.

### Level 14 — String Immutability (R7RS)
Strings are now immutable. `string-set!` must raise an error. Use `string->list` and `list->string` for character-level transformations. `string-copy` still works (returns an immutable copy). `char->integer` and `integer->char` convert between characters and their integer code points.

### Level 15 — Tail Call Optimization
Tail-position calls must not grow the stack. `(loop 1000000)` with a tail-recursive body must not overflow.

### Level 16 — set! & Mutation
`set!` mutates an existing binding (error if unbound). Closures that share a binding observe each other's mutations.

### Level 17 — Variadic & Apply
Dot notation for rest parameters: `(define (f x . rest) rest)`. `apply` calls a function with an argument list. `apply` accepts prefix arguments: `(apply + 1 2 '(3 4))`.

### Level 18 — Tail Position in All Forms
TCO must work through `cond`, named `let`, `and`, `or`, `begin`, and `let` body — not just `if`.

### Level 19 — First-Class Continuations
`call/cc` (call-with-current-continuation) captures the current continuation as a first-class value. Supports: non-local exit, saving and resuming continuations, continuations as values passed to higher-order functions.

### Level 20 — Hygienic Macros
`define-syntax` + `syntax-rules`. Pattern matching with literals and ellipsis (`...`). Macro-introduced bindings do not capture user bindings (hygiene). Definition-site bindings are preserved.

### Level 21 — Integration
Combined use of continuations, macros, mutation, and tail calls.

### Level 22 — Deep Equality
`equal?` compares values recursively. Works on numbers, strings, booleans, symbols, lists, and nested structures.

### Level 23 — Recursive Local Bindings
`letrec` and `letrec*`. All bindings in `letrec` are mutually visible. `letrec*` bindings are visible sequentially.

### Level 24 — Case Expression
`case` dispatches on datum equality (`eqv?`). Also requires `eqv?` builtin for value equivalence on numbers, chars, symbols, and booleans.

### Level 25 — Vectors
`vector`, `make-vector`, `vector-ref`, `vector-set!`, `vector-length`, `vector?`. Fixed-size mutable arrays. `vector->list` and `list->vector` for conversion.

### Level 26 — Numeric Utilities
`abs` returns absolute value. `modulo` and `remainder` compute division remainders (they differ in sign for negative operands — `modulo` takes the sign of the divisor, `remainder` takes the sign of the dividend). `quotient` returns integer division truncated toward zero. `min` and `max` are variadic. `expt` computes integer exponentiation.

### Level 27 — Numeric Predicates
`zero?`, `positive?`, `negative?` test the sign of a number. `odd?`, `even?` test integer parity. All take a single numeric argument and return `#t` or `#f`.

### Level 28 — List Utilities
`list-ref` returns the element at a given index. `list-tail` returns the sublist starting at a given index. `list?` returns `#t` for proper lists (including `'()`), `#f` for dotted pairs and non-pairs. `assoc` searches an association list using `equal?`. Built-in `map` supports multiple list arguments: `(map + '(1 2) '(3 4))` → `(4 6)`.

### Level 29 — Character Operations
Character literals: `#\a`, `#\Z`, `#\5`, `#\space`, `#\newline`. `char-alphabetic?` and `char-numeric?` are character class predicates. `char-upcase` and `char-downcase` convert case. `char=?` and `char<?` compare characters by code point.

### Level 30 — String Comparison
`string=?` tests string equality. `string<?` compares lexicographically. `string-ci=?` is case-insensitive equality. `string-upcase` and `string-downcase` return a new string with all characters converted.
