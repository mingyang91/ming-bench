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

### Level 1 — Atoms, Arithmetic & Comparisons
Self-evaluating literals: integers, booleans, strings. `+`, `-`, `*`, `/`. Variadic (`(+ 1 2 3 4)`). Unary minus (`(- 10)` → `-10`). Nested expressions. `<`, `>`, `=`, `<=` on numbers. `not`, `and`, `or` with short-circuit semantics. `and`/`or` return the deciding value, not just booleans.

### Level 2 — Variables, Conditionals & Lambda
`define` binds variables. `if` branches on a condition. `quote` returns its argument unevaluated. Falsy: only `#f`. Everything else (including `0`, `""`, `'()`) is truthy. `lambda` creates closures that capture their definition environment. Shorthand: `(define (f x) body)` ≡ `(define f (lambda (x) body))`. Recursion through self-reference.

### Level 3 — Lists, Recursion, Let/Begin/Cond & Predicates
`cons` constructs pairs. `car`/`cdr` destructure. `null?` tests for empty list. `list` creates a proper list. `length` counts elements. `'(1 2 3)` is sugar for nested cons. User-defined `map`, `filter`, `append`, `reverse` using these primitives. `let` introduces local bindings. `begin` sequences expressions, returns last. `cond` is multi-branch conditional with `else` clause. `string?`, `number?`, `boolean?`, `pair?`, `symbol?` — each returns `#t` or `#f`.

### Level 4 — Error Quality
All error messages must include source position info (line:col). Errors for undefined variables, wrong argument count, type mismatches, syntax errors, and division by zero must carry position information.

### Level 5 — Display, Write & String/Symbol Operations
`display` outputs a value without quotes. `write` outputs with quotes (for strings). `newline` outputs a newline. Implement `eval_str_with_output` to capture side-effect output alongside the expression result. `string-append`, `string-length`, `substring`, `string->number`, `number->string`. `symbol->string`, `string->symbol`. `string-ref` returns a character; `char?` predicate.

### Level 6 — Mutable Strings (R5RS)
`string-set!` mutates a character in a string by index. `string-copy` returns a mutable copy of a string. Strings created by `string-copy` are mutable.

### Level 7 — Tail Call Optimization (All Forms)
Tail-position calls must not grow the stack. `(loop 1000000)` with a tail-recursive body must not overflow. TCO must work through `cond`, named `let`, `and`, `or`, `begin`, and `let` body — not just `if`.

### Level 8 — set! & Mutation
`set!` mutates an existing binding (error if unbound). Closures that share a binding observe each other's mutations.

### Level 9 — Variadic & Apply
Dot notation for rest parameters: `(define (f x . rest) rest)`. `apply` calls a function with an argument list. `apply` accepts prefix arguments: `(apply + 1 2 '(3 4))`.

### Level 10 — First-Class Continuations
`call/cc` (call-with-current-continuation) captures the current continuation as a first-class value. Supports: non-local exit, saving and resuming continuations, continuations as values passed to higher-order functions.

### Level 11 — Hygienic Macros
`define-syntax` + `syntax-rules`. Pattern matching with literals and ellipsis (`...`). Macro-introduced bindings do not capture user bindings (hygiene). Definition-site bindings are preserved.

### Level 12 — Integration
Combined use of continuations, macros, mutation, and tail calls.

### Level 13 — Numeric/Char/String Utilities
`abs` returns absolute value. `modulo` and `remainder` compute division remainders (they differ in sign for negative operands — `modulo` takes the sign of the divisor, `remainder` takes the sign of the dividend). `quotient` returns integer division truncated toward zero. `min` and `max` are variadic. `expt` computes integer exponentiation. `zero?`, `positive?`, `negative?` test the sign of a number. `odd?`, `even?` test integer parity. `list-ref` returns the element at a given index. `list-tail` returns the sublist starting at a given index. `list?` returns `#t` for proper lists (including `'()`), `#f` for dotted pairs and non-pairs. `assoc` searches an association list using `equal?`. Built-in `map` supports multiple list arguments: `(map + '(1 2) '(3 4))` → `(4 6)`. Character literals: `#\a`, `#\Z`, `#\5`, `#\space`, `#\newline`. `char-alphabetic?` and `char-numeric?` are character class predicates. `char-upcase` and `char-downcase` convert case. `char=?` and `char<?` compare characters by code point. `string=?` tests string equality. `string<?` compares lexicographically. `string-ci=?` is case-insensitive equality. `string-upcase` and `string-downcase` return a new string with all characters converted.

### Level 14 — String Immutability (R7RS)
Strings are now immutable. `string-set!` must raise an error. Use `string->list` and `list->string` for character-level transformations. `string-copy` still works (returns an immutable copy). `char->integer` and `integer->char` convert between characters and their integer code points.

### Level 15 — Deep Equality, Letrec, Case, Vectors & Do
`equal?` compares values recursively. Works on numbers, strings, booleans, symbols, lists, and nested structures. `letrec` and `letrec*`. All bindings in `letrec` are mutually visible. `letrec*` bindings are visible sequentially. `case` dispatches on datum equality (`eqv?`). Also requires `eqv?` builtin. `vector`, `make-vector`, `vector-ref`, `vector-set!`, `vector-length`, `vector?`. Fixed-size mutable arrays. `vector->list` and `list->vector` for conversion. `(do ((var init step) ...) (test expr ...) body ...)` is an iteration construct. Variables are bound to their init values, then on each iteration all step expressions are evaluated using the *previous* iteration's values (parallel update, like `let` not `let*`). When test is true, the expr values are evaluated and the last is returned. Variables with no step expression keep their value across iterations.

### Level 16 — dynamic-wind
`dynamic-wind` takes three thunks: in-thunk, body-thunk, out-thunk. The in-thunk runs before the body, the out-thunk runs after — even on non-local exit via `call/cc`. On continuation re-entry, the in-thunk fires again. Nested `dynamic-wind` must unwind/rewind in the correct order. `dynamic-wind` returns the body's value.

### Level 17 — guard, raise & with-exception-handler
`raise` signals an exception with an arbitrary value. `guard` catches exceptions with cond-like clauses that test the raised value. If no clause matches and there is no `else`, the exception is re-raised. `with-exception-handler` installs a low-level handler. `guard` + `dynamic-wind` must cooperate: out-thunks run when an exception unwinds the stack.

### Level 18 — values & call-with-values
`values` returns zero or more values. A single value is transparent — `(+ 1 (values 41))` → `42`. `call-with-values` takes a producer thunk and a consumer procedure; the producer's values become the consumer's arguments. Zero values are valid: `(call-with-values (lambda () (values)) (lambda () 42))` → `42`. Nested `call-with-values` composes.

### Level 19 — Exact Arithmetic & Rationals
Numbers are either exact or inexact. Integers are exact. Division of exact integers produces exact rationals: `(/ 1 3)` → `1/3`. Rational arithmetic preserves exactness and always simplifies: `(/ 6 4)` → `3/2`. `exact?`, `inexact?` test exactness. `exact->inexact` and `inexact->exact` convert between representations. `numerator` and `denominator` extract parts of a rational. `integer?` returns `#f` for non-integer rationals but `#t` for `4/2`. `rational?` returns `#t` for all exact numbers. Cross-tower comparison: `(= 1/2 0.5)` → `#t`.

### Level 20 — define-record-type
R7RS `define-record-type` creates a new disjoint type with a constructor, predicate, and field accessors. Each record type is distinct — `(point? '(1 2))` → `#f`. Multiple record types can coexist. Records work with higher-order functions and can contain other records.

### Level 21 — Pair Mutation & Cycle Detection
`set-car!` and `set-cdr!` mutate pairs in place. Shared structure is observable: `(define b a)` then `(set-car! a 99)` makes `(car b)` → `99`. Circular lists created via `set-cdr!` must not cause `list?` to infinite-loop — `list?` on a circular structure must return `#f`. Deep tail-recursive allocation (1M cons cells) must not OOM — old unreachable cells must be reclaimable.

### Level 22 — syntax-case
`syntax-case` is a more powerful macro system than `syntax-rules`. Macros are defined as transformer procedures that receive a syntax object. Supports: pattern matching with `syntax-case`, template construction with `#'(...)`, hygiene (same guarantees as `syntax-rules`), `syntax->datum` and `datum->syntax` for computed identifiers, and ellipsis patterns. Fender expressions (guards) on patterns are optional.

### Level 23 — Final Integration
Combined use of all features: dynamic-wind + guard for resource cleanup on exceptions, values + call/cc for multi-value continuations, records as exception payloads, rationals in data structures (lists, vectors, record fields), macros generating record-based code, TCO inside guard bodies, and a full pipeline combining macros + records + exceptions + values + continuations.

### Level 24 — case-lambda
`case-lambda` creates a procedure with multiple clauses, each with different arity. When called, the clause matching the argument count is selected. Supports rest parameters with dot notation in individual clauses. `(procedure? (case-lambda ...))` returns `#t`. Wrong arity (no matching clause) raises an error. Works with `apply`, higher-order functions, and recursion.

### Level 25 — procedure? on all callable types
`procedure?` must return `#t` for every callable value: regular lambdas, `case-lambda` procedures (from L24), builtin procedures, and continuations captured by `call/cc`. It returns `#f` for all non-callable values (numbers, strings, booleans, lists, vectors, etc.).

### Level 26 — Real-World Integration Stress
All features from L1-L25 are exercised together by large (1000+ line) real-world Scheme programs. No new language features — this level tests whether your interpreter handles real code at scale. Programs include a type inferencer (dynamic, ~2,300 lines) and a complete macro expander (alexpander, ~1,950 lines) that stress closures, continuations, mutation, vectors, and macros simultaneously.

### Level 27 — Concurrent Evaluation
`eval_str` must be safe for concurrent use from multiple threads. Independent evaluations must not interfere — each call gets its own environment, and output capture must be isolated per-call. No global mutable state. The interpreter does not need Scheme-level threading primitives; it must simply be reentrant. Tests spawn 4-16 threads calling `eval_str` in parallel with independent computations, closures with `set!` counters, and `display` output capture.
