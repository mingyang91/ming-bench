# MING — Ming Interpreter Nurture Gauntlet

A benchmark framework for measuring how **prompt engineering strategies** affect coding agent performance. Agents build a Scheme interpreter in Rust from scratch — 100+ tests across 25 difficulty levels, from basic arithmetic to first-class continuations, hygienic macros, and data structure extensions.

## Why This Exists

Coding agents (Claude Code, Codex, OpenCode, etc.) can write working software, but their effectiveness varies wildly depending on the instructions they receive. This benchmark answers a specific question:

> **Does adding structure to agent instructions — quality gates, code style rules, modular architecture enforcement — improve outcomes compared to minimal "just do it" prompts?**

The task is deliberately chosen to stress-test this: a Scheme interpreter requires the agent to make hundreds of architectural decisions (data representation, evaluation strategy, environment model, continuation implementation) over 25 progressively harder levels. Bad early decisions compound. Good structure should help.

## How It Works

### The Task

The agent receives:
- A function signature: `eval_str(input: &str) -> Result<String, EvalError>`
- 100+ test cases across 25 levels (each test loads Scheme code from a `.scm` fixture file)
- Instructions in `bench/CLAUDE.md` (the only file the agent reads for guidance)

The agent implements a complete Scheme interpreter from scratch — lexer, parser, environment, evaluator, tail-call optimization, continuations, and hygienic macros. No starter code. No libraries beyond `thiserror` for error types.

### Project Structure

```
ming/                     # framework — orchestration & analysis
  xtask/                  # CLI: run-agent, test, bench, tokens, analyze, watch
  Dockerfile.bench        # container image for sandboxed testing
  CLAUDE.md               # framework maintainer instructions

  bench/                  # agent playground — what the agent sees
    SPEC.md               # interpreter specification (shared, all strategies)
    strategies/
      default.md          # group 1: minimal instructions
      quality-gate.md     # group 2: quality gates + code style rules
    CLAUDE.md             # ← symlink to strategy file, created at launch
    src/scheme/           # agent implements here
    src/scheme/tests/     # test suite (read-only to agent)
    src/scheme/tests/fixtures/  # .scm files loaded by tests
```

Agents are launched with `cwd = bench/` and only interact with files there. Framework code (xtask, Dockerfile) lives above the agent's working directory.

### The Experiment: Strategies

| Strategy | CLAUDE.md | Cargo feature | What it tests |
|----------|-----------|---------------|---------------|
| `default` | Minimal: implement the spec, test each level, fix failures | *(none)* | Baseline — how agents perform with standard guidance |
| `quality-gate` | Adds: clippy enforcement, mod.rs size limits, code style rules | `quality-gate` enabled | Whether structural enforcement improves agent code quality and completion rate |

Both strategies share `SPEC.md` (identical task definition) and the same test suite. The only difference is the instructions in `CLAUDE.md` and whether lint enforcement is active. Lint attributes in `bench/src/lib.rs` use `cfg_attr(feature = "quality-gate", ...)` — they are inert by default and activated per-worktree when the strategy includes `clippy.toml`. Strategy selection happens at runtime via `--strategy`.

### Execution Modes

Each agent run uses one of two modes:

- **Full mode** — one agent session tackles all 25 levels. Simpler, but if the agent gets stuck it burns budget.
- **Levels mode** — orchestrator runs a fresh agent per level. Fail-fast: stops on first failure. Per-level session data for granular analysis.

### Sandboxed Testing

Tests never run on the host. Every test execution happens inside a container with hard resource limits:
- 1 GB memory, 1 CPU, 256 PIDs
- 30s timeout per level, 300s for the full suite
- OOM or timeout = test failure

This prevents agent-written infinite loops or memory bombs from crashing the benchmark host.

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
| 10 | Error quality | 6 | Error messages with source position (line:col) |
| 11 | Display/write | 6 | `display`, `write`, `newline`, output capture |
| 12 | String & symbol ops | 7 | `string-append`, `substring`, `string->number`, `char?` |
| 13 | **Mutable strings** | 3 | `string-set!`, `string-copy` (R5RS) |
| 14 | **String immutability** | 4 | `string-set!` errors, `string->list`/`list->string` (R7RS) |
| 15 | Tail call opt. | 3 | Deep recursion without stack overflow |
| 16 | set! & mutation | 5 | Mutable bindings, shared state in closures |
| 17 | Variadic & apply | 6 | Rest args, `apply` with prefix args |
| 18 | Tail position (all) | 5 | TCO in `cond`, named `let`, `and`/`or`, `begin` |
| 19 | **call/cc** | 10 | First-class continuations, non-local exit, reentrant |
| 20 | **Macros** | 6 | `define-syntax`, `syntax-rules`, hygiene, ellipsis |
| 21 | **Integration** | 5 | call/cc + macros + mutation + TCO combined |
| 22 | Deep equality | 4 | `equal?` recursive structural comparison |
| 23 | Recursive bindings | 4 | `letrec`, `letrec*`, mutual recursion |
| 24 | Case expression | 4 | `case`, `eqv?`, datum dispatch |
| 25 | Vectors | 5 | `vector`, `vector-ref`, `vector-set!`, conversion |

Levels 1-9 are foundational. Levels 10-12 are **maintenance levels** — cross-cutting refactors on existing code (error quality, I/O, string ops). Levels 13-14 test **requirement changes** — the agent implements mutable strings (R5RS), then must refactor to immutable strings (R7RS). Levels 15-18 add architectural complexity (TCO, mutation, variadic). Levels 19-21 are where most agents struggle — continuations and macros demand non-obvious design decisions. Levels 22-25 are **maintenance extensions** — straightforward feature additions that test whether agents can cleanly extend a complex, mature codebase.

## Tooling

All benchmark infrastructure lives in a single Rust CLI: `cargo xtask`.

```
cargo xtask setup           # install podman, build container image
cargo xtask test 01         # run level 1 tests (containerized)
cargo xtask bench main      # score a branch (all 25 levels)
cargo xtask run-agent ...   # orchestrate an agent run (worktree + agent + scoring)
cargo xtask results         # tabular summary of all runs
cargo xtask tokens --all    # per-level token usage and cost estimates
cargo xtask analyze --all   # request-level cost analysis + anti-pattern detection
cargo xtask watch           # live dashboard of running agents
cargo xtask verify          # ground-truth check against Guile Scheme
```

### Agent Orchestration

> **Note:** Shell scripts under `scripts/` are deprecated launch helpers. `cargo xtask` is the only supported interface for running agents and benchmarks.

`cargo xtask run-agent` handles the full lifecycle:

1. Creates an isolated git worktree
2. Symlinks the selected strategy file as `bench/CLAUDE.md`
3. Pre-builds dependencies (warm cache)
4. Launches the agent (Claude, Codex, or OpenCode)
5. Captures session transcripts
6. Runs the benchmark scorer
7. Records results with metadata (timing, tokens, scores)
8. Commits checkpoints and pushes to origin

Each run is self-contained — unique worktree, UUID, results directory. Multiple runs execute in parallel without interference.

**Worktree path convention:** Worktrees are created at `<repo>/../workspace/<name>`. The agent's working directory is set to `<worktree>/bench/`, so it only sees the playground contents.

**Cleanup:** If a previous run left a stale worktree or branch, use `--clean` to auto-remove them:

```bash
cargo xtask run-agent --strategy default --name claude-r1 --clean --agent claude --mode levels
```

Without `--clean`, you'll get distinct errors for stale directories vs stale branches, with instructions on how to fix each.

### Cost Analysis

`cargo xtask analyze` goes beyond raw token counts. Session JSONL files contain multiple streaming rows per API request; the analyzer deduplicates by request ID and computes:

- **Billed request count** vs raw usage rows
- **Stop reason distribution** (tool_use, end_turn, max_tokens)
- **Tool usage patterns** (which tools, how often, batching efficiency)
- **Output size distribution** (micro-turns vs large generations)
- **Anti-pattern detection** — flags fragmented runs, cache-read cost dominance, single-tool loops, heavy narration
- **Side-by-side comparison** of two runs with percentage diffs

## Results Structure

```
results/
  default_claude-r1_20260319T120000/
    meta.json             # run metadata (strategy, agent, mode, score, timing)
    agent-output.txt      # agent stdout
    session.jsonl         # full session transcript
    bench.log             # scoring output
    L01/                  # per-level data (levels mode)
      agent-output.txt
      session.jsonl
      status.txt          # "Level 01 PASSED (51s)"
      test-result.txt
    L02/
      ...
```

## Quick Start

```bash
# One-time setup
cargo xtask setup

# Run with default strategy (minimal instructions)
cargo xtask run-agent --strategy default --name claude-r1 --agent claude --mode levels

# Run with quality-gate strategy (clippy + code style)
cargo xtask run-agent --strategy quality-gate --name claude-q1 --agent claude --mode levels

# Compare results
cargo xtask results
cargo xtask analyze \
  results/default_claude-r1_* \
  results/quality-gate_claude-q1_*
```

See [BENCH_GUIDE.md](BENCH_GUIDE.md) for the full supervisor reference.

## Ground Truth

Test expected values are verified against Guile Scheme. Run `cargo xtask verify` to re-check.
