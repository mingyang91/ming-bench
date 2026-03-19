# CS 61A Scheme Interpreter Benchmark

A benchmark framework for measuring how **prompt engineering strategies** affect coding agent performance. Agents build a Scheme interpreter in Rust from scratch — 97 tests across 16 difficulty levels, from basic arithmetic to first-class continuations and hygienic macros.

## Why This Exists

Coding agents (Claude Code, Codex, OpenCode, etc.) can write working software, but their effectiveness varies wildly depending on the instructions they receive. This benchmark answers a specific question:

> **Does adding structure to agent instructions — quality gates, code style rules, modular architecture enforcement — improve outcomes compared to minimal "just do it" prompts?**

The task is deliberately chosen to stress-test this: a Scheme interpreter requires the agent to make hundreds of architectural decisions (data representation, evaluation strategy, environment model, continuation implementation) over 16 progressively harder levels. Bad early decisions compound. Good structure should help.

## How It Works

### The Task

The agent receives:
- A function signature: `eval_str(input: &str) -> Result<String, String>`
- 97 test cases across 16 levels
- Instructions in `CLAUDE.md` (the only file the agent reads for guidance)

The agent implements a complete Scheme interpreter from scratch — lexer, parser, environment, evaluator, tail-call optimization, continuations, and hygienic macros. No starter code. No libraries beyond `thiserror` for error types.

### The Experiment: Two Branches

| Branch | CLAUDE.md | What it tests |
|--------|-----------|---------------|
| `main` | Minimal instructions: implement levels in order, run tests, fix failures | Baseline — how agents perform with standard guidance |
| `strategy` | Adds: quality gate (clippy + size limits), code style rules (typed errors, immutable-first, thin mod.rs) | Whether structural enforcement improves agent code quality and completion rate |

Both branches share identical test suites and infrastructure. The only difference is the instructions the agent sees.

### Execution Modes

Each agent run uses one of two modes:

- **Full mode** — one agent session tackles all 16 levels. Simpler, but if the agent gets stuck it burns budget.
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
| 10 | Tail call opt. | 3 | Deep recursion without stack overflow |
| 11 | set! & mutation | 5 | Mutable bindings, shared state in closures |
| 12 | Variadic & apply | 5 | Rest args, `apply` with prefix args |
| 13 | Tail position (all) | 5 | TCO in `cond`, named `let`, `and`/`or`, `begin` |
| 14 | **call/cc** | 7 | First-class continuations, non-local exit, reentrant |
| 15 | **Macros** | 5 | `define-syntax`, `syntax-rules`, hygiene, ellipsis |
| 16 | **Integration** | 5 | call/cc + macros + mutation + TCO combined |

Levels 1-9 are foundational. Level 10 requires a fundamental architectural change (trampoline or CPS). Levels 14-16 are where most agents struggle — continuations and macros demand non-obvious design decisions.

## Tooling

All benchmark infrastructure lives in a single Rust CLI: `cargo xtask`.

```
cargo xtask setup           # install podman, build container image
cargo xtask test 01         # run level 1 tests (containerized)
cargo xtask bench main      # score a branch (all 16 levels)
cargo xtask run-agent ...   # orchestrate an agent run (worktree + agent + scoring)
cargo xtask results         # tabular summary of all runs
cargo xtask tokens --all    # per-level token usage and cost estimates
cargo xtask analyze --all   # request-level cost analysis + anti-pattern detection
cargo xtask watch           # live dashboard of running agents
cargo xtask verify          # ground-truth check against Guile Scheme
```

### Agent Orchestration

`cargo xtask run-agent` handles the full lifecycle:

1. Creates an isolated git worktree from the target branch
2. Pre-builds dependencies (warm cache)
3. Launches the agent (Claude, Codex, or OpenCode)
4. Captures session transcripts
5. Runs the benchmark scorer
6. Records results with metadata (timing, tokens, scores)
7. Commits checkpoints and pushes to origin

Each run is self-contained — unique worktree, UUID, results directory. Multiple runs execute in parallel without interference.

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
  main_claude-r1_20260318T120000/
    meta.json             # run metadata (base, agent, mode, score, timing)
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

# Run an agent against the main branch
cargo xtask run-agent --base main --name claude-r1 --agent claude --mode levels

# Run the same agent against the strategy branch
cargo xtask run-agent --base strategy --name claude-s1 --agent claude --mode levels

# Compare results
cargo xtask results
cargo xtask analyze \
  results/main_claude-r1_* \
  results/strategy_claude-s1_*
```

See [BENCH_GUIDE.md](BENCH_GUIDE.md) for the full supervisor reference.

## Ground Truth

Test expected values are verified against Guile Scheme. Run `cargo xtask verify` to re-check.
