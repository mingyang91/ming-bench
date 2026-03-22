# MING — Ming Interpreter Nurture Gauntlet

A benchmark framework for measuring how **prompt engineering strategies** affect coding agent performance. Agents build a Scheme interpreter from scratch — 240+ tests across 27 difficulty levels, from basic arithmetic to first-class continuations, hygienic macros, and exact arithmetic. Supports **5 languages**: Rust, Go, Java, TypeScript, and Scala.

## Why This Exists

Coding agents (Claude Code, Codex, OpenCode, etc.) can write working software, but their effectiveness varies wildly depending on the instructions they receive. This benchmark answers a specific question:

> **Does adding structure to agent instructions — quality gates, code style rules, modular architecture enforcement — improve outcomes compared to minimal "just do it" prompts?**

The task is deliberately chosen to stress-test this: a Scheme interpreter requires the agent to make hundreds of architectural decisions (data representation, evaluation strategy, environment model, continuation implementation) over 27 progressively harder levels. Bad early decisions compound. Good structure should help.

Multi-language support adds another dimension: **does language choice affect agent performance on the same algorithmic task?**

## How It Works

### The Task

The agent receives:
- A function signature (language-specific): `evalStr(input) → result`
- 240+ test cases across 27 levels (each test loads Scheme code from a `.scm` fixture file)
- Instructions in `CLAUDE.md` (the only file the agent reads for guidance)

The agent implements a complete Scheme interpreter from scratch — lexer, parser, environment, evaluator, tail-call optimization, continuations, and hygienic macros. No starter code. No external parsing libraries.

### Supported Languages

| Language | Directory | Build System | Test Execution | Container |
|----------|-----------|-------------|----------------|-----------|
| Rust | `bench/rust/` | Cargo | Pre-compiled binary in `ming` | debian-slim |
| Go | `bench/go/` | Go modules | Pre-compiled binary in `ming` | debian-slim |
| Java | `bench/java/` | Gradle | JUnit 5 via `test.sh` | `ming-jvm` (Corretto 26) |
| TypeScript | `bench/ts/` | npm/tsc | vitest via `test.sh` | `ming-node` |
| Scala | `bench/scala/` | Mill 1.1.2 | Fat JAR in `ming-jvm` | Corretto 26 (2GB/45s) |

### Project Structure

```
ming/                         # framework — orchestration & analysis
  xtask/                      # CLI: run-agent, test, bench, tokens, analyze, watch
  Dockerfile.bench            # container image for Rust/Go (static binaries)
  Dockerfile.jvm              # container image for Java/Scala (amazoncorretto:26)
  Dockerfile.node             # container image for TypeScript (node:latest)
  CLAUDE.md                   # framework maintainer instructions

  bench/                      # agent playground
    SPEC.md                   # interpreter specification (shared, all languages)
    tests.json                # shared test manifest (214 test cases)
    fixtures/                 # shared .scm fixture files (214 files)
    strategies/               # strategy files (per-language subdirectories)
      default.md              # Rust default (legacy)
      quality-gate.md         # Rust quality-gate (legacy)
      rust/default.md         # Rust default strategy
      go/default.md           # Go default strategy
      java/default.md         # Java default strategy
      ts/default.md           # TypeScript default strategy
      scala/default.md        # Scala default strategy
      scala/quality-gate.md   # Scala quality-gate (pure FP, scalafix)
    rust/                     # Rust interpreter crate
    go/                       # Go interpreter scaffold
    java/                     # Java interpreter scaffold (Gradle)
    ts/                       # TypeScript interpreter scaffold (vitest)
    scala/                    # Scala interpreter scaffold (Mill + scalafix)
```

Agents are launched with `cwd = bench/{lang}/` and only interact with files there. Framework code (xtask, Dockerfiles) lives above the agent's working directory.

### The Experiment: Strategies

| Strategy | What it tests |
|----------|---------------|
| `default` | Baseline — minimal instructions: implement the spec, test each level, fix failures |
| `quality-gate` | Whether structural enforcement improves agent code quality and completion rate |

Both strategies share `SPEC.md` (identical task definition) and the same test suite. The only difference is the instructions in `CLAUDE.md` and whether lint enforcement is active.

Per-language strategies live in `bench/strategies/{lang}/`. The orchestrator picks the language-specific strategy first, falling back to the base strategy.

**Quality gate enforcement by language:**
- **Rust**: clippy + mod.rs size check (via `--gate` flag)
- **Scala**: scalafix (FileTooLong, MethodTooLong, NestingDepth) + scalafmt (via `--gate` flag)
- **Others**: `--gate` flag passed to language's `test.sh`

### Execution Modes

Each agent run uses one of two modes:

- **Full mode** — one agent session tackles all 27 levels. Simpler, but if the agent gets stuck it burns budget.
- **Levels mode** — orchestrator runs a fresh agent per level. After each level passes, all prior levels are re-run to catch regressions. If regressions are detected, a 15-turn fix-it pass is launched; if unfixable, the run halts. Per-level session data for granular analysis.

### Sandboxed Testing

Tests never run on the host. Every test execution happens inside a container with hard resource limits:

| Language | Memory | Timeout (per-level) | Timeout (all) | Container |
|----------|--------|-------------------|---------------|-----------|
| Rust / Go | 1 GB | 30s | 300s | `ming` |
| Java / Scala | 2 GB | 45s | 450s | `ming-jvm` |
| TypeScript | 1 GB | 30s | 300s | `ming-node` |

All containers: 1 CPU, 256 PIDs. OOM or timeout = test failure. This prevents agent-written infinite loops or memory bombs from crashing the benchmark host.

## Test Levels

| Level | Topic | Tests | Key Concepts |
|-------|-------|-------|-------------|
| 1 | Atoms, arithmetic, comparisons | 19 | Integers, booleans, strings, `+`/`-`/`*`/`/`, `<`/`>`/`=`, `and`/`or`/`not` |
| 2 | Variables, conditionals, lambda | 14 | `define`, `if`, `quote`, `lambda`, closures, recursion |
| 3 | Lists, recursion, let/begin/cond | 24 | `cons`/`car`/`cdr`, map/filter, `let`/`begin`/`cond`, type predicates |
| 4 | Error quality | 6 | Error messages with source position (line:col) |
| 5 | Display/write & string ops | 13 | `display`, `write`, `newline`, `string-append`, `substring` |
| 6 | Mutable strings (R5RS) | 3 | `string-set!`, `string-copy` — **seed: agent assumes mutability** |
| 7 | **Tail call optimization** | 8 | TCO in `if`, `cond`, named `let`, `and`/`or`, `begin` |
| 8 | set! & mutation | 5 | Mutable bindings, shared state in closures |
| 9 | Variadic & apply | 6 | Rest args, `apply` with prefix args |
| 10 | **call/cc** | 10 | First-class continuations, non-local exit, reentrant |
| 11 | **Macros** | 6 | `define-syntax`, `syntax-rules`, hygiene, ellipsis |
| 12 | **Integration** | 5 | call/cc + macros + mutation + TCO combined |
| 13 | Numeric/char/string utilities | 25 | `abs`, `modulo`, `min`/`max`, `char-upcase`, `string=?` |
| 14 | String immutability (R7RS) | 4 | **Requirement change** — `string-set!` now errors (8 levels after L06) |
| 15 | Equality, letrec, case, vectors | 17 | `equal?`, `letrec`/`letrec*`, `case`, `vector` |
| 16 | dynamic-wind | 6 | Resource cleanup on non-local exit |
| 17 | guard & raise | 6 | Exception signaling and catching |
| 18 | values & call-with-values | 6 | Multi-value returns |
| 19 | **Exact arithmetic** | 8 | Rationals, cross-tower comparison |
| 20 | define-record-type | 5 | R7RS records with disjoint types |
| 21 | **Pair mutation** | 5 | `set-car!`/`set-cdr!`, circular list detection |
| 22 | **syntax-case** | 5 | Advanced macro system with guards |
| 23 | **Final integration** | 8 | All features combined |
| 24 | **case-lambda** | 5 | Multi-arity closures — **forces closure restructure** |
| 25 | **procedure?** | 5 | Must return `#t` for all callable types (lambda, case-lambda, builtins, continuations) |
| 26 | **do loops** | 5 | Iteration with parallel step — **tests env model** |
| 27 | **Real-world integration** | 1 | 1000+ line macro expander — **stress-tests all features at scale** |

**Level design philosophy:** Levels are ordered to maximize tech-debt exposure. L06 plants mutable strings, then L14 (8 levels later) reverses the requirement. L24-L26 force restructuring of core infrastructure (closures, procedure naming, callable type unification, eval loop) established 10-20 levels earlier. L27 hits the agent with real-world programs (1000+ lines) that exercise all features simultaneously — maximum distance from when features were first implemented.

## Tooling

All benchmark infrastructure lives in a single Rust CLI: `cargo xtask`.

```bash
cargo xtask setup                          # install podman, build all container images
cargo xtask test 01                        # test level 1 (Rust, default)
cargo xtask test 01 --lang scala           # test level 1 (Scala)
cargo xtask test 01 --lang scala --gate    # test with quality gate
cargo xtask run-agent --lang scala --strategy default --name scala-r1 --mode levels
cargo xtask results                        # tabular summary of all runs
cargo xtask tokens --all                   # per-level token usage and cost estimates
cargo xtask analyze --all                  # request-level cost analysis
cargo xtask session-stats results/<run>    # session summary from a run directory
cargo xtask session-tools results/<run> --summary
cargo xtask session-turns results/<run>
cargo xtask session-dump results/<run> --text
cargo xtask watch                          # live dashboard of running agents
cargo xtask verify                         # ground-truth check against Guile
```

### Agent Orchestration

`cargo xtask run-agent` handles the full lifecycle:

1. Creates an isolated git worktree
2. Symlinks the selected strategy file as `bench/{lang}/CLAUDE.md`
3. Pre-builds dependencies (warm cache)
4. Launches the agent (Claude, Codex, or OpenCode)
5. Captures the exact session transcript into `results/.../session.jsonl`
6. Runs the benchmark scorer
7. Records results with metadata (timing, tokens, scores)
8. Commits checkpoints and pushes to origin

Each run is self-contained — unique worktree, UUID, results directory. Multiple runs execute in parallel without interference.

**Worktree path convention:** Worktrees are created at `<repo>/../workspace/<name>`. The agent's working directory is set to `<worktree>/bench/{lang}/`, so it only sees its language's playground contents.

### Cost Analysis

`cargo xtask analyze` goes beyond raw token counts. It works directly on `results/<run>` for both Claude and Codex runs.

- **Claude**: session JSONL contains multiple streaming rows per API request, so the analyzer deduplicates by request ID.
- **Codex**: the rollout JSONL is analyzed as request-equivalent turns, using distinct `token_count` snapshots and per-snapshot token deltas.

The analyzer computes:

- **Billed request count** vs raw usage rows
- **Stop reason distribution** (tool_use, end_turn, max_tokens)
- **Tool usage patterns** (which tools, how often, batching efficiency)
- **Output size distribution** (micro-turns vs large generations)
- **Anti-pattern detection** — flags fragmented runs, cache-read cost dominance, single-tool loops, heavy narration
- **Side-by-side comparison** of two runs with percentage diffs

The session inspection commands (`session-dump`, `session-stats`, `session-tools`, `session-turns`, `compare`, `analyze`) all accept a run directory directly:

```bash
cargo xtask session-stats results/default_cx-def-lvl12_20260321T091713
cargo xtask session-tools results/default_cx-def-lvl12_20260321T091713 --summary
cargo xtask session-turns results/default_cx-def-lvl12_20260321T091713
cargo xtask analyze results/default_cx-def-lvl12_20260321T091713
```

## Results Structure

```
results/
  default_scala-r1_20260321T120000/
    meta.json             # run metadata (strategy, agent, lang, mode, score, timing)
    agent-output.txt      # agent stdout
    session.jsonl         # canonical full session transcript (Claude or Codex)
    bench.log             # scoring output
    L01/                  # per-level data (levels mode)
      agent-output.txt
      session.jsonl       # exact per-level transcript when captured
      status.txt          # "Level 01 PASSED (130s)"
    L02/
      ...
```

New Codex runs are self-contained: `run-agent` copies the exact rollout JSONL into each results directory as `session.jsonl`.

Older Codex runs may only have `agent-output.txt`. The session commands still work on those runs by extracting the real Codex session id from `agent-output.txt` and resolving the matching rollout under `~/.codex/sessions/...`.

## Quick Start

```bash
# One-time setup
cargo xtask setup

# Run Rust with default strategy
cargo xtask run-agent --strategy default --name claude-r1 --agent claude --mode levels

# Run Scala with quality-gate strategy
cargo xtask run-agent --lang scala --strategy quality-gate --name scala-q1 --agent claude --mode levels

# Run Go with default strategy
cargo xtask run-agent --lang go --strategy default --name go-r1 --agent claude --mode levels

# Compare results
cargo xtask results
cargo xtask compare results/default_claude-r1_* results/default_scala-q1_*
```

See [BENCH_GUIDE.md](BENCH_GUIDE.md) for the full supervisor reference.

## Ground Truth

Test expected values are verified against Guile Scheme. Run `cargo xtask verify` to re-check. The shared test manifest (`bench/tests.json`) and fixture files (`bench/fixtures/`) are the canonical source — all language test harnesses read from them.
