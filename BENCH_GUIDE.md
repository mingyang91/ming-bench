# Bench Guide — Supervisor Reference

This file is for **humans supervising agent runs**. It is NOT read by agents.

## Quick Start

```bash
# One-time setup
cargo xtask setup

# Single full run (agent does L1→L27 in one session)
cargo xtask run-agent --name claude-r1

# Level-by-level run (fresh agent per level, fail-fast)
cargo xtask run-agent --name claude-r2 --mode levels

# Use quality-gate strategy
cargo xtask run-agent --strategy quality-gate --name claude-q1 --mode levels

# Run in a different language
cargo xtask run-agent --lang scala --name scala-r1 --mode levels
cargo xtask run-agent --lang go --name go-r1 --mode levels
cargo xtask run-agent --lang java --name java-r1 --mode levels
cargo xtask run-agent --lang ts --name ts-r1 --mode levels

# Resume a crashed/stopped run (skips passed levels, reuses worktree)
cargo xtask run-agent --strategy default --name claude-r2 --mode levels --resume

# Start from a specific level
cargo xtask run-agent --name claude-r3 --mode levels --from-level 13

# Skip scoring (useful for dry runs)
cargo xtask run-agent --name test-dry --skip-bench
```

## Languages

| Language | `--lang` | Build System | Container | Memory | Timeout |
|----------|----------|-------------|-----------|--------|---------|
| Rust | `rust` (default) | Cargo | `ming` | 1 GB | 30s/300s |
| Go | `go` | Go modules | `ming` | 1 GB | 30s/300s |
| Java | `java` | Gradle | `ming-jvm` | 2 GB | 45s/450s |
| TypeScript | `ts` | npm/tsc | `ming-node` | 1 GB | 30s/300s |
| Scala | `scala` | Mill | `ming-jvm` | 2 GB | 45s/450s |

All tests run inside containers with 1 CPU, 256 PIDs. OOM or timeout = test failure.

**Scala specifics:** No shell scripts — xtask calls `./mill` directly. Quality gate uses scalafix (FileTooLong, MethodTooLong, NestingDepth) + scalafmt. Tests run via a standalone `TestRunner` main JAR inside the container.

## Strategies

Strategy files live in `bench/strategies/`. Per-language strategies in `bench/strategies/{lang}/`. At launch, `run-agent` symlinks the selected strategy as `bench/{lang}/CLAUDE.md`.

| Strategy | What it does |
|----------|-------------|
| `default` | Minimal instructions: implement the spec, test each level |
| `quality-gate` | Adds lint enforcement, structural limits, code style rules |

**Available quality-gate strategies:**
- **Rust**: clippy + mod.rs size limits
- **Scala**: scalafix (FileTooLong 300, MethodTooLong 100, NestingDepth 5) + scalafmt. Pure FP: no `var` allowed.

Both share `bench/SPEC.md` (identical task definition) and `bench/tests.json` (shared test manifest). To add a new strategy, create `bench/strategies/{lang}/<name>.md` and use `--strategy <name> --lang <lang>`.

**Two-pass quality gate (levels mode):** The orchestrator splits each level into: (1) coding pass — plain tests, full turn budget, (2) cleanup pass — `--gate` flag, fresh session, 15 turns. The agent never sees lint checks during coding.

## Agent Examples

### Claude Code (default)

```bash
cargo xtask run-agent --name claude-r1 --agent claude
cargo xtask run-agent --name claude-r1 --agent claude --model claude-opus-4-6
```

### Codex (OpenAI)

```bash
cargo xtask run-agent --name codex-r1 --agent codex
```

### OpenCode

```bash
cargo xtask run-agent --name opencode-r1 --agent opencode
```

### Cross-Language Comparison

```bash
cargo xtask run-agent --lang rust --name rust-r1 --mode levels &
cargo xtask run-agent --lang scala --name scala-r1 --mode levels &
cargo xtask run-agent --lang go --name go-r1 --mode levels &
wait
cargo xtask results
```

## Execution Modes

### Full Mode (default)

One agent session tackles all 27 levels. Simple, but if the agent gets stuck it may burn budget.

```bash
cargo xtask run-agent --name claude-r1 --mode full
```

### Levels Mode

Orchestrator runs one agent per level. Pass → next level. Fail → stop.

```bash
cargo xtask run-agent --name claude-r2 --mode levels
```

Benefits:
- Each level gets a fresh context window
- Fail-fast: stops wasting budget when stuck
- Per-level session dumps for granular analysis
- Can compare "which level does agent X struggle on?"

### Turn Limits

| Levels | Tier | Default turns |
|--------|------|--------------|
| L01-L03 | Foundation | 45 |
| L04-L06 | Error/Strings/Mutable | 30 |
| L07-L09 | TCO/set!/Variadic | 45 |
| L10-L12 | call/cc/Macros/Integration | 90 |
| L13-L14 | Builtins/String Immutability | 45 |
| L15 | Equality/Letrec/Case/Vectors | 60 |
| L16-L18 | dynamic-wind/guard/values | 60 |
| L19-L20 | Rationals/Records | 75 |
| L21-L23 | Pair Mutation/syntax-case/Final Integration | 90 |
| L24-L26 | Tech-debt: case-lambda/procedure?/do | 60 |
| L27     | Real-world integration stress        | 90 |

Override with `--max-turns N`. Quality-gate cleanup pass: 15 turns. Regression fix-it pass: 15 turns.

### Regression Checking

After each level passes, the orchestrator re-runs **all previously-passed levels** against the current code. This catches regressions like L14 breaking L06's `string-set!` or L17 breaking L10's `call/cc` exception handler.

If regressions are detected:
1. A **fix-it agent pass** (15 turns) is launched with the failing test output
2. After the fix-it, all levels (including current) are re-checked
3. If regressions persist, the run **halts** with `REGRESSION` status

Checkpoint labels: `REGFIX` (regressions fixed and committed), `REGRESSION` (halted, unfixable).

Estimated overhead: ~4-6 minutes total across a full 27-level run (~4% of wall time).

### Resume & Recovery

If a run crashes, gets killed, or you want to retry a failed level:

```bash
# Resume — skips all PASSED levels, retries FAILED ones
cargo xtask run-agent --strategy default --name claude-r2 --mode levels --resume

# Start from a specific level (fresh run, skips earlier levels)
cargo xtask run-agent --name claude-r3 --mode levels --from-level 13
```

Failed levels auto-retry up to 2 times if the failure was infrastructure (timeout/529/crash), not turns exhaustion.

### Git Checkpoints

In levels mode, every passing level is automatically committed:
```
checkpoint: L01 PASSED (130s)
checkpoint: L02 PASSED (85s)
checkpoint: L03 REGFIX (0s)        # regression detected and fixed
checkpoint: L14 REGRESSION (326s)  # regression detected, fix-it failed — halted
...
```

### Process Safety

- **Lockfile:** Each run creates `.run.lock` in the results dir. Prevents duplicate agents.
- **Signal handling:** `SIGINT`/`SIGTERM` kills the child agent process and commits work.
- **Stale lock detection:** If a lock exists but the PID is dead, it's automatically cleaned up.

## Parallel Runs

Each invocation is self-contained (unique worktree, UUID, results dir). No shared state.

```bash
# Compare strategies
cargo xtask run-agent --strategy default --name claude-d1 --mode levels &
cargo xtask run-agent --strategy quality-gate --name claude-q1 --mode levels &
wait

# Compare languages
cargo xtask run-agent --lang rust --name rust-r1 --mode levels &
cargo xtask run-agent --lang scala --name scala-r1 --mode levels &
cargo xtask run-agent --lang go --name go-r1 --mode levels &
wait
```

## Monitoring

```bash
# Live dashboard
cargo xtask watch

# Single snapshot
cargo xtask watch --once

# Filter by timestamp
cargo xtask watch --ts 20260321

# Check progress in worktree
git -C ../workspace/scala-r1 log --oneline -5

# Tail agent output
tail -f results/default_scala-r1_*/L01/agent-output.txt
```

## Results

### View all results

```bash
cargo xtask results           # shows LANG column
cargo xtask results --json    # machine-readable
```

### Token usage & costs

```bash
cargo xtask tokens results/default_claude-r1_*    # single run
cargo xtask tokens --all                           # all runs + comparison
```

### Session analysis

```bash
cargo xtask session-turns <run>     # per-level turn/time/token analysis
cargo xtask session-stats <run>     # quick overview
cargo xtask session-dump <run>      # dump content (--thinking, --text, --tools)
cargo xtask session-grep <run> <keywords...>  # keyword search
cargo xtask session-tools <run>     # tool call timeline (--summary)
cargo xtask compare <run1> <run2>   # side-by-side comparison
```

### Results directory structure

```
results/
  default_scala-r1_20260321T120000/
    meta.json           # run metadata (strategy, agent, lang, score, timing)
    agent-output.txt    # stdout from agent
    session.jsonl       # full session transcript
    bench.log           # scoring output
    L01/                # levels mode: per-level data
      agent-output.txt
      session.jsonl
      status.txt
    L02/
      ...
```

## Cleanup

```bash
# Remove a specific worktree + branch
git worktree remove ../workspace/scala-r1
git branch -D scala-r1

# Remove all bench worktrees
git worktree list | grep workspace | awk '{print $1}' | xargs -I{} git worktree remove --force {}

# Remove stale worktree refs
git worktree prune

# Clear results (careful!)
rm -rf results/default_scala-r1_*
```

## All xtask Commands

```bash
cargo xtask --help                     # list all commands
cargo xtask setup                      # install deps + build all container images
cargo xtask test 01                    # run level tests (Rust default)
cargo xtask test 01 --lang scala       # run level tests (Scala)
cargo xtask test 01 --lang go --gate   # run with quality gate
cargo xtask bench main                 # score a branch
cargo xtask run-agent ...              # orchestrate agent run
cargo xtask results                    # list results (with LANG column)
cargo xtask tokens --all               # token usage + costs
cargo xtask analyze --all              # request-level cost analysis
cargo xtask watch                      # live dashboard
cargo xtask verify                     # guile ground-truth checks
cargo xtask session-turns <run>        # per-level analysis
cargo xtask compare <run1> <run2>      # side-by-side comparison
```
