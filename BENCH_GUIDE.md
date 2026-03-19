# Bench Guide — Supervisor Reference

This file is for **humans supervising agent runs**. It is NOT read by agents.

## Quick Start

```bash
# One-time setup
cargo xtask setup

# Single full run (agent does L1→L16 in one session)
cargo xtask run-agent --name claude-r1

# Level-by-level run (fresh agent per level, fail-fast)
cargo xtask run-agent --name claude-r2 --mode levels

# Use quality-gate strategy instead of default
cargo xtask run-agent --strategy quality-gate --name claude-q1 --mode levels

# Resume a crashed/stopped run (skips passed levels, reuses worktree)
cargo xtask run-agent --strategy default --name claude-r2 --mode levels --resume

# Start from a specific level
cargo xtask run-agent --name claude-r3 --mode levels --from-level 13

# Skip scoring (useful for dry runs)
cargo xtask run-agent --name test-dry --skip-bench
```

## Strategies

Strategy files live in `bench/strategies/`. At launch, `run-agent` symlinks the selected strategy as `bench/CLAUDE.md`.

| Strategy | File | What it does |
|----------|------|-------------|
| `default` | `bench/strategies/default.md` | Minimal instructions: implement the spec, test each level |
| `quality-gate` | `bench/strategies/quality-gate.md` | Adds clippy enforcement, mod.rs size limits, code style rules |

Both share `bench/SPEC.md` (identical task definition). To add a new strategy, create `bench/strategies/<name>.md` and use `--strategy <name>`.

## Agent Examples

### Claude Code (default)

```bash
cargo xtask run-agent --name claude-r1 --agent claude
cargo xtask run-agent --name claude-r1 --agent claude --model claude-opus-4-6
```

Under the hood:
```bash
claude -p --session-id <uuid> --dangerously-skip-permissions "prompt"
```

Session data: `~/.claude/projects/<project>/<uuid>.jsonl`

### Codex (OpenAI)

```bash
cargo xtask run-agent --name codex-r1 --agent codex
```

Under the hood:
```bash
codex --full-auto "prompt"
```

Session data: check `~/.codex/` for logs.

### OpenCode

```bash
cargo xtask run-agent --name opencode-r1 --agent opencode
```

Under the hood: pipes prompt to `opencode` via stdin.

Session data: check `~/.opencode/` for session JSON.

## Execution Modes

### Full Mode (default)

One agent session tackles all levels. Simple, but if the agent gets stuck it may burn budget.

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

### Effort Limits

| Mechanism | Flag | Notes |
|-----------|------|-------|
| `--max-turns N` | All modes | Limits tool call rounds. Defaults: 60 (L1-9), 100 (L10-13), 160 (L14-16) |
| Wall-clock timeout | `timeout 1h cargo xtask run-agent ...` | Hard kill from outside |

### Resume & Recovery

If a run crashes, gets killed, or you want to retry a failed level:

```bash
# Resume — skips all PASSED levels, retries FAILED ones
cargo xtask run-agent --strategy default --name claude-r2 --mode levels --resume

# Start from a specific level (fresh run, skips earlier levels)
cargo xtask run-agent --name claude-r3 --mode levels --from-level 13
```

Resume mode:
- Reuses the existing worktree (no collision error)
- Finds the latest results directory for the strategy+name
- Skips levels with `status.txt` containing `PASSED`
- Retries levels that were `FAILED` or incomplete

### Git Checkpoints

In levels mode, every passing level is automatically committed:
```
checkpoint: L01 passed (51s)
checkpoint: L02 passed (36s)
...
```

This means:
- You can `git log` in the worktree to see progress
- You can `git checkout` any checkpoint to inspect/restore state
- If a level fails, you can reset to the last good checkpoint

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
```

Or compare agents:
```bash
cargo xtask run-agent --name claude-r1 --agent claude &
cargo xtask run-agent --name codex-r1 --agent codex &
wait
```

## Monitoring

```bash
# Live dashboard
cargo xtask watch

# Single snapshot
cargo xtask watch --once

# Filter by timestamp
cargo xtask watch --ts 20260319

# Check progress in worktree
git -C ../workspace/claude-r1 log --oneline -5
git -C ../workspace/claude-r1 status

# Tail agent output
tail -f results/default_claude-r1_*/agent-output.txt
```

## Results

### View all results

```bash
cargo xtask results
cargo xtask results --json    # machine-readable
```

### Token usage & costs

```bash
cargo xtask tokens results/default_claude-r1_*    # single run
cargo xtask tokens --all                           # all runs + comparison
```

### Results directory structure

```
results/
  default_claude-r1_20260319T120000/
    meta.json           # run metadata (strategy, agent, score, timing)
    agent-output.txt    # stdout from agent
    session.jsonl       # full session transcript (Claude only)
    bench.log           # scoring output
  quality-gate_claude-q1_20260319T130000/
    meta.json
    L01/                # levels mode: per-level data
      agent-output.txt
      session.jsonl
      test-result.txt
      status.txt
    L02/
      ...
    bench.log
```

### Inspect meta.json

```bash
jq . results/default_claude-r1_*/meta.json
```

### Parse session JSONL (Claude)

```bash
# Count tool calls
jq 'select(.type == "tool_use")' results/*/session.jsonl | jq -s 'length'

# Extract thinking blocks
jq 'select(.type == "thinking") | .text' results/*/session.jsonl

# Find errors
jq 'select(.type == "tool_result") | select(.is_error == true)' results/*/session.jsonl

# Timeline of tool calls
jq 'select(.type == "tool_use") | {tool: .name, input: .input | keys}' results/*/session.jsonl
```

## Cleanup

```bash
# Remove a specific worktree + branch
git worktree remove ../workspace/claude-r1
git branch -D claude-r1

# Remove all bench worktrees
git worktree list | grep workspace | awk '{print $1}' | xargs -I{} git worktree remove --force {}

# Remove stale worktree refs
git worktree prune

# Clear results (careful!)
rm -rf results/default_claude-r1_*
```

## Custom Prompts

```bash
cargo xtask run-agent --name custom-r1 \
  --prompt "Implement only levels 1-5 of the Scheme interpreter. Follow CLAUDE.md. Run tests after each level."
```

## Comparing Strategies

```bash
cargo xtask run-agent --strategy default --name compare-d1 --mode levels &
cargo xtask run-agent --strategy quality-gate --name compare-q1 --mode levels &
wait
cargo xtask results
cargo xtask analyze results/default_compare-d1_* results/quality-gate_compare-q1_*
```

## All xtask Commands

```bash
cargo xtask --help          # list all commands
cargo xtask setup           # install deps + build container
cargo xtask test 01         # run level tests
cargo xtask bench main      # score a branch
cargo xtask run-agent ...   # orchestrate agent run
cargo xtask results         # list results
cargo xtask tokens --all    # token usage + costs
cargo xtask analyze --all   # request-level cost analysis
cargo xtask watch           # live dashboard
cargo xtask verify          # guile ground-truth checks
```
