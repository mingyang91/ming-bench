# Bench Guide — Supervisor Reference

This file is for **humans supervising agent runs**. It is NOT read by agents.

## Quick Start

```bash
# One-time setup
./scripts/setup.sh

# Single full run (agent does L1→L16 in one session)
./scripts/run-agent.sh --base main --name main-r1

# Level-by-level run (fresh agent per level, fail-fast)
./scripts/run-agent.sh --base main --name main-r2 --mode levels

# Resume a crashed/stopped run (skips passed levels, reuses worktree)
./scripts/run-agent.sh --base main --name main-r2 --mode levels --resume

# Start from a specific level
./scripts/run-agent.sh --base main --name main-r3 --mode levels --from-level 13

# Skip scoring (useful for dry runs)
./scripts/run-agent.sh --base main --name test-dry --skip-bench

# Keep worktree for inspection
./scripts/run-agent.sh --base main --name main-debug --keep-worktree
```

## Agent Examples

### Claude Code (default)

```bash
./scripts/run-agent.sh --base main --name main-r1 --agent claude
./scripts/run-agent.sh --base main --name main-r1 --agent claude --model claude-opus-4-6
```

Under the hood:
```bash
claude -p --session-id <uuid> --dangerously-skip-permissions "prompt"
```

Session data: `~/.claude/projects/<project>/<uuid>.jsonl`

### Codex (OpenAI)

```bash
./scripts/run-agent.sh --base main --name main-r2 --agent codex
```

Under the hood:
```bash
codex --full-auto "prompt"
```

Session data: check `~/.codex/` for logs.

### OpenCode

```bash
./scripts/run-agent.sh --base main --name main-r3 --agent opencode
```

Under the hood: pipes prompt to `opencode` via stdin.

Session data: check `~/.opencode/` for session JSON.

## Execution Modes

### Full Mode (default)

One agent session tackles all levels. Simple, but if the agent gets stuck it may burn budget.

```bash
./scripts/run-agent.sh --base main --name main-r1 --mode full
```

### Levels Mode

Shell loop runs one agent per level. Pass → next level. Fail → stop.

```bash
./scripts/run-agent.sh --base main --name main-r2 --mode levels
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
| Wall-clock timeout | `timeout 1h ./scripts/run-agent.sh ...` | Hard kill from outside |

### Resume & Recovery

If a run crashes, gets killed, or you want to retry a failed level:

```bash
# Resume — skips all PASSED levels, retries FAILED ones
./scripts/run-agent.sh --base main --name main-r2 --mode levels --resume

# Start from a specific level (fresh run, skips earlier levels)
./scripts/run-agent.sh --base main --name main-r3 --mode levels --from-level 13
```

Resume mode:
- Reuses the existing worktree (no collision error)
- Finds the latest results directory for the base+name
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
- **Signal handling:** `SIGINT`/`SIGTERM` on the outer script kills the child agent process.
- **Stale lock detection:** If a lock exists but the PID is dead, it's automatically cleaned up.

## Parallel Runs

Each invocation is self-contained (unique worktree, UUID, results dir). No shared state.

```bash
./scripts/run-agent.sh --base main --name main-r1 &
./scripts/run-agent.sh --base strategy --name strat-r1 &
wait
```

Or compare agents:
```bash
./scripts/run-agent.sh --base main --name claude-r1 --agent claude &
./scripts/run-agent.sh --base main --name codex-r1 --agent codex &
wait
```

## Monitoring

```bash
# Check if agents are alive
ps aux | grep claude
ps aux | grep codex

# Check progress in worktree
git -C ../workspace/main-r1 log --oneline -5
git -C ../workspace/main-r1 status

# Tail agent output
tail -f results/main_main-r1_*/agent-output.txt
```

## Results

### View all results

```bash
./scripts/list-results.sh
```

Output:
```
RUN                                      BASE       AGENT    SCORE      DURATION MODE
---                                      ----       -----    -----      -------- ----
main_main-r1_20260318T120000             main       claude   75/92      14m32s   full
strategy_strat-r1_20260318T120500        strategy   claude   80/92      18m05s   full
main_main-r2_20260318T130000             main       claude   62/92      22m10s   levels
```

### Results directory structure

```
results/
  main_main-r1_20260318T120000/
    meta.json           # run metadata (base, agent, score, timing)
    agent-output.txt    # stdout from agent
    session.jsonl       # full session transcript (Claude only)
    bench.log           # bench.sh scoring output
  main_main-r2_20260318T130000/
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
jq . results/main_main-r1_*/meta.json
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
git worktree remove ../workspace/main-r1
git branch -D main-r1

# Remove all bench worktrees
git worktree list | grep workspace | awk '{print $1}' | xargs -I{} git worktree remove --force {}

# Remove stale worktree refs
git worktree prune

# Clear results (careful!)
rm -rf results/main_main-r1_*
```

## Custom Prompts

```bash
./scripts/run-agent.sh --base main --name main-custom \
  --prompt "Implement only levels 1-5 of the Scheme interpreter. Follow CLAUDE.md. Run tests after each level."
```

## Comparing Branches

Run the same agent against different base branches to compare strategies:

```bash
./scripts/run-agent.sh --base main --name main-compare &
./scripts/run-agent.sh --base strategy --name strat-compare &
wait
./scripts/list-results.sh
```
