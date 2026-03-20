---
name: compliance
description: Analyze how well an agent followed its CLAUDE.md strategy rules during a benchmark run. Use when analyzing agent behavior, rule compliance, or debugging why an agent failed.
argument-hint: <run-name>
allowed-tools: Read, Grep, Glob, Bash
---

# CLAUDE.md Compliance Analysis

Analyze how well the agent in run `$ARGUMENTS` followed the rules in its strategy file.

## Step 1: Identify the Run

Resolve `$ARGUMENTS` to a results directory under `results/`. Read `meta.json` to get:
- **strategy** name (or infer: `quality-gate_*`/`strategy_*` → quality-gate, `default_*`/`main_*` → default)
- **name** (used to find the worktree at `../workspace/<name>/bench/`)
- **mode** (full or levels)

Run: `cargo xtask session-stats $ARGUMENTS`

This gives you the overview: event count, tool usage, bash commands, levels attempted.

## Step 2: Read the Strategy

Read `bench/strategies/<strategy>.md` and extract every discrete, testable rule. For each rule note:
- What it says (the requirement)
- Whether it has a **mechanical gate** (enforced by `cargo xtask test` — e.g., clippy, mod.rs size check)
- What **keywords** would appear in the session if the agent engaged with this rule

### Known rules by strategy

**default** (all are prose-only, no gates):
- Never run `cargo test` directly → keywords: `cargo test`, `xtask test`
- Use `cargo xtask test` → keywords: `xtask test`
- Don't modify test functions → keywords: `test`, `modify`
- Implement levels in order → keywords: `level`, `L01`, `L02`...
- Only allowed crates → keywords: `Cargo.toml`, `crate`, `thiserror`

**quality-gate** (inherits all default rules, adds):
- Clippy enforcement (HAS GATE) → keywords: `clippy`, `lint`, `warning`
- Mod.rs size limit (HAS GATE) → keywords: `mod.rs`, `thin`, `entry point`, `re-export`
- Structured thiserror variants (no String wrapping) → keywords: `thiserror`, `Other(String)`, `variant`, `structured`
- Immutable-first patterns → keywords: `mut`, `immutable`, `RefCell`
- Fix violations in touched files → keywords: `fix`, `clippy`, `violation`

## Step 3: Check Awareness

Run keyword searches for each rule's keywords:

```bash
cargo xtask session-grep $ARGUMENTS clippy "mod.rs" immutable thiserror "cargo test" "xtask test" "Other(String)"
```

For each keyword, note:
- **thinking hits** = agent reasoned about this rule
- **text hits** = agent mentioned it in visible output
- **tool_use hits** = agent ran commands related to this rule
- **span** = how much of the session the keyword appears across (low span + late absence = drift)

## Step 4: Check Timeline

```bash
cargo xtask session-tools $ARGUMENTS --summary
```

Key things to look for:
- Did the agent read CLAUDE.md? (Look for Read of CLAUDE.md or strategy file)
- Did it run `cargo test` directly? (VIOLATION)
- Did it run `cargo clippy`? (quality-gate awareness)
- What order did it test levels? (should be sequential)
- How many test runs per level? (retry patterns)

For deeper investigation:
```bash
cargo xtask session-tools $ARGUMENTS
```

## Step 5: Read Key Thinking Moments

```bash
cargo xtask session-dump $ARGUMENTS --thinking
```

Focus on:
- **First thinking block** — does it mention reading CLAUDE.md?
- **Pre-implementation thinking** — does it plan architecture with rules in mind?
- **After first test failure** — does it reference quality gates?
- **Late session thinking** — has it forgotten about rules?

## Step 6: Check Produced Code

If the worktree exists at `../workspace/<name>/bench/src/scheme/`:

1. **thiserror structured variants**: `grep -r "Other(String)" ../workspace/<name>/bench/src/scheme/`
2. **Immutable-first**: `grep -c "let mut" ../workspace/<name>/bench/src/scheme/*.rs`
3. **Mod.rs thin**: `wc -l ../workspace/<name>/bench/src/scheme/mod.rs`
4. **No extra crates**: Check `../workspace/<name>/bench/Cargo.toml` dependencies
5. **Test files unchanged**: `git -C ../workspace/<name> diff main -- bench/src/scheme/tests/`

If the worktree was cleaned up, note "worktree not found — code checks skipped" and rely on session analysis only.

## Step 7: Synthesize Verdicts

For each rule, classify using this decision tree:

1. **Enforced & Followed** — has a mechanical gate, code is compliant, agent showed awareness
2. **Path-Locked** — has a gate, code is compliant, but agent showed LOW awareness (followed because forced)
3. **Read & Followed** — NO gate, agent showed awareness (keywords in thinking), code is compliant
4. **Never Engaged** — zero keyword hits in thinking blocks, may or may not be compliant (accidental compliance or violation)
5. **Engaged Then Drifted** — keywords appear in early thinking blocks but not in later ones, AND violations exist in code
6. **Violated** — code or session shows clear rule violation regardless of awareness

## Step 8: Output Report

Format as a table followed by narrative analysis:

```
══════════════════════════════════════════════════════════
  Compliance: <run-name> (strategy: <strategy>)
══════════════════════════════════════════════════════════

  Rule                        Gate  Code     Aware    Verdict
  ─────────────────────────── ────  ───────  ───────  ──────────────────
  Never run cargo test         No   --       T:3 X:0  Read & Followed
  ...

  Summary: N followed, N enforced, N drifted, N never engaged, N violated
══════════════════════════════════════════════════════════
```

Then provide **narrative analysis** explaining:
- Which rules the agent internalized vs. ignored
- Whether prose-only rules had any effect (compare default vs. quality-gate)
- Root cause for any violations (path-locking? drift? never read CLAUDE.md?)
- Actionable insights: which rules need mechanical enforcement to be reliable?

## Important Notes

- The **correlation between "has a gate" and "was followed"** is the key output. If only gated rules are followed, prose instructions have no effect.
- **Thinking blocks are the primary evidence** for awareness. Text blocks can be performative ("I'll follow the rules") without actual compliance.
- **Tool calls are behavioral evidence** — what the agent actually did matters more than what it said.
- For levels-mode runs, you can filter by level: `cargo xtask session-grep $ARGUMENTS --level 05 clippy`
