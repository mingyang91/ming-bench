---
name: compare
description: Compare two benchmark runs side-by-side with narrative analysis of struggles, cost, and gate friction.
argument-hint: <run1> <run2>
allowed-tools: Read, Edit, Grep, Glob, Bash
---

# Run Comparison Analysis

Compare runs `$ARGUMENTS` side-by-side. Produce a data-driven narrative explaining where each agent struggled and why.

## Step 1: Get the Data

```bash
cargo xtask compare <run1> <run2>
```

Capture the full table. Note which run is A and which is B.

## Step 2: Identify Struggle Levels

A level is a "struggle" if ANY of these are true:
- **Turns > 75% of limit** (e.g., >30/40 for L01-L09, >45/60 for L10+)
- **Time > 2× the median level time** for that run
- **Test runs ≥ 3** (multiple retry cycles)
- **Friction ≥ 3** (heavy gate fighting)

List struggle levels for each run.

## Step 3: Read Thinking for Struggle Levels

For each struggle level, read the agent's reasoning:

```bash
cargo xtask session-dump <run> --thinking --level <NN>
```

Look for:
- What the agent got stuck on (conceptual misunderstanding? env corruption? test failure loop?)
- Whether it was a logic bug vs. gate friction (clippy/nesting)
- How many iterations it took to resolve

## Step 4: Check Gate Friction (if strategies differ)

For levels where friction differs between runs:

```bash
cargo xtask session-grep <run> --level <NN> nesting clippy refactor
```

Determine whether friction was productive (forced better code) or pure overhead.

## Step 5: Produce Narrative

Structure your analysis as:

### Overview
- Which run progressed further? Which was faster/cheaper?
- Did both reach the same final level?

### Per-Level Struggle Analysis
For each struggle level, one paragraph: what happened, why, and how it resolved.

### Gate Cost (if applicable)
- Total friction events: A vs B
- Which levels had the most gate fighting?
- Did the gate improve code quality or just add overhead?

### Verdict
- Was the cost/time difference justified by code quality?
- Which levels would benefit from tighter/looser turn limits?
- Any turn limit violations (turns = limit)?

## Step 6: Save to Run Log

After producing the narrative, append a comparison summary to `results/dev-sessions/RUN_LOG.md`.

Use the Edit tool to insert the entry at the TOP of the file, right after the header (before the first `---` separator that precedes a `## R` section).

Format the entry as:

```markdown
### Compare: {run1-short} vs {run2-short} — {YYYY-MM-DD}
**Winner:** {which run and why, one line}
**Levels:** {run1-short} {passed1}/{total1} vs {run2-short} {passed2}/{total2}
**Cost:** {run1-short} ${cost1} vs {run2-short} ${cost2}
**Key insight:** {one-sentence takeaway from the struggle analysis}
**Verdict:** {2-3 sentence summary of the narrative verdict}
```

If the runs belong to an existing round section (e.g., both are R25 runs), insert the comparison entry inside that section instead of at the top. Check the run names to determine the round number.

**Important:** Always save the comparison. The whole point is that analysis shouldn't be lost when the conversation ends.
