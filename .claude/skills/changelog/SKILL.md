---
name: changelog
description: Record a 5W1H changelog entry for the latest framework change into DESIGN_EVOLUTION.md. Run after committing changes to capture who/what/when/where/why/how.
argument-hint: "[commit-hash or 'latest']"
allowed-tools: Read, Edit, Bash, Grep, Glob
---

# 5W1H Changelog Entry

Record the design decision behind commit `$ARGUMENTS` (default: HEAD) into the changelog.

## Step 1: Gather Commit Data

Resolve the target commit. If `$ARGUMENTS` is empty or "latest", use HEAD.

```bash
# Get commit metadata
git log --format='%H %h %ai %an %s' -1 {commit}

# Get files changed
git show --stat --format='' {commit}

# Get full diff (truncate if huge)
git show --format='' {commit} | head -300
```

## Step 2: Read Current Changelog

Read `results/dev-sessions/DESIGN_EVOLUTION.md` to understand the existing format and avoid duplicating entries. The file is ordered latest-first (stack).

## Step 3: Determine the 5W1H

Using the git diff, commit message, AND the current conversation context (what the user asked for and why), determine:

- **Who:** Git author name
- **What:** One-line summary of the concrete change (not the commit message — your own summary of what actually changed)
- **When:** Commit date (YYYY-MM-DD)
- **Where:** Key files/areas affected (group by area: "xtask/", "bench/strategies/", "CLAUDE.md", etc.)
- **Why:** The motivation behind this change. This is the most important field. Extract from:
  1. The current conversation (what did the user ask for? what problem prompted this?)
  2. The commit message
  3. The diff context (what was wrong before?)
- **How:** Brief description of the implementation approach (new file? refactor? config change?)

## Step 4: Generate Entry

Format the entry as:

```markdown
### {What — short descriptive title}
**Date:** {YYYY-MM-DD} | **Commit:** `{short-hash}`
**Who:** {author}
**Where:** {files/areas}
**Why:** {1-3 sentences explaining the motivation}
**How:** {1-2 sentences on implementation approach}
```

## Step 5: Prepend to DESIGN_EVOLUTION.md

Insert the new entry at the TOP of the file, right after the header lines:

```
# MING Benchmark — Design Evolution

A record of key design decisions, what changed, and why. Latest first.

---

{NEW ENTRY HERE}

---

## Phase 5: ...
```

Use the Edit tool to insert the entry. Preserve the existing `---` separator between the header and Phase sections.

## Step 6: Confirm

Print the entry you just added so the user can verify it captures the right "Why".

## Important Notes

- The **Why** field is the primary value of this skill. Git diffs show what changed; this skill captures WHY it changed — the reasoning that would otherwise be lost when the conversation ends.
- If multiple commits should be grouped (e.g., a multi-commit feature), accept a range like `HEAD~3..HEAD` and summarize them as one entry.
- Do NOT duplicate entries — check if the commit hash already appears in DESIGN_EVOLUTION.md before adding.
- Keep entries concise. One entry = one design decision. If a commit has multiple unrelated changes, create multiple entries.
