#!/usr/bin/env bash
set -euo pipefail

# --- Ensure agent CLIs are in PATH ---
# Source user's shell profile for nvm/npm/cargo paths if running non-interactively
if [[ -z "${NVM_DIR:-}" ]] && [[ -f "$HOME/.nvm/nvm.sh" ]]; then
    export NVM_DIR="$HOME/.nvm"
    # shellcheck disable=SC1091
    source "$NVM_DIR/nvm.sh" 2>/dev/null || true
fi
# Add common bin paths
for p in "$HOME/.local/bin" "$HOME/.npm-global/bin" "$HOME/.cargo/bin"; do
    [[ -d "$p" ]] && [[ ":$PATH:" != *":$p:"* ]] && export PATH="$p:$PATH"
done

# ============================================================================
# Agent Orchestrator: worktree → agent → session capture → scoring
#
# Usage: ./scripts/run-agent.sh --base <branch> --name <run-name> [options]
#   --base <branch>       Base branch to fork from (e.g. main, strategy)
#   --name <run-name>     Unique run name (becomes branch + worktree name)
#   --prompt <text>       Agent prompt (default: built-in)
#   --model <model>       Model override (e.g. claude-opus-4-6)
#   --agent <agent>       Agent to use: claude (default), codex, opencode
#   --mode <mode>         Execution mode: full (default) or levels
#   --max-turns <n>       Max turns per agent session (default: varies by mode)
#   --skip-bench          Skip scoring after agent finishes
#   --keep-worktree       Don't remove worktree after run
#   --resume              Resume a previous run (skip passed levels, reuse worktree)
#   --from-level <NN>     Start from a specific level (levels mode only)
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# --- Defaults ---
BASE=""
NAME=""
PROMPT=""
MODEL=""
AGENT="claude"
MODE="full"
MAX_TURNS=""
SKIP_BENCH=false
KEEP_WORKTREE=false
RESUME=false
FROM_LEVEL=""

# --- Parse args ---
while [[ $# -gt 0 ]]; do
    case "$1" in
        --base)       BASE="$2"; shift 2 ;;
        --name)       NAME="$2"; shift 2 ;;
        --prompt)     PROMPT="$2"; shift 2 ;;
        --model)      MODEL="$2"; shift 2 ;;
        --agent)      AGENT="$2"; shift 2 ;;
        --mode)       MODE="$2"; MODE_EXPLICIT=1; shift 2 ;;
        --max-turns)  MAX_TURNS="$2"; shift 2 ;;
        --skip-bench) SKIP_BENCH=true; shift ;;
        --keep-worktree) KEEP_WORKTREE=true; shift ;;
        --resume)     RESUME=true; shift ;;
        --from-level) FROM_LEVEL="$2"; shift 2 ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

# Default non-Claude agents to levels mode (they don't self-regulate sequencing well)
if [[ "$AGENT" != "claude" && "$MODE" == "full" && -z "${MODE_EXPLICIT:-}" ]]; then
    echo "NOTE: Defaulting to --mode levels for $AGENT (override with explicit --mode full)"
    MODE="levels"
fi

if [[ -z "$BASE" || -z "$NAME" ]]; then
    echo "Usage: $0 --base <branch> --name <run-name> [options]"
    echo "  --base <branch>       Base branch to fork from"
    echo "  --name <run-name>     Unique run name"
    echo "  --prompt <text>       Agent prompt (default: built-in)"
    echo "  --model <model>       Model override"
    echo "  --agent <agent>       Agent: claude (default), codex, opencode"
    echo "  --mode <mode>         Mode: full (default) or levels"
    echo "  --max-turns <n>       Max turns per session"
    echo "  --skip-bench          Skip scoring"
    echo "  --keep-worktree       Keep worktree after run"
    echo "  --resume              Resume a previous run (skip passed levels)"
    echo "  --from-level <NN>     Start from a specific level (e.g. 13)"
    exit 1
fi

# --- Default prompt ---
DEFAULT_PROMPT="Implement the Scheme interpreter by following CLAUDE.md exactly.
Work through levels 1 through 16 in order.
After implementing each level, run ./scripts/test-level.sh NN to verify.
Fix failures before proceeding. Do not skip levels."

if [[ -z "$PROMPT" ]]; then
    PROMPT="$DEFAULT_PROMPT"
fi

# --- Generate IDs ---
SESSION_UUID="$(uuidgen)"
TIMESTAMP="$(date +%Y%m%dT%H%M%S)"
START_TIME="$(date -Iseconds)"

# --- Worktree path ---
WORKTREE_DIR="$(dirname "$PROJECT_DIR")/workspace/${NAME}"

if [[ "$RESUME" == "true" ]]; then
    # --- Resume mode: reuse existing worktree + find existing results dir ---
    if [[ ! -d "$WORKTREE_DIR" ]]; then
        echo "ERROR: --resume specified but worktree not found: $WORKTREE_DIR"
        echo "Run without --resume to start fresh."
        exit 1
    fi

    # Find the most recent results dir for this base+name
    RESULTS_DIR=$(find "$PROJECT_DIR/results" -maxdepth 1 -type d -name "${BASE}_${NAME}_*" | sort | tail -1)
    if [[ -z "$RESULTS_DIR" ]]; then
        echo "ERROR: --resume specified but no results directory found for ${BASE}_${NAME}_*"
        exit 1
    fi

    echo "Resuming run: $(basename "$RESULTS_DIR")"
    echo "Worktree:     $WORKTREE_DIR"
else
    # --- Fresh run: check for collisions ---
    if [[ -d "$WORKTREE_DIR" ]]; then
        echo "ERROR: Worktree directory already exists: $WORKTREE_DIR"
        echo "Pick a different --name, remove the existing worktree, or use --resume."
        exit 1
    fi

    if git -C "$PROJECT_DIR" show-ref --verify --quiet "refs/heads/$NAME" 2>/dev/null; then
        echo "ERROR: Branch '$NAME' already exists."
        echo "Pick a different --name, delete the existing branch, or use --resume."
        exit 1
    fi

    # --- Results dir ---
    RESULTS_DIR="$PROJECT_DIR/results/${BASE}_${NAME}_${TIMESTAMP}"
    mkdir -p "$RESULTS_DIR"
fi

# --- Write initial meta.json (only for fresh runs) ---
write_meta() {
    local extra="${1:-}"
    cat > "$RESULTS_DIR/meta.json" <<METAEOF
{
  "base": "$BASE",
  "name": "$NAME",
  "session_id": "$SESSION_UUID",
  "agent": "$AGENT",
  "mode": "$MODE",
  "model": "${MODEL:-default}",
  "prompt": $(printf '%s' "$PROMPT" | jq -Rs .),
  "start_time": "$START_TIME",
  "timestamp": "$TIMESTAMP"${extra}
}
METAEOF
}

if [[ "$RESUME" == "false" ]]; then
    write_meta
fi

echo "=== Agent Run: $NAME ==="
echo "Base:       $BASE"
echo "Agent:      $AGENT"
echo "Mode:       $MODE"
echo "Model:      ${MODEL:-default}"
echo "Session:    $SESSION_UUID"
echo "Results:    $RESULTS_DIR"
echo "Worktree:   $WORKTREE_DIR"
echo "Resume:     $RESUME"
echo ""

# --- Create worktree (only for fresh runs) ---
if [[ "$RESUME" == "false" ]]; then
    echo "Creating worktree from '$BASE'..."
    git -C "$PROJECT_DIR" worktree add -b "$NAME" "$WORKTREE_DIR" "$BASE" --quiet
    echo "Worktree created."
fi

# --- Warm dependency cache ---
echo "Pre-building dependencies in worktree..."
if (cd "$WORKTREE_DIR" && cargo build 2>&1); then
    echo "Pre-build complete."
else
    echo "WARNING: Pre-build failed. Agent may hit cold cache issues."
fi

# --- Lockfile to prevent duplicate agents ---
LOCKFILE="$RESULTS_DIR/.run.lock"

acquire_lock() {
    if [[ -f "$LOCKFILE" ]]; then
        local old_pid
        old_pid=$(cat "$LOCKFILE" 2>/dev/null || echo "")
        if [[ -n "$old_pid" ]] && kill -0 "$old_pid" 2>/dev/null; then
            echo "ERROR: Another agent is already running for this run (PID $old_pid)"
            echo "Lockfile: $LOCKFILE"
            echo "If this is stale, remove it manually: rm $LOCKFILE"
            exit 1
        else
            echo "WARNING: Stale lockfile found (PID $old_pid no longer running). Removing."
            rm -f "$LOCKFILE"
        fi
    fi
    echo $$ > "$LOCKFILE"
}

release_lock() {
    rm -f "$LOCKFILE"
}

acquire_lock

# --- Cleanup handler with signal trapping ---
CHILD_PID=""

cleanup() {
    echo ""
    echo "=== Cleaning up ==="

    # Kill child agent process if running
    if [[ -n "$CHILD_PID" ]] && kill -0 "$CHILD_PID" 2>/dev/null; then
        echo "Killing agent process (PID $CHILD_PID)..."
        kill -TERM "$CHILD_PID" 2>/dev/null || true
        # Wait briefly, then force kill
        sleep 2
        kill -9 "$CHILD_PID" 2>/dev/null || true
        wait "$CHILD_PID" 2>/dev/null || true
    fi

    # Release lockfile
    release_lock

    # Remove worktree if not keeping
    # In levels mode or resume mode, always preserve worktree (it has checkpoints)
    if [[ "$KEEP_WORKTREE" == "false" && "$RESUME" == "false" && "$MODE" != "levels" && -d "$WORKTREE_DIR" ]]; then
        echo "Cleaning up worktree..."
        git -C "$PROJECT_DIR" worktree remove --force "$WORKTREE_DIR" 2>/dev/null || true
        git -C "$PROJECT_DIR" branch -D "$NAME" 2>/dev/null || true
    fi
}
trap cleanup EXIT INT TERM

# --- Turn limits by level difficulty ---
turns_for_level() {
    local level="$1"
    local num=$((10#$level))
    if [[ -n "$MAX_TURNS" ]]; then
        echo "$MAX_TURNS"
    elif (( num <= 9 )); then
        echo "60"
    elif (( num <= 13 )); then
        echo "100"
    else
        echo "160"
    fi
}

# --- Agent launch helpers ---

launch_claude() {
    local workdir="$1"
    local prompt="$2"
    local session_id="$3"
    local output_file="$4"
    local turns="${5:-}"

    local cmd=(claude -p --session-id "$session_id" --dangerously-skip-permissions)
    if [[ -n "$MODEL" ]]; then
        cmd+=(--model "$MODEL")
    fi
    if [[ -n "$turns" ]]; then
        cmd+=(--max-turns "$turns")
    fi
    cmd+=("$prompt")

    (cd "$workdir" && "${cmd[@]}" 2>&1) | tee "$output_file"
}

launch_codex() {
    local workdir="$1"
    local prompt="$2"
    local _session_id="$3"
    local output_file="$4"

    # codex exec = non-interactive mode; script(1) provides required PTY
    script -qec "cd '$workdir' && codex exec --full-auto '$prompt'" "$output_file"
}

launch_opencode() {
    local workdir="$1"
    local prompt="$2"
    local _session_id="$3"
    local output_file="$4"

    # opencode may need a TTY — use script(1) for PTY wrapper
    script -qec "cd '$workdir' && echo '$prompt' | opencode" "$output_file"
}

launch_agent() {
    local workdir="$1"
    local prompt="$2"
    local session_id="$3"
    local output_file="$4"
    local turns="${5:-}"

    case "$AGENT" in
        claude)   launch_claude "$workdir" "$prompt" "$session_id" "$output_file" "$turns" &
                  CHILD_PID=$!
                  wait "$CHILD_PID"
                  local exit_code=$?
                  CHILD_PID=""
                  return $exit_code
                  ;;
        codex)    launch_codex "$workdir" "$prompt" "$session_id" "$output_file" ;;
        opencode) launch_opencode "$workdir" "$prompt" "$session_id" "$output_file" ;;
        *)        echo "ERROR: Unknown agent '$AGENT'"; exit 1 ;;
    esac
}

# --- Capture session data ---
capture_session() {
    local session_id="$1"
    local dest="$2"

    case "$AGENT" in
        claude)
            local session_file
            session_file=$(find ~/.claude/projects/ -name "${session_id}.jsonl" -type f 2>/dev/null | head -1)
            if [[ -n "$session_file" && -f "$session_file" ]]; then
                cp "$session_file" "$dest/session.jsonl"
                echo "Session captured: $dest/session.jsonl"
            else
                echo "WARNING: Session JSONL not found for $session_id"
            fi
            ;;
        codex)
            local codex_dir="$HOME/.codex"
            if [[ -d "$codex_dir" ]]; then
                local latest
                latest=$(find "$codex_dir" -name "*.log" -newer "$dest/meta.json" -type f 2>/dev/null | head -1)
                if [[ -n "$latest" ]]; then
                    cp "$latest" "$dest/session.log"
                    echo "Codex session captured: $dest/session.log"
                fi
            fi
            ;;
        opencode)
            local oc_dir="$HOME/.opencode"
            if [[ -d "$oc_dir" ]]; then
                local latest
                latest=$(find "$oc_dir" -name "*.json" -newer "$dest/meta.json" -type f 2>/dev/null | head -1)
                if [[ -n "$latest" ]]; then
                    cp "$latest" "$dest/session.json"
                    echo "OpenCode session captured: $dest/session.json"
                fi
            fi
            ;;
    esac
}

# ============================================================================
# Execution paths
# ============================================================================

if [[ "$MODE" == "levels" ]]; then
    # --- Path 2: Level-by-level orchestration ---
    echo "=== Level-by-level mode ==="

    # Determine starting level
    START_LEVEL=1
    if [[ -n "$FROM_LEVEL" ]]; then
        START_LEVEL=$((10#$FROM_LEVEL))
        echo "Starting from level $START_LEVEL (--from-level)"
    fi

    for level in $(seq -w 1 16); do
        level_num=$((10#$level))

        # --- Skip levels before --from-level ---
        if (( level_num < START_LEVEL )); then
            echo "Skipping L${level} (before --from-level $START_LEVEL)"
            continue
        fi

        LEVEL_DIR="$RESULTS_DIR/L${level}"

        # --- Resume: skip already-passed levels ---
        if [[ "$RESUME" == "true" && -f "$LEVEL_DIR/status.txt" ]]; then
            if grep -q "PASSED" "$LEVEL_DIR/status.txt" 2>/dev/null; then
                echo "Skipping L${level} — already PASSED (resume mode)"
                continue
            else
                # Level exists but wasn't passed — clean it and retry
                echo "Retrying L${level} — previous attempt was not PASSED"
                rm -rf "$LEVEL_DIR"
            fi
        fi

        mkdir -p "$LEVEL_DIR"
        LEVEL_UUID="$(uuidgen)"
        LEVEL_TURNS=$(turns_for_level "$level")

        echo ""
        echo "--- Level $level (max $LEVEL_TURNS turns) ---"

        # Build prompt for this level
        if [[ "$level" == "01" ]]; then
            LEVEL_PROMPT="Implement the Scheme interpreter. Your task: make level $level tests pass.
Read CLAUDE.md for full instructions.
Run ./scripts/test-level.sh $level to verify. Do not work on other levels."
        else
            # Generate context summary from previous work
            SUMMARY=""
            if command -v claude &>/dev/null; then
                SUMMARY=$( (cd "$WORKTREE_DIR" && claude -p --max-turns 1 \
                    "List the files under src/scheme/, their purpose, and which levels are implemented so far. Be brief, 5-10 lines." 2>/dev/null) || true)
            fi

            LEVEL_PROMPT="Context from previous levels:
${SUMMARY:-See src/scheme/ for current implementation.}

Your task: make level $level tests pass.
Read CLAUDE.md for full instructions.
Run ./scripts/test-level.sh $level to verify. Do not work on other levels."
        fi

        # Launch agent for this level
        LEVEL_START="$(date +%s)"

        set +e
        launch_agent "$WORKTREE_DIR" "$LEVEL_PROMPT" "$LEVEL_UUID" "$LEVEL_DIR/agent-output.txt" "$LEVEL_TURNS"
        AGENT_EXIT=$?
        set -e

        LEVEL_END="$(date +%s)"
        LEVEL_DURATION=$(( LEVEL_END - LEVEL_START ))

        # Capture session
        capture_session "$LEVEL_UUID" "$LEVEL_DIR"

        # Test this level
        set +e
        (cd "$WORKTREE_DIR" && ./scripts/test-level.sh "$level") > "$LEVEL_DIR/test-result.txt" 2>&1
        TEST_EXIT=$?
        set -e

        if [[ $TEST_EXIT -eq 0 ]]; then
            echo "Level $level PASSED (${LEVEL_DURATION}s)" | tee "$LEVEL_DIR/status.txt"

            # --- Git checkpoint: commit passing state ---
            echo "Committing checkpoint for L${level}..."
            (cd "$WORKTREE_DIR" && git add -A && git commit -m "checkpoint: L${level} passed (${LEVEL_DURATION}s)" --allow-empty) \
                >> "$LEVEL_DIR/git-checkpoint.log" 2>&1 || true
            echo "Checkpoint committed."
        else
            echo "Level $level FAILED (${LEVEL_DURATION}s) — stopping" | tee "$LEVEL_DIR/status.txt"
            break
        fi
    done

elif [[ "$MODE" == "full" ]]; then
    # --- Path 1: Full run (one agent, all levels) ---
    echo "=== Full run mode ==="

    FULL_TURNS=""
    if [[ -n "$MAX_TURNS" ]]; then
        FULL_TURNS="$MAX_TURNS"
    fi

    set +e
    launch_agent "$WORKTREE_DIR" "$PROMPT" "$SESSION_UUID" "$RESULTS_DIR/agent-output.txt" "$FULL_TURNS"
    AGENT_EXIT=$?
    set -e

    echo ""
    echo "Agent exited with code: $AGENT_EXIT"

    # Capture session
    capture_session "$SESSION_UUID" "$RESULTS_DIR"

else
    echo "ERROR: Unknown mode '$MODE'. Use 'full' or 'levels'."
    exit 1
fi

# --- Scoring ---
if [[ "$SKIP_BENCH" == "false" ]]; then
    echo ""
    echo "=== Scoring ==="
    set +e
    "$SCRIPT_DIR/bench.sh" "$NAME" "bench" 2>&1 | tee "$RESULTS_DIR/bench.log"
    BENCH_EXIT=$?
    set -e

    # Extract score from bench.log
    SCORE=$(grep -oP 'Score: \K[0-9]+/[0-9]+' "$RESULTS_DIR/bench.log" 2>/dev/null || echo "unknown")
else
    BENCH_EXIT=0
    SCORE="skipped"
fi

# --- Update meta.json with final state ---
END_TIME="$(date -Iseconds)"
write_meta ",
  \"end_time\": \"$END_TIME\",
  \"exit_code\": ${AGENT_EXIT:-0},
  \"bench_exit_code\": $BENCH_EXIT,
  \"score\": \"$SCORE\""

echo ""
echo "=== Run complete ==="
echo "Results: $RESULTS_DIR"
echo "Score:   $SCORE"
