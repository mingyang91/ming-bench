#!/usr/bin/env bash
set -euo pipefail

# ============================================================================
# CS 61A Scheme Interpreter Benchmark Runner
#
# Usage: ./scripts/bench.sh <branch> [run-id]
#
# Architecture:
#   - Compiles tests on the HOST (cargo test --no-run)
#   - Copies test binary into a minimal container (debian:bookworm-slim)
#   - Runs each level with resource limits (memory, CPU, PID, timeout)
#
# Per-level: timeout 30s, memory 2GB, 2 CPUs, 256 PIDs
# Results saved to results/<branch>_<run-id>_<timestamp>.log
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
IMAGE_NAME="cs61a-bench"
TIMEOUT=30
LEVELS=(01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16)

# --- Args ---
BRANCH="${1:?Usage: bench.sh <branch> [run-id]}"
RUN_ID="${2:-run-$(date +%s)}"
TIMESTAMP="$(date +%Y-%m-%dT%H:%M:%S)"

# --- Results setup ---
RESULTS_DIR="$PROJECT_DIR/results"
mkdir -p "$RESULTS_DIR"
RESULT_FILE="$RESULTS_DIR/${BRANCH}_${RUN_ID}_$(date +%Y%m%dT%H%M%S).log"

# --- Worktree for clean source ---
WORKTREE_DIR="$(mktemp -d)"
WORKTREE_BRANCH="bench-${RUN_ID}-$$"

cleanup() {
    if [ -d "$WORKTREE_DIR" ]; then
        git -C "$PROJECT_DIR" worktree remove --force "$WORKTREE_DIR" 2>/dev/null || true
    fi
    git -C "$PROJECT_DIR" branch -D "$WORKTREE_BRANCH" 2>/dev/null || true
}
trap cleanup EXIT

echo "=== CS 61A Bench: branch=$BRANCH run=$RUN_ID ==="
echo "Creating worktree from '$BRANCH'..."
git -C "$PROJECT_DIR" worktree add -b "$WORKTREE_BRANCH" "$WORKTREE_DIR" "$BRANCH" --quiet

# --- Ensure image exists ---
if ! sudo podman image exists "$IMAGE_NAME"; then
    echo "Image '$IMAGE_NAME' not found. Run ./scripts/setup.sh first."
    exit 1
fi

# --- Compile tests on host ---
echo "Compiling tests on host..."
TEST_BIN=$(cd "$WORKTREE_DIR" && cargo test --no-run --message-format=json 2>/dev/null \
    | jq -r 'select(.reason == "compiler-artifact") | select(.target.kind[] == "lib") | .executable // empty' \
    | tail -1)

if [ -z "$TEST_BIN" ]; then
    # Fallback: find the test binary by name
    TEST_BIN=$(cd "$WORKTREE_DIR" && cargo test --no-run 2>&1 \
        | grep -oP 'Executable.*\(\K[^)]+' | tail -1)
fi

if [ -z "$TEST_BIN" ] || [ ! -f "$TEST_BIN" ]; then
    echo "ERROR: Failed to locate test binary. Compilation output:"
    (cd "$WORKTREE_DIR" && cargo test --no-run 2>&1) | tee -a "$RESULT_FILE"
    exit 1
fi

echo "Test binary: $TEST_BIN"

# --- Run tests level by level ---
{
    echo "Branch: $BRANCH"
    echo "Run ID: $RUN_ID"
    echo "Date:   $TIMESTAMP"
    echo "Timeout: ${TIMEOUT}s per level"
    echo "---"
} | tee "$RESULT_FILE"

TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_LEVELS=()
TIMEOUT_LEVELS=()

for LEVEL in "${LEVELS[@]}"; do
    LEVEL_START="$(date +%s)"

    set +e
    OUTPUT=$(sudo podman run --rm \
        --memory=2g \
        --cpus=2 \
        --pids-limit=256 \
        -v "$TEST_BIN:/bench/test_bin:ro,Z" \
        "$IMAGE_NAME" \
        "timeout ${TIMEOUT}s /bench/test_bin test_l${LEVEL} --test-threads=1 2>&1" \
        2>&1)
    EXIT_CODE=$?
    set -e

    LEVEL_END="$(date +%s)"
    DURATION=$(( LEVEL_END - LEVEL_START ))

    # Parse test results
    TEST_LINE=$(echo "$OUTPUT" | grep -E "^test result:" | tail -1 || true)
    LEVEL_PASSED=$(echo "$TEST_LINE" | grep -oP '\d+ passed' | grep -oP '\d+' || echo "0")
    LEVEL_FAILED=$(echo "$TEST_LINE" | grep -oP '\d+ failed' | grep -oP '\d+' || echo "0")
    LEVEL_TOTAL=$(( LEVEL_PASSED + LEVEL_FAILED ))

    if [ "$EXIT_CODE" -eq 124 ] || [ "$DURATION" -ge "$TIMEOUT" ]; then
        STATUS="TIMEOUT"
        TIMEOUT_LEVELS+=("L${LEVEL}")
    elif [ "$EXIT_CODE" -eq 0 ]; then
        STATUS="PASS"
    else
        STATUS="FAIL"
        FAILED_LEVELS+=("L${LEVEL}")
    fi

    TOTAL_TESTS=$(( TOTAL_TESTS + LEVEL_TOTAL ))
    PASSED_TESTS=$(( PASSED_TESTS + LEVEL_PASSED ))

    RESULT_LINE="L${LEVEL}: ${STATUS} (${DURATION}s) [${LEVEL_PASSED}/${LEVEL_TOTAL} tests]"
    echo "$RESULT_LINE" | tee -a "$RESULT_FILE"

    # Append full output for failed/timeout levels
    if [ "$STATUS" != "PASS" ]; then
        {
            echo "  --- L${LEVEL} output ---"
            echo "$OUTPUT" | sed 's/^/  /'
            echo "  --- end L${LEVEL} ---"
        } >> "$RESULT_FILE"
    fi
done

# --- Summary ---
{
    echo "---"
    echo "Score: ${PASSED_TESTS}/${TOTAL_TESTS} tests passed"
    [ ${#FAILED_LEVELS[@]} -gt 0 ] && echo "Failed:  ${FAILED_LEVELS[*]}"
    [ ${#TIMEOUT_LEVELS[@]} -gt 0 ] && echo "Timeout: ${TIMEOUT_LEVELS[*]}"
    echo "Results: $RESULT_FILE"
} | tee -a "$RESULT_FILE"

echo "=== Bench complete ==="
