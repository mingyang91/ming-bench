#!/usr/bin/env bash
set -euo pipefail

# ============================================================================
# List all bench results in a table.
#
# Usage: ./scripts/list-results.sh
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
RESULTS_DIR="$PROJECT_DIR/results"

if [[ ! -d "$RESULTS_DIR" ]]; then
    echo "No results directory found."
    exit 0
fi

# Collect results
RUNS=()
while IFS= read -r meta_file; do
    RUNS+=("$meta_file")
done < <(find "$RESULTS_DIR" -name "meta.json" -type f 2>/dev/null | sort)

if [[ ${#RUNS[@]} -eq 0 ]]; then
    echo "No results found."
    exit 0
fi

# Header
printf "%-40s %-10s %-8s %-10s %-8s %s\n" "RUN" "BASE" "AGENT" "SCORE" "DURATION" "MODE"
printf "%-40s %-10s %-8s %-10s %-8s %s\n" "---" "----" "-----" "-----" "--------" "----"

for meta_file in "${RUNS[@]}"; do
    run_dir="$(dirname "$meta_file")"
    run_name="$(basename "$run_dir")"

    # Parse meta.json with jq
    base=$(jq -r '.base // "?"' "$meta_file")
    agent=$(jq -r '.agent // "?"' "$meta_file")
    score=$(jq -r '.score // "?"' "$meta_file")
    mode=$(jq -r '.mode // "?"' "$meta_file")
    start=$(jq -r '.start_time // ""' "$meta_file")
    end=$(jq -r '.end_time // ""' "$meta_file")

    # Calculate duration
    duration="?"
    if [[ -n "$start" && -n "$end" && "$end" != "null" ]]; then
        start_epoch=$(date -d "$start" +%s 2>/dev/null || echo "")
        end_epoch=$(date -d "$end" +%s 2>/dev/null || echo "")
        if [[ -n "$start_epoch" && -n "$end_epoch" ]]; then
            secs=$(( end_epoch - start_epoch ))
            mins=$(( secs / 60 ))
            remaining_secs=$(( secs % 60 ))
            duration="${mins}m${remaining_secs}s"
        fi
    fi

    # If no score in meta, try bench.log
    if [[ "$score" == "?" || "$score" == "null" ]]; then
        bench_log="$run_dir/bench.log"
        if [[ -f "$bench_log" ]]; then
            score=$(grep -oP 'Score: \K[0-9]+/[0-9]+' "$bench_log" 2>/dev/null || echo "?")
        fi
    fi

    printf "%-40s %-10s %-8s %-10s %-8s %s\n" "$run_name" "$base" "$agent" "$score" "$duration" "$mode"
done
