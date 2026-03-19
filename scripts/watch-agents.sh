#!/usr/bin/env bash
# watch-agents.sh — live dashboard for running/recent agent benchmarks
# Usage:
#   ./scripts/watch-agents.sh              # auto-refresh every 15s
#   ./scripts/watch-agents.sh --once       # single snapshot
#   ./scripts/watch-agents.sh --ts 20260319T025317  # filter by timestamp

set -uo pipefail

RESULTS_DIR="$(cd "$(dirname "$0")/.." && pwd)/results"
ONCE=false
TS_FILTER=""
REFRESH=15
ALL=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        --once)   ONCE=true; shift ;;
        --ts)     TS_FILTER="$2"; shift 2 ;;
        --all)    ALL=true; shift ;;
        --help|-h)
            echo "Usage: $0 [--once] [--ts TIMESTAMP] [--all]"
            echo "  --once   Print once and exit (no refresh loop)"
            echo "  --ts TS  Filter to runs matching timestamp"
            echo "  --all    Show all runs (default: only active + last 6h)"
            exit 0 ;;
        *) echo "Unknown arg: $1"; exit 1 ;;
    esac
done

# --- Colors ---
RED='\033[0;31m'
YEL='\033[0;33m'
GRN='\033[0;32m'
CYN='\033[0;36m'
DIM='\033[2m'
BOLD='\033[1m'
RST='\033[0m'

fmt_duration() {
    local secs=$1
    if (( secs >= 3600 )); then
        printf "%dh%02dm" $((secs/3600)) $(((secs%3600)/60))
    elif (( secs >= 60 )); then
        printf "%dm%02ds" $((secs/60)) $((secs%60))
    else
        printf "%ds" "$secs"
    fi
}

fmt_size() {
    local bytes=$1
    if (( bytes >= 1048576 )); then
        printf "%.1fMB" "$(awk "BEGIN{printf \"%.1f\", $bytes/1048576}")"
    elif (( bytes >= 1024 )); then
        printf "%dKB" $((bytes/1024))
    else
        printf "%dB" "$bytes"
    fi
}

# Get last tool call + cumulative tokens from Claude session.jsonl
claude_detail() {
    local jsonl="$1"
    [[ -f "$jsonl" ]] || return
    python3 -c "
import json, sys
last_tool = ''
last_ts = ''
total_out = 0
for line in open(sys.argv[1]):
    try:
        r = json.loads(line)
    except: continue
    if r.get('type') != 'assistant': continue
    msg = r.get('message', {})
    usage = msg.get('usage', {})
    total_out += usage.get('output_tokens', 0)
    ts = r.get('timestamp', '')[:19]
    content = msg.get('content', [])
    for b in content:
        if isinstance(b, dict) and b.get('type') == 'tool_use':
            last_tool = b.get('name', '?')
            last_ts = ts
print(f'{last_tool}|{last_ts}|{total_out}')
" "$jsonl" 2>/dev/null || echo "||0"
}

render() {
    local now
    now=$(date +%s)

    printf "${BOLD}%-28s  %-9s  %-8s  %-10s  %-8s  %-8s  %s${RST}\n" \
        "NAME" "STATUS" "LEVEL" "IDLE" "ELAPSED" "OUTPUT" "DETAIL"
    printf "%s\n" "$(printf '%.0s─' {1..100})"

    local dirs=()
    for d in "$RESULTS_DIR"/*/meta.json; do
        [[ -f "$d" ]] || continue
        local run_dir
        run_dir="$(dirname "$d")"
        local dirname
        dirname="$(basename "$run_dir")"

        # Apply timestamp filter
        if [[ -n "$TS_FILTER" && "$dirname" != *"$TS_FILTER"* ]]; then
            continue
        fi

        dirs+=("$run_dir")
    done

    # Sort by modification time (most recent first)
    if [[ ${#dirs[@]} -eq 0 ]]; then
        echo "  No runs found in $RESULTS_DIR"
        return
    fi

    for run_dir in "${dirs[@]}"; do
        local dirname
        dirname="$(basename "$run_dir")"
        local meta="$run_dir/meta.json"

        # Parse meta.json (single python call)
        local meta_line name agent mode start_time end_time score_val
        meta_line=$(python3 -c "
import json
m=json.load(open('$meta'))
print('|'.join([m.get('name','?'),m.get('agent','?'),m.get('mode','?'),m.get('start_time',''),m.get('end_time',''),str(m.get('score',''))]))" 2>/dev/null || echo "?|?|?|||")
        IFS='|' read -r name agent mode start_time end_time score_val <<< "$meta_line"

        # --- Status ---
        local status status_color
        local lockfile="$run_dir/.run.lock"
        if [[ -n "$end_time" ]]; then
            status="DONE"
            status_color="$GRN"
        elif [[ -f "$lockfile" ]]; then
            local pid
            pid=$(cat "$lockfile" 2>/dev/null || echo "0")
            if kill -0 "$pid" 2>/dev/null; then
                status="RUNNING"
                status_color="$CYN"
            else
                status="DEAD"
                status_color="$RED"
            fi
        else
            status="DONE"
            status_color="$GRN"
        fi

        # Skip old DONE runs unless --all or --ts filter
        if [[ "$status" == "DONE" && "$ALL" == "false" && -z "$TS_FILTER" ]]; then
            if [[ -n "$start_time" ]]; then
                local start_epoch
                start_epoch=$(date -d "$start_time" +%s 2>/dev/null || echo 0)
                if (( now - start_epoch > 21600 )); then  # >6h old
                    continue
                fi
            fi
        fi

        # --- Level progress ---
        local level_str="---"
        if [[ "$mode" == "levels" ]]; then
            local passed=0 total=0 last_level=""
            for ldir in "$run_dir"/L*/; do
                [[ -d "$ldir" ]] || continue
                total=$((total + 1))
                local lname
                lname="$(basename "$ldir")"
                if [[ -f "$ldir/status.txt" ]]; then
                    if grep -qi "PASSED" "$ldir/status.txt" 2>/dev/null; then
                        passed=$((passed + 1))
                    fi
                    last_level="$lname"
                else
                    # Level started but no status yet = currently running
                    last_level="${lname}…"
                fi
            done
            if [[ $total -gt 0 ]]; then
                level_str="${last_level} (${passed}/16)"
            fi
        fi

        # --- Idle time ---
        local idle_str="---" idle_secs=0
        # Find the most recently modified output file
        local output_file=""
        if [[ "$mode" == "levels" ]]; then
            # In levels mode, check level subdirs for latest agent-output.txt
            local latest_mtime=0
            for f in "$run_dir"/L*/agent-output.txt "$run_dir"/agent-output.txt; do
                [[ -f "$f" ]] || continue
                local mt
                mt=$(stat --format='%Y' "$f" 2>/dev/null || echo 0)
                if (( mt > latest_mtime )); then
                    latest_mtime=$mt
                    output_file="$f"
                fi
            done
        else
            output_file="$run_dir/agent-output.txt"
        fi

        if [[ -f "$output_file" ]]; then
            local mtime
            mtime=$(stat --format='%Y' "$output_file" 2>/dev/null || echo "$now")
            idle_secs=$((now - mtime))
            idle_str=$(fmt_duration $idle_secs)
        fi

        # Color idle time
        local idle_color="$RST"
        if [[ "$status" == "RUNNING" ]]; then
            if (( idle_secs > 900 )); then
                idle_color="$RED"
            elif (( idle_secs > 300 )); then
                idle_color="$YEL"
            fi
        fi

        # --- Elapsed ---
        local elapsed_str="---"
        if [[ -n "$start_time" ]]; then
            local start_epoch
            start_epoch=$(date -d "$start_time" +%s 2>/dev/null || echo "$now")
            if [[ -n "$end_time" ]]; then
                local end_epoch
                end_epoch=$(date -d "$end_time" +%s 2>/dev/null || echo "$now")
                elapsed_str=$(fmt_duration $((end_epoch - start_epoch)))
            else
                elapsed_str=$(fmt_duration $((now - start_epoch)))
            fi
        fi

        # --- Output size ---
        local out_str="---"
        if [[ -f "$output_file" ]]; then
            local sz
            sz=$(stat --format='%s' "$output_file" 2>/dev/null || echo 0)
            out_str=$(fmt_size "$sz")
        fi

        # --- Detail (Claude-specific) ---
        local detail=""
        if [[ "$agent" == "claude" ]]; then
            local jsonl="$run_dir/session.jsonl"
            # For running agents, find live JSONL from Claude's project dir
            if [[ "$status" == "RUNNING" && ! -s "$jsonl" ]]; then
                # Derive Claude project dir from worktree path
                local workspace_dir
                workspace_dir="$(cd "$(dirname "$0")/.." && pwd)/../workspace/$name"
                if [[ -d "$workspace_dir" ]]; then
                    local real_path
                    real_path=$(realpath "$workspace_dir" 2>/dev/null || echo "")
                    if [[ -n "$real_path" ]]; then
                        local proj_dir_name
                        proj_dir_name=$(echo "$real_path" | sed 's|^/||; s|[/.]|-|g; s|^|-|')
                        local proj_dir="$HOME/.claude/projects/$proj_dir_name"
                        if [[ -d "$proj_dir" ]]; then
                            # Find most recently modified JSONL
                            local live
                            live=$(ls -t "$proj_dir"/*.jsonl 2>/dev/null | head -1 || true)
                            if [[ -n "$live" && -f "$live" ]]; then
                                jsonl="$live"
                            fi
                        fi
                    fi
                fi
                # Fallback: try session_id from meta.json
                if [[ ! -s "$jsonl" || "$jsonl" == "$run_dir/session.jsonl" ]]; then
                    local sid
                    sid=$(python3 -c "import json; print(json.load(open('$meta')).get('session_id',''))" 2>/dev/null || echo "")
                    if [[ -n "$sid" ]]; then
                        local live
                        live=$(find ~/.claude/projects/ -name "${sid}.jsonl" -type f 2>/dev/null | head -1)
                        [[ -n "$live" && -f "$live" ]] && jsonl="$live"
                    fi
                fi
                # Update idle from live JSONL mtime
                if [[ -f "$jsonl" && -s "$jsonl" ]]; then
                    local live_mt
                    live_mt=$(stat --format='%Y' "$jsonl" 2>/dev/null || echo 0)
                    if (( live_mt > 0 )); then
                        idle_secs=$((now - live_mt))
                        idle_str=$(fmt_duration $idle_secs)
                        idle_color="$RST"
                        if (( idle_secs > 900 )); then
                            idle_color="$RED"
                        elif (( idle_secs > 300 )); then
                            idle_color="$YEL"
                        fi
                    fi
                fi
            fi
            if [[ -f "$jsonl" && -s "$jsonl" ]]; then
                local info
                info=$(claude_detail "$jsonl")
                local last_tool last_ts total_out
                IFS='|' read -r last_tool last_ts total_out <<< "$info"
                if [[ -n "$last_tool" ]]; then
                    detail="last: ${last_tool}"
                    if [[ -n "$total_out" && "$total_out" != "0" ]]; then
                        detail="${detail}  tok: ${total_out}"
                    fi
                fi
            fi
        fi

        # --- Score (for DONE runs) ---
        if [[ "$status" == "DONE" ]]; then
            if [[ -n "$score_val" ]]; then
                detail="score: ${score_val}"
            fi
        fi

        # --- Print row ---
        printf "%-28s  ${status_color}%-9s${RST}  %-8s  ${idle_color}%-10s${RST}  %-8s  %-8s  ${DIM}%s${RST}\n" \
            "$name" "$status" "$level_str" "$idle_str" "$elapsed_str" "$out_str" "$detail"
    done
}

if [[ "$ONCE" == "true" ]]; then
    render
else
    while true; do
        clear
        echo -e "${BOLD}Agent Monitor${RST}  $(date '+%H:%M:%S')  (refresh ${REFRESH}s, Ctrl-C to quit)"
        echo ""
        render
        sleep "$REFRESH"
    done
fi
