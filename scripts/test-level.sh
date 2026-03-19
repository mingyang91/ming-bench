#!/usr/bin/env bash
set -euo pipefail

# Run tests inside a container with regression coverage.
# When given a level N, runs levels 1..N (not just N) to catch regressions.
# Usage: ./scripts/test-level.sh 01      # test level 1
#        ./scripts/test-level.sh 05      # test levels 1-5
#        ./scripts/test-level.sh all     # test all levels (300s timeout)

IMAGE_NAME="cs61a-bench"
TIMEOUT=30

# ---------------------------------------------------------------------------
# Quality gates — only active when clippy.toml is present (strategy branch).
# On branches without clippy.toml, this entire block is skipped.
# ---------------------------------------------------------------------------
if [[ -f clippy.toml ]]; then
    LEVEL_NUM="${1:-all}"
    CLIPPY_ALLOWS=""
    FN_LIMIT=60

    if [[ "$LEVEL_NUM" != "all" ]]; then
        LN=$((10#$LEVEL_NUM))  # strip leading zero
        if [[ "$LN" -le 5 ]]; then
            CLIPPY_ALLOWS="-A dead_code"
            FN_LIMIT=80
        fi
    fi

    # Write leveled clippy.toml (backup and restore original)
    cp clippy.toml clippy.toml.bak 2>/dev/null || true
    cat > clippy.toml <<EOF
too-many-lines-threshold = ${FN_LIMIT}
excessive-nesting-threshold = 3
EOF

    echo "Running clippy --fix (auto-fixing trivial lints)..."
    cargo clippy --fix --allow-dirty --allow-staged -- \
        -D warnings \
        $CLIPPY_ALLOWS 2>&1 || true

    echo "Running clippy (verify, fn limit=${FN_LIMIT})..."
    if ! cargo clippy -- \
        -D warnings \
        $CLIPPY_ALLOWS 2>&1; then

        mv clippy.toml.bak clippy.toml 2>/dev/null || true
        echo "ERROR: clippy failed — fix warnings before testing"
        exit 1
    fi

    mv clippy.toml.bak clippy.toml 2>/dev/null || true

    # --- mod.rs size check ---
    check_mod_size() {
        local level="$1"
        local mod_limit=100
        if [[ "$level" -le 3 ]]; then
            mod_limit=300
        elif [[ "$level" -le 6 ]]; then
            mod_limit=200
        fi

        local mod_file="src/scheme/mod.rs"
        if [[ -f "$mod_file" ]]; then
            local impl_lines
            impl_lines=$(sed -n '1,/^#\[cfg(test)\]/p' "$mod_file" | wc -l)
            if [[ "$impl_lines" -gt "$mod_limit" ]]; then
                echo "ERROR: mod.rs has $impl_lines impl lines (limit for L$(printf '%02d' "$level"): $mod_limit)."
                echo "  Extract implementation logic into submodules."
                return 1
            fi
        fi
    }

    if [[ "${1:-}" != "all" && -n "${1:-}" ]]; then
        LN=$((10#$1))
        if ! check_mod_size "$LN"; then
            echo ""
            echo "Fix structural violations before testing."
            exit 1
        fi
    fi
fi

# ---------------------------------------------------------------------------
# Build and test
# ---------------------------------------------------------------------------

# Build test binary on host (release mode for realistic perf)
BIN=$(cargo test --no-run --release 2>&1 | grep -oP 'target/release/deps/cs61a_bench-[a-f0-9]+' | head -1)
if [[ -z "$BIN" ]]; then
    echo "ERROR: Failed to build test binary"
    exit 1
fi

# Build level list — when testing level N, run levels 1..N for regression coverage
if [[ -z "${1:-}" ]]; then
    echo "Usage: test-level.sh <level>   # e.g., 01, 05, 16"
    echo "       test-level.sh all       # run all levels (300s timeout)"
    exit 1
elif [[ "$1" == "all" ]]; then
    LEVELS=()
    TIMEOUT=300
else
    TARGET=$((10#$1))
    LEVELS=()
    for ((i=1; i<=TARGET; i++)); do
        LEVELS+=("$(printf '%02d' "$i")")
    done
    # Scale timeout: 30s per level
    TIMEOUT=$(( TARGET * 30 ))
fi

# Run inside container — one invocation per level for clear pass/fail reporting
if [[ ${#LEVELS[@]} -eq 0 ]]; then
    # "all" mode: single run, no filter
    sudo podman run --rm \
        --memory=1g \
        --cpus=1 \
        --pids-limit=256 \
        -v "./$BIN:/bench/test_bin:ro,Z" \
        "$IMAGE_NAME" \
        "timeout ${TIMEOUT}s /bench/test_bin --test-threads=1 2>&1"
else
    FAILED=0
    for LVL in "${LEVELS[@]}"; do
        echo ""
        echo "===== Level $LVL ====="
        if ! sudo podman run --rm \
            --memory=1g \
            --cpus=1 \
            --pids-limit=256 \
            -v "./$BIN:/bench/test_bin:ro,Z" \
            "$IMAGE_NAME" \
            "timeout 30s /bench/test_bin test_l${LVL} --test-threads=1 2>&1"; then
            echo "FAIL: Level $LVL"
            FAILED=1
            break
        fi
    done
    if [[ $FAILED -ne 0 ]]; then
        exit 1
    fi
    echo ""
    echo "All levels 01..${LEVELS[-1]} passed."
fi
