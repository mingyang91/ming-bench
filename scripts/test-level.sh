#!/usr/bin/env bash
set -euo pipefail

# Run tests for a single level inside a container.
# Usage: ./scripts/test-level.sh 01      # test level 1
#        ./scripts/test-level.sh 05      # test level 5
#        ./scripts/test-level.sh all     # test all levels (300s timeout)

IMAGE_NAME="cs61a-bench"
TIMEOUT=30

# Run clippy when the codebase has clippy discipline enabled (strategy branch and its descendants).
# Detection: check if src/lib.rs contains deny(clippy::unwrap_used) — the marker for clippy discipline.
if grep -q 'deny(clippy::unwrap_used)' src/lib.rs 2>/dev/null; then
    RUN_CLIPPY=true
else
    RUN_CLIPPY=false
fi

if [[ "$RUN_CLIPPY" == "true" ]]; then
    # ---------------------------------------------------------------------------
    # Leveled clippy: thresholds tighten as levels increase.
    #
    # clippy.toml holds the baseline config (too-many-lines-threshold,
    # excessive-nesting-threshold). We override per-level by writing a
    # temporary clippy.toml with the leveled thresholds, then restore it.
    #
    # Function length (too-many-lines):
    #   L01-L05: 80  (bootstrapping, design settling)
    #   L06+:    60  (standard)
    #
    # Nesting depth (excessive-nesting):
    #   All levels: 3 (consistent)
    #
    # Dead code:
    #   L01-L05: allowed (forward-declared types)
    #   L06+:    denied
    # ---------------------------------------------------------------------------
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

    # Pedantic lints are configured in src/lib.rs as #![warn(...)] attributes.
    # deny(warnings) in lib.rs promotes them to errors.
    # test-level.sh only needs to pass level-specific allows and run clippy.

    echo "Running clippy --fix (auto-fixing trivial lints)..."
    cargo clippy --fix --allow-dirty --allow-staged -- \
        -D warnings \
        $CLIPPY_ALLOWS 2>&1 || true

    echo "Running clippy (verify, fn limit=${FN_LIMIT})..."
    if ! cargo clippy -- \
        -D warnings \
        $CLIPPY_ALLOWS 2>&1; then

        # Restore original clippy.toml
        mv clippy.toml.bak clippy.toml 2>/dev/null || true
        echo "ERROR: clippy failed — fix warnings before testing"
        exit 1
    fi

    # Restore original clippy.toml
    mv clippy.toml.bak clippy.toml 2>/dev/null || true
fi

# ---------------------------------------------------------------------------
# mod.rs size check (clippy can't enforce per-file line limits)
# ---------------------------------------------------------------------------
check_mod_size() {
    local level="$1"

    # Leveled ramp:
    #   L01-L03: 300 (bootstrapping)
    #   L04-L06: 200 (time to split into submodules)
    #   L07+:    100 (entry point + reexports only)
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

# Only run mod.rs size check on leveled runs (not "all")
if [[ "${1:-}" != "all" && -n "${1:-}" ]]; then
    LN=$((10#$1))
    if ! check_mod_size "$LN"; then
        echo ""
        echo "Fix structural violations before testing."
        exit 1
    fi
fi

# Build test binary on host (release mode for realistic perf)
BIN=$(cargo test --no-run --release 2>&1 | grep -oP 'target/release/deps/cs61a_bench-[a-f0-9]+' | head -1)
if [[ -z "$BIN" ]]; then
    echo "ERROR: Failed to build test binary"
    exit 1
fi

# Build filter
if [[ -z "${1:-}" ]]; then
    echo "Usage: test-level.sh <level>   # e.g., 01, 05, 16"
    echo "       test-level.sh all       # run all levels (300s timeout)"
    exit 1
elif [[ "$1" == "all" ]]; then
    FILTER=""
    TIMEOUT=300
else
    FILTER="test_l${1}"
fi

# Run inside container
sudo podman run --rm \
    --memory=1g \
    --cpus=1 \
    --pids-limit=256 \
    -v "./$BIN:/bench/test_bin:ro,Z" \
    "$IMAGE_NAME" \
    "timeout ${TIMEOUT}s /bench/test_bin ${FILTER} --test-threads=1 2>&1"
