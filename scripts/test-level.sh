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
    echo "Running clippy..."
    if ! cargo clippy -- -D warnings 2>&1; then
        echo "ERROR: clippy failed — fix warnings before testing"
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
