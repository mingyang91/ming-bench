#!/usr/bin/env bash
set -euo pipefail

# Run tests for a single level inside a container.
# Usage: ./scripts/test-level.sh 01      # test level 1
#        ./scripts/test-level.sh 05      # test level 5
#        ./scripts/test-level.sh         # test all levels

IMAGE_NAME="bench-runner"
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

# Build test binary on host
BIN=$(cargo test --no-run 2>&1 | grep -oP 'target/debug/deps/cs61a_bench-[a-f0-9]+' | head -1)
if [[ -z "$BIN" ]]; then
    echo "ERROR: Failed to build test binary"
    exit 1
fi

# Build filter
if [[ -n "${1:-}" ]]; then
    FILTER="test_l${1}"
else
    FILTER=""
fi

# Run inside container
sudo podman run --rm \
    --memory=1g \
    --cpus=1 \
    --pids-limit=256 \
    -v "./$BIN:/bench/test_bin:ro,Z" \
    "$IMAGE_NAME" \
    "timeout ${TIMEOUT}s /bench/test_bin ${FILTER} --test-threads=1 2>&1"
