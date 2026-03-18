#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
IMAGE_NAME="cs61a-bench"

echo "=== CS 61A Bench Setup ==="

# 1. Install podman + jq if missing
for pkg in podman jq; do
    if ! command -v "$pkg" &>/dev/null; then
        echo "Installing $pkg..."
        sudo apt-get update -qq
        sudo apt-get install -y -qq "$pkg"
    fi
done
echo "Podman: $(podman --version)"

# 2. Build lightweight bench container image
echo "Building bench container image '${IMAGE_NAME}'..."
sudo podman build -t "$IMAGE_NAME" -f "$PROJECT_DIR/Dockerfile.bench" "$PROJECT_DIR"
echo "Image '${IMAGE_NAME}' built successfully."

echo "=== Setup complete ==="
echo "Run benchmarks with: ./scripts/bench.sh <branch> [run-id]"
