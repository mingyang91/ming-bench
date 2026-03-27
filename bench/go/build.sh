#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

sudo podman run --rm \
  -v "${SCRIPT_DIR}:/src:Z" \
  -w /src \
  docker.io/library/golang:1.23-bookworm \
  bash -lc 'export PATH="/usr/local/go/bin:${PATH}"; CGO_ENABLED=0 go test -c -o test_bin'
