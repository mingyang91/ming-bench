#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

ensure_node_modules() {
  if [ -d node_modules ]; then
    return
  fi

  if npm ci --ignore-scripts; then
    return
  fi

  rm -rf node_modules

  local workspace_root cache_dir
  workspace_root="$(cd ../../.. && pwd)"
  cache_dir="$(
    find "$workspace_root" -maxdepth 4 -path '*/bench/ts/node_modules' \
      ! -path "$PWD/node_modules" \
      | head -n 1
  )"

  if [ -z "$cache_dir" ]; then
    echo "npm ci failed and no cached node_modules directory was found" >&2
    exit 1
  fi

  cp -al "$cache_dir" ./node_modules 2>/dev/null || cp -a "$cache_dir" ./node_modules
}

ensure_node_modules
npm run build
