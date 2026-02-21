#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

exec "$REPO_ROOT/scripts/compile_swimmybridge_mt5.sh" \
  --src "src/mt5/InstitutionalHunterEA.mq5" \
  "$@"
