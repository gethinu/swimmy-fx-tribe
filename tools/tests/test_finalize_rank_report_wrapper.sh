#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT/tools/ops/finalize_rank_report.sh"

if [[ ! -x "$SCRIPT" ]]; then
  echo "missing executable: $SCRIPT"
  exit 1
fi

grep -q "sbcl_env.sh" "$SCRIPT"
grep -q "SWIMMY_SBCL_DYNAMIC_SPACE_MB" "$SCRIPT"
grep -q "finalize_rank_report.lisp" "$SCRIPT"
