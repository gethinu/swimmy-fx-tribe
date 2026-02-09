#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

if [[ -f "$ROOT/tools/sbcl_env.sh" ]]; then
  # shellcheck source=tools/sbcl_env.sh
  source "$ROOT/tools/sbcl_env.sh"
fi

: "${SWIMMY_SBCL_DYNAMIC_SPACE_MB:=4096}"
export SWIMMY_SBCL_DYNAMIC_SPACE_MB
export SWIMMY_HOME="${SWIMMY_HOME:-$ROOT}"

exec sbcl --dynamic-space-size "$SWIMMY_SBCL_DYNAMIC_SPACE_MB" --script "$ROOT/tools/ops/finalize_rank_report.lisp"
