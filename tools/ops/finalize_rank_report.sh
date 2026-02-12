#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

usage() {
  cat <<'USAGE'
Usage: finalize_rank_report.sh [--with-rank-eval]

Options:
  --with-rank-eval  Run rank evaluation before report generation.
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --with-rank-eval)
      export SWIMMY_FINALIZE_REPORT_RUN_RANK_EVAL=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [[ -f "$ROOT/tools/sbcl_env.sh" ]]; then
  # shellcheck source=tools/sbcl_env.sh
  source "$ROOT/tools/sbcl_env.sh"
fi

: "${SWIMMY_SBCL_DYNAMIC_SPACE_MB:=4096}"
export SWIMMY_SBCL_DYNAMIC_SPACE_MB
export SWIMMY_HOME="${SWIMMY_HOME:-$ROOT}"

exec sbcl --dynamic-space-size "$SWIMMY_SBCL_DYNAMIC_SPACE_MB" --script "$ROOT/tools/ops/finalize_rank_report.lisp"
