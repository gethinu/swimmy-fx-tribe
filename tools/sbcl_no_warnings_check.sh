#!/usr/bin/env bash
set -euo pipefail

SWIMMY_HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$SWIMMY_HOME"

if [ -f tools/sbcl_env.sh ]; then
  # shellcheck disable=SC1091
  source tools/sbcl_env.sh
fi

SBCL_BASE=("sbcl" "--dynamic-space-size" "${SWIMMY_SBCL_DYNAMIC_SPACE_MB:-1024}" "--noinform")
log="$(mktemp)"

"${SBCL_BASE[@]}" --eval '(load "swimmy.asd")' --eval '(asdf:load-system :swimmy :force t)' --eval '(quit)' 2>&1 | tee "$log"

if rg -q "WARNING:|STYLE-WARNING:" "$log"; then
  echo "[FAIL] SBCL warnings detected" >&2
  exit 1
fi

echo "[PASS] No SBCL warnings."
