#!/usr/bin/env bash
set -euo pipefail

SWIMMY_HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$SWIMMY_HOME"

if [ -f tools/sbcl_env.sh ]; then
    # shellcheck disable=SC1091
    source tools/sbcl_env.sh
fi

TIMEOUT_CMD=()
if command -v timeout >/dev/null 2>&1; then
  TIMEOUT_CMD=(timeout 60)
fi

SBCL_BASE=("sbcl" "--dynamic-space-size" "$SWIMMY_SBCL_DYNAMIC_SPACE_MB" "--noinform")

printf "[SANITY] SBCL load check...\n"
"${TIMEOUT_CMD[@]}" "${SBCL_BASE[@]}" --eval '(load "swimmy.asd")' --eval '(asdf:load-system :swimmy)' --eval '(quit)'

printf "[SANITY] Qualification cycle...\n"
"${TIMEOUT_CMD[@]}" "${SBCL_BASE[@]}" --eval '(load "swimmy.asd")' --eval '(asdf:load-system :swimmy)' --eval '(swimmy.school::run-qualification-cycle)' --eval '(quit)'

printf "[SANITY] Graveyard cache size...\n"
"${TIMEOUT_CMD[@]}" "${SBCL_BASE[@]}" --eval '(load "swimmy.asd")' --eval '(asdf:load-system :swimmy)' --eval '(format t "[CHECK] graveyard-cache size=~d~%" (length (swimmy.school::load-graveyard-cache)))' --eval '(quit)'

printf "[SANITY] Done.\n"
