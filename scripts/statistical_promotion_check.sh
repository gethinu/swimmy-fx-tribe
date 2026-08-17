#!/bin/bash
# Flag-ON functional check for the complete S-rank statistical promotion gate.
# A candidate must have a measurable DSR, CPCV refit provenance, and PBO <= max.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

if ! command -v sbcl >/dev/null 2>&1; then
  echo "SBCL not found — statistical promotion check skipped"
  exit 2
fi

# Explicit LOADs below avoid a stale Quicklisp/ASDF FASL cache during this check.
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(load "src/lisp/school/school-rank-system.lisp")' \
  --eval '(load "src/lisp/school/school-ranking.lisp")' \
  --eval '(load "src/lisp/school/school-validation.lisp")' \
  --eval '(load "src/lisp/school/school-db.lisp")' \
  --eval '(load "src/lisp/core/message-dispatcher.lisp")' \
  --eval '(load "src/lisp/strategies/strategies.lisp")' \
  --eval '(load "src/lisp/tests/backtest-db-tests.lisp")' \
  --eval '(load "src/lisp/tests/dsr-tests.lisp")' \
  --eval '(unless (and (swimmy.tests::test-statistical-s-gate-requires-refit-pbo-and-dsr)
                       (swimmy.tests::test-ensure-rank-refuses-overfit-cpcv-result)
                       (swimmy.tests::test-s-candidate-selection-requires-real-dsr)
                       (swimmy.tests::test-cpcv-statistical-proof-persists-and-clears-fail-closed))
            (sb-ext:exit :code 1))' \
  --eval '(format t "~&[STATISTICAL-S-GATE] PASS~%")'
