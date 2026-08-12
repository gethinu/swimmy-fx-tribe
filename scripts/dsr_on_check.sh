#!/bin/bash
# dsr_on_check.sh — Flag-ON functional check for the REAL Lisp Deflated Sharpe.
#
# Loads swimmy, turns on swimmy.school::*enable-real-dsr*, and asserts the ON path
# is not a silent no-op:
#   * a short pnl-history (< *dsr-min-samples*) => ABSTAINS (returns NIL);
#   * an adequate pnl-history => a probability in [0,1].
#
# Exit codes: 0 = check passed; 1 = real failure (ran but assertion failed);
#             2 = environment not provisioned (SBCL absent) — caller may SKIP.
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

if ! command -v sbcl >/dev/null 2>&1; then
  echo "❌ SBCL not found — DSR ON check SKIPPED (environment not provisioned)"
  exit 2
fi

sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(in-package :swimmy.school)' \
  --eval '(setf *enable-real-dsr* t)' \
  --eval '(let* ((short (make-strategy :name "short" :pnl-history (list 1.0 -1.0 2.0))) (mk (lambda (seed) (make-strategy :name "s" :pnl-history (loop for i from 0 below 60 collect (+ 0.1d0 (* 0.01d0 (mod (* seed (1+ i)) 7)) (if (evenp i) 0.05d0 -0.05d0)))))) (target (funcall mk 3)) (*strategy-knowledge-base* (list target (funcall mk 5) (funcall mk 2) (funcall mk 4)))) (let ((d-short (deflated-sharpe-ratio short)) (d-ok (deflated-sharpe-ratio target))) (format t "~&[DSR-ON] short=>~a  adequate=>~,4f~%" d-short d-ok) (sb-ext:exit :code (if (and (null d-short) d-ok (>= d-ok 0.0d0) (<= d-ok 1.0d0)) 0 1))))'
