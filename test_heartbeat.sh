#!/bin/bash
# Test heartbeat notification
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SWIMMY_HOME="${SWIMMY_HOME:-$SCRIPT_DIR}"
cd "$SWIMMY_HOME"
sbcl --noinform --non-interactive --load brain.lisp --eval "(heartbeat-now)" --quit 2>&1 | tail -20
