#!/usr/bin/env bash
set -euo pipefail

# Ensure we can source the script without executing main
source "$(cd "$(dirname "$0")/.." && pwd)/scripts/ci-test.sh"

sample_output=$(cat <<'SAMPLE'
═══════════════════════════════════════
📊 RESULTS: 119 passed, 0 failed
═══════════════════════════════════════
Running TEST-FOO... ✅ PASSED
Running TEST-BAR... ✅ PASSED
SAMPLE
)

extract_counts "$sample_output"

if [[ "$PASS_COUNT" -ne 119 ]]; then
  echo "Expected PASS_COUNT=119, got $PASS_COUNT" >&2
  exit 1
fi
if [[ "$FAIL_COUNT" -ne 0 ]]; then
  echo "Expected FAIL_COUNT=0, got $FAIL_COUNT" >&2
  exit 1
fi
