#!/bin/bash
# ci-test.sh - Graham Advisor: CI/CD Test Runner with History
# V2.0: Added JSON output and history tracking

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

# History file
HISTORY_FILE="$SCRIPT_DIR/.opus/ci_history.json"
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
JST_TIME=$(TZ=Asia/Tokyo date +"%Y-%m-%d %H:%M:%S")

echo "════════════════════════════════════════════════════"
echo "🧪 SWIMMY CI/CD TEST SUITE (Graham V2.0)"
echo "════════════════════════════════════════════════════"
echo "Time: $JST_TIME"
echo ""

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check SBCL
if ! command -v sbcl &> /dev/null; then
    echo -e "${RED}❌ SBCL not found${NC}"
    exit 1
fi

# Ensure .opus directory exists
mkdir -p "$(dirname "$HISTORY_FILE")"

echo "📋 Running test.lisp..."
echo ""

# Run tests and capture output
START_TIME=$(date +%s)
TEST_OUTPUT=$(sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests:run-all-tests)' 2>&1) || true
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# Count results
PASS_COUNT=$(echo "$TEST_OUTPUT" | grep -c "✓\|PASS\|passed" || true)
FAIL_COUNT=$(echo "$TEST_OUTPUT" | grep -c "❌ FAILED" || true)
TOTAL_COUNT=$((PASS_COUNT + FAIL_COUNT))

echo "$TEST_OUTPUT"
echo ""
echo "════════════════════════════════════════════════════"

# Determine result
if [ "$FAIL_COUNT" -gt 0 ]; then
    RESULT="FAIL"
    echo -e "${RED}❌ TESTS FAILED: $FAIL_COUNT failures${NC}"
    EXIT_CODE=1
else
    RESULT="PASS"
    echo -e "${GREEN}✅ ALL TESTS PASSED: $PASS_COUNT tests${NC}"
    EXIT_CODE=0
fi

# Create this run's JSON entry
RUN_JSON=$(cat <<EOF
{
  "timestamp": "$TIMESTAMP",
  "jst_time": "$JST_TIME",
  "result": "$RESULT",
  "passed": $PASS_COUNT,
  "failed": $FAIL_COUNT,
  "total": $TOTAL_COUNT,
  "duration_seconds": $DURATION,
  "git_commit": "$(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')",
  "git_branch": "$(git branch --show-current 2>/dev/null || echo 'unknown')"
}
EOF
)

# Append to history file
if [ -f "$HISTORY_FILE" ]; then
    # Read existing history and append
    TMP_FILE=$(mktemp)
    jq --argjson new "$RUN_JSON" '.runs += [$new] | .last_updated = "'"$TIMESTAMP"'" | .total_runs = (.total_runs + 1) | if $new.result == "PASS" then .pass_streak = .pass_streak + 1 else .pass_streak = 0 end' "$HISTORY_FILE" > "$TMP_FILE" 2>/dev/null || {
        # If jq fails, create simple append
        echo "$RUN_JSON" >> "${HISTORY_FILE}.log"
    }
    mv "$TMP_FILE" "$HISTORY_FILE" 2>/dev/null || true
else
    # Create new history file
    cat > "$HISTORY_FILE" <<EOF
{
  "description": "Graham CI/CD Test History",
  "created": "$TIMESTAMP",
  "last_updated": "$TIMESTAMP",
  "total_runs": 1,
  "pass_streak": $([ "$RESULT" = "PASS" ] && echo 1 || echo 0),
  "runs": [
    $RUN_JSON
  ]
}
EOF
fi

echo ""
echo "📊 History saved to $HISTORY_FILE"
echo "════════════════════════════════════════════════════"

exit $EXIT_CODE
