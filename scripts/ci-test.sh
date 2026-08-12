#!/bin/bash
# ci-test.sh - Graham Advisor: CI/CD Test Runner with History
# V2.0: Added JSON output and history tracking

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# ---------------------------------------------------------------------------
# Permanent CI fix (2026-08-11): auto-route SQLite test DBs to a NATIVE Linux FS.
#
# When the repo lives on a Windows drive seen through WSL as /mnt/c (DrvFs), SQLite
# in WAL mode raises "disk I/O error" (SQLITE_IOERR) because DrvFs cannot mmap the
# WAL "-shm" file. This does NOT depend on WSLENV reaching this process: we detect
# DrvFs here and default SWIMMY_TEST_DB_DIR to a native filesystem ourselves, so a
# direct `bash scripts/ci-test.sh` and the PowerShell runbook behave identically.
#
# An explicitly-set SWIMMY_TEST_DB_DIR is always respected (never overridden).
# The Lisp side reads SWIMMY_TEST_DB_DIR; unset on a native-FS checkout => historical
# data/memory/ behaviour (byte-parity), which is fine because native FS has no IOERR.
# ---------------------------------------------------------------------------
fstype_of() { df -T "$1" 2>/dev/null | awk 'NR==2{print $2}'; }
pick_native_dir() {
  # Print the first candidate directory that is NOT on DrvFs/9p (i.e. a real Linux FS).
  local d fstype
  for d in "${TMPDIR:-}" /tmp /dev/shm "$HOME/.cache"; do
    [ -n "$d" ] || continue
    mkdir -p "$d" 2>/dev/null || continue
    fstype="$(fstype_of "$d")"
    case "$fstype" in
      drvfs|9p|v9fs|cifs|"") continue ;;   # DrvFs/9p/unknown -> keep looking
      *) printf '%s\n' "$d"; return 0 ;;
    esac
  done
  return 1
}
if [ -z "${SWIMMY_TEST_DB_DIR:-}" ]; then
  repo_fstype="$(fstype_of "$REPO_ROOT")"
  case "$REPO_ROOT:$repo_fstype" in
    /mnt/*|*:drvfs|*:9p|*:v9fs|*:cifs)
      if native_dir="$(pick_native_dir)"; then
        export SWIMMY_TEST_DB_DIR="$native_dir/swimmy-ci-db"
        mkdir -p "$SWIMMY_TEST_DB_DIR" 2>/dev/null || true
        echo "[CI] repo on '$repo_fstype' ($REPO_ROOT) -> SWIMMY_TEST_DB_DIR=$SWIMMY_TEST_DB_DIR (native FS; avoids DrvFs SQLITE_IOERR)"
      else
        echo "[CI] ⚠️ repo on DrvFs but no native tmp FS found; test DBs stay on data/memory/ (SQLITE_IOERR risk)"
      fi
      ;;
  esac
else
  echo "[CI] SWIMMY_TEST_DB_DIR already set -> $SWIMMY_TEST_DB_DIR (respected)"
fi

# History file
HISTORY_FILE="$SCRIPT_DIR/.opus/ci_history.json"
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
JST_TIME=$(TZ=Asia/Tokyo date +"%Y-%m-%d %H:%M:%S")

extract_counts() {
    local output="$1"
    local results_line
    results_line=$(echo "$output" | grep -E "RESULTS: [0-9]+ passed, [0-9]+ failed" | tail -n 1 || true)
    if [ -n "$results_line" ]; then
        PASS_COUNT=$(echo "$results_line" | awk '{print $3}')
        FAIL_COUNT=$(echo "$results_line" | awk '{print $5}')
    else
        PASS_COUNT=$(echo "$output" | grep -c "✅ PASSED" || true)
        FAIL_COUNT=$(echo "$output" | grep -c "❌ FAILED" || true)
    fi
    TOTAL_COUNT=$((PASS_COUNT + FAIL_COUNT))
}

main() {
cd "$REPO_ROOT"

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
set +e
TEST_OUTPUT=$(sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests:run-all-tests)' 2>&1)
SBCL_EXIT=$?
set -e
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# Count results
extract_counts "$TEST_OUTPUT"

echo "$TEST_OUTPUT"
echo ""
echo "════════════════════════════════════════════════════"

# Determine result
if [ "${SBCL_EXIT:-0}" -ne 0 ]; then
    RESULT="FAIL"
    echo -e "${RED}❌ SBCL exited non-zero: ${SBCL_EXIT}${NC}"
    EXIT_CODE=1
elif [ "$FAIL_COUNT" -gt 0 ]; then
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
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
    main "$@"
fi
