#!/usr/bin/env bash
# tests/systemd_reliability_test.sh
# ============================================================================
# P4 (Thread B / reliability) invariants. Pure static assertions over the repo
# unit/config/script files — no systemd required, runs on any host.
# ============================================================================
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root"

fail=0
check() {  # check "<description>" <test-expression...>
    local desc="$1"; shift
    if "$@"; then
        echo "  ok: $desc"
    else
        echo "FAIL: $desc" >&2
        fail=1
    fi
}
has()   { grep -qF -- "$2" "$1"; }          # fixed-string present
hasE()  { grep -qE -- "$2" "$1"; }          # regex present
lacks() { ! grep -qF -- "$2" "$1"; }        # fixed-string absent
# absent as an ACTIVE directive (anchored at col 0; ignores '# ...' comments)
lacksD(){ ! grep -qE -- "^$2" "$1"; }

brain="systemd/swimmy-brain.service"
school="systemd/swimmy-school.service"
dk="systemd/swimmy-data-keeper.service"
alert="systemd/swimmy-alert@.service"

# --- 1. crash-loop discipline on brain + school ---------------------------
for u in "$brain" "$school"; do
    check "$u Restart=on-failure"          has "$u" "Restart=on-failure"
    check "$u no active Restart=always"    lacksD "$u" "Restart=always"
    check "$u RestartSec=30"               has "$u" "RestartSec=30"
    check "$u StartLimitIntervalSec=600"   has "$u" "StartLimitIntervalSec=600"
    check "$u StartLimitBurst=5"           has "$u" "StartLimitBurst=5"
    check "$u OnFailure alert wired"       has "$u" "OnFailure=swimmy-alert@%n.service"
done

# --- 2. heap centralization + split var names -----------------------------
check "config/heap.env brain=3072"   has "config/heap.env" "SWIMMY_BRAIN_HEAP_MB=3072"
check "config/heap.env school=4096"  has "config/heap.env" "SWIMMY_SCHOOL_HEAP_MB=4096"
check "brain loads heap.env"         has "$brain"  "EnvironmentFile=-/etc/swimmy/heap.env"
check "school loads heap.env"        has "$school" "EnvironmentFile=-/etc/swimmy/heap.env"
check "brain default BRAIN_HEAP_MB"  has "$brain"  "Environment=SWIMMY_BRAIN_HEAP_MB=3072"
check "school default SCHOOL_HEAP_MB" has "$school" "Environment=SWIMMY_SCHOOL_HEAP_MB=4096"
check "school ExecStart uses SCHOOL_HEAP_MB" has "$school" 'dynamic-space-size ${SWIMMY_SCHOOL_HEAP_MB}'
# split names: neither brain nor school still pins the legacy shared var
check "brain drops legacy heap var"  lacks "$brain"  "Environment=SWIMMY_SBCL_DYNAMIC_SPACE_MB"
check "school drops legacy heap var" lacks "$school" "Environment=SWIMMY_SBCL_DYNAMIC_SPACE_MB"
check "run.sh prefers BRAIN_HEAP_MB" has "run.sh" 'SWIMMY_BRAIN_HEAP_MB:-'

# --- 3. data-keeper memory cap --------------------------------------------
check "data-keeper MemoryMax=3G"     has "$dk" "MemoryMax=3G"

# --- 4. alerter cannot fail-loop ------------------------------------------
check "alert unit is oneshot"        has "$alert" "Type=oneshot"
check "alert unit no active Restart="   lacksD "$alert" "Restart="
check "alert unit no active OnFailure=" lacksD "$alert" "OnFailure="
check "alert script exits 0"         has "tools/ops/alert_on_failure.sh" "exit 0"
check "alert script executable"      test -x "tools/ops/alert_on_failure.sh"

# --- 5. EOL protection for ops files --------------------------------------
check ".gitattributes pins *.sh lf"      hasE ".gitattributes" '^\*\.sh[[:space:]]+text eol=lf'
check ".gitattributes pins *.service lf" hasE ".gitattributes" '^\*\.service text eol=lf'

if [ "$fail" -ne 0 ]; then
    echo "systemd_reliability_test: FAILED" >&2
    exit 1
fi
echo "systemd_reliability_test: all P4 invariants hold"
