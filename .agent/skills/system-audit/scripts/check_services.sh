#!/bin/bash
# Audit Swimmy System Services

SERVICES=("swimmy-brain" "swimmy-guardian" "swimmy-data-keeper")

echo "🔍 Auditing System Services..."
ALL_OK=true

for service in "${SERVICES[@]}"; do
    if systemctl --user is-active --quiet "$service"; then
        echo "✅ $service is ACTIVE"
    else
        echo "❌ $service is INACTIVE"
        ALL_OK=false
    fi
done

# Check if processes are actually running (double check)
if pgrep -f "sbcl" > /dev/null; then
    echo "✅ SBCL (Brain) process found"
else
    echo "❌ SBCL process MISSING"
    ALL_OK=false
fi

if pgrep -f "guardian" > /dev/null; then
    echo "✅ Guardian process found"
else
    echo "❌ Guardian process MISSING"
    ALL_OK=false
fi

if $ALL_OK; then
    echo "✨ Service Audit Passed."
    exit 0
else
    echo "⚠️ Service Audit Failed."
    exit 1
fi
