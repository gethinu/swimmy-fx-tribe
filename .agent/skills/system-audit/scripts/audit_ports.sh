#!/bin/bash
# Audit Swimmy System Ports

EXPECTED_PORTS=("5555" "5556" "5557" "5559")
FORBIDDEN_PORTS=("5580" "5558")

echo "🔍 Auditing ZMQ Ports..."
SS_OUTPUT=$(ss -tulnpe)

ALL_OK=true

for port in "${EXPECTED_PORTS[@]}"; do
    if echo "$SS_OUTPUT" | grep -q ":$port "; then
        echo "✅ Port $port is LISTENING (OK)"
    else
        echo "❌ Port $port is NOT LISTENING (MISSING)"
        ALL_OK=false
    fi
done

for port in "${FORBIDDEN_PORTS[@]}"; do
    if echo "$SS_OUTPUT" | grep -q ":$port "; then
        echo "🚨 Port $port is LISTENING (FORBIDDEN - Zombie Process?)"
        ALL_OK=false
    else
        echo "✅ Port $port is closed (OK)"
    fi
done

if $ALL_OK; then
    echo "✨ Port Audit Passed."
    exit 0
else
    echo "⚠️ Port Audit Failed."
    exit 1
fi
