#!/bin/bash
set -e

echo "🧹 Cleaning up old processes..."
pkill -9 sbcl 2>/dev/null || true
pkill -9 guardian 2>/dev/null || true
fuser -k 5555/tcp 2>/dev/null || true
fuser -k 5556/tcp 2>/dev/null || true
fuser -k 5557/tcp 2>/dev/null || true
fuser -k 5557/tcp 2>/dev/null || true
fuser -k 5560/tcp 2>/dev/null || true
pkill -9 -f "strategy_hunter.py" 2>/dev/null || true
pkill -9 -f "pending_manager.py" 2>/dev/null || true

truncate -s 0 /tmp/guardian.log

echo "🚀 Starting Guardian (Release) for Benchmarks..."
if [ -f "./guardian/target/release/guardian" ]; then
    nohup ./guardian/target/release/guardian > /tmp/guardian.log 2>&1 &
else
    echo "⚠️ Guardian binary not found. Building..."
    cd guardian && cargo build --release && cd ..
    nohup ./guardian/target/release/guardian > /tmp/guardian.log 2>&1 &
fi

sleep 2

echo "📊 Running Benchmarks..."
if [ -f "./ci-test.sh" ]; then
    ./ci-test.sh
fi

echo "🧪 Running Lisp Benchmarks..."
sbcl --noinform --load brain.lisp --load src/lisp/benchmark.lisp --eval '(swimmy.school:run-all-benchmarks)' --quit

echo "✅ Benchmarks Complete"
