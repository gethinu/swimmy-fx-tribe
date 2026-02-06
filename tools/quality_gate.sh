#!/bin/bash
set -e

echo "🛡️ Running Quality Gate (Tests + Integrity + SRP)..."

# 1. Integration Tests (Lisp)
echo "🧪 Running Lisp Tests..."
sbcl --script tests/test_runner.lisp

# 1.1 Regime Hard Lock (Musk v49.4)
echo "🔒 Verifying Regime Hard Lock..."
sbcl --script src/lisp/tests/test-regime-lock.lisp

# 2. Unit Tests (Rust)
if [ -d "guardian" ]; then
    echo "🦀 Running Rust Tests..."
    cd guardian && cargo test --release && cd ..
fi

# 3. System Integrity
if [ -f "./tools/check_integrity.sh" ]; then
    echo "🔍 Running Integrity Checks..."
    ./tools/check_integrity.sh
fi

# 4. SRP Check
if [ -f "tools/check_srp.py" ]; then
    echo "🦅 Running SRP Check..."
    python3 tools/check_srp.py
fi

echo "✅ Quality Gate PASSED - Ready for deployment"
