#!/bin/bash
# Extended Validation Campaign (H1, D1, W1, MN)

LOGFILE="logs/extended_campaign_$(date +%Y%m%d_%H%M%S).log"
echo "📝 Extended Campaign (H1+). Logging to $LOGFILE"
exec > >(tee -a $LOGFILE) 2>&1

STRATEGY="stress_test_strategy.json"

run_test() {
    SYMBOL=$1
    TF=$2
    echo "---------------------------------------------------"
    echo "🧪 Testing $SYMBOL $TF..."
    echo "---------------------------------------------------"
    python3 tools/alert.py "⏳ Extended Stress Test: $SYMBOL $TF" --type info
    
    # Timeout 1h per test (data is smaller)
    timeout 1h python3 tools/stress_test.py --symbol $SYMBOL --timeframe $TF --strategy $STRATEGY
    
    if [ $? -eq 0 ]; then
        echo "✅ $SYMBOL $TF passed."
        python3 tools/alert.py "✅ Passed: $SYMBOL $TF" --type success
    else
        echo "❌ $SYMBOL $TF failed or timed out!"
        python3 tools/alert.py "❌ Failed: $SYMBOL $TF" --type error
    fi
}

# Wait for M5/M1 tests to likely finish or just run low priority?
# We'll just run them.

# USDJPY
run_test "USDJPY" "H1"
run_test "USDJPY" "D1"
run_test "USDJPY" "W1"
run_test "USDJPY" "MN"

# EURUSD
run_test "EURUSD" "H1"
run_test "EURUSD" "D1"
run_test "EURUSD" "W1"
run_test "EURUSD" "MN"

# GBPUSD
run_test "GBPUSD" "H1"
run_test "GBPUSD" "D1"
run_test "GBPUSD" "W1"
run_test "GBPUSD" "MN"

echo "🎉 Extended Campaign Complete."
