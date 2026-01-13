#!/bin/bash
# Resume Validation Campaign (Recovery Mode)

LOGFILE="logs/resume_campaign_$(date +%Y%m%d_%H%M%S).log"
echo "📝 Resuming Campaign. Logging to $LOGFILE"
exec > >(tee -a $LOGFILE) 2>&1

STRATEGY="stress_test_strategy.json"

run_test() {
    SYMBOL=$1
    TF=$2
    echo "---------------------------------------------------"
    echo "🧪 Testing $SYMBOL $TF..."
    echo "---------------------------------------------------"
    python3 tools/alert.py "⏳ Resuming Stress Test: $SYMBOL $TF" --type info
    
    # Timeout 4h per test to prevent infinite hangs
    timeout 4h python3 tools/stress_test.py --symbol $SYMBOL --timeframe $TF --strategy $STRATEGY
    
    if [ $? -eq 0 ]; then
        echo "✅ $SYMBOL $TF passed."
        python3 tools/alert.py "✅ Passed: $SYMBOL $TF" --type success
    else
        echo "❌ $SYMBOL $TF failed or timed out!"
        python3 tools/alert.py "❌ Failed: $SYMBOL $TF" --type error
    fi
}

# Pending Tests
# run_test "USDJPY" "M1"  # Skipped per V7 Strategy (M5 priority)
run_test "EURUSD" "M5"
# run_test "EURUSD" "M1"
run_test "GBPUSD" "M5"
# run_test "GBPUSD" "M1"

echo "🎉 Resume Complete."
