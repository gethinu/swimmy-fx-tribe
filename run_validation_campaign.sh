#!/bin/bash
# Deep Validation Campaign Runner

echo "🚀 Starting Deep Validation Campaign (Overnight Mode)..."
echo "Date: $(date)"

# Log everything to file
LOGFILE="logs/validation_campaign_$(date +%Y%m%d_%H%M%S).log"
mkdir -p logs
echo "📝 Logging to $LOGFILE"
exec > >(tee -a $LOGFILE) 2>&1

# Strategy to test
STRATEGY="stress_test_strategy.json"

# Function to run test
run_test() {
    SYMBOL=$1
    TF=$2
    echo "---------------------------------------------------"
    echo "🧪 Testing $SYMBOL $TF..."
    echo "---------------------------------------------------"
    python3 tools/alert.py "⏳ Started Stress Test: $SYMBOL $TF" --type info
    
    python3 tools/stress_test.py --symbol $SYMBOL --timeframe $TF --strategy $STRATEGY
    
    if [ $? -eq 0 ]; then
        echo "✅ $SYMBOL $TF passed."
        python3 tools/alert.py "✅ Passed: $SYMBOL $TF" --type success
    else
        echo "❌ $SYMBOL $TF failed!"
        python3 tools/alert.py "❌ Failed: $SYMBOL $TF" --type error
    fi
}

# Run Tests
run_test "USDJPY" "M5"
run_test "USDJPY" "M1"  # Deepest Validation (~18 years raw)

run_test "EURUSD" "M5"
run_test "EURUSD" "M1"

run_test "GBPUSD" "M5"
run_test "GBPUSD" "M1"

# Optional: Run H1 tests too?
# run_test "USDJPY" "H1"
# run_test "EURUSD" "H1"
# run_test "GBPUSD" "H1"

echo "---------------------------------------------------"
echo "🎉 Campaign Complete."
echo "Date: $(date)"
