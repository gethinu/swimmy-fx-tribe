#!/bin/bash

echo "♾️  STARTING HYPER-TIME EVOLUTION LOOP (Naval/Musk Protocol)"
echo "    Target: Continuous optimization until S-Rank Domination."

while true; do
    echo "----------------------------------------------------------------"
    echo "🕒 Cycle Start: $(date)"
    
    # Run the cycle
    ./tools/run_evolution_cycle.py
    
    EXIT_CODE=$?
    
    if [ $EXIT_CODE -ne 0 ]; then
        echo "⚠️  Cycle failed with code $EXIT_CODE. Resting for 10s..."
        sleep 10
    else
        echo "✅ Cycle Complete. Immediate Restart."
    fi
    
    # No sleep (Marginal cost zero)
    # Just a breath.
    sleep 1
done
