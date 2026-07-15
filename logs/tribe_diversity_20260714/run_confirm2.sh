#!/usr/bin/env bash
set -e
D=logs/tribe_diversity_20260714
BIN=target/release/primitive_scan.exe
sel() { $BIN --data data/historical/$1_M1.csv --manifest $D/c2_$1.json --slippage $2 --out $D/c2_sel_$1.json > $D/c2_sel_$1.log 2>&1; }
hol() { $BIN --data data/historical/$1_M1.csv --manifest $D/c2_$1.json --slippage $2 \
    --oos-start 1420070400 --oos-end 1609459200 --cpcv-start 1420070400 --cpcv-end 1609459200 \
    --out $D/c2_hol_$1.json > $D/c2_hol_$1.log 2>&1; }
for pair in "EURUSD 0.0001" "GBPUSD 0.0001" "EURJPY 0.01"; do
  set -- $pair; S=$1; SLP=$2
  echo "[$(date +%H:%M:%S)] $S sel..."; sel $S $SLP
  echo "[$(date +%H:%M:%S)] $S hol..."; hol $S $SLP
done
echo "[$(date +%H:%M:%S)] CONFIRM2 DONE"
