#!/usr/bin/env bash
set -e
D=logs/tribe_diversity_20260714
BIN=target/release/kill_oos_cpcv.exe
# TS: 2015=1420070400 2021=1609459200 2025=1735689600
sel() { # selection windows (default: OOS 2021-24, CPCV full)
  $BIN --data data/historical/$1_M1.csv --manifest $D/ex_$1.json --slippage $2 \
    --out $D/ex_sel_$1.json > $D/ex_sel_$1.log 2>&1
}
hol() { # holdout: OOS + CPCV within 2015-2020
  $BIN --data data/historical/$1_M1.csv --manifest $D/ex_$1.json --slippage $2 \
    --is-start 1609459200 --is-end 1735689600 \
    --oos-start 1420070400 --oos-end 1609459200 --cpcv-start 1420070400 --cpcv-end 1609459200 \
    --out $D/ex_hol_$1.json > $D/ex_hol_$1.log 2>&1
}
for pair in "USDJPY 0.01" "EURJPY 0.01" "EURUSD 0.0001" "GBPUSD 0.0001"; do
  set -- $pair; S=$1; SLP=$2
  echo "[$(date +%H:%M:%S)] $S selection..."; sel $S $SLP
  echo "[$(date +%H:%M:%S)] $S holdout...";   hol $S $SLP
done
echo "[$(date +%H:%M:%S)] ALL RUNS DONE"
