#!/usr/bin/env bash
set -e
D=logs/tribe_diversity_20260714
BIN=target/release/primitive_scan.exe
# 5 disjoint 2-year windows 2015-2024 (epoch bounds)
# w1 2015-16 w2 2017-18 w3 2019-20 w4 2021-22 w5 2023-24
declare -A W=( [w1]="1420070400 1483228800" [w2]="1483228800 1546300800" [w3]="1546300800 1609459200" [w4]="1609459200 1672531200" [w5]="1672531200 1735689600" )
for sym in EURUSD GBPUSD; do
  slp=0.0001
  for w in w1 w2 w3 w4 w5; do
    set -- ${W[$w]}; lo=$1; hi=$2
    $BIN --data data/historical/${sym}_M1.csv --manifest $D/c2_${sym}.json --slippage $slp \
      --oos-start $lo --oos-end $hi --cpcv-start $lo --cpcv-end $hi \
      --out $D/wf_${sym}_${w}.json > $D/wf_${sym}_${w}.log 2>&1
    echo "[$(date +%H:%M:%S)] $sym $w done"
  done
done
echo "WALKFWD DONE"
