#!/usr/bin/env python3
"""Convert bundle canonical MT5 D1 CSV -> tribe primitive_scan CSV.

Bundle format: time_utc,open,high,low,close,tick_volume,spread_points,real_volume
  time_utc = ISO8601 e.g. 2003-01-01T00:00:00+00:00
Tribe format: timestamp,open,high,low,close,volume   (timestamp = Unix seconds, UTC)

Faithful passthrough of O/H/L/C. volume forced to 1.0 (tribe convention;
scorer does not use volume for bb/donchian). Verifies strict-monotonic timestamps,
no dups, and prints the exact bars falling inside the tribe OOS window (2021-2025).
"""
import csv, sys
from datetime import datetime, timezone

TS_2021 = 1_609_459_200  # 2021-01-01 UTC (tribe OOS start)
TS_2025 = 1_735_689_600  # 2025-01-01 UTC (tribe OOS end)

def convert(src, dst):
    rows = []
    with open(src, newline="") as f:
        r = csv.DictReader(f)
        for rec in r:
            iso = rec["time_utc"]
            dt = datetime.fromisoformat(iso)
            if dt.tzinfo is None:
                dt = dt.replace(tzinfo=timezone.utc)
            ts = int(dt.timestamp())
            rows.append((ts, float(rec["open"]), float(rec["high"]),
                         float(rec["low"]), float(rec["close"])))
    rows.sort(key=lambda x: x[0])
    # verify strict monotonic + dedup
    dups = 0
    clean = []
    prev = None
    for row in rows:
        if prev is not None and row[0] == prev:
            dups += 1
            continue
        if prev is not None and row[0] < prev:
            print(f"  !! non-monotonic at ts={row[0]}", file=sys.stderr)
        clean.append(row)
        prev = row[0]
    with open(dst, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["timestamp", "open", "high", "low", "close", "volume"])
        for ts, o, h, l, c in clean:
            w.writerow([ts, f"{o:.5f}", f"{h:.5f}", f"{l:.5f}", f"{c:.5f}", "1.0"])
    lo, hi = clean[0][0], clean[-1][0]
    oos = [r for r in clean if TS_2021 <= r[0] < TS_2025]
    d0 = datetime.fromtimestamp(lo, timezone.utc).date()
    d1 = datetime.fromtimestamp(hi, timezone.utc).date()
    print(f"{dst}")
    print(f"  bars={len(clean)}  dups_dropped={dups}  range={d0}..{d1}")
    print(f"  full_span_years={(hi-lo)/86400/365.25:.1f}")
    print(f"  OOS[2021..2025) bars={len(oos)}  (tribe OOS window)")
    print(f"  ts_lo={lo} ts_hi={hi}")

if __name__ == "__main__":
    convert(sys.argv[1], sys.argv[2])
