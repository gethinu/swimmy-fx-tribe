#!/usr/bin/env python3
"""
restore_m5_from_m1.py - Restore corrupted M5 data using M1 source (Standard Library Version)
"""
import csv
import sys
import os
from datetime import datetime

DATA_DIR = "data/historical"
SYMBOL = "USDJPY"
M1_PATH = os.path.join(DATA_DIR, f"{SYMBOL}_M1.csv")
M5_TARGET = os.path.join(DATA_DIR, f"{SYMBOL}_M5.csv")


def restore_m5():
    print(f"📖 Loading M1 data from {M1_PATH} stream...")
    if not os.path.exists(M1_PATH):
        print(f"❌ Error: Source file {M1_PATH} not found!")
        sys.exit(1)

    try:
        with open(M1_PATH, "r") as infile, open(M5_TARGET, "w", newline="") as outfile:
            reader = csv.DictReader(infile)
            writer = csv.writer(outfile)
            writer.writerow(["timestamp", "open", "high", "low", "close", "volume"])

            # State for aggregation
            current_bucket_ts = None
            agg = None
            count = 0

            for row in reader:
                try:
                    ts = int(row["timestamp"])
                    o = float(row["open"])
                    h = float(row["high"])
                    l = float(row["low"])
                    c = float(row["close"])
                    # FIX: Handle "1.0" string as float before int
                    v = int(float(row["volume"]))
                except ValueError:
                    continue  # Skip bad rows

                # Calculate bucket start (5 minutes = 300 seconds)
                bucket_ts = (ts // 300) * 300

                # Check if we moved to a new bucket
                if current_bucket_ts is not None and bucket_ts != current_bucket_ts:
                    # Write previous bucket
                    writer.writerow(
                        [
                            current_bucket_ts,
                            agg["open"],
                            agg["high"],
                            agg["low"],
                            agg["close"],
                            agg["volume"],
                        ]
                    )
                    agg = None

                # Initialize or update bucket
                if agg is None:
                    current_bucket_ts = bucket_ts
                    agg = {"open": o, "high": h, "low": l, "close": c, "volume": v}
                else:
                    agg["high"] = max(agg["high"], h)
                    agg["low"] = min(agg["low"], l)
                    agg["close"] = c
                    agg["volume"] += v

                count += 1
                if count % 100000 == 0:
                    print(f"Processed {count} candles...", end="\r")

            # Write critical last bucket
            if agg is not None:
                writer.writerow(
                    [
                        current_bucket_ts,
                        agg["open"],
                        agg["high"],
                        agg["low"],
                        agg["close"],
                        agg["volume"],
                    ]
                )

        print(f"\n✅ M5 Restoration Complete. Processed {count} M1 records.")
        print(f"💾 Saved to {M5_TARGET}")

    except Exception as e:
        print(f"\n❌ Restoration Failed: {e}")
        sys.exit(1)


if __name__ == "__main__":
    restore_m5()
