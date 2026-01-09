#!/usr/bin/env python3
"""
convert_mt5_csv.py - Convert MT5 exported CSV to Data Keeper format
===================================================================
MT5 Format (Tab-separated):
  <DATE>  <TIME>  <OPEN>  <HIGH>  <LOW>   <CLOSE> <TICKVOL> <VOL> <SPREAD>
  2025.10.01      08:14:00        147.602 ...

Target Format (Comma-separated):
  timestamp,open,high,low,close,volume
  1727772840,147.602,147.602,147.574,147.599,42
"""

import os
import sys
import re
from datetime import datetime

DATA_DIR = os.path.join(os.path.dirname(__file__), "..", "data", "historical")


def parse_mt5_datetime(date_str, time_str):
    """Parse MT5 datetime format to Unix timestamp."""
    # Format: 2025.10.01  08:14:00
    dt_str = f"{date_str} {time_str}"
    try:
        dt = datetime.strptime(dt_str, "%Y.%m.%d %H:%M:%S")
        return int(dt.timestamp())
    except:
        return None


def convert_file(filepath):
    """Convert a single MT5 CSV file to Data Keeper format."""
    lines = []
    converted = 0
    skipped = 0

    with open(filepath, "r", encoding="utf-8") as f:
        for line in f:
            # Skip header
            if line.startswith("<DATE>") or "DATE" in line:
                continue

            # Split by tabs or multiple spaces
            parts = re.split(r"\t+|\s{2,}", line.strip())

            if len(parts) < 6:
                skipped += 1
                continue

            # Determine format based on number of parts
            # M1/H1: DATE, TIME, OPEN, HIGH, LOW, CLOSE, TICKVOL, VOL, SPREAD (9 parts)
            # D1/W1: DATE, OPEN, HIGH, LOW, CLOSE, TICKVOL, VOL, SPREAD (8 parts, no TIME)

            if len(parts) >= 9 or ":" in parts[1]:
                # Has TIME column
                date_str = parts[0]
                time_str = parts[1]
                open_p = parts[2]
                high_p = parts[3]
                low_p = parts[4]
                close_p = parts[5]
                tick_vol = parts[6]
                ts = parse_mt5_datetime(date_str, time_str)
            else:
                # No TIME column (D1/W1)
                date_str = parts[0]
                time_str = "00:00:00"
                open_p = parts[1]
                high_p = parts[2]
                low_p = parts[3]
                close_p = parts[4]
                tick_vol = parts[5]
                ts = parse_mt5_datetime(date_str, time_str)

            if ts is None:
                skipped += 1
                continue

            # Create output line
            out_line = f"{ts},{open_p},{high_p},{low_p},{close_p},{tick_vol}"
            lines.append(out_line)
            converted += 1

    # Write back
    with open(filepath, "w", encoding="utf-8") as f:
        f.write("timestamp,open,high,low,close,volume\n")
        for line in lines:
            f.write(line + "\n")

    return converted, skipped


def main():
    print("🔄 Converting MT5 CSV files to Data Keeper format...")

    files = [f for f in os.listdir(DATA_DIR) if f.endswith(".csv")]

    for filename in files:
        filepath = os.path.join(DATA_DIR, filename)

        # Check if already converted (first line is header)
        with open(filepath, "r") as f:
            first_line = f.readline().strip()
            if first_line.startswith("timestamp,"):
                print(f"  ✓ {filename} - Already converted, skipping")
                continue

        converted, skipped = convert_file(filepath)
        print(f"  ✓ {filename} - Converted {converted} lines (skipped {skipped})")

    print("✅ Conversion complete!")


if __name__ == "__main__":
    main()
