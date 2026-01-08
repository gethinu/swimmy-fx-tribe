#!/usr/bin/env python3
"""
download_historical_data.py

Downloads extensive historical M1 data from MetaTrader5 for Swimmy.
Requires MT5 Python package: pip install MetaTrader5

Usage:
    python3 tools/download_historical_data.py

This script will download 1 year of M1 data for USDJPY, EURUSD, GBPUSD.
Total candles: ~525,600 per symbol (365 days * 24 hours * 60 minutes)
This is enough for Monthly timeframe analysis (12+ monthly candles).
"""

import os
import sys
from datetime import datetime, timedelta
from pathlib import Path

# Try to import MT5 - it only works on Windows with MT5 installed
try:
    import MetaTrader5 as mt5

    HAS_MT5 = True
except ImportError:
    HAS_MT5 = False
    print("⚠️  MetaTrader5 package not installed. Run: pip install MetaTrader5")
    print("    Note: MT5 Python only works on Windows with MT5 terminal installed.")

SYMBOLS = ["USDJPY", "EURUSD", "GBPUSD"]
OUTPUT_DIR = Path(__file__).parent.parent / "data" / "historical"
DAYS_TO_DOWNLOAD = 365  # 1 year of data


def download_from_mt5():
    """Download data directly from MT5 terminal."""
    if not HAS_MT5:
        return False

    if not mt5.initialize():
        print(f"❌ MT5 initialization failed: {mt5.last_error()}")
        return False

    print(f"✅ MT5 connected: {mt5.terminal_info().name}")

    end_time = datetime.now()
    start_time = end_time - timedelta(days=DAYS_TO_DOWNLOAD)

    for symbol in SYMBOLS:
        print(f"\n📥 Downloading {symbol} M1 data ({DAYS_TO_DOWNLOAD} days)...")

        rates = mt5.copy_rates_range(symbol, mt5.TIMEFRAME_M1, start_time, end_time)

        if rates is None or len(rates) == 0:
            print(f"❌ Failed to download {symbol}: {mt5.last_error()}")
            continue

        output_file = OUTPUT_DIR / f"{symbol}_M1.csv"

        # Backup existing file
        if output_file.exists():
            backup_file = OUTPUT_DIR / f"{symbol}_M1.backup.csv"
            output_file.rename(backup_file)
            print(f"  📦 Backed up existing data to {backup_file.name}")

        with open(output_file, "w") as f:
            for rate in rates:
                # Format: timestamp,open,high,low,close,tick_volume,spread,real_volume
                f.write(
                    f"{int(rate['time'])},{rate['open']},{rate['high']},{rate['low']},{rate['close']},{rate['tick_volume']},{rate['spread']},{rate['real_volume']}\n"
                )

        print(f"  ✅ Saved {len(rates):,} candles to {output_file.name}")
        print(
            f"  📊 Date range: {datetime.fromtimestamp(rates[0]['time'])} to {datetime.fromtimestamp(rates[-1]['time'])}"
        )

    mt5.shutdown()
    return True


def print_manual_instructions():
    """Print instructions for manual data download."""
    print(
        """
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📋 MANUAL DATA DOWNLOAD INSTRUCTIONS
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Since MT5 Python API is not available, please download data manually:

METHOD 1: MT5 History Center (Recommended)
1. Open MetaTrader 5
2. Go to View → Symbols → select symbol (USDJPY, EURUSD, GBPUSD)
3. Click "Bars" tab → set timeframe to M1
4. Click "Request" to load 1 year of data
5. Export via Tools → History Center → Export

METHOD 2: Third-party Data Providers
- Dukascopy: https://www.dukascopy.com/swiss/english/marketwatch/historical/
- HistData: https://www.histdata.com/download-free-forex-data/
- TrueFX: https://www.truefx.com/

Required CSV Format (no header):
timestamp,open,high,low,close,volume,spread,real_volume

Example:
1704067200,150.123,150.234,150.012,150.156,100,2,0

Place files in: {output_dir}
- USDJPY_M1.csv
- EURUSD_M1.csv
- GBPUSD_M1.csv

Target: ~525,000 candles per symbol (1 year of M1 data)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
""".format(
            output_dir=OUTPUT_DIR
        )
    )


def check_current_data():
    """Check current data availability."""
    print("\n📊 Current Data Status:")
    print("━" * 50)

    for symbol in SYMBOLS:
        csv_file = OUTPUT_DIR / f"{symbol}_M1.csv"
        if csv_file.exists():
            with open(csv_file, "r") as f:
                lines = f.readlines()
                count = len(lines)
                if count > 0:
                    first_ts = int(lines[0].split(",")[0])
                    last_ts = int(lines[-1].split(",")[0])
                    days = (last_ts - first_ts) / 86400
                    print(f"  {symbol}: {count:,} candles ({days:.1f} days)")
                else:
                    print(f"  {symbol}: Empty file")
        else:
            print(f"  {symbol}: No data file")

    print("━" * 50)
    print(f"\n⚠️  Recommended: 525,000+ candles per symbol (1 year)")
    print(f"   This enables Monthly timeframe analysis.\n")


def main():
    print("🐟 Swimmy Historical Data Downloader")
    print("=" * 50)

    # Ensure output directory exists
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    # Check current data
    check_current_data()

    # Try MT5 download
    if HAS_MT5:
        print("\n🔄 Attempting MT5 data download...")
        if download_from_mt5():
            print("\n✅ Data download complete!")
            check_current_data()
            return

    # Fall back to manual instructions
    print_manual_instructions()


if __name__ == "__main__":
    main()
