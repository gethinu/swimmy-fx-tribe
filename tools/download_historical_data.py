#!/usr/bin/env python3
"""
download_historical_data.py

Downloads extensive historical M1 data from MetaTrader5 for Swimmy.
Requires MT5 Python package: pip install MetaTrader5

Usage:
    python3 tools/download_historical_data.py

Data Requirements by Timeframe (for 200 SMA):
┌────────────┬─────────────────────┬──────────────────┐
│ Timeframe  │ M1 Candles Needed   │ Years of Data    │
├────────────┼─────────────────────┼──────────────────┤
│ M15        │ 3,000               │ ~2 days          │
│ H1         │ 12,000              │ ~8 days          │
│ D1 (Daily) │ 288,000             │ ~200 days        │
│ W1 (Weekly)│ 2,016,000           │ ~4 years         │
│ MN (Month) │ 8,640,000           │ ~16 years        │
└────────────┴─────────────────────┴──────────────────┘

This script downloads 4 years of M1 data (~2.1M candles per symbol).
This enables Weekly timeframe analysis with 200+ period indicators.
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
DAYS_TO_DOWNLOAD = 1460  # 4 years of data for Weekly timeframe support


def download_from_mt5():
    """Download data directly from MT5 terminal."""
    if not HAS_MT5:
        return False

    if not mt5.initialize():
        print(f"❌ MT5 initialization failed: {mt5.last_error()}")
        return False

    print(f"✅ MT5 connected: {mt5.terminal_info().name}")

    # Check available symbols and SELECT them in Market Watch
    print("\n📋 Checking symbol availability...")
    final_symbols = []

    for symbol in SYMBOLS:
        info = mt5.symbol_info(symbol)
        selected_symbol = symbol

        if info is None:
            print(f"  ⚠️ {symbol} not found, trying with suffix...")
            found = False
            for suffix in [".a", ".i", ".raw", ".m", ""]:
                alt_symbol = f"{symbol}{suffix}"
                info = mt5.symbol_info(alt_symbol)
                if info:
                    print(f"  ✅ Found: {alt_symbol}")
                    selected_symbol = alt_symbol
                    found = True
                    break
        else:
            print(f"  ✅ {symbol} available")

        if selected_symbol:
            # CRITICAL: Select symbol in Market Watch to ensure data is available
            if not mt5.symbol_select(selected_symbol, True):
                print(f"  ❌ Failed to select {selected_symbol}: {mt5.last_error()}")
            else:
                final_symbols.append(selected_symbol)

    # Download loop
    for symbol in final_symbols:
        print(f"\n📥 Downloading {symbol} M1 data ({DAYS_TO_DOWNLOAD} days)...")

        # Method 1: Date Range with UTC (Most Robust)
        # MT5 usually expects UTC-aligned datetime objects or naive.
        # We use current time in UTC
        utc_to = datetime.now().astimezone(None)
        utc_from = utc_to - timedelta(days=DAYS_TO_DOWNLOAD)

        rates = mt5.copy_rates_range(symbol, mt5.TIMEFRAME_M1, utc_from, utc_to)

        if rates is None or len(rates) == 0:
            print(
                f"  ⚠️ copy_rates_range failed: {mt5.last_error()}. Trying copy_rates_from_pos..."
            )

            # Method 2: Fallback to Count (Last 2M bars)
            rates = mt5.copy_rates_from_pos(symbol, mt5.TIMEFRAME_M1, 0, 2000000)

        if rates is None or len(rates) == 0:
            print(f"❌ Failed to download {symbol}: {mt5.last_error()}")
            continue

        # Standardize filename (e.g. USDJPY_M1.csv even if symbol is USDJPY.a)
        clean_name = symbol.split(".")[0]
        output_file = OUTPUT_DIR / f"{clean_name}_M1.csv"

        # Backup existing file
        if output_file.exists():
            backup_file = OUTPUT_DIR / f"{clean_name}_M1.backup.csv"
            output_file.rename(backup_file)
            print(f"  📦 Backed up existing data to {backup_file.name}")

        with open(output_file, "w") as f:
            for rate in rates:
                # Format: timestamp,open,high,low,close,tick_volume,spread,real_volume
                f.write(
                    f"{int(rate['time'])},{rate['open']},{rate['high']},{rate['low']},{rate['close']},{rate['tick_volume']},{rate['spread']},{rate['real_volume']}\n"
                )

        days_span = (rates[-1]["time"] - rates[0]["time"]) / 86400
        print(
            f"  ✅ Saved {len(rates):,} candles ({days_span:.1f} days) to {output_file.name}"
        )
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

Target: ~2,000,000 candles per symbol (4 years)
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
        # Check standard name first, then variations if needed, but we save as standard
        clean_name = symbol.split(".")[0]
        csv_file = OUTPUT_DIR / f"{clean_name}_M1.csv"

        if csv_file.exists():
            with open(csv_file, "r") as f:
                # Read first and last line efficiently? For now just read lines (warning: large file)
                # Just reading count is faster
                # Or seek?
                try:
                    # quick line count
                    i = 0
                    first_line = None
                    last_line = None
                    for line in f:
                        if i == 0:
                            first_line = line
                        last_line = line
                        i += 1

                    count = i
                    if count > 0 and first_line and last_line:
                        first_ts = int(first_line.split(",")[0])
                        last_ts = int(last_line.split(",")[0])
                        days = (last_ts - first_ts) / 86400
                        print(f"  {clean_name}: {count:,} candles ({days:.1f} days)")
                    else:
                        print(f"  {clean_name}: Empty or invalid file")
                except Exception as e:
                    print(f"  {clean_name}: Error reading file: {e}")
        else:
            print(f"  {clean_name}: No data file")

    print("━" * 50)
    print(f"\n⚠️  Recommended: 2,000,000+ candles per symbol (4 years)")
    print(f"   This enables Weekly timeframe analysis.\n")


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
            # check_current_data() # Skip re-check of large files to save time
            return

    # Fall back to manual instructions
    print_manual_instructions()


if __name__ == "__main__":
    main()
