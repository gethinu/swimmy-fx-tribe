#!/home/swimmy/swimmy/.venv/bin/python
"""
download_macro_data.py
Phase 19: Global Macro Matrix Data Fetcher
Fetches 14 Key Drivers from Yahoo Finance and saves to CSV.
Updates automatically via Systemd Timer.
"""

import yfinance as yf
import pandas as pd
import os
import shutil

# DATA MAP (Yahoo Finance Tickers)
MACRO_MAP = {
    "DXY": "DX-Y.NYB",
    "US10Y": "^TNX",
    "SPX": "^GSPC",
    "NI225": "^N225",
    "USDJPY": "USDJPY=X",
    "DAX": "^GDAXI",
    "EURUSD": "EURUSD=X",
    "FTSE": "^FTSE",
    "GBPUSD": "GBPUSD=X",
    "WTI": "CL=F",
    "XAU": "GC=F",
    "VIX": "^VIX",
}

DATA_DIR = "/home/swimmy/swimmy/data/macro"


def fetch_data():
    if not os.path.exists(DATA_DIR):
        os.makedirs(DATA_DIR)

    print(f"[MACRO] Fetching Global Drivers to {DATA_DIR}...")

    for name, ticker in MACRO_MAP.items():
        try:
            print(f"  > Downloading {name} ({ticker})...")
            # Fetch 1 year of hourly data
            data = yf.download(ticker, period="1y", interval="1h", progress=False)

            if data.empty:
                print(f"    [WARNING] No data for {name}")
                continue

            # Clean columns
            if isinstance(data.columns, pd.MultiIndex):
                data.columns = data.columns.get_level_values(0)

            data = data[["Open", "High", "Low", "Close", "Volume"]]

            # Atomic Write: Save to .tmp then rename
            outfile = os.path.join(DATA_DIR, f"{name}.csv")
            tmpfile = os.path.join(DATA_DIR, f"{name}.tmp")

            data.to_csv(tmpfile)
            os.replace(tmpfile, outfile)
            print(f"    [OK] Saved {len(data)} rows to {name}.csv (Atomic)")

        except Exception as e:
            print(f"    [ERROR] Failed to fetch {name}: {e}")


if __name__ == "__main__":
    fetch_data()
