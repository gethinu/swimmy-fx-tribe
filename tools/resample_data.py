import csv
import os
import sys
from datetime import datetime, timezone
from pathlib import Path


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent

BASE_DIR = str(resolve_base_dir())
DATA_DIR = os.path.join(BASE_DIR, "data", "historical")
SYMBOLS = ["USDJPY", "EURUSD", "GBPUSD"]

TF_SECONDS = {
    "M1": 60,  # Generate standardized M1
    "M5": 300,
    "M15": 900,
    "M30": 1800,
    "H1": 3600,
    "H4": 14400,
    "D1": 86400,
    "W1": 604800,
}


def parse_mt5_row(row):
    # Row: [DATE, TIME, OPEN, HIGH, LOW, CLOSE, T_VOL, VOL, SPREAD]
    # Date: 2006.01.02
    # Time: 00:02:00
    try:
        dt_str = f"{row[0]} {row[1]}"
        dt = datetime.strptime(dt_str, "%Y.%m.%d %H:%M:%S")
        ts = int(dt.replace(tzinfo=timezone.utc).timestamp())

        return {
            "t": ts,
            "o": float(row[2]),
            "h": float(row[3]),
            "l": float(row[4]),
            "c": float(row[5]),
            "v": float(row[6]),  # Tick Volume
        }
    except Exception as e:
        return None


def parse_guardian_row(row):
    # Row: [timestamp, open, high, low, close, volume]
    try:
        return {
            "t": int(float(row[0])),
            "o": float(row[1]),
            "h": float(row[2]),
            "l": float(row[3]),
            "c": float(row[4]),
            "v": float(row[5]),
        }
    except:
        return None


def resample_symbol(symbol):
    m1_path = os.path.join(DATA_DIR, f"{symbol}_M1.csv")
    if not os.path.exists(m1_path):
        print(f"⚠️ {symbol} M1 file not found: {m1_path}")
        return

    print(f"🔄 Processing {symbol} from M1...")
    candles = []

    # Detect delimiter
    delimiter = ","
    with open(m1_path, "r") as f:
        line = f.readline()
        if "\t" in line:
            delimiter = "\t"

    print(f"  Delimiter detected: '{'TAB' if delimiter == '\t' else ','}'")

    with open(m1_path, "r") as f:
        reader = csv.reader(f, delimiter=delimiter)
        header = next(reader, None)

        # Check format based on header or first row
        is_mt5_format = False
        if header and len(header) > 1 and ("<DATE>" in header[0] or "." in header[0]):
            is_mt5_format = True

        # Rewrite header logic
        # If header line looks like data, reset file pointer? No, complexity.
        # Just use try/parse.

        # Process first row if it was data
        if not is_mt5_format and header:
            c = parse_guardian_row(header)
            if c:
                candles.append(c)
        elif is_mt5_format and header and "." in header[0] and "<" not in header[0]:
            # It was data
            c = parse_mt5_row(header)
            if c:
                candles.append(c)

        for row in reader:
            if not row:
                continue
            c = None
            if is_mt5_format:
                c = parse_mt5_row(row)
            else:
                c = parse_guardian_row(row)

            if c:
                candles.append(c)

    print(f"  Loaded {len(candles)} M1 candles.")
    if len(candles) == 0:
        return

    candles.sort(key=lambda x: x["t"])

    # Generate ALL timeframes including cleaned M1
    for tf_name, seconds in TF_SECONDS.items():
        print(f"  generating {tf_name} ({seconds}s)...")

        # Optimization: for M1, if input is already clean M1, we can just write it out?
        # But here we standardize format.

        resampled = []
        current_bucket = None
        bucket_open = 0
        bucket_high = 0
        bucket_low = 0
        bucket_close = 0
        bucket_vol = 0

        for c in candles:
            ts = c["t"]
            bucket_ts = (ts // seconds) * seconds

            if current_bucket is None:
                current_bucket = bucket_ts
                bucket_open = c["o"]
                bucket_high = c["h"]
                bucket_low = c["l"]
                bucket_close = c["c"]
                bucket_vol = c["v"]
            elif bucket_ts == current_bucket:
                if c["h"] > bucket_high:
                    bucket_high = c["h"]
                if c["l"] < bucket_low:
                    bucket_low = c["l"]
                bucket_close = c["c"]
                bucket_vol += c["v"]
            else:
                resampled.append(
                    {
                        "t": current_bucket,
                        "o": bucket_open,
                        "h": bucket_high,
                        "l": bucket_low,
                        "c": bucket_close,
                        "v": bucket_vol,
                    }
                )
                current_bucket = bucket_ts
                bucket_open = c["o"]
                bucket_high = c["h"]
                bucket_low = c["l"]
                bucket_close = c["c"]
                bucket_vol = c["v"]

        if current_bucket is not None:
            resampled.append(
                {
                    "t": current_bucket,
                    "o": bucket_open,
                    "h": bucket_high,
                    "l": bucket_low,
                    "c": bucket_close,
                    "v": bucket_vol,
                }
            )

        out_path = os.path.join(DATA_DIR, f"{symbol}_{tf_name}.csv")
        try:
            with open(out_path, "w") as f:
                f.write("timestamp,open,high,low,close,volume\n")
                for r in resampled:
                    f.write(f"{r['t']},{r['o']},{r['h']},{r['l']},{r['c']},{r['v']}\n")
            print(f"  ✅ Saved {out_path} ({len(resampled)} bars)")
        except Exception as e:
            print(f"❌ Error writing {out_path}: {e}")


def main():
    print("🛠️ Swimmy Data Resampler (MT5 Support)")
    for s in SYMBOLS:
        resample_symbol(s)


if __name__ == "__main__":
    main()
