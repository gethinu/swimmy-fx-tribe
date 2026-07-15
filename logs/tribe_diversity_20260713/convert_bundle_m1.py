#!/usr/bin/env python3
"""Convert mt5_Bundle ohlc_m1 yearly CSVs -> swimmy harness schema (single file).

Bundle schema : datetime,open,high,low,close     (datetime is naive UTC string)
Harness schema: timestamp,open,high,low,close,volume   (unix epoch secs, vol=1.0)

The transform is fixed and lossless: datetime is read as naive UTC and mapped to
epoch via calendar.timegm; volume is a constant 1.0 (the existing USDJPY_M1.csv
uses vol=1.0 everywhere, and guardian's ratio metrics are volume-invariant).

VALIDATION: running this on `usdjpy` for 2015-2024 must byte-reproduce the
existing data/historical/USDJPY_M1.csv (which was built from the same source).
Use --validate-against to assert that before trusting new symbols.

Offline only. Reads the bundle read-only; writes only the --out file.
"""
from __future__ import annotations
import argparse, calendar, glob, os, sys, time

BUNDLE = "C:/Repos/mt5_Bundle-of-edges/.claude/worktrees/determined-newton-589bac/data/ohlc_m1"

def yearly_files(sym: str, y0: int, y1: int):
    out = []
    for y in range(y0, y1 + 1):
        # files are named <sym>_m1_YYYY0101_YYYY1231.csv
        hits = sorted(glob.glob(os.path.join(BUNDLE, f"{sym}_m1_{y}0101_{y}1231.csv")))
        out.extend(hits)
    return out

def convert(sym: str, y0: int, y1: int, out_path: str) -> tuple[int, int, int]:
    files = yearly_files(sym, y0, y1)
    if not files:
        print(f"!! no bundle files for {sym} {y0}-{y1} under {BUNDLE}", file=sys.stderr)
        sys.exit(2)
    n_rows = 0
    last_ts = None
    n_out_of_order = 0
    t0 = time.time()
    with open(out_path, "w", newline="") as w:
        w.write("timestamp,open,high,low,close,volume\n")
        for f in files:
            with open(f, "r") as r:
                header = r.readline()  # datetime,open,high,low,close
                if not header.lower().startswith("datetime,open,high,low,close"):
                    print(f"!! unexpected header in {f}: {header!r}", file=sys.stderr)
                    sys.exit(2)
                for line in r:
                    line = line.rstrip("\n").rstrip("\r")
                    if not line:
                        continue
                    dt, o, h, lo, c = line.split(",")
                    # naive UTC -> epoch seconds
                    st = time.strptime(dt, "%Y-%m-%d %H:%M:%S")
                    ts = calendar.timegm(st)
                    if last_ts is not None and ts <= last_ts:
                        n_out_of_order += 1
                    last_ts = ts
                    w.write(f"{ts},{o},{h},{lo},{c},1.0\n")
                    n_rows += 1
    dt = time.time() - t0
    print(f"{sym}: wrote {n_rows} rows from {len(files)} files -> {out_path} ({dt:.1f}s)")
    if n_out_of_order:
        print(f"   WARNING: {n_out_of_order} rows non-increasing timestamp (dup/gap)")
    return n_rows, len(files), n_out_of_order

def validate(out_path: str, ref_path: str, sample: int = 500000):
    """Assert the produced file matches a reference line-for-line (streamed)."""
    import itertools
    n = 0
    mism = 0
    with open(out_path) as a, open(ref_path) as b:
        for la, lb in itertools.zip_longest(a, b):
            if la != lb:
                mism += 1
                if mism <= 5:
                    print(f"   MISMATCH line {n}: got {la!r} ref {lb!r}")
            n += 1
    if mism == 0:
        print(f"   VALIDATION PASS: {n} lines identical to {ref_path}")
        return True
    print(f"   VALIDATION FAIL: {mism}/{n} lines differ from {ref_path}")
    return False

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--symbol", required=True, help="bundle symbol, lowercase e.g. eurusd")
    ap.add_argument("--y0", type=int, default=2015)
    ap.add_argument("--y1", type=int, default=2024)
    ap.add_argument("--out", required=True)
    ap.add_argument("--validate-against", default=None,
                    help="reference CSV to byte-compare (e.g. data/historical/USDJPY_M1.csv)")
    args = ap.parse_args()
    convert(args.symbol, args.y0, args.y1, args.out)
    if args.validate_against:
        ok = validate(args.out, args.validate_against)
        sys.exit(0 if ok else 1)

if __name__ == "__main__":
    main()
