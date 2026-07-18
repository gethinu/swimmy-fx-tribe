#!/usr/bin/env python3
"""Map CPCV block index -> calendar span (blocks are equal INDEX slices of the full series)."""
import csv, sys
from datetime import datetime, timezone
CPCV_BLOCKS = 10
def spans(path):
    ts = []
    with open(path) as f:
        r = csv.reader(f); next(r)
        for row in r: ts.append(int(row[0]))
    n = len(ts)
    print(f"{path}: n={n}")
    for b in range(CPCV_BLOCKS):
        s = b*n//CPCV_BLOCKS
        e = n if b==CPCV_BLOCKS-1 else (b+1)*n//CPCV_BLOCKS
        d0 = datetime.fromtimestamp(ts[s], timezone.utc).date()
        d1 = datetime.fromtimestamp(ts[e-1], timezone.utc).date()
        print(f"  block {b}: {d0} .. {d1}  ({e-s} bars)")
for p in sys.argv[1:]: spans(p)
