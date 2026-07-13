#!/usr/bin/env python3
"""Definitive production check: write the REAL 2pip OOS/CPCV metrics into a COPY
of swimmy.db (never the live DB), then run the real tools.tribe.export pipeline
against that copy. The feed PASS count it prints is exactly what the
backbone-fixed factory would publish. This removes any doubt that score_gate.py's
hand-rolled honest_gate mapping matches production.
"""
from __future__ import annotations
import json, shutil, sqlite3, os, sys
sys.path.insert(0, ".")

SRC = "data/memory/swimmy.db"
DST = "logs/tribe_gonogo_20260713/swimmy_backfilled.db"
RESULTS = "logs/tribe_gonogo_20260713/results_full.json"

def main():
    shutil.copy2(SRC, DST)
    res = {s["name"]: s for s in json.load(open(RESULTS, encoding="utf-8"))["strategies"]}
    con = sqlite3.connect(DST)
    n = 0
    for name, s in res.items():
        con.execute(
            "UPDATE strategies SET oos_sharpe=?, cpcv_median=?, cpcv_pass_rate=? WHERE name=?",
            (s["oos"]["sharpe"], s["cpcv"]["median_sharpe"], s["cpcv"]["pass_rate"], name),
        )
        n += 1
    con.commit(); con.close()
    print(f"backfilled {n} rows into {DST}")

    from tools.tribe.export import build_feed_from_sql, render_markdown
    from tools.tribe.honest_gate import GateConfig
    cfg = GateConfig()  # production defaults
    feed = build_feed_from_sql(DST, cfg)
    os.makedirs("logs/tribe_gonogo_20260713/feed_backfilled", exist_ok=True)
    json.dump(feed, open("logs/tribe_gonogo_20260713/feed_backfilled/strategies_feed.json", "w", encoding="utf-8"), indent=2, ensure_ascii=False)
    open("logs/tribe_gonogo_20260713/feed_backfilled/CATALOG.md", "w", encoding="utf-8").write(render_markdown(feed))
    c = feed["counts"]
    print("=== PRODUCTION FEED (export.py on backfilled copy) ===")
    print(f"  total={c['total']}  PASS={c.get('PASS')}  PROVISIONAL={c.get('PROVISIONAL')}  REJECT={c.get('REJECT')}  parse_errors={c['parse_errors']}")
    # list the PASS names + whether diverse
    passes = [r for r in feed["strategies"] if r["gate"]["verdict"] == "PASS"]
    div = [r for r in passes if (r.get("symbol") != "USDJPY") or (str(r.get("category") or "").lstrip(":").upper() != "TREND")]
    print(f"  PASS diverse (non-USDJPY or non-TREND): {len(div)}")
    print(f"  PASS symbols: {sorted(set(r.get('symbol') for r in passes))}")

if __name__ == "__main__":
    main()
