"""Export the 蠱毒 factory's survivors as a trustworthy strategy feed.

Walks ``data/library/`` (the surviving strategies), parses each ``#S(STRATEGY ..)``
file, runs the honest gate, ranks the result, and writes:

    feed/strategies_feed.json   - machine-readable feed for bundle-of-edge
    feed/CATALOG.md             - human-readable catalogue

Usage:
    python3 -m tools.tribe.export                 # default paths
    python3 -m tools.tribe.export --library data/library --out feed
"""

from __future__ import annotations

import argparse
import json
import os
from datetime import datetime, timezone

from . import sexpr
from .honest_gate import GateConfig, evaluate

# slots we surface as scalar metrics in the feed
_METRIC_KEYS = [
    "sharpe", "profit-factor", "win-rate", "trades", "max-dd",
    "oos-sharpe", "cpcv-median-sharpe", "cpcv-median-pf", "cpcv-median-wr",
    "cpcv-median-maxdd", "cpcv-pass-rate", "generation", "breeding-count",
]
_STRUCT_KEYS = ["indicators", "entry", "exit"]


def _strategy_to_record(raw: dict, rank_dir: str, path: str) -> dict:
    def g(*keys):
        for k in keys:
            if k in raw and raw[k] is not None:
                return raw[k]
        return None

    metrics = {}
    for k in _METRIC_KEYS:
        if k in raw and raw[k] is not None:
            metrics[k.replace("-", "_")] = raw[k]

    logic = {}
    for k in _STRUCT_KEYS:
        if k in raw and raw[k] is not None:
            logic[k] = sexpr.serialize(raw[k])

    return {
        "name": g("name"),
        "library_rank": rank_dir,
        "category": g("category"),
        "symbol": g("symbol"),
        "timeframe": g("timeframe"),
        "direction": g("direction"),
        "indicator_type": g("indicator-type"),
        "status": g("status"),
        "tier": g("tier"),
        "immortal": bool(g("immortal")) if g("immortal") is not None else False,
        "parents": g("parents"),
        "metrics": metrics,
        "logic": logic,
        "source_file": path,
    }


def _gate_candidate(rec: dict) -> dict:
    m = rec["metrics"]
    return {
        "trades": m.get("trades"),
        "profit_factor": m.get("profit_factor"),
        "sharpe": m.get("sharpe"),
        "win_rate": m.get("win_rate"),
        "max_dd": m.get("max_dd"),
        "oos_sharpe": m.get("oos_sharpe"),
        "cpcv_pass_rate": m.get("cpcv_pass_rate"),
        "cpcv_median_sharpe": m.get("cpcv_median_sharpe"),
        "entry": rec["logic"].get("entry"),
    }


def build_feed(library_dir: str, cfg: GateConfig) -> dict:
    records = []
    errors = []
    for root, _dirs, files in os.walk(library_dir):
        rank_dir = os.path.relpath(root, library_dir).split(os.sep)[0]
        for fn in sorted(files):
            if not fn.endswith(".lisp"):
                continue
            path = os.path.join(root, fn)
            try:
                with open(path, "r", encoding="utf-8", errors="replace") as f:
                    raw = sexpr.parse_strategy(f.read())
            except Exception as e:  # keep going; report at the end
                errors.append({"file": path, "error": str(e)})
                continue
            rec = _strategy_to_record(raw, rank_dir, os.path.relpath(path))
            verdict = evaluate(_gate_candidate(rec), cfg)
            rec["gate"] = verdict.to_dict()
            records.append(rec)

    records.sort(key=lambda r: r["gate"]["score"], reverse=True)

    summary = {"PASS": 0, "PROVISIONAL": 0, "REJECT": 0}
    for r in records:
        summary[r["gate"]["verdict"]] = summary.get(r["gate"]["verdict"], 0) + 1

    return {
        "schema": "swimmy.tribe.feed/v1",
        "generated_utc": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "gate_config": {
            "dd_budget_pct": cfg.dd_budget_pct,
            "kpi_monthly_pct": cfg.kpi_monthly_pct,
            "min_trades": cfg.min_trades,
            "min_profit_factor": cfg.min_profit_factor,
            "min_sharpe": cfg.min_sharpe,
        },
        "counts": {"total": len(records), **summary, "parse_errors": len(errors)},
        "parse_errors": errors,
        "strategies": records,
    }


def render_markdown(feed: dict) -> str:
    c = feed["counts"]
    gc = feed["gate_config"]
    lines = []
    lines.append("# Swimmy 蠱毒 Strategy Feed (CATALOG)")
    lines.append("")
    lines.append(f"_Generated {feed['generated_utc']} · schema `{feed['schema']}`_")
    lines.append("")
    lines.append("This catalogue is the **output of the Swimmy strategy factory**: strategies "
                 "that survived breeding + culling, scored by an honest promotion gate. "
                 "**\"Survived the jar\" is not \"verified edge.\"** Read the verdict column.")
    lines.append("")
    lines.append("## Summary")
    lines.append("")
    lines.append(f"- Total survivors parsed: **{c['total']}** (parse errors: {c['parse_errors']})")
    lines.append(f"- ✅ PASS: **{c.get('PASS', 0)}**  · 🟡 PROVISIONAL: **{c.get('PROVISIONAL', 0)}**  · ❌ REJECT: **{c.get('REJECT', 0)}**")
    lines.append("")
    lines.append("**Gate** (DD-budget "
                 f"{gc['dd_budget_pct']:.0f}%, KPI {gc['kpi_monthly_pct']:.0f}%/mo, "
                 f"min trades {gc['min_trades']}, min PF {gc['min_profit_factor']}, "
                 f"min Sharpe {gc['min_sharpe']}):")
    lines.append("")
    lines.append("- **PASS** — clears sample-size + PF + has OOS/CPCV validation. Deployable candidate.")
    lines.append("- **PROVISIONAL** — clears hard floors but is IS-only / thin. Needs forward proof before live.")
    lines.append("- **REJECT** — fails a hard floor (under-sampled, PF<floor, or fails DD-budget).")
    lines.append("")

    for verdict in ("PASS", "PROVISIONAL", "REJECT"):
        group = [r for r in feed["strategies"] if r["gate"]["verdict"] == verdict]
        if not group:
            continue
        mark = {"PASS": "✅", "PROVISIONAL": "🟡", "REJECT": "❌"}[verdict]
        lines.append(f"## {mark} {verdict} ({len(group)})")
        lines.append("")
        lines.append("| # | Name | Lib | Sym | TF | PF | Sharpe | Trades | OOS | Score |")
        lines.append("|---|------|-----|-----|----|----|--------|--------|-----|-------|")
        for i, r in enumerate(group, 1):
            m = r["metrics"]
            oos = "yes" if (m.get("oos_sharpe", 0) or m.get("cpcv_pass_rate", 0)) else "no"
            def fmt(x, p=3):
                return f"{x:.{p}f}" if isinstance(x, (int, float)) else "-"
            lines.append(
                f"| {i} | {r['name']} | {r['library_rank']} | {r.get('symbol') or '-'} | "
                f"{r.get('timeframe') or '-'} | {fmt(m.get('profit_factor'))} | "
                f"{fmt(m.get('sharpe'))} | {int(m['trades']) if m.get('trades') is not None else '-'} | "
                f"{oos} | {r['gate']['score']} |"
            )
        lines.append("")
    return "\n".join(lines) + "\n"


def main(argv=None):
    ap = argparse.ArgumentParser(description="Export 蠱毒 survivors as JSON + Markdown feed")
    ap.add_argument("--library", default="data/library")
    ap.add_argument("--out", default="feed")
    ap.add_argument("--dd-budget", type=float, default=12.0)
    ap.add_argument("--kpi-monthly", type=float, default=3.0)
    ap.add_argument("--min-trades", type=int, default=200)
    ap.add_argument("--min-pf", type=float, default=1.10)
    ap.add_argument("--min-sharpe", type=float, default=0.10)
    args = ap.parse_args(argv)

    cfg = GateConfig(
        dd_budget_pct=args.dd_budget,
        kpi_monthly_pct=args.kpi_monthly,
        min_trades=args.min_trades,
        min_profit_factor=args.min_pf,
        min_sharpe=args.min_sharpe,
    )
    feed = build_feed(args.library, cfg)
    os.makedirs(args.out, exist_ok=True)
    json_path = os.path.join(args.out, "strategies_feed.json")
    md_path = os.path.join(args.out, "CATALOG.md")
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(feed, f, indent=2, ensure_ascii=False)
    with open(md_path, "w", encoding="utf-8") as f:
        f.write(render_markdown(feed))

    c = feed["counts"]
    print(f"parsed {c['total']} survivors "
          f"(PASS {c.get('PASS', 0)} / PROVISIONAL {c.get('PROVISIONAL', 0)} / REJECT {c.get('REJECT', 0)}; "
          f"{c['parse_errors']} parse errors)")
    print(f"wrote {json_path}")
    print(f"wrote {md_path}")
    return feed


if __name__ == "__main__":
    main()
