#!/usr/bin/env python3
"""Run XAU autobot cycle across multiple periods and build comparison report."""

from __future__ import annotations

import argparse
import json
import subprocess
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Sequence


def parse_periods(value: str) -> List[str]:
    out: List[str] = []
    for token in str(value).split(","):
        period = token.strip()
        if period:
            out.append(period)
    return out


def parse_json_lines(text: str) -> List[Dict[str, object]]:
    rows: List[Dict[str, object]] = []
    for raw in text.splitlines():
        line = raw.strip()
        if not line:
            continue
        payload = json.loads(line)
        if isinstance(payload, dict):
            rows.append(payload)
    return rows


def pick_cycle_summary(rows: Sequence[Dict[str, object]]) -> Dict[str, object]:
    for row in rows:
        if (
            "period" in row
            and isinstance(row.get("backtest"), dict)
            and isinstance(row.get("readiness"), dict)
            and isinstance(row.get("cost_guard"), dict)
        ):
            return row
    raise ValueError("cycle summary row not found")


def build_cycle_command(
    *,
    python_exe: str,
    ticker: str,
    period: str,
    interval: str,
    cost_per_side: float,
    top_k: int,
    assumed_cost_side: float,
    spread_points: float,
    spread_grid: str,
    point: float,
    commission_roundtrip_pct: float,
    slippage_roundtrip_pct: float,
    reports_dir: str,
    write_config: str,
    write_summary: str,
    webhook: str,
    webhook_fallbacks: str,
    market_hours_only: bool,
) -> List[str]:
    cmd = [
        python_exe,
        "tools/xau_autobot_cycle.py",
        "--python-exe",
        python_exe,
        "--ticker",
        ticker,
        "--period",
        period,
        "--interval",
        interval,
        "--cost-per-side",
        str(cost_per_side),
        "--top-k",
        str(top_k),
        "--assumed-cost-side",
        str(assumed_cost_side),
        "--spread-points",
        str(spread_points),
        "--spread-grid",
        spread_grid,
        "--point",
        str(point),
        "--commission-roundtrip-pct",
        str(commission_roundtrip_pct),
        "--slippage-roundtrip-pct",
        str(slippage_roundtrip_pct),
        "--reports-dir",
        reports_dir,
        "--write-config",
        write_config,
        "--write-summary",
        write_summary,
    ]
    if webhook:
        cmd.extend(["--discord-webhook", webhook])
    if webhook_fallbacks:
        cmd.extend(["--discord-webhook-fallbacks", webhook_fallbacks])
    if market_hours_only:
        cmd.append("--market-hours-only")
    return cmd


def _run_command(command: Sequence[str]) -> str:
    proc = subprocess.run(command, capture_output=True, text=True, check=False)
    if proc.returncode != 0:
        raise RuntimeError(
            "command failed\n"
            + f"cmd: {' '.join(command)}\n"
            + f"exit: {proc.returncode}\n"
            + f"stdout:\n{proc.stdout}\n"
            + f"stderr:\n{proc.stderr}"
        )
    return proc.stdout


def main() -> None:
    parser = argparse.ArgumentParser(description="Run xau_autobot_cycle over multiple periods")
    parser.add_argument("--python-exe", default="./.venv/bin/python")
    parser.add_argument("--ticker", default="GC=F")
    parser.add_argument("--periods", default="45d,60d,90d")
    parser.add_argument("--interval", default="5m")
    parser.add_argument("--cost-per-side", type=float, default=0.0002)
    parser.add_argument("--top-k", type=int, default=8)
    parser.add_argument("--assumed-cost-side", type=float, default=0.0002)
    parser.add_argument("--spread-points", type=float, default=80.0)
    parser.add_argument("--spread-grid", default="20,40,60,80,100,120,140,160,180,200")
    parser.add_argument("--point", type=float, default=0.01)
    parser.add_argument("--commission-roundtrip-pct", type=float, default=0.02)
    parser.add_argument("--slippage-roundtrip-pct", type=float, default=0.01)
    parser.add_argument("--reports-dir", default="data/reports")
    parser.add_argument("--config-dir", default="tools/configs")
    parser.add_argument("--write-comparison", default="data/reports/xau_autobot_cycle_comparison.json")
    parser.add_argument("--discord-webhook", default="")
    parser.add_argument("--discord-webhook-fallbacks", default="")
    parser.add_argument("--market-hours-only", action="store_true")
    args = parser.parse_args()

    periods = parse_periods(args.periods)
    if not periods:
        raise RuntimeError("no periods configured")

    config_dir = Path(args.config_dir)
    config_dir.mkdir(parents=True, exist_ok=True)
    reports_dir = Path(args.reports_dir)
    reports_dir.mkdir(parents=True, exist_ok=True)

    period_rows: List[Dict[str, object]] = []
    for idx, period in enumerate(periods):
        notify = idx == len(periods) - 1
        config_path = config_dir / f"xau_autobot.tuned_auto_gc_m5_{period}.json"
        summary_path = reports_dir / f"xau_autobot_cycle_summary_{period}.json"
        cmd = build_cycle_command(
            python_exe=args.python_exe,
            ticker=args.ticker,
            period=period,
            interval=args.interval,
            cost_per_side=args.cost_per_side,
            top_k=args.top_k,
            assumed_cost_side=args.assumed_cost_side,
            spread_points=args.spread_points,
            spread_grid=args.spread_grid,
            point=args.point,
            commission_roundtrip_pct=args.commission_roundtrip_pct,
            slippage_roundtrip_pct=args.slippage_roundtrip_pct,
            reports_dir=str(reports_dir),
            write_config=str(config_path),
            write_summary=str(summary_path),
            webhook=(args.discord_webhook if notify else ""),
            webhook_fallbacks=(args.discord_webhook_fallbacks if notify else ""),
            market_hours_only=args.market_hours_only,
        )
        stdout = _run_command(cmd)
        rows = parse_json_lines(stdout)
        summary = pick_cycle_summary(rows)
        summary["period"] = period
        summary["reports"] = summary.get("reports", {})
        period_rows.append(summary)

    output = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "ticker": args.ticker,
        "interval": args.interval,
        "periods": period_rows,
    }

    output_path = Path(args.write_comparison)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", encoding="utf-8") as f:
        json.dump(output, f, ensure_ascii=True, indent=2)
        f.write("\n")

    print(json.dumps(output, ensure_ascii=True))


if __name__ == "__main__":
    main()
