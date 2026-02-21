#!/usr/bin/env python3
"""Run full XAU autobot cycle: backtest -> optimize -> readiness -> cost guard."""

from __future__ import annotations

import argparse
import json
import subprocess
import urllib.error
import urllib.request
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Sequence


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


def pick_backtest_summary(rows: Sequence[Dict[str, object]], *, preset: str, segment: str) -> Dict[str, object]:
    for row in rows:
        if row.get("preset") == preset and row.get("segment") == segment:
            return row
    raise ValueError(f"backtest summary not found: preset={preset} segment={segment}")


def pick_optimize_best(rows: Sequence[Dict[str, object]]) -> Dict[str, object]:
    ranked = [row for row in rows if "rank" in row]
    if not ranked:
        raise ValueError("optimize rank rows not found")
    return min(ranked, key=lambda row: int(row.get("rank", 999999)))


def build_optimize_command(
    *,
    python_exe: str,
    ticker: str,
    period: str,
    interval: str,
    cost_per_side: float,
    top_k: int,
    write_config: str,
) -> List[str]:
    return [
        python_exe,
        "tools/xau_autobot_optimize.py",
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
        "--write-config",
        write_config,
    ]


def build_cycle_summary(
    *,
    timestamp: str,
    period: str,
    interval: str,
    tuned_config_path: str,
    backtest: Dict[str, object],
    optimize_best: Dict[str, object],
    readiness: Dict[str, object],
    cost_guard: Dict[str, object],
    report_paths: Dict[str, str],
) -> Dict[str, object]:
    return {
        "timestamp": timestamp,
        "period": period,
        "interval": interval,
        "tuned_config": tuned_config_path,
        "backtest": backtest,
        "optimize_best": optimize_best,
        "readiness": readiness,
        "cost_guard": cost_guard,
        "reports": report_paths,
    }


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


def _write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def _load_json(path: Path) -> Dict[str, object]:
    with path.open("r", encoding="utf-8") as f:
        data = json.load(f)
    if not isinstance(data, dict):
        raise RuntimeError(f"json object expected: {path}")
    return data


def _compact_readiness(report: Dict[str, object]) -> Dict[str, object]:
    def _as_float(value: object, default: float = 0.0) -> float:
        try:
            if value is None:
                return default
            return float(value)
        except (TypeError, ValueError):
            return default

    def _summarize_oos_robustness(raw_splits: object) -> Dict[str, float]:
        if not isinstance(raw_splits, list):
            return {}

        oos_pf: List[float] = []
        oos_returns: List[float] = []
        for split in raw_splits:
            if not isinstance(split, dict):
                continue
            oos = split.get("oos")
            if not isinstance(oos, dict):
                continue
            oos_pf.append(_as_float(oos.get("pf", 0.0), 0.0))
            oos_returns.append(_as_float(oos.get("total_return", 0.0), 0.0))

        if not oos_returns:
            return {}

        neg_count = sum(1 for value in oos_returns if value < 0.0)
        count = float(len(oos_returns))
        return {
            "oos_count": count,
            "oos_worst_pf": float(min(oos_pf)),
            "oos_worst_total_return": float(min(oos_returns)),
            "oos_negative_return_ratio": float(neg_count / len(oos_returns)),
        }

    all_metrics = report.get("all") if isinstance(report.get("all"), dict) else {}
    compact = {
        "verdict": report.get("verdict", "UNKNOWN"),
        "break_even_roundtrip_cost": report.get("break_even_roundtrip_cost", 0.0),
        "trades": all_metrics.get("trades", 0.0),
        "pf": all_metrics.get("pf", 0.0),
        "total_return": all_metrics.get("total_return", 0.0),
        "max_dd": all_metrics.get("max_dd", 0.0),
    }
    robustness = _summarize_oos_robustness(report.get("splits"))
    if robustness:
        compact["robustness"] = robustness
    return compact


def _compact_cost_guard(report: Dict[str, object]) -> Dict[str, object]:
    return {
        "verdict": report.get("verdict", "UNKNOWN"),
        "effective_roundtrip_pct": report.get("effective_roundtrip_pct", 0.0),
        "spread_points": report.get("spread_points", -1.0),
        "max_spread_points_go": report.get("max_spread_points_go", 0.0),
        "max_spread_points_safe": report.get("max_spread_points_safe", 0.0),
        "max_spread_points_break_even": report.get("max_spread_points_break_even", 0.0),
    }


def _post_discord(webhook_url: str, summary: Dict[str, object]) -> None:
    title = "XAU AutoBot Cycle"
    readiness = summary.get("readiness", {}) if isinstance(summary.get("readiness"), dict) else {}
    cost_guard = summary.get("cost_guard", {}) if isinstance(summary.get("cost_guard"), dict) else {}
    backtest = summary.get("backtest", {}) if isinstance(summary.get("backtest"), dict) else {}

    lines = [
        f"period={summary.get('period')} interval={summary.get('interval')}",
        f"backtest_pf={backtest.get('pf')} total_return={backtest.get('total_return')}",
        f"readiness={readiness.get('verdict')} break_even_rt={readiness.get('break_even_roundtrip_cost')}",
        f"cost_guard={cost_guard.get('verdict')} spread={cost_guard.get('spread_points')}",
    ]

    payload = {
        "embeds": [
            {
                "title": title,
                "description": "\n".join(lines),
                "color": 3447003,
            }
        ]
    }
    body = json.dumps(payload).encode("utf-8")
    request = urllib.request.Request(
        webhook_url,
        data=body,
        method="POST",
        headers=build_discord_headers(),
    )
    try:
        with urllib.request.urlopen(request, timeout=10):
            return
    except urllib.error.URLError as exc:
        raise RuntimeError(f"failed to post discord webhook: {exc}") from exc


def build_discord_headers() -> Dict[str, str]:
    return {
        "Content-Type": "application/json",
        # Discord webhook endpoint in this environment rejects default urllib UA (Cloudflare 1010).
        "User-Agent": "Mozilla/5.0 (compatible; xau-autobot/1.0)",
    }


def dispatch_discord_notification(
    webhook_urls: Sequence[str],
    summary: Dict[str, object],
    *,
    strict: bool,
) -> Dict[str, object]:
    cleaned = [url.strip() for url in webhook_urls if isinstance(url, str) and url.strip()]
    if not cleaned:
        return {"notified": False, "error": "no webhook configured"}

    last_error = ""
    for idx, url in enumerate(cleaned, start=1):
        try:
            _post_discord(url, summary)
            return {"notified": True, "used_webhook": url, "attempted": idx}
        except Exception as exc:
            last_error = str(exc)
            continue

    if strict:
        raise RuntimeError(last_error or "notification failed")
    return {"notified": False, "error": last_error or "notification failed", "attempted": len(cleaned)}


def is_xau_market_open(
    now_utc: datetime,
    *,
    sunday_open_hour_utc: int = 22,
    friday_close_hour_utc: int = 22,
) -> bool:
    weekday = now_utc.weekday()  # Monday=0 .. Sunday=6
    hour = now_utc.hour

    if weekday == 5:
        return False
    if weekday == 6 and hour < sunday_open_hour_utc:
        return False
    if weekday == 4 and hour >= friday_close_hour_utc:
        return False
    return True


def main() -> None:
    parser = argparse.ArgumentParser(description="Run full XAU autobot cycle")
    parser.add_argument("--python-exe", default="./.venv/bin/python")
    parser.add_argument("--ticker", default="GC=F")
    parser.add_argument("--period", default="90d")
    parser.add_argument("--interval", default="5m")
    parser.add_argument("--cost-per-side", type=float, default=0.0002)
    parser.add_argument("--top-k", type=int, default=8)
    parser.add_argument("--assumed-cost-side", type=float, default=0.0002)
    parser.add_argument("--spread-points", type=float, default=80.0)
    parser.add_argument("--spread-grid", default="20,40,60,80,100,120,140,160,180,200")
    parser.add_argument("--point", type=float, default=0.01)
    parser.add_argument("--price", type=float, default=0.0)
    parser.add_argument("--commission-roundtrip-pct", type=float, default=0.02)
    parser.add_argument("--slippage-roundtrip-pct", type=float, default=0.01)
    parser.add_argument("--reports-dir", default="data/reports")
    parser.add_argument("--write-config", default="")
    parser.add_argument("--write-summary", default="")
    parser.add_argument("--discord-webhook", default="")
    parser.add_argument("--discord-webhook-fallbacks", default="")
    parser.add_argument("--notify-strict", action="store_true", help="Fail run when discord notification fails")
    parser.add_argument("--market-hours-only", action="store_true", help="Skip cycle outside XAU weekly market hours")
    parser.add_argument("--market-open-hour-utc", type=int, default=22)
    parser.add_argument("--market-close-hour-utc", type=int, default=22)
    args = parser.parse_args()

    ts = datetime.now(timezone.utc)

    if args.market_hours_only and not is_xau_market_open(
        ts,
        sunday_open_hour_utc=args.market_open_hour_utc,
        friday_close_hour_utc=args.market_close_hour_utc,
    ):
        print(
            json.dumps(
                {
                    "timestamp": ts.isoformat(),
                    "action": "SKIP",
                    "reason": "market_closed",
                    "market_open_hour_utc": args.market_open_hour_utc,
                    "market_close_hour_utc": args.market_close_hour_utc,
                },
                ensure_ascii=True,
            )
        )
        return

    stamp = ts.strftime("%Y%m%d_%H%M%S")
    period_tag = args.period.replace("/", "_").replace(" ", "")
    reports_dir = Path(args.reports_dir)
    reports_dir.mkdir(parents=True, exist_ok=True)

    config_out = args.write_config or f"tools/configs/xau_autobot.tuned_auto_gc_m5_{period_tag}.json"
    backtest_report_path = reports_dir / f"xau_autobot_backtest_{stamp}_{period_tag}.jsonl"
    optimize_report_path = reports_dir / f"xau_autobot_optimize_{stamp}_{period_tag}.jsonl"
    readiness_report_path = reports_dir / f"xau_autobot_readiness_{period_tag}.json"
    cost_guard_report_path = reports_dir / f"xau_autobot_cost_guard_{period_tag}.json"
    summary_report_path = Path(args.write_summary) if args.write_summary else reports_dir / f"xau_autobot_cycle_summary_{period_tag}.json"

    backtest_cmd = [
        args.python_exe,
        "tools/xau_autobot_backtest.py",
        "--ticker",
        args.ticker,
        "--period",
        args.period,
        "--interval",
        args.interval,
        "--cost-per-side",
        str(args.cost_per_side),
        "--mode",
        "both",
    ]
    backtest_stdout = _run_command(backtest_cmd)
    _write_text(backtest_report_path, backtest_stdout)
    backtest_rows = parse_json_lines(backtest_stdout)
    tuned_all = pick_backtest_summary(backtest_rows, preset="tuned", segment="all")

    optimize_cmd = build_optimize_command(
        python_exe=args.python_exe,
        ticker=args.ticker,
        period=args.period,
        interval=args.interval,
        cost_per_side=args.cost_per_side,
        top_k=args.top_k,
        write_config=config_out,
    )
    optimize_stdout = _run_command(optimize_cmd)
    _write_text(optimize_report_path, optimize_stdout)
    optimize_rows = parse_json_lines(optimize_stdout)
    best = pick_optimize_best(optimize_rows)

    readiness_cmd = [
        args.python_exe,
        "tools/xau_autobot_readiness.py",
        "--config",
        config_out,
        "--ticker",
        args.ticker,
        "--period",
        args.period,
        "--interval",
        args.interval,
        "--assumed-cost-side",
        str(args.assumed_cost_side),
        "--write-report",
        str(readiness_report_path),
    ]
    _run_command(readiness_cmd)
    readiness_report = _load_json(readiness_report_path)

    cost_guard_cmd = [
        args.python_exe,
        "tools/xau_autobot_cost_guard.py",
        "--readiness-report",
        str(readiness_report_path),
        "--point",
        str(args.point),
        "--spread-points",
        str(args.spread_points),
        "--spread-grid",
        args.spread_grid,
        "--commission-roundtrip-pct",
        str(args.commission_roundtrip_pct),
        "--slippage-roundtrip-pct",
        str(args.slippage_roundtrip_pct),
        "--price-ticker",
        args.ticker,
        "--write-report",
        str(cost_guard_report_path),
    ]
    if args.price > 0.0:
        cost_guard_cmd.extend(["--price", str(args.price)])

    _run_command(cost_guard_cmd)
    cost_guard_report = _load_json(cost_guard_report_path)

    summary = build_cycle_summary(
        timestamp=ts.isoformat(),
        period=args.period,
        interval=args.interval,
        tuned_config_path=config_out,
        backtest={
            "trades": tuned_all.get("trades", 0.0),
            "win_rate": tuned_all.get("win_rate", 0.0),
            "pf": tuned_all.get("pf", 0.0),
            "total_return": tuned_all.get("total_return", 0.0),
            "max_dd": tuned_all.get("max_dd", 0.0),
        },
        optimize_best={
            "rank": best.get("rank", 0),
            "score": best.get("score", 0.0),
            "candidate": best.get("candidate", {}),
        },
        readiness=_compact_readiness(readiness_report),
        cost_guard=_compact_cost_guard(cost_guard_report),
        report_paths={
            "backtest": str(backtest_report_path),
            "optimize": str(optimize_report_path),
            "readiness": str(readiness_report_path),
            "cost_guard": str(cost_guard_report_path),
            "summary": str(summary_report_path),
        },
    )

    with summary_report_path.open("w", encoding="utf-8") as f:
        json.dump(summary, f, ensure_ascii=True, indent=2)
        f.write("\n")

    print(json.dumps(summary, ensure_ascii=True))

    webhook_candidates: List[str] = []
    if args.discord_webhook:
        webhook_candidates.append(args.discord_webhook)
    if args.discord_webhook_fallbacks:
        webhook_candidates.extend(args.discord_webhook_fallbacks.split(","))

    if webhook_candidates:
        notify_result = dispatch_discord_notification(webhook_candidates, summary, strict=args.notify_strict)
        print(json.dumps(notify_result, ensure_ascii=True))


if __name__ == "__main__":
    main()
