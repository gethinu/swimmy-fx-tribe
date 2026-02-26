#!/usr/bin/env python3
"""Run XAU autobot cycle across multiple periods and build comparison report."""

from __future__ import annotations

import argparse
import json
import subprocess
import urllib.error
import urllib.request
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Optional, Sequence


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


def pick_cycle_summary_or_none(rows: Sequence[Dict[str, object]]) -> Optional[Dict[str, object]]:
    for row in rows:
        if (
            "period" in row
            and isinstance(row.get("backtest"), dict)
            and isinstance(row.get("readiness"), dict)
            and isinstance(row.get("cost_guard"), dict)
        ):
            return row
    return None


def pick_cycle_summary(rows: Sequence[Dict[str, object]]) -> Dict[str, object]:
    summary = pick_cycle_summary_or_none(rows)
    if summary is None:
        raise ValueError("cycle summary row not found")
    return summary


def pick_skip_row_or_none(rows: Sequence[Dict[str, object]]) -> Optional[Dict[str, object]]:
    for row in rows:
        if str(row.get("action", "")).upper() == "SKIP":
            return row
    return None


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


def build_skip_notification_lines(output: Dict[str, object]) -> List[str]:
    lines = [
        f"action={output.get('action')} reason={output.get('reason')}",
        f"ticker={output.get('ticker')} interval={output.get('interval')}",
    ]
    skipped = output.get("skipped_periods")
    if isinstance(skipped, list) and skipped:
        periods: List[str] = []
        for row in skipped:
            if isinstance(row, dict):
                period = str(row.get("period", "")).strip()
                if period:
                    periods.append(period)
        if periods:
            lines.append(f"skipped_periods={','.join(periods)}")
    return lines


def build_discord_headers() -> Dict[str, str]:
    return {
        "Content-Type": "application/json",
        "User-Agent": "Mozilla/5.0 (compatible; xau-autobot/1.0)",
    }


def _post_discord(webhook_url: str, output: Dict[str, object]) -> None:
    payload = {
        "embeds": [
            {
                "title": "XAU AutoBot Cycle Compare",
                "description": "\n".join(build_skip_notification_lines(output)),
                "color": 9807270,
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


def dispatch_discord_notification(
    webhook_urls: Sequence[str],
    output: Dict[str, object],
    *,
    strict: bool,
) -> Dict[str, object]:
    cleaned = [url.strip() for url in webhook_urls if isinstance(url, str) and url.strip()]
    if not cleaned:
        return {"notified": False, "error": "no webhook configured"}

    last_error = ""
    for idx, url in enumerate(cleaned, start=1):
        try:
            _post_discord(url, output)
            return {"notified": True, "used_webhook": url, "attempted": idx}
        except Exception as exc:
            last_error = str(exc)
            continue

    if strict:
        raise RuntimeError(last_error or "notification failed")
    return {"notified": False, "error": last_error or "notification failed", "attempted": len(cleaned)}


def apply_notify_result(output: Dict[str, object], notify_result: Dict[str, object]) -> Dict[str, object]:
    merged = dict(output)
    merged["notify"] = dict(notify_result)
    return merged


def append_history_jsonl(path: Path, output: Dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as f:
        f.write(json.dumps(output, ensure_ascii=True))
        f.write("\n")


def _as_float(value: object, default: float = 0.0) -> float:
    try:
        if value is None:
            return default
        return float(value)
    except (TypeError, ValueError):
        return default


def _load_notify_state(path: Path) -> Dict[str, object]:
    if not path.exists():
        return {}
    try:
        with path.open("r", encoding="utf-8") as f:
            payload = json.load(f)
    except Exception:
        return {}
    return payload if isinstance(payload, dict) else {}


def _write_notify_state(path: Path, state: Dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as f:
        json.dump(state, f, ensure_ascii=True, indent=2)
        f.write("\n")


def should_notify_skip(
    *,
    output: Dict[str, object],
    state: Dict[str, object],
    now_utc: datetime,
    cooldown_sec: int,
) -> bool:
    if str(output.get("action", "")).upper() != "SKIP":
        return False
    reason = str(output.get("reason", "")).strip().lower()
    if reason != "market_closed":
        return True
    if cooldown_sec <= 0:
        return True
    now_unix = float(now_utc.timestamp())
    last_notified = _as_float(state.get("market_closed_last_notified_unix"), 0.0)
    if last_notified <= 0.0:
        return True
    return (now_unix - last_notified) >= float(cooldown_sec)


def update_skip_notify_state(
    *,
    output: Dict[str, object],
    state: Dict[str, object],
    now_utc: datetime,
) -> Dict[str, object]:
    merged = dict(state)
    if str(output.get("action", "")).upper() == "SKIP" and str(output.get("reason", "")).strip().lower() == "market_closed":
        merged["market_closed_last_notified_unix"] = float(now_utc.timestamp())
        merged["market_closed_last_notified_utc"] = now_utc.isoformat()
    return merged


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
    parser.add_argument(
        "--write-history-jsonl",
        default="data/reports/xau_autobot_cycle_comparison_history.jsonl",
    )
    parser.add_argument("--discord-webhook", default="")
    parser.add_argument("--discord-webhook-fallbacks", default="")
    parser.add_argument("--notify-strict", action="store_true")
    parser.add_argument(
        "--skip-notify-state-path",
        default="data/reports/xau_autobot_cycle_compare_notify_state.json",
    )
    parser.add_argument("--skip-notify-cooldown-sec", type=int, default=86400)
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
    skipped_periods: List[Dict[str, object]] = []
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
        summary = pick_cycle_summary_or_none(rows)
        if summary is not None:
            summary["period"] = period
            summary["reports"] = summary.get("reports", {})
            period_rows.append(summary)
            continue

        skipped = pick_skip_row_or_none(rows)
        if skipped is not None:
            skipped_periods.append(
                {
                    "period": period,
                    "timestamp": skipped.get("timestamp"),
                    "action": skipped.get("action", "SKIP"),
                    "reason": skipped.get("reason", "unknown"),
                }
            )
            continue

        raise ValueError(f"cycle summary row not found for period={period}")

    output = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "ticker": args.ticker,
        "interval": args.interval,
        "periods": period_rows,
    }
    if skipped_periods:
        output["skipped_periods"] = skipped_periods
    if not period_rows and skipped_periods:
        output["action"] = "SKIP"
        output["reason"] = "market_closed"

    output_path = Path(args.write_comparison)
    history_path = Path(args.write_history_jsonl)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", encoding="utf-8") as f:
        json.dump(output, f, ensure_ascii=True, indent=2)
        f.write("\n")

    print(json.dumps(output, ensure_ascii=True))

    webhook_candidates: List[str] = []
    if args.discord_webhook:
        webhook_candidates.append(args.discord_webhook)
    if args.discord_webhook_fallbacks:
        webhook_candidates.extend(args.discord_webhook_fallbacks.split(","))
    if output.get("action") == "SKIP" and webhook_candidates:
        now_utc = datetime.now(timezone.utc)
        notify_state_path = Path(args.skip_notify_state_path)
        notify_state = _load_notify_state(notify_state_path)
        should_notify = should_notify_skip(
            output=output,
            state=notify_state,
            now_utc=now_utc,
            cooldown_sec=int(args.skip_notify_cooldown_sec),
        )
        if should_notify:
            notify_result = dispatch_discord_notification(webhook_candidates, output, strict=args.notify_strict)
            if bool(notify_result.get("notified", False)):
                notify_state = update_skip_notify_state(output=output, state=notify_state, now_utc=now_utc)
                _write_notify_state(notify_state_path, notify_state)
        else:
            notify_result = {
                "notified": False,
                "reason": "skip_notify_cooldown",
                "cooldown_sec": int(args.skip_notify_cooldown_sec),
            }
        output = apply_notify_result(output, notify_result)
        with output_path.open("w", encoding="utf-8") as f:
            json.dump(output, f, ensure_ascii=True, indent=2)
            f.write("\n")
        print(json.dumps(notify_result, ensure_ascii=True))

    append_history_jsonl(history_path, output)


if __name__ == "__main__":
    main()
