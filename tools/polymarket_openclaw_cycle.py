#!/usr/bin/env python3
"""One-shot runner: generate plan and report for OpenClaw + Polymarket."""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


def build_cycle_paths(output_dir: Path, *, run_id: str, run_date: str) -> Dict[str, Path]:
    return {
        "plan_file": output_dir / f"plan_{run_id}.json",
        "report_file": output_dir / f"report_{run_date}_{run_id}.json",
        "journal_file": output_dir / "journal.jsonl",
    }


def build_bot_command(
    *,
    base_dir: Path,
    config_file: str,
    signals_file: str,
    openclaw_cmd: str,
    markets_file: str,
    settlements_file: str,
    allow_duplicate_open_markets: bool,
    max_open_positions: int,
    max_daily_entries: int,
    max_daily_loss_streak: int,
    max_daily_realized_loss_usd: float,
    limit: int,
    run_id: str,
    plan_file: Path,
    journal_file: Path,
) -> List[str]:
    cmd = [
        sys.executable,
        str(base_dir / "tools" / "polymarket_openclaw_bot.py"),
        "--config-file",
        config_file,
        "--limit",
        str(limit),
        "--run-id",
        run_id,
        "--journal-file",
        str(journal_file),
        "--write-plan",
        str(plan_file),
    ]
    if signals_file:
        cmd.extend(["--signals-file", signals_file])
    if openclaw_cmd:
        cmd.extend(["--openclaw-cmd", openclaw_cmd])
    if markets_file:
        cmd.extend(["--markets-file", markets_file])
    if settlements_file:
        cmd.extend(["--settlements-file", settlements_file])
    if allow_duplicate_open_markets:
        cmd.append("--allow-duplicate-open-markets")
    if max_open_positions > 0:
        cmd.extend(["--max-open-positions", str(max_open_positions)])
    if max_daily_entries > 0:
        cmd.extend(["--max-daily-entries", str(max_daily_entries)])
    if max_daily_loss_streak > 0:
        cmd.extend(["--max-daily-loss-streak", str(max_daily_loss_streak)])
    if max_daily_realized_loss_usd > 0.0:
        cmd.extend(["--max-daily-realized-loss-usd", str(max_daily_realized_loss_usd)])
    return cmd


def build_report_command(
    *,
    base_dir: Path,
    journal_file: Path,
    run_date: str,
    report_file: Path,
    settlements_file: str,
    fee_bps_per_side: float,
    slippage_bps_per_side: float,
) -> List[str]:
    cmd = [
        sys.executable,
        str(base_dir / "tools" / "polymarket_openclaw_report.py"),
        "--journal-file",
        str(journal_file),
        "--date",
        run_date,
        "--write-report",
        str(report_file),
    ]
    if settlements_file:
        cmd.extend(
            [
                "--settlements-file",
                settlements_file,
                "--fee-bps-per-side",
                str(fee_bps_per_side),
                "--slippage-bps-per-side",
                str(slippage_bps_per_side),
            ]
        )
    return cmd


def build_fetch_settlements_command(
    *,
    base_dir: Path,
    journal_file: Path,
    date_filter: str,
    existing_settlements: Path,
    out_settlements: Path,
    min_price_for_win: float,
    min_gap: float,
    sleep_ms: int,
) -> List[str]:
    cmd = [
        sys.executable,
        str(base_dir / "tools" / "polymarket_fetch_settlements.py"),
        "--journal-file",
        str(journal_file),
        "--date",
        date_filter,
        "--write-settlements",
        str(out_settlements),
        "--min-price-for-win",
        str(min_price_for_win),
        "--min-gap",
        str(min_gap),
        "--sleep-ms",
        str(max(0, sleep_ms)),
    ]
    if existing_settlements and existing_settlements.is_file():
        cmd.extend(["--existing-settlements", str(existing_settlements)])
    return cmd


def build_autotune_command(
    *,
    base_dir: Path,
    journal_file: Path,
    settlements_file: Path,
    base_config_file: str,
    date_filter: str,
    out_report: Path,
    out_candidate_config: Path,
    min_trades: int,
    fee_bps_per_side: float,
    slippage_bps_per_side: float,
) -> List[str]:
    return [
        sys.executable,
        str(base_dir / "tools" / "polymarket_openclaw_autotune.py"),
        "--journal-file",
        str(journal_file),
        "--settlements-file",
        str(settlements_file),
        "--base-config-file",
        base_config_file,
        "--date",
        date_filter,
        "--min-trades",
        str(max(1, min_trades)),
        "--fee-bps-per-side",
        str(fee_bps_per_side),
        "--slippage-bps-per-side",
        str(slippage_bps_per_side),
        "--write-report",
        str(out_report),
        "--write-candidate-config",
        str(out_candidate_config),
    ]


def _run_json_command(cmd: List[str]) -> Dict:
    proc = subprocess.run(cmd, capture_output=True, text=True, check=True)
    stdout = proc.stdout.strip()
    return json.loads(stdout) if stdout else {}


def should_run_posttrade_steps(plan_summary: Dict[str, Any]) -> bool:
    try:
        return int(plan_summary.get("entries", 0) or 0) > 0
    except (TypeError, ValueError):
        return False


def should_apply_autotune_candidate(
    *,
    best: Dict[str, Any],
    min_trades: int,
    min_realized_pnl_usd: float,
) -> bool:
    if not best:
        return False
    try:
        trades = int(best.get("trades", 0) or 0)
        pnl = float(best.get("realized_pnl_usd", 0.0) or 0.0)
    except (TypeError, ValueError):
        return False
    return trades >= max(1, min_trades) and pnl >= float(min_realized_pnl_usd)


def apply_candidate_config_with_backup(
    *,
    candidate_config: Dict[str, Any],
    target_config_path: Path,
    backup_dir: Path,
    run_id: str,
) -> Dict[str, str]:
    target_config_path.parent.mkdir(parents=True, exist_ok=True)
    backup_dir.mkdir(parents=True, exist_ok=True)
    now_tag = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    backup_file = backup_dir / f"{target_config_path.stem}_{run_id}_{now_tag}.json"
    if target_config_path.exists():
        backup_file.write_text(target_config_path.read_text(encoding="utf-8"), encoding="utf-8")
    target_config_path.write_text(json.dumps(candidate_config, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    return {"applied_file": str(target_config_path), "backup_file": str(backup_file)}


def main() -> None:
    parser = argparse.ArgumentParser(description="Run bot+report cycle in one command")
    parser.add_argument("--config-file", required=True)
    parser.add_argument("--signals-file", default="")
    parser.add_argument("--openclaw-cmd", default="")
    parser.add_argument("--markets-file", default="")
    parser.add_argument("--max-open-positions", type=int, default=0)
    parser.add_argument("--max-daily-entries", type=int, default=0)
    parser.add_argument("--max-daily-loss-streak", type=int, default=0)
    parser.add_argument("--max-daily-realized-loss-usd", type=float, default=0.0)
    parser.add_argument("--limit", type=int, default=250)
    parser.add_argument("--output-dir", default="")
    parser.add_argument("--run-id", default="")
    parser.add_argument("--allow-duplicate-open-markets", action="store_true")
    parser.add_argument("--settlements-file", default="")
    parser.add_argument("--auto-fetch-settlements", action="store_true")
    parser.add_argument("--settlement-min-price-for-win", type=float, default=0.98)
    parser.add_argument("--settlement-min-gap", type=float, default=0.05)
    parser.add_argument("--settlement-sleep-ms", type=int, default=0)
    parser.add_argument("--autotune", action="store_true")
    parser.add_argument("--autotune-min-trades", type=int, default=10)
    parser.add_argument("--autotune-apply-best", action="store_true")
    parser.add_argument("--autotune-apply-min-trades", type=int, default=20)
    parser.add_argument("--autotune-apply-min-realized-pnl-usd", type=float, default=1.0)
    parser.add_argument("--autotune-apply-target-config", default="")
    parser.add_argument("--fee-bps-per-side", type=float, default=0.0)
    parser.add_argument("--slippage-bps-per-side", type=float, default=0.0)
    args = parser.parse_args()

    if not args.signals_file and not args.openclaw_cmd:
        raise SystemExit("provide --signals-file or --openclaw-cmd")

    base_dir = resolve_base_dir()
    output_dir = Path(args.output_dir) if args.output_dir else (base_dir / "data" / "reports" / "polymarket_openclaw")
    output_dir.mkdir(parents=True, exist_ok=True)

    now = datetime.now(timezone.utc)
    run_id = args.run_id.strip() or now.strftime("%Y%m%dT%H%M%SZ")
    run_date = now.strftime("%Y-%m-%d")
    paths = build_cycle_paths(output_dir, run_id=run_id, run_date=run_date)
    daily_settlement_file = output_dir / f"settlements_{run_date}.json"
    bot_settlement_source = args.settlements_file.strip()
    if not bot_settlement_source and daily_settlement_file.exists():
        bot_settlement_source = str(daily_settlement_file)

    bot_cmd = build_bot_command(
        base_dir=base_dir,
        config_file=args.config_file,
        signals_file=args.signals_file,
        openclaw_cmd=args.openclaw_cmd,
        markets_file=args.markets_file,
        settlements_file=bot_settlement_source,
        allow_duplicate_open_markets=args.allow_duplicate_open_markets,
        max_open_positions=max(0, args.max_open_positions),
        max_daily_entries=max(0, args.max_daily_entries),
        max_daily_loss_streak=max(0, args.max_daily_loss_streak),
        max_daily_realized_loss_usd=max(0.0, args.max_daily_realized_loss_usd),
        limit=max(1, args.limit),
        run_id=run_id,
        plan_file=paths["plan_file"],
        journal_file=paths["journal_file"],
    )
    bot_output = _run_json_command(bot_cmd)
    plan_summary = bot_output.get("summary", {})
    run_posttrade = should_run_posttrade_steps(plan_summary)

    settlement_source = args.settlements_file.strip()
    settlement_fetch_result: Dict[str, Any] = {}
    if run_posttrade and args.auto_fetch_settlements and not settlement_source:
        settlement_out = daily_settlement_file
        fetch_cmd = build_fetch_settlements_command(
            base_dir=base_dir,
            journal_file=paths["journal_file"],
            date_filter=run_date,
            existing_settlements=settlement_out if settlement_out.exists() else Path(""),
            out_settlements=settlement_out,
            min_price_for_win=args.settlement_min_price_for_win,
            min_gap=args.settlement_min_gap,
            sleep_ms=args.settlement_sleep_ms,
        )
        settlement_fetch_result = _run_json_command(fetch_cmd)
        settlement_source = str(settlement_out)

    report_cmd = build_report_command(
        base_dir=base_dir,
        journal_file=paths["journal_file"],
        run_date=run_date,
        report_file=paths["report_file"],
        settlements_file=settlement_source,
        fee_bps_per_side=args.fee_bps_per_side,
        slippage_bps_per_side=args.slippage_bps_per_side,
    )
    report_output = _run_json_command(report_cmd)

    autotune_result: Dict[str, Any] = {}
    autotune_candidate_path = ""
    config_apply_result: Dict[str, str] = {}
    if run_posttrade and args.autotune and settlement_source:
        tune_report = output_dir / f"autotune_report_{run_date}_{run_id}.json"
        tune_candidate = output_dir / f"autotune_candidate_{run_date}_{run_id}.json"
        autotune_cmd = build_autotune_command(
            base_dir=base_dir,
            journal_file=paths["journal_file"],
            settlements_file=Path(settlement_source),
            base_config_file=args.config_file,
            date_filter=run_date,
            out_report=tune_report,
            out_candidate_config=tune_candidate,
            min_trades=args.autotune_min_trades,
            fee_bps_per_side=args.fee_bps_per_side,
            slippage_bps_per_side=args.slippage_bps_per_side,
        )
        autotune_result = _run_json_command(autotune_cmd)
        if autotune_result.get("best") is not None and tune_candidate.exists():
            autotune_candidate_path = str(tune_candidate)
        if args.autotune_apply_best and autotune_result.get("candidate_config"):
            best = autotune_result.get("best") or {}
            if should_apply_autotune_candidate(
                best=best,
                min_trades=args.autotune_apply_min_trades,
                min_realized_pnl_usd=args.autotune_apply_min_realized_pnl_usd,
            ):
                target = Path(args.autotune_apply_target_config.strip() or args.config_file)
                config_apply_result = apply_candidate_config_with_backup(
                    candidate_config=autotune_result["candidate_config"],
                    target_config_path=target,
                    backup_dir=output_dir / "config_backups",
                    run_id=run_id,
                )

    result = {
        "run_id": run_id,
        "run_date": run_date,
        "paths": {k: str(v) for k, v in paths.items()},
        "plan_summary": plan_summary,
        "posttrade_enabled": run_posttrade,
        "settlement_source": settlement_source,
        "settlement_fetch": settlement_fetch_result,
        "report_summary": report_output,
        "autotune_candidate_config": autotune_candidate_path,
        "autotune_summary": autotune_result.get("best", {}),
        "config_apply_result": config_apply_result,
    }
    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
