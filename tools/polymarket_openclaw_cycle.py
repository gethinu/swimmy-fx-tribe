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
from typing import Any, Dict, List, Mapping


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
    min_liquidity_usd: float,
    min_volume_usd: float,
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
    if min_liquidity_usd > 0.0:
        cmd.extend(["--min-liquidity-usd", str(min_liquidity_usd)])
    if min_volume_usd > 0.0:
        cmd.extend(["--min-volume-usd", str(min_volume_usd)])
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


def build_execute_command(
    *,
    base_dir: Path,
    plan_file: Path,
    run_id: str,
    run_date: str,
    order_type: str,
    max_orders: int,
    min_expected_value_usd: float,
    min_stake_usd: float,
    fail_on_error: bool,
    write_report: Path,
) -> List[str]:
    cmd = [
        sys.executable,
        str(base_dir / "tools" / "polymarket_openclaw_execute.py"),
        "--plan-file",
        str(plan_file),
        "--run-id",
        run_id,
        "--run-date",
        run_date,
        "--order-type",
        (str(order_type).strip().upper() or "GTC"),
        "--max-orders",
        str(max(0, int(max_orders))),
        "--min-expected-value-usd",
        str(max(0.0, float(min_expected_value_usd))),
        "--min-stake-usd",
        str(max(0.0, float(min_stake_usd))),
        "--write-report",
        str(write_report),
    ]
    if fail_on_error:
        cmd.append("--fail-on-error")
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


def load_signal_summary(*, signals_meta_file: str, signals_file: str) -> Dict[str, Any]:
    meta_path: Path | None = None
    if signals_meta_file.strip():
        meta_path = Path(signals_meta_file).expanduser()
    if meta_path is None and signals_file.strip():
        signal_path = Path(signals_file).expanduser()
        if signal_path.suffix:
            meta_path = signal_path.with_suffix(".meta.json")
    if meta_path is None or not meta_path.exists() or not meta_path.is_file():
        return {}
    try:
        payload = json.loads(meta_path.read_text(encoding="utf-8"))
    except Exception:
        return {}
    if not isinstance(payload, dict):
        return {}

    source_counts_raw = payload.get("source_counts")
    source_counts: Dict[str, int] = {}
    if isinstance(source_counts_raw, dict):
        for key, value in source_counts_raw.items():
            name = str(key).strip()
            if not name:
                continue
            try:
                source_counts[name] = max(0, int(value))
            except (TypeError, ValueError):
                continue

    signal_count = payload.get("signal_count")
    agent_signal_count = payload.get("agent_signal_count", source_counts.get("openclaw_agent", 0))
    agent_signal_ratio = payload.get("agent_signal_ratio")
    if not isinstance(signal_count, int):
        try:
            signal_count = int(signal_count)
        except (TypeError, ValueError):
            signal_count = 0
    if not isinstance(agent_signal_count, int):
        try:
            agent_signal_count = int(agent_signal_count)
        except (TypeError, ValueError):
            agent_signal_count = int(source_counts.get("openclaw_agent", 0))
    if not isinstance(agent_signal_ratio, (int, float)):
        if signal_count > 0:
            agent_signal_ratio = float(agent_signal_count) / float(signal_count)
        else:
            agent_signal_ratio = 0.0

    updated_at = payload.get("updated_at")
    return {
        "signal_count": max(0, int(signal_count)),
        "source_counts": source_counts,
        "agent_signal_count": max(0, int(agent_signal_count)),
        "agent_signal_ratio": round(float(agent_signal_ratio), 6),
        "updated_at": str(updated_at) if updated_at else "",
        "signals_meta_file": str(meta_path),
    }


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


def _to_int(value: Any, default: int = 0) -> int:
    try:
        return int(value)
    except (TypeError, ValueError):
        return int(default)


def _to_float(value: Any, default: float = 0.0) -> float:
    try:
        return float(value)
    except (TypeError, ValueError):
        return float(default)


def build_latest_status_snapshot(*, result: Dict[str, Any], updated_at: str) -> Dict[str, Any]:
    plan_summary = result.get("plan_summary", {})
    signal_summary = result.get("signal_summary", {})
    execution = result.get("execution", {})
    report_summary = result.get("report_summary", {})
    paths = result.get("paths", {})
    live_execution_enabled = bool(result.get("live_execution_enabled", False))
    if live_execution_enabled:
        execution_ok = bool(execution.get("ok", False)) if isinstance(execution, dict) else False
        execution_skipped = bool(execution.get("skipped", False)) if isinstance(execution, dict) else False
    else:
        execution_ok = True
        execution_skipped = True
    return {
        "updated_at": str(updated_at),
        "run_id": str(result.get("run_id", "")),
        "run_date": str(result.get("run_date", "")),
        "entries": max(0, _to_int(plan_summary.get("entries"), 0)),
        "total_stake_usd": round(max(0.0, _to_float(plan_summary.get("total_stake_usd"), 0.0)), 8),
        "open_markets": max(0, _to_int(plan_summary.get("open_markets"), 0)),
        "blocked_open_markets": max(0, _to_int(plan_summary.get("blocked_open_markets"), 0)),
        "quality_filtered_markets": max(0, _to_int(plan_summary.get("quality_filtered_markets"), 0)),
        "live_execution_enabled": live_execution_enabled,
        "execution_ok": execution_ok,
        "execution_skipped": execution_skipped,
        "execution_attempted": max(0, _to_int(execution.get("attempted"), 0))
        if isinstance(execution, dict)
        else 0,
        "execution_sent": max(0, _to_int(execution.get("sent"), 0)) if isinstance(execution, dict) else 0,
        "execution_failed": max(0, _to_int(execution.get("failed"), 0))
        if isinstance(execution, dict)
        else 0,
        "signal_count": max(0, _to_int(signal_summary.get("signal_count"), 0))
        if isinstance(signal_summary, dict)
        else 0,
        "agent_signal_count": max(0, _to_int(signal_summary.get("agent_signal_count"), 0))
        if isinstance(signal_summary, dict)
        else 0,
        "agent_signal_ratio": round(max(0.0, _to_float(signal_summary.get("agent_signal_ratio"), 0.0)), 6)
        if isinstance(signal_summary, dict)
        else 0.0,
        "expected_value_usd": round(_to_float(report_summary.get("total_expected_value_usd"), 0.0), 8)
        if isinstance(report_summary, dict)
        else 0.0,
        "expected_return_on_stake": round(_to_float(report_summary.get("expected_return_on_stake"), 0.0), 8)
        if isinstance(report_summary, dict)
        else 0.0,
        "plan_file": str(paths.get("plan_file", "")) if isinstance(paths, dict) else "",
        "report_file": str(paths.get("report_file", "")) if isinstance(paths, dict) else "",
        "journal_file": str(paths.get("journal_file", "")) if isinstance(paths, dict) else "",
    }


def write_latest_status_snapshot(*, status_file: Path, snapshot: Dict[str, Any]) -> None:
    status_file.parent.mkdir(parents=True, exist_ok=True)
    tmp_file = status_file.with_suffix(status_file.suffix + ".tmp")
    text = json.dumps(snapshot, ensure_ascii=False, indent=2) + "\n"
    tmp_file.write_text(text, encoding="utf-8")
    tmp_file.replace(status_file)


def append_status_history(*, history_file: Path, snapshot: Mapping[str, Any], max_entries: int = 1000) -> None:
    history_file.parent.mkdir(parents=True, exist_ok=True)
    rows: List[Dict[str, Any]] = []
    if history_file.exists() and history_file.is_file():
        for line in history_file.read_text(encoding="utf-8").splitlines():
            text = line.strip()
            if not text:
                continue
            try:
                payload = json.loads(text)
            except json.JSONDecodeError:
                continue
            if isinstance(payload, Mapping):
                rows.append(dict(payload))

    run_id = str(snapshot.get("run_id", "")).strip()
    if run_id:
        rows = [row for row in rows if str(row.get("run_id", "")).strip() != run_id]
    rows.append(dict(snapshot))

    limit = max(0, int(max_entries))
    if limit > 0 and len(rows) > limit:
        rows = rows[-limit:]

    tmp_file = history_file.with_suffix(history_file.suffix + ".tmp")
    text = "".join(json.dumps(row, ensure_ascii=False) + "\n" for row in rows)
    tmp_file.write_text(text, encoding="utf-8")
    tmp_file.replace(history_file)


def main() -> None:
    parser = argparse.ArgumentParser(description="Run bot+report cycle in one command")
    parser.add_argument("--config-file", required=True)
    parser.add_argument("--signals-file", default="")
    parser.add_argument("--signals-meta-file", default="")
    parser.add_argument("--openclaw-cmd", default="")
    parser.add_argument("--markets-file", default="")
    parser.add_argument("--max-open-positions", type=int, default=0)
    parser.add_argument("--max-daily-entries", type=int, default=0)
    parser.add_argument("--max-daily-loss-streak", type=int, default=0)
    parser.add_argument("--max-daily-realized-loss-usd", type=float, default=0.0)
    parser.add_argument("--min-liquidity-usd", type=float, default=0.0)
    parser.add_argument("--min-volume-usd", type=float, default=0.0)
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
    parser.add_argument("--live-execution", action="store_true")
    parser.add_argument("--live-order-type", default="GTC")
    parser.add_argument("--live-max-orders", type=int, default=0)
    parser.add_argument("--live-min-expected-value-usd", type=float, default=0.0)
    parser.add_argument("--live-min-stake-usd", type=float, default=1.0)
    parser.add_argument("--live-fail-on-error", action="store_true")
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
        min_liquidity_usd=max(0.0, args.min_liquidity_usd),
        min_volume_usd=max(0.0, args.min_volume_usd),
        limit=max(1, args.limit),
        run_id=run_id,
        plan_file=paths["plan_file"],
        journal_file=paths["journal_file"],
    )
    bot_output = _run_json_command(bot_cmd)
    plan_summary = bot_output.get("summary", {})
    run_posttrade = should_run_posttrade_steps(plan_summary)
    execution_result: Dict[str, Any] = {}
    if args.live_execution:
        if run_posttrade:
            execution_report = output_dir / f"execution_{run_date}_{run_id}.json"
            execute_cmd = build_execute_command(
                base_dir=base_dir,
                plan_file=paths["plan_file"],
                run_id=run_id,
                run_date=run_date,
                order_type=args.live_order_type,
                max_orders=max(0, args.live_max_orders),
                min_expected_value_usd=max(0.0, args.live_min_expected_value_usd),
                min_stake_usd=max(0.0, args.live_min_stake_usd),
                fail_on_error=args.live_fail_on_error,
                write_report=execution_report,
            )
            execution_result = _run_json_command(execute_cmd)
        else:
            execution_result = {
                "ok": True,
                "skipped": True,
                "reason": "no_entries",
            }

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
        "signal_summary": load_signal_summary(
            signals_meta_file=args.signals_meta_file,
            signals_file=args.signals_file,
        ),
        "live_execution_enabled": bool(args.live_execution),
        "execution": execution_result,
        "posttrade_enabled": run_posttrade,
        "settlement_source": settlement_source,
        "settlement_fetch": settlement_fetch_result,
        "report_summary": report_output,
        "autotune_candidate_config": autotune_candidate_path,
        "autotune_summary": autotune_result.get("best", {}),
        "config_apply_result": config_apply_result,
    }
    latest_status_file = output_dir / "latest_status.json"
    latest_snapshot = build_latest_status_snapshot(
        result=result,
        updated_at=datetime.now(timezone.utc).isoformat(),
    )
    write_latest_status_snapshot(status_file=latest_status_file, snapshot=latest_snapshot)
    status_history_file = output_dir / "status_history.jsonl"
    append_status_history(history_file=status_history_file, snapshot=latest_snapshot, max_entries=2000)
    result["latest_status_file"] = str(latest_status_file)
    result["status_history_file"] = str(status_history_file)
    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
