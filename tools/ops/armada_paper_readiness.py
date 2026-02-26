#!/usr/bin/env python3
"""Generate Armada paper-readiness report (L3 evidence gate)."""

from __future__ import annotations

import argparse
import json
import math
import sqlite3
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Iterable, Mapping, Sequence


DEFAULT_PRIMARY_MODES: tuple[str, ...] = ("SHADOW", "PAPER")
DEFAULT_COMPAT_MODES: tuple[str, ...] = ("SHADOW", "PAPER", "LIVE")
DEFAULT_EXCLUDED_STRATEGY_PREFIXES: tuple[str, ...] = ("Synthetic-Close-Probe-",)
DEFAULT_RUNTIME_WINDOW_TRADES = 20
UNIVERSAL_UNIX_OFFSET = 2208988800

DEFAULT_FORWARD_CONTRACT = {
    "forward_days_min": 30,
    "forward_trades_min": 20,
    "forward_sharpe_min": 0.7,
    "forward_pf_min": 1.5,
}


def _to_float(value: Any, default: float = 0.0) -> float:
    try:
        if value is None:
            return default
        return float(value)
    except (TypeError, ValueError):
        return default


def _to_int(value: Any, default: int = 0) -> int:
    try:
        if value is None:
            return default
        return int(value)
    except (TypeError, ValueError):
        return default


def _save_json(path: Path, payload: Mapping[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    tmp.replace(path)


def _load_json(path: Path) -> dict[str, Any]:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return {}


def _normalize_excluded_strategy_prefixes(prefixes: Sequence[str] | None) -> tuple[str, ...]:
    if not prefixes:
        return ()
    normalized: list[str] = []
    for raw in prefixes:
        token = str(raw or "").strip()
        if token and token not in normalized:
            normalized.append(token)
    return tuple(normalized)


def _build_excluded_strategy_clause(prefixes: Sequence[str], *, column: str) -> tuple[str, tuple[Any, ...]]:
    normalized = _normalize_excluded_strategy_prefixes(prefixes)
    if not normalized:
        return "", ()
    clauses: list[str] = []
    params: list[str] = []
    for prefix in normalized:
        clauses.append(f"UPPER(COALESCE({column}, '')) NOT LIKE ?")
        params.append(prefix.upper() + "%")
    return " AND " + " AND ".join(clauses), tuple(params)


def _rows_for_modes(
    conn: sqlite3.Connection,
    modes: Sequence[str],
    *,
    excluded_strategy_prefixes: Sequence[str] | None = None,
) -> list[sqlite3.Row]:
    if not modes:
        return []
    placeholders = ",".join("?" for _ in modes)
    excluded_clause, excluded_params = _build_excluded_strategy_clause(
        excluded_strategy_prefixes or (),
        column="strategy_name",
    )
    sql = (
        "SELECT id, timestamp, pnl, execution_mode "
        "FROM trade_logs "
        f"WHERE UPPER(COALESCE(execution_mode, 'LIVE')) IN ({placeholders}) "
        f"{excluded_clause} "
        "ORDER BY COALESCE(timestamp, id) ASC, id ASC"
    )
    rows = conn.execute(
        sql,
        tuple(mode.upper() for mode in modes) + excluded_params,
    ).fetchall()
    return rows


def _compute_trade_metrics(rows: Iterable[sqlite3.Row]) -> dict[str, Any]:
    pnls = [_to_float(row["pnl"], 0.0) for row in rows]
    trades = len(pnls)
    if trades == 0:
        return {
            "trades": 0,
            "net_pnl": 0.0,
            "profit_factor": None,
            "win_rate": None,
            "latest_loss_streak": 0,
            "max_loss_streak": 0,
            "max_drawdown_abs": 0.0,
        }

    net_pnl = float(sum(pnls))
    wins = [p for p in pnls if p > 0.0]
    losses = [p for p in pnls if p < 0.0]
    gross_profit = float(sum(wins))
    gross_loss_abs = float(abs(sum(losses)))
    if gross_loss_abs > 0.0:
        profit_factor: float | None = gross_profit / gross_loss_abs
    else:
        profit_factor = None
    win_rate = float(len(wins) / trades)

    max_loss_streak = 0
    current_loss_streak = 0
    for pnl in pnls:
        if pnl < 0.0:
            current_loss_streak += 1
            if current_loss_streak > max_loss_streak:
                max_loss_streak = current_loss_streak
        else:
            current_loss_streak = 0

    latest_loss_streak = 0
    for pnl in reversed(pnls):
        if pnl < 0.0:
            latest_loss_streak += 1
        else:
            break

    cum = 0.0
    peak = 0.0
    max_drawdown_abs = 0.0
    for pnl in pnls:
        cum += pnl
        if cum > peak:
            peak = cum
        dd = peak - cum
        if dd > max_drawdown_abs:
            max_drawdown_abs = dd

    return {
        "trades": trades,
        "net_pnl": net_pnl,
        "profit_factor": profit_factor,
        "win_rate": win_rate,
        "latest_loss_streak": latest_loss_streak,
        "max_loss_streak": max_loss_streak,
        "max_drawdown_abs": max_drawdown_abs,
    }


def _trade_timestamp_to_unix(raw_timestamp: Any) -> int:
    ts = _to_int(raw_timestamp, 0)
    if ts <= 0:
        return 0
    # trade_logs currently stores Lisp universal-time (1900 epoch). Keep Unix inputs
    # unchanged so mixed test fixtures remain valid.
    if ts >= UNIVERSAL_UNIX_OFFSET:
        return ts - UNIVERSAL_UNIX_OFFSET
    return ts


def _compute_monthly_performance(rows: Iterable[sqlite3.Row], *, baseline_equity: float) -> dict[str, Any]:
    buckets: dict[str, dict[str, Any]] = {}
    for row in rows:
        ts_unix = _trade_timestamp_to_unix(row["timestamp"])
        if ts_unix <= 0:
            continue
        month = datetime.fromtimestamp(ts_unix, tz=timezone.utc).strftime("%Y-%m")
        bucket = buckets.setdefault(
            month,
            {
                "month": month,
                "trade_count": 0,
                "net_pnl": 0.0,
            },
        )
        bucket["trade_count"] = _to_int(bucket.get("trade_count"), 0) + 1
        bucket["net_pnl"] = _to_float(bucket.get("net_pnl"), 0.0) + _to_float(row["pnl"], 0.0)

    ordered = [buckets[k] for k in sorted(buckets.keys())]
    base = _to_float(baseline_equity, 0.0)
    for row in ordered:
        row["net_pnl"] = _to_float(row.get("net_pnl"), 0.0)
        row["return_pct"] = (_to_float(row["net_pnl"], 0.0) / base * 100.0) if base > 0.0 else None

    latest = ordered[-1] if ordered else None
    return {
        "baseline_equity": base,
        "months": ordered,
        "latest_month": latest,
    }


def _compute_slippage_metrics(
    conn: sqlite3.Connection,
    *,
    excluded_strategy_prefixes: Sequence[str] | None = None,
) -> tuple[int, float | None]:
    excluded_clause, excluded_params = _build_excluded_strategy_clause(
        excluded_strategy_prefixes or (),
        column="strategy_name",
    )
    sql = (
        "SELECT sample_abs_pips "
        "FROM dryrun_slippage_samples "
        "WHERE sample_abs_pips IS NOT NULL "
        f"{excluded_clause} "
        "ORDER BY observed_at ASC, id ASC"
    )
    rows = conn.execute(sql, excluded_params).fetchall()
    samples = [_to_float(row[0], 0.0) for row in rows]
    if not samples:
        return 0, None
    ordered = sorted(samples)
    rank_index = max(0, int(math.ceil(0.95 * len(ordered))) - 1)
    p95 = float(ordered[rank_index])
    return len(ordered), p95


def _collect_players(strict_payload: Mapping[str, Any], *, decision: str) -> list[dict[str, Any]]:
    raw_players = strict_payload.get("players", [])
    players: list[dict[str, Any]] = []
    if isinstance(raw_players, list):
        for idx, entry in enumerate(raw_players):
            if not isinstance(entry, Mapping):
                continue
            player = str(entry.get("player") or f"player_{idx + 1}")
            base_classification = str(entry.get("classification") or "保留")
            if decision == "GO":
                if base_classification == "再探索":
                    classification = "再探索"
                    l3_ready = False
                    reason = "strict blocker remains"
                else:
                    classification = "投入可"
                    l3_ready = True
                    reason = "paper evidence reached"
            else:
                classification = "保留"
                l3_ready = False
                reason = "paper evidence not reached"
            players.append(
                {
                    "player": player,
                    "classification": classification,
                    "l3_ready": l3_ready,
                    "reason": reason,
                }
            )
    return players


def build_armada_paper_readiness_report(
    *,
    db_path: Path,
    b1r_summary_path: Path,
    deploy_readiness_path: Path,
    deploy_readiness_proxy_path: Path,
    title: str,
    paper_min_trades: int = 20,
    slippage_min_samples: int = 20,
    runtime_net_pnl_min: float = 0.0,
    runtime_latest_loss_streak_max: int = 3,
    runtime_window_trades: int = DEFAULT_RUNTIME_WINDOW_TRADES,
    drawdown_hard_dd_percent_max: float = 12.0,
    drawdown_weekly_dd_percent_max: float = 4.0,
    slippage_p95_abs_pips_max: float = 3.0,
    monthly_baseline_equity: float = 100000.0,
    monthly_return_target_pct: float | None = None,
    excluded_strategy_prefixes: Sequence[str] = DEFAULT_EXCLUDED_STRATEGY_PREFIXES,
    generated_at: str | None = None,
) -> dict[str, Any]:
    strict_payload = _load_json(deploy_readiness_path)
    normalized_excluded_prefixes = _normalize_excluded_strategy_prefixes(excluded_strategy_prefixes)

    with sqlite3.connect(str(db_path)) as conn:
        conn.row_factory = sqlite3.Row
        primary_rows = _rows_for_modes(
            conn,
            DEFAULT_PRIMARY_MODES,
            excluded_strategy_prefixes=normalized_excluded_prefixes,
        )
        compat_rows = _rows_for_modes(
            conn,
            DEFAULT_COMPAT_MODES,
            excluded_strategy_prefixes=normalized_excluded_prefixes,
        )
        paper_primary_metrics = _compute_trade_metrics(primary_rows)
        paper_primary_monthly = _compute_monthly_performance(
            primary_rows,
            baseline_equity=monthly_baseline_equity,
        )
        if runtime_window_trades <= 0:
            runtime_rows = primary_rows
        else:
            runtime_rows = primary_rows[-runtime_window_trades:]
        paper_runtime_metrics = _compute_trade_metrics(runtime_rows)
        compat_metrics = _compute_trade_metrics(compat_rows)
        slippage_sample_count, slippage_p95_abs_pips = _compute_slippage_metrics(
            conn,
            excluded_strategy_prefixes=normalized_excluded_prefixes,
        )

    blockers: list[str] = []
    if paper_primary_metrics["trades"] < paper_min_trades:
        blockers.append(f"paper_evidence_shortage: {paper_primary_metrics['trades']}/{paper_min_trades}")
    if slippage_sample_count < slippage_min_samples:
        blockers.append(f"slippage_samples_shortage: {slippage_sample_count}/{slippage_min_samples}")

    if paper_primary_metrics["trades"] >= paper_min_trades:
        if _to_float(paper_runtime_metrics["net_pnl"], 0.0) < runtime_net_pnl_min:
            blockers.append(
                f"runtime_net_pnl_below_min: {paper_runtime_metrics['net_pnl']:.6f}<{runtime_net_pnl_min:.6f}"
            )
        if _to_int(paper_runtime_metrics["latest_loss_streak"], 0) > runtime_latest_loss_streak_max:
            blockers.append(
                "runtime_latest_loss_streak_exceeded: "
                f"{paper_runtime_metrics['latest_loss_streak']}>{runtime_latest_loss_streak_max}"
            )
        if monthly_return_target_pct is not None:
            latest_month = paper_primary_monthly.get("latest_month")
            latest_month_return_pct = (
                _to_float(latest_month.get("return_pct"), 0.0) if isinstance(latest_month, Mapping) else None
            )
            if latest_month_return_pct is None:
                blockers.append("monthly_return_missing")
            elif latest_month_return_pct < float(monthly_return_target_pct):
                blockers.append(
                    f"monthly_return_below_target: {latest_month_return_pct:.6f}<{float(monthly_return_target_pct):.6f}"
                )

    if slippage_sample_count >= slippage_min_samples and slippage_p95_abs_pips is not None:
        if slippage_p95_abs_pips > slippage_p95_abs_pips_max:
            blockers.append(
                f"slippage_p95_exceeded: {slippage_p95_abs_pips:.4f}>{slippage_p95_abs_pips_max:.4f}"
            )

    decision = "GO" if not blockers else "HOLD"
    report_players = _collect_players(strict_payload, decision=decision)

    generated = generated_at or datetime.now(timezone.utc).isoformat()
    output_title = title.strip() if title.strip() else f"armada_paper_readiness_{datetime.now(timezone.utc).strftime('%Y%m%d')}"

    report = {
        "generated_at": generated,
        "title": output_title,
        "criteria": {
            "L3": "L2達成 + paper 20 tradesで重大逸脱なし（DD/スリッページ/連敗/実現PnL）",
            "decision": "GO/HOLD (fail-closed)",
        },
        "thresholds": {
            "paper_min_trades": paper_min_trades,
            "forward_contract": dict(DEFAULT_FORWARD_CONTRACT),
            "runtime_guard": {
                "net_pnl_min": runtime_net_pnl_min,
                "latest_loss_streak_max": runtime_latest_loss_streak_max,
                "window_trades": runtime_window_trades,
            },
            "drawdown_guard": {
                "hard_dd_percent_max": drawdown_hard_dd_percent_max,
                "weekly_dd_percent_max": drawdown_weekly_dd_percent_max,
            },
            "slippage_guard": {
                "p95_abs_pips_max": slippage_p95_abs_pips_max,
                "min_samples": slippage_min_samples,
            },
            "monthly_guard": {
                "baseline_equity": monthly_baseline_equity,
                "target_return_pct": monthly_return_target_pct,
            },
        },
        "inputs": {
            "trade_logs_db": str(db_path.resolve()),
            "execution_modes_primary": list(DEFAULT_PRIMARY_MODES),
            "execution_modes_compat": list(DEFAULT_COMPAT_MODES),
            "excluded_strategy_prefixes": list(normalized_excluded_prefixes),
            "b1r_summary": str(b1r_summary_path.resolve()),
            "deploy_readiness_fix10": str(deploy_readiness_path.resolve()),
            "deploy_readiness_proxy_fix10": str(deploy_readiness_proxy_path.resolve()),
        },
            "summary": {
                "decision": decision,
                "paper_trade_count": paper_primary_metrics["trades"],
                "paper_trade_required": paper_min_trades,
                "runtime_window_trade_count": paper_runtime_metrics["trades"],
                "runtime_window_net_pnl": paper_runtime_metrics["net_pnl"],
                "slippage_sample_count": slippage_sample_count,
                "slippage_samples_required": slippage_min_samples,
            "latest_month": (
                paper_primary_monthly["latest_month"]["month"] if paper_primary_monthly["latest_month"] else None
            ),
            "latest_month_return_pct": (
                paper_primary_monthly["latest_month"]["return_pct"]
                if paper_primary_monthly["latest_month"]
                else None
            ),
            "latest_month_return_target_pct": monthly_return_target_pct,
            "blockers": blockers,
        },
        "metrics": {
            "paper_primary": paper_primary_metrics,
            "paper_runtime_window": paper_runtime_metrics,
            "paper_primary_monthly": paper_primary_monthly,
            "compat_with_live": compat_metrics,
            "slippage": {
                "sample_count": slippage_sample_count,
                "p95_abs_pips": slippage_p95_abs_pips,
                "threshold_abs_pips": slippage_p95_abs_pips_max,
            },
        },
        "players": report_players,
    }
    return report


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate Armada paper-readiness report")
    parser.add_argument("--db-path", default="data/memory/swimmy.db")
    parser.add_argument("--b1r-summary", required=True)
    parser.add_argument("--deploy-readiness", required=True)
    parser.add_argument("--deploy-readiness-proxy", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--title", default="")
    parser.add_argument("--paper-min-trades", type=int, default=20)
    parser.add_argument("--slippage-min-samples", type=int, default=20)
    parser.add_argument("--runtime-net-pnl-min", type=float, default=0.0)
    parser.add_argument("--runtime-latest-loss-streak-max", type=int, default=3)
    parser.add_argument("--runtime-window-trades", type=int, default=DEFAULT_RUNTIME_WINDOW_TRADES)
    parser.add_argument("--drawdown-hard-dd-percent-max", type=float, default=12.0)
    parser.add_argument("--drawdown-weekly-dd-percent-max", type=float, default=4.0)
    parser.add_argument("--slippage-p95-abs-pips-max", type=float, default=3.0)
    parser.add_argument("--monthly-baseline-equity", type=float, default=100000.0)
    parser.add_argument("--monthly-return-target-pct", type=float, default=None)
    parser.add_argument(
        "--exclude-strategy-prefix",
        action="append",
        default=[],
        help="Exclude rows where strategy_name starts with this prefix (repeatable).",
    )
    parser.add_argument(
        "--include-synthetic-probes",
        action="store_true",
        help="Disable default exclusion of Synthetic-Close-Probe-* rows.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()

    output_path = Path(args.output)
    excluded_prefixes = list(args.exclude_strategy_prefix)
    if not args.include_synthetic_probes:
        for prefix in DEFAULT_EXCLUDED_STRATEGY_PREFIXES:
            if prefix not in excluded_prefixes:
                excluded_prefixes.append(prefix)
    report = build_armada_paper_readiness_report(
        db_path=Path(args.db_path),
        b1r_summary_path=Path(args.b1r_summary),
        deploy_readiness_path=Path(args.deploy_readiness),
        deploy_readiness_proxy_path=Path(args.deploy_readiness_proxy),
        title=args.title or output_path.stem,
        paper_min_trades=args.paper_min_trades,
        slippage_min_samples=args.slippage_min_samples,
        runtime_net_pnl_min=args.runtime_net_pnl_min,
        runtime_latest_loss_streak_max=args.runtime_latest_loss_streak_max,
        runtime_window_trades=args.runtime_window_trades,
        drawdown_hard_dd_percent_max=args.drawdown_hard_dd_percent_max,
        drawdown_weekly_dd_percent_max=args.drawdown_weekly_dd_percent_max,
        slippage_p95_abs_pips_max=args.slippage_p95_abs_pips_max,
        monthly_baseline_equity=args.monthly_baseline_equity,
        monthly_return_target_pct=args.monthly_return_target_pct,
        excluded_strategy_prefixes=tuple(excluded_prefixes),
    )
    _save_json(output_path, report)
    print(
        "armada_paper_readiness: "
        f"decision={report['summary']['decision']} "
        f"paper={report['summary']['paper_trade_count']}/{report['summary']['paper_trade_required']} "
        f"slippage={report['summary']['slippage_sample_count']}/{report['summary']['slippage_samples_required']}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
