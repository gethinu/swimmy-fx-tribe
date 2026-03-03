#!/usr/bin/env python3
"""Bridge Armada replica selections into MT5 trial configs for xau_autobot."""

from __future__ import annotations

import argparse
import json
import re
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, Iterable, Optional

def repo_root() -> Path:
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parents[2]


ROOT = repo_root()
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

try:
    from tools.xau_autobot import validate_trade_comment
except Exception:
    from xau_autobot import validate_trade_comment  # type: ignore


SUPPORTED_TIMEFRAME_MAP: Dict[int, str] = {
    1: "M1",
    5: "M5",
    15: "M15",
    20: "M20",
    30: "M30",
    60: "H1",
    240: "H4",
}


def _safe_int(value: object, default: int = 0) -> int:
    try:
        return int(value)
    except (TypeError, ValueError):
        return default


def _safe_float(value: object, default: float = 0.0) -> float:
    try:
        return float(value)
    except (TypeError, ValueError):
        return default


def _normalize_indicator_set(raw: Iterable[str] | str | None) -> set[str]:
    if raw is None:
        return set()
    if isinstance(raw, str):
        tokens = [token.strip().lower() for token in raw.split(",")]
    else:
        tokens = [str(token).strip().lower() for token in raw]
    return {token for token in tokens if token}


def _sanitize_token(text: str) -> str:
    return re.sub(r"[^A-Za-z0-9_]+", "_", str(text or "")).strip("_")


def _build_comment(*, run_tag: str, player_key: str, indicator: str, timeframe_min: int, ordinal: int) -> str:
    compact_tag = _sanitize_token(run_tag)[-8:] or "tag"
    compact_player = (_sanitize_token(player_key).lower() or "plyr")[:8]
    compact_indicator = (_sanitize_token(indicator).lower() or "ind")[:4]
    raw = f"a{compact_tag}_{compact_player}_{compact_indicator}{timeframe_min}_{ordinal}"
    # MT5 comment limit is 31 chars; keep deterministic and valid.
    normalized = validate_trade_comment(raw[:31])
    if not normalized:
        return "armada_mt5_bridge"
    return normalized


def _build_trial_config(
    *,
    symbol: str,
    timeframe: str,
    fast_ema: int,
    slow_ema: int,
    pullback_atr: float,
    sl_atr: float,
    tp_atr: float,
    session_start_hour_utc: int,
    session_end_hour_utc: int,
    min_atr_ratio_to_median: float,
    max_atr_ratio_to_median: float,
    max_spread_points: float,
    magic: int,
    comment: str,
) -> dict:
    return {
        "symbol": str(symbol),
        "timeframe": str(timeframe),
        "bars": 800,
        "fast_ema": int(fast_ema),
        "slow_ema": int(slow_ema),
        "atr_period": 14,
        "strategy_mode": "trend",
        "regime_trend_threshold": 1.2,
        "pullback_atr": float(pullback_atr),
        "reversion_atr": 0.8,
        "sl_atr": float(sl_atr),
        "tp_atr": float(tp_atr),
        "reversion_sl_atr": 1.2,
        "reversion_tp_atr": 1.2,
        "lot": 0.01,
        "max_spread_points": float(max_spread_points),
        "max_positions": 1,
        "session_start_hour_utc": int(session_start_hour_utc),
        "session_end_hour_utc": int(session_end_hour_utc),
        "atr_filter_window": 288,
        "atr_filter_min_samples": 120,
        "min_atr_ratio_to_median": float(min_atr_ratio_to_median),
        "max_atr_ratio_to_median": float(max_atr_ratio_to_median),
        "deviation": 30,
        "magic": int(magic),
        "comment": str(comment),
        "dry_run": True,
        "once": True,
        "poll_seconds": 10,
        "max_cycles": 0,
    }


def build_mt5_bridge_plan(
    *,
    report: dict,
    run_tag: str,
    config_dir: Path,
    top_per_player: int,
    allowed_indicators: Iterable[str] | str | None,
    symbol: str,
    magic_base: int,
    pullback_atr: float,
    session_start_hour_utc: int,
    session_end_hour_utc: int,
    min_atr_ratio_to_median: float,
    max_atr_ratio_to_median: float,
    max_spread_points: float,
) -> dict:
    config_dir = Path(config_dir)
    config_dir.mkdir(parents=True, exist_ok=True)
    allowed = _normalize_indicator_set(allowed_indicators) or {"ema"}
    players = report.get("players") if isinstance(report, dict) else []
    if not isinstance(players, list):
        players = []

    skipped = {
        "unsupported_indicator": 0,
        "unsupported_timeframe": 0,
        "invalid_candidate": 0,
    }
    runs = []
    planned_count = 0
    top_n = max(1, int(top_per_player))

    for player in players:
        if not isinstance(player, dict):
            continue
        profile = player.get("profile") if isinstance(player.get("profile"), dict) else {}
        player_key = str(profile.get("key", "player")).strip() or "player"
        selected = player.get("selected") if isinstance(player.get("selected"), list) else []
        for idx, row in enumerate(selected[:top_n]):
            if not isinstance(row, dict):
                continue
            candidate = row.get("candidate") if isinstance(row.get("candidate"), dict) else {}
            indicator = str(candidate.get("indicator_type", "")).strip().lower()
            if indicator not in allowed:
                skipped["unsupported_indicator"] += 1
                continue

            timeframe_min = _safe_int(candidate.get("timeframe"), 0)
            timeframe = SUPPORTED_TIMEFRAME_MAP.get(timeframe_min, "")
            if timeframe == "":
                skipped["unsupported_timeframe"] += 1
                continue

            fast_ema = _safe_int(candidate.get("sma_short"), 0)
            slow_ema = _safe_int(candidate.get("sma_long"), 0)
            sl_atr = _safe_float(candidate.get("sl"), 0.0)
            tp_atr = _safe_float(candidate.get("tp"), 0.0)
            if fast_ema <= 0 or slow_ema <= 0 or fast_ema >= slow_ema or sl_atr <= 0.0 or tp_atr <= sl_atr:
                skipped["invalid_candidate"] += 1
                continue

            ordinal = planned_count + 1
            comment = _build_comment(
                run_tag=run_tag,
                player_key=player_key,
                indicator=indicator,
                timeframe_min=timeframe_min,
                ordinal=ordinal,
            )
            magic = int(magic_base) + planned_count
            run_id = f"trial_v2_armada_{_sanitize_token(run_tag)}_{_sanitize_token(player_key)}_{ordinal}"
            config_name = (
                f"xau_autobot.armada_mt5_{_sanitize_token(run_tag)}_"
                f"{_sanitize_token(player_key)}_{ordinal:02d}.json"
            )
            config_path = config_dir / config_name

            cfg = _build_trial_config(
                symbol=symbol,
                timeframe=timeframe,
                fast_ema=fast_ema,
                slow_ema=slow_ema,
                pullback_atr=pullback_atr,
                sl_atr=sl_atr,
                tp_atr=tp_atr,
                session_start_hour_utc=session_start_hour_utc,
                session_end_hour_utc=session_end_hour_utc,
                min_atr_ratio_to_median=min_atr_ratio_to_median,
                max_atr_ratio_to_median=max_atr_ratio_to_median,
                max_spread_points=max_spread_points,
                magic=magic,
                comment=comment,
            )
            config_path.write_text(json.dumps(cfg, ensure_ascii=True, indent=2) + "\n", encoding="utf-8")

            rel_config_path = str(config_path)
            start_command = (
                f"XAU_AUTOBOT_TRIAL_RUN_ID={run_id} "
                f"XAU_AUTOBOT_TRIAL_CONFIG={rel_config_path} "
                "tools/xau_autobot_trial_v2_start.sh"
            )
            eval_command = (
                f"XAU_AUTOBOT_TRIAL_RUN_ID={run_id} "
                f"XAU_AUTOBOT_TRIAL_CONFIG={rel_config_path} "
                "XAU_AUTOBOT_TRIAL_WATCHDOG_ENABLED=1 "
                "tools/xau_autobot_trial_v2_eval.sh"
            )
            runs.append(
                {
                    "run_id": run_id,
                    "player_key": player_key,
                    "selection_index": idx,
                    "indicator_type": indicator,
                    "timeframe_minutes": timeframe_min,
                    "timeframe": timeframe,
                    "magic": magic,
                    "comment": comment,
                    "config_path": rel_config_path,
                    "start_command": start_command,
                    "eval_command": eval_command,
                }
            )
            planned_count += 1

    generated_at = datetime.now(timezone.utc).isoformat()
    return {
        "generated_at_utc": generated_at,
        "input_summary": {
            "players": len(players),
            "top_per_player": top_n,
            "allowed_indicators": sorted(allowed),
            "symbol": str(symbol),
            "magic_base": int(magic_base),
            "run_tag": str(run_tag),
        },
        "summary": {
            "planned_runs": planned_count,
            "skipped": skipped,
        },
        "runs": runs,
    }


def parse_args(argv: Optional[Iterable[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build MT5 trial configs from Armada replica report")
    parser.add_argument("--replica-report", required=True, help="Path to armada_player_replica report JSON")
    parser.add_argument(
        "--output",
        default="data/reports/armada_mt5_bridge_plan_latest.json",
        help="Output plan JSON path",
    )
    parser.add_argument(
        "--config-dir",
        default="tools/configs",
        help="Directory where generated xau_autobot trial configs are written",
    )
    parser.add_argument(
        "--run-tag",
        default=datetime.now(timezone.utc).strftime("%Y%m%d"),
        help="Run tag embedded into run_id/comment/config names",
    )
    parser.add_argument("--top-per-player", type=int, default=1, help="How many selected candidates per player to convert")
    parser.add_argument(
        "--allowed-indicators",
        default="ema",
        help="Comma-separated Armada indicator types allowed for MT5 bridge (default: ema)",
    )
    parser.add_argument("--symbol", default="XAUUSD", help="MT5 symbol for generated trial configs")
    parser.add_argument("--magic-base", type=int, default=561000, help="Base magic number for generated runs")
    parser.add_argument("--pullback-atr", type=float, default=0.2, help="Pullback ATR used by xau_autobot trend mode")
    parser.add_argument("--session-start-hour-utc", type=int, default=7, help="Session start hour (UTC)")
    parser.add_argument("--session-end-hour-utc", type=int, default=19, help="Session end hour (UTC)")
    parser.add_argument("--min-atr-ratio", type=float, default=0.0, help="min_atr_ratio_to_median")
    parser.add_argument("--max-atr-ratio", type=float, default=999.0, help="max_atr_ratio_to_median")
    parser.add_argument("--max-spread-points", type=float, default=80.0, help="max_spread_points")
    return parser.parse_args(list(argv) if argv is not None else None)


def main(argv: Optional[Iterable[str]] = None) -> int:
    args = parse_args(argv)
    report_path = Path(args.replica_report)
    if not report_path.exists():
        raise FileNotFoundError(f"replica report not found: {report_path}")
    report = json.loads(report_path.read_text(encoding="utf-8"))
    if not isinstance(report, dict):
        raise RuntimeError("replica report must be a JSON object")

    plan = build_mt5_bridge_plan(
        report=report,
        run_tag=str(args.run_tag),
        config_dir=Path(args.config_dir),
        top_per_player=int(args.top_per_player),
        allowed_indicators=str(args.allowed_indicators),
        symbol=str(args.symbol),
        magic_base=int(args.magic_base),
        pullback_atr=float(args.pullback_atr),
        session_start_hour_utc=int(args.session_start_hour_utc),
        session_end_hour_utc=int(args.session_end_hour_utc),
        min_atr_ratio_to_median=float(args.min_atr_ratio),
        max_atr_ratio_to_median=float(args.max_atr_ratio),
        max_spread_points=float(args.max_spread_points),
    )

    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(plan, ensure_ascii=True, indent=2) + "\n", encoding="utf-8")
    print(f"saved plan: {output_path}")
    print(
        json.dumps(
            {
                "planned_runs": plan["summary"]["planned_runs"],
                "skipped": plan["summary"]["skipped"],
            },
            ensure_ascii=True,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
