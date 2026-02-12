#!/usr/bin/env python3
"""Systemd entrypoint for polymarket_openclaw_cycle.py (env-driven)."""

from __future__ import annotations

import os
import subprocess
import sys
import json
import shlex
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Mapping


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


def _env_bool(env: Mapping[str, str], key: str, default: bool = False) -> bool:
    text = str(env.get(key, "")).strip().lower()
    if not text:
        return default
    return text in {"1", "true", "yes", "on"}


def _env_int(env: Mapping[str, str], key: str, default: int) -> int:
    text = str(env.get(key, "")).strip()
    if not text:
        return default
    try:
        return int(text)
    except ValueError:
        return default


def _env_float(env: Mapping[str, str], key: str, default: float) -> float:
    text = str(env.get(key, "")).strip()
    if not text:
        return default
    try:
        return float(text)
    except ValueError:
        return default


def _env_csv(env: Mapping[str, str], key: str, default: str = "") -> List[str]:
    text = str(env.get(key, "")).strip()
    if not text:
        text = default
    if not text:
        return []
    return [item.strip() for item in text.split(",") if item and item.strip()]


def build_heuristic_openclaw_cmd(*, env: Mapping[str, str], base_dir: Path) -> str:
    script = env.get("POLYCLAW_HEURISTIC_SCRIPT", str(base_dir / "tools" / "openclaw_signal_heuristic.py")).strip()
    gamma_url = env.get("POLYCLAW_HEURISTIC_GAMMA_URL", "https://gamma-api.polymarket.com/markets").strip()
    heuristic_limit = _env_int(env, "POLYCLAW_HEURISTIC_LIMIT", _env_int(env, "POLYCLAW_LIMIT", 250))
    keywords = _env_csv(
        env,
        "POLYCLAW_HEURISTIC_QUESTION_KEYWORDS",
        default="nba,nfl,mlb,nhl,ncaa,lakers,warriors,chiefs,yankees",
    )
    parts = [
        sys.executable,
        script,
        "--gamma-url",
        gamma_url,
        "--limit",
        str(max(1, heuristic_limit)),
    ]
    for keyword in keywords:
        parts.extend(["--question-keyword", keyword])
    return " ".join(shlex.quote(part) for part in parts)


def build_cycle_args(*, env: Mapping[str, str], base_dir: Path) -> List[str]:
    config_file = env.get(
        "POLYCLAW_CONFIG_FILE",
        str(base_dir / "tools" / "configs" / "polymarket_openclaw.contrarian.example.json"),
    ).strip()
    signals_file = env.get("POLYCLAW_SIGNALS_FILE", "").strip()
    openclaw_cmd = env.get("POLYCLAW_OPENCLAW_CMD", "").strip()
    markets_file = env.get("POLYCLAW_MARKETS_FILE", "").strip()
    output_dir = env.get(
        "POLYCLAW_OUTPUT_DIR",
        str(base_dir / "data" / "reports" / "polymarket_openclaw"),
    ).strip()

    if not signals_file and not openclaw_cmd:
        raise ValueError("Set POLYCLAW_SIGNALS_FILE or POLYCLAW_OPENCLAW_CMD")

    limit = _env_int(env, "POLYCLAW_LIMIT", 250)
    auto_fetch = _env_bool(env, "POLYCLAW_AUTO_FETCH_SETTLEMENTS", True)
    autotune = _env_bool(env, "POLYCLAW_AUTOTUNE", True)
    allow_duplicate_open_markets = _env_bool(env, "POLYCLAW_ALLOW_DUPLICATE_OPEN_MARKETS", False)
    max_open_positions = _env_int(env, "POLYCLAW_MAX_OPEN_POSITIONS", 0)
    max_daily_entries = _env_int(env, "POLYCLAW_MAX_DAILY_ENTRIES", 0)
    max_daily_loss_streak = _env_int(env, "POLYCLAW_MAX_DAILY_LOSS_STREAK", 0)
    max_daily_realized_loss_usd = _env_float(env, "POLYCLAW_MAX_DAILY_REALIZED_LOSS_USD", 0.0)
    min_liquidity_usd = _env_float(env, "POLYCLAW_MIN_LIQUIDITY_USD", 0.0)
    min_volume_usd = _env_float(env, "POLYCLAW_MIN_VOLUME_USD", 0.0)
    autotune_min_trades = _env_int(env, "POLYCLAW_AUTOTUNE_MIN_TRADES", 10)
    autotune_apply_best = _env_bool(env, "POLYCLAW_AUTOTUNE_APPLY_BEST", False)
    autotune_apply_min_trades = _env_int(env, "POLYCLAW_AUTOTUNE_APPLY_MIN_TRADES", 20)
    autotune_apply_min_realized_pnl = _env_float(env, "POLYCLAW_AUTOTUNE_APPLY_MIN_REALIZED_PNL_USD", 1.0)
    autotune_apply_target_config = env.get("POLYCLAW_AUTOTUNE_APPLY_TARGET_CONFIG", "").strip()
    fee = _env_float(env, "POLYCLAW_FEE_BPS_PER_SIDE", 20.0)
    slippage = _env_float(env, "POLYCLAW_SLIPPAGE_BPS_PER_SIDE", 30.0)

    args = [
        sys.executable,
        str(base_dir / "tools" / "polymarket_openclaw_cycle.py"),
        "--config-file",
        config_file,
        "--output-dir",
        output_dir,
        "--limit",
        str(max(1, limit)),
        "--fee-bps-per-side",
        str(fee),
        "--slippage-bps-per-side",
        str(slippage),
        "--autotune-min-trades",
        str(max(1, autotune_min_trades)),
    ]

    if signals_file:
        args.extend(["--signals-file", signals_file])
    if openclaw_cmd:
        args.extend(["--openclaw-cmd", openclaw_cmd])
    if markets_file:
        args.extend(["--markets-file", markets_file])
    if auto_fetch:
        args.append("--auto-fetch-settlements")
    if autotune:
        args.append("--autotune")
    if allow_duplicate_open_markets:
        args.append("--allow-duplicate-open-markets")
    if max_open_positions > 0:
        args.extend(["--max-open-positions", str(max_open_positions)])
    if max_daily_entries > 0:
        args.extend(["--max-daily-entries", str(max_daily_entries)])
    if max_daily_loss_streak > 0:
        args.extend(["--max-daily-loss-streak", str(max_daily_loss_streak)])
    if max_daily_realized_loss_usd > 0.0:
        args.extend(["--max-daily-realized-loss-usd", str(max_daily_realized_loss_usd)])
    if min_liquidity_usd > 0.0:
        args.extend(["--min-liquidity-usd", str(min_liquidity_usd)])
    if min_volume_usd > 0.0:
        args.extend(["--min-volume-usd", str(min_volume_usd)])
    if autotune_apply_best:
        args.append("--autotune-apply-best")
        args.extend(["--autotune-apply-min-trades", str(max(1, autotune_apply_min_trades))])
        args.extend(["--autotune-apply-min-realized-pnl-usd", str(autotune_apply_min_realized_pnl)])
        if autotune_apply_target_config:
            args.extend(["--autotune-apply-target-config", autotune_apply_target_config])
    settlements_file = env.get("POLYCLAW_SETTLEMENTS_FILE", "").strip()
    if settlements_file:
        args.extend(["--settlements-file", settlements_file])
    return args


def build_signal_sync_args(*, env: Mapping[str, str], base_dir: Path) -> List[str]:
    openclaw_cmd = env.get("POLYCLAW_OPENCLAW_CMD", "").strip()
    if not openclaw_cmd and _env_bool(env, "POLYCLAW_USE_HEURISTIC_IF_NO_OPENCLAW_CMD", True):
        openclaw_cmd = build_heuristic_openclaw_cmd(env=env, base_dir=base_dir)
    signals_file = env.get(
        "POLYCLAW_SIGNALS_FILE",
        str(base_dir / "data" / "openclaw" / "signals.jsonl"),
    ).strip()
    meta_file = env.get(
        "POLYCLAW_SIGNALS_META_FILE",
        str(base_dir / "data" / "openclaw" / "signals.meta.json"),
    ).strip()
    min_signals = _env_int(env, "POLYCLAW_SIGNAL_SYNC_MIN_SIGNALS", 1)
    timeout_seconds = _env_int(env, "POLYCLAW_SIGNAL_SYNC_TIMEOUT_SECONDS", 30)
    if not openclaw_cmd:
        raise ValueError("POLYCLAW_OPENCLAW_CMD is required for signal sync")
    return [
        sys.executable,
        str(base_dir / "tools" / "openclaw_signal_sync.py"),
        "--openclaw-cmd",
        openclaw_cmd,
        "--signals-file",
        signals_file,
        "--meta-file",
        meta_file,
        "--min-signals",
        str(max(0, min_signals)),
        "--timeout-seconds",
        str(max(1, timeout_seconds)),
    ]


def _parse_iso8601(value: str):
    text = value.strip()
    if not text:
        return None
    if text.endswith("Z"):
        text = text[:-1] + "+00:00"
    try:
        return datetime.fromisoformat(text)
    except ValueError:
        return None


def _signal_count_from_file(path: Path) -> int:
    if not path.exists():
        return 0
    count = 0
    with path.open("r", encoding="utf-8") as handle:
        for raw in handle:
            if raw.strip():
                count += 1
    return count


def evaluate_signal_health(*, env: Mapping[str, str], base_dir: Path) -> Dict[str, object]:
    signals_file = Path(
        env.get("POLYCLAW_SIGNALS_FILE", str(base_dir / "data" / "openclaw" / "signals.jsonl"))
    )
    meta_file = Path(
        env.get("POLYCLAW_SIGNALS_META_FILE", str(base_dir / "data" / "openclaw" / "signals.meta.json"))
    )
    require_fresh = _env_bool(env, "POLYCLAW_REQUIRE_FRESH_SIGNALS", True)
    min_signal_count = _env_int(env, "POLYCLAW_MIN_SIGNAL_COUNT", 1)
    max_age_seconds = _env_int(env, "POLYCLAW_MAX_SIGNAL_AGE_SECONDS", 1800)
    min_agent_signal_count = _env_int(env, "POLYCLAW_MIN_AGENT_SIGNAL_COUNT", 0)
    min_agent_signal_ratio = _env_float(env, "POLYCLAW_MIN_AGENT_SIGNAL_RATIO", 0.0)

    now = datetime.now(timezone.utc)
    if not signals_file.exists():
        return {
            "ok": False,
            "reason": "signals_file_missing",
            "signal_count": 0,
            "agent_signal_count": 0,
            "agent_signal_ratio": 0.0,
            "age_seconds": None,
            "signals_file": str(signals_file),
        }

    signal_count = _signal_count_from_file(signals_file)
    source_counts: Dict[str, int] = {}
    agent_signal_count = 0
    agent_signal_ratio = 0.0
    updated_at = None
    if meta_file.exists():
        try:
            payload = json.loads(meta_file.read_text(encoding="utf-8"))
            if isinstance(payload, dict):
                meta_count = payload.get("signal_count")
                if isinstance(meta_count, int):
                    signal_count = meta_count
                meta_updated = payload.get("updated_at")
                if isinstance(meta_updated, str):
                    updated_at = _parse_iso8601(meta_updated)
                meta_sources = payload.get("source_counts")
                if isinstance(meta_sources, dict):
                    parsed_sources: Dict[str, int] = {}
                    for key, value in meta_sources.items():
                        name = str(key).strip()
                        if not name:
                            continue
                        if isinstance(value, int):
                            parsed_sources[name] = max(0, value)
                            continue
                        try:
                            parsed_sources[name] = max(0, int(value))
                        except (TypeError, ValueError):
                            continue
                    source_counts = parsed_sources
                meta_agent_count = payload.get("agent_signal_count")
                if isinstance(meta_agent_count, int):
                    agent_signal_count = max(0, meta_agent_count)
                else:
                    try:
                        agent_signal_count = max(0, int(meta_agent_count))
                    except (TypeError, ValueError):
                        pass
                meta_agent_ratio = payload.get("agent_signal_ratio")
                if isinstance(meta_agent_ratio, (int, float)):
                    agent_signal_ratio = float(meta_agent_ratio)
                else:
                    try:
                        agent_signal_ratio = float(meta_agent_ratio)
                    except (TypeError, ValueError):
                        pass
        except Exception:
            pass

    if source_counts and agent_signal_count <= 0:
        agent_signal_count = int(source_counts.get("openclaw_agent", 0))
    if signal_count > 0 and agent_signal_ratio <= 0.0:
        agent_signal_ratio = float(agent_signal_count) / float(signal_count)

    if updated_at is None:
        updated_at = datetime.fromtimestamp(signals_file.stat().st_mtime, timezone.utc)

    age_seconds = int(max(0.0, (now - updated_at).total_seconds()))
    if signal_count < max(0, min_signal_count):
        return {
            "ok": False,
            "reason": "low_signal_count",
            "signal_count": signal_count,
            "agent_signal_count": agent_signal_count,
            "agent_signal_ratio": round(agent_signal_ratio, 6),
            "age_seconds": age_seconds,
            "signals_file": str(signals_file),
        }
    if require_fresh and max_age_seconds > 0 and age_seconds > max_age_seconds:
        return {
            "ok": False,
            "reason": "stale_signals",
            "signal_count": signal_count,
            "agent_signal_count": agent_signal_count,
            "agent_signal_ratio": round(agent_signal_ratio, 6),
            "age_seconds": age_seconds,
            "signals_file": str(signals_file),
        }
    if min_agent_signal_count > 0 and agent_signal_count < min_agent_signal_count:
        return {
            "ok": False,
            "reason": "low_agent_signal_count",
            "signal_count": signal_count,
            "agent_signal_count": agent_signal_count,
            "agent_signal_ratio": round(agent_signal_ratio, 6),
            "age_seconds": age_seconds,
            "signals_file": str(signals_file),
        }
    if min_agent_signal_ratio > 0.0 and agent_signal_ratio < min_agent_signal_ratio:
        return {
            "ok": False,
            "reason": "low_agent_signal_ratio",
            "signal_count": signal_count,
            "agent_signal_count": agent_signal_count,
            "agent_signal_ratio": round(agent_signal_ratio, 6),
            "age_seconds": age_seconds,
            "signals_file": str(signals_file),
        }

    return {
        "ok": True,
        "reason": "",
        "signal_count": signal_count,
        "agent_signal_count": agent_signal_count,
        "agent_signal_ratio": round(agent_signal_ratio, 6),
        "age_seconds": age_seconds,
        "signals_file": str(signals_file),
    }


def main() -> None:
    base_dir = resolve_base_dir()
    env: Dict[str, str] = dict(os.environ)
    if _env_bool(env, "POLYCLAW_SYNC_SIGNALS_BEFORE_RUN", False):
        sync_args = build_signal_sync_args(env=env, base_dir=base_dir)
        subprocess.run(sync_args, check=True)
    signal_health = evaluate_signal_health(env=env, base_dir=base_dir)
    if not bool(signal_health.get("ok")):
        if _env_bool(env, "POLYCLAW_SKIP_ON_BAD_SIGNALS", True):
            print(
                json.dumps(
                    {
                        "status": "skipped",
                        "reason": str(signal_health.get("reason", "bad_signals")),
                        "signal_health": signal_health,
                    },
                    ensure_ascii=False,
                    indent=2,
                )
            )
            return
        raise RuntimeError(f"Bad signals: {signal_health}")
    args = build_cycle_args(env=env, base_dir=base_dir)
    subprocess.run(args, check=True)


if __name__ == "__main__":
    main()
