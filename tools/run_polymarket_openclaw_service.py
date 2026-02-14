#!/usr/bin/env python3
"""Systemd entrypoint for polymarket_openclaw_cycle.py (env-driven)."""

from __future__ import annotations

import os
import subprocess
import sys
import json
import shlex
import math
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
    signals_meta_file = env.get(
        "POLYCLAW_SIGNALS_META_FILE",
        str(base_dir / "data" / "openclaw" / "signals.meta.json"),
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
    live_execution = _env_bool(env, "POLYCLAW_LIVE_EXECUTION", False)
    live_order_type = (env.get("POLYCLAW_LIVE_ORDER_TYPE", "GTC").strip().upper() or "GTC")
    live_max_orders = _env_int(env, "POLYCLAW_LIVE_MAX_ORDERS_PER_RUN", 0)
    live_min_expected_value_usd = _env_float(env, "POLYCLAW_LIVE_MIN_EXPECTED_VALUE_USD", 0.0)
    live_min_stake_usd = _env_float(env, "POLYCLAW_LIVE_MIN_STAKE_USD", 1.0)
    live_fail_on_error = _env_bool(env, "POLYCLAW_LIVE_FAIL_ON_ERROR", False)

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
    if signals_meta_file:
        args.extend(["--signals-meta-file", signals_meta_file])
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
    if live_execution:
        args.append("--live-execution")
        args.extend(["--live-order-type", live_order_type])
        if live_max_orders > 0:
            args.extend(["--live-max-orders", str(max(0, live_max_orders))])
        if live_min_expected_value_usd > 0.0:
            args.extend(["--live-min-expected-value-usd", str(max(0.0, live_min_expected_value_usd))])
        if live_min_stake_usd > 0.0:
            args.extend(["--live-min-stake-usd", str(max(0.0, live_min_stake_usd))])
        if live_fail_on_error:
            args.append("--live-fail-on-error")
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
    last_good_signals_file = env.get(
        "POLYCLAW_LAST_GOOD_SIGNALS_FILE",
        str(base_dir / "data" / "openclaw" / "signals.last_good.jsonl"),
    ).strip()
    last_good_meta_file = env.get(
        "POLYCLAW_LAST_GOOD_SIGNALS_META_FILE",
        str(base_dir / "data" / "openclaw" / "signals.last_good.meta.json"),
    ).strip()
    min_signals = _env_int(env, "POLYCLAW_SIGNAL_SYNC_MIN_SIGNALS", 1)
    min_agent_signals = _env_int(env, "POLYCLAW_SIGNAL_SYNC_MIN_AGENT_SIGNALS", 0)
    min_agent_ratio = _env_float(env, "POLYCLAW_SIGNAL_SYNC_MIN_AGENT_RATIO", 0.0)
    timeout_seconds = _env_int(env, "POLYCLAW_SIGNAL_SYNC_TIMEOUT_SECONDS", 30)
    timeout_seconds = max(timeout_seconds, _estimate_openclaw_cmd_timeout_seconds(openclaw_cmd))
    if not openclaw_cmd:
        raise ValueError("POLYCLAW_OPENCLAW_CMD is required for signal sync")
    args = [
        sys.executable,
        str(base_dir / "tools" / "openclaw_signal_sync.py"),
        "--openclaw-cmd",
        openclaw_cmd,
        "--signals-file",
        signals_file,
        "--meta-file",
        meta_file,
        "--last-good-signals-file",
        last_good_signals_file,
        "--last-good-meta-file",
        last_good_meta_file,
        "--min-signals",
        str(max(0, min_signals)),
    ]
    if min_agent_signals > 0:
        args.extend(["--min-agent-signals", str(max(0, min_agent_signals))])
    if min_agent_ratio > 0.0:
        args.extend(["--min-agent-ratio", str(max(0.0, min(1.0, min_agent_ratio)))])
    args.extend([
        "--timeout-seconds",
        str(max(1, timeout_seconds)),
    ])
    return args


def run_signal_sync(*, env: Mapping[str, str], base_dir: Path) -> Dict[str, object]:
    if not _env_bool(env, "POLYCLAW_SYNC_SIGNALS_BEFORE_RUN", False):
        return {"ran": False, "ok": True, "soft_failed": False}
    args = build_signal_sync_args(env=env, base_dir=base_dir)
    proc = subprocess.run(args, capture_output=True, text=True, check=False)
    result: Dict[str, object] = {
        "ran": True,
        "ok": proc.returncode == 0,
        "exit_code": int(proc.returncode),
        "args": args,
    }
    stdout = (proc.stdout or "").strip()
    if stdout:
        try:
            payload = json.loads(stdout)
            if isinstance(payload, dict):
                result["result"] = payload
        except json.JSONDecodeError:
            result["stdout"] = stdout
    stderr = (proc.stderr or "").strip()
    if stderr:
        result["stderr"] = stderr
    if proc.returncode == 0:
        result["soft_failed"] = False
        return result

    soft_fail = _env_bool(env, "POLYCLAW_SYNC_SOFT_FAIL", True)
    result["soft_failed"] = bool(soft_fail)
    if soft_fail:
        return result
    raise subprocess.CalledProcessError(proc.returncode, args, output=proc.stdout, stderr=proc.stderr)


def select_effective_signal_source(*, env: Mapping[str, str], base_dir: Path) -> Dict[str, object]:
    primary_health = evaluate_signal_health(env=env, base_dir=base_dir)
    selected_env: Dict[str, str] = dict(env)
    result: Dict[str, object] = {
        "signal_health": primary_health,
        "used_last_good_signals": False,
        "selected_env": selected_env,
        "last_good_signal_health": {},
    }
    if bool(primary_health.get("ok")):
        return result
    if not _env_bool(env, "POLYCLAW_USE_LAST_GOOD_SIGNALS_ON_BAD", True):
        return result

    last_good_signals = env.get(
        "POLYCLAW_LAST_GOOD_SIGNALS_FILE",
        str(base_dir / "data" / "openclaw" / "signals.last_good.jsonl"),
    ).strip()
    last_good_meta = env.get(
        "POLYCLAW_LAST_GOOD_SIGNALS_META_FILE",
        str(base_dir / "data" / "openclaw" / "signals.last_good.meta.json"),
    ).strip()
    if not last_good_signals:
        return result

    fallback_env: Dict[str, str] = dict(env)
    fallback_env["POLYCLAW_SIGNALS_FILE"] = last_good_signals
    if last_good_meta:
        fallback_env["POLYCLAW_SIGNALS_META_FILE"] = last_good_meta
    fallback_health = evaluate_signal_health(env=fallback_env, base_dir=base_dir)
    result["last_good_signal_health"] = fallback_health
    if bool(fallback_health.get("ok")):
        result["signal_health"] = fallback_health
        result["used_last_good_signals"] = True
        result["selected_env"] = fallback_env
    return result


def format_signal_source_for_log(signal_source: Mapping[str, object]) -> Dict[str, object]:
    selected_env = signal_source.get("selected_env", {})
    if not isinstance(selected_env, Mapping):
        selected_env = {}
    return {
        "used_last_good_signals": bool(signal_source.get("used_last_good_signals")),
        "selected_signals_file": str(selected_env.get("POLYCLAW_SIGNALS_FILE", "")),
        "selected_signals_meta_file": str(selected_env.get("POLYCLAW_SIGNALS_META_FILE", "")),
        "signal_health": signal_source.get("signal_health", {}),
        "last_good_signal_health": signal_source.get("last_good_signal_health", {}),
    }


def _arg_value(tokens: List[str], key: str, default: int) -> int:
    for idx, token in enumerate(tokens):
        if token == key and idx + 1 < len(tokens):
            try:
                return int(tokens[idx + 1])
            except (TypeError, ValueError):
                return default
    return default


def _arg_csv_positive_ints(tokens: List[str], key: str, default: str = "") -> List[int]:
    raw = ""
    for idx, token in enumerate(tokens):
        if token == key and idx + 1 < len(tokens):
            raw = tokens[idx + 1]
            break
    if not raw:
        raw = default
    out: List[int] = []
    for part in str(raw).split(","):
        text = part.strip()
        if not text:
            continue
        try:
            value = int(text)
        except (TypeError, ValueError):
            continue
        if value > 0:
            out.append(value)
    return out


def _build_cap_attempts_for_timeout(*, primary_cap: int, fallback_caps: List[int]) -> List[int]:
    cap = max(1, int(primary_cap))
    attempts: List[int] = [cap]
    seen = {cap}
    for raw in fallback_caps:
        try:
            candidate = int(raw)
        except (TypeError, ValueError):
            continue
        if candidate < 1 or candidate >= cap or candidate in seen:
            continue
        attempts.append(candidate)
        seen.add(candidate)
    return attempts


def _estimate_openclaw_cmd_timeout_seconds(openclaw_cmd: str) -> int:
    cmd = str(openclaw_cmd or "").strip()
    if not cmd or "openclaw_agent_signal_bridge.py" not in cmd:
        return 0
    try:
        tokens = shlex.split(cmd)
    except ValueError:
        return 0
    bridge_timeout = max(1, _arg_value(tokens, "--timeout-seconds", 12))
    primary_retries = max(0, _arg_value(tokens, "--agent-retries", 2))
    fallback_retries = max(0, _arg_value(tokens, "--agent-fallback-retries", 0))
    retry_sleep_ms = max(0, _arg_value(tokens, "--agent-retry-sleep-ms", 300))
    primary_cap = max(1, _arg_value(tokens, "--agent-market-cap", 20))
    fallback_caps = _arg_csv_positive_ints(
        tokens,
        "--agent-market-cap-fallbacks",
        default="12,8,5",
    )
    cap_attempts = _build_cap_attempts_for_timeout(primary_cap=primary_cap, fallback_caps=fallback_caps)

    primary_seconds = ((primary_retries + 1) * bridge_timeout) + ((primary_retries * retry_sleep_ms) / 1000.0)
    fallback_seconds = ((fallback_retries + 1) * bridge_timeout) + ((fallback_retries * retry_sleep_ms) / 1000.0)
    estimated = primary_seconds + max(0, len(cap_attempts) - 1) * fallback_seconds + 5.0
    return max(1, int(math.ceil(estimated)))


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


def _source_counts_from_file(path: Path) -> Dict[str, int]:
    if not path.exists():
        return {}
    by_market_id: Dict[str, str] = {}
    with path.open("r", encoding="utf-8") as handle:
        for raw in handle:
            line = raw.strip()
            if not line:
                continue
            try:
                payload = json.loads(line)
            except json.JSONDecodeError:
                continue
            if not isinstance(payload, dict):
                continue
            market_id = str(payload.get("market_id") or payload.get("market") or payload.get("id") or "").strip()
            if not market_id:
                continue
            try:
                p_yes = float(payload.get("p_yes", payload.get("prob_yes", payload.get("probability_yes"))))
            except (TypeError, ValueError):
                continue
            if p_yes < 0.0 or p_yes > 1.0:
                continue
            source = str(payload.get("source") or "unknown").strip() or "unknown"
            by_market_id[market_id] = source

    counts: Dict[str, int] = {}
    for source in by_market_id.values():
        counts[source] = counts.get(source, 0) + 1
    return counts


_UNTRUSTED_SIGNAL_SOURCES = {"heuristic_fallback", "unknown"}


def _count_trusted_source_counts(source_counts: Mapping[str, int]) -> int:
    total = 0
    for source, count in source_counts.items():
        name = str(source or "").strip() or "unknown"
        if name in _UNTRUSTED_SIGNAL_SOURCES:
            continue
        try:
            value = int(count)
        except (TypeError, ValueError):
            continue
        if value > 0:
            total += value
    return total


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

    if not source_counts and (min_agent_signal_count > 0 or min_agent_signal_ratio > 0.0):
        source_counts = _source_counts_from_file(signals_file)
    if source_counts and agent_signal_count <= 0:
        agent_signal_count = _count_trusted_source_counts(source_counts)
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


def validate_live_runtime_configuration(*, env: Mapping[str, str]) -> Dict[str, object]:
    live_execution = _env_bool(env, "POLYCLAW_LIVE_EXECUTION", False)
    live_dry_run = _env_bool(env, "POLYCLAW_LIVE_DRY_RUN", False)
    private_key = str(env.get("POLYCLAW_LIVE_PRIVATE_KEY", "")).strip()
    if not private_key:
        key_file = str(env.get("POLYCLAW_LIVE_PRIVATE_KEY_FILE", "")).strip()
        if key_file:
            path = Path(key_file).expanduser()
            try:
                if path.exists() and path.is_file():
                    private_key = path.read_text(encoding="utf-8").strip()
            except Exception:
                pass

    result: Dict[str, object] = {
        "enabled": bool(live_execution),
        "dry_run": bool(live_dry_run),
        "ok": True,
        "reason": "disabled",
    }
    if not live_execution:
        return result

    if live_dry_run:
        result["reason"] = "dry_run"
        return result

    if not private_key:
        result["ok"] = False
        result["reason"] = "missing_private_key"
        return result

    if private_key.lower() in {"0x...", "changeme", "change_me", "your_private_key"}:
        result["ok"] = False
        result["reason"] = "placeholder_private_key"
        return result

    result["reason"] = "ready"
    return result


def main() -> None:
    base_dir = resolve_base_dir()
    env: Dict[str, str] = dict(os.environ)
    sync_result = run_signal_sync(env=env, base_dir=base_dir)
    if bool(sync_result.get("ran")) and not bool(sync_result.get("ok")) and bool(sync_result.get("soft_failed")):
        print(
            json.dumps(
                {
                    "status": "sync_soft_failed",
                    "sync": sync_result,
                },
                ensure_ascii=False,
                indent=2,
            )
        )
    source_selection = select_effective_signal_source(env=env, base_dir=base_dir)
    signal_health = source_selection.get("signal_health", {})
    selected_env_raw = source_selection.get("selected_env", dict(env))
    selected_env: Dict[str, str] = selected_env_raw if isinstance(selected_env_raw, dict) else dict(env)
    if bool(source_selection.get("used_last_good_signals")):
        print(
            json.dumps(
                {
                    "status": "using_last_good_signals",
                    "signal_source": format_signal_source_for_log(source_selection),
                },
                ensure_ascii=False,
                indent=2,
            )
        )
    if not bool(signal_health.get("ok")):
        if _env_bool(env, "POLYCLAW_SKIP_ON_BAD_SIGNALS", True):
            print(
                json.dumps(
                    {
                        "status": "skipped",
                        "reason": str(signal_health.get("reason", "bad_signals")),
                        "signal_sync": sync_result,
                        "signal_source": format_signal_source_for_log(source_selection),
                        "signal_health": signal_health,
                    },
                    ensure_ascii=False,
                    indent=2,
                )
            )
            return
        raise RuntimeError(f"Bad signals: {signal_health}")
    live_runtime = validate_live_runtime_configuration(env=selected_env)
    if bool(live_runtime.get("enabled")):
        print(
            json.dumps(
                {
                    "status": "live_runtime",
                    "live_runtime": live_runtime,
                },
                ensure_ascii=False,
                indent=2,
            )
        )
    if not bool(live_runtime.get("ok")):
        raise RuntimeError(f"Live execution misconfigured: {live_runtime}")
    args = build_cycle_args(env=selected_env, base_dir=base_dir)
    subprocess.run(args, check=True)


if __name__ == "__main__":
    main()
