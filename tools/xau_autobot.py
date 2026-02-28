#!/usr/bin/env python3
"""Lightweight XAUUSD auto-trading bot for MT5 (separate from core Swimmy system)."""

from __future__ import annotations

import argparse
import json
import os
import time
import urllib.error
import urllib.request
from datetime import datetime, timedelta, timezone
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

try:
    import MetaTrader5 as mt5  # type: ignore
except Exception:  # pragma: no cover - optional runtime dependency
    mt5 = None


DEFAULT_CONFIG_CANDIDATES: Tuple[str, ...] = (
    "tools/configs/xau_autobot.tuned_auto_active.json",
    "tools/configs/xau_autobot.tuned_auto_gc_m5_90d.json",
    "tools/configs/xau_autobot.tuned_auto_gc_m5.json",
    "tools/configs/xau_autobot.tuned_gc_m5.json",
    "tools/configs/xau_autobot.example.json",
)
DISCORD_WEBHOOK_ENV_KEYS: Tuple[str, ...] = (
    "SWIMMY_XAU_NOTIFY_WEBHOOK",
    "SWIMMY_DISCORD_REPORTS",
    "SWIMMY_DISCORD_SYSTEM_LOGS",
    "SWIMMY_DISCORD_ALERTS",
    "SWIMMY_DISCORD_APEX",
)
JST = timezone(timedelta(hours=9))
WAIT_ACTIONS = {"SKIP", "BLOCKED", "HOLD"}
MT5_COMMENT_MAX_LEN = 31
STRATEGY_MODES = {"trend", "reversion", "hybrid"}
LIVE_ACCEPT_TIMEFRAMES: Tuple[str, ...] = ("M1", "M5", "M15", "M20", "M30", "H1", "H4")
RESEARCH_ACCEPT_TIMEFRAMES: Tuple[str, ...] = ("M45", "H2", "H3", "H5")
DEFAULT_RUNTIME_JOURNAL_PATH = "data/reports/xau_autobot_runtime_journal_latest.jsonl"
DEFAULT_TRIAL_RUN_META_PATH = "data/reports/xau_autobot_trial_v2_current_run.json"
REPO_ROOT = Path(__file__).resolve().parent.parent
RUNTIME_METRICS_SCHEMA_VERSION = 1
M45_BIAS_GATE_POLICIES = {"none", "block_opposite", "hard_lock"}
M45_NEUTRAL_POLICIES = {"allow_all", "block_all"}
DEFAULT_RESAMPLE_ANCHOR = "UTC_00:00"


def validate_trade_comment(comment: str, *, max_len: int = MT5_COMMENT_MAX_LEN) -> str:
    text = str(comment or "").strip()
    if not text:
        raise ValueError("comment must not be empty")
    if len(text) > max_len:
        raise ValueError(f"comment exceeds MT5 max length {max_len}: len={len(text)} comment={text!r}")
    return text


def validate_strategy_mode(mode: str) -> str:
    text = str(mode or "trend").strip().lower()
    if text not in STRATEGY_MODES:
        raise ValueError(f"unsupported strategy_mode: {mode}")
    return text


def validate_m45_bias_gate_policy(policy: str) -> str:
    text = str(policy or "none").strip().lower()
    if text not in M45_BIAS_GATE_POLICIES:
        raise ValueError(f"unsupported m45_bias_gate_policy: {policy}")
    return text


def validate_m45_neutral_policy(policy: str) -> str:
    text = str(policy or "allow_all").strip().lower()
    if text not in M45_NEUTRAL_POLICIES:
        raise ValueError(f"unsupported m45_neutral_policy: {policy}")
    return text


def _optional_float(value: Any) -> Optional[float]:
    if value is None:
        return None
    text = str(value).strip()
    if text == "":
        return None
    return float(text)


def _normalize_timeframe_label(timeframe: str) -> str:
    text = str(timeframe or "").strip().upper()
    if text == "":
        raise ValueError("timeframe must not be empty")
    return text


def validate_runtime_timeframe_contract(timeframe: str, *, mode: str = "live") -> str:
    normalized_mode = str(mode or "live").strip().lower()
    if normalized_mode not in {"live", "research"}:
        raise ValueError(f"unsupported mode: {mode}")
    tf = _normalize_timeframe_label(timeframe)

    if normalized_mode == "live":
        if tf not in LIVE_ACCEPT_TIMEFRAMES:
            raise ValueError(f"unsupported timeframe for live mode: {timeframe}")
        return tf

    if tf in RESEARCH_ACCEPT_TIMEFRAMES:
        raise ValueError(
            "unsupported timeframe for research mode execution: "
            f"{timeframe} (use xau_autobot_optimize.py/readiness.py)"
        )
    if tf not in LIVE_ACCEPT_TIMEFRAMES:
        raise ValueError(f"unsupported timeframe for research mode: {timeframe}")
    return tf


def _env_bool(name: str, default: bool) -> bool:
    raw = os.getenv(name, "")
    if raw == "":
        return default
    return raw.strip().lower() in {"1", "true", "yes", "on"}


def _env_int(name: str, default: int) -> int:
    raw = os.getenv(name, "")
    if raw == "":
        return default
    try:
        return int(raw.strip())
    except ValueError:
        return default


def _env_float(name: str, default: float) -> float:
    raw = os.getenv(name, "")
    if raw == "":
        return default
    try:
        return float(raw.strip())
    except ValueError:
        return default


def resolve_discord_webhook_from_env() -> str:
    for key in DISCORD_WEBHOOK_ENV_KEYS:
        val = os.getenv(key, "").strip()
        if val:
            return val
    return ""


def session_window_jst_label(session_start_utc: int, session_end_utc: int) -> str:
    start_jst = (session_start_utc + 9) % 24
    end_jst = (session_end_utc + 9) % 24
    return f"{start_jst:02d}:00-{end_jst:02d}:00 JST"


def next_session_open_utc(now_utc: datetime, session_start: int) -> datetime:
    base = now_utc.astimezone(timezone.utc).replace(minute=0, second=0, microsecond=0)
    open_today = base.replace(hour=session_start)
    if now_utc < open_today:
        return open_today
    return open_today + timedelta(days=1)


def should_send_session_block_notification(
    payload: Dict[str, Any],
    *,
    last_sent_unix: int,
    now_unix: int,
    cooldown_sec: int,
) -> bool:
    action = str(payload.get("action", "")).upper()
    reason = str(payload.get("reason", "")).lower()
    if action != "BLOCKED" or reason != "session":
        return False
    if cooldown_sec <= 0:
        return True
    if last_sent_unix <= 0:
        return True
    return (now_unix - last_sent_unix) >= cooldown_sec


def wait_reason_key(payload: Dict[str, Any]) -> str:
    action = str(payload.get("action", "")).upper()
    if action not in WAIT_ACTIONS:
        return ""
    reason = str(payload.get("reason", "")).lower().strip()
    if action == "HOLD":
        reason = "hold"
    if reason == "":
        reason = "unknown"
    return f"{action.lower()}:{reason}"


def should_send_wait_summary_notification(
    *,
    wait_streak_total: int,
    last_sent_unix: int,
    now_unix: int,
    cooldown_sec: int,
) -> bool:
    if wait_streak_total <= 0:
        return False
    if cooldown_sec <= 0:
        return True
    if last_sent_unix <= 0:
        return True
    return (now_unix - last_sent_unix) >= cooldown_sec


def _build_discord_headers() -> Dict[str, str]:
    return {
        "Content-Type": "application/json",
        "User-Agent": "Mozilla/5.0 (compatible; xau-autobot-live/1.0)",
    }


def runtime_journal_path_from_env() -> str:
    raw = os.getenv("XAU_AUTOBOT_RUNTIME_JOURNAL_PATH", "").strip()
    candidate = raw or DEFAULT_RUNTIME_JOURNAL_PATH
    path = Path(candidate).expanduser()
    if not path.is_absolute():
        path = REPO_ROOT / path
    return str(path)


def append_runtime_journal(path: Path, payload: Dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as f:
        f.write(json.dumps(payload, ensure_ascii=True))
        f.write("\n")


def _new_runtime_metrics() -> Dict[str, Any]:
    return {
        "signal_eval_count": 0,
        "gap_reject_count": 0,
        "spread_reject_count": 0,
        "session_reject_count": 0,
        "maxpos_reject_count": 0,
        "gate_check_count": 0,
        "gate_reject_gap_count": 0,
        "signal_counts": {"BUY": 0, "SELL": 0, "HOLD": 0},
    }


def _runtime_signal_label(payload: Dict[str, Any]) -> str:
    action = str(payload.get("action", "")).upper()
    if action == "ORDER":
        side = str(payload.get("side", "")).upper()
        if side in {"BUY", "SELL"}:
            return side
        return "HOLD"

    signal = str(payload.get("signal", "")).upper()
    if signal in {"BUY", "SELL"}:
        return signal
    if action == "HOLD":
        return "HOLD"
    return ""


def update_runtime_metrics(metrics: Dict[str, Any], payload: Dict[str, Any]) -> Dict[str, Any]:
    action = str(payload.get("action", "")).upper()
    reason = str(payload.get("reason", "")).lower()

    if action == "BLOCKED" and reason == "session":
        metrics["session_reject_count"] = int(metrics.get("session_reject_count", 0)) + 1
    if action == "BLOCKED" and reason == "spread":
        metrics["spread_reject_count"] = int(metrics.get("spread_reject_count", 0)) + 1
    if action == "BLOCKED" and reason == "max_positions":
        metrics["maxpos_reject_count"] = int(metrics.get("maxpos_reject_count", 0)) + 1

    if not bool(payload.get("gap_gate_checked", False)):
        return metrics

    metrics["signal_eval_count"] = int(metrics.get("signal_eval_count", 0)) + 1
    metrics["gate_check_count"] = int(metrics.get("gate_check_count", 0)) + 1
    if reason == "ema_gap_out_of_range":
        metrics["gap_reject_count"] = int(metrics.get("gap_reject_count", 0)) + 1
        metrics["gate_reject_gap_count"] = int(metrics.get("gate_reject_gap_count", 0)) + 1

    signal_counts = metrics.get("signal_counts", {})
    if not isinstance(signal_counts, dict):
        signal_counts = {"BUY": 0, "SELL": 0, "HOLD": 0}
        metrics["signal_counts"] = signal_counts
    for key in ("BUY", "SELL", "HOLD"):
        signal_counts[key] = int(signal_counts.get(key, 0))

    label = _runtime_signal_label(payload)
    if label in signal_counts:
        signal_counts[label] = int(signal_counts.get(label, 0)) + 1
    return metrics


def runtime_metrics_snapshot(metrics: Dict[str, Any]) -> Dict[str, Any]:
    signal_eval_count = int(metrics.get("signal_eval_count", metrics.get("gate_check_count", 0)))
    gap_reject_count = int(metrics.get("gap_reject_count", metrics.get("gate_reject_gap_count", 0)))
    spread_reject_count = int(metrics.get("spread_reject_count", 0))
    session_reject_count = int(metrics.get("session_reject_count", 0))
    maxpos_reject_count = int(metrics.get("maxpos_reject_count", 0))
    gate_check_count = int(metrics.get("gate_check_count", signal_eval_count))
    gate_reject_gap_count = int(metrics.get("gate_reject_gap_count", gap_reject_count))
    signal_counts_raw = metrics.get("signal_counts", {})
    signal_counts = signal_counts_raw if isinstance(signal_counts_raw, dict) else {}
    out_signal_counts = {
        "BUY": int(signal_counts.get("BUY", 0)),
        "SELL": int(signal_counts.get("SELL", 0)),
        "HOLD": int(signal_counts.get("HOLD", 0)),
    }
    gap_reject_rate = (
        float(gap_reject_count) / float(signal_eval_count)
        if signal_eval_count > 0
        else 0.0
    )
    return {
        "signal_eval_count": signal_eval_count,
        "gap_reject_count": gap_reject_count,
        "spread_reject_count": spread_reject_count,
        "session_reject_count": session_reject_count,
        "maxpos_reject_count": maxpos_reject_count,
        "gate_check_count": gate_check_count,
        "gate_reject_gap_count": gate_reject_gap_count,
        "gap_reject_rate": gap_reject_rate,
        "signal_counts": out_signal_counts,
    }


def _config_basename(path_str: str) -> str:
    text = str(path_str or "").strip()
    if not text:
        return ""
    return Path(text.replace("\\", "/")).name.lower()


def trial_run_meta_path_from_env() -> Path:
    raw = os.getenv("XAU_AUTOBOT_TRIAL_RUN_META_PATH", "").strip()
    candidate = raw or DEFAULT_TRIAL_RUN_META_PATH
    path = Path(candidate).expanduser()
    if not path.is_absolute():
        path = REPO_ROOT / path
    return path


def _run_id_from_meta_path(meta_path: Path, *, expected_config: str) -> str:
    try:
        payload = json.loads(meta_path.read_text(encoding="utf-8"))
    except (OSError, ValueError, TypeError):
        return ""
    if not isinstance(payload, dict):
        return ""

    run_id = str(payload.get("run_id", "")).strip()
    if not run_id:
        return ""

    meta_config = _config_basename(str(payload.get("trial_config", "")))
    if expected_config and meta_config and expected_config != meta_config:
        return ""
    return run_id


def runtime_trial_run_id_from_env(*, config_path: str = "") -> str:
    explicit = str(os.getenv("XAU_AUTOBOT_TRIAL_RUN_ID", "")).strip()
    if explicit:
        return explicit

    expected_config = _config_basename(config_path)
    meta_path = trial_run_meta_path_from_env()
    run_id = _run_id_from_meta_path(meta_path, expected_config=expected_config)
    if run_id:
        return run_id

    explicit_meta = str(os.getenv("XAU_AUTOBOT_TRIAL_RUN_META_PATH", "")).strip()
    if explicit_meta:
        return ""

    reports_dir = REPO_ROOT / "data" / "reports"
    for suffix in ("_r1", "_r2", "_r3"):
        candidate = reports_dir / f"xau_autobot_trial_v2_current_run{suffix}.json"
        if candidate == meta_path:
            continue
        run_id = _run_id_from_meta_path(candidate, expected_config=expected_config)
        if run_id:
            return run_id
    return run_id


def runtime_metrics_with_metadata(
    metrics: Dict[str, Any],
    *,
    snapshot_time_utc: str,
    run_id: str,
    config: "BotConfig",
) -> Dict[str, Any]:
    out = runtime_metrics_snapshot(metrics)
    out["snapshot_time_utc"] = str(snapshot_time_utc)
    out["run_id"] = str(run_id)
    out["magic"] = int(config.magic)
    out["schema_version"] = int(RUNTIME_METRICS_SCHEMA_VERSION)
    return out


def _load_json_object(path: Path) -> Dict[str, Any]:
    if not path.exists():
        return {}
    try:
        with path.open("r", encoding="utf-8") as f:
            payload = json.load(f)
        return payload if isinstance(payload, dict) else {}
    except Exception:
        return {}


def _path_age_hours(path: Path, *, now_utc: datetime) -> float:
    modified = datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc)
    return float((now_utc - modified).total_seconds() / 3600.0)


def extract_live_underperforming_signal(report: Dict[str, Any]) -> Tuple[bool, List[str]]:
    if not report:
        return False, []
    reasons: List[str] = []
    live_gap = report.get("live_gap", {})
    if isinstance(live_gap, dict):
        raw = live_gap.get("underperforming_reasons", [])
        if isinstance(raw, list):
            for item in raw:
                text = str(item).strip()
                if text:
                    reasons.append(text)
        sample_quality = str(live_gap.get("sample_quality", "")).lower()
        underperforming = bool(live_gap.get("underperforming", False))
        if sample_quality == "ok" and underperforming:
            return True, (reasons or ["live_underperforming"])
    if bool(report.get("promotion_blocked", False)):
        return True, (reasons or ["promotion_blocked"])
    return False, []


@dataclass
class LiveUnderperformanceGuard:
    enabled: bool
    report_path: Path
    min_streak: int
    max_report_age_hours: float
    _streak: int = 0

    @classmethod
    def from_env(cls) -> "LiveUnderperformanceGuard":
        enabled = _env_bool("XAU_AUTOBOT_LIVE_GUARD_ENABLED", False)
        report_path = Path(os.getenv("XAU_AUTOBOT_LIVE_GUARD_REPORT_PATH", "data/reports/xau_autobot_promotion.json"))
        min_streak = max(1, _env_int("XAU_AUTOBOT_LIVE_GUARD_MIN_STREAK", 2))
        max_report_age_hours = _env_float("XAU_AUTOBOT_LIVE_GUARD_MAX_REPORT_AGE_HOURS", 72.0)
        return cls(
            enabled=enabled,
            report_path=report_path,
            min_streak=min_streak,
            max_report_age_hours=max_report_age_hours,
        )

    def check(self, *, now_utc: Optional[datetime] = None) -> Optional[Dict[str, Any]]:
        if not self.enabled:
            return None
        if now_utc is None:
            now_utc = datetime.now(timezone.utc)
        if not self.report_path.exists():
            self._streak = 0
            return None
        if self.max_report_age_hours > 0.0:
            if _path_age_hours(self.report_path, now_utc=now_utc) > self.max_report_age_hours:
                self._streak = 0
                return None

        report = _load_json_object(self.report_path)
        underperforming, reasons = extract_live_underperforming_signal(report)
        if not underperforming:
            self._streak = 0
            return None

        self._streak += 1
        if self._streak < self.min_streak:
            return None

        return {
            "action": "BLOCKED",
            "reason": "live_underperforming_guard",
            "guard_streak": self._streak,
            "guard_min_streak": self.min_streak,
            "underperforming_reasons": reasons,
            "report_path": str(self.report_path),
            "report_generated_at": report.get("generated_at", ""),
            "selected_period": report.get("selected_period", ""),
        }


class SessionBlockNotifier:
    def __init__(self, *, webhook_url: str, enabled: bool, cooldown_sec: int):
        self.webhook_url = webhook_url.strip()
        self.enabled = enabled and bool(self.webhook_url)
        self.cooldown_sec = max(0, cooldown_sec)
        self._last_sent_unix = 0
        self.wait_enabled = bool(self.webhook_url)
        self.wait_cooldown_sec = 1800
        self._last_wait_sent_unix = 0
        self._wait_streak_total = 0
        self._wait_streak_by_reason: Dict[str, int] = {}
        self._wait_streak_started_unix = 0

    @classmethod
    def from_env(cls) -> "SessionBlockNotifier":
        out = cls(
            webhook_url=resolve_discord_webhook_from_env(),
            enabled=_env_bool("XAU_AUTOBOT_NOTIFY_SESSION_BLOCK", True),
            cooldown_sec=_env_int("XAU_AUTOBOT_SESSION_NOTIFY_COOLDOWN_SEC", 1800),
        )
        out.wait_enabled = _env_bool("XAU_AUTOBOT_NOTIFY_WAIT_SUMMARY", True) and bool(out.webhook_url)
        out.wait_cooldown_sec = _env_int("XAU_AUTOBOT_WAIT_SUMMARY_COOLDOWN_SEC", 1800)
        return out

    def _record_wait_state(self, payload: Dict[str, Any], *, now_unix: int) -> None:
        action = str(payload.get("action", "")).upper()
        if action in WAIT_ACTIONS:
            key = wait_reason_key(payload)
            if self._wait_streak_total == 0:
                self._wait_streak_started_unix = now_unix
            self._wait_streak_total += 1
            if key:
                self._wait_streak_by_reason[key] = self._wait_streak_by_reason.get(key, 0) + 1
            return

        self._wait_streak_total = 0
        self._wait_streak_by_reason = {}
        self._wait_streak_started_unix = 0

    def _notify_wait_summary(self, *, payload: Dict[str, Any], config: "BotConfig", now_unix: int) -> None:
        if not self.wait_enabled:
            return
        if not should_send_wait_summary_notification(
            wait_streak_total=self._wait_streak_total,
            last_sent_unix=self._last_wait_sent_unix,
            now_unix=now_unix,
            cooldown_sec=self.wait_cooldown_sec,
        ):
            return
        self._last_wait_sent_unix = now_unix

        now_utc = datetime.fromtimestamp(now_unix, tz=timezone.utc)
        now_jst = now_utc.astimezone(JST)
        streak_from = (
            datetime.fromtimestamp(self._wait_streak_started_unix, tz=timezone.utc).astimezone(JST)
            if self._wait_streak_started_unix > 0
            else None
        )
        reason_counts = ", ".join(
            f"{key}={count}" for key, count in sorted(self._wait_streak_by_reason.items(), key=lambda kv: kv[0])
        )
        if not reason_counts:
            reason_counts = "none"

        last_reason = str(payload.get("reason", ""))
        title = "XAU AutoBot Wait Summary"
        lines = [
            f"symbol={config.symbol} wait_streak_total={self._wait_streak_total}",
            f"reason_counts={reason_counts}",
            f"last_action={payload.get('action')} last_reason={last_reason}",
            f"jst_now={now_jst:%Y-%m-%d %H:%M}",
        ]
        if streak_from is not None:
            lines.append(f"streak_started_jst={streak_from:%Y-%m-%d %H:%M}")
        body = json.dumps(
            {
                "embeds": [
                    {
                        "title": title,
                        "description": "\n".join(lines),
                        "color": 15158332,
                    }
                ]
            }
        ).encode("utf-8")
        req = urllib.request.Request(
            self.webhook_url,
            data=body,
            method="POST",
            headers=_build_discord_headers(),
        )
        try:
            with urllib.request.urlopen(req, timeout=10):
                return
        except urllib.error.URLError as exc:
            print(
                json.dumps(
                    {"action": "WARN", "reason": "wait_summary_notify_failed", "error": str(exc)},
                    ensure_ascii=True,
                )
            )

    def maybe_notify(self, payload: Dict[str, Any], *, config: "BotConfig", bar_time: int) -> None:
        if not self.webhook_url:
            return
        now_unix = int(bar_time if bar_time > 0 else time.time())
        self._record_wait_state(payload, now_unix=now_unix)
        self._notify_wait_summary(payload=payload, config=config, now_unix=now_unix)

        if not self.enabled:
            return
        if not should_send_session_block_notification(
            payload,
            last_sent_unix=self._last_sent_unix,
            now_unix=now_unix,
            cooldown_sec=self.cooldown_sec,
        ):
            return
        self._last_sent_unix = now_unix

        start_utc = int(payload.get("session_start_hour_utc", config.session_start_hour_utc))
        end_utc = int(payload.get("session_end_hour_utc", config.session_end_hour_utc))
        now_utc = datetime.fromtimestamp(now_unix, tz=timezone.utc)
        now_jst = now_utc.astimezone(JST)
        next_open_utc = next_session_open_utc(now_utc, session_start=start_utc)
        next_open_jst = next_open_utc.astimezone(JST)
        title = "XAU AutoBot Session Blocked"
        lines = [
            f"symbol={config.symbol} reason=session",
            f"utc_now={now_utc:%Y-%m-%d %H:%M} utc_window={start_utc:02d}:00-{end_utc:02d}:00",
            f"jst_now={now_jst:%Y-%m-%d %H:%M} jst_window={session_window_jst_label(start_utc, end_utc)}",
            f"next_open_jst={next_open_jst:%Y-%m-%d %H:%M}",
        ]
        body = json.dumps(
            {
                "embeds": [
                    {
                        "title": title,
                        "description": "\n".join(lines),
                        "color": 15158332,
                    }
                ]
            }
        ).encode("utf-8")
        req = urllib.request.Request(
            self.webhook_url,
            data=body,
            method="POST",
            headers=_build_discord_headers(),
        )
        try:
            with urllib.request.urlopen(req, timeout=10):
                return
        except urllib.error.URLError as exc:
            print(
                json.dumps(
                    {"action": "WARN", "reason": "session_notify_failed", "error": str(exc)},
                    ensure_ascii=True,
                )
            )


def ema_last(values: List[float], period: int) -> float:
    if period <= 0:
        raise ValueError("period must be > 0")
    if not values:
        return 0.0
    alpha = 2.0 / (period + 1.0)
    ema = values[0]
    for price in values[1:]:
        ema = ema + alpha * (price - ema)
    return ema


def atr_last(highs: List[float], lows: List[float], closes: List[float], period: int) -> float:
    values = atr_series(highs, lows, closes, period)
    return values[-1] if values else 0.0


def atr_series(highs: List[float], lows: List[float], closes: List[float], period: int) -> List[float]:
    if period <= 0:
        raise ValueError("period must be > 0")
    if not highs or len(highs) != len(lows) or len(highs) != len(closes):
        return []
    true_ranges: List[float] = [0.0] * len(highs)
    prev_close = closes[0]
    for i in range(len(highs)):
        high = highs[i]
        low = lows[i]
        tr = (high - low) if i == 0 else max(high - low, abs(high - prev_close), abs(low - prev_close))
        true_ranges[i] = tr
        prev_close = closes[i]

    out: List[float] = [0.0] * len(highs)
    rolling_sum = 0.0
    for i, tr in enumerate(true_ranges):
        rolling_sum += tr
        if i >= period:
            rolling_sum -= true_ranges[i - period]
        if i >= period - 1:
            out[i] = rolling_sum / float(period)
        else:
            out[i] = rolling_sum / float(i + 1)
    return out


def atr_pct_series(atr_values: List[float], close_values: List[float]) -> List[float]:
    out: List[float] = []
    for i, atr_value in enumerate(atr_values):
        close = close_values[i] if i < len(close_values) else 0.0
        out.append(0.0 if close <= 0.0 else atr_value / close)
    return out


def is_session_allowed(*, hour_utc: int, session_start: int, session_end: int) -> bool:
    if session_start == session_end:
        return True
    if session_start < session_end:
        return session_start <= hour_utc <= session_end
    return hour_utc >= session_start or hour_utc <= session_end


def volatility_filter_pass(
    *,
    atr_pct_values: List[float],
    min_ratio_to_median: float,
    max_ratio_to_median: float,
    window: int,
    min_samples: int,
) -> bool:
    if min_ratio_to_median <= 0.0 and max_ratio_to_median >= 999.0:
        return True
    if not atr_pct_values:
        return True
    if window <= 0:
        return True
    values = atr_pct_values[-window:] if len(atr_pct_values) > window else atr_pct_values
    if len(values) < min_samples:
        return True
    ordered = sorted(values)
    median = ordered[len(ordered) // 2]
    current = values[-1]
    if median <= 0.0:
        return True
    ratio = current / median
    if min_ratio_to_median > 0.0 and ratio < min_ratio_to_median:
        return False
    if max_ratio_to_median > 0.0 and ratio > max_ratio_to_median:
        return False
    return True


def decide_signal(
    *,
    last_close: float,
    ema_fast: float,
    ema_slow: float,
    atr_value: float,
    pullback_atr: float,
) -> str:
    if atr_value <= 0.0:
        return "HOLD"
    pullback = atr_value * pullback_atr
    if ema_fast > ema_slow and last_close <= (ema_fast - pullback):
        return "BUY"
    if ema_fast < ema_slow and last_close >= (ema_fast + pullback):
        return "SELL"
    return "HOLD"


def detect_regime(
    *,
    ema_fast: float,
    ema_slow: float,
    atr_value: float,
    trend_threshold: float,
) -> str:
    if atr_value <= 0.0:
        return "range"
    if trend_threshold <= 0.0:
        return "trend"
    trend_strength = abs(ema_fast - ema_slow) / atr_value
    if trend_strength >= trend_threshold:
        return "trend"
    return "range"


def decide_reversion_signal(
    *,
    last_close: float,
    ema_anchor: float,
    atr_value: float,
    reversion_atr: float,
) -> str:
    if atr_value <= 0.0 or reversion_atr <= 0.0:
        return "HOLD"
    distance = atr_value * reversion_atr
    if last_close <= (ema_anchor - distance):
        return "BUY"
    if last_close >= (ema_anchor + distance):
        return "SELL"
    return "HOLD"


def decide_signal_with_mode(
    *,
    strategy_mode: str,
    last_close: float,
    ema_fast: float,
    ema_slow: float,
    atr_value: float,
    pullback_atr: float,
    reversion_atr: float,
    trend_threshold: float,
) -> Dict[str, str]:
    mode = validate_strategy_mode(strategy_mode)
    if mode == "trend":
        return {
            "regime": "trend",
            "source": "trend",
            "signal": decide_signal(
                last_close=last_close,
                ema_fast=ema_fast,
                ema_slow=ema_slow,
                atr_value=atr_value,
                pullback_atr=pullback_atr,
            ),
        }
    if mode == "reversion":
        return {
            "regime": "range",
            "source": "reversion",
            "signal": decide_reversion_signal(
                last_close=last_close,
                ema_anchor=ema_slow,
                atr_value=atr_value,
                reversion_atr=reversion_atr,
            ),
        }

    regime = detect_regime(
        ema_fast=ema_fast,
        ema_slow=ema_slow,
        atr_value=atr_value,
        trend_threshold=trend_threshold,
    )
    if regime == "trend":
        return {
            "regime": regime,
            "source": "trend",
            "signal": decide_signal(
                last_close=last_close,
                ema_fast=ema_fast,
                ema_slow=ema_slow,
                atr_value=atr_value,
                pullback_atr=pullback_atr,
            ),
        }
    return {
        "regime": regime,
        "source": "reversion",
        "signal": decide_reversion_signal(
            last_close=last_close,
            ema_anchor=ema_slow,
            atr_value=atr_value,
            reversion_atr=reversion_atr,
        ),
    }


def next_two_state_mode(
    *,
    prev_state: Optional[str],
    regime_strength: float,
    trend_on: float,
    trend_off: float,
) -> str:
    state = str(prev_state or "").strip().lower()
    if state not in {"trend", "non_trend"}:
        state = "non_trend"
    if state == "non_trend" and float(regime_strength) >= float(trend_on):
        return "trend"
    if state == "trend" and float(regime_strength) <= float(trend_off):
        return "non_trend"
    return state


def _signal_to_int(signal: str) -> int:
    text = str(signal or "").strip().upper()
    if text == "BUY":
        return 1
    if text == "SELL":
        return -1
    return 0


def _int_to_signal(signal: int) -> str:
    if int(signal) > 0:
        return "BUY"
    if int(signal) < 0:
        return "SELL"
    return "HOLD"


def _format_bias_label(bias: int) -> str:
    if int(bias) > 0:
        return "LONG"
    if int(bias) < 0:
        return "SHORT"
    return "NEUTRAL"


def apply_m45_bias_gate_signal(
    *,
    signal: str,
    bias: int,
    gate_policy: str,
    neutral_policy: str,
    gate_active: bool = True,
) -> Tuple[str, bool]:
    signal_int = _signal_to_int(signal)
    if signal_int == 0:
        return "HOLD", False
    if not bool(gate_active):
        return _int_to_signal(signal_int), False

    policy = validate_m45_bias_gate_policy(gate_policy)
    neutral = validate_m45_neutral_policy(neutral_policy)
    if policy == "none":
        return _int_to_signal(signal_int), False

    if int(bias) == 0:
        if neutral == "block_all":
            return "HOLD", True
        return _int_to_signal(signal_int), False

    if policy == "block_opposite":
        if int(signal_int) == -int(bias):
            return "HOLD", True
        return _int_to_signal(signal_int), False

    # hard_lock
    if int(signal_int) != int(bias):
        return "HOLD", True
    return _int_to_signal(signal_int), False


def _timestamp_to_utc(value: object) -> datetime:
    if isinstance(value, datetime):
        out = value
    elif isinstance(value, (int, float)):
        out = datetime.fromtimestamp(float(value), tz=timezone.utc)
    else:
        raise ValueError(f"unsupported timestamp type: {type(value)!r}")
    if out.tzinfo is None:
        return out.replace(tzinfo=timezone.utc)
    return out.astimezone(timezone.utc)


def _resample_ohlc_fixed(
    *,
    times: Sequence[int],
    highs: Sequence[float],
    lows: Sequence[float],
    closes: Sequence[float],
    factor: int,
    base_interval_minutes: int,
    anchor: str,
    offset_minutes: int,
) -> Dict[str, List[float]]:
    if factor <= 1:
        return {
            "time": [int(t) for t in times],
            "high": [float(v) for v in highs],
            "low": [float(v) for v in lows],
            "close": [float(v) for v in closes],
        }
    if base_interval_minutes <= 0:
        raise ValueError(f"base_interval_minutes must be > 0: {base_interval_minutes}")
    if str(anchor or "").strip().upper() != DEFAULT_RESAMPLE_ANCHOR:
        raise ValueError(f"unsupported resample anchor: {anchor}")
    if int(offset_minutes) != 0:
        raise ValueError(f"unsupported resample offset minutes for live contract: {offset_minutes}")

    size = min(len(times), len(highs), len(lows), len(closes))
    if size < factor:
        raise RuntimeError(f"not enough bars to resample: size={size} factor={factor}")
    bucket_seconds = int(base_interval_minutes * factor * 60)
    out_time: List[int] = []
    out_high: List[float] = []
    out_low: List[float] = []
    out_close: List[float] = []
    current_bucket: Optional[int] = None
    bucket_indices: List[int] = []

    def _flush(indices: List[int]) -> None:
        if len(indices) < factor:
            return
        lo = int(indices[0])
        hi = int(indices[-1])
        out_time.append(int(times[hi]))
        out_high.append(float(max(highs[idx] for idx in indices)))
        out_low.append(float(min(lows[idx] for idx in indices)))
        out_close.append(float(closes[hi]))

    for idx in range(size):
        ts_utc = _timestamp_to_utc(times[idx])
        day_anchor = ts_utc.replace(hour=0, minute=0, second=0, microsecond=0)
        elapsed = int((ts_utc - day_anchor).total_seconds())
        bucket = int((day_anchor + timedelta(seconds=(elapsed // bucket_seconds) * bucket_seconds)).timestamp())
        if current_bucket is None:
            current_bucket = bucket
        if bucket != current_bucket:
            _flush(bucket_indices)
            bucket_indices = []
            current_bucket = bucket
        bucket_indices.append(idx)
    _flush(bucket_indices)
    if not out_time:
        raise RuntimeError(f"not enough aligned bars to resample: size={size} factor={factor}")
    return {"time": out_time, "high": out_high, "low": out_low, "close": out_close}


def _requires_m45_commander(config: "BotConfig") -> bool:
    return bool(config.m45_bias_gate_policy != "none" or config.trend_override_enabled)


def _derive_m45_commander_snapshot(config: "BotConfig", gateway: "Mt5Gateway") -> M45CommanderSnapshot:
    source = gateway.fetch_rates_for_timeframe(config.m45_commander_source_timeframe, config.m45_commander_source_bars)
    derived = _resample_ohlc_fixed(
        times=source["time"],
        highs=source["high"],
        lows=source["low"],
        closes=source["close"],
        factor=config.m45_commander_resample_factor,
        base_interval_minutes=15,
        anchor=config.m45_commander_resample_anchor,
        offset_minutes=config.m45_commander_resample_offset_minutes,
    )
    closes = [float(v) for v in derived["close"]]
    highs = [float(v) for v in derived["high"]]
    lows = [float(v) for v in derived["low"]]
    min_required = max(config.m45_commander_slow_ema, config.m45_commander_atr_period) + 2
    if len(closes) < min_required:
        raise RuntimeError(
            "not enough derived M45 bars for commander: "
            f"size={len(closes)} required={min_required}"
        )
    ema_fast = ema_last(closes, config.m45_commander_fast_ema)
    ema_slow = ema_last(closes, config.m45_commander_slow_ema)
    atr_values = atr_series(highs, lows, closes, config.m45_commander_atr_period)
    atr_value = atr_values[-1] if atr_values else 0.0
    if atr_value <= 0.0:
        regime_strength = 0.0
    else:
        regime_strength = abs(ema_fast - ema_slow) / atr_value
    bias = 0
    if regime_strength >= float(config.m45_commander_regime_trend_threshold):
        if ema_fast > ema_slow:
            bias = 1
        elif ema_fast < ema_slow:
            bias = -1
    return M45CommanderSnapshot(
        bar_time=int(derived["time"][-1]),
        regime_strength=float(regime_strength),
        bias=int(bias),
    )


def _resolve_live_trend_override(
    *,
    config: "BotConfig",
    state: LiveEvalState,
    regime_strength: float,
) -> Tuple[str, float, float]:
    if not bool(config.trend_override_enabled):
        return "disabled", float(config.min_ema_gap_over_atr), float(config.pullback_atr)
    seed_state = state.trend_override_state
    if seed_state is None:
        seed_state = str(config.trend_override_initial_state).strip().lower() or "non_trend"
    next_state = next_two_state_mode(
        prev_state=seed_state,
        regime_strength=float(regime_strength),
        trend_on=float(config.trend_override_on),
        trend_off=float(config.trend_override_off),
    )
    state.trend_override_state = next_state

    min_gap = float(config.min_ema_gap_over_atr)
    pullback = float(config.pullback_atr)
    if next_state == "trend":
        if config.trend_override_min_ema_gap_over_atr is not None:
            min_gap = float(config.trend_override_min_ema_gap_over_atr)
        if config.trend_override_pullback_atr is not None:
            pullback = float(config.trend_override_pullback_atr)
    return next_state, min_gap, pullback


def build_sl_tp(
    *,
    side: str,
    entry_price: float,
    atr_value: float,
    sl_atr: float,
    tp_atr: float,
) -> Tuple[float, float]:
    sl_distance = atr_value * sl_atr
    tp_distance = atr_value * tp_atr
    normalized_side = side.upper()
    if normalized_side == "BUY":
        return entry_price - sl_distance, entry_price + tp_distance
    if normalized_side == "SELL":
        return entry_price + sl_distance, entry_price - tp_distance
    raise ValueError(f"unsupported side: {side}")


def can_open_trade(
    *,
    spread_points: float,
    max_spread_points: float,
    open_positions: int,
    max_positions: int,
) -> bool:
    return spread_points <= max_spread_points and open_positions < max_positions


def _tf_to_mt5(timeframe: str) -> int:
    if mt5 is None:
        raise RuntimeError("MetaTrader5 package is not available")
    mapping = {
        "M1": "TIMEFRAME_M1",
        "M5": "TIMEFRAME_M5",
        "M15": "TIMEFRAME_M15",
        "M20": "TIMEFRAME_M20",
        "M30": "TIMEFRAME_M30",
        "H1": "TIMEFRAME_H1",
        "H4": "TIMEFRAME_H4",
    }
    key = _normalize_timeframe_label(timeframe)
    attr = mapping.get(key, "")
    if attr == "":
        raise ValueError(f"unsupported timeframe for live mode: {timeframe}")
    value = getattr(mt5, attr, None)
    if value is None:
        raise ValueError(f"unsupported timeframe for live mode: {timeframe}")
    return int(value)


@dataclass
class BotConfig:
    symbol: str = "XAUUSD"
    timeframe: str = "M5"
    bars: int = 300
    fast_ema: int = 20
    slow_ema: int = 80
    atr_period: int = 14
    strategy_mode: str = "trend"
    regime_trend_threshold: float = 1.2
    pullback_atr: float = 0.6
    reversion_atr: float = 0.8
    sl_atr: float = 1.5
    tp_atr: float = 2.0
    reversion_sl_atr: float = 1.2
    reversion_tp_atr: float = 1.2
    lot: float = 0.01
    max_spread_points: float = 80.0
    max_positions: int = 1
    session_start_hour_utc: int = 0
    session_end_hour_utc: int = 23
    atr_filter_window: int = 288
    atr_filter_min_samples: int = 120
    min_atr_ratio_to_median: float = 0.0
    max_atr_ratio_to_median: float = 999.0
    min_ema_gap_over_atr: float = 0.9
    max_ema_gap_over_atr: float = 2.5
    m45_bias_gate_policy: str = "none"
    m45_neutral_policy: str = "allow_all"
    trend_override_enabled: bool = False
    trend_override_on: float = 2.35
    trend_override_off: float = 2.25
    trend_override_initial_state: str = "non_trend"
    trend_override_min_ema_gap_over_atr: Optional[float] = None
    trend_override_pullback_atr: Optional[float] = None
    m45_commander_config_path: str = ""
    m45_commander_source_timeframe: str = "M15"
    m45_commander_resample_factor: int = 3
    m45_commander_source_bars: int = 900
    m45_commander_fast_ema: int = 24
    m45_commander_slow_ema: int = 200
    m45_commander_atr_period: int = 14
    m45_commander_regime_trend_threshold: float = 2.4
    m45_commander_resample_anchor: str = DEFAULT_RESAMPLE_ANCHOR
    m45_commander_resample_offset_minutes: int = 0
    m45_gate_min_strength_to_block_opposite: Optional[float] = None
    deviation: int = 30
    magic: int = 560061
    comment: str = "xau_autobot_v1"
    dry_run: bool = True
    once: bool = True
    poll_seconds: int = 15
    max_cycles: int = 0

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "BotConfig":
        comment = validate_trade_comment(str(data.get("comment", "xau_autobot_v1")))
        strategy_mode = validate_strategy_mode(str(data.get("strategy_mode", "trend")))
        m45_bias_gate_policy = validate_m45_bias_gate_policy(str(data.get("m45_bias_gate_policy", "none")))
        m45_neutral_policy = validate_m45_neutral_policy(str(data.get("m45_neutral_policy", "allow_all")))
        min_ema_gap_over_atr = float(data.get("min_ema_gap_over_atr", 0.9))
        max_ema_gap_over_atr = float(data.get("max_ema_gap_over_atr", 2.5))
        if max_ema_gap_over_atr < min_ema_gap_over_atr:
            raise ValueError(
                f"max_ema_gap_over_atr must be >= min_ema_gap_over_atr: "
                f"{max_ema_gap_over_atr} < {min_ema_gap_over_atr}"
            )
        timeframe = _normalize_timeframe_label(str(data.get("timeframe", "M5")))
        trend_override_enabled = bool(data.get("trend_override_enabled", False))
        trend_override_on = float(data.get("trend_override_on", 2.35))
        trend_override_off = float(data.get("trend_override_off", 2.25))
        if trend_override_on < trend_override_off:
            raise ValueError(
                "trend_override_on must be >= trend_override_off: "
                f"{trend_override_on} < {trend_override_off}"
            )
        trend_override_initial_state = str(data.get("trend_override_initial_state", "non_trend")).strip().lower()
        if trend_override_initial_state == "":
            trend_override_initial_state = "non_trend"
        if trend_override_initial_state not in {"trend", "non_trend"}:
            raise ValueError(f"unsupported trend_override_initial_state: {trend_override_initial_state}")
        m45_commander_source_timeframe = _normalize_timeframe_label(
            str(data.get("m45_commander_source_timeframe", "M15"))
        )
        if m45_commander_source_timeframe != "M15":
            raise ValueError(
                "m45_commander_source_timeframe must be M15 for live derived-M45 contract: "
                f"{m45_commander_source_timeframe}"
            )
        m45_commander_resample_factor = int(data.get("m45_commander_resample_factor", 3))
        if m45_commander_resample_factor <= 1:
            raise ValueError(f"m45_commander_resample_factor must be > 1: {m45_commander_resample_factor}")
        m45_commander_resample_anchor = str(
            data.get("m45_commander_resample_anchor", DEFAULT_RESAMPLE_ANCHOR)
        ).strip().upper()
        if m45_commander_resample_anchor != DEFAULT_RESAMPLE_ANCHOR:
            raise ValueError(f"unsupported m45_commander_resample_anchor: {m45_commander_resample_anchor}")
        m45_commander_resample_offset_minutes = int(data.get("m45_commander_resample_offset_minutes", 0))
        if m45_commander_resample_offset_minutes != 0:
            raise ValueError(
                "m45_commander_resample_offset_minutes must be 0 for live contract: "
                f"{m45_commander_resample_offset_minutes}"
            )
        m45_commander_source_bars = int(data.get("m45_commander_source_bars", 900))
        if m45_commander_source_bars <= 0:
            raise ValueError(f"m45_commander_source_bars must be > 0: {m45_commander_source_bars}")

        m45_gate_min_strength_to_block_opposite = _optional_float(
            data.get("m45_gate_min_strength_to_block_opposite")
        )

        m45_commander_required = (m45_bias_gate_policy != "none") or trend_override_enabled
        if m45_commander_required and timeframe != "M20":
            raise ValueError(
                "m45 commander gate/override is only supported for timeframe=M20 in live contract"
            )
        return cls(
            symbol=str(data.get("symbol", "XAUUSD")),
            timeframe=timeframe,
            bars=int(data.get("bars", 300)),
            fast_ema=int(data.get("fast_ema", 20)),
            slow_ema=int(data.get("slow_ema", 80)),
            atr_period=int(data.get("atr_period", 14)),
            strategy_mode=strategy_mode,
            regime_trend_threshold=float(data.get("regime_trend_threshold", 1.2)),
            pullback_atr=float(data.get("pullback_atr", 0.6)),
            reversion_atr=float(data.get("reversion_atr", 0.8)),
            sl_atr=float(data.get("sl_atr", 1.5)),
            tp_atr=float(data.get("tp_atr", 2.0)),
            reversion_sl_atr=float(data.get("reversion_sl_atr", data.get("sl_atr", 1.2))),
            reversion_tp_atr=float(data.get("reversion_tp_atr", data.get("tp_atr", 1.2))),
            lot=float(data.get("lot", 0.01)),
            max_spread_points=float(data.get("max_spread_points", 80.0)),
            max_positions=int(data.get("max_positions", 1)),
            session_start_hour_utc=int(data.get("session_start_hour_utc", 0)),
            session_end_hour_utc=int(data.get("session_end_hour_utc", 23)),
            atr_filter_window=int(data.get("atr_filter_window", 288)),
            atr_filter_min_samples=int(data.get("atr_filter_min_samples", 120)),
            min_atr_ratio_to_median=float(data.get("min_atr_ratio_to_median", 0.0)),
            max_atr_ratio_to_median=float(data.get("max_atr_ratio_to_median", 999.0)),
            min_ema_gap_over_atr=min_ema_gap_over_atr,
            max_ema_gap_over_atr=max_ema_gap_over_atr,
            m45_bias_gate_policy=m45_bias_gate_policy,
            m45_neutral_policy=m45_neutral_policy,
            trend_override_enabled=trend_override_enabled,
            trend_override_on=trend_override_on,
            trend_override_off=trend_override_off,
            trend_override_initial_state=trend_override_initial_state,
            trend_override_min_ema_gap_over_atr=_optional_float(
                data.get("trend_override_min_ema_gap_over_atr")
            ),
            trend_override_pullback_atr=_optional_float(data.get("trend_override_pullback_atr")),
            m45_commander_config_path=str(data.get("m45_commander_config_path", "")).strip(),
            m45_commander_source_timeframe=m45_commander_source_timeframe,
            m45_commander_resample_factor=m45_commander_resample_factor,
            m45_commander_source_bars=m45_commander_source_bars,
            m45_commander_fast_ema=int(data.get("m45_commander_fast_ema", 24)),
            m45_commander_slow_ema=int(data.get("m45_commander_slow_ema", 200)),
            m45_commander_atr_period=int(data.get("m45_commander_atr_period", 14)),
            m45_commander_regime_trend_threshold=float(data.get("m45_commander_regime_trend_threshold", 2.4)),
            m45_commander_resample_anchor=m45_commander_resample_anchor,
            m45_commander_resample_offset_minutes=m45_commander_resample_offset_minutes,
            m45_gate_min_strength_to_block_opposite=m45_gate_min_strength_to_block_opposite,
            deviation=int(data.get("deviation", 30)),
            magic=int(data.get("magic", 560061)),
            comment=comment,
            dry_run=bool(data.get("dry_run", True)),
            once=bool(data.get("once", True)),
            poll_seconds=int(data.get("poll_seconds", 15)),
            max_cycles=int(data.get("max_cycles", 0)),
        )


def _resolve_profile_path(*, profile_path: str, config_path: str) -> Path:
    candidate = Path(profile_path).expanduser()
    if candidate.is_absolute():
        return candidate
    if config_path:
        config_dir = Path(config_path).expanduser().resolve().parent
        maybe = config_dir / candidate
        if maybe.exists():
            return maybe
    return (REPO_ROOT / candidate).resolve()


def hydrate_m45_commander_from_profile(config: BotConfig, *, config_path: str) -> BotConfig:
    profile = str(config.m45_commander_config_path or "").strip()
    if profile == "":
        return config
    path = _resolve_profile_path(profile_path=profile, config_path=config_path)
    if not path.exists():
        raise ValueError(f"m45_commander_config_path not found: {path}")
    payload = _load_json_object(path)
    if not payload:
        raise ValueError(f"m45_commander_config_path is empty or invalid: {path}")
    timeframe = _normalize_timeframe_label(str(payload.get("timeframe", "M45")))
    if timeframe != "M45":
        raise ValueError(f"m45 commander profile must use timeframe=M45: {timeframe}")
    config.m45_commander_fast_ema = int(payload.get("fast_ema", config.m45_commander_fast_ema))
    config.m45_commander_slow_ema = int(payload.get("slow_ema", config.m45_commander_slow_ema))
    config.m45_commander_atr_period = int(payload.get("atr_period", config.m45_commander_atr_period))
    config.m45_commander_regime_trend_threshold = float(
        payload.get("regime_trend_threshold", config.m45_commander_regime_trend_threshold)
    )
    profile_bars = int(payload.get("bars", 0))
    if profile_bars > 0:
        derived_source_bars = profile_bars * int(config.m45_commander_resample_factor)
        if derived_source_bars > config.m45_commander_source_bars:
            config.m45_commander_source_bars = derived_source_bars
    return config


@dataclass
class LiveEvalState:
    trend_override_state: Optional[str] = None


@dataclass
class M45CommanderSnapshot:
    bar_time: int
    regime_strength: float
    bias: int


class Mt5Gateway:
    def __init__(self, config: BotConfig):
        self.config = config
        self._preferred_filling_mode: Optional[int] = None

    def connect(self) -> None:
        if mt5 is None:
            raise RuntimeError(
                "MetaTrader5 Python package is missing. Install with `pip install MetaTrader5`."
            )
        if not mt5.initialize():
            raise RuntimeError(f"mt5.initialize() failed: {mt5.last_error()}")
        if not mt5.symbol_select(self.config.symbol, True):
            raise RuntimeError(f"failed to select symbol: {self.config.symbol}")

    def shutdown(self) -> None:
        if mt5 is not None:
            mt5.shutdown()

    def fetch_rates_for_timeframe(self, timeframe: str, bars: int) -> Dict[str, List[float]]:
        assert mt5 is not None
        tf = _tf_to_mt5(timeframe)
        rates = mt5.copy_rates_from_pos(self.config.symbol, tf, 0, bars)
        if rates is None:
            raise RuntimeError("not enough bars from MT5")
        return {
            "time": [int(r["time"]) for r in rates],
            "high": [float(r["high"]) for r in rates],
            "low": [float(r["low"]) for r in rates],
            "close": [float(r["close"]) for r in rates],
        }

    def fetch_rates(self) -> Dict[str, List[float]]:
        rates = self.fetch_rates_for_timeframe(self.config.timeframe, self.config.bars)
        if len(rates["close"]) < max(self.config.slow_ema, self.config.atr_period) + 2:
            raise RuntimeError("not enough bars from MT5")
        return {
            "time": list(rates["time"]),
            "high": list(rates["high"]),
            "low": list(rates["low"]),
            "close": list(rates["close"]),
        }

    def get_tick_context(self) -> Tuple[float, float, float]:
        assert mt5 is not None
        tick = mt5.symbol_info_tick(self.config.symbol)
        info = mt5.symbol_info(self.config.symbol)
        if tick is None or info is None:
            raise RuntimeError(f"failed to get tick/symbol info for {self.config.symbol}")
        ask = float(tick.ask)
        bid = float(tick.bid)
        point = float(info.point or 0.0)
        if point <= 0.0:
            raise RuntimeError("invalid point size")
        return ask, bid, point

    def open_positions(self) -> int:
        assert mt5 is not None
        return len(self.own_positions())

    def own_positions(self) -> List[Any]:
        assert mt5 is not None
        positions = mt5.positions_get(symbol=self.config.symbol)
        if not positions:
            return []
        out: List[Any] = []
        for position in positions:
            if int(getattr(position, "magic", 0)) == int(self.config.magic):
                out.append(position)
        return out

    def has_opposite_position(self, signal: str) -> bool:
        normalized_signal = signal.upper()
        for position in self.own_positions():
            side = self._position_side(position)
            if side and side != normalized_signal:
                return True
        return False

    def _default_filling_modes(self) -> List[int]:
        assert mt5 is not None
        base_modes: List[int] = []
        for attr in ("ORDER_FILLING_FOK", "ORDER_FILLING_IOC", "ORDER_FILLING_RETURN"):
            raw = getattr(mt5, attr, None)
            if raw is None:
                continue
            mode = int(raw)
            if mode not in base_modes:
                base_modes.append(mode)

        symbol_modes: List[int] = []
        symbol_info = mt5.symbol_info(self.config.symbol)
        raw_symbol_filling_mode = int(getattr(symbol_info, "filling_mode", 0) or 0) if symbol_info is not None else 0
        if raw_symbol_filling_mode > 0:
            fill_pairs = (
                ("ORDER_FILLING_FOK", "SYMBOL_FILLING_FOK", 1),
                ("ORDER_FILLING_IOC", "SYMBOL_FILLING_IOC", 2),
            )
            for order_attr, symbol_attr, fallback_flag in fill_pairs:
                order_fill = getattr(mt5, order_attr, None)
                if order_fill is None:
                    continue
                symbol_flag = int(getattr(mt5, symbol_attr, fallback_flag))
                if raw_symbol_filling_mode & symbol_flag:
                    mode = int(order_fill)
                    if mode not in symbol_modes:
                        symbol_modes.append(mode)

        modes: List[int] = []
        if self._preferred_filling_mode is not None:
            modes.append(int(self._preferred_filling_mode))
        for mode in symbol_modes:
            if mode not in modes:
                modes.append(mode)
        for mode in base_modes:
            if mode not in modes:
                modes.append(mode)
        return modes

    def _initial_filling_mode(self) -> int:
        assert mt5 is not None
        modes = self._default_filling_modes()
        if modes:
            return int(modes[0])
        fallback = getattr(mt5, "ORDER_FILLING_IOC", None)
        if fallback is None:
            raise RuntimeError("MT5 ORDER_FILLING constants are unavailable")
        return int(fallback)

    def _is_success_retcode(self, retcode: int) -> bool:
        assert mt5 is not None
        success_codes = (
            int(getattr(mt5, "TRADE_RETCODE_DONE", 10009)),
            int(getattr(mt5, "TRADE_RETCODE_DONE_PARTIAL", 10010)),
            int(getattr(mt5, "TRADE_RETCODE_PLACED", 10008)),
        )
        return int(retcode) in success_codes

    def _is_disk_error_result(self, result_dict: Dict[str, Any]) -> bool:
        retcode = int(result_dict.get("retcode", -1))
        comment = str(result_dict.get("comment", "")).lower()
        return retcode == 5 or "disk error" in comment

    def _reconnect_for_order_retry(self) -> Tuple[bool, str]:
        assert mt5 is not None
        try:
            mt5.shutdown()
        except Exception:
            # Best effort: continue with initialize attempt even when shutdown reports errors.
            pass
        if not mt5.initialize():
            return False, f"initialize_failed:{mt5.last_error()}"
        if not mt5.symbol_select(self.config.symbol, True):
            return False, f"symbol_select_failed:{mt5.last_error()}"
        return True, "ok"

    def _order_send_with_filling_fallback(self, request: Dict[str, Any]) -> Dict[str, Any]:
        assert mt5 is not None
        base_request = dict(request)
        preferred = base_request.get("type_filling")
        base_request.pop("type_filling", None)

        fill_modes: List[int] = []
        if preferred is not None:
            fill_modes.append(int(preferred))
        for mode in self._default_filling_modes():
            if mode not in fill_modes:
                fill_modes.append(mode)

        invalid_fill_retcode = int(getattr(mt5, "TRADE_RETCODE_INVALID_FILL", 10030))
        attempts: List[Dict[str, Any]] = []
        last_request = dict(request)
        last_result: Optional[Dict[str, Any]] = None

        for fill_mode in fill_modes:
            attempt_request = dict(base_request)
            attempt_request["type_filling"] = fill_mode
            last_request = dict(attempt_request)
            result = mt5.order_send(attempt_request)
            if result is None:
                attempts.append({"type_filling": fill_mode, "retcode": -1, "error": str(mt5.last_error())})
                continue

            result_dict = result._asdict()
            retcode = int(result_dict.get("retcode", -1))
            result_dict["mt5_last_error"] = str(mt5.last_error())
            attempts.append(
                {
                    "type_filling": fill_mode,
                    "retcode": retcode,
                    "mt5_last_error": result_dict["mt5_last_error"],
                }
            )
            last_result = dict(result_dict)
            if retcode == invalid_fill_retcode:
                if self._preferred_filling_mode == fill_mode:
                    self._preferred_filling_mode = None
                continue

            if self._is_disk_error_result(result_dict):
                reconnect_ok, reconnect_detail = self._reconnect_for_order_retry()
                if reconnect_ok:
                    retry_result = mt5.order_send(attempt_request)
                    if retry_result is not None:
                        retry_dict = retry_result._asdict()
                        retry_retcode = int(retry_dict.get("retcode", -1))
                        retry_dict["mt5_last_error"] = str(mt5.last_error())
                        attempts.append(
                            {
                                "type_filling": fill_mode,
                                "retcode": retry_retcode,
                                "mt5_last_error": retry_dict["mt5_last_error"],
                                "reconnect_retry": True,
                            }
                        )
                        last_result = dict(retry_dict)
                        if retry_retcode == invalid_fill_retcode:
                            if self._preferred_filling_mode == fill_mode:
                                self._preferred_filling_mode = None
                            continue
                        if self._is_success_retcode(retry_retcode):
                            self._preferred_filling_mode = fill_mode
                        retry_dict["request"] = attempt_request
                        retry_dict["reconnect_retried"] = True
                        retry_dict["reconnect_success"] = True
                        retry_dict["reconnect_detail"] = reconnect_detail
                        if attempts:
                            retry_dict["filling_attempts"] = attempts
                        return retry_dict
                    attempts.append(
                        {
                            "type_filling": fill_mode,
                            "retcode": -1,
                            "error": str(mt5.last_error()),
                            "reconnect_retry": True,
                        }
                    )
                result_dict["request"] = attempt_request
                result_dict["reconnect_retried"] = True
                result_dict["reconnect_success"] = bool(reconnect_ok)
                result_dict["reconnect_detail"] = reconnect_detail
                if attempts:
                    result_dict["filling_attempts"] = attempts
                return result_dict

            if self._is_success_retcode(retcode):
                self._preferred_filling_mode = fill_mode
            result_dict["request"] = attempt_request
            if len(attempts) > 1:
                result_dict["filling_attempts"] = attempts
            return result_dict

        if last_result is not None:
            out = dict(last_result)
            out["request"] = last_request
            if attempts:
                out["filling_attempts"] = attempts
            return out
        return {"retcode": -1, "error": str(mt5.last_error()), "request": last_request, "filling_attempts": attempts}

    def close_opposite_positions(self, signal: str) -> List[Dict[str, Any]]:
        assert mt5 is not None
        ask, bid, _point = self.get_tick_context()
        normalized_signal = signal.upper()
        results: List[Dict[str, Any]] = []
        for position in self.own_positions():
            side = self._position_side(position)
            if not side or side == normalized_signal:
                continue
            volume = float(getattr(position, "volume", 0.0))
            ticket = int(getattr(position, "ticket", 0))
            if side == "BUY":
                close_type = mt5.ORDER_TYPE_SELL
                price = bid
            else:
                close_type = mt5.ORDER_TYPE_BUY
                price = ask
            request: Dict[str, Any] = {
                "action": mt5.TRADE_ACTION_DEAL,
                "symbol": self.config.symbol,
                "position": ticket,
                "volume": volume,
                "type": close_type,
                "price": price,
                "deviation": self.config.deviation,
                "magic": self.config.magic,
                "comment": f"{self.config.comment}_close",
                "type_time": mt5.ORDER_TIME_GTC,
                "type_filling": self._initial_filling_mode(),
            }
            if self.config.dry_run:
                results.append(
                    {
                        "retcode": 0,
                        "dry_run": True,
                        "closed_side": side,
                        "request": request,
                    }
                )
                continue
            result_dict = self._order_send_with_filling_fallback(request)
            result_dict["closed_side"] = side
            results.append(result_dict)
        return results

    def _position_side(self, position: Any) -> Optional[str]:
        assert mt5 is not None
        position_type = int(getattr(position, "type", -1))
        if position_type == int(mt5.POSITION_TYPE_BUY):
            return "BUY"
        if position_type == int(mt5.POSITION_TYPE_SELL):
            return "SELL"
        return None

    def send_market_order(self, side: str, sl: float, tp: float) -> Dict[str, Any]:
        assert mt5 is not None
        ask, bid, _point = self.get_tick_context()
        normalized_side = side.upper()
        price = ask if normalized_side == "BUY" else bid
        order_type = mt5.ORDER_TYPE_BUY if normalized_side == "BUY" else mt5.ORDER_TYPE_SELL
        request: Dict[str, Any] = {
            "action": mt5.TRADE_ACTION_DEAL,
            "symbol": self.config.symbol,
            "volume": self.config.lot,
            "type": order_type,
            "price": price,
            "sl": sl,
            "tp": tp,
            "deviation": self.config.deviation,
            "magic": self.config.magic,
            "comment": self.config.comment,
            "type_time": mt5.ORDER_TIME_GTC,
            "type_filling": self._initial_filling_mode(),
        }
        if self.config.dry_run:
            return {"retcode": 0, "dry_run": True, "request": request}
        return self._order_send_with_filling_fallback(request)


def evaluate_once(
    config: BotConfig,
    gateway: Mt5Gateway,
    last_bar_time: Optional[int],
    *,
    eval_state: Optional[LiveEvalState] = None,
) -> Tuple[Dict[str, Any], int]:
    try:
        rates = gateway.fetch_rates()
    except RuntimeError as exc:
        # MT5 can temporarily return too few bars around session boundaries.
        # Treat it as a retryable wait condition instead of crashing the live loop.
        if "not enough bars" in str(exc).lower():
            fallback_time = int(last_bar_time) if last_bar_time is not None else 0
            return {
                "action": "SKIP",
                "reason": "not_enough_bars",
                "symbol": config.symbol,
                "gap_gate_checked": False,
            }, fallback_time
        raise
    bar_time = int(rates["time"][-1])
    if last_bar_time is not None and bar_time == last_bar_time:
        return {"action": "SKIP", "reason": "no_new_bar", "gap_gate_checked": False}, bar_time

    closes = rates["close"]
    highs = rates["high"]
    lows = rates["low"]
    hour_utc = datetime.fromtimestamp(bar_time, tz=timezone.utc).hour

    if not is_session_allowed(
        hour_utc=hour_utc,
        session_start=config.session_start_hour_utc,
        session_end=config.session_end_hour_utc,
    ):
        return {
            "action": "BLOCKED",
            "reason": "session",
            "symbol": config.symbol,
            "hour_utc": hour_utc,
            "session_start_hour_utc": config.session_start_hour_utc,
            "session_end_hour_utc": config.session_end_hour_utc,
            "gap_gate_checked": False,
        }, bar_time

    local_state = eval_state if eval_state is not None else LiveEvalState()
    commander_payload: Dict[str, Any] = {}
    commander_snapshot: Optional[M45CommanderSnapshot] = None
    effective_min_ema_gap_over_atr = float(config.min_ema_gap_over_atr)
    effective_pullback_atr = float(config.pullback_atr)
    trend_override_state = "disabled"
    if _requires_m45_commander(config):
        try:
            commander_snapshot = _derive_m45_commander_snapshot(config, gateway)
        except (RuntimeError, ValueError) as exc:
            return {
                "action": "BLOCKED",
                "reason": "m45_commander_unavailable",
                "symbol": config.symbol,
                "error": str(exc),
                "m45_gate_policy": config.m45_bias_gate_policy,
                "trend_override_enabled": bool(config.trend_override_enabled),
                "gap_gate_checked": False,
            }, bar_time
        trend_override_state, effective_min_ema_gap_over_atr, effective_pullback_atr = _resolve_live_trend_override(
            config=config,
            state=local_state,
            regime_strength=commander_snapshot.regime_strength,
        )
        commander_payload = {
            "m45_regime_strength": float(commander_snapshot.regime_strength),
            "m45_bias": _format_bias_label(commander_snapshot.bias),
            "m45_bar_time": int(commander_snapshot.bar_time),
            "m45_gate_policy": str(config.m45_bias_gate_policy),
            "m45_neutral_policy": str(config.m45_neutral_policy),
            "m45_gate_min_strength_to_block_opposite": config.m45_gate_min_strength_to_block_opposite,
            "m45_commander_source_timeframe": config.m45_commander_source_timeframe,
            "m45_commander_resample_factor": int(config.m45_commander_resample_factor),
            "m45_commander_resample_anchor": config.m45_commander_resample_anchor,
            "m45_commander_resample_offset_minutes": int(config.m45_commander_resample_offset_minutes),
            "trend_override_state": trend_override_state,
        }

    ema_fast = ema_last(closes, config.fast_ema)
    ema_slow = ema_last(closes, config.slow_ema)
    atr_values = atr_series(highs, lows, closes, config.atr_period)
    atr_value = atr_values[-1] if atr_values else 0.0
    atr_pct_values = atr_pct_series(atr_values, closes)
    ema_gap_over_atr = abs(ema_fast - ema_slow) / atr_value if atr_value > 0.0 else 0.0
    if not volatility_filter_pass(
        atr_pct_values=atr_pct_values,
        min_ratio_to_median=config.min_atr_ratio_to_median,
        max_ratio_to_median=config.max_atr_ratio_to_median,
        window=config.atr_filter_window,
        min_samples=config.atr_filter_min_samples,
    ):
        return {
            "action": "BLOCKED",
            "reason": "volatility",
            "symbol": config.symbol,
            "atr_pct": atr_pct_values[-1] if atr_pct_values else 0.0,
            "min_ratio_to_median": config.min_atr_ratio_to_median,
            "max_ratio_to_median": config.max_atr_ratio_to_median,
            "gap_gate_checked": False,
        }, bar_time
    if atr_value <= 0.0:
        return {
            "action": "HOLD",
            "reason": "atr_unavailable",
            "symbol": config.symbol,
            "ema_fast": ema_fast,
            "ema_slow": ema_slow,
            "atr": atr_value,
            "ema_gap_over_atr": ema_gap_over_atr,
            "min_ema_gap_over_atr": effective_min_ema_gap_over_atr,
            "max_ema_gap_over_atr": config.max_ema_gap_over_atr,
            "pullback_atr": effective_pullback_atr,
            "gap_gate_checked": True,
            **commander_payload,
        }, bar_time
    if not (effective_min_ema_gap_over_atr <= ema_gap_over_atr <= config.max_ema_gap_over_atr):
        return {
            "action": "HOLD",
            "reason": "ema_gap_out_of_range",
            "symbol": config.symbol,
            "ema_fast": ema_fast,
            "ema_slow": ema_slow,
            "atr": atr_value,
            "ema_gap_over_atr": ema_gap_over_atr,
            "min_ema_gap_over_atr": effective_min_ema_gap_over_atr,
            "max_ema_gap_over_atr": config.max_ema_gap_over_atr,
            "pullback_atr": effective_pullback_atr,
            "gap_gate_checked": True,
            **commander_payload,
        }, bar_time
    signal_ctx = decide_signal_with_mode(
        strategy_mode=config.strategy_mode,
        last_close=closes[-1],
        ema_fast=ema_fast,
        ema_slow=ema_slow,
        atr_value=atr_value,
        pullback_atr=effective_pullback_atr,
        reversion_atr=config.reversion_atr,
        trend_threshold=config.regime_trend_threshold,
    )
    signal = str(signal_ctx.get("signal", "HOLD"))
    signal_source = str(signal_ctx.get("source", "trend"))
    regime = str(signal_ctx.get("regime", "trend"))
    gate_active = False
    gate_rejected = False
    if commander_snapshot is not None:
        gate_active = config.m45_bias_gate_policy != "none"
        if gate_active and config.m45_gate_min_strength_to_block_opposite is not None:
            gate_active = commander_snapshot.regime_strength >= float(config.m45_gate_min_strength_to_block_opposite)
        signal, gate_rejected = apply_m45_bias_gate_signal(
            signal=signal,
            bias=commander_snapshot.bias,
            gate_policy=config.m45_bias_gate_policy,
            neutral_policy=config.m45_neutral_policy,
            gate_active=gate_active,
        )
        commander_payload["m45_gate_active"] = bool(gate_active)
        commander_payload["m45_gate_rejected"] = bool(gate_rejected)
        if gate_rejected:
            return {
                "action": "HOLD",
                "reason": "m45_bias_gate_blocked",
                "symbol": config.symbol,
                "regime": regime,
                "signal_source": signal_source,
                "ema_gap_over_atr": ema_gap_over_atr,
                "min_ema_gap_over_atr": effective_min_ema_gap_over_atr,
                "max_ema_gap_over_atr": config.max_ema_gap_over_atr,
                "pullback_atr": effective_pullback_atr,
                "gap_gate_checked": True,
                **commander_payload,
            }, bar_time

    ask, bid, point = gateway.get_tick_context()
    spread_points = (ask - bid) / point
    open_positions = gateway.open_positions()
    if signal == "HOLD":
        return {
            "action": "HOLD",
            "symbol": config.symbol,
            "ema_fast": ema_fast,
            "ema_slow": ema_slow,
            "atr": atr_value,
            "ema_gap_over_atr": ema_gap_over_atr,
            "strategy_mode": config.strategy_mode,
            "regime": regime,
            "signal_source": signal_source,
            "min_ema_gap_over_atr": effective_min_ema_gap_over_atr,
            "max_ema_gap_over_atr": config.max_ema_gap_over_atr,
            "pullback_atr": effective_pullback_atr,
            "spread_points": spread_points,
            "open_positions": open_positions,
            "gap_gate_checked": True,
            **commander_payload,
        }, bar_time

    if gateway.has_opposite_position(signal):
        close_results = gateway.close_opposite_positions(signal)
        remaining_positions = gateway.open_positions()
        if remaining_positions > 0:
            return {
                "action": "BLOCKED",
                "reason": "opposite_close_failed",
                "symbol": config.symbol,
                "signal": signal,
                "regime": regime,
                "signal_source": signal_source,
                "ema_gap_over_atr": ema_gap_over_atr,
                "min_ema_gap_over_atr": effective_min_ema_gap_over_atr,
                "pullback_atr": effective_pullback_atr,
                "open_positions": remaining_positions,
                "close_results": close_results,
                "gap_gate_checked": True,
                **commander_payload,
            }, bar_time
        return {
            "action": "CLOSE",
            "symbol": config.symbol,
            "signal": signal,
            "regime": regime,
            "signal_source": signal_source,
            "ema_gap_over_atr": ema_gap_over_atr,
            "min_ema_gap_over_atr": effective_min_ema_gap_over_atr,
            "pullback_atr": effective_pullback_atr,
            "spread_points": spread_points,
            "close_results": close_results,
            "open_positions": remaining_positions,
            "gap_gate_checked": True,
            **commander_payload,
        }, bar_time

    if not can_open_trade(
        spread_points=spread_points,
        max_spread_points=config.max_spread_points,
        open_positions=open_positions,
        max_positions=config.max_positions,
    ):
        if spread_points > config.max_spread_points:
            blocked_reason = "spread"
        elif open_positions >= config.max_positions:
            blocked_reason = "max_positions"
        else:
            blocked_reason = "spread_or_max_positions"
        return {
            "action": "BLOCKED",
            "reason": blocked_reason,
            "symbol": config.symbol,
            "signal": signal,
            "regime": regime,
            "signal_source": signal_source,
            "ema_gap_over_atr": ema_gap_over_atr,
            "min_ema_gap_over_atr": effective_min_ema_gap_over_atr,
            "pullback_atr": effective_pullback_atr,
            "spread_points": spread_points,
            "max_spread_points": config.max_spread_points,
            "open_positions": open_positions,
            "max_positions": config.max_positions,
            "gap_gate_checked": True,
            **commander_payload,
        }, bar_time

    entry_price = ask if signal == "BUY" else bid
    sl_atr = config.reversion_sl_atr if signal_source == "reversion" else config.sl_atr
    tp_atr = config.reversion_tp_atr if signal_source == "reversion" else config.tp_atr
    sl, tp = build_sl_tp(
        side=signal,
        entry_price=entry_price,
        atr_value=atr_value,
        sl_atr=sl_atr,
        tp_atr=tp_atr,
    )
    order_result = gateway.send_market_order(signal, sl=sl, tp=tp)
    return {
        "action": "ORDER",
        "symbol": config.symbol,
        "side": signal,
        "regime": regime,
        "signal_source": signal_source,
        "ema_gap_over_atr": ema_gap_over_atr,
        "min_ema_gap_over_atr": effective_min_ema_gap_over_atr,
        "max_ema_gap_over_atr": config.max_ema_gap_over_atr,
        "strategy_mode": config.strategy_mode,
        "pullback_atr": effective_pullback_atr,
        "entry_price": entry_price,
        "sl": sl,
        "tp": tp,
        "spread_points": spread_points,
        "order_result": order_result,
        "gap_gate_checked": True,
        **commander_payload,
    }, bar_time


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Lightweight MT5 XAUUSD auto-trading bot")
    parser.add_argument(
        "--config",
        default="",
        help="Path to JSON config file (default: auto-select active/tuned config)",
    )
    parser.add_argument("--live", action="store_true", help="Enable live order sending")
    parser.add_argument("--mode", choices=["live", "research"], default="live", help="Execution contract mode")
    parser.add_argument("--loop", action="store_true", help="Run continuously on new bars")
    parser.add_argument("--poll-seconds", type=int, default=0, help="Polling interval in seconds")
    parser.add_argument("--max-cycles", type=int, default=-1, help="Stop after N loop cycles (0 = unlimited)")
    return parser.parse_args()


def _load_config(path: str) -> Dict[str, Any]:
    if not path:
        return {}
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)


def resolve_config_path(path: str, default_candidates: Tuple[str, ...] = DEFAULT_CONFIG_CANDIDATES) -> str:
    if path:
        return path
    for candidate in default_candidates:
        if Path(candidate).exists():
            return candidate
    return ""


def run(config: BotConfig, *, config_path: str = "") -> None:
    notifier = SessionBlockNotifier.from_env()
    live_guard = LiveUnderperformanceGuard.from_env()
    runtime_metrics = _new_runtime_metrics()
    runtime_journal_path = Path(runtime_journal_path_from_env())
    trial_run_id = runtime_trial_run_id_from_env(config_path=config_path)
    comment_prefix = str(config.comment[:16])
    gateway = Mt5Gateway(config)
    eval_state = LiveEvalState(trend_override_state=config.trend_override_initial_state)
    gateway.connect()
    try:
        last_bar_time: Optional[int] = None
        cycles = 0
        while True:
            guard_payload = live_guard.check()
            if guard_payload is not None:
                now_unix = int(time.time())
                snapshot_time_utc = datetime.fromtimestamp(now_unix, tz=timezone.utc).isoformat()
                guard_payload["timestamp_utc"] = snapshot_time_utc
                guard_payload["run_id"] = trial_run_id
                guard_payload["magic"] = int(config.magic)
                guard_payload["comment"] = str(config.comment)
                guard_payload["comment_prefix"] = comment_prefix
                guard_payload["runtime_metrics"] = runtime_metrics_with_metadata(
                    runtime_metrics,
                    snapshot_time_utc=snapshot_time_utc,
                    run_id=trial_run_id,
                    config=config,
                )
                print(json.dumps(guard_payload, ensure_ascii=True))
                try:
                    append_runtime_journal(runtime_journal_path, guard_payload)
                except (OSError, TypeError, ValueError) as exc:
                    print(
                        json.dumps(
                            {
                                "action": "WARN",
                                "reason": "runtime_journal_write_failed",
                                "journal_path": str(runtime_journal_path),
                                "error": str(exc),
                            },
                            ensure_ascii=True,
                        )
                    )
                notifier.maybe_notify(guard_payload, config=config, bar_time=now_unix)
                cycles += 1
                if config.once:
                    break
                if config.max_cycles > 0 and cycles >= config.max_cycles:
                    break
                time.sleep(max(1, config.poll_seconds))
                continue

            payload, last_bar_time = evaluate_once(
                config,
                gateway,
                last_bar_time,
                eval_state=eval_state,
            )
            update_runtime_metrics(runtime_metrics, payload)
            snapshot_time_utc = datetime.fromtimestamp(last_bar_time, tz=timezone.utc).isoformat()
            payload["timestamp_utc"] = snapshot_time_utc
            payload["run_id"] = trial_run_id
            payload["magic"] = int(config.magic)
            payload["comment"] = str(config.comment)
            payload["comment_prefix"] = comment_prefix
            payload["runtime_metrics"] = runtime_metrics_with_metadata(
                runtime_metrics,
                snapshot_time_utc=snapshot_time_utc,
                run_id=trial_run_id,
                config=config,
            )
            print(json.dumps(payload, ensure_ascii=True))
            try:
                append_runtime_journal(runtime_journal_path, payload)
            except (OSError, TypeError, ValueError) as exc:
                print(
                    json.dumps(
                        {
                            "action": "WARN",
                            "reason": "runtime_journal_write_failed",
                            "journal_path": str(runtime_journal_path),
                            "error": str(exc),
                        },
                        ensure_ascii=True,
                    )
                )
            notifier.maybe_notify(payload, config=config, bar_time=last_bar_time)
            cycles += 1
            if config.once:
                break
            if config.max_cycles > 0 and cycles >= config.max_cycles:
                break
            time.sleep(max(1, config.poll_seconds))
    finally:
        gateway.shutdown()


def main() -> None:
    args = _parse_args()
    config_path = resolve_config_path(args.config)
    raw = _load_config(config_path)
    config = BotConfig.from_dict(raw)
    config = hydrate_m45_commander_from_profile(config, config_path=config_path)

    if args.live and str(args.mode) == "research":
        raise ValueError("unsupported combination: --live with --mode research")

    if args.live:
        config.dry_run = False
    if str(args.mode) == "research":
        config.dry_run = True
    if args.loop:
        config.once = False
    if args.poll_seconds > 0:
        config.poll_seconds = args.poll_seconds
    if args.max_cycles >= 0:
        config.max_cycles = args.max_cycles

    config.timeframe = validate_runtime_timeframe_contract(config.timeframe, mode=str(args.mode))
    run(config, config_path=config_path)


if __name__ == "__main__":
    main()
