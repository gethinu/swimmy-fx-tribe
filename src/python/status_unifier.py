from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime, timezone, timedelta
from pathlib import Path
from typing import Optional

from sexp_utils import load_sexp_alist, load_sexp_list, SexpParseError

JST = timezone(timedelta(hours=9))
DEFAULT_STALE_SECONDS = 180


@dataclass
class UpdatedInfo:
    updated_at: Optional[datetime]
    stale: bool
    stale_seconds: int
    clock_skew: bool
    source: str


def parse_jst_timestamp(value: str) -> Optional[datetime]:
    if not value:
        return None
    try:
        dt = datetime.strptime(value, "%Y-%m-%d %H:%M:%S")
        return dt.replace(tzinfo=JST)
    except Exception:
        return None


def _get_mtime(path: Path) -> Optional[datetime]:
    try:
        ts = path.stat().st_mtime
        return datetime.fromtimestamp(ts, JST)
    except Exception:
        return None


def compute_updated_info(
    live_status: dict | None,
    live_path: Path | None,
    now: datetime | None = None,
    stale_threshold: int = DEFAULT_STALE_SECONDS,
) -> UpdatedInfo:
    now = now or datetime.now(JST)
    updated_at = None
    source = "unknown"

    if live_status and isinstance(live_status, dict):
        parsed = parse_jst_timestamp(live_status.get("last_updated"))
        if parsed:
            updated_at = parsed
            source = "last_updated"

    if updated_at is None and live_path:
        updated_at = _get_mtime(live_path)
        if updated_at:
            source = "mtime"

    if updated_at is None:
        return UpdatedInfo(updated_at=None, stale=True, stale_seconds=0, clock_skew=False, source=source)

    delta = (now - updated_at).total_seconds()
    clock_skew = delta < -30
    if clock_skew:
        return UpdatedInfo(updated_at=updated_at, stale=False, stale_seconds=0, clock_skew=True, source=source)

    stale = delta > stale_threshold
    return UpdatedInfo(updated_at=updated_at, stale=stale, stale_seconds=int(delta), clock_skew=False, source=source)


def _format_duration(seconds: int) -> str:
    if seconds < 0:
        seconds = 0
    hrs = seconds // 3600
    mins = (seconds % 3600) // 60
    secs = seconds % 60
    return f"{hrs:d}:{mins:02d}:{secs:02d}"


def format_system_health(metrics: dict | None) -> str:
    if not metrics:
        return "UNKNOWN (metrics missing)"
    parts = []
    uptime = metrics.get("uptime_seconds")
    if isinstance(uptime, (int, float)):
        parts.append(f"uptime {_format_duration(int(uptime))}")
    strategies = metrics.get("strategy_count")
    if strategies is not None:
        parts.append(f"strats {int(strategies)}")
    if parts:
        return "OK (" + ", ".join(parts) + ")"
    return "OK"


def format_updated_line(info: UpdatedInfo) -> str:
    if not info.updated_at:
        return "Updated: UNKNOWN"
    time_str = info.updated_at.strftime("%H:%M:%S")
    if info.clock_skew:
        return f"Updated: {time_str} (Clock Skew?)"
    mins = max(0, info.stale_seconds // 60)
    if info.stale:
        return f"Updated: {time_str} (STALE {mins}m)"
    return f"Updated: {time_str} ({mins}m ago)"


def format_status_message(snapshot: dict) -> str:
    return (
        "🐟 **Swimmy Status**\n"
        "━━━━━━━━━━━━━━━━━\n"
        f"📊 **Daily PnL**: ¥{snapshot.get('daily_pnl', 0):,.0f}\n"
        f"💰 **Total PnL**: ¥{snapshot.get('total_pnl', 0):,.0f}\n"
        f"🎯 **Goal**: {snapshot.get('goal_progress', 0):.1f}%\n"
        f"🖥️ **System Health**: {snapshot.get('system_health', 'UNKNOWN')}\n"
        "━━━━━━━━━━━━━━━━━\n"
        f"🕐 {snapshot.get('updated_line', 'Updated: UNKNOWN')}"
    )
