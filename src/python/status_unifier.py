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
