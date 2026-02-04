from datetime import datetime, timezone, timedelta
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[1]
PY_SRC = ROOT / "src" / "python"
if str(PY_SRC) not in sys.path:
    sys.path.insert(0, str(PY_SRC))

from status_unifier import (
    JST,
    parse_jst_timestamp,
    compute_updated_info,
    format_system_health,
    format_status_message,
)


def test_parse_jst_timestamp():
    ts = parse_jst_timestamp("2026-02-04 22:52:22")
    assert ts.year == 2026
    assert ts.month == 2
    assert ts.day == 4
    assert ts.tzinfo == JST


def test_compute_updated_info_fresh():
    now = datetime(2026, 2, 4, 23, 0, 0, tzinfo=JST)
    live_status = {"last_updated": "2026-02-04 22:59:00"}
    info = compute_updated_info(live_status, live_path=None, now=now, stale_threshold=180)
    assert info.stale is False
    assert info.clock_skew is False


def test_compute_updated_info_stale():
    now = datetime(2026, 2, 4, 23, 0, 0, tzinfo=JST)
    live_status = {"last_updated": "2026-02-04 22:50:00"}
    info = compute_updated_info(live_status, live_path=None, now=now, stale_threshold=180)
    assert info.stale is True


def test_compute_updated_info_clock_skew():
    now = datetime(2026, 2, 4, 23, 0, 0, tzinfo=JST)
    live_status = {"last_updated": "2026-02-04 23:10:00"}
    info = compute_updated_info(live_status, live_path=None, now=now, stale_threshold=180)
    assert info.clock_skew is True
    assert info.stale is False


def test_format_system_health_missing():
    assert format_system_health(None) == "UNKNOWN (metrics missing)"


def test_format_system_health_ok():
    metrics = {"uptime_seconds": 3600, "strategy_count": 5}
    text = format_system_health(metrics)
    assert "uptime" in text
    assert "strats 5" in text


def test_format_status_message_basic():
    msg = format_status_message(
        {
            "daily_pnl": 100,
            "total_pnl": 200,
            "goal_progress": 10.5,
            "system_health": "OK",
            "updated_line": "Updated: 22:52:22 (1m ago)",
        }
    )
    assert "Daily PnL" in msg
    assert "System Health" in msg
    assert "Updated" in msg
