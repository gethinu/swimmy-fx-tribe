from datetime import datetime, timezone, timedelta
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[1]
PY_SRC = ROOT / "src" / "python"
if str(PY_SRC) not in sys.path:
    sys.path.insert(0, str(PY_SRC))

from status_unifier import JST, parse_jst_timestamp, compute_updated_info


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
