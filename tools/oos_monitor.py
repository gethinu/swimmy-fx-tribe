#!/usr/bin/env python3
"""Lightweight OOS monitor.

- Reads incremental portion of logs/swimmy.log
- Summarizes OOS queue status from SQLite
- Appends a concise line to logs/oos_monitor.log
"""
from __future__ import annotations

import json
import os
import sqlite3
import time
from datetime import datetime
from pathlib import Path
from typing import Dict, Tuple

BASE_DIR = Path("/home/swimmy/swimmy")
LOG_PATH = BASE_DIR / "logs" / "swimmy.log"
OUT_LOG_PATH = BASE_DIR / "logs" / "oos_monitor.log"
STATE_PATH = BASE_DIR / "data" / "reports" / "oos_monitor_state.json"
STATUS_PATH = BASE_DIR / "data" / "reports" / "oos_monitor_status.txt"
DB_PATH = BASE_DIR / "data" / "memory" / "swimmy.db"

PATTERNS = {
    "not_found": "Strategy not found for OOS result",
    "stale": "Stale OOS result ignored",
    "ignored": "OOS result ignored",
}

ALERT_THRESHOLD = {
    "not_found": 1,
    "stale": 3,
    "ignored": 5,
}

ALERT_QUEUE_ERROR_THRESHOLD = 1


def _load_state() -> Dict[str, int]:
    if not STATE_PATH.exists():
        return {"inode": 0, "offset": 0}
    try:
        return json.loads(STATE_PATH.read_text())
    except Exception:
        return {"inode": 0, "offset": 0}


def _save_state(inode: int, offset: int) -> None:
    STATE_PATH.parent.mkdir(parents=True, exist_ok=True)
    STATE_PATH.write_text(json.dumps({"inode": inode, "offset": offset}))


def _scan_log() -> Tuple[Dict[str, int], int, int, bool]:
    if not LOG_PATH.exists():
        return {k: 0 for k in PATTERNS}, 0, 0, False

    st = LOG_PATH.stat()
    inode = st.st_ino
    size = st.st_size

    state = _load_state()
    last_inode = int(state.get("inode", 0))
    last_offset = int(state.get("offset", 0))

    # Reset offset if rotated or truncated
    if inode != last_inode or size < last_offset:
        last_offset = 0

    counts = {k: 0 for k in PATTERNS}
    truncated = False

    with LOG_PATH.open("r", errors="ignore") as f:
        f.seek(last_offset)
        chunk = f.read()
        # Guardrail: if chunk too large, only keep tail to avoid memory spikes.
        if len(chunk) > 5_000_000:
            truncated = True
            chunk = chunk[-5_000_000:]
        for line in chunk.splitlines():
            for key, needle in PATTERNS.items():
                if needle in line:
                    counts[key] += 1

        new_offset = f.tell()

    _save_state(inode, new_offset)
    return counts, new_offset, size, truncated


def _fetch_oos_queue_counts() -> Dict[str, int]:
    if not DB_PATH.exists():
        return {}
    try:
        con = sqlite3.connect(str(DB_PATH))
        cur = con.cursor()
        cur.execute("select status, count(*) from oos_queue group by status order by status")
        rows = cur.fetchall()
        con.close()
        return {str(status): int(count) for status, count in rows}
    except Exception:
        return {"error": -1}


def main() -> int:
    ts = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    counts, offset, size, truncated = _scan_log()
    queue_counts = _fetch_oos_queue_counts()

    alert_reasons = []
    for key, threshold in ALERT_THRESHOLD.items():
        if counts.get(key, 0) >= threshold:
            alert_reasons.append(f"log:{key}>={threshold}")
    if queue_counts.get("error", 0) >= ALERT_QUEUE_ERROR_THRESHOLD:
        alert_reasons.append(f"queue:error>={ALERT_QUEUE_ERROR_THRESHOLD}")

    line = {
        "ts": ts,
        "log_new": counts,
        "log_truncated": truncated,
        "log_offset": offset,
        "log_size": size,
        "oos_queue": queue_counts,
        "alert": bool(alert_reasons),
        "alert_reasons": alert_reasons,
    }

    OUT_LOG_PATH.parent.mkdir(parents=True, exist_ok=True)
    with OUT_LOG_PATH.open("a") as out:
        out.write(json.dumps(line, ensure_ascii=True) + "\n")

    STATUS_PATH.parent.mkdir(parents=True, exist_ok=True)
    queue_text = " ".join(f"{k}={v}" for k, v in sorted(queue_counts.items())) or "empty"
    status_lines = [
        f"timestamp: {ts}",
        f"log_new_not_found: {counts.get('not_found', 0)}",
        f"log_new_stale: {counts.get('stale', 0)}",
        f"log_new_ignored: {counts.get('ignored', 0)}",
        f"oos_queue: {queue_text}",
        f"alert: {bool(alert_reasons)}",
        f"alert_reasons: {', '.join(alert_reasons) if alert_reasons else 'none'}",
    ]
    STATUS_PATH.write_text("\n".join(status_lines) + "\n")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
