#!/usr/bin/env python3
"""Trend Arbitrage status checker."""

from __future__ import annotations

import argparse
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict


def _parse_iso8601(text: str) -> datetime | None:
    value = (text or "").strip()
    if not value:
        return None
    if value.endswith("Z"):
        value = value[:-1] + "+00:00"
    try:
        dt = datetime.fromisoformat(value)
    except ValueError:
        return None
    if dt.tzinfo is None:
        dt = dt.replace(tzinfo=timezone.utc)
    return dt.astimezone(timezone.utc)


def check_status(*, latest_run_path: Path, max_age_seconds: int) -> Dict[str, Any]:
    result: Dict[str, Any] = {
        "ok": True,
        "latest_run_path": str(latest_run_path),
        "max_age_seconds": max_age_seconds,
        "age_seconds": None,
        "ran_at": None,
        "selected_count": None,
        "candidate_count": None,
        "issues": [],
    }
    issues = result["issues"]

    if not latest_run_path.exists():
        result["ok"] = False
        issues.append("latest_run.json not found")
        return result

    try:
        payload = json.loads(latest_run_path.read_text(encoding="utf-8"))
    except Exception as exc:
        result["ok"] = False
        issues.append(f"latest_run.json parse failed: {exc}")
        return result

    if not isinstance(payload, dict):
        result["ok"] = False
        issues.append("latest_run.json must be an object")
        return result

    result["selected_count"] = payload.get("selected_count")
    result["candidate_count"] = payload.get("candidate_count")
    ran_at = _parse_iso8601(str(payload.get("ran_at", "")))
    if ran_at is None:
        result["ok"] = False
        issues.append("ran_at missing or invalid")
        return result

    now = datetime.now(timezone.utc)
    age = int((now - ran_at).total_seconds())
    result["ran_at"] = ran_at.isoformat()
    result["age_seconds"] = age
    if age > max(0, int(max_age_seconds)):
        result["ok"] = False
        issues.append(f"run too old: age={age}s > max_age={max_age_seconds}s")
    return result


def main() -> None:
    parser = argparse.ArgumentParser(description="Check trend arbitrage run freshness")
    parser.add_argument(
        "--latest-run",
        default="data/trend_arbitrage/latest_run.json",
        help="Path to latest run json",
    )
    parser.add_argument("--max-age-seconds", type=int, default=4 * 3600)
    parser.add_argument("--fail-on-problem", action="store_true")
    args = parser.parse_args()

    status = check_status(
        latest_run_path=Path(args.latest_run),
        max_age_seconds=max(0, args.max_age_seconds),
    )
    print(json.dumps(status, ensure_ascii=False))
    if args.fail_on_problem and not status.get("ok", False):
        raise SystemExit(1)


if __name__ == "__main__":
    main()

