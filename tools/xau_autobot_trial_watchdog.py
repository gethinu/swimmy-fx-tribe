#!/usr/bin/env python3
"""Fail-closed watchdog for XAU trial runs.

Contract:
- If trial window reaches min_window_hours and closed_positions <= max_closed_positions,
  mark NO_GO and optionally rotate to the next trial config.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Optional


def _as_float(value: object, default: float = 0.0) -> float:
    try:
        if value is None:
            return default
        return float(value)
    except (TypeError, ValueError):
        return default


def _parse_utc(value: object) -> Optional[datetime]:
    text = str(value or "").strip()
    if not text:
        return None
    try:
        dt = datetime.fromisoformat(text.replace("Z", "+00:00"))
    except ValueError:
        return None
    if dt.tzinfo is None:
        return dt.replace(tzinfo=timezone.utc)
    return dt.astimezone(timezone.utc)


def _window_hours_from_reports(live_report: Dict[str, Any], judge_report: Dict[str, Any]) -> float:
    start_utc = _parse_utc(live_report.get("start_utc"))
    end_utc = _parse_utc(live_report.get("end_utc"))
    if start_utc and end_utc and end_utc > start_utc:
        return float((end_utc - start_utc).total_seconds() / 3600.0)
    window_days = _as_float(judge_report.get("window_days"), 0.0)
    return float(window_days * 24.0)


def _closed_positions_from_reports(live_report: Dict[str, Any], judge_report: Dict[str, Any]) -> float:
    judge_summary = judge_report.get("summary", {})
    if isinstance(judge_summary, dict):
        judge_value = _as_float(judge_summary.get("closed_positions"), -1.0)
        if judge_value >= 0.0:
            return judge_value
    live_summary = live_report.get("summary", {})
    if isinstance(live_summary, dict):
        return _as_float(live_summary.get("closed_positions"), 0.0)
    return 0.0


def evaluate_zero_trade_gate(
    *,
    run_id: str,
    live_report: Dict[str, Any],
    judge_report: Dict[str, Any],
    min_window_hours: float,
    max_closed_positions: float,
) -> Dict[str, Any]:
    window_hours = _window_hours_from_reports(live_report, judge_report)
    closed_positions = _closed_positions_from_reports(live_report, judge_report)

    triggered = window_hours >= float(min_window_hours) and closed_positions <= float(max_closed_positions)
    reason_codes: List[str] = []
    decision = "CONTINUE"
    if triggered:
        decision = "NO_GO_ROTATE_REQUIRED"
        reason_codes.append("zero_closed_positions_24h")

    return {
        "run_id": str(run_id),
        "window_hours": float(window_hours),
        "closed_positions": float(closed_positions),
        "thresholds": {
            "min_window_hours": float(min_window_hours),
            "max_closed_positions": float(max_closed_positions),
        },
        "triggered": bool(triggered),
        "decision": decision,
        "reason_codes": reason_codes,
    }


def _load_json(path: Path) -> Dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise RuntimeError(f"JSON object required: {path}")
    return payload


def _resolve_report_path(explicit: str, default_path: Path) -> Path:
    if explicit:
        return Path(explicit)
    return default_path


def _find_autobot_pids() -> List[int]:
    proc = subprocess.run(
        ["ps", "-eo", "pid=,args="],
        capture_output=True,
        text=True,
        check=False,
    )
    if proc.returncode != 0:
        return []
    pids: List[int] = []
    for line in proc.stdout.splitlines():
        text = line.strip()
        if not text:
            continue
        if "xau_autobot_live_loop.ps1" not in text and "xau_autobot.py" not in text:
            continue
        parts = text.split(maxsplit=1)
        if not parts:
            continue
        try:
            pid = int(parts[0])
        except ValueError:
            continue
        pids.append(pid)
    return sorted(set(pids))


def rotate_trial_run(
    *,
    root: Path,
    next_config: str,
    next_run_id: str,
    log_path: Path,
) -> Dict[str, Any]:
    stopped_pids = _find_autobot_pids()
    if stopped_pids:
        subprocess.run(["kill", *[str(pid) for pid in stopped_pids]], check=False)

    log_path.parent.mkdir(parents=True, exist_ok=True)
    env = dict(os.environ)
    env.update(
        {
            "XAU_AUTOBOT_TRIAL_RUN_ID": str(next_run_id),
            "XAU_AUTOBOT_TRIAL_CONFIG": str(next_config),
            "XAU_AUTOBOT_TRIAL_ALLOW_EXISTING_PROCESSES": "0",
        }
    )
    start_script = root / "tools" / "xau_autobot_trial_v2_start.sh"
    with log_path.open("ab") as logf:
        process = subprocess.Popen(  # noqa: S603
            [str(start_script)],
            cwd=str(root),
            env=env,
            stdout=logf,
            stderr=subprocess.STDOUT,
            start_new_session=True,
        )

    return {
        "stopped_pids": [int(pid) for pid in stopped_pids],
        "started_pid": int(process.pid),
        "next_run_id": str(next_run_id),
        "next_config": str(next_config),
        "start_log": str(log_path),
    }


def _default_rotated_run_id(run_id: str, *, now_utc: datetime) -> str:
    suffix = now_utc.strftime("%Y%m%d_%H%M%S")
    base = str(run_id or "trial_v2").strip() or "trial_v2"
    return f"{base}_rotated_{suffix}"


def main() -> None:
    parser = argparse.ArgumentParser(description="Enforce fail-closed rule for XAU trial zero-trade windows")
    parser.add_argument("--run-meta", default="data/reports/xau_autobot_trial_v2_current_run.json")
    parser.add_argument("--live-report", default="")
    parser.add_argument("--judge-report", default="")
    parser.add_argument("--min-window-hours", type=float, default=24.0)
    parser.add_argument("--max-closed-positions", type=float, default=0.0)
    parser.add_argument("--rotate-config", default="")
    parser.add_argument("--rotate-run-id", default="")
    parser.add_argument("--execute-rotate", action="store_true")
    parser.add_argument("--fail-on-trigger", action="store_true")
    parser.add_argument("--write-report", default="data/reports/xau_autobot_trial_watchdog.json")
    args = parser.parse_args()

    now_utc = datetime.now(timezone.utc)
    root = Path(__file__).resolve().parents[1]

    run_meta_path = Path(args.run_meta)
    run_meta = _load_json(run_meta_path)
    run_id = str(run_meta.get("run_id", "")).strip()
    if not run_id:
        raise RuntimeError(f"run_id missing in run meta: {run_meta_path}")

    default_live = root / "data" / "reports" / f"xau_autobot_live_report_{run_id}.json"
    default_judge = root / "data" / "reports" / f"xau_autobot_trial_judge_{run_id}.json"
    live_report_path = _resolve_report_path(args.live_report, default_live)
    judge_report_path = _resolve_report_path(args.judge_report, default_judge)

    live_report = _load_json(live_report_path)
    judge_report = _load_json(judge_report_path)

    decision = evaluate_zero_trade_gate(
        run_id=run_id,
        live_report=live_report,
        judge_report=judge_report,
        min_window_hours=float(args.min_window_hours),
        max_closed_positions=float(args.max_closed_positions),
    )

    rotation: Dict[str, Any] = {"requested": bool(args.execute_rotate), "performed": False}
    if decision["triggered"] and args.execute_rotate:
        rotate_config = str(args.rotate_config).strip()
        if not rotate_config:
            raise RuntimeError("--rotate-config is required when --execute-rotate and gate is triggered")
        rotate_run_id = str(args.rotate_run_id).strip()
        if not rotate_run_id:
            rotate_run_id = _default_rotated_run_id(run_id, now_utc=now_utc)
        log_path = root / "data" / "runtime" / f"xau_autobot_trial_start_{rotate_run_id}.log"
        result = rotate_trial_run(
            root=root,
            next_config=rotate_config,
            next_run_id=rotate_run_id,
            log_path=log_path,
        )
        rotation = {"requested": True, "performed": True, **result}

    output = {
        "generated_at": now_utc.isoformat(),
        "run_meta_path": str(run_meta_path),
        "live_report_path": str(live_report_path),
        "judge_report_path": str(judge_report_path),
        **decision,
        "rotation": rotation,
    }
    print(json.dumps(output, ensure_ascii=True))

    write_path = Path(args.write_report)
    write_path.parent.mkdir(parents=True, exist_ok=True)
    write_path.write_text(json.dumps(output, ensure_ascii=True, indent=2) + "\n", encoding="utf-8")

    if args.fail_on_trigger and decision["triggered"]:
        raise SystemExit("watchdog triggered: zero_closed_positions_24h")


if __name__ == "__main__":
    main()
