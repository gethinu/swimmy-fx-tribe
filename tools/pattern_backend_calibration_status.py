#!/usr/bin/env python3
"""Health check for Pattern backend calibration timer/report."""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Mapping, Tuple


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()


def parse_iso8601(value: str) -> datetime:
    text = str(value or "").strip()
    if text.endswith("Z"):
        text = text[:-1] + "+00:00"
    dt = datetime.fromisoformat(text)
    if dt.tzinfo is None:
        dt = dt.replace(tzinfo=timezone.utc)
    return dt.astimezone(timezone.utc)


def check_timer_enabled(unit: str) -> Tuple[str, str]:
    if not str(unit or "").strip():
        return "unknown", "empty timer unit"
    try:
        proc = subprocess.run(
            ["systemctl", "--user", "is-enabled", str(unit)],
            capture_output=True,
            text=True,
            check=False,
        )
    except FileNotFoundError:
        return "unknown", "systemctl not found"
    except Exception as exc:
        return "unknown", str(exc)

    out = str(proc.stdout or "").strip()
    err = str(proc.stderr or "").strip()
    state = out or err or f"rc={proc.returncode}"
    if state in ("enabled", "enabled-runtime"):
        return "enabled", state
    if state in ("disabled", "masked", "static", "not-found"):
        return "disabled", state
    return "unknown", state


def evaluate_status(
    report_path: Path,
    *,
    max_age_seconds: int,
    timer_unit: str,
    check_timer: bool,
) -> Dict[str, Any]:
    issues: List[str] = []
    warnings: List[str] = []
    report_payload: Mapping[str, Any] = {}
    now = datetime.now(timezone.utc)

    if not report_path.exists():
        issues.append(f"report missing: {report_path}")
    else:
        try:
            payload = json.loads(report_path.read_text(encoding="utf-8"))
            if isinstance(payload, Mapping):
                report_payload = payload
            else:
                issues.append("report json is not an object")
        except Exception as exc:
            issues.append(f"report parse error: {exc}")

    age_seconds = None
    best_weight = None
    combos = None
    if report_payload:
        generated_at = report_payload.get("generated_at")
        try:
            ts = parse_iso8601(str(generated_at))
            age_seconds = max(0, int((now - ts).total_seconds()))
            if age_seconds > int(max_age_seconds):
                issues.append(f"report stale: age={age_seconds}s > {int(max_age_seconds)}s")
        except Exception as exc:
            issues.append(f"invalid generated_at: {exc}")

        status = report_payload.get("status")
        ok = status.get("ok") if isinstance(status, Mapping) else None
        if ok is not True:
            issues.append("report status.ok is not true")
        if isinstance(status, Mapping):
            try:
                combos = int(status.get("combos_evaluated", 0))
                if combos <= 0:
                    issues.append("combos_evaluated <= 0")
            except Exception:
                issues.append("combos_evaluated is invalid")

        global_obj = report_payload.get("global")
        if isinstance(global_obj, Mapping):
            try:
                best_weight = float(global_obj.get("best_vector_weight"))
                if best_weight < 0.0 or best_weight > 1.0:
                    issues.append("best_vector_weight out of range [0,1]")
            except Exception:
                issues.append("best_vector_weight is invalid")
        else:
            issues.append("global section missing")

    timer_state = "skipped"
    timer_detail = ""
    if check_timer:
        timer_state, timer_detail = check_timer_enabled(str(timer_unit))
        if timer_state == "disabled":
            issues.append(f"timer disabled: {timer_unit} ({timer_detail})")
        elif timer_state == "unknown":
            warnings.append(f"timer state unknown: {timer_unit} ({timer_detail})")

    return {
        "schema_version": 1,
        "generated_at": now.isoformat(),
        "ok": len(issues) == 0,
        "issues": issues,
        "warnings": warnings,
        "timer": {
            "unit": str(timer_unit),
            "state": timer_state,
            "detail": timer_detail,
            "checked": bool(check_timer),
        },
        "report": {
            "path": str(report_path),
            "age_seconds": age_seconds,
            "combos_evaluated": combos,
            "best_vector_weight": best_weight,
        },
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--report",
        default=str(BASE_DIR / "data" / "reports" / "pattern_backend_calibration_latest.json"),
    )
    parser.add_argument(
        "--timer-unit",
        default="swimmy-pattern-backend-calibration.timer",
    )
    parser.add_argument("--max-age-seconds", type=int, default=172800)
    parser.add_argument("--skip-timer-check", action="store_true")
    parser.add_argument("--fail-on-problem", action="store_true")
    args = parser.parse_args()

    status = evaluate_status(
        Path(str(args.report)).expanduser(),
        max_age_seconds=max(60, int(args.max_age_seconds)),
        timer_unit=str(args.timer_unit),
        check_timer=(not bool(args.skip_timer_check)),
    )

    print(json.dumps(status, ensure_ascii=True, indent=2))
    if bool(args.fail_on_problem) and not bool(status.get("ok")):
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
