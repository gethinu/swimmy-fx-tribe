#!/usr/bin/env python3
"""Judge fixed-period XAU autobot trial result with explicit GO/NO-GO thresholds."""

from __future__ import annotations

import argparse
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, Optional


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


def _path_age_hours(path: Path, *, now_utc: datetime) -> float:
    modified = datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc)
    return float((now_utc - modified).total_seconds() / 3600.0)


def resolve_live_report_path(
    *,
    live_report: str,
    reports_dir: Path,
    max_age_hours: float,
    now_utc: Optional[datetime] = None,
) -> Optional[Path]:
    if now_utc is None:
        now_utc = datetime.now(timezone.utc)

    explicit = str(live_report or "").strip()
    if explicit:
        return Path(explicit)

    candidates = sorted(
        reports_dir.glob("xau_autobot_live_report_*.json"),
        key=lambda p: p.stat().st_mtime,
        reverse=True,
    )
    for candidate in candidates:
        if max_age_hours > 0.0:
            if _path_age_hours(candidate, now_utc=now_utc) > max_age_hours:
                continue
        return candidate
    return None


def evaluate_trial_report(
    report: Dict[str, object],
    *,
    min_days: float,
    min_closed_positions: float,
    min_profit_factor: float,
    min_win_rate: float,
    min_net_profit: float,
) -> Dict[str, object]:
    summary = report.get("summary", {}) if isinstance(report.get("summary"), dict) else {}
    diagnostics = report.get("diagnostics", {}) if isinstance(report.get("diagnostics"), dict) else {}
    start_utc = _parse_utc(report.get("start_utc"))
    end_utc = _parse_utc(report.get("end_utc"))
    window_days = 0.0
    if start_utc is not None and end_utc is not None and end_utc > start_utc:
        window_days = float((end_utc - start_utc).total_seconds() / 86400.0)

    closed_positions = _as_float(summary.get("closed_positions"), 0.0)
    profit_factor = _as_float(summary.get("profit_factor"), 0.0)
    win_rate = _as_float(summary.get("win_rate"), 0.0)
    net_profit = _as_float(summary.get("net_profit"), 0.0)

    readiness_checks = {
        "window_days": window_days >= float(min_days),
        "closed_positions": closed_positions >= float(min_closed_positions),
    }
    performance_checks = {
        "profit_factor": profit_factor >= float(min_profit_factor),
        "win_rate": win_rate >= float(min_win_rate),
        "net_profit": net_profit >= float(min_net_profit),
    }
    invalid_reasons = []
    if "after_magic_filter" in diagnostics and _as_float(diagnostics.get("after_magic_filter"), -1.0) <= 0.0:
        invalid_reasons.append("after_magic_filter")
    if "after_comment_prefix_filter" in diagnostics and _as_float(
        diagnostics.get("after_comment_prefix_filter"), -1.0
    ) <= 0.0:
        invalid_reasons.append("after_comment_prefix_filter")
    trial_valid = len(invalid_reasons) == 0

    checks = {
        **readiness_checks,
        **performance_checks,
    }
    readiness_failed_checks = [name for name, ok in readiness_checks.items() if not ok]
    performance_failed_checks = [name for name, ok in performance_checks.items() if not ok]
    failed_checks = [name for name, ok in checks.items() if not ok]
    verdict = "GO"
    if not trial_valid:
        verdict = "INVALID_TRIAL"
    elif failed_checks:
        verdict = "NO_GO"
    return {
        "verdict": verdict,
        "trial_valid": trial_valid,
        "invalid_reasons": invalid_reasons,
        "window_days": window_days,
        "summary": {
            "closed_positions": closed_positions,
            "profit_factor": profit_factor,
            "win_rate": win_rate,
            "net_profit": net_profit,
        },
        "thresholds": {
            "min_days": float(min_days),
            "min_closed_positions": float(min_closed_positions),
            "min_profit_factor": float(min_profit_factor),
            "min_win_rate": float(min_win_rate),
            "min_net_profit": float(min_net_profit),
        },
        "readiness": {
            "checks": readiness_checks,
            "failed_checks": readiness_failed_checks,
        },
        "performance": {
            "checks": performance_checks,
            "failed_checks": performance_failed_checks,
        },
        "checks": checks,
        "failed_checks": failed_checks,
    }


def _load_report(path: Path) -> Dict[str, object]:
    with path.open("r", encoding="utf-8") as f:
        payload = json.load(f)
    if not isinstance(payload, dict):
        raise RuntimeError("live report must be a JSON object")
    return payload


def main() -> None:
    parser = argparse.ArgumentParser(description="Judge XAU autobot dry-run/live trial with fixed criteria")
    parser.add_argument("--live-report", default="")
    parser.add_argument("--reports-dir", default="data/reports")
    parser.add_argument("--max-age-hours", type=float, default=168.0)
    parser.add_argument("--min-days", type=float, default=14.0)
    parser.add_argument("--min-closed-positions", type=float, default=12.0)
    parser.add_argument("--min-profit-factor", type=float, default=1.10)
    parser.add_argument("--min-win-rate", type=float, default=0.42)
    parser.add_argument("--min-net-profit", type=float, default=0.0)
    parser.add_argument("--write-report", default="data/reports/xau_autobot_trial_judge.json")
    parser.add_argument("--fail-on-no-go", action="store_true")
    args = parser.parse_args()

    now_utc = datetime.now(timezone.utc)
    report_path = resolve_live_report_path(
        live_report=args.live_report,
        reports_dir=Path(args.reports_dir),
        max_age_hours=float(args.max_age_hours),
        now_utc=now_utc,
    )
    if report_path is None:
        raise RuntimeError("live report not found")
    if not report_path.exists():
        raise RuntimeError(f"live report missing: {report_path}")

    report = _load_report(report_path)
    judged = evaluate_trial_report(
        report,
        min_days=float(args.min_days),
        min_closed_positions=float(args.min_closed_positions),
        min_profit_factor=float(args.min_profit_factor),
        min_win_rate=float(args.min_win_rate),
        min_net_profit=float(args.min_net_profit),
    )
    output = {
        "generated_at": now_utc.isoformat(),
        "live_report": str(report_path),
        **judged,
    }
    print(json.dumps(output, ensure_ascii=True))

    write_path = Path(args.write_report)
    write_path.parent.mkdir(parents=True, exist_ok=True)
    with write_path.open("w", encoding="utf-8") as f:
        json.dump(output, f, ensure_ascii=True, indent=2)
        f.write("\n")

    if args.fail_on_no_go and output.get("verdict") != "GO":
        raise SystemExit(f"trial verdict: {output.get('verdict')}")


if __name__ == "__main__":
    main()
