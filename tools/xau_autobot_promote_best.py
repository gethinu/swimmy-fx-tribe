#!/usr/bin/env python3
"""Promote best XAU autobot period config from comparison summary."""

from __future__ import annotations

import argparse
import json
import shutil
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple


def _verdict_score(verdict: str) -> float:
    key = str(verdict or "").upper()
    if key == "GO":
        return 1.0
    if key == "CAUTION":
        return 0.4
    return -1.0


def _as_float(value: object, default: float = 0.0) -> float:
    try:
        if value is None:
            return default
        return float(value)
    except (TypeError, ValueError):
        return default


def _build_live_summary_from_report(data: Dict[str, object]) -> Dict[str, float]:
    summary = data.get("summary") if isinstance(data.get("summary"), dict) else {}
    return {
        "closed_positions": _as_float(summary.get("closed_positions", 0.0), 0.0),
        "win_rate": _as_float(summary.get("win_rate", 0.0), 0.0),
        "net_profit": _as_float(summary.get("net_profit", 0.0), 0.0),
        "profit_factor": _as_float(summary.get("profit_factor", 0.0), 0.0),
    }


def _load_live_report(path: Path) -> Optional[Dict[str, float]]:
    try:
        with path.open("r", encoding="utf-8") as f:
            data = json.load(f)
    except Exception:
        return None
    if not isinstance(data, dict):
        return None
    return _build_live_summary_from_report(data)


def _path_age_hours(path: Path, *, now_utc: datetime) -> float:
    modified = datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc)
    return float((now_utc - modified).total_seconds() / 3600.0)


def resolve_live_summary(
    *,
    live_report: str,
    live_reports_dir: Path,
    max_age_hours: float = 48.0,
    now_utc: Optional[datetime] = None,
) -> Tuple[Optional[Path], Dict[str, float]]:
    if now_utc is None:
        now_utc = datetime.now(timezone.utc)

    explicit = str(live_report or "").strip()
    if explicit:
        explicit_path = Path(explicit)
        summary = _load_live_report(explicit_path)
        return (explicit_path, summary or {})

    candidates = sorted(
        live_reports_dir.glob("xau_autobot_live_report_*.json"),
        key=lambda p: p.stat().st_mtime,
        reverse=True,
    )

    valid: List[Tuple[Path, Dict[str, float]]] = []
    nonzero: List[Tuple[Path, Dict[str, float]]] = []
    for candidate in candidates:
        summary = _load_live_report(candidate)
        if summary is None:
            continue
        if max_age_hours > 0.0:
            age_hours = _path_age_hours(candidate, now_utc=now_utc)
            if age_hours > max_age_hours:
                continue
        valid.append((candidate, summary))
        if summary.get("closed_positions", 0.0) > 0.0:
            nonzero.append((candidate, summary))

    if nonzero:
        return nonzero[0]
    if valid:
        return valid[0]
    return (None, {})


def _live_stress_multiplier(live_summary: Optional[Dict[str, float]]) -> float:
    if not live_summary:
        return 1.0
    closed_positions = _as_float(live_summary.get("closed_positions", 0.0), 0.0)
    if closed_positions < 10.0:
        return 1.0

    win_rate = _as_float(live_summary.get("win_rate", 0.0), 0.0)
    profit_factor = _as_float(live_summary.get("profit_factor", 0.0), 0.0)
    net_profit = _as_float(live_summary.get("net_profit", 0.0), 0.0)

    stress = 0.0
    if profit_factor < 1.0:
        stress += min(1.0, 1.0 - profit_factor)
    if win_rate < 0.45:
        stress += min(0.8, 0.45 - win_rate)
    if net_profit < 0.0:
        stress += 0.6
    return 1.0 + stress


def score_period_summary(row: Dict[str, object], *, live_summary: Optional[Dict[str, float]] = None) -> float:
    backtest = row.get("backtest", {}) if isinstance(row.get("backtest"), dict) else {}
    readiness = row.get("readiness", {}) if isinstance(row.get("readiness"), dict) else {}
    cost_guard = row.get("cost_guard", {}) if isinstance(row.get("cost_guard"), dict) else {}
    robustness = readiness.get("robustness", {}) if isinstance(readiness.get("robustness"), dict) else {}

    pf = _as_float(backtest.get("pf", 0.0), 0.0)
    total_return = _as_float(backtest.get("total_return", 0.0), 0.0)
    max_dd = _as_float(backtest.get("max_dd", 1.0), 1.0)

    readiness_v = _verdict_score(str(readiness.get("verdict", "")))
    cost_v = _verdict_score(str(cost_guard.get("verdict", "")))

    if readiness_v < 0.0 or cost_v < 0.0:
        return -999.0

    stress_multiplier = _live_stress_multiplier(live_summary)
    score = (5.0 * total_return) + (1.5 * pf) - ((3.0 * stress_multiplier) * max_dd) + readiness_v + cost_v

    oos_count = _as_float(robustness.get("oos_count", 0.0), 0.0)
    if oos_count > 0.0:
        oos_worst_total_return = _as_float(robustness.get("oos_worst_total_return", 0.0), 0.0)
        oos_worst_pf = _as_float(robustness.get("oos_worst_pf", 1.0), 1.0)
        oos_negative_ratio = _as_float(robustness.get("oos_negative_return_ratio", 0.0), 0.0)
        score += (
            (4.0 * oos_worst_total_return)
            + (0.8 * (oos_worst_pf - 1.0))
            - ((1.2 * stress_multiplier) * oos_negative_ratio)
        )
        if oos_worst_pf < 1.0:
            score -= (0.6 * stress_multiplier) * (1.0 - oos_worst_pf)
        if oos_worst_total_return < 0.0:
            score -= 0.5 * stress_multiplier

    return score


def choose_best_period(
    rows: Sequence[Dict[str, object]],
    *,
    live_summary: Optional[Dict[str, float]] = None,
) -> Dict[str, object]:
    if not rows:
        raise ValueError("no period rows")
    scored = [(score_period_summary(row, live_summary=live_summary), row) for row in rows]
    scored.sort(key=lambda x: x[0], reverse=True)
    return scored[0][1]


def _load_comparison(path: Path) -> Dict[str, object]:
    with path.open("r", encoding="utf-8") as f:
        data = json.load(f)
    if not isinstance(data, dict):
        raise RuntimeError("comparison JSON must be object")
    return data


def _extract_period_rows(data: Dict[str, object]) -> List[Dict[str, object]]:
    periods = data.get("periods", [])
    if not isinstance(periods, list):
        return []
    return [row for row in periods if isinstance(row, dict)]


def _source_config_from_period(period: str, config_dir: Path) -> Path:
    return config_dir / f"xau_autobot.tuned_auto_gc_m5_{period}.json"


def main() -> None:
    parser = argparse.ArgumentParser(description="Promote best XAU period config to active config")
    parser.add_argument("--comparison", default="data/reports/xau_autobot_cycle_comparison.json")
    parser.add_argument("--config-dir", default="tools/configs")
    parser.add_argument("--write-active", default="tools/configs/xau_autobot.tuned_auto_active.json")
    parser.add_argument("--write-report", default="data/reports/xau_autobot_promotion.json")
    parser.add_argument("--live-report", default="")
    parser.add_argument("--live-reports-dir", default="data/reports")
    parser.add_argument("--live-max-age-hours", type=float, default=48.0)
    args = parser.parse_args()

    comparison_path = Path(args.comparison)
    config_dir = Path(args.config_dir)
    active_path = Path(args.write_active)
    report_path = Path(args.write_report)
    live_reports_dir = Path(args.live_reports_dir)

    data = _load_comparison(comparison_path)
    rows = _extract_period_rows(data)
    live_report_path, live_summary = resolve_live_summary(
        live_report=args.live_report,
        live_reports_dir=live_reports_dir,
        max_age_hours=float(args.live_max_age_hours),
    )
    best = choose_best_period(rows, live_summary=live_summary)
    period = str(best.get("period", "")).strip()
    if not period:
        raise RuntimeError("best period missing")

    src_cfg = _source_config_from_period(period, config_dir)
    if not src_cfg.exists():
        raise RuntimeError(f"source config missing: {src_cfg}")

    active_path.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(src_cfg, active_path)

    report = {
        "comparison": str(comparison_path),
        "selected_period": period,
        "selected_score": score_period_summary(best, live_summary=live_summary),
        "source_config": str(src_cfg),
        "active_config": str(active_path),
        "best_row": best,
        "live_max_age_hours": float(args.live_max_age_hours),
    }
    if live_summary:
        report["live_summary"] = live_summary
    if live_report_path is not None:
        report["live_report"] = str(live_report_path)

    report_path.parent.mkdir(parents=True, exist_ok=True)
    with report_path.open("w", encoding="utf-8") as f:
        json.dump(report, f, ensure_ascii=True, indent=2)
        f.write("\n")

    print(json.dumps(report, ensure_ascii=True))


if __name__ == "__main__":
    main()
