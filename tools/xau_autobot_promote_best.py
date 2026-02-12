#!/usr/bin/env python3
"""Promote best XAU autobot period config from comparison summary."""

from __future__ import annotations

import argparse
import json
import shutil
from pathlib import Path
from typing import Dict, List, Sequence


def _verdict_score(verdict: str) -> float:
    key = str(verdict or "").upper()
    if key == "GO":
        return 1.0
    if key == "CAUTION":
        return 0.4
    return -1.0


def score_period_summary(row: Dict[str, object]) -> float:
    backtest = row.get("backtest", {}) if isinstance(row.get("backtest"), dict) else {}
    readiness = row.get("readiness", {}) if isinstance(row.get("readiness"), dict) else {}
    cost_guard = row.get("cost_guard", {}) if isinstance(row.get("cost_guard"), dict) else {}

    pf = float(backtest.get("pf", 0.0) or 0.0)
    total_return = float(backtest.get("total_return", 0.0) or 0.0)
    max_dd = float(backtest.get("max_dd", 1.0) or 1.0)

    readiness_v = _verdict_score(str(readiness.get("verdict", "")))
    cost_v = _verdict_score(str(cost_guard.get("verdict", "")))

    if readiness_v < 0.0 or cost_v < 0.0:
        return -999.0

    return (5.0 * total_return) + (1.5 * pf) - (3.0 * max_dd) + readiness_v + cost_v


def choose_best_period(rows: Sequence[Dict[str, object]]) -> Dict[str, object]:
    if not rows:
        raise ValueError("no period rows")
    scored = [(score_period_summary(row), row) for row in rows]
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
    args = parser.parse_args()

    comparison_path = Path(args.comparison)
    config_dir = Path(args.config_dir)
    active_path = Path(args.write_active)
    report_path = Path(args.write_report)

    data = _load_comparison(comparison_path)
    rows = _extract_period_rows(data)
    best = choose_best_period(rows)
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
        "selected_score": score_period_summary(best),
        "source_config": str(src_cfg),
        "active_config": str(active_path),
        "best_row": best,
    }

    report_path.parent.mkdir(parents=True, exist_ok=True)
    with report_path.open("w", encoding="utf-8") as f:
        json.dump(report, f, ensure_ascii=True, indent=2)
        f.write("\n")

    print(json.dumps(report, ensure_ascii=True))


if __name__ == "__main__":
    main()
