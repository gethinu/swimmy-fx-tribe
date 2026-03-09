#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Sequence


REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_REPORTS_DIR = REPO_ROOT / "data" / "reports" / "mt5"


def _load_json(path: Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def _utc_now() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def _sort_key(item: Dict[str, Any]) -> tuple[int, str]:
    try:
        rank = int(item.get("shortlist_rank"))
    except (TypeError, ValueError):
        rank = 999
    return rank, str(item.get("strategy_name", ""))


def _safe_ratio(observed: object, target: object) -> float:
    try:
        observed_value = float(observed)
        target_value = float(target)
    except (TypeError, ValueError):
        return 0.0
    if target_value <= 0:
        return 0.0
    return round(observed_value / target_value, 6)


def _mean(values: List[float]) -> float:
    if not values:
        return 0.0
    return round(sum(values) / len(values), 6)


def build_status_report(stage_report_path: Path) -> Dict[str, Any]:
    stage_report = _load_json(stage_report_path)
    stage_items = stage_report.get("items", []) if isinstance(stage_report, dict) else []

    items: List[Dict[str, Any]] = []
    days_progress: List[float] = []
    trades_progress: List[float] = []
    sharpe_progress: List[float] = []
    pf_progress: List[float] = []

    for stage_item in stage_items:
        if not isinstance(stage_item, dict):
            continue
        manifest_path = Path(str(stage_item.get("manifest_path", ""))).resolve()
        manifest = _load_json(manifest_path)
        item = {
            "paper_run_id": str(manifest.get("paper_run_id", stage_item.get("paper_run_id", ""))),
            "strategy_name": str(manifest.get("strategy_name", stage_item.get("strategy_name", ""))),
            "shortlist_rank": stage_item.get("shortlist_rank"),
            "shortlist_role": str(stage_item.get("shortlist_role", "")),
            "manifest_path": str(manifest_path),
            "planned_run_root": str(stage_item.get("planned_run_root", "")),
            "execution_mode": str(manifest.get("execution_mode", "PAPER")),
            "paper_comment_prefix": str(manifest.get("paper_comment_prefix", "")),
            "target_forward_days": int(manifest.get("target_forward_days", 30)),
            "target_forward_trades": int(manifest.get("target_forward_trades", 300)),
            "target_forward_sharpe": float(manifest.get("target_forward_sharpe", 0.70)),
            "target_forward_pf": float(manifest.get("target_forward_pf", 1.50)),
            "observed_forward_days": int(manifest.get("observed_forward_days", 0)),
            "observed_forward_trades": int(manifest.get("observed_forward_trades", 0)),
            "observed_forward_sharpe": manifest.get("observed_forward_sharpe"),
            "observed_forward_pf": manifest.get("observed_forward_pf"),
            "status": str(manifest.get("status", "UNKNOWN")),
            "status_reason": str(manifest.get("status_reason", "")),
        }
        item["days_progress_ratio"] = _safe_ratio(item["observed_forward_days"], item["target_forward_days"])
        item["trades_progress_ratio"] = _safe_ratio(item["observed_forward_trades"], item["target_forward_trades"])
        item["sharpe_progress_ratio"] = _safe_ratio(item["observed_forward_sharpe"], item["target_forward_sharpe"])
        item["pf_progress_ratio"] = _safe_ratio(item["observed_forward_pf"], item["target_forward_pf"])
        items.append(item)
        days_progress.append(item["days_progress_ratio"])
        trades_progress.append(item["trades_progress_ratio"])
        sharpe_progress.append(item["sharpe_progress_ratio"])
        pf_progress.append(item["pf_progress_ratio"])

    items.sort(key=_sort_key)
    status_counts = dict(Counter(str(item.get("status", "")) for item in items))
    return {
        "schema_version": 1,
        "generated_at_utc": _utc_now(),
        "stage_report_source": str(stage_report_path),
        "runner_input_source": str(stage_report.get("runner_input_source", "")) if isinstance(stage_report, dict) else "",
        "items": items,
        "status_counts": status_counts,
        "ready_count": status_counts.get("STAGED", 0) + status_counts.get("READY_FOR_PAPER_FORWARD", 0),
        "running_count": status_counts.get("RUNNING", 0),
        "completed_count": status_counts.get("COMPLETED", 0),
        "progress_summary": {
            "days_progress_ratio_mean": _mean(days_progress),
            "trades_progress_ratio_mean": _mean(trades_progress),
            "sharpe_progress_ratio_mean": _mean(sharpe_progress),
            "pf_progress_ratio_mean": _mean(pf_progress),
        },
    }


def default_output_path() -> Path:
    stamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    return DEFAULT_REPORTS_DIR / f"legend_paper_forward_status_{stamp}.json"


def write_status_report(report: Dict[str, Any], output_path: Path) -> Path:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(report, indent=2, sort_keys=False), encoding="utf-8")
    return output_path


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Aggregate staged Legend paper-forward manifests into a status artifact.")
    parser.add_argument("--stage-report", required=True, help="Path to legend_paper_forward_stage_*.json")
    parser.add_argument(
        "--output",
        help="Optional output path. Defaults to data/reports/mt5/legend_paper_forward_status_<timestamp>.json",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)
    stage_report_path = Path(args.stage_report).resolve()
    report = build_status_report(stage_report_path)
    output_path = Path(args.output).resolve() if args.output else default_output_path()
    write_status_report(report, output_path)
    print(output_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
