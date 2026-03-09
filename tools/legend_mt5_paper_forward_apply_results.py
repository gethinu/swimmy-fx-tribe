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
    return rank, str(item.get("paper_run_id", ""))


def build_apply_report(stage_report_path: Path, results_path: Path) -> Dict[str, Any]:
    stage_report = _load_json(stage_report_path)
    results_payload = _load_json(results_path)
    stage_items = stage_report.get("items", []) if isinstance(stage_report, dict) else []
    result_items = results_payload.get("items", []) if isinstance(results_payload, dict) else []

    stage_by_id: Dict[str, Dict[str, Any]] = {}
    statuses: Dict[str, str] = {}
    for stage_item in stage_items:
        if not isinstance(stage_item, dict):
            continue
        run_id = str(stage_item.get("paper_run_id", ""))
        if not run_id:
            continue
        stage_by_id[run_id] = stage_item
        statuses[run_id] = str(stage_item.get("status", "STAGED"))

    report_items: List[Dict[str, Any]] = []
    updated_count = 0
    missing_count = 0
    for result_item in result_items:
        if not isinstance(result_item, dict):
            continue
        run_id = str(result_item.get("paper_run_id", ""))
        stage_item = stage_by_id.get(run_id)
        if stage_item is None:
            missing_count += 1
            report_items.append(
                {
                    "paper_run_id": run_id,
                    "apply_status": "missing",
                    "status": str(result_item.get("status", "")),
                    "status_reason": str(result_item.get("status_reason", "")),
                }
            )
            continue

        next_status = str(result_item.get("status", "STAGED"))
        statuses[run_id] = next_status
        updated_count += 1
        report_items.append(
            {
                "paper_run_id": run_id,
                "strategy_name": str(stage_item.get("strategy_name", "")),
                "shortlist_rank": stage_item.get("shortlist_rank"),
                "manifest_path": str(Path(str(stage_item.get("manifest_path", ""))).resolve()),
                "apply_status": "updated",
                "status": next_status,
                "status_reason": str(result_item.get("status_reason", "")),
                "observed_forward_days": int(result_item.get("observed_forward_days", 0)),
                "observed_forward_trades": int(result_item.get("observed_forward_trades", 0)),
                "observed_forward_sharpe": result_item.get("observed_forward_sharpe"),
                "observed_forward_pf": result_item.get("observed_forward_pf"),
            }
        )

    untouched_ids = set(stage_by_id) - {str(item.get("paper_run_id", "")) for item in result_items if isinstance(item, dict)}
    for run_id in untouched_ids:
        stage_item = stage_by_id[run_id]
        report_items.append(
            {
                "paper_run_id": run_id,
                "strategy_name": str(stage_item.get("strategy_name", "")),
                "shortlist_rank": stage_item.get("shortlist_rank"),
                "manifest_path": str(Path(str(stage_item.get("manifest_path", ""))).resolve()),
                "apply_status": "unchanged",
                "status": statuses[run_id],
                "status_reason": str(stage_item.get("status_reason", "")),
            }
        )

    report_items.sort(key=_sort_key)
    status_counts = dict(Counter(statuses.values()))
    return {
        "schema_version": 1,
        "generated_at_utc": _utc_now(),
        "stage_report_source": str(stage_report_path),
        "results_source": str(results_path),
        "updated_count": updated_count,
        "missing_count": missing_count,
        "status_counts": status_counts,
        "items": report_items,
    }


def default_output_path() -> Path:
    stamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    return DEFAULT_REPORTS_DIR / f"legend_paper_forward_apply_results_{stamp}.json"


def write_apply_report(report: Dict[str, Any], output_path: Path) -> Path:
    for item in report.get("items", []):
        if not isinstance(item, dict):
            continue
        if str(item.get("apply_status", "")) != "updated":
            continue
        manifest_path = Path(str(item.get("manifest_path", ""))).resolve()
        manifest = _load_json(manifest_path)
        manifest["observed_forward_days"] = int(item.get("observed_forward_days", 0))
        manifest["observed_forward_trades"] = int(item.get("observed_forward_trades", 0))
        manifest["observed_forward_sharpe"] = item.get("observed_forward_sharpe")
        manifest["observed_forward_pf"] = item.get("observed_forward_pf")
        manifest["status"] = str(item.get("status", manifest.get("status", "STAGED")))
        manifest["status_reason"] = str(item.get("status_reason", manifest.get("status_reason", "")))
        manifest_path.write_text(json.dumps(manifest, indent=2, sort_keys=False), encoding="utf-8")

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(report, indent=2, sort_keys=False), encoding="utf-8")
    return output_path


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Apply offline Legend paper-forward executor results onto staged manifests.")
    parser.add_argument("--stage-report", required=True, help="Path to legend_paper_forward_stage_*.json")
    parser.add_argument("--results", required=True, help="Path to executor result batch JSON")
    parser.add_argument(
        "--output",
        help="Optional output path. Defaults to data/reports/mt5/legend_paper_forward_apply_results_<timestamp>.json",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)
    stage_report_path = Path(args.stage_report).resolve()
    results_path = Path(args.results).resolve()
    report = build_apply_report(stage_report_path, results_path)
    output_path = Path(args.output).resolve() if args.output else default_output_path()
    write_apply_report(report, output_path)
    print(output_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
