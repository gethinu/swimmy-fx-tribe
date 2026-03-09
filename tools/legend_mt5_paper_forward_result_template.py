#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
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


def build_result_template(stage_report_path: Path) -> Dict[str, Any]:
    stage_report = _load_json(stage_report_path)
    stage_items = stage_report.get("items", []) if isinstance(stage_report, dict) else []

    items: List[Dict[str, Any]] = []
    for stage_item in stage_items:
        if not isinstance(stage_item, dict):
            continue
        items.append(
            {
                "paper_run_id": str(stage_item.get("paper_run_id", "")),
                "strategy_name": str(stage_item.get("strategy_name", "")),
                "shortlist_rank": stage_item.get("shortlist_rank"),
                "manifest_path": str(stage_item.get("manifest_path", "")),
                "planned_run_root": str(stage_item.get("planned_run_root", "")),
                "execution_mode": "PAPER",
                "target_forward_days": int(stage_item.get("target_forward_days", 30)),
                "target_forward_trades": int(stage_item.get("target_forward_trades", 300)),
                "target_forward_sharpe": float(stage_item.get("target_forward_sharpe", 0.70)),
                "target_forward_pf": float(stage_item.get("target_forward_pf", 1.50)),
                "observed_forward_days": 0,
                "observed_forward_trades": 0,
                "observed_forward_sharpe": None,
                "observed_forward_pf": None,
                "status": "STAGED",
                "status_reason": "fill_from_executor",
            }
        )

    items.sort(key=_sort_key)
    return {
        "schema_version": 1,
        "generated_at_utc": _utc_now(),
        "stage_report_source": str(stage_report_path),
        "items": items,
    }


def default_output_path() -> Path:
    stamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    return DEFAULT_REPORTS_DIR / f"legend_paper_forward_result_template_{stamp}.json"


def write_result_template(report: Dict[str, Any], output_path: Path) -> Path:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(report, indent=2, sort_keys=False), encoding="utf-8")
    return output_path


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate executor-fillable result batch templates for Legend paper-forward runs.")
    parser.add_argument("--stage-report", required=True, help="Path to legend_paper_forward_stage_*.json")
    parser.add_argument(
        "--output",
        help="Optional output path. Defaults to data/reports/mt5/legend_paper_forward_result_template_<timestamp>.json",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)
    stage_report_path = Path(args.stage_report).resolve()
    report = build_result_template(stage_report_path)
    output_path = Path(args.output).resolve() if args.output else default_output_path()
    write_result_template(report, output_path)
    print(output_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
