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


def _manifest_path(item: Dict[str, Any]) -> Path:
    return Path(str(item.get("planned_run_root", ""))).resolve() / "run_manifest.json"


def _manifest_payload(item: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "paper_run_id": str(item.get("paper_run_id", "")),
        "strategy_name": str(item.get("strategy_name", "")),
        "shortlist_rank": item.get("shortlist_rank"),
        "shortlist_role": str(item.get("shortlist_role", "")),
        "symbol": str(item.get("symbol", "")),
        "timeframe": str(item.get("timeframe", "")),
        "execution_mode": "PAPER",
        "paper_comment_prefix": str(item.get("paper_comment_prefix", "")),
        "target_forward_days": int(item.get("target_forward_days", 30)),
        "target_forward_trades": int(item.get("target_forward_trades", 300)),
        "target_forward_sharpe": float(item.get("target_forward_sharpe", 0.70)),
        "target_forward_pf": float(item.get("target_forward_pf", 1.50)),
        "queue_source": str(item.get("queue_source", "")),
        "bridge_source": str(item.get("bridge_source", "")),
        "observed_forward_days": 0,
        "observed_forward_trades": 0,
        "observed_forward_sharpe": None,
        "observed_forward_pf": None,
        "status": "STAGED",
        "status_reason": "awaiting_paper_forward_executor",
    }


def build_stage_report(runner_input_path: Path) -> Dict[str, Any]:
    payload = _load_json(runner_input_path)
    runner_items = payload.get("items", []) if isinstance(payload, dict) else []

    items: List[Dict[str, Any]] = []
    for runner_item in runner_items:
        if not isinstance(runner_item, dict):
            continue
        manifest_path = _manifest_path(runner_item)
        items.append(
            {
                "paper_run_id": str(runner_item.get("paper_run_id", "")),
                "strategy_name": str(runner_item.get("strategy_name", "")),
                "shortlist_rank": runner_item.get("shortlist_rank"),
                "shortlist_role": str(runner_item.get("shortlist_role", "")),
                "symbol": str(runner_item.get("symbol", "")),
                "timeframe": str(runner_item.get("timeframe", "")),
                "execution_mode": "PAPER",
                "paper_comment_prefix": str(runner_item.get("paper_comment_prefix", "")),
                "planned_run_root": str(Path(str(runner_item.get("planned_run_root", ""))).resolve()),
                "manifest_path": str(manifest_path),
                "target_forward_days": int(runner_item.get("target_forward_days", 30)),
                "target_forward_trades": int(runner_item.get("target_forward_trades", 300)),
                "target_forward_sharpe": float(runner_item.get("target_forward_sharpe", 0.70)),
                "target_forward_pf": float(runner_item.get("target_forward_pf", 1.50)),
                "queue_source": str(runner_item.get("queue_source", "")),
                "bridge_source": str(runner_item.get("bridge_source", "")),
                "status": "STAGED",
                "status_reason": "awaiting_paper_forward_executor",
            }
        )

    items.sort(key=_sort_key)
    status_counts = dict(Counter(str(item.get("status", "")) for item in items))
    return {
        "schema_version": 1,
        "generated_at_utc": _utc_now(),
        "runner_input_source": str(runner_input_path),
        "queue_source": str(payload.get("queue_source", "")) if isinstance(payload, dict) else "",
        "bridge_source": str(payload.get("bridge_source", "")) if isinstance(payload, dict) else "",
        "items": items,
        "status_counts": status_counts,
    }


def default_output_path() -> Path:
    stamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    return DEFAULT_REPORTS_DIR / f"legend_paper_forward_stage_{stamp}.json"


def write_stage_report(report: Dict[str, Any], output_path: Path) -> Path:
    for item in report.get("items", []):
        if not isinstance(item, dict):
            continue
        manifest_path = Path(str(item.get("manifest_path", ""))).resolve()
        manifest_path.parent.mkdir(parents=True, exist_ok=True)
        manifest_path.write_text(json.dumps(_manifest_payload(item), indent=2, sort_keys=False), encoding="utf-8")

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(report, indent=2, sort_keys=False), encoding="utf-8")
    return output_path


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Stage Legend paper-forward run manifests from runner-input artifacts.")
    parser.add_argument("--runner-input", required=True, help="Path to legend_paper_forward_runner_input_*.json")
    parser.add_argument(
        "--output",
        help="Optional output path. Defaults to data/reports/mt5/legend_paper_forward_stage_<timestamp>.json",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)
    runner_input_path = Path(args.runner_input).resolve()
    report = build_stage_report(runner_input_path)
    output_path = Path(args.output).resolve() if args.output else default_output_path()
    write_stage_report(report, output_path)
    print(output_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
