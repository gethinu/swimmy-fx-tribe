#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Sequence


REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_REPORTS_DIR = REPO_ROOT / "data" / "reports" / "mt5"
FORWARD_TARGETS = {
    "target_forward_days": 30,
    "target_forward_trades": 300,
    "target_forward_sharpe": 0.70,
    "target_forward_pf": 1.50,
}


def _load_json(path: Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def _utc_now() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def _sort_key(item: Dict[str, Any]) -> tuple[int, str]:
    rank = item.get("shortlist_rank")
    try:
        normalized_rank = int(rank)
    except (TypeError, ValueError):
        normalized_rank = 999
    return normalized_rank, str(item.get("strategy_name", ""))


def build_paper_forward_queue(bridge_report_path: Path) -> Dict[str, Any]:
    payload = _load_json(bridge_report_path)
    strategies = payload.get("strategies", []) if isinstance(payload, dict) else []
    items: List[Dict[str, Any]] = []
    for strategy in strategies:
        if not isinstance(strategy, dict):
            continue
        alignment = strategy.get("contract_alignment", {})
        if not isinstance(alignment, dict):
            continue
        if not bool(alignment.get("requires_paper_forward")):
            continue
        job = strategy.get("job", {}) if isinstance(strategy.get("job"), dict) else {}
        items.append(
            {
                "strategy_name": str(strategy.get("strategy_name", "")),
                "shortlist_rank": strategy.get("shortlist_rank"),
                "shortlist_role": str(strategy.get("shortlist_role", "")),
                "symbol": str(job.get("symbol", "")),
                "timeframe": str(job.get("period", "")),
                "bridge_source": str(bridge_report_path),
                "queue_reason": str(strategy.get("next_action", "queue_paper_forward_probe")),
                "status": "QUEUED",
                **FORWARD_TARGETS,
            }
        )
    items.sort(key=_sort_key)
    return {
        "schema_version": 1,
        "generated_at_utc": _utc_now(),
        "bridge_source": str(bridge_report_path),
        "targets": dict(FORWARD_TARGETS),
        "items": items,
    }


def default_output_path() -> Path:
    stamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    return DEFAULT_REPORTS_DIR / f"legend_paper_forward_queue_{stamp}.json"


def write_queue_report(report: Dict[str, Any], output_path: Path) -> Path:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(report, indent=2, sort_keys=False), encoding="utf-8")
    return output_path


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate offline paper-forward queue artifacts from Legend MT5 bridge reports.")
    parser.add_argument("--bridge-report", required=True, help="Path to legend_evidence_bridge_*.json")
    parser.add_argument(
        "--output",
        help="Optional output path. Defaults to data/reports/mt5/legend_paper_forward_queue_<timestamp>.json",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)
    bridge_report_path = Path(args.bridge_report).resolve()
    report = build_paper_forward_queue(bridge_report_path)
    output_path = Path(args.output).resolve() if args.output else default_output_path()
    write_queue_report(report, output_path)
    print(output_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
