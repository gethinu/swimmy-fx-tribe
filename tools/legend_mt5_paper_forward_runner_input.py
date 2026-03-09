#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Sequence


REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_REPORTS_DIR = REPO_ROOT / "data" / "reports" / "mt5"
DEFAULT_RUNS_ROOT = DEFAULT_REPORTS_DIR / "legend_paper_forward_runs"


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


def _sanitize_token(text: object) -> str:
    raw = str(text or "").strip().lower()
    normalized = re.sub(r"[^a-z0-9]+", "_", raw).strip("_")
    return normalized or "unknown"


def _paper_run_id(strategy_name: str, shortlist_rank: object) -> str:
    try:
        rank = int(shortlist_rank)
        rank_token = f"r{rank}"
    except (TypeError, ValueError):
        rank_token = "rx"
    return f"legend-pf-{rank_token}-{strategy_name}"


def build_runner_input(queue_report_path: Path) -> Dict[str, Any]:
    payload = _load_json(queue_report_path)
    queue_items = payload.get("items", []) if isinstance(payload, dict) else []
    targets = payload.get("targets", {}) if isinstance(payload, dict) else {}
    bridge_source = str(payload.get("bridge_source", ""))

    items: List[Dict[str, Any]] = []
    for queue_item in queue_items:
        if not isinstance(queue_item, dict):
            continue
        strategy_name = str(queue_item.get("strategy_name", ""))
        shortlist_rank = queue_item.get("shortlist_rank")
        run_id = _paper_run_id(strategy_name, shortlist_rank)
        comment_prefix = f"legend_paper_forward_{_sanitize_token(strategy_name)}"
        items.append(
            {
                "paper_run_id": run_id,
                "strategy_name": strategy_name,
                "shortlist_rank": shortlist_rank,
                "shortlist_role": str(queue_item.get("shortlist_role", "")),
                "symbol": str(queue_item.get("symbol", "")),
                "timeframe": str(queue_item.get("timeframe", "")),
                "execution_mode": "PAPER",
                "paper_comment_prefix": comment_prefix,
                "planned_run_root": str((DEFAULT_RUNS_ROOT / run_id).resolve()),
                "target_forward_days": int(queue_item.get("target_forward_days", targets.get("target_forward_days", 30))),
                "target_forward_trades": int(
                    queue_item.get("target_forward_trades", targets.get("target_forward_trades", 300))
                ),
                "target_forward_sharpe": float(
                    queue_item.get("target_forward_sharpe", targets.get("target_forward_sharpe", 0.70))
                ),
                "target_forward_pf": float(queue_item.get("target_forward_pf", targets.get("target_forward_pf", 1.50))),
                "queue_source": str(queue_report_path),
                "bridge_source": bridge_source or str(queue_item.get("bridge_source", "")),
                "status": "READY_FOR_PAPER_FORWARD",
            }
        )

    items.sort(key=_sort_key)
    return {
        "schema_version": 1,
        "generated_at_utc": _utc_now(),
        "queue_source": str(queue_report_path),
        "bridge_source": bridge_source,
        "items": items,
    }


def default_output_path() -> Path:
    stamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    return DEFAULT_REPORTS_DIR / f"legend_paper_forward_runner_input_{stamp}.json"


def write_runner_input(report: Dict[str, Any], output_path: Path) -> Path:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(report, indent=2, sort_keys=False), encoding="utf-8")
    return output_path


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate runner-ready paper-forward seed input from Legend queue artifacts.")
    parser.add_argument("--queue-report", required=True, help="Path to legend_paper_forward_queue_*.json")
    parser.add_argument(
        "--output",
        help="Optional output path. Defaults to data/reports/mt5/legend_paper_forward_runner_input_<timestamp>.json",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)
    queue_report_path = Path(args.queue_report).resolve()
    report = build_runner_input(queue_report_path)
    output_path = Path(args.output).resolve() if args.output else default_output_path()
    write_runner_input(report, output_path)
    print(output_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
