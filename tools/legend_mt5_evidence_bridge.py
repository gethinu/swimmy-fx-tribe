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
SHORTLIST_ORDER = [
    "legend-macd-above-zero-cross",
    "historical-s-bred940-trend-core",
    "legend-pullback-breakout",
]
SHORTLIST_META: Dict[str, Dict[str, Any]] = {
    "legend-macd-above-zero-cross": {
        "shortlist_rank": 1,
        "shortlist_role": "primary_stability_first",
        "preferred_retune_profile": "wide",
    },
    "historical-s-bred940-trend-core": {
        "shortlist_rank": 2,
        "shortlist_role": "second_adaptive_contender",
        "preferred_retune_profile": "medium",
    },
    "legend-pullback-breakout": {
        "shortlist_rank": 3,
        "shortlist_role": "third_higher_turnover_line",
        "preferred_retune_profile": "wide",
    },
}
PROXY_REASON_CODES = [
    "mt5_inventory_not_canonical_rank_evidence",
    "mt5_walkforward_not_canonical_paper_forward",
]


def _load_json(path: Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def _as_float(value: object, default: float = 0.0) -> float:
    if value is None:
        return default
    if isinstance(value, (int, float)):
        return float(value)
    text = str(value).strip()
    if not text:
        return default
    cleaned = text.replace(",", "").replace(" ", "")
    match = re.search(r"-?\d+(?:\.\d+)?", cleaned)
    if not match:
        return default
    try:
        return float(match.group(0))
    except ValueError:
        return default


def _as_int(value: object, default: int = 0) -> int:
    return int(round(_as_float(value, float(default))))


def _utc_now() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def load_inventory_entries(manifest_paths: Sequence[Path]) -> Dict[str, List[Dict[str, Any]]]:
    by_slug: Dict[str, List[Dict[str, Any]]] = {}
    for manifest_path in manifest_paths:
        payload = _load_json(manifest_path)
        if not isinstance(payload, list):
            raise ValueError(f"inventory manifest must be a JSON array: {manifest_path}")
        for entry in payload:
            job = entry.get("job", {}) if isinstance(entry, dict) else {}
            slug = str(job.get("slug", "")).strip()
            if not slug:
                continue
            summary = entry.get("summary", {}) if isinstance(entry, dict) else {}
            by_slug.setdefault(slug, []).append(
                {
                    "source_path": str(manifest_path),
                    "run_id": str(entry.get("run_id", "")),
                    "from_date": str(entry.get("from_date", "")),
                    "to_date": str(entry.get("to_date", "")),
                    "job": job,
                    "summary": {
                        "profit_factor": _as_float(summary.get("profit_factor")),
                        "sharpe_ratio": _as_float(summary.get("sharpe_ratio")),
                        "total_trades": _as_int(summary.get("total_trades")),
                        "total_net_profit": _as_float(summary.get("total_net_profit")),
                    },
                }
            )
    return by_slug


def discover_walkforward_summaries(walkforward_roots: Sequence[Path]) -> Dict[str, List[Dict[str, Any]]]:
    by_slug: Dict[str, List[Dict[str, Any]]] = {}
    for root in walkforward_roots:
        for summary_path in sorted(root.glob("*/walkforward_summary.json")):
            payload = _load_json(summary_path)
            slug = str(payload.get("job") or summary_path.parent.name).strip()
            if not slug:
                continue
            aggregate = payload.get("aggregate", {}) if isinstance(payload, dict) else {}
            by_slug.setdefault(slug, []).append(
                {
                    "strategy_name": slug,
                    "source_path": str(summary_path),
                    "source_root": str(root),
                    "retune_profile": str(payload.get("retune_profile") or "narrow"),
                    "folds_total": _as_int(payload.get("folds_total")),
                    "folds_passing": _as_int(payload.get("folds_passing")),
                    "aggregate": {
                        "forward_profit_total": _as_float(aggregate.get("forward_profit_total")),
                        "forward_profit_median": _as_float(aggregate.get("forward_profit_median")),
                        "forward_pf_mean": _as_float(aggregate.get("forward_pf_mean")),
                        "forward_sharpe_mean": _as_float(aggregate.get("forward_sharpe_mean")),
                        "forward_trades_total": _as_int(aggregate.get("forward_trades_total")),
                        "aggregate_mode": str(aggregate.get("aggregate_mode", "")),
                    },
                }
            )
    return by_slug


def _inventory_sort_key(entry: Dict[str, Any]) -> tuple[str, str, str, str]:
    return (
        str(entry.get("run_id", "")),
        str(entry.get("from_date", "")),
        str(entry.get("to_date", "")),
        str(entry.get("source_path", "")),
    )


def summarize_inventory(records: Sequence[Dict[str, Any]]) -> Dict[str, Any] | None:
    if not records:
        return None
    latest = max(records, key=_inventory_sort_key)
    positive_windows = sum(1 for record in records if _as_float(record["summary"].get("total_net_profit")) > 0.0)
    return {
        "windows_total": len(records),
        "positive_windows": positive_windows,
        "latest_run_id": str(latest.get("run_id", "")),
        "latest_window": {
            "from_date": str(latest.get("from_date", "")),
            "to_date": str(latest.get("to_date", "")),
        },
        "latest_summary": latest.get("summary", {}),
        "source_path": str(latest.get("source_path", "")),
    }


def choose_canonical_walkforward(strategy_name: str, candidates: Sequence[Dict[str, Any]]) -> Dict[str, Any] | None:
    if not candidates:
        return None
    preferred = SHORTLIST_META.get(strategy_name, {}).get("preferred_retune_profile")

    def _sort_key(candidate: Dict[str, Any]) -> tuple[int, float, int, float, float, float, str]:
        folds_total = max(1, _as_int(candidate.get("folds_total"), 1))
        folds_passing = _as_int(candidate.get("folds_passing"))
        aggregate = candidate.get("aggregate", {})
        return (
            1 if preferred and candidate.get("retune_profile") == preferred else 0,
            folds_passing / folds_total,
            folds_passing,
            _as_float(aggregate.get("forward_pf_mean")),
            _as_float(aggregate.get("forward_sharpe_mean")),
            _as_float(aggregate.get("forward_profit_total")),
            str(candidate.get("source_path", "")),
        )

    selected = max(candidates, key=_sort_key)
    return {
        "retune_profile": str(selected.get("retune_profile", "")),
        "folds_total": _as_int(selected.get("folds_total")),
        "folds_passing": _as_int(selected.get("folds_passing")),
        "aggregate": selected.get("aggregate", {}),
        "source_path": str(selected.get("source_path", "")),
        "source_root": str(selected.get("source_root", "")),
    }


def _shortlist_sort_key(strategy_name: str) -> tuple[int, str]:
    meta = SHORTLIST_META.get(strategy_name, {})
    rank = meta.get("shortlist_rank")
    return (int(rank) if isinstance(rank, int) else 999, strategy_name)


def build_bridge_report(
    *,
    inventory_manifests: Sequence[Path],
    walkforward_roots: Sequence[Path],
) -> Dict[str, Any]:
    inventory_by_slug = load_inventory_entries(inventory_manifests)
    walkforward_by_slug = discover_walkforward_summaries(walkforward_roots)
    all_slugs = [
        slug
        for slug in SHORTLIST_ORDER
        if slug in inventory_by_slug or slug in walkforward_by_slug
    ]

    strategies: List[Dict[str, Any]] = []
    for slug in all_slugs:
        meta = SHORTLIST_META.get(slug, {})
        inventory_summary = summarize_inventory(inventory_by_slug.get(slug, []))
        canonical_walkforward = choose_canonical_walkforward(slug, walkforward_by_slug.get(slug, []))
        strategy_entry = {
            "strategy_name": slug,
            "shortlist_rank": meta.get("shortlist_rank"),
            "shortlist_role": meta.get("shortlist_role"),
            "job": (inventory_by_slug.get(slug, [{}])[-1].get("job", {}) if inventory_by_slug.get(slug) else {}),
            "inventory_validation": inventory_summary,
            "canonical_walkforward": canonical_walkforward,
            "contract_alignment": {
                "requires_paper_forward": True,
                "rank_evidence_satisfied": False,
                "deployment_gate_ready": False,
                "reason_codes": list(PROXY_REASON_CODES),
            },
            "next_action": "queue_paper_forward_probe",
        }
        strategies.append(strategy_entry)

    shortlist = [slug for slug in SHORTLIST_ORDER if slug in all_slugs]
    return {
        "schema_version": 1,
        "generated_at_utc": _utc_now(),
        "contract": {
            "type": "legend_mt5_offline_bridge",
            "writes_to_db": False,
            "rank_evidence_semantics": "mt5_inventory_validation_only",
            "forward_evidence_semantics": "mt5_walkforward_proxy_only",
            "requires_paper_forward": True,
        },
        "inputs": {
            "inventory_manifests": [str(path) for path in inventory_manifests],
            "walkforward_roots": [str(path) for path in walkforward_roots],
            "inventory_strategy_count": len(inventory_by_slug),
            "walkforward_strategy_count": len(walkforward_by_slug),
        },
        "shortlist": shortlist,
        "strategies": strategies,
    }


def default_output_path() -> Path:
    stamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    return DEFAULT_REPORTS_DIR / f"legend_evidence_bridge_{stamp}.json"


def write_bridge_report(report: Dict[str, Any], output_path: Path) -> Path:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(report, indent=2, sort_keys=False), encoding="utf-8")
    return output_path


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build a contract-aware offline bridge from Legend MT5 artifacts.")
    parser.add_argument(
        "--inventory-manifest",
        action="append",
        default=[],
        help="Path to inventory_tester_manifest.json (repeatable).",
    )
    parser.add_argument(
        "--walkforward-root",
        action="append",
        default=[],
        help="Path to a legend_walkforward run root (repeatable).",
    )
    parser.add_argument(
        "--output",
        help="Optional output path. Defaults to data/reports/mt5/legend_evidence_bridge_<timestamp>.json",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)
    inventory_manifests = [Path(path).resolve() for path in args.inventory_manifest]
    walkforward_roots = [Path(path).resolve() for path in args.walkforward_root]
    if not inventory_manifests:
        raise SystemExit("--inventory-manifest is required at least once")
    if not walkforward_roots:
        raise SystemExit("--walkforward-root is required at least once")
    report = build_bridge_report(
        inventory_manifests=inventory_manifests,
        walkforward_roots=walkforward_roots,
    )
    output_path = Path(args.output).resolve() if args.output else default_output_path()
    write_bridge_report(report, output_path)
    print(output_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
