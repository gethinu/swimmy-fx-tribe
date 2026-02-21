#!/usr/bin/env python3
"""Summarize XAU autobot compare/promotion history for drift monitoring."""

from __future__ import annotations

import argparse
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Optional, Sequence


def _as_float(value: object) -> Optional[float]:
    try:
        if value is None:
            return None
        return float(value)
    except (TypeError, ValueError):
        return None


def _tail(rows: Sequence[Dict[str, object]], lookback: int) -> List[Dict[str, object]]:
    if lookback <= 0:
        return list(rows)
    return list(rows[-lookback:])


def load_jsonl_rows(path: Path) -> List[Dict[str, object]]:
    if not path.exists():
        return []
    rows: List[Dict[str, object]] = []
    with path.open("r", encoding="utf-8") as f:
        for raw in f:
            line = raw.strip()
            if not line:
                continue
            try:
                payload = json.loads(line)
            except json.JSONDecodeError:
                continue
            if isinstance(payload, dict):
                rows.append(payload)
    return rows


def summarize_compare_history(rows: Sequence[Dict[str, object]], *, lookback: int) -> Dict[str, object]:
    window = _tail(rows, lookback)
    runs = len(window)
    skip_runs = 0
    period_rows_total = 0.0
    skip_reason_counts: Dict[str, int] = {}

    for row in window:
        periods = row.get("periods", [])
        if isinstance(periods, list):
            period_rows_total += float(len([item for item in periods if isinstance(item, dict)]))

        action = str(row.get("action", "")).upper()
        if action == "SKIP":
            skip_runs += 1
            reason = str(row.get("reason", "unknown"))
            skip_reason_counts[reason] = skip_reason_counts.get(reason, 0) + 1

    non_skip_runs = max(0, runs - skip_runs)
    avg_period_rows = float(period_rows_total / runs) if runs > 0 else 0.0
    sorted_reasons = dict(sorted(skip_reason_counts.items(), key=lambda kv: kv[1], reverse=True))
    return {
        "runs": runs,
        "skip_runs": skip_runs,
        "non_skip_runs": non_skip_runs,
        "avg_period_rows": avg_period_rows,
        "skip_reason_counts": sorted_reasons,
    }


def summarize_promotion_history(rows: Sequence[Dict[str, object]], *, lookback: int) -> Dict[str, object]:
    window = _tail(rows, lookback)
    runs = len(window)
    blocked_runs = 0
    underperforming_runs = 0
    selected_period_counts: Dict[str, int] = {}
    underperforming_reason_counts: Dict[str, int] = {}
    selected_scores: List[float] = []

    for row in window:
        if bool(row.get("promotion_blocked", False)):
            blocked_runs += 1

        period = str(row.get("selected_period", "")).strip()
        if period:
            selected_period_counts[period] = selected_period_counts.get(period, 0) + 1

        score = _as_float(row.get("selected_score"))
        if score is not None:
            selected_scores.append(score)

        live_gap = row.get("live_gap", {})
        if not isinstance(live_gap, dict):
            continue
        if str(live_gap.get("sample_quality", "")).lower() != "ok":
            continue
        if not bool(live_gap.get("underperforming", False)):
            continue

        underperforming_runs += 1
        reasons = live_gap.get("underperforming_reasons", [])
        if isinstance(reasons, list):
            for reason in reasons:
                text = str(reason).strip()
                if text:
                    underperforming_reason_counts[text] = underperforming_reason_counts.get(text, 0) + 1

    avg_selected_score = float(sum(selected_scores) / len(selected_scores)) if selected_scores else 0.0
    sorted_periods = dict(sorted(selected_period_counts.items(), key=lambda kv: kv[1], reverse=True))
    sorted_reasons = dict(sorted(underperforming_reason_counts.items(), key=lambda kv: kv[1], reverse=True))
    return {
        "runs": runs,
        "blocked_runs": blocked_runs,
        "underperforming_runs": underperforming_runs,
        "avg_selected_score": avg_selected_score,
        "selected_period_counts": sorted_periods,
        "underperforming_reason_counts": sorted_reasons,
    }


def derive_status(
    latest_promotion: Dict[str, object],
    latest_compare: Dict[str, object],
) -> str:
    if not latest_promotion and not latest_compare:
        return "UNKNOWN"

    if bool(latest_promotion.get("promotion_blocked", False)):
        return "ALERT"

    live_gap = latest_promotion.get("live_gap", {})
    if isinstance(live_gap, dict):
        if str(live_gap.get("sample_quality", "")).lower() == "ok" and bool(live_gap.get("underperforming", False)):
            return "ALERT"

    if str(latest_compare.get("action", "")).upper() == "SKIP":
        return "CAUTION"

    return "OK"


def _compact_latest_compare(row: Dict[str, object]) -> Dict[str, object]:
    periods = row.get("periods", [])
    count = len(periods) if isinstance(periods, list) else 0
    out = {
        "generated_at": row.get("generated_at"),
        "action": row.get("action", "RUN"),
        "reason": row.get("reason", ""),
        "period_count": count,
    }
    notify = row.get("notify")
    if isinstance(notify, dict):
        out["notify"] = notify
    return out


def _compact_latest_promotion(row: Dict[str, object]) -> Dict[str, object]:
    out: Dict[str, object] = {
        "selected_period": row.get("selected_period", ""),
        "selected_score": row.get("selected_score", 0.0),
        "promotion_blocked": bool(row.get("promotion_blocked", False)),
    }
    live_gap = row.get("live_gap")
    if isinstance(live_gap, dict):
        out["live_gap"] = live_gap
    notify = row.get("notify")
    if isinstance(notify, dict):
        out["notify"] = notify
    return out


def main() -> None:
    parser = argparse.ArgumentParser(description="Summarize XAU autobot compare/promotion history")
    parser.add_argument(
        "--compare-history",
        default="data/reports/xau_autobot_cycle_comparison_history.jsonl",
    )
    parser.add_argument(
        "--promotion-history",
        default="data/reports/xau_autobot_promotion_history.jsonl",
    )
    parser.add_argument("--lookback", type=int, default=30)
    parser.add_argument(
        "--write-report",
        default="data/reports/xau_autobot_performance_summary.json",
    )
    args = parser.parse_args()

    compare_history_path = Path(args.compare_history)
    promotion_history_path = Path(args.promotion_history)
    compare_rows = load_jsonl_rows(compare_history_path)
    promotion_rows = load_jsonl_rows(promotion_history_path)

    latest_compare = compare_rows[-1] if compare_rows else {}
    latest_promotion = promotion_rows[-1] if promotion_rows else {}

    output = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "lookback": int(args.lookback),
        "status": derive_status(latest_promotion, latest_compare),
        "compare_summary": summarize_compare_history(compare_rows, lookback=int(args.lookback)),
        "promotion_summary": summarize_promotion_history(promotion_rows, lookback=int(args.lookback)),
        "paths": {
            "compare_history": str(compare_history_path),
            "promotion_history": str(promotion_history_path),
        },
    }
    if latest_compare:
        output["latest_compare"] = _compact_latest_compare(latest_compare)
    if latest_promotion:
        output["latest_promotion"] = _compact_latest_promotion(latest_promotion)

    print(json.dumps(output, ensure_ascii=True))

    report_path = Path(args.write_report)
    report_path.parent.mkdir(parents=True, exist_ok=True)
    with report_path.open("w", encoding="utf-8") as f:
        json.dump(output, f, ensure_ascii=True, indent=2)
        f.write("\n")


if __name__ == "__main__":
    main()
