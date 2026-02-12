#!/usr/bin/env python3
"""Summarize OpenClaw + Polymarket paper-trade journal."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any, Dict, List, Mapping, Optional, Sequence


def load_journal_records(path: Path) -> List[Dict[str, Any]]:
    records: List[Dict[str, Any]] = []
    if not path.exists():
        return records
    with path.open("r", encoding="utf-8") as handle:
        for raw in handle:
            line = raw.strip()
            if not line:
                continue
            try:
                payload = json.loads(line)
            except json.JSONDecodeError:
                continue
            if isinstance(payload, dict):
                records.append(payload)
    return records


def summarize_journal(records: Sequence[Mapping[str, Any]], target_date: Optional[str] = None) -> Dict[str, Any]:
    run_ids = set()
    entry_count = 0
    total_stake = 0.0
    total_expected = 0.0
    edge_sum = 0.0
    edge_n = 0
    side_counts: Dict[str, int] = {"YES": 0, "NO": 0}

    for record in records:
        date_text = str(record.get("date", ""))
        if target_date and date_text != target_date:
            continue

        row_type = str(record.get("type", ""))
        run_id = str(record.get("run_id", ""))
        if row_type == "run_summary" and run_id:
            run_ids.add(run_id)
        if row_type != "entry":
            continue

        entry_count += 1
        if run_id:
            run_ids.add(run_id)

        stake = float(record.get("stake_usd", 0.0) or 0.0)
        expected = float(record.get("expected_value_usd", 0.0) or 0.0)
        edge = float(record.get("edge", 0.0) or 0.0)
        side = str(record.get("side", "")).upper()

        total_stake += stake
        total_expected += expected
        edge_sum += edge
        edge_n += 1
        if side in side_counts:
            side_counts[side] += 1
        else:
            side_counts[side] = side_counts.get(side, 0) + 1

    avg_edge = (edge_sum / edge_n) if edge_n > 0 else 0.0
    expected_return_on_stake = (total_expected / total_stake) if total_stake > 0.0 else 0.0

    return {
        "date": target_date or "ALL",
        "runs": len(run_ids),
        "entries": entry_count,
        "total_stake_usd": round(total_stake, 4),
        "total_expected_value_usd": round(total_expected, 4),
        "avg_edge": round(avg_edge, 6),
        "expected_return_on_stake": round(expected_return_on_stake, 6),
        "side_counts": side_counts,
    }


def _normalize_outcome(value: Any) -> Optional[str]:
    text = str(value or "").strip().upper()
    if text in {"YES", "Y", "TRUE", "1"}:
        return "YES"
    if text in {"NO", "N", "FALSE", "0"}:
        return "NO"
    return None


def load_settlements(path: Path) -> Dict[str, str]:
    if not path.exists():
        return {}
    payload = json.loads(path.read_text(encoding="utf-8"))
    out: Dict[str, str] = {}
    if isinstance(payload, dict):
        for market_id, outcome in payload.items():
            normalized = _normalize_outcome(outcome)
            if normalized:
                out[str(market_id)] = normalized
        return out
    if isinstance(payload, list):
        for item in payload:
            if not isinstance(item, dict):
                continue
            market_id = str(item.get("market_id") or item.get("id") or "").strip()
            outcome = item.get("winner", item.get("outcome", item.get("result")))
            normalized = _normalize_outcome(outcome)
            if market_id and normalized:
                out[market_id] = normalized
    return out


def summarize_realized(
    records: Sequence[Mapping[str, Any]],
    settlements: Mapping[str, str],
    *,
    target_date: Optional[str] = None,
    fee_bps_per_side: float = 0.0,
    slippage_bps_per_side: float = 0.0,
) -> Dict[str, Any]:
    cost_rate = max(0.0, 2.0 * (float(fee_bps_per_side) + float(slippage_bps_per_side)) / 10000.0)
    resolved_entries = 0
    unresolved_entries = 0
    resolved_stake = 0.0
    expected_total = 0.0
    realized_total = 0.0
    wins = 0
    model_prob_sum = 0.0
    model_prob_n = 0
    brier_sum = 0.0
    brier_n = 0

    for record in records:
        if str(record.get("type", "")) != "entry":
            continue
        date_text = str(record.get("date", ""))
        if target_date and date_text != target_date:
            continue

        market_id = str(record.get("market_id", "")).strip()
        side = _normalize_outcome(record.get("side"))
        winner = _normalize_outcome(settlements.get(market_id)) if market_id else None
        stake = float(record.get("stake_usd", 0.0) or 0.0)
        entry_price = float(record.get("entry_price", 0.0) or 0.0)
        expected = float(record.get("expected_value_usd", 0.0) or 0.0)
        model_prob = float(record.get("model_prob", 0.0) or 0.0)

        if not market_id or side is None or winner is None or stake <= 0.0 or entry_price <= 0.0 or entry_price >= 1.0:
            unresolved_entries += 1
            continue

        resolved_entries += 1
        resolved_stake += stake
        expected_total += expected

        is_win = side == winner
        gross_pnl = (stake * ((1.0 / entry_price) - 1.0)) if is_win else -stake
        net_pnl = gross_pnl - (stake * cost_rate)
        realized_total += net_pnl
        if net_pnl > 0.0:
            wins += 1
        if 0.0 < model_prob < 1.0:
            outcome = 1.0 if is_win else 0.0
            model_prob_sum += model_prob
            model_prob_n += 1
            brier_sum += (model_prob - outcome) ** 2
            brier_n += 1

    realized_win_rate = (wins / resolved_entries) if resolved_entries > 0 else 0.0
    realized_return_on_stake = (realized_total / resolved_stake) if resolved_stake > 0.0 else 0.0
    mean_model_win_prob = (model_prob_sum / model_prob_n) if model_prob_n > 0 else 0.0
    brier_score = (brier_sum / brier_n) if brier_n > 0 else 0.0
    return {
        "date": target_date or "ALL",
        "resolved_entries": resolved_entries,
        "unresolved_entries": unresolved_entries,
        "resolved_stake_usd": round(resolved_stake, 4),
        "expected_value_usd": round(expected_total, 4),
        "realized_pnl_usd": round(realized_total, 4),
        "realized_win_rate": round(realized_win_rate, 6),
        "realized_return_on_stake": round(realized_return_on_stake, 6),
        "realized_minus_expected_usd": round(realized_total - expected_total, 4),
        "mean_model_win_prob": round(mean_model_win_prob, 6),
        "brier_score": round(brier_score, 6),
    }


def main() -> None:
    parser = argparse.ArgumentParser(description="Summarize paper-trade journal for polymarket_openclaw_bot")
    parser.add_argument("--journal-file", required=True, help="JSONL path generated by --journal-file option")
    parser.add_argument("--date", default="", help="Optional YYYY-MM-DD filter")
    parser.add_argument("--settlements-file", default="", help="Optional JSON file of resolved outcomes by market_id")
    parser.add_argument("--fee-bps-per-side", type=float, default=0.0)
    parser.add_argument("--slippage-bps-per-side", type=float, default=0.0)
    parser.add_argument("--write-report", default="", help="Optional output JSON path")
    args = parser.parse_args()

    path = Path(args.journal_file)
    records = load_journal_records(path)
    date_filter = args.date.strip() or None
    summary = summarize_journal(records, target_date=date_filter)
    output: Dict[str, Any] = dict(summary)
    if args.settlements_file:
        settlements = load_settlements(Path(args.settlements_file))
        output["realized"] = summarize_realized(
            records,
            settlements,
            target_date=date_filter,
            fee_bps_per_side=args.fee_bps_per_side,
            slippage_bps_per_side=args.slippage_bps_per_side,
        )
    text = json.dumps(output, ensure_ascii=False, indent=2)
    print(text)
    if args.write_report:
        Path(args.write_report).write_text(text + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
