#!/usr/bin/env python3
"""Rank conformance audit (A/S floors + A/S/B criteria checks)."""

from __future__ import annotations

import argparse
import json
import sqlite3
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Mapping

A_MIN_TRADES = 50
S_MIN_TRADES = 100

_RANK_ORDER = {
    "GRAVEYARD": 0,
    "RETIRED": 0,
    "INCUBATOR": 1,
    "B": 2,
    "A": 3,
    "S": 4,
    "LEGEND": 5,
}


def _to_float(value: Any, default: float = 0.0) -> float:
    try:
        if value is None:
            return default
        return float(value)
    except (TypeError, ValueError):
        return default


def _to_int(value: Any, default: int = 0) -> int:
    try:
        if value is None:
            return default
        return int(value)
    except (TypeError, ValueError):
        return default


def normalize_rank(value: Any) -> str:
    text = str(value or "").strip().upper()
    if text.startswith(":"):
        text = text[1:]
    if not text or text in {"NIL", "NONE", "NULL"}:
        return "INCUBATOR"
    return text


def _s_staged_pf_wr(trades: int) -> tuple[float, float]:
    if trades >= 150:
        return (1.30, 0.38)
    if trades >= 100:
        return (1.40, 0.42)
    if trades >= 30:
        return (1.55, 0.45)
    return (1.70, 0.50)


def meets_rank_criteria(row: Mapping[str, Any], rank: str) -> bool:
    target = normalize_rank(rank)
    sharpe = _to_float(row.get("sharpe"), 0.0)
    pf = _to_float(row.get("profit_factor"), 0.0)
    wr = _to_float(row.get("win_rate"), 0.0)
    maxdd = _to_float(row.get("max_dd"), 1.0)
    trades = _to_int(row.get("trades"), 0)

    if target == "B":
        return sharpe >= 0.15 and pf >= 1.05 and wr >= 0.35 and maxdd < 0.25
    if target == "A":
        oos = _to_float(row.get("oos_sharpe"), 0.0)
        return (
            trades >= A_MIN_TRADES
            and sharpe >= 0.45
            and pf >= 1.30
            and wr >= 0.43
            and maxdd < 0.16
            and oos >= 0.35
        )
    if target == "S":
        cpcv_pass = _to_float(row.get("cpcv_pass_rate"), 0.0)
        cpcv_maxdd = _to_float(row.get("cpcv_median_maxdd"), 1.0)
        s_pf_min, s_wr_min = _s_staged_pf_wr(trades)
        return (
            trades >= S_MIN_TRADES
            and sharpe >= 0.75
            and pf >= s_pf_min
            and wr >= s_wr_min
            and maxdd < 0.10
            and cpcv_pass >= 0.70
            and cpcv_maxdd < 0.12
        )
    return False


def build_transition_summary(previous_rank_map: Mapping[str, str], current_rank_map: Mapping[str, str]) -> Dict[str, Any]:
    promotion_count = 0
    demotion_count = 0
    changed_count = 0
    new_count = 0
    removed_count = 0
    transitions: Counter[str] = Counter()

    previous_keys = set(previous_rank_map.keys())
    current_keys = set(current_rank_map.keys())
    for name in sorted(previous_keys | current_keys):
        prev = normalize_rank(previous_rank_map.get(name))
        curr = normalize_rank(current_rank_map.get(name))
        if name not in previous_keys:
            new_count += 1
            continue
        if name not in current_keys:
            removed_count += 1
            continue
        if prev == curr:
            continue
        changed_count += 1
        transitions[f"{prev}->{curr}"] += 1
        prev_score = _RANK_ORDER.get(prev)
        curr_score = _RANK_ORDER.get(curr)
        if prev_score is None or curr_score is None:
            continue
        if curr_score > prev_score:
            promotion_count += 1
        elif curr_score < prev_score:
            demotion_count += 1

    return {
        "promotion_count": promotion_count,
        "demotion_count": demotion_count,
        "changed_count": changed_count,
        "new_count": new_count,
        "removed_count": removed_count,
        "by_route": dict(sorted(transitions.items())),
    }


def evaluate_rank_conformance(
    rows: List[Mapping[str, Any]],
    *,
    previous_rank_map: Mapping[str, str],
) -> Dict[str, Any]:
    rank_counts: Counter[str] = Counter()
    current_rank_map: Dict[str, str] = {}

    floor = {
        "A_lt_50": 0,
        "S_lt_100": 0,
        "S_lt_50": 0,
    }
    conformance = {
        "S_fail": 0,
        "S_fail_to_A": 0,
        "S_fail_to_B": 0,
        "A_fail_to_B": 0,
        "B_fail_to_graveyard": 0,
    }

    for row in rows:
        name = str(row.get("name", "")).strip()
        if not name:
            continue
        rank = normalize_rank(row.get("rank"))
        trades = _to_int(row.get("trades"), 0)

        current_rank_map[name] = rank
        rank_counts[rank] += 1

        if rank == "S":
            if trades < S_MIN_TRADES:
                floor["S_lt_100"] += 1
            if trades < A_MIN_TRADES:
                floor["S_lt_50"] += 1
            if not meets_rank_criteria(row, "S"):
                conformance["S_fail"] += 1
                if meets_rank_criteria(row, "A"):
                    conformance["S_fail_to_A"] += 1
                else:
                    conformance["S_fail_to_B"] += 1

        elif rank == "A":
            if trades < A_MIN_TRADES:
                floor["A_lt_50"] += 1
            if not meets_rank_criteria(row, "A"):
                conformance["A_fail_to_B"] += 1

        elif rank == "B":
            if not meets_rank_criteria(row, "B"):
                conformance["B_fail_to_graveyard"] += 1

    transitions = build_transition_summary(previous_rank_map, current_rank_map)
    total_violations = sum(floor.values()) + sum(conformance.values())
    return {
        "counts": dict(sorted(rank_counts.items())),
        "violations": {
            "floor": floor,
            "conformance": conformance,
            "total": total_violations,
        },
        "transitions": transitions,
        "rank_map": current_rank_map,
    }


def _load_strategy_rows(db_path: Path) -> List[Dict[str, Any]]:
    query = """
    SELECT
      name, rank, trades, sharpe, profit_factor, win_rate, max_dd,
      oos_sharpe, cpcv_pass_rate, cpcv_median_maxdd
    FROM strategies
    """
    with sqlite3.connect(str(db_path)) as conn:
        conn.row_factory = sqlite3.Row
        rows = conn.execute(query).fetchall()
    return [dict(row) for row in rows]


def _load_rank_map_from_report(report_path: Path) -> Dict[str, str]:
    if not report_path.exists():
        return {}
    try:
        payload = json.loads(report_path.read_text(encoding="utf-8"))
    except Exception:
        return {}
    if not isinstance(payload, dict):
        return {}
    rank_map = payload.get("rank_map")
    return rank_map if isinstance(rank_map, dict) else {}


def _parse_history_report_ts(path: Path) -> datetime | None:
    name = path.name
    if not name.startswith("rank_conformance_") or not name.endswith(".json"):
        return None
    stamp = name.removeprefix("rank_conformance_").removesuffix(".json")
    try:
        return datetime.strptime(stamp, "%Y%m%d_%H%M%S").replace(tzinfo=timezone.utc)
    except ValueError:
        return None


def _select_previous_day_report(history_dir: Path, now_utc: datetime) -> Path | None:
    if not history_dir.exists():
        return None
    baseline_day = now_utc.date()
    best: tuple[datetime, Path] | None = None
    for path in history_dir.glob("rank_conformance_*.json"):
        ts = _parse_history_report_ts(path)
        if ts is None or ts.date() >= baseline_day:
            continue
        if best is None or ts > best[0]:
            best = (ts, path)
    return best[1] if best else None


def _load_previous_rank_map(
    latest_report_path: Path,
    *,
    history_dir: Path | None = None,
    now_utc: datetime | None = None,
) -> Dict[str, str]:
    anchor_now = now_utc or datetime.now(timezone.utc)
    if history_dir is not None:
        baseline_report = _select_previous_day_report(history_dir, anchor_now)
        if baseline_report is not None:
            baseline_rank_map = _load_rank_map_from_report(baseline_report)
            if baseline_rank_map:
                return baseline_rank_map
    return _load_rank_map_from_report(latest_report_path)


def _save_json(path: Path, payload: Mapping[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    text = json.dumps(payload, ensure_ascii=False, indent=2)
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(text, encoding="utf-8")
    tmp.replace(path)


def run_audit(*, db_path: Path, out_path: Path, history_dir: Path) -> Dict[str, Any]:
    rows = _load_strategy_rows(db_path)
    previous_rank_map = _load_previous_rank_map(out_path, history_dir=history_dir)
    report = evaluate_rank_conformance(rows, previous_rank_map=previous_rank_map)
    report["ran_at"] = datetime.now(timezone.utc).replace(microsecond=0).isoformat()
    report["db_path"] = str(db_path)
    report["strategy_count"] = len(rows)

    _save_json(out_path, report)
    history_dir.mkdir(parents=True, exist_ok=True)
    stamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    _save_json(history_dir / f"rank_conformance_{stamp}.json", report)
    return report


def render_summary_line(report: Mapping[str, Any]) -> str:
    violations = report.get("violations", {}) if isinstance(report, Mapping) else {}
    transitions = report.get("transitions", {}) if isinstance(report, Mapping) else {}
    total = int((violations.get("total", 0) if isinstance(violations, Mapping) else 0) or 0)
    promotions = int((transitions.get("promotion_count", 0) if isinstance(transitions, Mapping) else 0) or 0)
    demotions = int((transitions.get("demotion_count", 0) if isinstance(transitions, Mapping) else 0) or 0)
    changed = int((transitions.get("changed_count", 0) if isinstance(transitions, Mapping) else 0) or 0)
    return (
        "Rank conformance summary: "
        f"violations={total} promotions={promotions} demotions={demotions} changed={changed}"
    )


def main() -> None:
    parser = argparse.ArgumentParser(description="Check rank conformance from Swimmy DB")
    parser.add_argument("--db", default="data/memory/swimmy.db")
    parser.add_argument("--out", default="data/reports/rank_conformance_latest.json")
    parser.add_argument("--history-dir", default="data/reports/rank_conformance")
    parser.add_argument("--fail-on-problem", action="store_true")
    parser.add_argument("--max-violations", type=int, default=0)
    parser.add_argument("--print-json", action="store_true", help="Print full JSON report")
    args = parser.parse_args()

    report = run_audit(
        db_path=Path(args.db),
        out_path=Path(args.out),
        history_dir=Path(args.history_dir),
    )
    if args.print_json:
        print(json.dumps(report, ensure_ascii=False))
    else:
        transitions = report.get("transitions", {}) if isinstance(report, dict) else {}
        compact = {
            "ran_at": report.get("ran_at"),
            "strategy_count": report.get("strategy_count"),
            "violations_total": report.get("violations", {}).get("total"),
            "promotion_count": transitions.get("promotion_count"),
            "demotion_count": transitions.get("demotion_count"),
            "changed_count": transitions.get("changed_count"),
        }
        print(json.dumps(compact, ensure_ascii=False))
    print(render_summary_line(report))
    if args.fail_on_problem:
        max_violations = max(0, int(args.max_violations))
        if int(report["violations"]["total"]) > max_violations:
            raise SystemExit(1)


if __name__ == "__main__":
    main()
