#!/usr/bin/env python3
"""Gate InstitutionalHunter MT5 optimization promotions with monthly return targets."""

from __future__ import annotations

import argparse
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List
import xml.etree.ElementTree as ET


AVG_DAYS_PER_MONTH = 30.4375


def _as_float(value: object, default: float = 0.0) -> float:
    try:
        if value is None:
            return default
        return float(value)
    except (TypeError, ValueError):
        return default


def _build_thresholds(
    *,
    min_monthly_return_pct: float,
    min_forward_trades: int,
    min_forward_pf: float,
    max_forward_dd_pct: float,
    forward_days: int,
) -> Dict[str, object]:
    return {
        "min_monthly_return_pct": float(min_monthly_return_pct),
        "min_forward_trades": int(min_forward_trades),
        "min_forward_pf": float(min_forward_pf),
        "max_forward_dd_pct": float(max_forward_dd_pct),
        "forward_days": int(forward_days),
    }


def compute_monthly_metrics(*, forward_result: float, profit: float, forward_days: int) -> Dict[str, float | bool]:
    if forward_days <= 0:
        return {"valid": False, "reason": "invalid_forward_days"}
    initial_balance = forward_result - profit
    if initial_balance <= 0.0:
        return {"valid": False, "reason": "invalid_initial_balance"}

    total_return = (forward_result / initial_balance) - 1.0
    months = forward_days / AVG_DAYS_PER_MONTH
    monthly_simple = total_return / months if months > 0.0 else 0.0
    monthly_cagr = (forward_result / initial_balance) ** (AVG_DAYS_PER_MONTH / float(forward_days)) - 1.0
    return {
        "valid": True,
        "initial_balance": initial_balance,
        "total_return_pct": total_return * 100.0,
        "monthly_simple_pct": monthly_simple * 100.0,
        "monthly_cagr_pct": monthly_cagr * 100.0,
    }


def load_rows_from_mt5_xml(path: Path) -> List[Dict[str, str]]:
    ns = {"ss": "urn:schemas-microsoft-com:office:spreadsheet"}
    root = ET.parse(path).getroot()
    table = root.find(".//ss:Worksheet/ss:Table", ns)
    if table is None:
        return []
    xml_rows = table.findall("ss:Row", ns)
    if not xml_rows:
        return []

    index_attr = "{urn:schemas-microsoft-com:office:spreadsheet}Index"

    def _row_values(row: ET.Element) -> List[str]:
        out: List[str] = []
        for cell in row.findall("ss:Cell", ns):
            idx_raw = cell.attrib.get(index_attr)
            if idx_raw:
                idx = int(_as_float(idx_raw, 0.0))
                if idx > 0 and idx > (len(out) + 1):
                    out.extend([""] * (idx - len(out) - 1))
            data = cell.find("ss:Data", ns)
            out.append((data.text or "") if data is not None else "")
        return out

    header = [x.strip() for x in _row_values(xml_rows[0])]
    if not header:
        return []

    rows: List[Dict[str, str]] = []
    for raw in xml_rows[1:]:
        vals = _row_values(raw)
        if not vals:
            continue
        if len(vals) < len(header):
            vals.extend([""] * (len(header) - len(vals)))
        row = {header[i]: str(vals[i]).strip() for i in range(len(header))}
        if not any(str(v).strip() for v in row.values()):
            continue
        rows.append(row)
    return rows


def _extract_selected_parameters(row: Dict[str, object]) -> Dict[str, str]:
    out: Dict[str, str] = {}
    for key, value in row.items():
        if str(key).startswith("Inp"):
            out[str(key)] = str(value)
    return out


def _candidate_metrics(
    row: Dict[str, object],
    *,
    forward_days: int,
) -> Dict[str, object]:
    forward_result = _as_float(row.get("Forward Result"), 0.0)
    profit = _as_float(row.get("Profit"), 0.0)
    profit_factor = _as_float(row.get("Profit Factor"), 0.0)
    dd_pct = _as_float(row.get("Equity DD %"), 0.0)
    trades = int(_as_float(row.get("Trades"), 0.0))
    monthly = compute_monthly_metrics(
        forward_result=forward_result,
        profit=profit,
        forward_days=forward_days,
    )
    out: Dict[str, object] = {
        "pass": str(row.get("Pass", "")),
        "forward_result": forward_result,
        "profit": profit,
        "profit_factor": profit_factor,
        "equity_dd_pct": dd_pct,
        "trades": trades,
        "monthly_valid": bool(monthly.get("valid", False)),
        "monthly_reason": str(monthly.get("reason", "")),
        "initial_balance": _as_float(monthly.get("initial_balance"), 0.0),
        "total_return_pct": _as_float(monthly.get("total_return_pct"), 0.0),
        "monthly_simple_pct": _as_float(monthly.get("monthly_simple_pct"), 0.0),
        "monthly_cagr_pct": _as_float(monthly.get("monthly_cagr_pct"), 0.0),
        "selected_parameters": _extract_selected_parameters(row),
    }
    return out


def _format_metrics(candidate: Dict[str, object]) -> Dict[str, object]:
    return {
        "forward_result": _as_float(candidate.get("forward_result"), 0.0),
        "profit": _as_float(candidate.get("profit"), 0.0),
        "profit_factor": _as_float(candidate.get("profit_factor"), 0.0),
        "equity_dd_pct": _as_float(candidate.get("equity_dd_pct"), 0.0),
        "trades": int(_as_float(candidate.get("trades"), 0.0)),
        "initial_balance": _as_float(candidate.get("initial_balance"), 0.0),
        "total_return_pct": _as_float(candidate.get("total_return_pct"), 0.0),
        "monthly_simple_pct": _as_float(candidate.get("monthly_simple_pct"), 0.0),
        "monthly_cagr_pct": _as_float(candidate.get("monthly_cagr_pct"), 0.0),
    }


def evaluate_promotion_from_rows(
    rows: List[Dict[str, object]],
    *,
    forward_days: int = 365,
    min_monthly_return_pct: float = 10.0,
    min_forward_trades: int = 5,
    min_forward_pf: float = 1.0,
    max_forward_dd_pct: float = 10.0,
) -> Dict[str, object]:
    thresholds = _build_thresholds(
        min_monthly_return_pct=min_monthly_return_pct,
        min_forward_trades=min_forward_trades,
        min_forward_pf=min_forward_pf,
        max_forward_dd_pct=max_forward_dd_pct,
        forward_days=forward_days,
    )
    if not rows:
        return {
            "generated_at_utc": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
            "accept_for_promotion": False,
            "reasons": ["forward_rows_missing"],
            "thresholds": thresholds,
            "selected_pass": "",
            "selected_parameters": {},
            "metrics": {
                "forward_result": 0.0,
                "profit": 0.0,
                "profit_factor": 0.0,
                "equity_dd_pct": 0.0,
                "trades": 0,
                "initial_balance": 0.0,
                "total_return_pct": 0.0,
                "monthly_simple_pct": 0.0,
                "monthly_cagr_pct": 0.0,
            },
            "candidates": {"total": 0, "eligible": 0},
        }

    all_candidates: List[Dict[str, object]] = []
    eligible: List[Dict[str, object]] = []
    for row in rows:
        cand = _candidate_metrics(row, forward_days=forward_days)
        reject_reasons: List[str] = []
        if not bool(cand.get("monthly_valid", False)):
            reject_reasons.append(str(cand.get("monthly_reason", "monthly_metrics_invalid")))
        else:
            if _as_float(cand.get("monthly_cagr_pct"), 0.0) < float(min_monthly_return_pct):
                reject_reasons.append("monthly_return_below_target")
        if int(_as_float(cand.get("trades"), 0.0)) < int(min_forward_trades):
            reject_reasons.append("trades_below_min")
        if _as_float(cand.get("profit_factor"), 0.0) < float(min_forward_pf):
            reject_reasons.append("profit_factor_below_min")
        if _as_float(cand.get("equity_dd_pct"), 0.0) > float(max_forward_dd_pct):
            reject_reasons.append("drawdown_above_max")
        cand["rejections"] = reject_reasons
        all_candidates.append(cand)
        if not reject_reasons:
            eligible.append(cand)

    def _sort_key(c: Dict[str, object]) -> tuple[float, float, float, int]:
        return (
            _as_float(c.get("monthly_cagr_pct"), 0.0),
            _as_float(c.get("profit_factor"), 0.0),
            -_as_float(c.get("equity_dd_pct"), 0.0),
            int(_as_float(c.get("trades"), 0.0)),
        )

    best_any = max(all_candidates, key=_sort_key)
    if not eligible:
        details: List[str] = []
        for c in all_candidates:
            for r in c.get("rejections", []):
                s = str(r)
                if s and s not in details:
                    details.append(s)
        reasons = ["no_candidate_meets_thresholds"] + details
        return {
            "generated_at_utc": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
            "accept_for_promotion": False,
            "reasons": reasons,
            "thresholds": thresholds,
            "selected_pass": str(best_any.get("pass", "")),
            "selected_parameters": best_any.get("selected_parameters", {}),
            "metrics": _format_metrics(best_any),
            "candidates": {"total": len(all_candidates), "eligible": 0},
        }

    selected = max(eligible, key=_sort_key)
    return {
        "generated_at_utc": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "accept_for_promotion": True,
        "reasons": [],
        "thresholds": thresholds,
        "selected_pass": str(selected.get("pass", "")),
        "selected_parameters": selected.get("selected_parameters", {}),
        "metrics": _format_metrics(selected),
        "candidates": {"total": len(all_candidates), "eligible": len(eligible)},
    }


def apply_selected_parameters_to_set(set_path: Path, selected_parameters: Dict[str, object]) -> None:
    lines = set_path.read_text(encoding="utf-8").splitlines()
    out: List[str] = []
    for raw in lines:
        line = raw
        if "=" in raw and not raw.startswith("#"):
            key, value = raw.split("=", 1)
            key = key.strip()
            if key in selected_parameters:
                new_value = str(selected_parameters[key])
                if "||" in value:
                    _, suffix = value.split("||", 1)
                    line = f"{key}={new_value}||{suffix}"
                else:
                    line = f"{key}={new_value}"
        out.append(line)
    set_path.write_text("\n".join(out) + "\n", encoding="utf-8")


def evaluate_promotion(
    summary: Dict[str, object],
    *,
    forward_days: int = 365,
    min_monthly_return_pct: float = 10.0,
    min_forward_trades: int = 5,
    min_forward_pf: float = 1.0,
    max_forward_dd_pct: float = 10.0,
) -> Dict[str, object]:
    run_id = str(summary.get("run_id", "")).strip()
    forward = summary.get("forward") if isinstance(summary.get("forward"), dict) else {}
    top = forward.get("top") if isinstance(forward.get("top"), dict) else {}
    base = evaluate_promotion_from_rows(
        [top] if top else [],
        forward_days=forward_days,
        min_monthly_return_pct=min_monthly_return_pct,
        min_forward_trades=min_forward_trades,
        min_forward_pf=min_forward_pf,
        max_forward_dd_pct=max_forward_dd_pct,
    )
    base["run_id"] = run_id
    if not top and base.get("reasons") == ["forward_rows_missing"]:
        base["reasons"] = ["forward_top_missing"]
    return base


def _default_decision_path(summary_report: Path) -> Path:
    name = summary_report.name
    if name.endswith(".summary.json"):
        return summary_report.with_name(name.replace(".summary.json", ".decision.json"))
    if name.endswith(".xml"):
        return summary_report.with_name(name.replace(".xml", ".decision.json"))
    return summary_report.with_suffix(".decision.json")


def main() -> int:
    parser = argparse.ArgumentParser(description="InstitutionalHunter MT5 promotion gate with monthly return target")
    parser.add_argument("--summary-report", default="", help="Path to MT5 optimization summary JSON")
    parser.add_argument("--forward-xml", default="", help="Path to MT5 forward optimization XML report")
    parser.add_argument("--write-decision", default="", help="Output decision JSON path")
    parser.add_argument("--apply-to-set", default="", help="Apply selected parameters to this .set file on accept")
    parser.add_argument("--forward-days", type=int, default=365)
    parser.add_argument("--min-monthly-return-pct", type=float, default=10.0)
    parser.add_argument("--min-forward-trades", type=int, default=5)
    parser.add_argument("--min-forward-pf", type=float, default=1.0)
    parser.add_argument("--max-forward-dd-pct", type=float, default=10.0)
    args = parser.parse_args()

    summary_report = str(args.summary_report).strip()
    forward_xml = str(args.forward_xml).strip()
    if bool(summary_report) == bool(forward_xml):
        raise ValueError("Specify exactly one of --summary-report or --forward-xml")

    if summary_report:
        summary_path = Path(summary_report)
        with summary_path.open("r", encoding="utf-8") as f:
            summary = json.load(f)
        if not isinstance(summary, dict):
            raise ValueError("summary report must be a JSON object")
        out = evaluate_promotion(
            summary,
            forward_days=int(args.forward_days),
            min_monthly_return_pct=float(args.min_monthly_return_pct),
            min_forward_trades=int(args.min_forward_trades),
            min_forward_pf=float(args.min_forward_pf),
            max_forward_dd_pct=float(args.max_forward_dd_pct),
        )
        out["summary_report"] = str(summary_path.resolve())
        decision_input_path = summary_path
    else:
        xml_path = Path(forward_xml)
        rows = load_rows_from_mt5_xml(xml_path)
        out = evaluate_promotion_from_rows(
            rows,
            forward_days=int(args.forward_days),
            min_monthly_return_pct=float(args.min_monthly_return_pct),
            min_forward_trades=int(args.min_forward_trades),
            min_forward_pf=float(args.min_forward_pf),
            max_forward_dd_pct=float(args.max_forward_dd_pct),
        )
        out["run_id"] = xml_path.stem
        out["forward_xml"] = str(xml_path.resolve())
        decision_input_path = xml_path

    apply_set = str(args.apply_to_set).strip()
    if apply_set and bool(out.get("accept_for_promotion", False)):
        selected_params = out.get("selected_parameters")
        if isinstance(selected_params, dict) and selected_params:
            set_path = Path(apply_set)
            apply_selected_parameters_to_set(set_path, selected_params)
            out["applied_to_set"] = str(set_path.resolve())

    decision_path = Path(args.write_decision) if str(args.write_decision).strip() else _default_decision_path(decision_input_path)
    decision_path.parent.mkdir(parents=True, exist_ok=True)
    with decision_path.open("w", encoding="utf-8") as f:
        json.dump(out, f, ensure_ascii=False, indent=2)
        f.write("\n")

    print(json.dumps(out, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
