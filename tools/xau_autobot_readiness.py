#!/usr/bin/env python3
"""Readiness report for xau_autobot config using offline intraday backtest."""

from __future__ import annotations

import argparse
import json
import os
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Sequence, Tuple

try:
    from tools.xau_autobot_data import load_ohlc
except Exception:
    from xau_autobot_data import load_ohlc  # type: ignore

DEFAULT_RUNTIME_JOURNAL_PATH = "data/reports/xau_autobot_runtime_journal_latest.jsonl"
REPO_ROOT = Path(__file__).resolve().parent.parent
RUNTIME_METRICS_SCHEMA_VERSION = 2


def resolve_runtime_journal_path(cli_value: str) -> Path:
    text = str(cli_value or "").strip()
    if text:
        path = Path(text).expanduser()
    else:
        raw = os.getenv("XAU_AUTOBOT_RUNTIME_JOURNAL_PATH", "").strip()
        candidate = raw or DEFAULT_RUNTIME_JOURNAL_PATH
        path = Path(candidate).expanduser()
    if not path.is_absolute():
        path = REPO_ROOT / path
    return path


def append_runtime_journal(path: Path, payload: Dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as f:
        f.write(json.dumps(payload, ensure_ascii=True))
        f.write("\n")


def _new_runtime_metrics() -> Dict[str, object]:
    return {
        "signal_eval_count": 0,
        "gap_reject_count": 0,
        "spread_reject_count": 0,
        "session_reject_count": 0,
        "maxpos_reject_count": 0,
        "signal_counts": {"BUY": 0, "SELL": 0, "HOLD": 0},
    }


def _runtime_metrics_snapshot(metrics: Dict[str, object]) -> Dict[str, object]:
    signal_eval_count = max(0, int(metrics.get("signal_eval_count", 0)))
    gap_reject_count = max(0, int(metrics.get("gap_reject_count", 0)))
    spread_reject_count = max(0, int(metrics.get("spread_reject_count", 0)))
    session_reject_count = max(0, int(metrics.get("session_reject_count", 0)))
    maxpos_reject_count = max(0, int(metrics.get("maxpos_reject_count", 0)))
    signal_counts_raw = metrics.get("signal_counts", {})
    signal_counts = signal_counts_raw if isinstance(signal_counts_raw, dict) else {}
    out_signal_counts = {
        "BUY": max(0, int(signal_counts.get("BUY", 0))),
        "SELL": max(0, int(signal_counts.get("SELL", 0))),
        "HOLD": max(0, int(signal_counts.get("HOLD", 0))),
    }
    gap_reject_rate = (
        float(gap_reject_count) / float(signal_eval_count)
        if signal_eval_count > 0
        else 0.0
    )
    return {
        "signal_eval_count": signal_eval_count,
        "gap_reject_count": gap_reject_count,
        "spread_reject_count": spread_reject_count,
        "session_reject_count": session_reject_count,
        "maxpos_reject_count": maxpos_reject_count,
        "gap_reject_rate": gap_reject_rate,
        "gate_check_count": signal_eval_count,
        "gate_reject_gap_count": gap_reject_count,
        "signal_counts": out_signal_counts,
        "schema_version": RUNTIME_METRICS_SCHEMA_VERSION,
    }

def total_return_from_gross(gross_returns: Sequence[float], cost_per_side: float) -> float:
    equity = 1.0
    for gross in gross_returns:
        net = gross - (2.0 * cost_per_side)
        equity *= 1.0 + net
    return equity - 1.0


def estimate_break_even_cost(
    total_return_fn,
    *,
    lo: float = 0.0,
    hi: float = 0.002,
    iterations: int = 40,
) -> float:
    if total_return_fn(lo) <= 0.0:
        return lo
    if total_return_fn(hi) > 0.0:
        return hi
    left = lo
    right = hi
    for _ in range(iterations):
        mid = (left + right) / 2.0
        if total_return_fn(mid) > 0.0:
            left = mid
        else:
            right = mid
    return left


def readiness_verdict(*, assumed_cost_side: float, break_even_cost_side: float) -> str:
    if break_even_cost_side <= 0.0:
        return "NO_GO"
    if assumed_cost_side <= break_even_cost_side * 0.7:
        return "GO"
    if assumed_cost_side <= break_even_cost_side:
        return "CAUTION"
    return "NO_GO"


def _load_ohlc(
    ticker: str,
    period: str,
    interval: str,
    *,
    data_source: str = "auto",
    source_csv: str = "",
) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    source = str(source_csv or "").strip()
    return load_ohlc(
        ticker=ticker,
        period=period,
        interval=interval,
        data_source=data_source,
        source_csv_path=source or None,
    )


def _ema_series(values: Sequence[float], period: int) -> List[float]:
    alpha = 2.0 / (period + 1.0)
    out = [0.0] * len(values)
    ema = float(values[0])
    out[0] = ema
    for i in range(1, len(values)):
        ema = ema + alpha * (float(values[i]) - ema)
        out[i] = ema
    return out


def _atr_series(highs: Sequence[float], lows: Sequence[float], closes: Sequence[float], period: int) -> List[float]:
    tr = [0.0] * len(closes)
    prev_close = float(closes[0])
    for i in range(len(closes)):
        high = float(highs[i])
        low = float(lows[i])
        tr[i] = (high - low) if i == 0 else max(high - low, abs(high - prev_close), abs(low - prev_close))
        prev_close = float(closes[i])
    out = [0.0] * len(closes)
    rolling = 0.0
    for i, value in enumerate(tr):
        rolling += value
        if i >= period:
            rolling -= tr[i - period]
        out[i] = rolling / float(period) if i >= period - 1 else rolling / float(i + 1)
    return out


def _is_session_allowed(hour_utc: int, start: int, end: int) -> bool:
    if start == end:
        return True
    if start < end:
        return start <= hour_utc <= end
    return hour_utc >= start or hour_utc <= end


def reversion_guard_allows(
    *,
    atr_ratio_to_median: float | None,
    ema_gap_over_atr: float,
    max_atr_ratio_to_median: float,
    max_ema_gap_over_atr: float,
) -> bool:
    if max_atr_ratio_to_median > 0.0 and atr_ratio_to_median is not None:
        if atr_ratio_to_median > max_atr_ratio_to_median:
            return False
    if max_ema_gap_over_atr > 0.0:
        if ema_gap_over_atr > max_ema_gap_over_atr:
            return False
    return True


def _detect_regime(
    *,
    ema_fast: float,
    ema_slow: float,
    atr_value: float,
    trend_threshold: float,
) -> str:
    if atr_value <= 0.0:
        return "range"
    if trend_threshold <= 0.0:
        return "trend"
    trend_strength = abs(ema_fast - ema_slow) / atr_value
    return "trend" if trend_strength >= trend_threshold else "range"


def _trend_signal(
    *,
    last_close: float,
    ema_fast: float,
    ema_slow: float,
    atr_value: float,
    pullback_atr: float,
) -> str:
    if atr_value <= 0.0:
        return "HOLD"
    pull = atr_value * pullback_atr
    if ema_fast > ema_slow and last_close <= ema_fast - pull:
        return "BUY"
    if ema_fast < ema_slow and last_close >= ema_fast + pull:
        return "SELL"
    return "HOLD"


def _reversion_signal(
    *,
    last_close: float,
    ema_anchor: float,
    atr_value: float,
    reversion_atr: float,
) -> str:
    if atr_value <= 0.0 or reversion_atr <= 0.0:
        return "HOLD"
    distance = atr_value * reversion_atr
    if last_close <= ema_anchor - distance:
        return "BUY"
    if last_close >= ema_anchor + distance:
        return "SELL"
    return "HOLD"


def _decide_signal_with_mode(
    *,
    strategy_mode: str,
    last_close: float,
    ema_fast: float,
    ema_slow: float,
    atr_value: float,
    pullback_atr: float,
    reversion_atr: float,
    trend_threshold: float,
) -> Tuple[str, str]:
    mode = str(strategy_mode or "trend").strip().lower()
    if mode == "trend":
        return (
            _trend_signal(
                last_close=last_close,
                ema_fast=ema_fast,
                ema_slow=ema_slow,
                atr_value=atr_value,
                pullback_atr=pullback_atr,
            ),
            "trend",
        )
    if mode == "reversion":
        return (
            _reversion_signal(
                last_close=last_close,
                ema_anchor=ema_slow,
                atr_value=atr_value,
                reversion_atr=reversion_atr,
            ),
            "reversion",
        )
    regime = _detect_regime(
        ema_fast=ema_fast,
        ema_slow=ema_slow,
        atr_value=atr_value,
        trend_threshold=trend_threshold,
    )
    if regime == "trend":
        return (
            _trend_signal(
                last_close=last_close,
                ema_fast=ema_fast,
                ema_slow=ema_slow,
                atr_value=atr_value,
                pullback_atr=pullback_atr,
            ),
            "trend",
        )
    return (
        _reversion_signal(
            last_close=last_close,
            ema_anchor=ema_slow,
            atr_value=atr_value,
            reversion_atr=reversion_atr,
        ),
        "reversion",
    )


def _simulate_gross_returns(
    cfg: Dict[str, float],
    *,
    start_idx: int,
    end_idx: int,
    times: Sequence,
    opens: Sequence[float],
    highs: Sequence[float],
    lows: Sequence[float],
    closes: Sequence[float],
    ema_fast: Sequence[float],
    ema_slow: Sequence[float],
    atr_values: Sequence[float],
    runtime_metrics: Dict[str, object] | None = None,
) -> List[float]:
    warmup = max(int(cfg["slow_ema"]), int(cfg["atr_period"])) + 5
    start = max(start_idx, warmup)
    end = min(end_idx - 1, len(closes) - 1)

    atr_pct = [(atr_values[i] / closes[i]) if closes[i] > 0.0 else 0.0 for i in range(len(closes))]
    strategy_mode = str(cfg.get("strategy_mode", "trend")).strip().lower()
    regime_trend_threshold = float(cfg.get("regime_trend_threshold", 1.2))
    reversion_atr = float(cfg.get("reversion_atr", 0.8))
    reversion_sl_atr = float(cfg.get("reversion_sl_atr", cfg.get("sl_atr", 1.2)))
    reversion_tp_atr = float(cfg.get("reversion_tp_atr", cfg.get("tp_atr", 1.2)))
    reversion_max_atr_ratio_to_median = float(cfg.get("reversion_max_atr_ratio_to_median", 999.0))
    reversion_max_ema_gap_over_atr = float(cfg.get("reversion_max_ema_gap_over_atr", 999.0))
    min_ema_gap_over_atr = float(cfg.get("min_ema_gap_over_atr", 0.0))
    max_ema_gap_over_atr = float(cfg.get("max_ema_gap_over_atr", 999.0))
    max_positions = max(1, int(cfg.get("max_positions", 1)))

    position = 0
    entry = 0.0
    sl = 0.0
    tp = 0.0
    gross_returns: List[float] = []

    for i in range(start, end):
        if position == 1:
            sl_hit = lows[i] <= sl
            tp_hit = highs[i] >= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                gross_returns.append((exit_px - entry) / entry)
                position = 0
                continue
        elif position == -1:
            sl_hit = highs[i] >= sl
            tp_hit = lows[i] <= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                gross_returns.append((entry - exit_px) / entry)
                position = 0
                continue

        hour = times[i].hour
        if not _is_session_allowed(hour, int(cfg["session_start_hour_utc"]), int(cfg["session_end_hour_utc"])):
            if runtime_metrics is not None:
                runtime_metrics["session_reject_count"] = int(runtime_metrics.get("session_reject_count", 0)) + 1
            continue

        window = atr_pct[max(0, i - int(cfg["atr_filter_window"]) + 1) : i + 1]
        atr_ratio_to_median = None
        if len(window) >= int(cfg["atr_filter_min_samples"]):
            ordered = sorted(window)
            median = ordered[len(ordered) // 2]
            if median > 0.0:
                ratio = atr_pct[i] / median
                atr_ratio_to_median = float(ratio)
                if ratio < float(cfg["min_atr_ratio_to_median"]) or ratio > float(cfg["max_atr_ratio_to_median"]):
                    continue

        if runtime_metrics is not None:
            runtime_metrics["signal_eval_count"] = int(runtime_metrics.get("signal_eval_count", 0)) + 1
            signal_counts = runtime_metrics.get("signal_counts", {})
            if not isinstance(signal_counts, dict):
                signal_counts = {"BUY": 0, "SELL": 0, "HOLD": 0}
                runtime_metrics["signal_counts"] = signal_counts

        atr_now = float(atr_values[i])
        if atr_now <= 0.0:
            if runtime_metrics is not None:
                signal_counts = runtime_metrics.get("signal_counts", {})
                if isinstance(signal_counts, dict):
                    signal_counts["HOLD"] = int(signal_counts.get("HOLD", 0)) + 1
            continue

        ema_gap_over_atr = abs(float(ema_fast[i]) - float(ema_slow[i])) / atr_now
        if not (min_ema_gap_over_atr <= ema_gap_over_atr <= max_ema_gap_over_atr):
            if runtime_metrics is not None:
                runtime_metrics["gap_reject_count"] = int(runtime_metrics.get("gap_reject_count", 0)) + 1
                signal_counts = runtime_metrics.get("signal_counts", {})
                if isinstance(signal_counts, dict):
                    signal_counts["HOLD"] = int(signal_counts.get("HOLD", 0)) + 1
            continue

        signal_text, signal_source = _decide_signal_with_mode(
            strategy_mode=strategy_mode,
            last_close=closes[i],
            ema_fast=ema_fast[i],
            ema_slow=ema_slow[i],
            atr_value=atr_values[i],
            pullback_atr=float(cfg["pullback_atr"]),
            reversion_atr=reversion_atr,
            trend_threshold=regime_trend_threshold,
        )
        signal = 1 if signal_text == "BUY" else -1 if signal_text == "SELL" else 0
        if signal_source == "reversion" and signal != 0:
            if not reversion_guard_allows(
                atr_ratio_to_median=atr_ratio_to_median,
                ema_gap_over_atr=ema_gap_over_atr,
                max_atr_ratio_to_median=reversion_max_atr_ratio_to_median,
                max_ema_gap_over_atr=reversion_max_ema_gap_over_atr,
            ):
                signal = 0
        if runtime_metrics is not None:
            signal_counts = runtime_metrics.get("signal_counts", {})
            if isinstance(signal_counts, dict):
                label = "BUY" if signal > 0 else "SELL" if signal < 0 else "HOLD"
                signal_counts[label] = int(signal_counts.get(label, 0)) + 1

        if position == 0 and signal != 0:
            entry = opens[i + 1]
            sl_atr = reversion_sl_atr if signal_source == "reversion" else float(cfg["sl_atr"])
            tp_atr = reversion_tp_atr if signal_source == "reversion" else float(cfg["tp_atr"])
            sl_dist = atr_values[i] * sl_atr
            tp_dist = atr_values[i] * tp_atr
            if signal == 1:
                sl = entry - sl_dist
                tp = entry + tp_dist
            else:
                sl = entry + sl_dist
                tp = entry - tp_dist
            position = signal
            continue

        if position != 0 and signal != 0 and ((position == 1 and signal == 1) or (position == -1 and signal == -1)):
            if runtime_metrics is not None and max_positions <= 1:
                runtime_metrics["maxpos_reject_count"] = int(runtime_metrics.get("maxpos_reject_count", 0)) + 1
            continue

        if position != 0 and signal != 0 and ((position == 1 and signal == -1) or (position == -1 and signal == 1)):
            exit_px = opens[i + 1]
            gross_returns.append((exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry)
            position = 0

    if position != 0:
        exit_px = closes[min(end_idx - 1, len(closes) - 1)]
        gross_returns.append((exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry)

    return gross_returns


def _metrics(gross_returns: Sequence[float], cost_per_side: float) -> Dict[str, float]:
    net = [r - (2.0 * cost_per_side) for r in gross_returns]
    if not net:
        return {"trades": 0.0, "win_rate": 0.0, "pf": 0.0, "total_return": 0.0, "max_dd": 0.0}
    wins = [r for r in net if r > 0.0]
    losses = [r for r in net if r < 0.0]
    pf = (sum(wins) / abs(sum(losses))) if losses else 99.0
    equity = 1.0
    peak = 1.0
    max_dd = 0.0
    for r in net:
        equity *= 1.0 + r
        if equity > peak:
            peak = equity
        max_dd = max(max_dd, (peak - equity) / peak)
    return {
        "trades": float(len(net)),
        "win_rate": float(len(wins) / len(net)),
        "pf": float(pf),
        "total_return": float(equity - 1.0),
        "max_dd": float(max_dd),
    }


def _parse_split_ratios(value: str) -> List[float]:
    out: List[float] = []
    for token in value.split(","):
        token = token.strip()
        if not token:
            continue
        ratio = float(token)
        if ratio <= 0.0 or ratio >= 1.0:
            raise ValueError("split ratios must be between 0 and 1")
        out.append(ratio)
    return out


def main() -> None:
    parser = argparse.ArgumentParser(description="Readiness report for xau_autobot tuned config")
    parser.add_argument("--config", required=True, help="Config JSON file path")
    parser.add_argument("--ticker", default="GC=F")
    parser.add_argument("--period", default="60d")
    parser.add_argument("--interval", default="5m")
    parser.add_argument("--data-source", default="auto", choices=["auto", "yahoo", "mt5_csv"])
    parser.add_argument("--source-csv", default="", help="CSV path for --data-source mt5_csv")
    parser.add_argument("--assumed-cost-side", type=float, default=0.0002)
    parser.add_argument("--split-ratios", default="0.5,0.6,0.7")
    parser.add_argument("--write-report", default="")
    parser.add_argument(
        "--runtime-journal-path",
        default="",
        help="Runtime journal output path (default: env XAU_AUTOBOT_RUNTIME_JOURNAL_PATH or repo report path)",
    )
    parser.add_argument(
        "--disable-runtime-journal",
        action="store_true",
        help="Disable runtime journal append for this readiness run",
    )
    args = parser.parse_args()
    if args.data_source == "mt5_csv" and str(args.source_csv).strip() == "":
        parser.error("--source-csv is required when --data-source=mt5_csv")

    cfg: Dict[str, float] = json.load(open(args.config, "r", encoding="utf-8"))
    split_ratios = _parse_split_ratios(args.split_ratios)
    times, opens, highs, lows, closes = _load_ohlc(
        args.ticker,
        args.period,
        args.interval,
        data_source=args.data_source,
        source_csv=args.source_csv,
    )
    ema_fast = _ema_series(closes, int(cfg["fast_ema"]))
    ema_slow = _ema_series(closes, int(cfg["slow_ema"]))
    atr_values = _atr_series(highs, lows, closes, int(cfg["atr_period"]))

    runtime_metrics = _new_runtime_metrics()
    gross_all = _simulate_gross_returns(
        cfg,
        start_idx=0,
        end_idx=len(closes),
        times=times,
        opens=opens,
        highs=highs,
        lows=lows,
        closes=closes,
        ema_fast=ema_fast,
        ema_slow=ema_slow,
        atr_values=atr_values,
        runtime_metrics=runtime_metrics,
    )
    total_fn = lambda cps: total_return_from_gross(gross_all, cps)
    break_even_cost_side = estimate_break_even_cost(total_fn, lo=0.0, hi=0.002, iterations=40)
    verdict = readiness_verdict(assumed_cost_side=args.assumed_cost_side, break_even_cost_side=break_even_cost_side)

    trial_run_id = str(os.getenv("XAU_AUTOBOT_TRIAL_RUN_ID", "")).strip() or Path(args.config).stem
    runtime_snapshot = _runtime_metrics_snapshot(runtime_metrics)

    report: Dict[str, object] = {
        "config": args.config,
        "run_id": trial_run_id,
        "ticker": args.ticker,
        "period": args.period,
        "interval": args.interval,
        "bars": len(closes),
        "range_start": str(times[0]),
        "range_end": str(times[-1]),
        "assumed_cost_side": args.assumed_cost_side,
        "break_even_cost_side": break_even_cost_side,
        "break_even_roundtrip_cost": 2.0 * break_even_cost_side,
        "verdict": verdict,
        "all": _metrics(gross_all, args.assumed_cost_side),
        "runtime_metrics": runtime_snapshot,
        "splits": [],
    }

    for ratio in split_ratios:
        split = int(len(closes) * ratio)
        is_gross = _simulate_gross_returns(
            cfg,
            start_idx=0,
            end_idx=split,
            times=times,
            opens=opens,
            highs=highs,
            lows=lows,
            closes=closes,
            ema_fast=ema_fast,
            ema_slow=ema_slow,
            atr_values=atr_values,
        )
        oos_gross = _simulate_gross_returns(
            cfg,
            start_idx=split,
            end_idx=len(closes),
            times=times,
            opens=opens,
            highs=highs,
            lows=lows,
            closes=closes,
            ema_fast=ema_fast,
            ema_slow=ema_slow,
            atr_values=atr_values,
        )
        report["splits"].append(
            {
                "ratio": ratio,
                "is": _metrics(is_gross, args.assumed_cost_side),
                "oos": _metrics(oos_gross, args.assumed_cost_side),
            }
        )

    print(json.dumps(report, ensure_ascii=True))

    runtime_journal_path = resolve_runtime_journal_path(args.runtime_journal_path)
    if not args.disable_runtime_journal:
        journal_payload: Dict[str, object] = {
            "timestamp_utc": datetime.now(timezone.utc).isoformat(),
            "run_id": trial_run_id,
            "magic": int(cfg.get("magic", 0)),
            "comment": str(cfg.get("comment", "")),
            "comment_prefix": str(cfg.get("comment", ""))[:16],
            "runtime_metrics": runtime_snapshot,
            "source": "xau_autobot_readiness",
        }
        append_runtime_journal(runtime_journal_path, journal_payload)

    if args.write_report:
        with open(args.write_report, "w", encoding="utf-8") as f:
            json.dump(report, f, ensure_ascii=True, indent=2)
            f.write("\n")
        print(json.dumps({"written_report": args.write_report}, ensure_ascii=True))


if __name__ == "__main__":
    main()
