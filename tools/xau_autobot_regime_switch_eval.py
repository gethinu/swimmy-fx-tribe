#!/usr/bin/env python3
"""Evaluate M5 executor with M45 regime-switch gating on MT5 CSV data."""

from __future__ import annotations

import argparse
import bisect
import json
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

try:
    from tools.xau_autobot import atr_series, decide_signal_with_mode
    from tools.xau_autobot_data import load_ohlc
    from tools.xau_autobot_readiness import _metrics
except Exception:
    from xau_autobot import atr_series, decide_signal_with_mode  # type: ignore
    from xau_autobot_data import load_ohlc  # type: ignore
    from xau_autobot_readiness import _metrics  # type: ignore


ModeName = str


def ema_series(values: Sequence[float], period: int) -> List[float]:
    alpha = 2.0 / (float(period) + 1.0)
    out = [0.0] * len(values)
    ema = float(values[0])
    out[0] = ema
    for i in range(1, len(values)):
        ema = ema + alpha * (float(values[i]) - ema)
        out[i] = ema
    return out


def mode_from_strength_with_hysteresis(
    prev_mode: Optional[ModeName],
    *,
    strength: float,
    high: float,
    low: float,
) -> ModeName:
    if strength >= high:
        return "trend"
    if strength <= low:
        return "reversion"
    if prev_mode in {"trend", "reversion"}:
        return str(prev_mode)
    # Fail-closed: start from reversion when regime is ambiguous.
    return "reversion"


def m45_bias_from_state(
    *,
    ema_fast: float,
    ema_slow: float,
    regime_strength: float,
    min_strength: float,
) -> int:
    if regime_strength < min_strength:
        return 0
    if ema_fast > ema_slow:
        return 1
    if ema_fast < ema_slow:
        return -1
    return 0


def window_pass_reasons(
    *,
    closed: float,
    pf: float,
    max_dd: float,
    closed_floor: float,
    pf_floor: float,
    dd_ceiling: float,
) -> List[str]:
    reasons: List[str] = []
    if closed < closed_floor:
        reasons.append("closed_lt_floor")
    if pf < pf_floor:
        reasons.append("pf_lt_floor")
    if max_dd > dd_ceiling:
        reasons.append("dd_gt_ceiling")
    return reasons


def _parse_cost_sides(value: str) -> List[float]:
    out: List[float] = []
    for token in str(value).split(","):
        text = token.strip()
        if text == "":
            continue
        out.append(float(text))
    if not out:
        raise ValueError("cost_sides must not be empty")
    return out


def _guess_source_tf(path_text: str) -> str:
    name = Path(path_text).name.upper()
    if "_M1" in name:
        return "M1"
    if "_M5" in name:
        return "M5"
    if "_M15" in name:
        return "M15"
    if "_H1" in name:
        return "H1"
    return "UNKNOWN"


def _as_time_list(values: Sequence[object]) -> List[datetime]:
    out: List[datetime] = []
    for ts in values:
        if isinstance(ts, datetime):
            dt = ts
        elif hasattr(ts, "to_pydatetime"):
            dt = ts.to_pydatetime()
        else:
            raise ValueError(f"unsupported timestamp type: {type(ts)!r}")
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        out.append(dt.astimezone(timezone.utc))
    return out


def _is_session_allowed(*, hour_utc: int, session_start: int, session_end: int) -> bool:
    if session_start == session_end:
        return True
    if session_start < session_end:
        return session_start <= hour_utc <= session_end
    return hour_utc >= session_start or hour_utc <= session_end


def _atr_ratio_to_median(
    *,
    atr_pct_values: Sequence[float],
    index: int,
    window: int,
    min_samples: int,
) -> Optional[float]:
    if window <= 0:
        return None
    start = max(0, index - window + 1)
    values = atr_pct_values[start : index + 1]
    if len(values) < min_samples:
        return None
    ordered = sorted(values)
    median = ordered[len(ordered) // 2]
    if median <= 0.0:
        return None
    return float(values[-1] / median)


def _signal_to_int(signal: str) -> int:
    text = str(signal).upper()
    if text == "BUY":
        return 1
    if text == "SELL":
        return -1
    return 0


@dataclass
class ExecutorContext:
    times: List[datetime]
    opens: List[float]
    highs: List[float]
    lows: List[float]
    closes: List[float]
    ema_fast: List[float]
    ema_slow: List[float]
    atr_values: List[float]
    atr_pct: List[float]
    cfg: Dict[str, float]


@dataclass
class M45RegimeContext:
    times: List[datetime]
    regime_strength: List[float]
    bias: List[int]


def _build_executor_context(*, cfg: Dict[str, float], source_csv: str, period: str, interval: str) -> ExecutorContext:
    times, opens, highs, lows, closes = load_ohlc(
        ticker="XAUUSD",
        period=period,
        interval=interval,
        data_source="mt5_csv",
        source_csv_path=source_csv,
    )
    times_dt = _as_time_list(times)
    ema_fast_values = ema_series(closes, int(cfg["fast_ema"]))
    ema_slow_values = ema_series(closes, int(cfg["slow_ema"]))
    atr_values = atr_series(highs, lows, closes, int(cfg["atr_period"]))
    atr_pct = [(atr_values[i] / closes[i]) if closes[i] > 0.0 else 0.0 for i in range(len(closes))]
    return ExecutorContext(
        times=times_dt,
        opens=[float(v) for v in opens],
        highs=[float(v) for v in highs],
        lows=[float(v) for v in lows],
        closes=[float(v) for v in closes],
        ema_fast=[float(v) for v in ema_fast_values],
        ema_slow=[float(v) for v in ema_slow_values],
        atr_values=[float(v) for v in atr_values],
        atr_pct=[float(v) for v in atr_pct],
        cfg=cfg,
    )


def _build_m45_regime_context(*, m45_cfg: Dict[str, float], source_csv: str, period: str) -> M45RegimeContext:
    times, _, highs, lows, closes = load_ohlc(
        ticker="XAUUSD",
        period=period,
        interval="m45",
        data_source="mt5_csv",
        source_csv_path=source_csv,
    )
    times_dt = _as_time_list(times)
    ema_fast_values = ema_series(closes, int(m45_cfg["fast_ema"]))
    ema_slow_values = ema_series(closes, int(m45_cfg["slow_ema"]))
    atr_values = atr_series(highs, lows, closes, int(m45_cfg["atr_period"]))
    threshold = float(m45_cfg.get("regime_trend_threshold", 2.4))

    strengths: List[float] = []
    bias_values: List[int] = []
    for i in range(len(times_dt)):
        atr_value = float(atr_values[i])
        if atr_value <= 0.0:
            strength = 0.0
        else:
            strength = abs(float(ema_fast_values[i]) - float(ema_slow_values[i])) / atr_value
        strengths.append(float(strength))
        bias_values.append(
            m45_bias_from_state(
                ema_fast=float(ema_fast_values[i]),
                ema_slow=float(ema_slow_values[i]),
                regime_strength=float(strength),
                min_strength=threshold,
            )
        )
    return M45RegimeContext(times=times_dt, regime_strength=strengths, bias=bias_values)


def _resolve_m45_state_at(
    *,
    ts: datetime,
    m45_ctx: M45RegimeContext,
) -> Tuple[float, int]:
    idx = bisect.bisect_right(m45_ctx.times, ts) - 1
    if idx < 0:
        return 0.0, 0
    return float(m45_ctx.regime_strength[idx]), int(m45_ctx.bias[idx])


def _simulate_single_mode(
    *,
    context: ExecutorContext,
    start_idx: int,
    end_idx: int,
) -> List[float]:
    cfg = context.cfg
    warmup = max(int(cfg["slow_ema"]), int(cfg["atr_period"])) + 5
    start = max(start_idx, warmup)
    end = min(end_idx - 1, len(context.closes) - 1)

    position = 0
    entry = 0.0
    sl = 0.0
    tp = 0.0
    gross_returns: List[float] = []

    for i in range(start, end):
        if position == 1:
            sl_hit = context.lows[i] <= sl
            tp_hit = context.highs[i] >= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                gross_returns.append((exit_px - entry) / entry)
                position = 0
                continue
        elif position == -1:
            sl_hit = context.highs[i] >= sl
            tp_hit = context.lows[i] <= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                gross_returns.append((entry - exit_px) / entry)
                position = 0
                continue

        if not _is_session_allowed(
            hour_utc=context.times[i].hour,
            session_start=int(cfg["session_start_hour_utc"]),
            session_end=int(cfg["session_end_hour_utc"]),
        ):
            continue

        ratio = _atr_ratio_to_median(
            atr_pct_values=context.atr_pct,
            index=i,
            window=int(cfg["atr_filter_window"]),
            min_samples=int(cfg["atr_filter_min_samples"]),
        )
        if ratio is not None:
            if ratio < float(cfg["min_atr_ratio_to_median"]) or ratio > float(cfg["max_atr_ratio_to_median"]):
                continue

        atr_value = float(context.atr_values[i])
        if atr_value <= 0.0:
            continue
        ema_gap = abs(float(context.ema_fast[i]) - float(context.ema_slow[i])) / atr_value
        if ema_gap < float(cfg.get("min_ema_gap_over_atr", 0.0)) or ema_gap > float(cfg.get("max_ema_gap_over_atr", 999.0)):
            continue

        signal_ctx = decide_signal_with_mode(
            strategy_mode=str(cfg["strategy_mode"]),
            last_close=float(context.closes[i]),
            ema_fast=float(context.ema_fast[i]),
            ema_slow=float(context.ema_slow[i]),
            atr_value=float(context.atr_values[i]),
            pullback_atr=float(cfg["pullback_atr"]),
            reversion_atr=float(cfg.get("reversion_atr", 0.2)),
            trend_threshold=float(cfg.get("regime_trend_threshold", 1.2)),
        )
        signal = _signal_to_int(str(signal_ctx.get("signal", "HOLD")))
        source = str(signal_ctx.get("source", "trend"))

        if position == 0 and signal != 0:
            entry = float(context.opens[i + 1])
            sl_atr = float(cfg.get("reversion_sl_atr", cfg["sl_atr"])) if source == "reversion" else float(cfg["sl_atr"])
            tp_atr = float(cfg.get("reversion_tp_atr", cfg["tp_atr"])) if source == "reversion" else float(cfg["tp_atr"])
            sl_dist = float(context.atr_values[i]) * sl_atr
            tp_dist = float(context.atr_values[i]) * tp_atr
            if signal == 1:
                sl = entry - sl_dist
                tp = entry + tp_dist
            else:
                sl = entry + sl_dist
                tp = entry - tp_dist
            position = signal
            continue

        if position != 0 and signal != 0 and ((position == 1 and signal == -1) or (position == -1 and signal == 1)):
            exit_px = float(context.opens[i + 1])
            gross_returns.append((exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry)
            position = 0

    if position != 0:
        exit_px = float(context.closes[min(end_idx - 1, len(context.closes) - 1)])
        gross_returns.append((exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry)
    return gross_returns


def _simulate_regime_switch(
    *,
    trend_context: ExecutorContext,
    reversion_context: ExecutorContext,
    m45_ctx: M45RegimeContext,
    switch_high: float,
    switch_low: float,
    switch_initial_mode: Optional[str],
    start_idx: int,
    end_idx: int,
) -> List[float]:
    warmup = max(
        int(trend_context.cfg["slow_ema"]),
        int(reversion_context.cfg["slow_ema"]),
        int(trend_context.cfg["atr_period"]),
        int(reversion_context.cfg["atr_period"]),
    ) + 5
    start = max(start_idx, warmup)
    end = min(end_idx - 1, len(trend_context.closes) - 1)

    mode: Optional[str] = str(switch_initial_mode or "").strip().lower() or None
    if mode not in {"trend", "reversion"}:
        mode = None

    position = 0
    entry = 0.0
    sl = 0.0
    tp = 0.0
    gross_returns: List[float] = []

    for i in range(start, end):
        if position == 1:
            sl_hit = trend_context.lows[i] <= sl
            tp_hit = trend_context.highs[i] >= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                gross_returns.append((exit_px - entry) / entry)
                position = 0
                continue
        elif position == -1:
            sl_hit = trend_context.highs[i] >= sl
            tp_hit = trend_context.lows[i] <= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                gross_returns.append((entry - exit_px) / entry)
                position = 0
                continue

        strength, bias = _resolve_m45_state_at(ts=trend_context.times[i], m45_ctx=m45_ctx)
        mode = mode_from_strength_with_hysteresis(mode, strength=strength, high=switch_high, low=switch_low)
        active_context = trend_context if mode == "trend" else reversion_context
        cfg = active_context.cfg

        if not _is_session_allowed(
            hour_utc=active_context.times[i].hour,
            session_start=int(cfg["session_start_hour_utc"]),
            session_end=int(cfg["session_end_hour_utc"]),
        ):
            continue

        ratio = _atr_ratio_to_median(
            atr_pct_values=active_context.atr_pct,
            index=i,
            window=int(cfg["atr_filter_window"]),
            min_samples=int(cfg["atr_filter_min_samples"]),
        )
        if ratio is not None:
            if ratio < float(cfg["min_atr_ratio_to_median"]) or ratio > float(cfg["max_atr_ratio_to_median"]):
                continue

        atr_value = float(active_context.atr_values[i])
        if atr_value <= 0.0:
            continue
        ema_gap = abs(float(active_context.ema_fast[i]) - float(active_context.ema_slow[i])) / atr_value
        if ema_gap < float(cfg.get("min_ema_gap_over_atr", 0.0)) or ema_gap > float(cfg.get("max_ema_gap_over_atr", 999.0)):
            continue

        signal_ctx = decide_signal_with_mode(
            strategy_mode=str(cfg["strategy_mode"]),
            last_close=float(active_context.closes[i]),
            ema_fast=float(active_context.ema_fast[i]),
            ema_slow=float(active_context.ema_slow[i]),
            atr_value=float(active_context.atr_values[i]),
            pullback_atr=float(cfg["pullback_atr"]),
            reversion_atr=float(cfg.get("reversion_atr", 0.2)),
            trend_threshold=float(cfg.get("regime_trend_threshold", 1.2)),
        )
        signal = _signal_to_int(str(signal_ctx.get("signal", "HOLD")))
        source = str(signal_ctx.get("source", "trend"))

        # Trend mode uses M45 bias as hard directional gate.
        if mode == "trend":
            if bias == 0 or signal != bias:
                signal = 0

        if position == 0 and signal != 0:
            entry = float(active_context.opens[i + 1])
            sl_atr = float(cfg.get("reversion_sl_atr", cfg["sl_atr"])) if source == "reversion" else float(cfg["sl_atr"])
            tp_atr = float(cfg.get("reversion_tp_atr", cfg["tp_atr"])) if source == "reversion" else float(cfg["tp_atr"])
            sl_dist = float(active_context.atr_values[i]) * sl_atr
            tp_dist = float(active_context.atr_values[i]) * tp_atr
            if signal == 1:
                sl = entry - sl_dist
                tp = entry + tp_dist
            else:
                sl = entry + sl_dist
                tp = entry - tp_dist
            position = signal
            continue

        if position != 0 and signal != 0 and ((position == 1 and signal == -1) or (position == -1 and signal == 1)):
            exit_px = float(active_context.opens[i + 1])
            gross_returns.append((exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry)
            position = 0

    if position != 0:
        exit_px = float(trend_context.closes[min(end_idx - 1, len(trend_context.closes) - 1)])
        gross_returns.append((exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry)
    return gross_returns


def _build_windows(*, times: Sequence[datetime], window_days: int, step_days: int) -> List[Tuple[int, datetime, datetime, int, int]]:
    if not times:
        return []
    out: List[Tuple[int, datetime, datetime, int, int]] = []
    cursor = times[0]
    last_time = times[-1]
    idx = 1
    while cursor + timedelta(days=window_days) <= last_time:
        start_ts = cursor
        end_ts = cursor + timedelta(days=window_days)
        start_idx = bisect.bisect_left(times, start_ts)
        end_idx = bisect.bisect_right(times, end_ts)
        out.append((idx, start_ts, end_ts, start_idx, end_idx))
        idx += 1
        cursor += timedelta(days=step_days)
    return out


def main() -> None:
    parser = argparse.ArgumentParser(description="Evaluate M5 regime-switch executor vs baseline")
    parser.add_argument("--baseline-config", required=True)
    parser.add_argument("--regime-switch-config", required=True)
    parser.add_argument("--m45-config", required=True)
    parser.add_argument("--source-csv", required=True)
    parser.add_argument("--source-tf", default="", help="Optional source timeframe label (e.g. M1, M15)")
    parser.add_argument("--period", default="365d")
    parser.add_argument("--cost-sides", default="0.0003")
    parser.add_argument("--window-days", type=int, default=30)
    parser.add_argument("--step-days", type=int, default=1)
    parser.add_argument("--closed-floor", type=float, default=12.0)
    parser.add_argument("--pf-floor", type=float, default=1.10)
    parser.add_argument("--max-dd-ceiling", type=float, default=0.07)
    parser.add_argument("--account-balance", type=float, default=100000.0)
    parser.add_argument("--write-report", default="data/reports/xau_autobot_regime_switch_eval.json")
    parser.add_argument("--write-tsv", default="data/reports/xau_autobot_regime_switch_eval.tsv")
    args = parser.parse_args()

    baseline_cfg = json.loads(Path(args.baseline_config).read_text(encoding="utf-8"))
    regime_switch_cfg = json.loads(Path(args.regime_switch_config).read_text(encoding="utf-8"))
    m45_cfg = json.loads(Path(args.m45_config).read_text(encoding="utf-8"))
    cost_sides = _parse_cost_sides(args.cost_sides)

    trend_mode_cfg = regime_switch_cfg["trend_mode"]
    reversion_mode_cfg = regime_switch_cfg["reversion_mode"]

    baseline_ctx = _build_executor_context(cfg=baseline_cfg, source_csv=args.source_csv, period=args.period, interval="m5")
    trend_ctx = _build_executor_context(cfg=trend_mode_cfg, source_csv=args.source_csv, period=args.period, interval="m5")
    reversion_ctx = _build_executor_context(cfg=reversion_mode_cfg, source_csv=args.source_csv, period=args.period, interval="m5")
    m45_ctx = _build_m45_regime_context(m45_cfg=m45_cfg, source_csv=args.source_csv, period=args.period)

    windows = _build_windows(times=baseline_ctx.times, window_days=args.window_days, step_days=args.step_days)
    switch_high = float(regime_switch_cfg.get("switch_trend_strength_high", 2.35))
    switch_low = float(regime_switch_cfg.get("switch_trend_strength_low", 2.05))
    switch_initial_mode = str(regime_switch_cfg.get("switch_initial_mode", "reversion")).strip().lower()

    rows: List[Dict[str, object]] = []

    experiments = [
        ("E0_m5_baseline", "M5 single executor baseline", "baseline"),
        ("E2_m5_regime_switch", "M5 regime-switch executor + M45 commander", "switch"),
    ]

    for exp_id, exp_label, exp_kind in experiments:
        for cost_side in cost_sides:
            window_rows: List[Dict[str, object]] = []
            fail_closed = 0
            fail_pf = 0
            fail_dd = 0
            for wi, start_ts, end_ts, start_idx, end_idx in windows:
                if exp_kind == "baseline":
                    gross = _simulate_single_mode(
                        context=baseline_ctx,
                        start_idx=start_idx,
                        end_idx=end_idx,
                    )
                else:
                    gross = _simulate_regime_switch(
                        trend_context=trend_ctx,
                        reversion_context=reversion_ctx,
                        m45_ctx=m45_ctx,
                        switch_high=switch_high,
                        switch_low=switch_low,
                        switch_initial_mode=switch_initial_mode,
                        start_idx=start_idx,
                        end_idx=end_idx,
                    )
                metrics = _metrics(gross, float(cost_side))
                closed = float(metrics["trades"])
                pf = float(metrics["pf"])
                max_dd = float(metrics["max_dd"])
                net_profit = float(metrics["total_return"]) * float(args.account_balance)
                reasons = window_pass_reasons(
                    closed=closed,
                    pf=pf,
                    max_dd=max_dd,
                    closed_floor=float(args.closed_floor),
                    pf_floor=float(args.pf_floor),
                    dd_ceiling=float(args.max_dd_ceiling),
                )
                if "closed_lt_floor" in reasons:
                    fail_closed += 1
                if "pf_lt_floor" in reasons:
                    fail_pf += 1
                if "dd_gt_ceiling" in reasons:
                    fail_dd += 1
                window_rows.append(
                    {
                        "window_index": wi,
                        "start_utc": start_ts.isoformat(),
                        "end_utc": end_ts.isoformat(),
                        "closed": closed,
                        "pf": pf,
                        "max_dd": max_dd,
                        "net_profit": net_profit,
                        "window_pass": len(reasons) == 0,
                        "window_fail_reasons": reasons,
                    }
                )

            pass_window_count = sum(1 for w in window_rows if bool(w["window_pass"]))
            summary = {
                "window_count": len(window_rows),
                "pass_window_count": pass_window_count,
                "fail_window_count": len(window_rows) - pass_window_count,
                "fail_closed_count": fail_closed,
                "fail_pf_count": fail_pf,
                "fail_dd_count": fail_dd,
                "pass_all_windows": (fail_closed == 0 and fail_pf == 0 and fail_dd == 0),
                "worst_pf": min((float(w["pf"]) for w in window_rows), default=0.0),
                "worst_closed": min((float(w["closed"]) for w in window_rows), default=0.0),
                "worst_max_dd": max((float(w["max_dd"]) for w in window_rows), default=0.0),
                "worst_net_profit": min((float(w["net_profit"]) for w in window_rows), default=0.0),
            }
            rows.append(
                {
                    "experiment_id": exp_id,
                    "experiment_label": exp_label,
                    "cost_side": float(cost_side),
                    "summary": summary,
                    "windows": window_rows,
                }
            )

    report = {
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "dataset": {
            "ticker": "XAUUSD",
            "period": str(args.period),
            "data_source": "mt5_csv",
            "source_csv": str(args.source_csv),
            "source_tf": str(args.source_tf).strip().upper() or _guess_source_tf(str(args.source_csv)),
            "resample_anchor": "UTC_00:00",
            "resample_offset_minutes": 0,
        },
        "configs": {
            "baseline_config": str(args.baseline_config),
            "regime_switch_config": str(args.regime_switch_config),
            "m45_config": str(args.m45_config),
        },
        "protocol": {
            "window_days": int(args.window_days),
            "step_days": int(args.step_days),
            "closed_floor": float(args.closed_floor),
            "pf_floor": float(args.pf_floor),
            "max_dd_ceiling": float(args.max_dd_ceiling),
            "switch_trend_strength_high": switch_high,
            "switch_trend_strength_low": switch_low,
            "switch_initial_mode": switch_initial_mode,
        },
        "rows": rows,
    }

    report_path = Path(args.write_report)
    tsv_path = Path(args.write_tsv)
    report_path.parent.mkdir(parents=True, exist_ok=True)
    tsv_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(json.dumps(report, ensure_ascii=True, indent=2) + "\n", encoding="utf-8")

    with tsv_path.open("w", encoding="utf-8") as f:
        f.write(
            "\t".join(
                [
                    "experiment_id",
                    "cost_side",
                    "window_count",
                    "pass_window_count",
                    "fail_closed_count",
                    "fail_pf_count",
                    "fail_dd_count",
                    "pass_all_windows",
                    "worst_pf",
                    "worst_closed",
                    "worst_max_dd",
                    "worst_net_profit",
                ]
            )
            + "\n"
        )
        for row in rows:
            summary = row["summary"]
            assert isinstance(summary, dict)
            f.write(
                "\t".join(
                    [
                        str(row["experiment_id"]),
                        f"{float(row['cost_side']):.4f}",
                        str(int(summary["window_count"])),
                        str(int(summary["pass_window_count"])),
                        str(int(summary["fail_closed_count"])),
                        str(int(summary["fail_pf_count"])),
                        str(int(summary["fail_dd_count"])),
                        "true" if bool(summary["pass_all_windows"]) else "false",
                        f"{float(summary['worst_pf']):.6f}",
                        f"{float(summary['worst_closed']):.2f}",
                        f"{float(summary['worst_max_dd']):.6f}",
                        f"{float(summary['worst_net_profit']):.2f}",
                    ]
                )
                + "\n"
            )

    print(json.dumps({"written_report": str(report_path), "written_tsv": str(tsv_path)}, ensure_ascii=True))
    for row in rows:
        summary = row["summary"]
        print(
            json.dumps(
                {
                    "experiment_id": row["experiment_id"],
                    "cost_side": row["cost_side"],
                    "summary": summary,
                },
                ensure_ascii=True,
            )
        )


if __name__ == "__main__":
    main()
