#!/usr/bin/env python3
"""Evaluate M20 executor baseline vs M45 soft-gated variant on MT5 CSV data."""

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


def _signal_to_int(signal: str) -> int:
    text = str(signal).upper()
    if text == "BUY":
        return 1
    if text == "SELL":
        return -1
    return 0


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


def timeframe_to_interval(timeframe: str) -> str:
    tf = str(timeframe or "").strip().upper()
    if tf == "":
        raise ValueError("timeframe is required")
    return tf.lower()


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


def classify_commander_mode(
    *,
    has_state: bool,
    regime_strength: float,
    trend_threshold: float,
    neutral_max_strength: Optional[float] = None,
) -> ModeName:
    if not has_state:
        return "neutral"
    if neutral_max_strength is not None and regime_strength < float(neutral_max_strength):
        return "neutral"
    if regime_strength >= trend_threshold:
        return "trend"
    return "reversion"


def next_two_state_mode(
    *,
    prev_state: Optional[str],
    regime_strength: float,
    trend_on: float,
    trend_off: float,
) -> str:
    state = str(prev_state or "").strip().lower()
    if state not in {"trend", "non_trend"}:
        state = "non_trend"
    if state == "non_trend" and float(regime_strength) >= float(trend_on):
        return "trend"
    if state == "trend" and float(regime_strength) <= float(trend_off):
        return "non_trend"
    return state


def select_gate_policy_for_state(state: str) -> str:
    return "block_opposite" if str(state).strip().lower() == "trend" else "none"


def resolve_trend_override_params(
    *,
    enabled: bool,
    prev_state: Optional[str],
    initial_state: str,
    regime_strength: float,
    trend_on: float,
    trend_off: float,
    base_min_ema_gap_over_atr: float,
    base_pullback_atr: float,
    override_min_ema_gap_over_atr: Optional[float],
    override_pullback_atr: Optional[float],
) -> Tuple[str, float, float]:
    if not bool(enabled):
        return "disabled", float(base_min_ema_gap_over_atr), float(base_pullback_atr)

    seed_state = prev_state
    if seed_state is None:
        seed_state = str(initial_state or "").strip().lower() or "non_trend"
    state = next_two_state_mode(
        prev_state=seed_state,
        regime_strength=float(regime_strength),
        trend_on=float(trend_on),
        trend_off=float(trend_off),
    )

    min_ema_gap = float(base_min_ema_gap_over_atr)
    pullback = float(base_pullback_atr)
    if state == "trend":
        if override_min_ema_gap_over_atr is not None:
            min_ema_gap = float(override_min_ema_gap_over_atr)
        if override_pullback_atr is not None:
            pullback = float(override_pullback_atr)
    return state, min_ema_gap, pullback


def apply_m45_bias_gate(
    *,
    signal: int,
    bias: int,
    gate_policy: str,
    neutral_policy: str,
    gate_active: bool = True,
) -> Tuple[int, bool]:
    if signal == 0:
        return 0, False
    if not bool(gate_active):
        return signal, False

    policy = str(gate_policy or "none").strip().lower()
    neutral = str(neutral_policy or "allow_all").strip().lower()

    if policy not in {"none", "block_opposite", "hard_lock"}:
        raise ValueError(f"unsupported m45_bias_gate_policy: {gate_policy}")
    if neutral not in {"allow_all", "block_all"}:
        raise ValueError(f"unsupported m45_neutral_policy: {neutral_policy}")

    if policy == "none":
        return signal, False

    if bias == 0:
        if neutral == "block_all":
            return 0, True
        return signal, False

    if policy == "block_opposite":
        if signal == -bias:
            return 0, True
        return signal, False

    # hard_lock
    if signal != bias:
        return 0, True
    return signal, False


def should_start_bias_flip_cooldown(*, prev_bias: Optional[int], current_bias: int) -> bool:
    if prev_bias is None:
        return False
    return int(prev_bias) != int(current_bias)


def update_flip_relax_until(
    *,
    prev_bias: Optional[int],
    current_bias: int,
    current_index: int,
    flip_relax_m20_bars: int,
    relax_until_index: int,
) -> int:
    bars = max(0, int(flip_relax_m20_bars))
    if bars <= 0:
        return int(relax_until_index)
    if should_start_bias_flip_cooldown(prev_bias=prev_bias, current_bias=current_bias):
        return max(int(relax_until_index), int(current_index + bars))
    return int(relax_until_index)


def is_flip_relax_active(*, current_index: int, relax_until_index: int) -> bool:
    return int(current_index) < int(relax_until_index)


def gate_active_with_flip_relax(*, has_state: bool, gate_policy: str, flip_relax_active: bool) -> bool:
    gate_active = bool(has_state and str(gate_policy or "none").strip().lower() != "none")
    return bool(gate_active and not bool(flip_relax_active))


def is_flip_relax_opposite_entry(*, signal: int, bias: int, in_flip_relax: bool) -> bool:
    return bool(bool(in_flip_relax) and int(signal) != 0 and int(bias) != 0 and int(signal) == -int(bias))


def should_start_loss_cooldown(*, exit_reason: str, trade_return: float) -> bool:
    # E6 canonical: trigger only on SL-hit losses.
    return str(exit_reason or "").strip().lower() == "sl" and float(trade_return) < 0.0


def update_loss_relax_until(
    *,
    exit_reason: str,
    trade_return: float,
    current_index: int,
    loss_relax_m20_bars: int,
    relax_until_index: int,
) -> Tuple[int, bool]:
    bars = max(0, int(loss_relax_m20_bars))
    if bars <= 0:
        return int(relax_until_index), False
    if should_start_loss_cooldown(exit_reason=exit_reason, trade_return=trade_return):
        return max(int(relax_until_index), int(current_index + bars)), True
    return int(relax_until_index), False


def is_loss_relax_active(*, current_index: int, relax_until_index: int) -> bool:
    return int(current_index) < int(relax_until_index)


def loss_streak_p95_from_returns(returns: Sequence[float]) -> float:
    streaks: List[int] = []
    run = 0
    for value in returns:
        if float(value) < 0.0:
            run += 1
            continue
        if run > 0:
            streaks.append(run)
            run = 0
    if run > 0:
        streaks.append(run)
    if not streaks:
        return 0.0
    ordered = sorted(streaks)
    rank = max(1, int((len(ordered) * 95 + 99) // 100))
    return float(ordered[min(len(ordered), rank) - 1])


def percentile_nearest_rank(values: Sequence[float], pct: float) -> float:
    if not values:
        return 0.0
    ordered = sorted(float(v) for v in values)
    if len(ordered) == 1:
        return float(ordered[0])
    p = max(0.0, min(1.0, float(pct)))
    rank = max(1, int((len(ordered) * (p * 100.0) + 99.0) // 100.0))
    return float(ordered[min(len(ordered), rank) - 1])


def pf_active_from_window(*, closed: float, pf: float, closed_floor: float) -> Optional[float]:
    if float(closed) < float(closed_floor):
        return None
    return float(pf)


def _optional_float(value: object) -> Optional[float]:
    if value is None:
        return None
    if isinstance(value, str):
        text = value.strip()
        if text == "":
            return None
        return float(text)
    return float(value)


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
    cfg: Dict[str, object]


@dataclass
class M45CommanderContext:
    times: List[datetime]
    regime_strength: List[float]
    bias: List[int]


@dataclass
class SimulationDiagnostics:
    mode_counts: Dict[str, int]
    reject_reason_breakdown: Dict[str, int]
    signal_eval_count: int
    trade_returns_by_mode: Dict[str, List[float]]
    all_trade_returns: List[float]
    gate_active_bar_count: int
    evaluated_bar_count: int
    cooldown_block_count: int
    loss_cooldown_trigger_count: int
    loss_cooldown_block_count: int
    loss_cooldown_block_bars_total: int


@dataclass
class SwitchSimulationDiagnostics:
    state_counts: Dict[str, int]
    reject_reason_breakdown: Dict[str, int]
    signal_eval_count: int
    trade_returns_by_state: Dict[str, List[float]]
    all_trade_returns: List[float]
    gate_active_bar_count: int
    evaluated_bar_count: int
    bias_gate_reject_count_trend: int
    flip_relax_bar_count: int
    flip_relax_trade_count: int
    flip_relax_trade_returns: List[float]
    flip_relax_opposite_trade_count: int
    loss_relax_trigger_count: int
    loss_relax_bar_count: int
    loss_relax_trade_count: int
    loss_relax_trade_returns: List[float]
    loss_relax_opposite_trade_count: int


def _build_executor_context(*, cfg: Dict[str, object], source_csv: str, period: str) -> ExecutorContext:
    interval = timeframe_to_interval(str(cfg.get("timeframe", "M20")))
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


def _build_m45_commander_context(*, m45_cfg: Dict[str, object], source_csv: str, period: str) -> M45CommanderContext:
    interval = timeframe_to_interval(str(m45_cfg.get("timeframe", "M45")))
    times, _, highs, lows, closes = load_ohlc(
        ticker="XAUUSD",
        period=period,
        interval=interval,
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
    return M45CommanderContext(times=times_dt, regime_strength=strengths, bias=bias_values)


def _resolve_m45_state_at(*, ts: datetime, m45_ctx: M45CommanderContext) -> Tuple[bool, float, int]:
    idx = bisect.bisect_right(m45_ctx.times, ts) - 1
    if idx < 0:
        return False, 0.0, 0
    return True, float(m45_ctx.regime_strength[idx]), int(m45_ctx.bias[idx])


def _simulate_executor(
    *,
    context: ExecutorContext,
    m45_ctx: M45CommanderContext,
    m45_trend_threshold: float,
    m45_neutral_max_strength: Optional[float],
    gate_policy: str,
    neutral_policy: str,
    gate_min_strength_to_activate: Optional[float],
    cooldown_bars_after_bias_flip: int,
    neutral_min_ema_gap_over_atr: Optional[float],
    neutral_pullback_atr: Optional[float],
    after_loss_cooldown_m20_bars: int,
    start_idx: int,
    end_idx: int,
) -> Tuple[List[float], SimulationDiagnostics]:
    cfg = context.cfg
    warmup = max(int(cfg["slow_ema"]), int(cfg["atr_period"])) + 5
    start = max(start_idx, warmup)
    end = min(end_idx - 1, len(context.closes) - 1)

    position = 0
    entry = 0.0
    sl = 0.0
    tp = 0.0
    position_mode = "neutral"
    gross_returns: List[float] = []

    mode_counts = {"trend": 0, "reversion": 0, "neutral": 0}
    reject_counts = {
        "session": 0,
        "atr_ratio": 0,
        "ema_gap": 0,
        "bias_gate": 0,
        "cooldown": 0,
        "loss_cooldown": 0,
    }
    signal_eval_count = 0
    trade_returns_by_mode: Dict[str, List[float]] = {"trend": [], "reversion": [], "neutral": []}
    gate_active_bar_count = 0
    evaluated_bar_count = 0
    cooldown_block_count = 0
    cooldown_until_index = -1
    loss_cooldown_until_index = -1
    loss_cooldown_trigger_count = 0
    loss_cooldown_block_count = 0
    loss_cooldown_block_bars_total = 0
    prev_bias: Optional[int] = None
    trend_override_enabled = bool(cfg.get("trend_override_enabled", False))
    trend_override_state: Optional[str] = None
    trend_override_on = float(cfg.get("trend_override_on", 2.35))
    trend_override_off = float(cfg.get("trend_override_off", 2.25))
    trend_override_initial_state = str(cfg.get("trend_override_initial_state", "non_trend")).strip().lower() or "non_trend"
    trend_override_min_ema_gap = _optional_float(cfg.get("trend_override_min_ema_gap_over_atr"))
    trend_override_pullback = _optional_float(cfg.get("trend_override_pullback_atr"))

    for i in range(start, end):
        has_state, strength, bias = _resolve_m45_state_at(ts=context.times[i], m45_ctx=m45_ctx)
        if should_start_bias_flip_cooldown(prev_bias=prev_bias, current_bias=bias):
            cooldown_until_index = max(int(cooldown_until_index), int(i + cooldown_bars_after_bias_flip))
        prev_bias = int(bias)
        cooldown_active = i < cooldown_until_index

        mode = classify_commander_mode(
            has_state=has_state,
            regime_strength=strength,
            trend_threshold=m45_trend_threshold,
            neutral_max_strength=m45_neutral_max_strength,
        )
        mode_counts[mode] = int(mode_counts.get(mode, 0)) + 1
        evaluated_bar_count += 1
        if i < loss_cooldown_until_index:
            loss_cooldown_block_bars_total += 1

        base_min_ema_gap = float(cfg.get("min_ema_gap_over_atr", 0.0))
        base_pullback_atr = float(cfg["pullback_atr"])
        trend_override_state, min_ema_gap, pullback_atr = resolve_trend_override_params(
            enabled=trend_override_enabled,
            prev_state=trend_override_state,
            initial_state=trend_override_initial_state,
            regime_strength=float(strength),
            trend_on=trend_override_on,
            trend_off=trend_override_off,
            base_min_ema_gap_over_atr=base_min_ema_gap,
            base_pullback_atr=base_pullback_atr,
            override_min_ema_gap_over_atr=trend_override_min_ema_gap,
            override_pullback_atr=trend_override_pullback,
        )

        gate_active = False
        if str(gate_policy or "none").strip().lower() != "none":
            gate_active = bool(has_state)
            if gate_active and gate_min_strength_to_activate is not None:
                gate_active = strength >= float(gate_min_strength_to_activate)
        if gate_active:
            gate_active_bar_count += 1

        if position == 1:
            sl_hit = context.lows[i] <= sl
            tp_hit = context.highs[i] >= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                trade_return = (exit_px - entry) / entry
                gross_returns.append(trade_return)
                trade_returns_by_mode.setdefault(position_mode, []).append(trade_return)
                if int(after_loss_cooldown_m20_bars) > 0:
                    exit_reason = "sl" if sl_hit else "tp"
                    if should_start_loss_cooldown(exit_reason=exit_reason, trade_return=trade_return):
                        loss_cooldown_trigger_count += 1
                        loss_cooldown_until_index = max(
                            int(loss_cooldown_until_index),
                            int(i + after_loss_cooldown_m20_bars),
                        )
                position = 0
                position_mode = "neutral"
                continue
        elif position == -1:
            sl_hit = context.highs[i] >= sl
            tp_hit = context.lows[i] <= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                trade_return = (entry - exit_px) / entry
                gross_returns.append(trade_return)
                trade_returns_by_mode.setdefault(position_mode, []).append(trade_return)
                if int(after_loss_cooldown_m20_bars) > 0:
                    exit_reason = "sl" if sl_hit else "tp"
                    if should_start_loss_cooldown(exit_reason=exit_reason, trade_return=trade_return):
                        loss_cooldown_trigger_count += 1
                        loss_cooldown_until_index = max(
                            int(loss_cooldown_until_index),
                            int(i + after_loss_cooldown_m20_bars),
                        )
                position = 0
                position_mode = "neutral"
                continue

        if not _is_session_allowed(
            hour_utc=context.times[i].hour,
            session_start=int(cfg["session_start_hour_utc"]),
            session_end=int(cfg["session_end_hour_utc"]),
        ):
            reject_counts["session"] += 1
            continue

        ratio = _atr_ratio_to_median(
            atr_pct_values=context.atr_pct,
            index=i,
            window=int(cfg["atr_filter_window"]),
            min_samples=int(cfg["atr_filter_min_samples"]),
        )
        if ratio is not None:
            if ratio < float(cfg["min_atr_ratio_to_median"]) or ratio > float(cfg["max_atr_ratio_to_median"]):
                reject_counts["atr_ratio"] += 1
                continue

        atr_value = float(context.atr_values[i])
        if atr_value <= 0.0:
            reject_counts["ema_gap"] += 1
            continue
        if mode == "neutral" and neutral_min_ema_gap_over_atr is not None:
            min_ema_gap = max(min_ema_gap, float(neutral_min_ema_gap_over_atr))
        ema_gap = abs(float(context.ema_fast[i]) - float(context.ema_slow[i])) / atr_value
        if ema_gap < min_ema_gap or ema_gap > float(cfg.get("max_ema_gap_over_atr", 999.0)):
            reject_counts["ema_gap"] += 1
            continue

        if mode == "neutral" and neutral_pullback_atr is not None:
            pullback_atr = float(neutral_pullback_atr)
        signal_ctx = decide_signal_with_mode(
            strategy_mode=str(cfg["strategy_mode"]),
            last_close=float(context.closes[i]),
            ema_fast=float(context.ema_fast[i]),
            ema_slow=float(context.ema_slow[i]),
            atr_value=float(context.atr_values[i]),
            pullback_atr=pullback_atr,
            reversion_atr=float(cfg.get("reversion_atr", 0.2)),
            trend_threshold=float(cfg.get("regime_trend_threshold", 1.2)),
        )
        signal_eval_count += 1
        signal = _signal_to_int(str(signal_ctx.get("signal", "HOLD")))
        source = str(signal_ctx.get("source", "trend"))

        gated_signal, rejected = apply_m45_bias_gate(
            signal=signal,
            bias=bias,
            gate_policy=gate_policy,
            neutral_policy=neutral_policy,
            gate_active=gate_active,
        )
        if rejected:
            reject_counts["bias_gate"] += 1
        signal = gated_signal

        if position == 0 and signal != 0 and cooldown_active:
            cooldown_block_count += 1
            reject_counts["cooldown"] += 1
            signal = 0

        if position == 0 and signal != 0 and i < loss_cooldown_until_index:
            loss_cooldown_block_count += 1
            reject_counts["loss_cooldown"] += 1
            signal = 0

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
            position_mode = mode
            continue

        if position != 0 and signal != 0 and ((position == 1 and signal == -1) or (position == -1 and signal == 1)):
            exit_px = float(context.opens[i + 1])
            trade_return = (exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry
            gross_returns.append(trade_return)
            trade_returns_by_mode.setdefault(position_mode, []).append(trade_return)
            position = 0
            position_mode = "neutral"

    if position != 0:
        exit_px = float(context.closes[min(end_idx - 1, len(context.closes) - 1)])
        trade_return = (exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry
        gross_returns.append(trade_return)
        trade_returns_by_mode.setdefault(position_mode, []).append(trade_return)

    diagnostics = SimulationDiagnostics(
        mode_counts=mode_counts,
        reject_reason_breakdown=reject_counts,
        signal_eval_count=signal_eval_count,
        trade_returns_by_mode=trade_returns_by_mode,
        all_trade_returns=list(gross_returns),
        gate_active_bar_count=gate_active_bar_count,
        evaluated_bar_count=evaluated_bar_count,
        cooldown_block_count=cooldown_block_count,
        loss_cooldown_trigger_count=loss_cooldown_trigger_count,
        loss_cooldown_block_count=loss_cooldown_block_count,
        loss_cooldown_block_bars_total=loss_cooldown_block_bars_total,
    )
    return gross_returns, diagnostics


def _simulate_two_state_switch(
    *,
    trend_context: ExecutorContext,
    non_trend_context: ExecutorContext,
    m45_ctx: M45CommanderContext,
    switch_trend_on: float,
    switch_trend_off: float,
    switch_initial_state: str,
    flip_relax_m20_bars: int,
    loss_relax_m20_bars: int,
    start_idx: int,
    end_idx: int,
) -> Tuple[List[float], SwitchSimulationDiagnostics]:
    warmup = max(
        int(trend_context.cfg["slow_ema"]),
        int(non_trend_context.cfg["slow_ema"]),
        int(trend_context.cfg["atr_period"]),
        int(non_trend_context.cfg["atr_period"]),
    ) + 5
    start = max(start_idx, warmup)
    end = min(end_idx - 1, len(trend_context.closes) - 1)

    state = str(switch_initial_state or "").strip().lower()
    if state not in {"trend", "non_trend"}:
        state = "non_trend"

    position = 0
    entry = 0.0
    sl = 0.0
    tp = 0.0
    position_state = "non_trend"
    position_entered_in_flip_relax = False
    position_entered_in_loss_relax = False
    gross_returns: List[float] = []

    state_counts = {"trend": 0, "non_trend": 0}
    reject_counts = {
        "session": 0,
        "atr_ratio": 0,
        "ema_gap": 0,
        "bias_gate": 0,
    }
    signal_eval_count = 0
    trade_returns_by_state: Dict[str, List[float]] = {"trend": [], "non_trend": []}
    gate_active_bar_count = 0
    evaluated_bar_count = 0
    bias_gate_reject_count_trend = 0
    flip_relax_until_index = -1
    loss_relax_until_index = -1
    prev_bias: Optional[int] = None
    flip_relax_bar_count = 0
    flip_relax_trade_count = 0
    flip_relax_trade_returns: List[float] = []
    flip_relax_opposite_trade_count = 0
    loss_relax_trigger_count = 0
    loss_relax_bar_count = 0
    loss_relax_trade_count = 0
    loss_relax_trade_returns: List[float] = []
    loss_relax_opposite_trade_count = 0

    for i in range(start, end):
        has_state, strength, bias = _resolve_m45_state_at(ts=trend_context.times[i], m45_ctx=m45_ctx)
        flip_relax_until_index = update_flip_relax_until(
            prev_bias=prev_bias,
            current_bias=bias,
            current_index=i,
            flip_relax_m20_bars=flip_relax_m20_bars,
            relax_until_index=flip_relax_until_index,
        )
        prev_bias = int(bias)
        flip_relax_active = is_flip_relax_active(current_index=i, relax_until_index=flip_relax_until_index)
        if flip_relax_active:
            flip_relax_bar_count += 1
        loss_relax_active = is_loss_relax_active(current_index=i, relax_until_index=loss_relax_until_index)
        if loss_relax_active:
            loss_relax_bar_count += 1
        state = next_two_state_mode(
            prev_state=state,
            regime_strength=strength,
            trend_on=switch_trend_on,
            trend_off=switch_trend_off,
        )
        state_counts[state] = int(state_counts.get(state, 0)) + 1
        evaluated_bar_count += 1

        active_context = trend_context if state == "trend" else non_trend_context
        cfg = active_context.cfg

        if position == 1:
            sl_hit = active_context.lows[i] <= sl
            tp_hit = active_context.highs[i] >= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                trade_return = (exit_px - entry) / entry
                gross_returns.append(trade_return)
                trade_returns_by_state.setdefault(position_state, []).append(trade_return)
                if position_entered_in_flip_relax:
                    flip_relax_trade_count += 1
                    flip_relax_trade_returns.append(float(trade_return))
                if position_entered_in_loss_relax:
                    loss_relax_trade_count += 1
                    loss_relax_trade_returns.append(float(trade_return))
                exit_reason = "sl" if sl_hit else "tp"
                loss_relax_until_index, loss_relax_triggered = update_loss_relax_until(
                    exit_reason=exit_reason,
                    trade_return=trade_return,
                    current_index=i,
                    loss_relax_m20_bars=loss_relax_m20_bars,
                    relax_until_index=loss_relax_until_index,
                )
                if loss_relax_triggered:
                    loss_relax_trigger_count += 1
                position = 0
                position_state = "non_trend"
                position_entered_in_flip_relax = False
                position_entered_in_loss_relax = False
                continue
        elif position == -1:
            sl_hit = active_context.highs[i] >= sl
            tp_hit = active_context.lows[i] <= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                trade_return = (entry - exit_px) / entry
                gross_returns.append(trade_return)
                trade_returns_by_state.setdefault(position_state, []).append(trade_return)
                if position_entered_in_flip_relax:
                    flip_relax_trade_count += 1
                    flip_relax_trade_returns.append(float(trade_return))
                if position_entered_in_loss_relax:
                    loss_relax_trade_count += 1
                    loss_relax_trade_returns.append(float(trade_return))
                exit_reason = "sl" if sl_hit else "tp"
                loss_relax_until_index, loss_relax_triggered = update_loss_relax_until(
                    exit_reason=exit_reason,
                    trade_return=trade_return,
                    current_index=i,
                    loss_relax_m20_bars=loss_relax_m20_bars,
                    relax_until_index=loss_relax_until_index,
                )
                if loss_relax_triggered:
                    loss_relax_trigger_count += 1
                position = 0
                position_state = "non_trend"
                position_entered_in_flip_relax = False
                position_entered_in_loss_relax = False
                continue

        if not _is_session_allowed(
            hour_utc=active_context.times[i].hour,
            session_start=int(cfg["session_start_hour_utc"]),
            session_end=int(cfg["session_end_hour_utc"]),
        ):
            reject_counts["session"] += 1
            continue

        ratio = _atr_ratio_to_median(
            atr_pct_values=active_context.atr_pct,
            index=i,
            window=int(cfg["atr_filter_window"]),
            min_samples=int(cfg["atr_filter_min_samples"]),
        )
        if ratio is not None:
            if ratio < float(cfg["min_atr_ratio_to_median"]) or ratio > float(cfg["max_atr_ratio_to_median"]):
                reject_counts["atr_ratio"] += 1
                continue

        atr_value = float(active_context.atr_values[i])
        if atr_value <= 0.0:
            reject_counts["ema_gap"] += 1
            continue
        ema_gap = abs(float(active_context.ema_fast[i]) - float(active_context.ema_slow[i])) / atr_value
        if ema_gap < float(cfg.get("min_ema_gap_over_atr", 0.0)) or ema_gap > float(cfg.get("max_ema_gap_over_atr", 999.0)):
            reject_counts["ema_gap"] += 1
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
        signal_eval_count += 1
        signal = _signal_to_int(str(signal_ctx.get("signal", "HOLD")))
        source = str(signal_ctx.get("source", "trend"))

        gate_policy = select_gate_policy_for_state(state)
        gate_active = gate_active_with_flip_relax(
            has_state=has_state,
            gate_policy=gate_policy,
            flip_relax_active=(flip_relax_active or loss_relax_active),
        )
        if gate_active:
            gate_active_bar_count += 1
        gated_signal, rejected = apply_m45_bias_gate(
            signal=signal,
            bias=bias,
            gate_policy=gate_policy,
            neutral_policy="allow_all",
            gate_active=gate_active,
        )
        if rejected:
            reject_counts["bias_gate"] += 1
            if state == "trend":
                bias_gate_reject_count_trend += 1
        signal = gated_signal

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
            position_state = state
            position_entered_in_flip_relax = bool(flip_relax_active)
            position_entered_in_loss_relax = bool(loss_relax_active)
            if is_flip_relax_opposite_entry(signal=signal, bias=bias, in_flip_relax=position_entered_in_flip_relax):
                flip_relax_opposite_trade_count += 1
            if is_flip_relax_opposite_entry(signal=signal, bias=bias, in_flip_relax=position_entered_in_loss_relax):
                loss_relax_opposite_trade_count += 1
            continue

        if position != 0 and signal != 0 and ((position == 1 and signal == -1) or (position == -1 and signal == 1)):
            exit_px = float(active_context.opens[i + 1])
            trade_return = (exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry
            gross_returns.append(trade_return)
            trade_returns_by_state.setdefault(position_state, []).append(trade_return)
            if position_entered_in_flip_relax:
                flip_relax_trade_count += 1
                flip_relax_trade_returns.append(float(trade_return))
            if position_entered_in_loss_relax:
                loss_relax_trade_count += 1
                loss_relax_trade_returns.append(float(trade_return))
            position = 0
            position_state = "non_trend"
            position_entered_in_flip_relax = False
            position_entered_in_loss_relax = False

    if position != 0:
        exit_px = float(trend_context.closes[min(end_idx - 1, len(trend_context.closes) - 1)])
        trade_return = (exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry
        gross_returns.append(trade_return)
        trade_returns_by_state.setdefault(position_state, []).append(trade_return)
        if position_entered_in_flip_relax:
            flip_relax_trade_count += 1
            flip_relax_trade_returns.append(float(trade_return))
        if position_entered_in_loss_relax:
            loss_relax_trade_count += 1
            loss_relax_trade_returns.append(float(trade_return))

    diagnostics = SwitchSimulationDiagnostics(
        state_counts=state_counts,
        reject_reason_breakdown=reject_counts,
        signal_eval_count=signal_eval_count,
        trade_returns_by_state=trade_returns_by_state,
        all_trade_returns=list(gross_returns),
        gate_active_bar_count=gate_active_bar_count,
        evaluated_bar_count=evaluated_bar_count,
        bias_gate_reject_count_trend=bias_gate_reject_count_trend,
        flip_relax_bar_count=flip_relax_bar_count,
        flip_relax_trade_count=flip_relax_trade_count,
        flip_relax_trade_returns=list(flip_relax_trade_returns),
        flip_relax_opposite_trade_count=flip_relax_opposite_trade_count,
        loss_relax_trigger_count=loss_relax_trigger_count,
        loss_relax_bar_count=loss_relax_bar_count,
        loss_relax_trade_count=loss_relax_trade_count,
        loss_relax_trade_returns=list(loss_relax_trade_returns),
        loss_relax_opposite_trade_count=loss_relax_opposite_trade_count,
    )
    return gross_returns, diagnostics


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


def _diagnostics_payload(diag: SimulationDiagnostics, *, cost_side: float) -> Dict[str, object]:
    total_modes = sum(int(v) for v in diag.mode_counts.values())
    mode_time_share = {
        key: (float(diag.mode_counts.get(key, 0)) / float(total_modes) if total_modes > 0 else 0.0)
        for key in ["trend", "reversion", "neutral"]
    }

    trade_count_by_mode = {
        key: int(len(diag.trade_returns_by_mode.get(key, [])))
        for key in ["trend", "reversion", "neutral"]
    }
    pf_by_mode: Dict[str, float] = {}
    for key in ["trend", "reversion", "neutral"]:
        returns = diag.trade_returns_by_mode.get(key, [])
        if len(returns) == 0:
            pf_by_mode[key] = 0.0
            continue
        metrics = _metrics(returns, float(cost_side))
        pf_by_mode[key] = float(metrics.get("pf", 0.0))

    after_cost_returns = [float(v) - (2.0 * float(cost_side)) for v in diag.all_trade_returns]
    return {
        "mode_time_share": mode_time_share,
        "gate_active_time_share": (
            float(diag.gate_active_bar_count) / float(diag.evaluated_bar_count)
            if int(diag.evaluated_bar_count) > 0
            else 0.0
        ),
        "trade_count_by_mode": trade_count_by_mode,
        "pf_by_mode": pf_by_mode,
        "reject_reason_breakdown": {k: int(v) for k, v in diag.reject_reason_breakdown.items()},
        "signal_eval_count": int(diag.signal_eval_count),
        "cooldown_block_count": int(diag.cooldown_block_count),
        "loss_cooldown_trigger_count": int(diag.loss_cooldown_trigger_count),
        "loss_cooldown_block_count": int(diag.loss_cooldown_block_count),
        "loss_cooldown_block_bars_total": int(diag.loss_cooldown_block_bars_total),
        "loss_cooldown_block_time_share": (
            float(diag.loss_cooldown_block_bars_total) / float(diag.evaluated_bar_count)
            if int(diag.evaluated_bar_count) > 0
            else 0.0
        ),
        "loss_streak_p95": loss_streak_p95_from_returns(after_cost_returns),
    }


def _switch_diagnostics_payload(diag: SwitchSimulationDiagnostics, *, cost_side: float) -> Dict[str, object]:
    total_states = sum(int(v) for v in diag.state_counts.values())
    state_time_share = {
        key: (float(diag.state_counts.get(key, 0)) / float(total_states) if total_states > 0 else 0.0)
        for key in ["trend", "non_trend"]
    }
    trade_count_by_state = {
        key: int(len(diag.trade_returns_by_state.get(key, [])))
        for key in ["trend", "non_trend"]
    }
    pf_by_state: Dict[str, float] = {}
    for key in ["trend", "non_trend"]:
        returns = diag.trade_returns_by_state.get(key, [])
        if len(returns) == 0:
            pf_by_state[key] = 0.0
            continue
        metrics = _metrics(returns, float(cost_side))
        pf_by_state[key] = float(metrics.get("pf", 0.0))
    after_cost_returns = [float(v) - (2.0 * float(cost_side)) for v in diag.all_trade_returns]
    flip_relax_pf = (
        float(_metrics(diag.flip_relax_trade_returns, float(cost_side)).get("pf", 0.0))
        if diag.flip_relax_trade_returns
        else 0.0
    )
    loss_relax_pf = (
        float(_metrics(diag.loss_relax_trade_returns, float(cost_side)).get("pf", 0.0))
        if diag.loss_relax_trade_returns
        else 0.0
    )
    return {
        "state_time_share": state_time_share,
        "gate_active_time_share": (
            float(diag.gate_active_bar_count) / float(diag.evaluated_bar_count)
            if int(diag.evaluated_bar_count) > 0
            else 0.0
        ),
        "trade_count_by_state": trade_count_by_state,
        "pf_by_state": pf_by_state,
        "reject_reason_breakdown": {k: int(v) for k, v in diag.reject_reason_breakdown.items()},
        "signal_eval_count": int(diag.signal_eval_count),
        "bias_gate_reject_count_trend": int(diag.bias_gate_reject_count_trend),
        "flip_relax_time_share": (
            float(diag.flip_relax_bar_count) / float(diag.evaluated_bar_count)
            if int(diag.evaluated_bar_count) > 0
            else 0.0
        ),
        "flip_relax_trade_count": int(diag.flip_relax_trade_count),
        "flip_relax_pf": float(flip_relax_pf),
        "flip_relax_opposite_trade_count": int(diag.flip_relax_opposite_trade_count),
        "loss_relax_trigger_count": int(diag.loss_relax_trigger_count),
        "loss_relax_time_share": (
            float(diag.loss_relax_bar_count) / float(diag.evaluated_bar_count)
            if int(diag.evaluated_bar_count) > 0
            else 0.0
        ),
        "loss_relax_trade_count": int(diag.loss_relax_trade_count),
        "loss_relax_pf": float(loss_relax_pf),
        "loss_relax_opposite_trade_count": int(diag.loss_relax_opposite_trade_count),
        "loss_streak_p95": loss_streak_p95_from_returns(after_cost_returns),
    }


def main() -> None:
    parser = argparse.ArgumentParser(description="Evaluate M20 executor baseline vs M45 soft-gated variant")
    parser.add_argument("--executor-config", required=True)
    parser.add_argument("--gated-config", required=True)
    parser.add_argument("--m45-config", required=True)
    parser.add_argument("--baseline-id", default="E3_m20_executor")
    parser.add_argument("--baseline-label", default="M20 executor baseline")
    parser.add_argument("--gated-id", default="E4_m20_m45_soft_gate")
    parser.add_argument("--gated-label", default="M20 executor + M45 opposite-direction block gate")
    parser.add_argument("--switch-config", default="")
    parser.add_argument("--switch-id", default="E8_m20_two_state_switch")
    parser.add_argument("--switch-label", default="M20 two-state trend/non-trend switch + M45 commander")
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
    parser.add_argument("--write-report", default="data/reports/xau_autobot_m20_bias_gate_eval.json")
    parser.add_argument("--write-tsv", default="data/reports/xau_autobot_m20_bias_gate_eval.tsv")
    args = parser.parse_args()

    executor_cfg = json.loads(Path(args.executor_config).read_text(encoding="utf-8"))
    gated_cfg = json.loads(Path(args.gated_config).read_text(encoding="utf-8"))
    m45_cfg = json.loads(Path(args.m45_config).read_text(encoding="utf-8"))
    switch_cfg = json.loads(Path(args.switch_config).read_text(encoding="utf-8")) if str(args.switch_config).strip() else None
    cost_sides = _parse_cost_sides(args.cost_sides)

    executor_ctx = _build_executor_context(cfg=executor_cfg, source_csv=args.source_csv, period=args.period)
    gated_ctx = _build_executor_context(cfg=gated_cfg, source_csv=args.source_csv, period=args.period)
    m45_ctx = _build_m45_commander_context(m45_cfg=m45_cfg, source_csv=args.source_csv, period=args.period)
    switch_trend_ctx: Optional[ExecutorContext] = None
    switch_non_trend_ctx: Optional[ExecutorContext] = None
    switch_trend_on = 2.35
    switch_trend_off = 2.25
    switch_initial_state = "non_trend"
    switch_flip_relax_m20_bars = 0
    switch_loss_relax_m20_bars = 0
    if switch_cfg is not None:
        switch_trend_cfg = switch_cfg["trend_mode"]
        switch_non_trend_cfg = switch_cfg["non_trend_mode"]
        switch_trend_ctx = _build_executor_context(cfg=switch_trend_cfg, source_csv=args.source_csv, period=args.period)
        switch_non_trend_ctx = _build_executor_context(cfg=switch_non_trend_cfg, source_csv=args.source_csv, period=args.period)
        switch_trend_on = float(switch_cfg.get("switch_trend_on", 2.35))
        switch_trend_off = float(switch_cfg.get("switch_trend_off", 2.25))
        switch_initial_state = str(switch_cfg.get("switch_initial_state", "non_trend")).strip().lower()
        switch_flip_relax_m20_bars = int(switch_cfg.get("flip_relax_m20_bars", 0))
        switch_loss_relax_m20_bars = int(switch_cfg.get("loss_relax_m20_bars", 0))
        if switch_initial_state not in {"trend", "non_trend"}:
            switch_initial_state = "non_trend"

    if executor_ctx.times != gated_ctx.times:
        raise RuntimeError("executor and gated config must produce identical evaluation timestamps")
    if switch_trend_ctx is not None and executor_ctx.times != switch_trend_ctx.times:
        raise RuntimeError("switch trend mode must produce identical evaluation timestamps")
    if switch_non_trend_ctx is not None and executor_ctx.times != switch_non_trend_ctx.times:
        raise RuntimeError("switch non-trend mode must produce identical evaluation timestamps")

    m45_threshold = float(m45_cfg.get("regime_trend_threshold", 2.4))
    windows = _build_windows(times=executor_ctx.times, window_days=args.window_days, step_days=args.step_days)

    experiments: List[Dict[str, object]] = [
        {
            "kind": "single",
            "experiment_id": str(args.baseline_id),
            "experiment_label": str(args.baseline_label),
            "context": executor_ctx,
            "gate_policy": str(executor_cfg.get("m45_bias_gate_policy", "none")),
            "neutral_policy": str(executor_cfg.get("m45_neutral_policy", "allow_all")),
            "gate_min_strength_to_activate": _optional_float(executor_cfg.get("m45_gate_min_strength_to_block_opposite")),
            "cooldown_bars_after_bias_flip": int(executor_cfg.get("cooldown_m20_bars_after_bias_flip", 0)),
            "m45_neutral_max_strength": _optional_float(executor_cfg.get("m45_neutral_max_strength")),
            "neutral_min_ema_gap_over_atr": _optional_float(executor_cfg.get("neutral_min_ema_gap_over_atr")),
            "neutral_pullback_atr": _optional_float(executor_cfg.get("neutral_pullback_atr")),
            "after_loss_cooldown_m20_bars": int(executor_cfg.get("after_loss_cooldown_m20_bars", 0)),
        },
        {
            "kind": "single",
            "experiment_id": str(args.gated_id),
            "experiment_label": str(args.gated_label),
            "context": gated_ctx,
            "gate_policy": str(gated_cfg.get("m45_bias_gate_policy", "none")),
            "neutral_policy": str(gated_cfg.get("m45_neutral_policy", "allow_all")),
            "gate_min_strength_to_activate": _optional_float(gated_cfg.get("m45_gate_min_strength_to_block_opposite")),
            "cooldown_bars_after_bias_flip": int(gated_cfg.get("cooldown_m20_bars_after_bias_flip", 0)),
            "m45_neutral_max_strength": _optional_float(gated_cfg.get("m45_neutral_max_strength")),
            "neutral_min_ema_gap_over_atr": _optional_float(gated_cfg.get("neutral_min_ema_gap_over_atr")),
            "neutral_pullback_atr": _optional_float(gated_cfg.get("neutral_pullback_atr")),
            "after_loss_cooldown_m20_bars": int(gated_cfg.get("after_loss_cooldown_m20_bars", 0)),
        },
    ]
    if switch_cfg is not None and switch_trend_ctx is not None and switch_non_trend_ctx is not None:
        experiments.append(
            {
                "kind": "switch",
                "experiment_id": str(args.switch_id),
                "experiment_label": str(args.switch_label),
                "trend_context": switch_trend_ctx,
                "non_trend_context": switch_non_trend_ctx,
                "switch_trend_on": float(switch_trend_on),
                "switch_trend_off": float(switch_trend_off),
                "switch_initial_state": str(switch_initial_state),
                "flip_relax_m20_bars": int(switch_flip_relax_m20_bars),
                "loss_relax_m20_bars": int(switch_loss_relax_m20_bars),
            }
        )

    rows: List[Dict[str, object]] = []

    for exp in experiments:
        exp_kind = str(exp["kind"])
        exp_id = str(exp["experiment_id"])
        exp_label = str(exp["experiment_label"])
        for cost_side in cost_sides:
            if exp_kind == "single":
                exp_ctx = exp["context"]
                assert isinstance(exp_ctx, ExecutorContext)
                gate_policy = str(exp["gate_policy"])
                neutral_policy = str(exp["neutral_policy"])
                gate_min_strength_to_activate = exp["gate_min_strength_to_activate"]
                cooldown_bars_after_bias_flip = int(exp["cooldown_bars_after_bias_flip"])
                m45_neutral_max_strength = exp["m45_neutral_max_strength"]
                neutral_min_ema_gap_over_atr = exp["neutral_min_ema_gap_over_atr"]
                neutral_pullback_atr = exp["neutral_pullback_atr"]
                after_loss_cooldown_m20_bars = int(exp["after_loss_cooldown_m20_bars"])
                flip_relax_m20_bars = 0
                loss_relax_m20_bars = 0
                full_gross, full_diag = _simulate_executor(
                    context=exp_ctx,
                    m45_ctx=m45_ctx,
                    m45_trend_threshold=m45_threshold,
                    m45_neutral_max_strength=(
                        float(m45_neutral_max_strength) if m45_neutral_max_strength is not None else None
                    ),
                    gate_policy=gate_policy,
                    neutral_policy=neutral_policy,
                    gate_min_strength_to_activate=(
                        float(gate_min_strength_to_activate) if gate_min_strength_to_activate is not None else None
                    ),
                    cooldown_bars_after_bias_flip=cooldown_bars_after_bias_flip,
                    neutral_min_ema_gap_over_atr=(
                        float(neutral_min_ema_gap_over_atr) if neutral_min_ema_gap_over_atr is not None else None
                    ),
                    neutral_pullback_atr=(float(neutral_pullback_atr) if neutral_pullback_atr is not None else None),
                    after_loss_cooldown_m20_bars=after_loss_cooldown_m20_bars,
                    start_idx=0,
                    end_idx=len(exp_ctx.times),
                )
                diagnostic_payload = _diagnostics_payload(full_diag, cost_side=float(cost_side))
            else:
                trend_ctx_obj = exp["trend_context"]
                non_trend_ctx_obj = exp["non_trend_context"]
                assert isinstance(trend_ctx_obj, ExecutorContext)
                assert isinstance(non_trend_ctx_obj, ExecutorContext)
                gate_policy = "two_state_switch"
                neutral_policy = "two_state_switch"
                gate_min_strength_to_activate = None
                cooldown_bars_after_bias_flip = 0
                m45_neutral_max_strength = None
                neutral_min_ema_gap_over_atr = None
                neutral_pullback_atr = None
                after_loss_cooldown_m20_bars = 0
                flip_relax_m20_bars = int(exp.get("flip_relax_m20_bars", 0))
                loss_relax_m20_bars = int(exp.get("loss_relax_m20_bars", 0))
                full_gross, full_switch_diag = _simulate_two_state_switch(
                    trend_context=trend_ctx_obj,
                    non_trend_context=non_trend_ctx_obj,
                    m45_ctx=m45_ctx,
                    switch_trend_on=float(exp["switch_trend_on"]),
                    switch_trend_off=float(exp["switch_trend_off"]),
                    switch_initial_state=str(exp["switch_initial_state"]),
                    flip_relax_m20_bars=flip_relax_m20_bars,
                    loss_relax_m20_bars=loss_relax_m20_bars,
                    start_idx=0,
                    end_idx=len(trend_ctx_obj.times),
                )
                diagnostic_payload = _switch_diagnostics_payload(full_switch_diag, cost_side=float(cost_side))

            window_rows: List[Dict[str, object]] = []
            fail_closed = 0
            fail_pf = 0
            fail_dd = 0
            active_flip_relax_returns: List[float] = []
            active_flip_relax_trade_count = 0
            active_flip_relax_opposite_trade_count = 0
            active_loss_relax_returns: List[float] = []
            active_loss_relax_trade_count = 0
            active_loss_relax_opposite_trade_count = 0

            for wi, start_ts, end_ts, start_idx, end_idx in windows:
                window_switch_diag: Optional[SwitchSimulationDiagnostics] = None
                if exp_kind == "single":
                    gross, _ = _simulate_executor(
                        context=exp_ctx,
                        m45_ctx=m45_ctx,
                        m45_trend_threshold=m45_threshold,
                        m45_neutral_max_strength=(
                            float(m45_neutral_max_strength) if m45_neutral_max_strength is not None else None
                        ),
                        gate_policy=gate_policy,
                        neutral_policy=neutral_policy,
                        gate_min_strength_to_activate=(
                            float(gate_min_strength_to_activate) if gate_min_strength_to_activate is not None else None
                        ),
                        cooldown_bars_after_bias_flip=cooldown_bars_after_bias_flip,
                        neutral_min_ema_gap_over_atr=(
                            float(neutral_min_ema_gap_over_atr) if neutral_min_ema_gap_over_atr is not None else None
                        ),
                        neutral_pullback_atr=(float(neutral_pullback_atr) if neutral_pullback_atr is not None else None),
                        after_loss_cooldown_m20_bars=after_loss_cooldown_m20_bars,
                        start_idx=start_idx,
                        end_idx=end_idx,
                    )
                    closed_by_state: Optional[Dict[str, int]] = None
                    pf_by_state: Optional[Dict[str, float]] = None
                    bias_gate_reject_count_trend: Optional[int] = None
                else:
                    gross, window_switch_diag = _simulate_two_state_switch(
                        trend_context=trend_ctx_obj,
                        non_trend_context=non_trend_ctx_obj,
                        m45_ctx=m45_ctx,
                        switch_trend_on=float(exp["switch_trend_on"]),
                        switch_trend_off=float(exp["switch_trend_off"]),
                        switch_initial_state=str(exp["switch_initial_state"]),
                        flip_relax_m20_bars=flip_relax_m20_bars,
                        loss_relax_m20_bars=loss_relax_m20_bars,
                        start_idx=start_idx,
                        end_idx=end_idx,
                    )
                    closed_by_state = {
                        "trend": int(len(window_switch_diag.trade_returns_by_state.get("trend", []))),
                        "non_trend": int(len(window_switch_diag.trade_returns_by_state.get("non_trend", []))),
                    }
                    pf_by_state = {}
                    for state_key in ["trend", "non_trend"]:
                        state_returns = window_switch_diag.trade_returns_by_state.get(state_key, [])
                        if len(state_returns) == 0:
                            pf_by_state[state_key] = 0.0
                        else:
                            pf_by_state[state_key] = float(_metrics(state_returns, float(cost_side)).get("pf", 0.0))
                    bias_gate_reject_count_trend = int(window_switch_diag.bias_gate_reject_count_trend)

                metrics = _metrics(gross, float(cost_side))
                closed = float(metrics["trades"])
                pf = float(metrics["pf"])
                max_dd = float(metrics["max_dd"])
                net_profit = float(metrics["total_return"]) * float(args.account_balance)
                pf_active = pf_active_from_window(
                    closed=closed,
                    pf=pf,
                    closed_floor=float(args.closed_floor),
                )
                if exp_kind == "switch" and window_switch_diag is not None and pf_active is not None:
                    active_flip_relax_returns.extend(window_switch_diag.flip_relax_trade_returns)
                    active_flip_relax_trade_count += int(window_switch_diag.flip_relax_trade_count)
                    active_flip_relax_opposite_trade_count += int(window_switch_diag.flip_relax_opposite_trade_count)
                    active_loss_relax_returns.extend(window_switch_diag.loss_relax_trade_returns)
                    active_loss_relax_trade_count += int(window_switch_diag.loss_relax_trade_count)
                    active_loss_relax_opposite_trade_count += int(window_switch_diag.loss_relax_opposite_trade_count)
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
                        "pf_active": pf_active,
                        "max_dd": max_dd,
                        "net_profit": net_profit,
                        "window_pass": len(reasons) == 0,
                        "window_fail_reasons": reasons,
                        "closed_by_state": closed_by_state,
                        "pf_by_state": pf_by_state,
                        "bias_gate_reject_count_trend": bias_gate_reject_count_trend,
                    }
                )

            pass_window_count = sum(1 for w in window_rows if bool(w["window_pass"]))
            active_pfs = [float(w["pf_active"]) for w in window_rows if w["pf_active"] is not None]
            closed_values = [float(w["closed"]) for w in window_rows]
            flip_relax_pf_active: Optional[float] = None
            loss_relax_pf_active: Optional[float] = None
            if exp_kind == "switch":
                flip_relax_pf_active = float(_metrics(active_flip_relax_returns, float(cost_side)).get("pf", 0.0))
                loss_relax_pf_active = float(_metrics(active_loss_relax_returns, float(cost_side)).get("pf", 0.0))
            worst_active_window_breakdown: Optional[Dict[str, object]] = None
            if exp_kind == "switch" and active_pfs:
                active_rows = [w for w in window_rows if w["pf_active"] is not None]
                if active_rows:
                    worst_active = min(active_rows, key=lambda w: float(w["pf_active"]))
                    worst_active_window_breakdown = {
                        "window_start_utc": str(worst_active["start_utc"]),
                        "window_end_utc": str(worst_active["end_utc"]),
                        "closed_by_state": worst_active["closed_by_state"],
                        "pf_by_state": worst_active["pf_by_state"],
                        "bias_gate_reject_count_trend": worst_active["bias_gate_reject_count_trend"],
                    }
            summary = {
                "window_count": len(window_rows),
                "pass_window_count": pass_window_count,
                "fail_window_count": len(window_rows) - pass_window_count,
                "fail_closed_count": fail_closed,
                "fail_pf_count": fail_pf,
                "fail_dd_count": fail_dd,
                "pass_all_windows": (fail_closed == 0 and fail_pf == 0 and fail_dd == 0),
                "active_window_count": len(active_pfs),
                "worst_pf": min((float(w["pf"]) for w in window_rows), default=0.0),
                "worst_pf_active": min(active_pfs) if active_pfs else None,
                "p05_pf_active": percentile_nearest_rank(active_pfs, 0.05) if active_pfs else None,
                "flip_relax_pf_active": flip_relax_pf_active,
                "flip_relax_trade_count_active": int(active_flip_relax_trade_count) if exp_kind == "switch" else 0,
                "flip_relax_opposite_trade_count_active": (
                    int(active_flip_relax_opposite_trade_count) if exp_kind == "switch" else 0
                ),
                "loss_relax_pf_active": loss_relax_pf_active,
                "loss_relax_trade_count_active": int(active_loss_relax_trade_count) if exp_kind == "switch" else 0,
                "loss_relax_opposite_trade_count_active": (
                    int(active_loss_relax_opposite_trade_count) if exp_kind == "switch" else 0
                ),
                "p05_closed": percentile_nearest_rank(closed_values, 0.05) if closed_values else 0.0,
                "worst_closed": min((float(w["closed"]) for w in window_rows), default=0.0),
                "worst_max_dd": max((float(w["max_dd"]) for w in window_rows), default=0.0),
                "worst_net_profit": min((float(w["net_profit"]) for w in window_rows), default=0.0),
                "full_period_trades": float(len(full_gross)),
                "worst_active_window_breakdown": worst_active_window_breakdown,
            }

            rows.append(
                {
                    "experiment_id": exp_id,
                    "experiment_label": exp_label,
                    "cost_side": float(cost_side),
                    "gate_policy": gate_policy,
                    "neutral_policy": neutral_policy,
                    "gate_min_strength_to_activate": gate_min_strength_to_activate,
                    "cooldown_m20_bars_after_bias_flip": cooldown_bars_after_bias_flip,
                    "m45_neutral_max_strength": m45_neutral_max_strength,
                    "neutral_min_ema_gap_over_atr": neutral_min_ema_gap_over_atr,
                    "neutral_pullback_atr": neutral_pullback_atr,
                    "after_loss_cooldown_m20_bars": after_loss_cooldown_m20_bars,
                    "flip_relax_m20_bars": int(flip_relax_m20_bars) if exp_kind == "switch" else 0,
                    "loss_relax_m20_bars": int(loss_relax_m20_bars) if exp_kind == "switch" else 0,
                    "summary": summary,
                    "diagnostics": diagnostic_payload,
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
            "executor_config": str(args.executor_config),
            "gated_config": str(args.gated_config),
            "switch_config": str(args.switch_config).strip() or None,
            "m45_config": str(args.m45_config),
        },
        "protocol": {
            "window_days": int(args.window_days),
            "step_days": int(args.step_days),
            "closed_floor": float(args.closed_floor),
            "pf_floor": float(args.pf_floor),
            "max_dd_ceiling": float(args.max_dd_ceiling),
            "m45_regime_trend_threshold": m45_threshold,
            "switch_trend_on": float(switch_trend_on) if switch_cfg is not None else None,
            "switch_trend_off": float(switch_trend_off) if switch_cfg is not None else None,
            "switch_flip_relax_m20_bars": int(switch_flip_relax_m20_bars) if switch_cfg is not None else None,
            "switch_loss_relax_m20_bars": int(switch_loss_relax_m20_bars) if switch_cfg is not None else None,
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
                    "gate_policy",
                    "window_count",
                    "pass_window_count",
                    "fail_closed_count",
                    "fail_pf_count",
                    "fail_dd_count",
                    "pass_all_windows",
                    "worst_pf",
                    "worst_pf_active",
                    "p05_pf_active",
                    "p05_closed",
                    "worst_closed",
                    "worst_max_dd",
                    "worst_net_profit",
                    "gate_active_time_share",
                    "cooldown_block_count",
                    "loss_cooldown_trigger_count",
                    "loss_cooldown_block_count",
                    "loss_cooldown_block_bars_total",
                    "loss_cooldown_block_time_share",
                    "loss_streak_p95",
                    "flip_relax_time_share",
                    "flip_relax_trade_count",
                    "flip_relax_pf_active",
                    "flip_relax_opposite_trade_count",
                    "loss_relax_trigger_count",
                    "loss_relax_time_share",
                    "loss_relax_trade_count",
                    "loss_relax_pf_active",
                    "loss_relax_opposite_trade_count",
                    "state_time_share_trend",
                    "state_time_share_non_trend",
                    "bias_gate_reject_count_trend",
                ]
            )
            + "\n"
        )
        for row in rows:
            summary = row["summary"]
            assert isinstance(summary, dict)
            diagnostics = row["diagnostics"]
            assert isinstance(diagnostics, dict)
            state_time_share = diagnostics.get("state_time_share", {})
            if not isinstance(state_time_share, dict):
                state_time_share = {}
            worst_pf_active = summary.get("worst_pf_active")
            p05_pf_active = summary.get("p05_pf_active")
            f.write(
                "\t".join(
                    [
                        str(row["experiment_id"]),
                        f"{float(row['cost_side']):.4f}",
                        str(row["gate_policy"]),
                        str(int(summary["window_count"])),
                        str(int(summary["pass_window_count"])),
                        str(int(summary["fail_closed_count"])),
                        str(int(summary["fail_pf_count"])),
                        str(int(summary["fail_dd_count"])),
                        "true" if bool(summary["pass_all_windows"]) else "false",
                        f"{float(summary['worst_pf']):.6f}",
                        "NA" if worst_pf_active is None else f"{float(worst_pf_active):.6f}",
                        "NA" if p05_pf_active is None else f"{float(p05_pf_active):.6f}",
                        f"{float(summary['p05_closed']):.6f}",
                        f"{float(summary['worst_closed']):.2f}",
                        f"{float(summary['worst_max_dd']):.6f}",
                        f"{float(summary['worst_net_profit']):.2f}",
                        f"{float(diagnostics.get('gate_active_time_share', 0.0)):.6f}",
                        str(int(diagnostics.get("cooldown_block_count", 0))),
                        str(int(diagnostics.get("loss_cooldown_trigger_count", 0))),
                        str(int(diagnostics.get("loss_cooldown_block_count", 0))),
                        str(int(diagnostics.get("loss_cooldown_block_bars_total", 0))),
                        f"{float(diagnostics.get('loss_cooldown_block_time_share', 0.0)):.6f}",
                        f"{float(diagnostics.get('loss_streak_p95', 0.0)):.6f}",
                        f"{float(diagnostics.get('flip_relax_time_share', 0.0)):.6f}",
                        str(int(diagnostics.get("flip_relax_trade_count", 0))),
                        (
                            "NA"
                            if summary.get("flip_relax_pf_active") is None
                            else f"{float(summary.get('flip_relax_pf_active', 0.0)):.6f}"
                        ),
                        str(int(diagnostics.get("flip_relax_opposite_trade_count", 0))),
                        str(int(diagnostics.get("loss_relax_trigger_count", 0))),
                        f"{float(diagnostics.get('loss_relax_time_share', 0.0)):.6f}",
                        str(int(diagnostics.get("loss_relax_trade_count", 0))),
                        (
                            "NA"
                            if summary.get("loss_relax_pf_active") is None
                            else f"{float(summary.get('loss_relax_pf_active', 0.0)):.6f}"
                        ),
                        str(int(diagnostics.get("loss_relax_opposite_trade_count", 0))),
                        f"{float(state_time_share.get('trend', 0.0)):.6f}",
                        f"{float(state_time_share.get('non_trend', 0.0)):.6f}",
                        str(int(diagnostics.get("bias_gate_reject_count_trend", 0))),
                    ]
                )
                + "\n"
            )

    print(json.dumps({"written_report": str(report_path), "written_tsv": str(tsv_path)}, ensure_ascii=True))
    for row in rows:
        print(
            json.dumps(
                {
                    "experiment_id": row["experiment_id"],
                    "cost_side": row["cost_side"],
                    "gate_policy": row["gate_policy"],
                    "summary": row["summary"],
                    "diagnostics": row["diagnostics"],
                },
                ensure_ascii=True,
            )
        )


if __name__ == "__main__":
    main()
