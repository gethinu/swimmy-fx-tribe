#!/usr/bin/env python3
"""
Player-centric Armada behavioral replica search.

This tool replaces broad random generation with a deterministic "player -> target
profile -> closest strategy" workflow.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import random
import sys
import tempfile
import time
from collections import deque
from dataclasses import asdict, dataclass
from pathlib import Path
from statistics import median
from typing import Iterable, Optional


def repo_root() -> Path:
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parents[2]


ROOT = repo_root()
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))
SRC_PY = ROOT / "src" / "python"
if str(SRC_PY) not in sys.path:
    sys.path.insert(0, str(SRC_PY))

from sexp_utils import parse_sexp_alist
from tools.backtest_service import BacktestService


@dataclass(frozen=True)
class PlayerProfile:
    key: str
    label: str
    cluster: str
    target_profit_factor: float
    target_drawdown_ratio: float
    target_trades: int
    target_avg_hold_hours: float


@dataclass(frozen=True)
class CandidateSpec:
    indicator_type: str
    sma_short: int
    sma_long: int
    timeframe: int
    sl: float
    tp: float


_TIMEFRAMES = {
    "fast": (15, 30, 60),
    "mid": (60, 120, 240),
    "slow": (120, 240, 360, 720),
}

_MA_PAIRS = {
    "fast": ((3, 8), (5, 14), (8, 21), (10, 30)),
    "mid": ((5, 14), (8, 21), (10, 30), (12, 36), (20, 50)),
    "slow": ((8, 21), (10, 30), (12, 36), (20, 50), (20, 100)),
}

_SL_VALUES = {
    "fast": (0.8, 1.0, 1.2),
    "mid": (1.0, 1.2, 1.5),
    "slow": (1.2, 1.6, 2.0),
}

_TP_VALUES = {
    "fast": (2.0, 2.4, 3.0, 3.6),
    "mid": (2.4, 3.0, 4.0, 5.0),
    "slow": (3.0, 4.0, 6.0, 8.0),
}

_RSI_PERIODS = {
    "fast": (7, 10, 14),
    "mid": (10, 14, 21),
    "slow": (14, 21, 28, 35),
}

_BB_PERIODS = {
    "fast": (14, 20),
    "mid": (20, 30),
    "slow": (20, 30, 40),
}

_STOCH_PERIODS = {
    "fast": (9, 14),
    "mid": (14, 21),
    "slow": (14, 21, 28),
}

_VWAP_PERIODS = {
    "fast": (10, 20, 30),
    "mid": (20, 30, 50),
    "slow": (30, 50, 80),
}

_VOLSMA_PERIODS = {
    "fast": (5, 8, 12),
    "mid": (8, 12, 20),
    "slow": (12, 20, 30),
}

_VOLSMA_SPIKE_PCTS = {
    "fast": (120, 140, 170),
    "mid": (130, 160, 190),
    "slow": (140, 180, 220),
}

_VPOC_PERIODS = {
    "fast": (20, 30, 40),
    "mid": (30, 50, 70),
    "slow": (50, 80, 120),
}

_VPOC_BINS = {
    "fast": (8, 12, 16),
    "mid": (10, 14, 20),
    "slow": (12, 16, 24),
}

_VWAPVR_PERIODS = {
    "fast": (8, 12, 16, 20, 30),
    "mid": (12, 16, 20, 30, 50),
    "slow": (12, 20, 30, 50, 80),
}

_VWAPVR_THRESH_PCTS = {
    "fast": (110, 130, 150, 170, 200),
    "mid": (120, 140, 160, 190, 220),
    "slow": (130, 150, 180, 220, 260),
}

_INDICATORS = ("sma", "ema", "rsi", "bb", "macd", "stoch", "vwap", "volsma", "vpoc", "vwapvr")


def _safe_float(value, default: float = 0.0) -> float:
    try:
        return float(value)
    except (TypeError, ValueError):
        return default


def _safe_int(value, default: int = 0) -> int:
    try:
        return int(value)
    except (TypeError, ValueError):
        return default


def tempo_bucket(target_trades: int) -> str:
    if target_trades >= 750:
        return "fast"
    if target_trades >= 450:
        return "mid"
    return "slow"


def parse_indicator_filter(raw: Optional[str]) -> tuple[str, ...]:
    if not raw:
        return ()
    tokens = [t.strip().lower() for t in str(raw).split(",") if t and t.strip()]
    if not tokens:
        return ()
    allowed = set(_INDICATORS)
    return tuple(dict.fromkeys(t for t in tokens if t in allowed))


def load_profiles(
    config_path: Path | str,
    *,
    include_aggressive: bool,
    players: Optional[set[str]],
) -> list[PlayerProfile]:
    path = Path(config_path)
    data = json.loads(path.read_text(encoding="utf-8"))
    requested = {p.strip().lower() for p in (players or set()) if p and p.strip()}
    out: list[PlayerProfile] = []
    for row in data.get("players", []):
        key = str(row.get("key", "")).strip()
        cluster = str(row.get("cluster", "core")).strip().lower() or "core"
        if not key:
            continue
        if cluster == "aggressive" and not include_aggressive:
            continue
        if requested and key.lower() not in requested:
            continue
        target = row.get("target", {})
        out.append(
            PlayerProfile(
                key=key,
                label=str(row.get("label", key)),
                cluster=cluster,
                target_profit_factor=_safe_float(target.get("profit_factor"), 0.0),
                target_drawdown_ratio=_safe_float(target.get("drawdown_percent"), 0.0) / 100.0,
                target_trades=max(1, _safe_int(target.get("trades"), 1)),
                target_avg_hold_hours=_safe_float(target.get("avg_hold_hours"), 0.0),
            )
        )
    return out


def _indicator_param_pairs(indicator_type: str, bucket: str) -> tuple[tuple[int, int], ...]:
    indicator = indicator_type.strip().lower()
    if indicator in ("sma", "ema"):
        return tuple((int(a), int(b)) for a, b in _MA_PAIRS[bucket] if int(a) < int(b))
    # Guardian currently drives RSI threshold with fixed 30/70 gates.
    if indicator == "rsi":
        return tuple((int(period), 50) for period in _RSI_PERIODS[bucket])
    # Guardian BB uses fixed deviation=2.0; keep sma_long canonical (20) to avoid duplicates.
    if indicator == "bb":
        return tuple((int(period), 20) for period in _BB_PERIODS[bucket])
    # Guardian MACD implementation is fixed 12/26 internally.
    if indicator == "macd":
        return ((12, 26),)
    # Guardian STOCH currently uses fixed 20/80 gates.
    if indicator == "stoch":
        return tuple((int(period), 20) for period in _STOCH_PERIODS[bucket])
    # Guardian VWAP uses rolling typical-price*volume / volume.
    if indicator == "vwap":
        return tuple((int(period), 0) for period in _VWAP_PERIODS[bucket])
    # Guardian VOLSMA interprets sma_long as spike threshold percent.
    if indicator == "volsma":
        return tuple(
            (int(period), int(spike_pct))
            for period in _VOLSMA_PERIODS[bucket]
            for spike_pct in _VOLSMA_SPIKE_PCTS[bucket]
        )
    # Guardian VPOC uses close-binned volume profile proxy.
    if indicator == "vpoc":
        return tuple(
            (int(period), int(bins))
            for period in _VPOC_PERIODS[bucket]
            for bins in _VPOC_BINS[bucket]
        )
    # Guardian VWAPVR interprets sma_long as orange threshold percent.
    if indicator == "vwapvr":
        return tuple(
            (int(period), int(threshold_pct))
            for period in _VWAPVR_PERIODS[bucket]
            for threshold_pct in _VWAPVR_THRESH_PCTS[bucket]
        )
    return ()


def build_candidate_pool(
    profile: PlayerProfile,
    *,
    candidates_per_player: int,
    seed: int,
    indicators: Optional[Iterable[str]] = None,
) -> list[CandidateSpec]:
    bucket = tempo_bucket(profile.target_trades)
    requested = tuple(indicators) if indicators else _INDICATORS
    candidates: list[CandidateSpec] = []
    for indicator in requested:
        param_pairs = _indicator_param_pairs(indicator, bucket)
        if not param_pairs:
            continue
        for tf in _TIMEFRAMES[bucket]:
            for short, long_p in param_pairs:
                if indicator in ("sma", "ema", "macd") and short >= long_p:
                    continue
                for sl in _SL_VALUES[bucket]:
                    for tp in _TP_VALUES[bucket]:
                        if tp <= sl:
                            continue
                        candidates.append(
                            CandidateSpec(
                                indicator_type=indicator,
                                sma_short=int(short),
                                sma_long=int(long_p),
                                timeframe=int(tf),
                                sl=round(float(sl), 3),
                                tp=round(float(tp), 3),
                            )
                        )

    # Deduplicate exact candidates (important for indicator families with canonical placeholders).
    candidates = list(dict.fromkeys(candidates))

    # Deterministic ordering per player and seed.
    rng = random.Random(f"{seed}:{profile.key}:{bucket}")
    rng.shuffle(candidates)
    limit = max(1, int(candidates_per_player))
    return candidates[:limit]


def normalize_backtest_response(raw) -> dict:
    if raw is None:
        return {}
    if isinstance(raw, dict):
        result = raw.get("result")
        if isinstance(result, dict):
            return result
        return raw
    if not isinstance(raw, str):
        return {}
    text = raw.strip()
    if not text:
        return {}
    if text.startswith("{"):
        try:
            obj = json.loads(text)
            if isinstance(obj, dict):
                result = obj.get("result")
                if isinstance(result, dict):
                    return result
                return obj
        except Exception:
            return {}
    try:
        parsed = parse_sexp_alist(text)
    except Exception:
        return {}
    if not isinstance(parsed, dict):
        return {}
    result = parsed.get("result")
    if isinstance(result, dict):
        return result
    return parsed


def compute_replica_score(profile: PlayerProfile, metrics: dict) -> float:
    pf = _safe_float(metrics.get("profit_factor"), 0.0)
    dd = _safe_float(metrics.get("max_drawdown"), 1.0)
    trades = max(0, _safe_int(metrics.get("trades"), 0))
    sharpe = _safe_float(metrics.get("sharpe"), 0.0)

    pf_dist = abs(pf - profile.target_profit_factor) / max(profile.target_profit_factor, 1e-6)
    dd_dist = abs(dd - profile.target_drawdown_ratio) / max(profile.target_drawdown_ratio, 1e-6)
    trades_dist = abs(trades - profile.target_trades) / max(profile.target_trades, 1)

    distance = 1.5 * pf_dist + 1.2 * dd_dist + 0.9 * trades_dist

    min_evidence = max(20, int(profile.target_trades * 0.10))
    if trades < min_evidence:
        distance += 0.8
    if pf < 1.0:
        distance += 0.6
    if dd > profile.target_drawdown_ratio * 1.8:
        distance += 0.6

    return -distance + 0.35 * max(-1.0, min(2.0, sharpe))


def compute_strength_score(metrics: dict) -> float:
    pf = _safe_float(metrics.get("profit_factor"), 0.0)
    dd = _safe_float(metrics.get("max_drawdown"), 1.0)
    trades = max(0, _safe_int(metrics.get("trades"), 0))
    sharpe = _safe_float(metrics.get("sharpe"), 0.0)
    return sharpe + 0.55 * max(0.0, pf - 1.0) + (trades / 2000.0) - 2.0 * dd


def core_bt_floor_pass(
    profile: PlayerProfile,
    metrics: dict,
    *,
    pf_floor: float = 1.20,
    sharpe_floor: float = 0.05,
    dd_cap: float = 0.12,
    trade_ratio_floor: float = 0.12,
) -> bool:
    if profile.cluster != "core":
        return True
    pf = _safe_float(metrics.get("profit_factor"), 0.0)
    sharpe = _safe_float(metrics.get("sharpe"), 0.0)
    dd = _safe_float(metrics.get("max_drawdown"), 1.0)
    trades = _safe_int(metrics.get("trades"), 0)
    min_trades = max(50, int(profile.target_trades * max(0.01, float(trade_ratio_floor))))
    return (
        pf >= float(pf_floor)
        and sharpe >= float(sharpe_floor)
        and dd <= float(dd_cap)
        and trades >= min_trades
    )


def compute_selection_score(
    profile: PlayerProfile,
    metrics: dict,
    *,
    strength_weight: float = 0.40,
    core_strength_penalty: float = 1.0,
    core_bt_pf_floor: float = 1.20,
    core_bt_sharpe_floor: float = 0.05,
    core_bt_dd_cap: float = 0.12,
    core_bt_trade_ratio_floor: float = 0.12,
) -> float:
    replica = compute_replica_score(profile, metrics)
    strength = compute_strength_score(metrics)
    score = replica + float(strength_weight) * strength
    if not core_bt_floor_pass(
        profile,
        metrics,
        pf_floor=core_bt_pf_floor,
        sharpe_floor=core_bt_sharpe_floor,
        dd_cap=core_bt_dd_cap,
        trade_ratio_floor=core_bt_trade_ratio_floor,
    ):
        score -= float(core_strength_penalty)
    return score


def compute_cpcv_summary(
    fold_metrics: Iterable[dict],
    *,
    min_fold_trades: int = 20,
    pf_floor: float = 1.05,
    sharpe_floor: float = 0.0,
) -> dict:
    rows = list(fold_metrics or [])
    valid = [m for m in rows if _safe_int(m.get("trades"), 0) >= max(1, int(min_fold_trades))]
    passed = [
        m
        for m in valid
        if _safe_float(m.get("profit_factor"), 0.0) >= float(pf_floor)
        and _safe_float(m.get("sharpe"), 0.0) >= float(sharpe_floor)
    ]
    pf_vals = [_safe_float(m.get("profit_factor"), 0.0) for m in valid]
    sharpe_vals = [_safe_float(m.get("sharpe"), 0.0) for m in valid]
    trades_vals = [_safe_int(m.get("trades"), 0) for m in valid]
    valid_count = len(valid)
    pass_rate = (len(passed) / valid_count) if valid_count > 0 else 0.0
    return {
        "folds": len(rows),
        "valid_folds": valid_count,
        "passed_folds": len(passed),
        "pass_rate": pass_rate,
        "median_profit_factor": float(median(pf_vals)) if pf_vals else 0.0,
        "median_sharpe": float(median(sharpe_vals)) if sharpe_vals else 0.0,
        "median_trades": int(median(trades_vals)) if trades_vals else 0,
    }


def cpcv_is_strong(
    summary: Optional[dict],
    *,
    min_valid_folds: int = 3,
    pass_rate_min: float = 0.60,
    median_pf_min: float = 1.05,
    median_sharpe_min: float = 0.0,
) -> bool:
    if not isinstance(summary, dict):
        return False
    valid_folds = _safe_int(summary.get("valid_folds"), 0)
    pass_rate = _safe_float(summary.get("pass_rate"), 0.0)
    median_pf = _safe_float(summary.get("median_profit_factor"), 0.0)
    median_sharpe = _safe_float(summary.get("median_sharpe"), 0.0)
    return (
        valid_folds >= max(1, int(min_valid_folds))
        and pass_rate >= float(pass_rate_min)
        and median_pf >= float(median_pf_min)
        and median_sharpe >= float(median_sharpe_min)
    )


def _strategy_name(profile: PlayerProfile, candidate: CandidateSpec, *, seed: int, index: int) -> str:
    raw = (
        f"{profile.key}|{candidate.indicator_type}|{candidate.sma_short}|{candidate.sma_long}|"
        f"{candidate.timeframe}|{candidate.sl}|{candidate.tp}|{seed}|{index}"
    )
    digest = hashlib.sha1(raw.encode("utf-8")).hexdigest()[:8]
    return (
        f"USR-ARMADA-{profile.key.upper()}-{candidate.indicator_type.upper()}"
        f"{candidate.sma_short}-{candidate.sma_long}-TF{candidate.timeframe}-{digest}"
    )


def _evaluate_candidate(
    service: BacktestService,
    *,
    profile: PlayerProfile,
    candidate: CandidateSpec,
    candles_file: Path,
    symbol: str,
    seed: int,
    index: int,
    start_date: Optional[str] = None,
    end_date: Optional[str] = None,
) -> dict:
    strategy_name = _strategy_name(profile, candidate, seed=seed, index=index)
    request_id = f"RID-ARMADA-{int(time.time())}-{index}"
    payload = {
        "action": "BACKTEST",
        "request_id": request_id,
        "strategy": {
            "name": strategy_name,
            "sma_short": candidate.sma_short,
            "sma_long": candidate.sma_long,
            "sl": candidate.sl,
            "tp": candidate.tp,
            "volume": 0.01,
            "indicator_type": candidate.indicator_type,
            "filter_enabled": False,
        },
        "candles_file": str(candles_file),
        "symbol": symbol,
        "timeframe": candidate.timeframe,
    }
    if start_date:
        payload["from_date"] = start_date
    if end_date:
        payload["to_date"] = end_date

    raw = service.run_backtest(payload)
    metrics = normalize_backtest_response(raw)
    if not metrics:
        return {}

    out = {
        "strategy_name": str(metrics.get("strategy_name", strategy_name)),
        "sharpe": _safe_float(metrics.get("sharpe"), 0.0),
        "profit_factor": _safe_float(metrics.get("profit_factor"), 0.0),
        "max_drawdown": _safe_float(metrics.get("max_drawdown"), 1.0),
        "trades": _safe_int(metrics.get("trades"), 0),
        "win_rate": _safe_float(metrics.get("win_rate"), 0.0),
        "pnl": _safe_float(metrics.get("pnl"), 0.0),
        "request_id": str(metrics.get("request_id", request_id)),
    }
    if metrics.get("error"):
        out["error"] = str(metrics.get("error"))
    return out


def _count_data_rows(csv_path: Path) -> int:
    rows = 0
    with csv_path.open("r", encoding="utf-8") as fh:
        _ = fh.readline()
        for _line in fh:
            rows += 1
    return rows


def build_walkforward_windows(
    *,
    total_rows: int,
    folds: int,
    test_ratio: float,
    min_test_rows: int = 200,
) -> list[tuple[int, int]]:
    rows = max(0, int(total_rows))
    k = max(0, int(folds))
    if rows <= 0 or k <= 0:
        return []
    test_rows = max(1, int(rows * max(0.05, min(0.5, float(test_ratio)))))
    test_rows = max(test_rows, max(1, int(min_test_rows)))
    if rows < test_rows * 2:
        return []

    max_start = rows - test_rows
    if k == 1:
        return [(max_start, rows)]

    starts = [int(round(i * max_start / (k - 1))) for i in range(k)]
    windows: list[tuple[int, int]] = []
    last = None
    for start in starts:
        if start == last:
            continue
        end = min(rows, start + test_rows)
        windows.append((start, end))
        last = start
    return windows


def build_oos_tail_csv(candles_file: Path, ratio: float) -> Path:
    total_rows = _count_data_rows(candles_file)
    keep_rows = max(200, int(total_rows * max(0.05, min(0.5, ratio))))
    tail: deque[str] = deque(maxlen=keep_rows)
    with candles_file.open("r", encoding="utf-8") as fh:
        header = fh.readline().rstrip("\n")
        for line in fh:
            tail.append(line.rstrip("\n"))
    tmp = Path(tempfile.mkstemp(prefix="armada_oos_", suffix=".csv")[1])
    with tmp.open("w", encoding="utf-8") as out:
        out.write(header + "\n")
        for line in tail:
            out.write(line + "\n")
    return tmp


def _write_window_csv(candles_file: Path, start_row: int, end_row: int, *, prefix: str) -> Path:
    tmp = Path(tempfile.mkstemp(prefix=prefix, suffix=".csv")[1])
    start_idx = max(0, int(start_row))
    end_idx = max(start_idx, int(end_row))
    with candles_file.open("r", encoding="utf-8") as src, tmp.open("w", encoding="utf-8") as out:
        header = src.readline().rstrip("\n")
        out.write(header + "\n")
        for idx, line in enumerate(src):
            if idx < start_idx:
                continue
            if idx >= end_idx:
                break
            out.write(line if line.endswith("\n") else (line + "\n"))
    return tmp


def build_cpcv_fold_files(
    candles_file: Path,
    *,
    folds: int,
    test_ratio: float,
    min_test_rows: int = 200,
) -> list[Path]:
    total_rows = _count_data_rows(candles_file)
    windows = build_walkforward_windows(
        total_rows=total_rows,
        folds=folds,
        test_ratio=test_ratio,
        min_test_rows=min_test_rows,
    )
    files: list[Path] = []
    for fold_idx, (start_row, end_row) in enumerate(windows):
        files.append(
            _write_window_csv(
                candles_file,
                start_row,
                end_row,
                prefix=f"armada_cpcv_fold{fold_idx}_",
            )
        )
    return files


def run_cpcv_lite(
    service: BacktestService,
    *,
    profile: PlayerProfile,
    candidate: CandidateSpec,
    fold_files: Iterable[Path],
    symbol: str,
    seed: int,
    min_fold_trades: int,
    pf_floor: float,
    sharpe_floor: float,
) -> dict:
    fold_metrics: list[dict] = []
    for fold_idx, fold_file in enumerate(fold_files):
        metrics = _evaluate_candidate(
            service,
            profile=profile,
            candidate=candidate,
            candles_file=fold_file,
            symbol=symbol,
            seed=seed + 3111,
            index=fold_idx,
        )
        if not metrics or metrics.get("error"):
            continue
        fold_metrics.append(
            {
                "fold": fold_idx,
                "profit_factor": _safe_float(metrics.get("profit_factor"), 0.0),
                "sharpe": _safe_float(metrics.get("sharpe"), 0.0),
                "trades": _safe_int(metrics.get("trades"), 0),
                "max_drawdown": _safe_float(metrics.get("max_drawdown"), 1.0),
            }
        )
    summary = compute_cpcv_summary(
        fold_metrics,
        min_fold_trades=min_fold_trades,
        pf_floor=pf_floor,
        sharpe_floor=sharpe_floor,
    )
    summary["fold_metrics"] = fold_metrics
    return summary


def _strength_verdict(
    profile: PlayerProfile,
    bt: dict,
    oos: Optional[dict],
    cpcv_summary: Optional[dict] = None,
    *,
    require_cpcv_for_core: bool = False,
) -> dict:
    bt_pf = _safe_float(bt.get("profit_factor"), 0.0)
    bt_sh = _safe_float(bt.get("sharpe"), 0.0)
    bt_dd = _safe_float(bt.get("max_drawdown"), 1.0)
    bt_tr = _safe_int(bt.get("trades"), 0)

    min_trades = max(50, int(profile.target_trades * 0.12))
    bt_ok = bt_pf >= 1.30 and bt_sh >= 0.10 and bt_dd <= 0.12 and bt_tr >= min_trades

    cpcv_ok = cpcv_is_strong(cpcv_summary) if cpcv_summary is not None else None

    if not oos:
        cpcv_required = bool(require_cpcv_for_core and profile.cluster == "core")
        return {
            "bt_ok": bt_ok,
            "oos_ok": None,
            "cpcv_ok": cpcv_ok,
            "is_strong": bt_ok and ((cpcv_ok is True) if cpcv_required else True),
            "reason": "bt-only+cpcv" if cpcv_required else "bt-only",
        }

    oos_pf = _safe_float(oos.get("profit_factor"), 0.0)
    oos_sh = _safe_float(oos.get("sharpe"), 0.0)
    oos_tr = _safe_int(oos.get("trades"), 0)
    oos_ok = oos_pf >= 1.10 and oos_sh >= 0.0 and oos_tr >= max(15, int(min_trades * 0.2))
    cpcv_required = bool(require_cpcv_for_core and profile.cluster == "core")
    strong = bt_ok and oos_ok and ((cpcv_ok is True) if cpcv_required else True)
    return {
        "bt_ok": bt_ok,
        "oos_ok": oos_ok,
        "cpcv_ok": cpcv_ok,
        "is_strong": strong,
        "reason": "bt+oos+cpcv" if cpcv_required else "bt+oos",
    }


def _split_players(raw: str) -> set[str]:
    if not raw or not raw.strip():
        return set()
    return {token.strip().lower() for token in raw.split(",") if token.strip()}


def _cleanup_service(service: BacktestService) -> None:
    try:
        if hasattr(service, "_cleanup_guardian_processes"):
            service._cleanup_guardian_processes()
    except Exception:
        pass


def run_replica_search(args: argparse.Namespace) -> int:
    config_path = Path(args.profiles).resolve()
    candles_file = Path(args.candles).resolve()
    if not config_path.exists():
        raise FileNotFoundError(f"profiles config not found: {config_path}")
    if not candles_file.exists():
        raise FileNotFoundError(f"candles file not found: {candles_file}")

    selected_players = _split_players(args.players)
    indicator_filter = parse_indicator_filter(args.indicators)
    profiles = load_profiles(
        config_path,
        include_aggressive=bool(args.include_aggressive),
        players=selected_players or None,
    )
    if not profiles:
        raise RuntimeError("no matching player profiles")

    oos_file = None
    if not args.no_oos:
        oos_file = build_oos_tail_csv(candles_file, args.oos_ratio)
    cpcv_fold_files: list[Path] = []
    if args.cpcv_folds > 0:
        cpcv_fold_files = build_cpcv_fold_files(
            candles_file,
            folds=args.cpcv_folds,
            test_ratio=args.cpcv_test_ratio,
            min_test_rows=args.cpcv_min_test_rows,
        )

    started_at = int(time.time())
    report = {
        "generated_at": started_at,
        "symbol": args.symbol,
        "profiles_file": str(config_path),
        "candles_file": str(candles_file),
        "oos_file": str(oos_file) if oos_file else None,
        "cpcv": {
            "enabled": len(cpcv_fold_files) > 0,
            "folds": len(cpcv_fold_files),
            "test_ratio": args.cpcv_test_ratio,
            "min_test_rows": args.cpcv_min_test_rows,
            "min_fold_trades": args.cpcv_min_fold_trades,
            "pf_floor": args.cpcv_pf_floor,
            "sharpe_floor": args.cpcv_sharpe_floor,
            "require_for_core": bool(args.cpcv_require_for_core),
        },
        "selection": {
            "strength_weight": args.selection_strength_weight,
            "core_bt_pf_floor": args.core_bt_pf_floor,
            "core_bt_sharpe_floor": args.core_bt_sharpe_floor,
            "core_bt_dd_cap": args.core_bt_dd_cap,
            "core_bt_trade_ratio_floor": args.core_bt_trade_ratio_floor,
            "core_strength_penalty": args.core_strength_penalty,
        },
        "players": [],
    }

    service = BacktestService(use_zmq=False)
    try:
        for profile in profiles:
            pool = build_candidate_pool(
                profile,
                candidates_per_player=args.candidates_per_player,
                seed=args.seed,
                indicators=indicator_filter or None,
            )
            ranked: list[dict] = []
            for idx, candidate in enumerate(pool):
                bt_metrics = _evaluate_candidate(
                    service,
                    profile=profile,
                    candidate=candidate,
                    candles_file=candles_file,
                    symbol=args.symbol,
                    seed=args.seed,
                    index=idx,
                )
                if not bt_metrics or bt_metrics.get("error"):
                    continue
                replica_score = compute_replica_score(profile, bt_metrics)
                strength_score = compute_strength_score(bt_metrics)
                selection_score = compute_selection_score(
                    profile,
                    bt_metrics,
                    strength_weight=args.selection_strength_weight,
                    core_strength_penalty=args.core_strength_penalty,
                    core_bt_pf_floor=args.core_bt_pf_floor,
                    core_bt_sharpe_floor=args.core_bt_sharpe_floor,
                    core_bt_dd_cap=args.core_bt_dd_cap,
                    core_bt_trade_ratio_floor=args.core_bt_trade_ratio_floor,
                )
                ranked.append(
                    {
                        "candidate": asdict(candidate),
                        "bt_metrics": bt_metrics,
                        "replica_score": replica_score,
                        "strength_score": strength_score,
                        "selection_score": selection_score,
                    }
                )

            ranked.sort(
                key=lambda item: (
                    item.get("selection_score", item["replica_score"]),
                    item["replica_score"],
                ),
                reverse=True,
            )
            selected: list[dict] = []
            for pick_idx, item in enumerate(ranked[: max(1, args.top_per_player)]):
                oos_metrics = None
                if oos_file:
                    oos_metrics = _evaluate_candidate(
                        service,
                        profile=profile,
                        candidate=CandidateSpec(**item["candidate"]),
                        candles_file=oos_file,
                        symbol=args.symbol,
                        seed=args.seed + 997,
                        index=pick_idx,
                    )
                    if oos_metrics and oos_metrics.get("error"):
                        oos_metrics = None
                cpcv_summary = None
                if cpcv_fold_files:
                    cpcv_summary = run_cpcv_lite(
                        service,
                        profile=profile,
                        candidate=CandidateSpec(**item["candidate"]),
                        fold_files=cpcv_fold_files,
                        symbol=args.symbol,
                        seed=args.seed + 2113,
                        min_fold_trades=args.cpcv_min_fold_trades,
                        pf_floor=args.cpcv_pf_floor,
                        sharpe_floor=args.cpcv_sharpe_floor,
                    )
                verdict = _strength_verdict(
                    profile,
                    item["bt_metrics"],
                    oos_metrics,
                    cpcv_summary,
                    require_cpcv_for_core=bool(args.cpcv_require_for_core),
                )
                item["oos_metrics"] = oos_metrics
                item["cpcv_summary"] = cpcv_summary
                item["verdict"] = verdict
                selected.append(item)

            best = selected[0] if selected else None
            print(
                f"[{profile.key}] candidates={len(ranked)} "
                f"best_replica={best['replica_score']:.4f}" if best else f"[{profile.key}] no viable candidates"
            )

            report["players"].append(
                {
                    "profile": asdict(profile),
                    "evaluated_candidates": len(ranked),
                    "selected": selected,
                }
            )
    finally:
        _cleanup_service(service)
        if oos_file and not args.keep_oos_file:
            try:
                oos_file.unlink(missing_ok=True)
            except Exception:
                pass
        for fold_file in cpcv_fold_files:
            try:
                fold_file.unlink(missing_ok=True)
            except Exception:
                pass

    output_path = Path(args.output)
    if not output_path.is_absolute():
        output_path = ROOT / output_path
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(report, indent=2, ensure_ascii=False), encoding="utf-8")
    print(f"saved report: {output_path}")
    return 0


def parse_args(argv: Optional[Iterable[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Armada player-centric replica search")
    parser.add_argument(
        "--profiles",
        default=str(ROOT / "tools" / "configs" / "armada_player_profiles_v1.json"),
        help="player profile config JSON",
    )
    parser.add_argument(
        "--candles",
        default=str(ROOT / "data" / "historical" / "USDJPY_M1.csv"),
        help="input candles csv",
    )
    parser.add_argument("--symbol", default="USDJPY", help="symbol label")
    parser.add_argument(
        "--players",
        default="",
        help="comma separated player keys (default: all non-aggressive)",
    )
    parser.add_argument(
        "--include-aggressive",
        action="store_true",
        help="include aggressive cluster players",
    )
    parser.add_argument(
        "--candidates-per-player",
        type=int,
        default=24,
        help="candidate count evaluated per player",
    )
    parser.add_argument("--top-per-player", type=int, default=1, help="top candidates to keep per player")
    parser.add_argument("--seed", type=int, default=20260220, help="deterministic seed")
    parser.add_argument(
        "--indicators",
        default="",
        help="comma separated indicator types to include (e.g. vwapvr,volsma)",
    )
    parser.add_argument(
        "--selection-strength-weight",
        type=float,
        default=0.40,
        help="weight of strength score in selection ranking",
    )
    parser.add_argument(
        "--core-bt-pf-floor",
        type=float,
        default=1.20,
        help="core-only BT PF floor for selection penalty",
    )
    parser.add_argument(
        "--core-bt-sharpe-floor",
        type=float,
        default=0.05,
        help="core-only BT Sharpe floor for selection penalty",
    )
    parser.add_argument(
        "--core-bt-dd-cap",
        type=float,
        default=0.12,
        help="core-only BT DD cap for selection penalty",
    )
    parser.add_argument(
        "--core-bt-trade-ratio-floor",
        type=float,
        default=0.12,
        help="core-only minimum trade evidence ratio vs target trades",
    )
    parser.add_argument(
        "--core-strength-penalty",
        type=float,
        default=1.0,
        help="penalty applied to selection score when core BT floor is not met",
    )
    parser.add_argument("--no-oos", action="store_true", help="skip OOS tail evaluation")
    parser.add_argument("--oos-ratio", type=float, default=0.2, help="OOS tail ratio from latest data")
    parser.add_argument(
        "--cpcv-folds",
        type=int,
        default=0,
        help="enable CPCV-lite fold evaluation for selected candidates (0=disabled)",
    )
    parser.add_argument(
        "--cpcv-test-ratio",
        type=float,
        default=0.2,
        help="CPCV-lite fold test window ratio",
    )
    parser.add_argument(
        "--cpcv-min-test-rows",
        type=int,
        default=200,
        help="minimum rows per CPCV-lite fold",
    )
    parser.add_argument(
        "--cpcv-min-fold-trades",
        type=int,
        default=20,
        help="minimum trades required for each fold to count in pass_rate",
    )
    parser.add_argument(
        "--cpcv-pf-floor",
        type=float,
        default=1.05,
        help="minimum fold PF to count as pass",
    )
    parser.add_argument(
        "--cpcv-sharpe-floor",
        type=float,
        default=0.0,
        help="minimum fold Sharpe to count as pass",
    )
    parser.add_argument(
        "--cpcv-require-for-core",
        action="store_true",
        help="require CPCV-lite pass for strong=True on core players",
    )
    parser.add_argument(
        "--keep-oos-file",
        action="store_true",
        help="keep generated OOS tail file",
    )
    parser.add_argument(
        "--output",
        default="data/reports/armada_player_replica_latest.json",
        help="output report json",
    )
    return parser.parse_args(list(argv) if argv is not None else None)


def main(argv: Optional[Iterable[str]] = None) -> int:
    args = parse_args(argv)
    return run_replica_search(args)


if __name__ == "__main__":
    raise SystemExit(main())
