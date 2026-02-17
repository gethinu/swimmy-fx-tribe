#!/usr/bin/env python3
"""Periodic calibration for Pattern Similarity backend ensemble weight.

Runs rolling offline comparisons between:
- clip-vit-b32
- vector-siamese-v1

Writes:
- calibration report JSON
- runtime ensemble weight JSON (vector_weight in [0,1])
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Mapping, Sequence, Tuple

try:
    import numpy as np
except Exception:  # pragma: no cover
    np = None

try:
    import torch  # type: ignore
except Exception:  # pragma: no cover
    torch = None


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
if str(BASE_DIR / "tools") not in sys.path:
    sys.path.insert(0, str(BASE_DIR / "tools"))

import pattern_similarity_service as svc  # noqa: E402


DEFAULT_TIMEFRAMES: Tuple[str, ...] = ("M15", "H1", "H4", "D1", "W1", "MN1")
DEFAULT_LOOKBACK_BARS: Dict[str, int] = {
    "M15": 7000,
    "H1": 4500,
    "H4": 2800,
    "D1": 1400,
    "W1": 700,
    "MN1": 300,
}


def normalize_multi_values(raw: Sequence[str], *, defaults: Sequence[str]) -> List[str]:
    out: List[str] = []
    seen: set[str] = set()
    for item in raw or ():
        for part in str(item or "").split(","):
            name = str(part or "").strip().upper()
            if not name or name in seen:
                continue
            seen.add(name)
            out.append(name)
    if out:
        return out
    for item in defaults or ():
        name = str(item or "").strip().upper()
        if not name or name in seen:
            continue
        seen.add(name)
        out.append(name)
    return out


def discover_symbols() -> List[str]:
    symbols = sorted({p.parent.parent.name.upper() for p in (BASE_DIR / "data" / "patterns").glob("*/*/patterns.npz")})
    return [s for s in symbols if s]


def build_weight_grid(step: float) -> List[float]:
    try:
        s = float(step)
    except Exception:
        s = 0.05
    s = max(0.01, min(1.0, s))
    vals: List[float] = []
    x = 0.0
    while x < 1.0:
        vals.append(round(x, 6))
        x += s
    vals.append(1.0)
    out: List[float] = []
    seen: set[float] = set()
    for v in vals:
        vv = max(0.0, min(1.0, float(v)))
        if vv in seen:
            continue
        seen.add(vv)
        out.append(vv)
    return out


def pick_label(probs: Mapping[str, Any]) -> str:
    pu = float(probs.get("p_up", 0.0) or 0.0)
    pd = float(probs.get("p_down", 0.0) or 0.0)
    pf = float(probs.get("p_flat", 0.0) or 0.0)
    if pu >= pd and pu >= pf:
        return "UP"
    if pd >= pu and pd >= pf:
        return "DOWN"
    return "FLAT"


def _metrics(preds: Sequence[str], truths: Sequence[str]) -> Dict[str, Any]:
    if not truths:
        return {"accuracy": 0.0, "directional_accuracy": None, "directional_n": 0}
    acc = sum(int(p == y) for p, y in zip(preds, truths)) / float(len(truths))
    idx = [i for i, y in enumerate(truths) if y != "FLAT"]
    if idx:
        dacc = sum(int(preds[i] == truths[i]) for i in idx) / float(len(idx))
    else:
        dacc = None
    return {
        "accuracy": round(float(acc), 6),
        "directional_accuracy": None if dacc is None else round(float(dacc), 6),
        "directional_n": int(len(idx)),
    }


def _blend(clip_probs: Mapping[str, Any], vector_probs: Mapping[str, Any], vector_weight: float) -> Dict[str, float]:
    w = max(0.0, min(1.0, float(vector_weight)))
    wc = 1.0 - w
    return {
        "p_up": wc * float(clip_probs.get("p_up", 0.0) or 0.0) + w * float(vector_probs.get("p_up", 0.0) or 0.0),
        "p_down": wc * float(clip_probs.get("p_down", 0.0) or 0.0) + w * float(vector_probs.get("p_down", 0.0) or 0.0),
        "p_flat": wc * float(clip_probs.get("p_flat", 0.0) or 0.0) + w * float(vector_probs.get("p_flat", 0.0) or 0.0),
    }


def choose_best_weight(eval_rows: Sequence[Tuple[str, Mapping[str, Any], Mapping[str, Any]]], weight_grid: Sequence[float]) -> Dict[str, Any]:
    best_weight = None
    best_dir = -1.0
    best_acc = -1.0
    for w in weight_grid:
        preds: List[str] = []
        truths: List[str] = []
        for truth, clip_probs, vector_probs in eval_rows:
            preds.append(pick_label(_blend(clip_probs, vector_probs, w)))
            truths.append(str(truth))
        metrics = _metrics(preds, truths)
        dacc = metrics["directional_accuracy"]
        dscore = -1.0 if dacc is None else float(dacc)
        ascore = float(metrics["accuracy"])
        if dscore > best_dir or (dscore == best_dir and ascore > best_acc):
            best_dir = dscore
            best_acc = ascore
            best_weight = float(w)
    return {
        "best_weight": 0.0 if best_weight is None else round(float(best_weight), 6),
        "best_accuracy": round(best_acc if best_acc >= 0.0 else 0.0, 6),
        "best_directional_accuracy": None if best_dir < 0.0 else round(best_dir, 6),
    }


def _uniform_sample(items: List[Any], max_samples: int) -> List[Any]:
    if len(items) <= max_samples:
        return items
    step = len(items) / float(max_samples)
    out: List[Any] = []
    x = 0.0
    while int(x) < len(items) and len(out) < max_samples:
        out.append(items[int(x)])
        x += step
    return out


def _knn_probs(train_x: "np.ndarray", train_y: Sequence[str], x: "np.ndarray", k: int) -> Dict[str, float]:
    sims = train_x @ x
    kk = max(1, min(int(k), len(train_y)))
    idx = np.argpartition(-sims, kk - 1)[:kk]
    idx = idx[np.argsort(-sims[idx])]
    dists = np.maximum(0.0, 1.0 - sims[idx]).astype(np.float32)
    labels = [str(train_y[int(i)]) for i in idx.tolist()]
    probs, _ = svc._distance_weighted_probs(labels, [float(d) for d in dists.tolist()])
    return {
        "p_up": float(probs.get("p_up", 0.0)),
        "p_down": float(probs.get("p_down", 0.0)),
        "p_flat": float(probs.get("p_flat", 0.0)),
    }


def _embed_dataset(model: str, dataset: Sequence[Tuple[List[Dict[str, Any]], str]]) -> Tuple["np.ndarray", List[str]]:
    vecs: List["np.ndarray"] = []
    labels: List[str] = []
    for candles, label in dataset:
        _backend, vec = svc._embed_for_model(model, candles)
        vecs.append(vec.astype(np.float32))
        labels.append(str(label))
    x = np.stack(vecs, axis=0).astype(np.float32)
    norms = np.linalg.norm(x, axis=1, keepdims=True)
    norms[norms == 0] = 1.0
    x = x / norms
    return x, labels


def _collect_labeled_windows(symbol: str, timeframe: str, *, lookback_bars: int, max_samples: int) -> List[Tuple[List[Dict[str, Any]], str]]:
    candles = svc._load_candles(symbol, timeframe)
    if not candles:
        return []
    window = svc.WINDOW_BARS.get(timeframe)
    horizon = svc.HORIZON_BARS.get(timeframe)
    if window is None or horizon is None:
        return []

    start_i = max(window - 1, len(candles) - int(lookback_bars))
    end_i = len(candles) - horizon
    stride = max(1, int(svc.STRIDE_BARS.get(timeframe, 1)))
    samples: List[Tuple[List[Dict[str, Any]], str]] = []
    for end_idx in range(start_i, end_i, stride):
        win = candles[end_idx - window + 1 : end_idx + 1]
        atr = svc._atr(win, svc.LABEL_ATR_PERIOD)
        if atr is None or atr <= 0:
            continue
        cur = svc._coerce_float(win[-1].get("close"))
        fut = svc._coerce_float(candles[end_idx + horizon].get("close"))
        if cur is None or fut is None:
            continue
        label = svc._label_from_future_return(cur, fut, atr)
        samples.append((win, label))
    return _uniform_sample(samples, max_samples)


def _write_json(path: Path, payload: Mapping[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(json.dumps(payload, ensure_ascii=True, indent=2), encoding="utf-8")
    tmp.replace(path)


def extract_symbol_timeframe_weights(combo_results: Sequence[Mapping[str, Any]]) -> Dict[str, float]:
    out: Dict[str, float] = {}
    for row in combo_results:
        sym = str(row.get("symbol") or "").strip().upper()
        tf = str(row.get("timeframe") or "").strip().upper()
        if not sym or not tf:
            continue
        try:
            w = float(row.get("best_vector_weight"))
        except Exception:
            continue
        if w < 0.0 or w > 1.0:
            continue
        out[f"{sym}:{tf}"] = round(float(w), 6)
    return dict(sorted(out.items()))


def write_weight_file(
    path: Path,
    *,
    vector_weight: float,
    source_report: Path,
    generated_at: str,
    symbol_timeframe_weights: Mapping[str, Any] | None = None,
) -> Dict[str, Any]:
    w = max(0.0, min(1.0, float(vector_weight)))
    st_weights: Dict[str, float] = {}
    if isinstance(symbol_timeframe_weights, Mapping):
        for key, value in symbol_timeframe_weights.items():
            key_text = str(key or "").strip().upper()
            if ":" not in key_text:
                continue
            try:
                vv = float(value)
            except Exception:
                continue
            if vv < 0.0 or vv > 1.0:
                continue
            st_weights[key_text] = round(float(vv), 6)
    payload = {
        "schema_version": 1,
        "generated_at": generated_at,
        "vector_weight": round(w, 6),
        "clip_weight": round(1.0 - w, 6),
        "source_report": str(source_report),
    }
    if st_weights:
        payload["symbol_timeframe_weights"] = dict(sorted(st_weights.items()))
    _write_json(path, payload)
    return payload


def run_calibration(
    *,
    symbols: Sequence[str],
    timeframes: Sequence[str],
    k: int,
    max_samples: int,
    min_samples: int,
    train_ratio: float,
    weight_grid: Sequence[float],
) -> Dict[str, Any]:
    if np is None:
        raise RuntimeError("numpy not available")

    combos: List[Dict[str, Any]] = []
    global_eval_rows: List[Tuple[str, Mapping[str, Any], Mapping[str, Any]]] = []

    for symbol in symbols:
        for tf in timeframes:
            samples = _collect_labeled_windows(
                symbol,
                tf,
                lookback_bars=int(DEFAULT_LOOKBACK_BARS.get(tf, 3000)),
                max_samples=int(max_samples),
            )
            if len(samples) < int(min_samples):
                continue

            split = int(len(samples) * float(train_ratio))
            split = max(20, min(split, len(samples) - 20))
            train = samples[:split]
            test = samples[split:]
            if len(test) < 20:
                continue

            try:
                clip_train_x, clip_train_y = _embed_dataset("clip-vit-b32", train)
                clip_test_x, _clip_test_y = _embed_dataset("clip-vit-b32", test)
                vec_train_x, vec_train_y = _embed_dataset("vector-siamese-v1", train)
                vec_test_x, _vec_test_y = _embed_dataset("vector-siamese-v1", test)
            except Exception:
                continue

            truths = [label for _candles, label in test]
            clip_probs_list: List[Dict[str, float]] = []
            vector_probs_list: List[Dict[str, float]] = []
            clip_preds: List[str] = []
            vector_preds: List[str] = []

            for i in range(len(test)):
                cp = _knn_probs(clip_train_x, clip_train_y, clip_test_x[i], int(k))
                vp = _knn_probs(vec_train_x, vec_train_y, vec_test_x[i], int(k))
                clip_probs_list.append(cp)
                vector_probs_list.append(vp)
                clip_preds.append(pick_label(cp))
                vector_preds.append(pick_label(vp))
                global_eval_rows.append((truths[i], cp, vp))

            combo_rows = [(truths[i], clip_probs_list[i], vector_probs_list[i]) for i in range(len(truths))]
            best = choose_best_weight(combo_rows, weight_grid)
            label_dist = dict(Counter([label for _candles, label in samples]))
            combos.append(
                {
                    "symbol": symbol,
                    "timeframe": tf,
                    "train_n": int(len(train)),
                    "test_n": int(len(test)),
                    "label_dist": label_dist,
                    "clip": _metrics(clip_preds, truths),
                    "vector": _metrics(vector_preds, truths),
                    "best_vector_weight": best["best_weight"],
                    "best_mix_accuracy": best["best_accuracy"],
                    "best_mix_directional_accuracy": best["best_directional_accuracy"],
                }
            )

    global_best = choose_best_weight(global_eval_rows, weight_grid) if global_eval_rows else {
        "best_weight": 0.0,
        "best_accuracy": 0.0,
        "best_directional_accuracy": None,
    }
    clip_avg = [c["clip"]["directional_accuracy"] for c in combos if c["clip"]["directional_accuracy"] is not None]
    vec_avg = [c["vector"]["directional_accuracy"] for c in combos if c["vector"]["directional_accuracy"] is not None]

    return {
        "schema_version": 1,
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "config": {
            "k": int(k),
            "max_samples": int(max_samples),
            "min_samples": int(min_samples),
            "train_ratio": float(train_ratio),
            "weight_grid": list(weight_grid),
            "symbols": list(symbols),
            "timeframes": list(timeframes),
        },
        "status": {
            "ok": bool(len(combos) > 0),
            "combos_evaluated": int(len(combos)),
        },
        "global": {
            "best_vector_weight": global_best["best_weight"],
            "best_mix_accuracy": global_best["best_accuracy"],
            "best_mix_directional_accuracy": global_best["best_directional_accuracy"],
            "avg_clip_directional_accuracy": None if not clip_avg else round(float(sum(clip_avg) / len(clip_avg)), 6),
            "avg_vector_directional_accuracy": None if not vec_avg else round(float(sum(vec_avg) / len(vec_avg)), 6),
        },
        "combo_results": combos,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--symbol", action="append", default=[], help="Target symbol(s), repeatable/comma-separated.")
    parser.add_argument("--timeframe", action="append", default=[], help="Target TF(s), repeatable/comma-separated.")
    parser.add_argument("--k", type=int, default=9)
    parser.add_argument("--max-samples", type=int, default=120)
    parser.add_argument("--min-samples", type=int, default=60)
    parser.add_argument("--train-ratio", type=float, default=0.70)
    parser.add_argument("--weight-step", type=float, default=0.05)
    parser.add_argument("--torch-threads", type=int, default=4)
    parser.add_argument(
        "--write-report",
        default=str(BASE_DIR / "data" / "reports" / "pattern_backend_calibration_latest.json"),
    )
    parser.add_argument(
        "--write-weight",
        default=str(BASE_DIR / "data" / "patterns" / "models" / "ensemble_weight.json"),
    )
    args = parser.parse_args()

    if torch is not None:
        try:
            torch.set_num_threads(max(1, int(args.torch_threads)))
        except Exception:
            pass

    symbols = normalize_multi_values(args.symbol or [], defaults=discover_symbols())
    timeframes = normalize_multi_values(args.timeframe or [], defaults=DEFAULT_TIMEFRAMES)
    timeframes = [tf for tf in timeframes if tf in DEFAULT_TIMEFRAMES]
    weight_grid = build_weight_grid(float(args.weight_step))

    summary = run_calibration(
        symbols=symbols,
        timeframes=timeframes,
        k=max(1, int(args.k)),
        max_samples=max(40, int(args.max_samples)),
        min_samples=max(20, int(args.min_samples)),
        train_ratio=max(0.50, min(0.90, float(args.train_ratio))),
        weight_grid=weight_grid,
    )

    report_path = Path(str(args.write_report)).expanduser()
    _write_json(report_path, summary)

    if str(args.write_weight or "").strip():
        weight_path = Path(str(args.write_weight)).expanduser()
        generated_at = str(summary.get("generated_at") or datetime.now(timezone.utc).isoformat())
        best_weight = float((summary.get("global") or {}).get("best_vector_weight") or 0.0)
        combo_results = summary.get("combo_results")
        by_symbol_tf = (
            extract_symbol_timeframe_weights(combo_results)
            if isinstance(combo_results, list)
            else {}
        )
        payload = write_weight_file(
            weight_path,
            vector_weight=best_weight,
            source_report=report_path,
            generated_at=generated_at,
            symbol_timeframe_weights=by_symbol_tf,
        )
        summary["weight_file"] = payload

    print(json.dumps(summary, ensure_ascii=True, indent=2))
    return 0 if bool((summary.get("status") or {}).get("ok")) else 1


if __name__ == "__main__":
    raise SystemExit(main())
