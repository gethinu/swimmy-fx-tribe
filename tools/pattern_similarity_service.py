#!/usr/bin/env python3
"""
Pattern Similarity Service (Phase 1)
===================================
- ZMQ REP service on port 5565
- S-expression contract only
- Actions: STATUS / BUILD_INDEX / QUERY
- Index storage: data/patterns/<symbol>/<timeframe>/
"""

from __future__ import annotations

import csv
import json
import os
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np
import zmq


SUPPORTED_SYMBOLS = {"USDJPY", "EURUSD", "GBPUSD"}

WINDOW_BARS = {
    "M5": 120,
    "M15": 120,
    "H1": 120,
    "H4": 120,
    "D1": 120,
    "W1": 104,
    "MN1": 120,
}

STRIDE_BARS = {
    "M5": 6,
    "M15": 4,
    "H1": 1,
    "H4": 1,
    "D1": 1,
    "W1": 1,
    "MN1": 1,
}

HORIZON_BARS = {
    "M5": 48,
    "M15": 16,
    "H1": 24,
    "H4": 6,
    "D1": 7,
    "W1": 4,
    "MN1": 1,
}

ATR_PERIOD = 14
ATR_MULT = 0.5
DEFAULT_K = 30


def _env_int(key: str, default: int) -> int:
    raw = os.getenv(key, "").strip()
    if not raw:
        return default
    try:
        return int(raw)
    except ValueError:
        return default


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
PYTHON_SRC = BASE_DIR / "src" / "python"
if str(PYTHON_SRC) not in sys.path:
    sys.path.insert(0, str(PYTHON_SRC))

from aux_sexp import parse_aux_request, sexp_response


ZMQ_PORT = _env_int("SWIMMY_PORT_PATTERN_SIMILARITY", 5565)
MAX_PAYLOAD_BYTES = _env_int("SWIMMY_PATTERN_MAX_PAYLOAD_BYTES", 2 * 1024 * 1024)

HISTORICAL_DIR = BASE_DIR / "data" / "historical"
PATTERN_ROOT = BASE_DIR / "data" / "patterns"


@dataclass
class IndexBundle:
    vectors: np.ndarray
    labels: np.ndarray
    ids: np.ndarray
    timestamps: np.ndarray
    backend: str
    dim: int
    index: object


_INDEX_CACHE: Dict[Tuple[str, str], IndexBundle] = {}


def _reset_runtime_state_for_tests() -> None:
    _INDEX_CACHE.clear()


def _error_response(message: str) -> str:
    return sexp_response(
        {
            "type": "PATTERN_SIMILARITY_RESULT",
            "status": "error",
            "error": str(message),
        }
    )


def _coerce_int(value, default=None):
    if value is None:
        return default
    try:
        return int(float(value))
    except (TypeError, ValueError):
        return default


def _coerce_float(value, default=None):
    if value is None:
        return default
    try:
        return float(value)
    except (TypeError, ValueError):
        return default


def _normalize_symbol(symbol) -> Optional[str]:
    if not symbol:
        return None
    s = str(symbol).upper()
    if s not in SUPPORTED_SYMBOLS:
        return None
    return s


def _normalize_tf(tf) -> Optional[str]:
    if not tf:
        return None
    t = str(tf).upper()
    if t not in WINDOW_BARS:
        return None
    return t


def _parse_timeframes(raw) -> List[str]:
    if raw is None:
        return ["H1"]
    if isinstance(raw, str):
        t = _normalize_tf(raw)
        return [t] if t else []
    if isinstance(raw, list):
        out = []
        for item in raw:
            t = _normalize_tf(item)
            if t:
                out.append(t)
        return out
    return []


def _find_csv_path(symbol: str, timeframe: str) -> Optional[Path]:
    for name in (f"{symbol}_{timeframe}.csv", f"{symbol}.a_{timeframe}.csv"):
        p = HISTORICAL_DIR / name
        if p.exists():
            return p
    return None


def _load_candles(symbol: str, timeframe: str, start_time=None, end_time=None) -> List[dict]:
    path = _find_csv_path(symbol, timeframe)
    if path is None:
        return []

    candles = []
    with path.open("r", encoding="utf-8") as f:
        reader = csv.reader(f)
        first = True
        for row in reader:
            if not row or len(row) < 5:
                continue
            if first:
                first = False
                token = str(row[0]).strip().lower()
                if token in {"timestamp", "time"}:
                    continue
            ts = _coerce_int(row[0])
            o = _coerce_float(row[1])
            h = _coerce_float(row[2])
            l = _coerce_float(row[3])
            c = _coerce_float(row[4])
            v = _coerce_int(row[5], 0) if len(row) > 5 else 0
            if None in (ts, o, h, l, c):
                continue
            if start_time is not None and ts < start_time:
                continue
            if end_time is not None and ts > end_time:
                continue
            candles.append(
                {
                    "timestamp": ts,
                    "open": o,
                    "high": h,
                    "low": l,
                    "close": c,
                    "volume": v,
                }
            )
    candles.sort(key=lambda x: x["timestamp"])
    return candles


def _resample(series: np.ndarray, out_len: int) -> np.ndarray:
    if len(series) == out_len:
        return series.astype(np.float32)
    if len(series) <= 1:
        return np.zeros(out_len, dtype=np.float32)
    x_old = np.linspace(0.0, 1.0, len(series), dtype=np.float64)
    x_new = np.linspace(0.0, 1.0, out_len, dtype=np.float64)
    return np.interp(x_new, x_old, series).astype(np.float32)


def _l2_normalize(vec: np.ndarray) -> np.ndarray:
    n = float(np.linalg.norm(vec))
    if n <= 1e-12:
        return vec
    return (vec / n).astype(np.float32)


def _fallback_embed(candles: List[dict]) -> np.ndarray:
    close = np.array([c["close"] for c in candles], dtype=np.float64)
    open_ = np.array([c["open"] for c in candles], dtype=np.float64)
    high = np.array([c["high"] for c in candles], dtype=np.float64)
    low = np.array([c["low"] for c in candles], dtype=np.float64)
    volume = np.array([c.get("volume", 0) for c in candles], dtype=np.float64)

    base = max(abs(close[0]), 1e-9)
    close_norm = (close / base) - 1.0
    spread = (high - low) / np.maximum(np.abs(close), 1e-9)
    body = (close - open_) / np.maximum(np.abs(close), 1e-9)
    if np.std(volume) > 1e-9:
        vol_norm = (volume - np.mean(volume)) / np.std(volume)
    else:
        vol_norm = np.zeros_like(volume)

    feats = [
        _resample(close_norm, 64),
        _resample(spread, 32),
        _resample(body, 32),
        _resample(vol_norm, 32),
    ]

    # Add short-horizon returns as shape hint
    rets = np.diff(np.log(np.maximum(close, 1e-9)))
    feats.append(_resample(rets, 32))

    vec = np.concatenate(feats).astype(np.float32)
    return _l2_normalize(vec)


def _embed(candles: List[dict]) -> Tuple[str, np.ndarray]:
    # Phase 1 baseline: deterministic vector embedding.
    # CLIP backend can be plugged in later behind this function.
    return "vector-fallback-v1", _fallback_embed(candles)


def _atr(candles: List[dict], period: int = ATR_PERIOD) -> float:
    if len(candles) < 2:
        return 0.0
    trs = []
    prev_close = candles[0]["close"]
    for c in candles[1:]:
        h = c["high"]
        l = c["low"]
        tr = max(h - l, abs(h - prev_close), abs(l - prev_close))
        trs.append(float(tr))
        prev_close = c["close"]
    if not trs:
        return 0.0
    use = trs[-period:] if len(trs) > period else trs
    return float(sum(use) / len(use))


def _label_window(window: List[dict], future: List[dict]) -> int:
    if not future:
        return 2  # flat fallback

    anchor = float(window[-1]["close"])
    atr = _atr(window)
    if atr <= 1e-9:
        atr = max(anchor * 0.001, 1e-6)

    up_th = anchor + ATR_MULT * atr
    dn_th = anchor - ATR_MULT * atr

    for c in future:
        if c["high"] >= up_th:
            return 0  # up
        if c["low"] <= dn_th:
            return 1  # down
    return 2  # flat


def _label_name(label: int) -> str:
    if label == 0:
        return "UP"
    if label == 1:
        return "DOWN"
    return "FLAT"


def _index_dir(symbol: str, timeframe: str) -> Path:
    return PATTERN_ROOT / symbol / timeframe


def _persist_bundle(symbol: str, timeframe: str, backend: str, vectors, labels, ids, timestamps) -> None:
    out_dir = _index_dir(symbol, timeframe)
    out_dir.mkdir(parents=True, exist_ok=True)

    np.savez_compressed(
        out_dir / "patterns.npz",
        vectors=vectors.astype(np.float32),
        labels=labels.astype(np.int32),
        ids=ids,
        timestamps=timestamps.astype(np.int64),
    )

    meta = {
        "symbol": symbol,
        "timeframe": timeframe,
        "count": int(len(ids)),
        "last_built": int(time.time()),
        "backend": backend,
        "dim": int(vectors.shape[1]) if vectors.size else 0,
    }
    with (out_dir / "meta.json").open("w", encoding="utf-8") as f:
        json.dump(meta, f, ensure_ascii=False)


def _try_build_faiss(vectors: np.ndarray):
    try:
        import faiss  # type: ignore

        index = faiss.IndexFlatIP(vectors.shape[1])
        index.add(vectors.astype(np.float32))
        return index
    except Exception:
        return None


def _load_bundle(symbol: str, timeframe: str) -> Optional[IndexBundle]:
    key = (symbol, timeframe)
    if key in _INDEX_CACHE:
        return _INDEX_CACHE[key]

    pdir = _index_dir(symbol, timeframe)
    npz_path = pdir / "patterns.npz"
    meta_path = pdir / "meta.json"
    if not npz_path.exists() or not meta_path.exists():
        return None

    with np.load(npz_path, allow_pickle=False) as data:
        vectors = data["vectors"].astype(np.float32)
        labels = data["labels"].astype(np.int32)
        ids = data["ids"]
        timestamps = data["timestamps"].astype(np.int64)

    if vectors.ndim != 2 or len(vectors) == 0:
        return None

    with meta_path.open("r", encoding="utf-8") as f:
        meta = json.load(f)

    bundle = IndexBundle(
        vectors=vectors,
        labels=labels,
        ids=ids,
        timestamps=timestamps,
        backend=str(meta.get("backend", "vector-fallback-v1")),
        dim=int(vectors.shape[1]),
        index=_try_build_faiss(vectors),
    )
    _INDEX_CACHE[key] = bundle
    return bundle


def _collect_index_status() -> List[dict]:
    out = []
    if not PATTERN_ROOT.exists():
        return out
    for sym_dir in sorted([p for p in PATTERN_ROOT.iterdir() if p.is_dir()]):
        for tf_dir in sorted([p for p in sym_dir.iterdir() if p.is_dir()]):
            meta_path = tf_dir / "meta.json"
            if not meta_path.exists():
                continue
            try:
                with meta_path.open("r", encoding="utf-8") as f:
                    meta = json.load(f)
            except Exception:
                continue
            out.append(
                {
                    "symbol": meta.get("symbol", sym_dir.name),
                    "timeframe": meta.get("timeframe", tf_dir.name),
                    "count": int(meta.get("count", 0)),
                    "last_built": int(meta.get("last_built", 0)),
                }
            )
    return out


def _build_index_for(symbol: str, timeframe: str, start_time=None, end_time=None) -> dict:
    candles = _load_candles(symbol, timeframe, start_time=start_time, end_time=end_time)
    window = WINDOW_BARS[timeframe]
    stride = STRIDE_BARS[timeframe]
    horizon = HORIZON_BARS[timeframe]

    max_i = len(candles) - horizon
    if len(candles) < (window + horizon):
        vectors = np.zeros((0, 1), dtype=np.float32)
        labels = np.zeros((0,), dtype=np.int32)
        ids = np.array([], dtype="<U1")
        timestamps = np.array([], dtype=np.int64)
        _persist_bundle(symbol, timeframe, "vector-fallback-v1", vectors, labels, ids, timestamps)
        _INDEX_CACHE.pop((symbol, timeframe), None)
        return {"symbol": symbol, "timeframe": timeframe, "count": 0}

    rows_vec = []
    rows_lbl = []
    rows_id = []
    rows_ts = []
    backend = "vector-fallback-v1"

    for i in range(window - 1, max_i, stride):
        win = candles[i - window + 1 : i + 1]
        fut = candles[i + 1 : i + 1 + horizon]
        backend, vec = _embed(win)
        rows_vec.append(vec)
        rows_lbl.append(_label_window(win, fut))
        ts = int(win[-1]["timestamp"])
        rows_id.append(f"{timeframe}:{symbol}:{ts}")
        rows_ts.append(ts)

    vectors = np.vstack(rows_vec).astype(np.float32)
    labels = np.array(rows_lbl, dtype=np.int32)
    ids = np.array(rows_id, dtype="<U64")
    timestamps = np.array(rows_ts, dtype=np.int64)

    _persist_bundle(symbol, timeframe, backend, vectors, labels, ids, timestamps)
    _INDEX_CACHE.pop((symbol, timeframe), None)
    return {"symbol": symbol, "timeframe": timeframe, "count": int(len(ids))}


def _search(bundle: IndexBundle, query_vec: np.ndarray, k: int):
    k = max(1, min(k, len(bundle.ids)))

    if bundle.index is not None:
        q = query_vec.astype(np.float32).reshape(1, -1)
        sims, idx = bundle.index.search(q, k)
        sims = sims[0]
        idx = idx[0]
    else:
        sims_all = bundle.vectors @ query_vec.astype(np.float32)
        idx = np.argpartition(-sims_all, kth=k - 1)[:k]
        idx = idx[np.argsort(-sims_all[idx])]
        sims = sims_all[idx]

    out = []
    for i, sim in zip(idx.tolist(), sims.tolist()):
        dist = max(0.0, 1.0 - float(sim))
        label = int(bundle.labels[i])
        out.append(
            {
                "id": str(bundle.ids[i]),
                "distance": dist,
                "label": _label_name(label),
                "_label": label,
            }
        )
    return out


def _probabilities(neighbors: List[dict]) -> Tuple[float, float, float]:
    if not neighbors:
        return 1.0 / 3.0, 1.0 / 3.0, 1.0 / 3.0

    weights = {0: 0.0, 1: 0.0, 2: 0.0}
    for n in neighbors:
        d = float(n["distance"])
        w = 1.0 / max(d, 1e-6)
        weights[int(n["_label"])] += w

    total = max(sum(weights.values()), 1e-9)
    return (
        float(weights[0] / total),
        float(weights[1] / total),
        float(weights[2] / total),
    )


def handle_request_sexp(message: str) -> str:
    if len(message.encode("utf-8")) > MAX_PAYLOAD_BYTES:
        return _error_response("Payload too large")

    try:
        data = parse_aux_request(message)
    except Exception as e:
        return _error_response(str(e))

    msg_type = str(data.get("type", "")).upper()
    if msg_type != "PATTERN_SIMILARITY":
        return _error_response(f"Invalid type: {msg_type}")

    schema_version = _coerce_int(data.get("schema_version"))
    if schema_version != 1:
        return _error_response("Unsupported schema_version")

    action = str(data.get("action", "")).upper()
    if not action:
        return _error_response("Missing action")

    if action == "STATUS":
        return sexp_response(
            {
                "type": "PATTERN_SIMILARITY_RESULT",
                "status": "ok",
                "model": "vector-fallback-v1",
                "indices": _collect_index_status(),
            }
        )

    if action == "BUILD_INDEX":
        symbol = _normalize_symbol(data.get("symbol"))
        if not symbol:
            return _error_response("Missing or unsupported symbol")

        timeframes = _parse_timeframes(data.get("timeframes"))
        if not timeframes:
            return _error_response("Missing or invalid timeframes")

        start_time = _coerce_int(data.get("start_time"))
        end_time = _coerce_int(data.get("end_time"))

        details = []
        try:
            for tf in timeframes:
                details.append(
                    _build_index_for(symbol, tf, start_time=start_time, end_time=end_time)
                )
        except Exception as e:
            return _error_response(f"BUILD_INDEX failed: {e}")

        return sexp_response(
            {
                "type": "PATTERN_SIMILARITY_RESULT",
                "status": "ok",
                "message": "build started",
                "details": details,
            }
        )

    if action == "QUERY":
        symbol = _normalize_symbol(data.get("symbol"))
        if not symbol:
            return _error_response("Missing or unsupported symbol")

        timeframe = _normalize_tf(data.get("timeframe"))
        if not timeframe:
            return _error_response("Missing or invalid timeframe")

        candles = data.get("candles")
        if not isinstance(candles, list):
            return _error_response("Missing candles")

        expected_len = WINDOW_BARS[timeframe]
        if len(candles) != expected_len:
            return _error_response(
                f"candles length must be {expected_len} for {timeframe}"
            )

        # Normalize candle schema defensively
        norm = []
        for c in candles:
            if not isinstance(c, dict):
                return _error_response("Invalid candle format")
            ts = _coerce_int(c.get("timestamp"))
            o = _coerce_float(c.get("open"))
            h = _coerce_float(c.get("high"))
            l = _coerce_float(c.get("low"))
            cl = _coerce_float(c.get("close"))
            v = _coerce_int(c.get("volume"), 0)
            if None in (ts, o, h, l, cl):
                return _error_response("Invalid candle fields")
            norm.append(
                {
                    "timestamp": ts,
                    "open": o,
                    "high": h,
                    "low": l,
                    "close": cl,
                    "volume": v,
                }
            )

        bundle = _load_bundle(symbol, timeframe)
        if bundle is None:
            return _error_response("Index not found. Build index first.")

        backend, qvec = _embed(norm)
        if qvec.shape[0] != bundle.dim:
            return _error_response(
                f"Embedding dim mismatch (query={qvec.shape[0]}, index={bundle.dim}); rebuild index"
            )
        if backend != bundle.backend:
            return _error_response(
                f"Embedding backend mismatch (query={backend}, index={bundle.backend}); rebuild index"
            )

        k = _coerce_int(data.get("k"), DEFAULT_K)
        if not k or k <= 0:
            k = DEFAULT_K

        neighbors = _search(bundle, qvec, k)
        p_up, p_down, p_flat = _probabilities(neighbors)

        top_k = []
        for n in neighbors:
            top_k.append(
                {
                    "id": n["id"],
                    "distance": float(round(n["distance"], 8)),
                    "label": n["label"],
                }
            )

        return sexp_response(
            {
                "type": "PATTERN_SIMILARITY_RESULT",
                "status": "ok",
                "result": {
                    "p_up": float(round(p_up, 8)),
                    "p_down": float(round(p_down, 8)),
                    "p_flat": float(round(p_flat, 8)),
                    "top_k": top_k,
                },
            }
        )

    return _error_response(f"Unknown action: {action}")


def run_server() -> None:
    print("=" * 60)
    print("  Pattern Similarity Service (Phase 1)")
    print("=" * 60)
    print(f"[PATTERN] Listening on port {ZMQ_PORT} (REP)")

    context = zmq.Context()
    socket = context.socket(zmq.REP)
    socket.bind(f"tcp://*:{ZMQ_PORT}")

    while True:
        try:
            message = socket.recv_string()
            response = handle_request_sexp(message)
            socket.send_string(response)
        except KeyboardInterrupt:
            break
        except Exception as e:
            socket.send_string(_error_response(f"Unhandled error: {e}"))

    socket.close()
    context.term()


def main() -> None:
    PATTERN_ROOT.mkdir(parents=True, exist_ok=True)
    run_server()


if __name__ == "__main__":
    main()
