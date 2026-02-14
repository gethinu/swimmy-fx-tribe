import os
import sys
import time
import threading
import csv
from pathlib import Path

import zmq

try:
    import numpy as np
    from PIL import Image, ImageDraw
except Exception:  # pragma: no cover (service can still answer STATUS/errors)
    np = None
    Image = None
    ImageDraw = None

try:  # optional (deep learning)
    import torch  # type: ignore
except Exception:  # pragma: no cover
    torch = None

try:  # optional (CLIP backend)
    import open_clip  # type: ignore
except Exception:  # pragma: no cover
    open_clip = None

try:  # optional (fast ANN)
    import faiss  # type: ignore
except Exception:  # pragma: no cover
    faiss = None


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


def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default


def _env_float(key: str, default: float) -> float:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return float(val)
    except ValueError:
        return default


ZMQ_PORT = _env_int("SWIMMY_PORT_PATTERN_SIMILARITY", 5564)
PAYLOAD_MAX_BYTES = _env_int("SWIMMY_PATTERN_SIM_PAYLOAD_MAX_BYTES", 2_000_000)

WINDOW_BARS = {
    "M5": 120,
    "M15": 120,
    "H1": 120,
    "H4": 120,
    "D1": 120,
    "W1": 104,
    "MN1": 120,
}

# Pattern DB persistence
PATTERNS_DIR = Path(os.getenv("SWIMMY_PATTERNS_DIR", str(BASE_DIR / "data" / "patterns")))

# Embedding backend: "clip" (preferred) or "pixel" (deterministic fallback)
EMBED_BACKEND = os.getenv("SWIMMY_PATTERN_EMBED_BACKEND", "clip").strip().lower() or "clip"

# Labeling (ATR-based)
LABEL_ATR_PERIOD = _env_int("SWIMMY_PATTERN_LABEL_ATR_PERIOD", 14)
LABEL_ATR_MULT = _env_float("SWIMMY_PATTERN_LABEL_ATR_MULT", 0.50)

# Guardrail: avoid accidental OOM on long histories (especially M5)
MAX_BUILD_SAMPLES = _env_int("SWIMMY_PATTERN_MAX_BUILD_SAMPLES", 300_000)

# Horizon bars per timeframe (SPEC: fixed evaluation horizon by TF group)
HORIZON_BARS = {
    "M5": 48,    # 4 hours
    "M15": 16,   # 4 hours
    "H1": 24,    # 1 day
    "H4": 6,     # 1 day
    "D1": 7,     # 1 week
    "W1": 4,     # ~1 month
    "MN1": 1,    # 1 month
}

# Stride (sample interval) per timeframe
STRIDE_BARS = {
    "M5": 6,    # 30 minutes
    "M15": 4,   # 1 hour
    "H1": 1,
    "H4": 1,
    "D1": 1,
    "W1": 1,
    "MN1": 1,
}


_state_lock = threading.Lock()
_build_threads = {}  # (symbol, tf) -> Thread
_build_status = {}  # (symbol, tf) -> dict(status=..., message=..., built=..., total=...)


class PatternIndex:
    def __init__(
        self,
        *,
        symbol: str,
        timeframe: str,
        model: str,
        ids: list[str],
        labels: list[str],
        embeddings,
        last_built: int,
        ann_index=None,
    ):
        self.symbol = symbol
        self.timeframe = timeframe
        self.model = model
        self.ids = ids
        self.labels = labels
        self.embeddings = embeddings  # np.ndarray (n, d), unit-normalized
        self.last_built = last_built
        self.ann_index = ann_index  # faiss index (optional)


_indices = {}  # (symbol, tf) -> PatternIndex


def _error_response(message: str) -> str:
    return sexp_response(
        {
            "type": "PATTERN_SIMILARITY_RESULT",
            "status": "error",
            "error": message,
        }
    )

def _normalize_timeframe(value: str | None) -> str | None:
    if value is None:
        return None
    tf = str(value).upper()
    if tf == "MN":
        tf = "MN1"
    if tf not in WINDOW_BARS:
        return None
    return tf


def _coerce_float(value, default=None):
    if value is None:
        return default
    try:
        return float(value)
    except Exception:
        return default


def _coerce_int(value, default=None):
    if value is None:
        return default
    try:
        return int(float(value))
    except Exception:
        return default


def _sort_candles(candles: list[dict]) -> list[dict]:
    return sorted(candles, key=lambda c: _coerce_int(c.get("timestamp"), 0))


def _candles_to_image(candles: list[dict], *, width: int = 224, height: int = 224):
    if Image is None or ImageDraw is None:
        raise RuntimeError("PIL not available")

    candles = _sort_candles(candles)
    highs = [_coerce_float(c.get("high")) for c in candles]
    lows = [_coerce_float(c.get("low")) for c in candles]
    if not highs or any(v is None for v in highs) or any(v is None for v in lows):
        raise ValueError("Invalid candle values")
    hi = max(highs)
    lo = min(lows)
    if hi <= lo:
        hi = lo + 1e-6

    def y(price: float) -> int:
        # Top is high.
        norm = (hi - price) / (hi - lo)
        return int(max(0, min(height - 1, round(norm * (height - 1)))))

    img = Image.new("RGB", (width, height), (255, 255, 255))
    draw = ImageDraw.Draw(img)

    n = len(candles)
    if n <= 1:
        return img
    pad = 4
    x0 = pad
    x1 = width - pad - 1
    step = (x1 - x0) / (n - 1)

    for i, c in enumerate(candles):
        o = _coerce_float(c.get("open"))
        h = _coerce_float(c.get("high"))
        l = _coerce_float(c.get("low"))
        cl = _coerce_float(c.get("close"))
        if None in (o, h, l, cl):
            continue
        x = x0 + i * step
        xi = int(round(x))
        # Wick
        draw.line([(xi, y(h)), (xi, y(l))], fill=(0, 0, 0), width=1)
        # Body
        y_open = y(o)
        y_close = y(cl)
        top = min(y_open, y_close)
        bot = max(y_open, y_close)
        body_w = 2
        draw.rectangle([(xi - body_w, top), (xi + body_w, bot)], outline=(0, 0, 0), fill=(0, 0, 0))

    return img


def _pixel_embed(candles: list[dict]) -> tuple[str, "np.ndarray"]:
    if np is None:
        raise RuntimeError("numpy not available")
    img = _candles_to_image(candles)
    img_g = img.convert("L").resize((32, 32))
    arr = np.asarray(img_g, dtype=np.float32).reshape(-1)
    norm = float(np.linalg.norm(arr))
    if norm > 0:
        arr = arr / norm
    return "pixel-32x32", arr.astype(np.float32)

_clip_lock = threading.Lock()
_clip_model = None
_clip_preprocess = None
_clip_device = None


def _ensure_clip_loaded():
    global _clip_model, _clip_preprocess, _clip_device
    if torch is None or open_clip is None:
        return None, None, None
    with _clip_lock:
        if _clip_model is None:
            device = "cuda" if hasattr(torch, "cuda") and torch.cuda.is_available() else "cpu"
            model, _, preprocess = open_clip.create_model_and_transforms(
                "ViT-B-32",
                pretrained="laion2b_s34b_b79k",
            )
            model.eval()
            model.to(device)
            _clip_model = model
            _clip_preprocess = preprocess
            _clip_device = device
    return _clip_model, _clip_preprocess, _clip_device


def _clip_embed(candles: list[dict]) -> tuple[str, "np.ndarray"]:
    if np is None:
        raise RuntimeError("numpy not available")
    model, preprocess, device = _ensure_clip_loaded()
    if model is None or preprocess is None:
        raise RuntimeError("CLIP backend not available")
    img = _candles_to_image(candles, width=224, height=224)
    x = preprocess(img).unsqueeze(0)
    if device:
        x = x.to(device)
    with torch.no_grad():
        feat = model.encode_image(x)
        feat = feat / feat.norm(dim=-1, keepdim=True)
    vec = feat[0].detach().cpu().numpy().astype(np.float32)
    return "clip-vit-b32", vec


def _embed_candles(candles: list[dict]) -> tuple[str, "np.ndarray"]:
    backend = EMBED_BACKEND
    if backend == "pixel":
        return _pixel_embed(candles)
    if backend == "clip":
        try:
            return _clip_embed(candles)
        except Exception:
            # Fail-open for embedding backend issues: keep service usable even without torch/open_clip.
            return _pixel_embed(candles)
    # Unknown backend -> fallback
    return _pixel_embed(candles)


def _knn_search(embeddings: "np.ndarray", q: "np.ndarray", k: int) -> tuple[list[int], list[float]]:
    # Both embeddings and q are expected unit-normalized.
    n = int(embeddings.shape[0])
    if n == 0:
        return [], []
    k = max(1, min(int(k), n))
    sims = embeddings @ q  # (n,)
    # Take top-k highest similarity.
    idx = np.argpartition(-sims, k - 1)[:k]
    idx = idx[np.argsort(-sims[idx])]
    dists = (1.0 - sims[idx]).astype(np.float32)
    # Numerical safety: cosine distance should not be negative.
    dists = np.maximum(dists, 0.0)
    dists = dists.tolist()
    return idx.astype(int).tolist(), [float(d) for d in dists]


def _build_ann_index(embeddings: "np.ndarray"):
    if faiss is None:
        return None
    d = int(embeddings.shape[1])
    index = faiss.IndexFlatIP(d)
    index.add(embeddings.astype(np.float32))
    return index


def _knn_search_index(idx: PatternIndex, q: "np.ndarray", k: int) -> tuple[list[int], list[float]]:
    if idx.ann_index is not None and faiss is not None:
        q2 = q.reshape(1, -1).astype(np.float32)
        sims, I = idx.ann_index.search(q2, int(k))
        I = I.reshape(-1)
        sims = sims.reshape(-1)
        dists = (1.0 - sims).astype(np.float32)
        dists = np.maximum(dists, 0.0)
        return [int(i) for i in I.tolist()], [float(d) for d in dists.tolist()]
    return _knn_search(idx.embeddings, q, k)


def _distance_weighted_probs(labels: list[str], dists: list[float]) -> tuple[dict, list[dict]]:
    eps = 1e-6
    w_up = 0.0
    w_down = 0.0
    w_flat = 0.0
    top_k: list[dict] = []
    for i, (lab, dist) in enumerate(zip(labels, dists)):
        lab_u = str(lab).upper()
        w = 1.0 / (float(dist) + eps)
        if lab_u == "UP":
            w_up += w
        elif lab_u == "DOWN":
            w_down += w
        else:
            w_flat += w
        top_k.append({"distance": float(dist), "label": lab_u})
    s = w_up + w_down + w_flat
    if s <= 0:
        probs = {"p_up": 0.0, "p_down": 0.0, "p_flat": 1.0}
    else:
        probs = {"p_up": w_up / s, "p_down": w_down / s, "p_flat": w_flat / s}
    return probs, top_k


def _reset_state_for_tests() -> None:
    with _state_lock:
        _indices.clear()
        _build_threads.clear()
        _build_status.clear()


def _build_index_from_inline_samples(*, symbol: str, timeframe: str, samples: list[dict]) -> None:
    tf = _normalize_timeframe(timeframe)
    if not tf:
        raise ValueError("Invalid timeframe")
    expected = WINDOW_BARS[tf]
    ids: list[str] = []
    labels: list[str] = []
    vecs: list["np.ndarray"] = []
    model = None
    for s in samples:
        candles = s.get("candles")
        if not isinstance(candles, list) or len(candles) != expected:
            raise ValueError("Invalid candles length")
        m, v = _embed_candles(candles)
        model = model or m
        ids.append(str(s.get("id", "")))
        labels.append(str(s.get("label", "FLAT")).upper())
        vecs.append(v)
    if np is None:
        raise RuntimeError("numpy not available")
    embs = np.stack(vecs, axis=0).astype(np.float32)
    # Ensure unit normalization (defensive).
    norms = np.linalg.norm(embs, axis=1, keepdims=True)
    norms[norms == 0] = 1.0
    embs = embs / norms
    ann = _build_ann_index(embs)
    idx = PatternIndex(
        symbol=str(symbol).upper(),
        timeframe=tf,
        model=str(model or "pixel-32x32"),
        ids=ids,
        labels=labels,
        embeddings=embs,
        last_built=int(time.time()),
        ann_index=ann,
    )
    with _state_lock:
        _indices[(idx.symbol, idx.timeframe)] = idx


def _index_dir(symbol: str, timeframe: str) -> Path:
    return (PATTERNS_DIR / str(symbol).upper() / str(timeframe).upper()).resolve()


def _index_npz_path(symbol: str, timeframe: str) -> Path:
    return _index_dir(symbol, timeframe) / "index.npz"


def _index_faiss_path(symbol: str, timeframe: str) -> Path:
    return _index_dir(symbol, timeframe) / "index.faiss"


def _save_index_to_disk(idx: PatternIndex) -> None:
    if np is None:
        raise RuntimeError("numpy not available")
    out_dir = _index_dir(idx.symbol, idx.timeframe)
    out_dir.mkdir(parents=True, exist_ok=True)
    np.savez_compressed(
        out_dir / "index.npz",
        embeddings=idx.embeddings.astype(np.float32),
        ids=np.asarray(idx.ids, dtype=object),
        labels=np.asarray(idx.labels, dtype=object),
        model=np.asarray(idx.model, dtype=object),
        last_built=np.asarray(int(idx.last_built), dtype=np.int64),
    )
    if idx.ann_index is not None and faiss is not None:
        faiss.write_index(idx.ann_index, str(out_dir / "index.faiss"))


def _load_index_from_disk(symbol: str, timeframe: str) -> PatternIndex | None:
    if np is None:
        return None
    path = _index_npz_path(symbol, timeframe)
    if not path.exists():
        return None
    data = np.load(path, allow_pickle=True)
    embeddings = data["embeddings"].astype(np.float32)
    # Defensive normalization (in case older files were not unit-normalized)
    norms = np.linalg.norm(embeddings, axis=1, keepdims=True)
    norms[norms == 0] = 1.0
    embeddings = embeddings / norms
    ids = [str(x) for x in data["ids"].tolist()] if "ids" in data.files else []
    labels = [str(x) for x in data["labels"].tolist()] if "labels" in data.files else []
    model = str(data["model"].item()) if "model" in data.files else "unknown"
    last_built = int(data["last_built"].item()) if "last_built" in data.files else 0

    ann = None
    if faiss is not None:
        fpath = _index_faiss_path(symbol, timeframe)
        if fpath.exists():
            ann = faiss.read_index(str(fpath))
        else:
            ann = _build_ann_index(embeddings)

    return PatternIndex(
        symbol=str(symbol).upper(),
        timeframe=str(timeframe).upper(),
        model=model,
        ids=ids,
        labels=labels,
        embeddings=embeddings,
        last_built=last_built,
        ann_index=ann,
    )


def load_indices_from_disk() -> int:
    "Load persisted indices from PATTERNS_DIR into memory. Returns number loaded."
    loaded = 0
    if not PATTERNS_DIR.exists():
        return 0
    for npz in PATTERNS_DIR.glob("*/*/index.npz"):
        try:
            tf = npz.parent.name
            sym = npz.parent.parent.name
            idx = _load_index_from_disk(sym, tf)
            if idx is None:
                continue
            with _state_lock:
                _indices[(idx.symbol, idx.timeframe)] = idx
            loaded += 1
        except Exception:
            continue
    return loaded


def _resolve_historical_csv_path(symbol: str, timeframe: str) -> Path | None:
    tf = str(timeframe).upper()
    sym = str(symbol).upper()
    tf_file = "MN" if tf in ("MN1", "MN") else tf
    data_dir = (BASE_DIR / "data" / "historical").resolve()
    candidates = [f"{sym}_{tf_file}.csv", f"{sym}.a_{tf_file}.csv"]
    for name in candidates:
        path = (data_dir / name).resolve()
        if path.exists():
            return path
    return None


def _load_candles_from_csv(path: Path) -> list[dict]:
    candles: list[dict] = []
    with open(path, "r", newline="") as f:
        reader = csv.reader(f)
        for row in reader:
            if not row:
                continue
            head = str(row[0]).strip().lower()
            if head in ("timestamp", "time"):
                continue
            if len(row) < 5:
                continue
            try:
                ts = int(float(row[0]))
                o = float(row[1])
                h = float(row[2])
                l = float(row[3])
                c = float(row[4])
                v = int(float(row[5])) if len(row) > 5 and str(row[5]).strip() else 0
            except Exception:
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
    return _sort_candles(candles)


def _atr(candles: list[dict], period: int) -> float | None:
    if period <= 0:
        return None
    if len(candles) < period + 1:
        return None
    trs: list[float] = []
    # Use the last `period` bars, with prev_close from the preceding bar.
    start = len(candles) - period
    for i in range(start, len(candles)):
        cur = candles[i]
        prev = candles[i - 1]
        hi = _coerce_float(cur.get("high"))
        lo = _coerce_float(cur.get("low"))
        prev_close = _coerce_float(prev.get("close"))
        if None in (hi, lo, prev_close):
            return None
        tr = max(hi - lo, abs(hi - prev_close), abs(lo - prev_close))
        trs.append(float(tr))
    if not trs:
        return None
    return float(sum(trs) / float(period))


def _label_from_future_return(cur_close: float, future_close: float, atr: float) -> str:
    thr = float(atr) * float(LABEL_ATR_MULT)
    ret = float(future_close) - float(cur_close)
    if ret >= thr:
        return "UP"
    if ret <= -thr:
        return "DOWN"
    return "FLAT"


def _build_index_from_csv(
    *,
    symbol: str,
    timeframe: str,
    start_time: int | None,
    end_time: int | None,
    force: bool,
) -> PatternIndex:
    if np is None:
        raise RuntimeError("numpy not available")

    sym = str(symbol).upper()
    tf = _normalize_timeframe(timeframe)
    if not tf:
        raise ValueError("Invalid timeframe")

    window = WINDOW_BARS[tf]
    horizon = HORIZON_BARS.get(tf)
    stride = STRIDE_BARS.get(tf, 1)
    if horizon is None:
        raise ValueError("Unsupported timeframe")

    # If not force, and index exists on disk, just load it.
    if not force:
        existing = _load_index_from_disk(sym, tf)
        if existing is not None:
            return existing

    csv_path = _resolve_historical_csv_path(sym, tf)
    if csv_path is None:
        raise FileNotFoundError(f"historical csv not found for {sym} {tf}")

    candles_all = _load_candles_from_csv(csv_path)
    n = len(candles_all)
    if n < window + horizon:
        raise ValueError("not enough candles to build index")

    est = max(0, (n - horizon) - (window - 1))
    est = (est + stride - 1) // stride
    if est > MAX_BUILD_SAMPLES:
        raise ValueError(f"too many samples ({est}) > MAX_BUILD_SAMPLES={MAX_BUILD_SAMPLES}; narrow time range or increase stride")

    ids: list[str] = []
    labels: list[str] = []
    vecs: list["np.ndarray"] = []

    for end_idx in range(window - 1, n - horizon, stride):
        end_ts = _coerce_int(candles_all[end_idx].get("timestamp"), 0) or 0
        if start_time is not None and end_ts < int(start_time):
            continue
        if end_time is not None and end_ts > int(end_time):
            break
        window_candles = candles_all[end_idx - window + 1 : end_idx + 1]
        atr = _atr(window_candles, LABEL_ATR_PERIOD)
        if atr is None or atr <= 0:
            continue
        cur_close = _coerce_float(window_candles[-1].get("close"))
        future_close = _coerce_float(candles_all[end_idx + horizon].get("close"))
        if cur_close is None or future_close is None:
            continue
        label = _label_from_future_return(cur_close, future_close, atr)
        model, v = _embed_candles(window_candles)
        ids.append(f"{tf}:{sym}:{end_ts}")
        labels.append(label)
        vecs.append(v)

    if not vecs:
        raise ValueError("no samples built (check ATR params / time range)")

    embs = np.stack(vecs, axis=0).astype(np.float32)
    norms = np.linalg.norm(embs, axis=1, keepdims=True)
    norms[norms == 0] = 1.0
    embs = embs / norms
    ann = _build_ann_index(embs)
    idx = PatternIndex(
        symbol=sym,
        timeframe=tf,
        model=model,
        ids=ids,
        labels=labels,
        embeddings=embs,
        last_built=int(time.time()),
        ann_index=ann,
    )
    _save_index_to_disk(idx)
    return idx


def handle_request_sexp(message: str) -> str:
    if isinstance(message, str):
        size = len(message.encode("utf-8", errors="replace"))
        if size > PAYLOAD_MAX_BYTES:
            return _error_response("payload too large")

    try:
        data = parse_aux_request(message)
    except Exception as e:
        return _error_response(str(e))

    msg_type = str(data.get("type", "")).upper()
    if msg_type != "PATTERN_SIMILARITY":
        return _error_response(f"Invalid type: {msg_type}")

    schema_version = data.get("schema_version")
    try:
        schema_version = int(float(schema_version))
    except Exception:
        schema_version = None
    if schema_version != 1:
        return _error_response("Unsupported schema_version")

    action = str(data.get("action", "")).upper()
    if not action:
        return _error_response("Missing action")

    if action == "STATUS":
        with _state_lock:
            indices = []
            models = set()
            for (symbol, tf), idx in sorted(_indices.items()):
                indices.append(
                    {
                        "symbol": symbol,
                        "timeframe": tf,
                        "count": int(getattr(idx, "embeddings", []).shape[0] if getattr(idx, "embeddings", None) is not None else 0),
                        "last_built": int(getattr(idx, "last_built", 0)),
                    }
                )
                try:
                    models.add(str(getattr(idx, "model", "")))
                except Exception:
                    pass
            builds = []
            for (symbol, tf), st in sorted(_build_status.items()):
                entry = {"symbol": symbol, "timeframe": tf}
                if isinstance(st, dict):
                    entry.update(st)
                builds.append(entry)

            if len(models) == 1:
                model = next(iter(models))
            elif len(models) >= 2:
                model = "mixed"
            else:
                if EMBED_BACKEND == "clip" and torch is not None and open_clip is not None:
                    model = "clip-vit-b32"
                else:
                    model = "pixel-32x32"
        return sexp_response(
            {
                "type": "PATTERN_SIMILARITY_RESULT",
                "status": "ok",
                "model": model,
                "indices": indices,
                "builds": builds,
            }
        )

    if action == "BUILD_INDEX":
        symbol = data.get("symbol")
        if not symbol:
            return _error_response("Missing symbol")
        symbol = str(symbol).upper()

        timeframes = data.get("timeframes")
        if not (isinstance(timeframes, list) and timeframes):
            return _error_response("Missing timeframes")

        start_time = _coerce_int(data.get("start_time"))
        end_time = _coerce_int(data.get("end_time"))
        force = bool(data.get("force", False))

        invalid = []
        normalized: list[str] = []

        def _worker(sym: str, tf: str, st: int | None, et: int | None, force_: bool) -> None:
            key = (sym, tf)
            try:
                idx = _build_index_from_csv(symbol=sym, timeframe=tf, start_time=st, end_time=et, force=force_)
                with _state_lock:
                    _indices[(idx.symbol, idx.timeframe)] = idx
                    _build_status[key] = {"status": "ok", "count": int(idx.embeddings.shape[0]), "last_built": int(idx.last_built)}
            except Exception as e:
                with _state_lock:
                    _build_status[key] = {"status": "error", "error": str(e)}
            finally:
                with _state_lock:
                    _build_threads.pop(key, None)

        for tf_raw in timeframes:
            tf = _normalize_timeframe(tf_raw)
            if not tf:
                invalid.append(str(tf_raw))
            else:
                normalized.append(tf)
        if invalid:
            return _error_response(f"Invalid timeframe(s): {invalid}")

        started = []
        for tf in list(dict.fromkeys(normalized)):
            key = (symbol, tf)
            with _state_lock:
                existing = _build_threads.get(key)
                if existing and existing.is_alive():
                    continue
                _build_status[key] = {"status": "building"}
                th = threading.Thread(target=_worker, args=(symbol, tf, start_time, end_time, force), daemon=True)
                _build_threads[key] = th
                th.start()
            started.append(tf)

        return sexp_response(
            {
                "type": "PATTERN_SIMILARITY_RESULT",
                "status": "ok",
                "message": "build started",
                "started": started,
            }
        )

    if action == "QUERY":
        symbol = data.get("symbol")
        tf = data.get("timeframe")
        if not symbol:
            return _error_response("Missing symbol")
        if not tf:
            return _error_response("Missing timeframe")
        tf_norm = _normalize_timeframe(tf)
        if not tf_norm:
            return _error_response("Invalid timeframe")
        candles = data.get("candles")
        if not isinstance(candles, list):
            return _error_response("Missing candles")
        expected = WINDOW_BARS[tf_norm]
        if len(candles) != expected:
            return _error_response("candles length mismatch")
        k = data.get("k")
        k = _coerce_int(k, default=30) or 30

        key = (str(symbol).upper(), tf_norm)
        with _state_lock:
            idx = _indices.get(key)
        if idx is None:
            return _error_response("index not built")

        try:
            model, q = _embed_candles(candles)
        except Exception as e:
            return _error_response(f"embed error: {e}")

        try:
            nn_idx, dists = _knn_search_index(idx, q, k)
        except Exception as e:
            return _error_response(f"search error: {e}")

        neighbor_labels = [idx.labels[i] for i in nn_idx]
        probs, top_k = _distance_weighted_probs(neighbor_labels, dists)
        # Fill top_k ids.
        for j, i in enumerate(nn_idx):
            top_k[j]["id"] = idx.ids[i] if i < len(idx.ids) else str(i)

        return sexp_response(
            {
                "type": "PATTERN_SIMILARITY_RESULT",
                "status": "ok",
                "result": {
                    "p_up": float(probs["p_up"]),
                    "p_down": float(probs["p_down"]),
                    "p_flat": float(probs["p_flat"]),
                    "top_k": top_k,
                },
            }
        )

    return _error_response(f"Unknown action: {action}")


def run_server() -> None:
    print("👁️ Swimmy Pattern Similarity Service (S-exp, REQ/REP)")
    print("====================================================")

    try:
        loaded = load_indices_from_disk()
        print(f"[PATTERN] Loaded {loaded} index file(s) from {PATTERNS_DIR}")
    except Exception as e:
        print(f"[PATTERN] Index load failed: {e}")

    context = zmq.Context()
    socket = context.socket(zmq.REP)
    socket.bind(f"tcp://*:{ZMQ_PORT}")
    print(f"[PATTERN] Listening on port {ZMQ_PORT}...")

    while True:
        try:
            message = socket.recv_string()
            response = handle_request_sexp(message)
            socket.send_string(response)
        except KeyboardInterrupt:
            print("\n[PATTERN] Shutting down...")
            break
        except Exception as e:
            try:
                socket.send_string(_error_response(f"Unhandled error: {e}"))
            except Exception:
                pass

    socket.close()
    context.term()


if __name__ == "__main__":
    run_server()
