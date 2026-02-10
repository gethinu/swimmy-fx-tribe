import os
import sys
import time
import threading
from pathlib import Path

import zmq

try:
    import numpy as np
    from PIL import Image, ImageDraw
except Exception:  # pragma: no cover (service can still answer STATUS/errors)
    np = None
    Image = None
    ImageDraw = None


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


_state_lock = threading.Lock()
_build_threads = {}  # (symbol, tf) -> Thread


class PatternIndex:
    def __init__(self, *, symbol: str, timeframe: str, model: str, ids: list[str], labels: list[str], embeddings, last_built: int):
        self.symbol = symbol
        self.timeframe = timeframe
        self.model = model
        self.ids = ids
        self.labels = labels
        self.embeddings = embeddings  # np.ndarray (n, d), unit-normalized
        self.last_built = last_built


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


def _embed_candles(candles: list[dict]) -> tuple[str, "np.ndarray"]:
    # v1: CLIP support can be added later; for now use deterministic pixel embedding.
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
    idx = PatternIndex(
        symbol=str(symbol).upper(),
        timeframe=tf,
        model=str(model or "pixel-32x32"),
        ids=ids,
        labels=labels,
        embeddings=embs,
        last_built=int(time.time()),
    )
    with _state_lock:
        _indices[(idx.symbol, idx.timeframe)] = idx


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
            for (symbol, tf), idx in sorted(_indices.items()):
                indices.append(
                    {
                        "symbol": symbol,
                        "timeframe": tf,
                        "count": int(getattr(idx, "embeddings", []).shape[0] if getattr(idx, "embeddings", None) is not None else 0),
                        "last_built": int(getattr(idx, "last_built", 0)),
                    }
                )
            model = "clip-vit-b32"
        return sexp_response(
            {
                "type": "PATTERN_SIMILARITY_RESULT",
                "status": "ok",
                "model": model,
                "indices": indices,
            }
        )

    if action == "BUILD_INDEX":
        symbol = data.get("symbol")
        if not symbol:
            return _error_response("Missing symbol")
        # v1: start a background build (implemented in a later task)
        # NOTE: BUILD_INDEX is intentionally asynchronous. v1 implements only an in-memory build
        # helper for tests; historical building is implemented later.
        return sexp_response(
            {
                "type": "PATTERN_SIMILARITY_RESULT",
                "status": "ok",
                "message": "build started",
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
            nn_idx, dists = _knn_search(idx.embeddings, q, k)
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
