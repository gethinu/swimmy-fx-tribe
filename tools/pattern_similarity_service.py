import os
import sys
import time
import threading
from pathlib import Path

import zmq


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


_state_lock = threading.Lock()
_indices = {}  # (symbol, tf) -> {"count": int, "last_built": int, "model": str}
_build_threads = {}  # (symbol, tf) -> Thread


def _error_response(message: str) -> str:
    return sexp_response(
        {
            "type": "PATTERN_SIMILARITY_RESULT",
            "status": "error",
            "error": message,
        }
    )


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
            for (symbol, tf), meta in sorted(_indices.items()):
                indices.append(
                    {
                        "symbol": symbol,
                        "timeframe": tf,
                        "count": int(meta.get("count", 0)),
                        "last_built": int(meta.get("last_built", 0)),
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
        with _state_lock:
            _indices[(str(symbol).upper(), "ALL")] = {
                "count": 0,
                "last_built": int(time.time()),
                "model": "clip-vit-b32",
            }
        return sexp_response(
            {
                "type": "PATTERN_SIMILARITY_RESULT",
                "status": "ok",
                "message": "build started",
            }
        )

    if action == "QUERY":
        # v1 skeleton: return an explicit error until an index exists
        symbol = data.get("symbol")
        tf = data.get("timeframe")
        if not symbol:
            return _error_response("Missing symbol")
        if not tf:
            return _error_response("Missing timeframe")
        key = (str(symbol).upper(), str(tf).upper())
        with _state_lock:
            has_index = key in _indices
        if not has_index:
            return _error_response("index not built")
        return _error_response("query not implemented")

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

