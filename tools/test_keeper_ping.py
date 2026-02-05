import os
import sys
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

from aux_sexp import sexp_request


def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default

context = zmq.Context()
socket = context.socket(zmq.REQ)
port = _env_int("SWIMMY_PORT_DATA_KEEPER", 5561)
socket.connect(f"tcp://localhost:{port}")
socket.setsockopt(zmq.RCVTIMEO, 2000)

print("Sending STATUS request to Data Keeper...")
try:
    msg = sexp_request({"type": "DATA_KEEPER", "action": "STATUS"})
    socket.send_string(msg)
    msg = socket.recv_string()
    print("Received:", msg)
except Exception as e:
    print("Error/Timeout:", e)
