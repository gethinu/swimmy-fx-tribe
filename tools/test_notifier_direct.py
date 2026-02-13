#!/usr/bin/env python3
import os
import sys
import zmq
from pathlib import Path
import json


# Port 5562 is where tools/notifier.py listens
_ENV_LOADED = False


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME", "").strip()
    if env:
        return Path(env)
    return Path(__file__).resolve().parents[1]


def load_env() -> None:
    global _ENV_LOADED
    if _ENV_LOADED:
        return
    _ENV_LOADED = True
    env_path = resolve_base_dir() / ".env"
    if not env_path.exists():
        return
    with env_path.open("r") as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("#") or "=" not in line:
                continue
            key, val = line.split("=", 1)
            key = key.strip()
            if key.startswith("export "):
                key = key[len("export ") :].strip()
            val = val.strip().strip('"').strip("'")
            if not os.getenv(key, "").strip():
                os.environ[key] = val


def _env_int(key: str, default: int) -> int:
    load_env()
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default


# Port 5562 is where tools/notifier.py listens
NOTIFIER_PORT = _env_int("SWIMMY_PORT_NOTIFIER", 5562)

BASE_DIR = resolve_base_dir()
PYTHON_SRC = BASE_DIR / "src" / "python"
if str(PYTHON_SRC) not in sys.path:
    sys.path.insert(0, str(PYTHON_SRC))

from aux_sexp import sexp_request


def build_notifier_message(webhook, payload):
    return sexp_request(
        {
            "type": "NOTIFIER",
            "action": "SEND",
            "webhook": webhook,
            "payload_json": json.dumps(payload, ensure_ascii=False),
        }
    )


def get_webhook() -> str:
    load_env()
    return os.getenv("SWIMMY_DISCORD_RECRUIT", "").strip()


def main() -> None:
    webhook = get_webhook()
    if not webhook:
        print("❌ SWIMMY_DISCORD_RECRUIT not set.", file=sys.stderr)
        sys.exit(1)

    # Never print webhook URLs (tokens are embedded in the URL).
    print("Target Webhook: (configured)", flush=True)

    ctx = zmq.Context()
    sock = ctx.socket(zmq.PUSH)
    sock.connect(f"tcp://localhost:{NOTIFIER_PORT}")

    payload = {
        "embeds": [
            {
                "title": "🧪 Direct Notifier Test",
                "description": "If you see this, Notifier.py is working correctly.",
                "color": 3447003,
            }
        ]
    }

    print(f"Sending to port {NOTIFIER_PORT}...")
    sock.send_string(build_notifier_message(webhook, payload))
    print("Sent. Check Discord.")


if __name__ == "__main__":
    main()
