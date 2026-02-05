import json
import os
import sys
import time
from pathlib import Path

import zmq

WEBHOOK_URL = "https://discord.com/api/webhooks/1455351646962979000/p9cWLthwfP8gB1TgvukeJixren_kgJvjjIq-oVQ-doAsX_C4chGBQyf05Eh_iDmLu1Dy"


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


def build_notifier_message(webhook, payload):
    return sexp_request(
        {
            "type": "NOTIFIER",
            "action": "SEND",
            "webhook": webhook,
            "payload_json": json.dumps(payload, ensure_ascii=False),
        }
    )


def main():
    def _env_int(key: str, default: int) -> int:
        val = os.getenv(key, "").strip()
        if not val:
            return default
        try:
            return int(val)
        except ValueError:
            return default

    ctx = zmq.Context()
    sock = ctx.socket(zmq.PUSH)
    port = _env_int("SWIMMY_PORT_NOTIFIER", 5562)
    sock.connect(f"tcp://localhost:{port}")

    print("Sending test notification...")

    payload = {
        "webhook": WEBHOOK_URL,
        "data": {
            "embeds": [
                {
                    "title": "🧪 Debug Test",
                    "description": "This is a test from tools/test_notifier.py. If you see this, Notifier is working.",
                    "color": 3447003,
                }
            ]
        },
    }

    sock.send_string(build_notifier_message(WEBHOOK_URL, payload["data"]))
    print("Sent.")

    # Wait a bit to ensure ZMQ flushes in this short script
    time.sleep(1)


if __name__ == "__main__":
    main()
