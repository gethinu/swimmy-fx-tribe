#!/usr/bin/env python3
import json
import os
import sys
from pathlib import Path

import zmq

def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default

NOTIFIER_PORT = _env_int("SWIMMY_PORT_NOTIFIER", 5562)
# The URL from .env (System Logs)
WEBHOOK_URL = "https://discord.com/api/webhooks/1460946551580004515/CSTIPAVwzqBdhnon9_KZ38hNVgfrK3AhoRPNXcOW_0aeLtUuN53IFlz0csvIiUXM7qiy"


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
    ctx = zmq.Context()
    sock = ctx.socket(zmq.PUSH)
    sock.connect(f"tcp://localhost:{NOTIFIER_PORT}")

    payload = {
        "embeds": [
            {
                "title": "🧪 Status/Logs Channel Test",
                "description": "Testing System Logs Webhook directly.",
                "color": 15158332,
            }
        ]
    }

    print(f"Sending to System Logs Webhook via port {NOTIFIER_PORT}...")
    sock.send_string(build_notifier_message(WEBHOOK_URL, payload))
    print("Sent.")


if __name__ == "__main__":
    main()
