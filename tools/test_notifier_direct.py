#!/usr/bin/env python3
import zmq
import json
import os
import sys


def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default

# Port 5562 is where tools/notifier.py listens
NOTIFIER_PORT = _env_int("SWIMMY_PORT_NOTIFIER", 5562)


def get_webhook() -> str:
    return os.getenv("SWIMMY_DISCORD_RECRUIT", "").strip()


def main() -> None:
    webhook = get_webhook()
    if not webhook:
        print("❌ SWIMMY_DISCORD_RECRUIT not set.", file=sys.stderr)
        sys.exit(1)

    print(f"Target Webhook: {webhook[-20:]}")

    ctx = zmq.Context()
    sock = ctx.socket(zmq.PUSH)
    sock.connect(f"tcp://localhost:{NOTIFIER_PORT}")

    payload = {
        "webhook": webhook,
        "data": {
            "embeds": [
                {
                    "title": "🧪 Direct Notifier Test",
                    "description": "If you see this, Notifier.py is working correctly.",
                    "color": 3447003,
                }
            ]
        },
    }

    print(f"Sending to port {NOTIFIER_PORT}...")
    sock.send_json(payload)
    print("Sent. Check Discord.")


if __name__ == "__main__":
    main()
