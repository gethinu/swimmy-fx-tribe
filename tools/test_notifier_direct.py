#!/usr/bin/env python3
import zmq
import json
import os


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

# We need a valid webhook URL to test.
# We will read it from the environment or use a hardcoded fallback if accessible.
# Since we don't have easy env access here, let's try to get it from .env or just ask the user?
# Wait, I can read .env
# But better: notifier.py expects the webhook URL in the payload.


def get_webhook():
    # Try to grab a webhook from the config file or just use the Recruit one mentioned in globals
    # Recruit: https://discord.com/api/webhooks/1413195529680191538/OLcthUXpQr6fM32o8Vx-zlEJfgDTXfq14RPPSJdEKBJJZUUVBWJ9Hwq7ZPNFOMDkmQSW
    return "https://discord.com/api/webhooks/1413195529680191538/OLcthUXpQr6fM32o8Vx-zlEJfgDTXfq14RPPSJdEKBJJZUUVBWJ9Hwq7ZPNFOMDkmQSW"


def main():
    webhook = get_webhook()
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
