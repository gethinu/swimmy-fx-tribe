#!/usr/bin/env python3
import zmq
import json
import os

NOTIFIER_PORT = 5562
# The URL from .env (System Logs)
WEBHOOK_URL = "https://discord.com/api/webhooks/1460946551580004515/CSTIPAVwzqBdhnon9_KZ38hNVgfrK3AhoRPNXcOW_0aeLtUuN53IFlz0csvIiUXM7qiy"


def main():
    ctx = zmq.Context()
    sock = ctx.socket(zmq.PUSH)
    sock.connect(f"tcp://localhost:{NOTIFIER_PORT}")

    payload = {
        "webhook": WEBHOOK_URL,
        "data": {
            "embeds": [
                {
                    "title": "🧪 Status/Logs Channel Test",
                    "description": "Testing System Logs Webhook directly.",
                    "color": 15158332,
                }
            ]
        },
    }

    print(f"Sending to System Logs Webhook via port {NOTIFIER_PORT}...")
    sock.send_json(payload)
    print("Sent.")


if __name__ == "__main__":
    main()
