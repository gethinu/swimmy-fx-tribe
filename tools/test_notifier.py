import zmq
import json
import time

WEBHOOK_URL = "https://discord.com/api/webhooks/1455351646962979000/p9cWLthwfP8gB1TgvukeJixren_kgJvjjIq-oVQ-doAsX_C4chGBQyf05Eh_iDmLu1Dy"


def main():
    ctx = zmq.Context()
    sock = ctx.socket(zmq.PUSH)
    sock.connect("tcp://localhost:5562")

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

    sock.send_json(payload)
    print("Sent.")

    # Wait a bit to ensure ZMQ flushes in this short script
    time.sleep(1)


if __name__ == "__main__":
    main()
