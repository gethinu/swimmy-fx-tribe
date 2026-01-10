import zmq
import json
import time

CONTEXT = zmq.Context()
SOCKET = CONTEXT.socket(zmq.PUSH)
SOCKET.connect("tcp://localhost:5562")

payload = {
    "webhook": "https://discord.com/api/webhooks/1455558971367882762/gOf_SFW0JvQd7tX1CqSGZbtGMcOz5wcwAiVgPhvCzEp7QAkl1g8u1dNx9qhfXbt5lAyB",
    "data": {
        "embeds": [
            {
                "title": "🐟 GBPUSD DIRECT TEST",
                "description": "Direct Python -> Notifier Test",
                "color": 3066993,
            }
        ]
    },
}

print("Sending direct test message...")
SOCKET.send_json(payload)
time.sleep(1)
print("Sent.")
