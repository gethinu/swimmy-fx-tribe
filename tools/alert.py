#!/usr/bin/env python3
"""
alert.py - Simple CLI for Discord Alerts
Usage: python3 tools/alert.py "Message" [--type info/error/success]
"""
import os
import sys
import requests
import argparse


# Load .env manually if needed, or rely on env vars
def load_env():
    env_path = os.path.join(os.path.dirname(__file__), "..", ".env")
    if os.path.exists(env_path):
        with open(env_path) as f:
            for line in f:
                if line.strip() and not line.startswith("#"):
                    key, val = line.strip().split("=", 1)
                    os.environ[key] = val.strip("\"'")


load_env()

WEBHOOK_URL = os.getenv("SWIMMY_DISCORD_ALERTS") or os.getenv(
    "SWIMMY_DISCORD_SYSTEM_LOGS"
)


def send_alert(message, msg_type="info"):
    if not WEBHOOK_URL:
        print("❌ Error: SWIMMY_DISCORD_ALERTS not found in .env")
        return

    colors = {
        "info": 3447003,  # Blue
        "error": 15158332,  # Red
        "success": 3066993,  # Green
    }

    color = colors.get(msg_type, 3447003)

    payload = {
        "embeds": [
            {
                "title": "🧪 Validation Campaign",
                "description": message,
                "color": color,
                "footer": {"text": "Swimmy Deep Validation Service"},
            }
        ]
    }

    try:
        resp = requests.post(WEBHOOK_URL, json=payload, timeout=5)
        if resp.status_code == 204:
            print("✅ Discord Alert Sent.")
        else:
            print(f"❌ Failed to send alert: {resp.status_code}")
    except Exception as e:
        print(f"❌ Error sending alert: {e}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("message", help="Message to send")
    parser.add_argument("--type", choices=["info", "error", "success"], default="info")
    args = parser.parse_args()

    send_alert(args.message, args.type)
