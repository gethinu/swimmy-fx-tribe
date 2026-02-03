#!/usr/bin/env python3
"""Send test heartbeat notification to Discord."""
from __future__ import annotations

from datetime import datetime
import os

import requests


def build_message() -> str:
    return f"""🐟 **Swimmy Heartbeat** - {datetime.now().strftime('%Y/%m/%d %H:%M')}

📊 PnL: ¥0
🏦 Treasury: ¥0
📈 Trades today: 0
⏰ Status: TEST NOTIFICATION

_System is running normally._ ✅"""


def send_heartbeat(webhook_url: str) -> int:
    payload = {"content": build_message(), "embeds": []}
    response = requests.post(webhook_url, json=payload, timeout=10)
    return response.status_code


def main() -> int:
    webhook_url = os.getenv("SWIMMY_HEARTBEAT_WEBHOOK", "").strip()
    if not webhook_url:
        print("❌ SWIMMY_HEARTBEAT_WEBHOOK is not set")
        return 1

    try:
        status_code = send_heartbeat(webhook_url)
    except Exception as exc:
        print(f"❌ Error: {exc}")
        return 1

    if status_code in (200, 204):
        print("✅ Test heartbeat sent to Discord!")
        return 0

    print(f"❌ Failed: {status_code}")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
