#!/usr/bin/env python3
"""Send test heartbeat notification to Discord"""
import requests
import json
import os
from datetime import datetime

# Discord webhook URL for heartbeat channel (dedicated)
WEBHOOK_URL = "https://discord.com/api/webhooks/1413195529680191538/OLcthUXpQr6fM32o8Vx-zlEJfgDTXfq14RPPSJdEKBJJZUUVBWJ9Hwq7ZPNFOMDkmQSW"

message = f"""🐟 **Swimmy Heartbeat** - {datetime.now().strftime('%Y/%m/%d %H:%M')}

📊 PnL: ¥0
🏦 Treasury: ¥0
📈 Trades today: 0
⏰ Status: TEST NOTIFICATION

_System is running normally._ ✅"""

payload = {"content": message, "embeds": []}

try:
    response = requests.post(WEBHOOK_URL, json=payload, timeout=10)
    if response.status_code in [200, 204]:
        print("✅ Test heartbeat sent to Discord!")
    else:
        print(f"❌ Failed: {response.status_code} - {response.text}")
except Exception as e:
    print(f"❌ Error: {e}")
