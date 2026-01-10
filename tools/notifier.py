#!/usr/bin/env python3
"""
notifier.py - Async Discord Notification Service
================================================
Expert Panel Approved (2026-01-07) - Phase 2

Purpose:
    Receives notification messages from Swimmy (Lisp) via ZMQ PULL socket
    and posts them to Discord Webhooks asynchronously.
    Removes network I/O latency from the main trading loop.

Architecture:
    [Swimmy Lisp] --(ZMQ PUSH)--> [notifier.py] --(HTTP)--> [Discord]

Usage:
    python3 tools/notifier.py

Message Format (JSON):
    {
        "webhook": "https://discord.com/api/webhooks/...",
        "content_type": "embed" | "text",
        "data": { ... }
    }
"""

import zmq
import json
import time
import requests
import os
import threading
from collections import deque

# Configuration
ZMQ_PORT = 5562  # Notifier Port
RETRY_LIMIT = 3
RATE_LIMIT_DELAY = 1.0  # Seconds between requests to same webhook

# Queue for outgoing messages
# (webhook_url, payload)
message_queue = deque()


def process_queue():
    """Worker thread to process the message queue with rate limiting."""
    while True:
        if not message_queue:
            time.sleep(0.1)
            continue

        webhook_url, payload = message_queue.popleft()

        try:
            # Simple rate limiting enforcement
            # (In a real pro system, we'd track per-webhook buckets)

            headers = {"Content-Type": "application/json"}
            response = requests.post(
                webhook_url, json=payload, headers=headers, timeout=5
            )

            if response.status_code == 429:
                # Rate limited - requeue and backoff
                retry_after = response.json().get("retry_after", 1)
                print(f"[NOTIFIER] Rate limited. Waiting {retry_after}s...", flush=True)
                time.sleep(retry_after)
                message_queue.appendleft((webhook_url, payload))

            elif response.status_code >= 400:
                print(
                    f"[NOTIFIER] Error {response.status_code}: {response.text}",
                    flush=True,
                )

            else:
                pass
                # Success - silent

            time.sleep(0.2)  # Basic global rate limit safety

        except Exception as e:
            print(f"[NOTIFIER] Delivery failed: {e}", flush=True)


def main():
    print("=" * 60)
    print("  🔔 NOTIFIER SERVICE - Async Discord Relay")
    print("  Expert Panel Phase 2")
    print("=" * 60)

    # Start Worker Thread
    worker = threading.Thread(target=process_queue, daemon=True)
    worker.start()

    # Setup ZMQ PULL socket
    context = zmq.Context()
    socket = context.socket(zmq.PULL)
    socket.bind(f"tcp://*:{ZMQ_PORT}")
    print(f"[NOTIFIER] Listening on port {ZMQ_PORT} (PULL)")

    while True:
        try:
            # Wait for message
            msg_str = socket.recv_string()

            try:
                msg = json.loads(msg_str)
                webhook = msg.get("webhook")
                payload = msg.get("data")

                if webhook and payload:
                    message_queue.append((webhook, payload))
                    print(
                        f"[NOTIFIER] Queued: {payload.get('embeds', [{}])[0].get('title', 'Message')}",
                        flush=True,
                    )
                else:
                    print(f"[NOTIFIER] Invalid message format")

            except json.JSONDecodeError:
                print(f"[NOTIFIER] Invalid JSON received")

        except KeyboardInterrupt:
            print("\n[NOTIFIER] Shutting down...")
            break
        except Exception as e:
            print(f"[NOTIFIER] Critical Error: {e}")

    socket.close()
    context.term()


if __name__ == "__main__":
    main()
