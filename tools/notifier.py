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
    """Worker thread to process the message queue with rate limiting and retry."""
    failed_count = {}  # Track retry count per message

    while True:
        if not message_queue:
            time.sleep(0.1)
            continue

        webhook_url, payload = message_queue.popleft()
        msg_id = id(payload)  # Unique ID for tracking retries
        retry_count = failed_count.get(msg_id, 0)

        try:
            headers = {"Content-Type": "application/json"}
            response = requests.post(
                webhook_url, json=payload, headers=headers, timeout=10
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
                # Don't retry HTTP errors (webhook invalid, etc)
                if msg_id in failed_count:
                    del failed_count[msg_id]

            else:
                # Success - cleanup
                if msg_id in failed_count:
                    del failed_count[msg_id]

            time.sleep(0.2)  # Basic global rate limit safety

        except (
            requests.exceptions.ConnectionError,
            requests.exceptions.Timeout,
            requests.exceptions.RequestException,
        ) as e:
            # Network/DNS failure - retry with exponential backoff
            retry_count += 1
            failed_count[msg_id] = retry_count

            if retry_count <= RETRY_LIMIT:
                backoff = min(30, 2**retry_count)  # 2, 4, 8... max 30s
                print(
                    f"[NOTIFIER] Network error (retry {retry_count}/{RETRY_LIMIT}). "
                    f"Backoff {backoff}s: {type(e).__name__}",
                    flush=True,
                )
                time.sleep(backoff)
                message_queue.appendleft((webhook_url, payload))  # Requeue
            else:
                print(
                    f"[NOTIFIER] DROPPED after {RETRY_LIMIT} retries: {type(e).__name__}",
                    flush=True,
                )
                del failed_count[msg_id]

        except Exception as e:
            print(f"[NOTIFIER] Unexpected error: {e}", flush=True)


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
