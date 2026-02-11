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

Message Format (S-expression alist):
    ((type . "NOTIFIER")
     (schema_version . 1)
     (action . "SEND")
     (webhook . "https://discord.com/api/webhooks/...")
     (payload . ((embeds . (((title . "Swimmy")
                             (description . "...")
                             (color . 3447003)))))))
    ;; payload_json (JSON string) is accepted for backward compatibility.
"""

import json
import os
import sys
import threading
import time
from pathlib import Path

import requests
import zmq
from collections import deque

# Configuration
def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default

ZMQ_PORT = _env_int("SWIMMY_PORT_NOTIFIER", 5562)  # Notifier Port
RETRY_LIMIT = 3
RATE_LIMIT_DELAY = 1.0  # Seconds between requests to same webhook
PREVIEW_MAX_LEN = _env_int("SWIMMY_NOTIFIER_PREVIEW_MAX_LEN", 120)
STATUS_PREVIEW_MAX_LEN = _env_int("SWIMMY_NOTIFIER_STATUS_PREVIEW_MAX_LEN", 360)

# Queue for outgoing messages
# (webhook_url, payload)
message_queue = deque()


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
PYTHON_SRC = BASE_DIR / "src" / "python"
if str(PYTHON_SRC) not in sys.path:
    sys.path.insert(0, str(PYTHON_SRC))

from aux_sexp import parse_aux_request


def parse_notifier_message(message: str):
    data = parse_aux_request(message)
    msg_type = str(data.get("type", "")).upper()
    if msg_type != "NOTIFIER":
        raise ValueError(f"Invalid type: {msg_type}")
    schema_version = data.get("schema_version")
    if schema_version != 1:
        raise ValueError("Unsupported schema_version")
    action = str(data.get("action", "")).upper()
    if action != "SEND":
        raise ValueError(f"Invalid action: {action}")
    webhook = data.get("webhook")
    payload_json_present = "payload_json" in data
    payload_present = "payload" in data
    payload_json = data.get("payload_json")
    payload_sexp = data.get("payload")
    if not webhook:
        raise ValueError("Missing webhook")
    if payload_present and payload_json_present:
        raise ValueError("Specify either payload or payload_json")
    if payload_present:
        if payload_sexp is None:
            raise ValueError("payload is empty")
        if not isinstance(payload_sexp, (dict, list)):
            raise ValueError("payload must be an alist or list")
        payload = payload_sexp
    elif payload_json_present:
        if not payload_json:
            raise ValueError("payload_json is empty")
        payload = json.loads(payload_json)
    else:
        raise ValueError("Missing payload or payload_json")
    return webhook, payload


def format_payload_preview(payload: dict, max_len: int = 120) -> str:
    """Create a short, single-line preview of the payload for logs."""
    try:
        text = ""
        embeds = payload.get("embeds") or []
        if embeds and isinstance(embeds, list) and isinstance(embeds[0], dict):
            desc = embeds[0].get("description")
            if isinstance(desc, str) and desc.strip():
                text = desc
            else:
                title = embeds[0].get("title")
                if isinstance(title, str) and title.strip():
                    text = title
        if not text:
            content = payload.get("content")
            if isinstance(content, str) and content.strip():
                text = content
        if not text:
            return ""
        text = " ".join(text.split())
        if max_len > 3 and len(text) > max_len:
            text = text[: max_len - 1] + "…"
        return text
    except Exception:
        return ""


def select_preview_max_len(title: str) -> int:
    """Return preview max length based on embed title."""
    if isinstance(title, str) and "status" in title.casefold():
        return max(PREVIEW_MAX_LEN, STATUS_PREVIEW_MAX_LEN)
    return PREVIEW_MAX_LEN


def process_queue():
    """Worker thread to process the message queue with rate limiting and retry."""
    failed_count = {}  # Track retry count per message

    while True:
        if not message_queue:
            time.sleep(0.1)
            continue

        webhook_url, payload = message_queue.popleft()
        print(f"[DEBUG] Worker popped message for {webhook_url[:40]}...", flush=True)
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
                    f"[NOTIFIER] Error {response.status_code} for {webhook_url}: {response.text}",
                    flush=True,
                )
                # Don't retry HTTP errors (webhook invalid, etc)
                if msg_id in failed_count:
                    del failed_count[msg_id]

            else:
                # Success - cleanup
                print(f"[NOTIFIER] Sent to {webhook_url[:60]}...", flush=True)
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
    print("[NOTIFIER] Starting worker thread...", flush=True)
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
                webhook, payload = parse_notifier_message(msg_str)
                message_queue.append((webhook, payload))
                title = payload.get("embeds", [{}])[0].get("title", "Message")
                preview = format_payload_preview(
                    payload, max_len=select_preview_max_len(title)
                )
                if preview:
                    print(f"[NOTIFIER] Queued: {title} | {preview}", flush=True)
                else:
                    print(f"[NOTIFIER] Queued: {title}", flush=True)

            except json.JSONDecodeError:
                print(f"[NOTIFIER] Invalid payload_json")
            except Exception as e:
                print(f"[NOTIFIER] Invalid message format: {e}")

        except KeyboardInterrupt:
            print("\n[NOTIFIER] Shutting down...")
            break
        except Exception as e:
            print(f"[NOTIFIER] Critical Error: {e}")

    socket.close()
    context.term()


if __name__ == "__main__":
    main()
