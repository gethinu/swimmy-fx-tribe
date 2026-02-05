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
from pathlib import Path

# Configuration
def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default

def _env_float(key: str, default: float) -> float:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return float(val)
    except ValueError:
        return default

ZMQ_PORT = _env_int("SWIMMY_PORT_NOTIFIER", 5562)  # Notifier Port
RETRY_LIMIT = 3
RATE_LIMIT_DELAY = 1.0  # Seconds between requests to same webhook


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME", "").strip()
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
NOTIFIER_METRICS_FILE = os.getenv(
    "SWIMMY_NOTIFIER_METRICS_FILE", str(BASE_DIR / "data" / "notifier_metrics.sexp")
).strip()
NOTIFIER_METRICS_INTERVAL_SEC = _env_float("SWIMMY_NOTIFIER_METRICS_INTERVAL_SEC", 5.0)
NOTIFIER_QUEUE_MAX = max(1, _env_int("SWIMMY_NOTIFIER_QUEUE_MAX", 1000))

# Queue for outgoing messages
# (webhook_url, payload)
message_queue = deque()
queue_lock = threading.Lock()
_metrics_drops = 0
_metrics_errors = 0


def _sexp_atom(value) -> str:
    if value is None:
        return "nil"
    if value is True:
        return "t"
    if value is False:
        return "nil"
    if isinstance(value, str):
        return json.dumps(value, ensure_ascii=False)
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        return str(value)
    return json.dumps(str(value), ensure_ascii=False)


def _sexp_alist(payload: dict) -> str:
    parts = []
    for k, v in payload.items():
        key = str(k).lower()
        parts.append(f"({key} . {_sexp_atom(v)})")
    return f"({' '.join(parts)})"


def _atomic_write(path: str, content: str) -> None:
    target = Path(path)
    target.parent.mkdir(parents=True, exist_ok=True)
    tmp = target.with_name(target.name + ".tmp")
    tmp.write_text(content, encoding="utf-8")
    os.replace(tmp, target)


def write_notifier_metrics_sexp(
    path: str,
    *,
    queue_len: int,
    drops: int,
    errors: int,
    timestamp: int | None = None,
) -> None:
    """Write Notifier health metrics as a single S-expression alist."""
    ts = int(time.time()) if timestamp is None else int(timestamp)
    payload = {
        "schema_version": 1,
        "timestamp": ts,
        "queue_len": int(queue_len),
        "drops": int(drops),
        "errors": int(errors),
    }
    _atomic_write(path, _sexp_alist(payload))


def _enqueue_message(webhook_url: str, payload: dict) -> None:
    global _metrics_drops
    with queue_lock:
        while len(message_queue) >= NOTIFIER_QUEUE_MAX:
            message_queue.popleft()  # drop oldest
            _metrics_drops += 1
        message_queue.append((webhook_url, payload))


def _inc_error() -> None:
    global _metrics_errors
    with queue_lock:
        _metrics_errors += 1


def _metrics_loop() -> None:
    last_print = 0.0
    interval = max(0.2, float(NOTIFIER_METRICS_INTERVAL_SEC))
    while True:
        try:
            with queue_lock:
                qlen = len(message_queue)
                drops = int(_metrics_drops)
                errors = int(_metrics_errors)
            write_notifier_metrics_sexp(
                NOTIFIER_METRICS_FILE,
                queue_len=qlen,
                drops=drops,
                errors=errors,
            )
            now = time.time()
            if now - last_print >= 30:
                last_print = now
                print(
                    f"[NOTIFIER] metrics queue_len={qlen} drops={drops} errors={errors}",
                    flush=True,
                )
        except Exception as e:
            print(f"[NOTIFIER] Metrics write error: {e}", flush=True)
        time.sleep(interval)


def process_queue():
    """Worker thread to process the message queue with rate limiting and retry."""
    failed_count = {}  # Track retry count per message

    while True:
        with queue_lock:
            if message_queue:
                webhook_url, payload = message_queue.popleft()
            else:
                webhook_url, payload = None, None

        if not webhook_url:
            time.sleep(0.1)
            continue

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
                with queue_lock:
                    message_queue.appendleft((webhook_url, payload))
                _inc_error()

            elif response.status_code >= 400:
                print(
                    f"[NOTIFIER] Error {response.status_code} for {webhook_url}: {response.text}",
                    flush=True,
                )
                # Don't retry HTTP errors (webhook invalid, etc)
                _inc_error()
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
            _inc_error()
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
                with queue_lock:
                    message_queue.appendleft((webhook_url, payload))  # Requeue
            else:
                print(
                    f"[NOTIFIER] DROPPED after {RETRY_LIMIT} retries: {type(e).__name__}",
                    flush=True,
                )
                global _metrics_drops
                with queue_lock:
                    _metrics_drops += 1
                del failed_count[msg_id]

        except Exception as e:
            _inc_error()
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

    # Start Metrics Thread
    print("[NOTIFIER] Starting metrics thread...", flush=True)
    metrics = threading.Thread(target=_metrics_loop, daemon=True)
    metrics.start()

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
                    _enqueue_message(webhook, payload)
                    print(
                        f"[NOTIFIER] Queued: {payload.get('embeds', [{}])[0].get('title', 'Message')}",
                        flush=True,
                    )
                else:
                    print(f"[NOTIFIER] Invalid message format")
                    _inc_error()

            except json.JSONDecodeError:
                print(f"[NOTIFIER] Invalid JSON received")
                _inc_error()

        except KeyboardInterrupt:
            print("\n[NOTIFIER] Shutting down...")
            break
        except Exception as e:
            print(f"[NOTIFIER] Critical Error: {e}")
            _inc_error()

    socket.close()
    context.term()


if __name__ == "__main__":
    main()
