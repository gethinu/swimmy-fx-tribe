#!/usr/bin/env python3
"""
Send a one-shot test embed through the Notifier service (ZMQ PUSH -> tools/notifier.py).

SECURITY:
  - Do NOT hardcode Discord webhook URLs in this repo.
  - Read webhook URL from environment variables.
"""

from __future__ import annotations

import json
import os
import sys
import time
from pathlib import Path

import zmq


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default


def _pick_webhook() -> tuple[str, str]:
    # Prefer consolidated names from config/.env.template.
    for key in ("SWIMMY_DISCORD_ALERTS", "SWIMMY_DISCORD_SYSTEM_LOGS", "SWIMMY_DISCORD_WEBHOOK"):
        value = os.getenv(key, "").strip()
        if value:
            return value, key
    return "", ""


BASE_DIR = resolve_base_dir()
PYTHON_SRC = BASE_DIR / "src" / "python"
if str(PYTHON_SRC) not in sys.path:
    sys.path.insert(0, str(PYTHON_SRC))

from aux_sexp import sexp_request  # noqa: E402


def build_notifier_message(webhook: str, payload: dict) -> str:
    return sexp_request(
        {
            "type": "NOTIFIER",
            "action": "SEND",
            "webhook": webhook,
            "payload_json": json.dumps(payload, ensure_ascii=False),
        }
    )


def main() -> None:
    webhook, src_key = _pick_webhook()
    if not webhook:
        print("❌ No Discord webhook configured. Set SWIMMY_DISCORD_ALERTS or SWIMMY_DISCORD_SYSTEM_LOGS.", file=sys.stderr)
        raise SystemExit(1)

    port = _env_int("SWIMMY_PORT_NOTIFIER", 5562)
    ctx = zmq.Context()
    sock = ctx.socket(zmq.PUSH)
    sock.connect(f"tcp://localhost:{port}")

    print(f"Sending test notification via Notifier (env={src_key}, port={port})...", flush=True)

    payload = {
        "embeds": [
            {
                "title": "Debug Test",
                "description": "This is a test from tools/test_notifier.py. If you see this, Notifier is working.",
                "color": 3447003,
            }
        ]
    }

    sock.send_string(build_notifier_message(webhook, payload))
    print("Sent.", flush=True)
    time.sleep(0.2)


if __name__ == "__main__":
    main()

