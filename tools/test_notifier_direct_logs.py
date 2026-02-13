#!/usr/bin/env python3
"""
Send a one-shot test embed to the "system logs" Discord webhook via Notifier service.

SECURITY:
  - Do NOT hardcode webhook URLs here.
  - Do NOT print webhook URLs (tokens are embedded).
"""

from __future__ import annotations

import json
import os
import sys
from pathlib import Path

import zmq


def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


def get_system_logs_webhook() -> tuple[str, str]:
    for key in ("SWIMMY_DISCORD_SYSTEM_LOGS", "SWIMMY_DISCORD_WEBHOOK"):
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
    port = _env_int("SWIMMY_PORT_NOTIFIER", 5562)
    webhook, src_key = get_system_logs_webhook()
    if not webhook:
        print("❌ SWIMMY_DISCORD_SYSTEM_LOGS not set.", file=sys.stderr)
        raise SystemExit(1)

    ctx = zmq.Context()
    sock = ctx.socket(zmq.PUSH)
    sock.connect(f"tcp://localhost:{port}")

    payload = {
        "embeds": [
            {
                "title": "Status/Logs Channel Test",
                "description": "Testing System Logs webhook via Notifier.",
                "color": 15158332,
            }
        ]
    }

    print(f"Sending test embed via Notifier (env={src_key}, port={port})...", flush=True)
    sock.send_string(build_notifier_message(webhook, payload))
    print("Sent.", flush=True)


if __name__ == "__main__":
    main()

