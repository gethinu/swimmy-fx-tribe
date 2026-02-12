#!/usr/bin/env python3
"""Detect systemd drift for Swimmy backtest service."""

from __future__ import annotations

import re
import subprocess
import sys
from typing import Optional, Tuple

SERVICE_NAME = "swimmy-backtest.service"
LISTEN_PORT = 5580


def parse_listener_pid_from_ss(ss_output: str, port: int = LISTEN_PORT) -> Optional[int]:
    port_token = f":{port}"
    for line in (ss_output or "").splitlines():
        if port_token not in line or "LISTEN" not in line:
            continue
        match = re.search(r"pid=(\d+)", line)
        if match:
            return int(match.group(1))
    return None


def assess_backtest_drift(
    *,
    service_state: str,
    service_main_pid: int,
    listener_pid: Optional[int],
) -> Tuple[bool, str]:
    state = (service_state or "").strip().lower()
    if state in {"activating", "deactivating", "reloading"}:
        return True, f"service in transitional state ({state}); skip drift decision"

    if state == "active":
        if service_main_pid <= 0:
            return False, "service active but MainPID is missing"
        if listener_pid is None:
            return False, "service active but no 5580 listener"
        if listener_pid != service_main_pid:
            return (
                False,
                f"service/listener pid mismatch (MainPID={service_main_pid}, listener={listener_pid})",
            )
        return True, f"service/listener aligned (pid={service_main_pid})"

    if listener_pid is not None:
        return (
            False,
            f"service {state or 'unknown'} but 5580 listener exists (listener={listener_pid})",
        )

    return True, f"service {state or 'unknown'} and no 5580 listener"


def _run(args: list[str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(args, capture_output=True, text=True)


def _read_service_state(service_name: str) -> Optional[str]:
    cp = _run(["systemctl", "is-active", service_name])
    out = (cp.stdout or "").strip()
    if out:
        return out
    return None


def _read_service_main_pid(service_name: str) -> int:
    cp = _run(["systemctl", "show", "-p", "MainPID", "--value", service_name])
    raw = (cp.stdout or "").strip()
    if not raw:
        return 0
    try:
        return int(raw)
    except ValueError:
        return 0


def _read_listener_pid(port: int = LISTEN_PORT) -> Optional[int]:
    cp = _run(["ss", "-tulnp"])
    if cp.returncode != 0:
        return None
    return parse_listener_pid_from_ss(cp.stdout, port=port)


def main() -> int:
    state = _read_service_state(SERVICE_NAME)
    if not state:
        print(f"[FAIL] unable to read service state: {SERVICE_NAME}")
        return 2

    main_pid = _read_service_main_pid(SERVICE_NAME)
    listener_pid = _read_listener_pid(LISTEN_PORT)
    ok, message = assess_backtest_drift(
        service_state=state,
        service_main_pid=main_pid,
        listener_pid=listener_pid,
    )

    level = "OK" if ok else "FAIL"
    print(
        f"[{level}] backtest drift check: state={state} main_pid={main_pid} "
        f"listener_pid={listener_pid} :: {message}"
    )
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
