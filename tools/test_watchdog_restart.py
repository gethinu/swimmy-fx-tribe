#!/usr/bin/env python3
"""
Minimal regression tests for watchdog service revival logic.

Run:
  python3 tools/test_watchdog_restart.py
"""

from __future__ import annotations

import os
import sys
import signal
import warnings
from unittest import mock


def _completed(returncode: int, stdout: str = "", stderr: str = ""):
    cp = mock.Mock()
    cp.returncode = returncode
    cp.stdout = stdout
    cp.stderr = stderr
    return cp


def test_restart_service_falls_back_to_kill_main_pid_on_auth_failure():
    # Allow importing watchdog.py from the tools/ directory without packaging.
    sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__))))
    import watchdog as wd

    calls = {"term": 0, "kill": 0}
    alive = {"yes": True}

    def fake_run(argv, capture_output=False, text=False):
        # watchdog uses list argv
        if argv[:2] == ["systemctl", "restart"]:
            return _completed(
                1,
                stderr="Failed to restart swimmy-brain.service: Interactive authentication required.\n",
            )
        if argv[:2] == ["systemctl", "show"] and "MainPID" in argv:
            return _completed(0, stdout="424242\n")
        raise AssertionError(f"Unexpected subprocess.run argv={argv!r}")

    def fake_kill(pid: int, sig: int):
        # Support os.kill(pid, 0) liveness checks if implemented.
        if sig == 0:
            if alive["yes"]:
                return None
            raise ProcessLookupError()
        if sig == signal.SIGTERM:
            calls["term"] += 1
            alive["yes"] = False
            return None
        if sig == signal.SIGKILL:
            calls["kill"] += 1
            alive["yes"] = False
            return None
        raise AssertionError(f"Unexpected signal: {sig}")

    tracker = wd.RevivalTracker("swimmy-brain")

    with mock.patch.object(wd.subprocess, "run", side_effect=fake_run):
        with mock.patch.object(wd.os, "kill", side_effect=fake_kill):
            with mock.patch.object(wd.time, "sleep", return_value=None):
                ok = wd.restart_service(tracker)

    assert ok is True
    assert calls["term"] == 1
    assert calls["kill"] == 0


def test_send_discord_alert_does_not_emit_deprecation_warning():
    # Allow importing watchdog.py from the tools/ directory without packaging.
    sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__))))
    import watchdog as wd

    # Ensure the code path builds a payload, but don't do real HTTP.
    wd.WEBHOOK_URL = "http://example.invalid/webhook"

    with mock.patch.object(wd.requests, "post") as post:
        post.return_value.ok = True

        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always", DeprecationWarning)
            wd.send_discord_alert("t", "d", mention=False)

    # We specifically want to prevent datetime.utcnow() deprecation spam in logs.
    assert not any(isinstance(w.message, DeprecationWarning) for w in caught)


if __name__ == "__main__":
    test_restart_service_falls_back_to_kill_main_pid_on_auth_failure()
    test_send_discord_alert_does_not_emit_deprecation_warning()
    print("OK")
