#!/usr/bin/env python3
"""Smoke test for report_system_status."""

import os
import tempfile
import importlib.util
from pathlib import Path
from unittest import mock


class _DummySocket:
    def __init__(self):
        self.connected = None
        self.sent = None

    def connect(self, address):
        self.connected = address

    def send_string(self, message):
        self.sent = message


class _DummyContext:
    def __init__(self):
        self.socket_instance = _DummySocket()

    def socket(self, _socket_type):
        return self.socket_instance


def _write(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)


def main() -> int:
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        _write(root / "swimmy.asd", "")

        lib_root = root / "data" / "library"
        for rank in ["INCUBATOR", "B", "A", "S", "LEGEND", "GRAVEYARD"]:
            (lib_root / rank).mkdir(parents=True, exist_ok=True)
        _write(lib_root / "B" / "test.lisp", ";; test")

        optimized_path = root / "src" / "lisp" / "school" / "school-optimized-params.lisp"
        _write(optimized_path, '(:name "X")\n(:name "Y")\n')

        with mock.patch.dict(
            os.environ,
            {
                "SWIMMY_HOME": str(root),
                "SWIMMY_DISCORD_REPORTS": "https://example.invalid/webhook",
            },
            clear=False,
        ):
            report_path = Path(__file__).resolve().parent / "report_system_status.py"
            spec = importlib.util.spec_from_file_location("report_system_status", report_path)
            if spec is None or spec.loader is None:
                raise AssertionError("Failed to load report_system_status module")
            report = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(report)

            dummy_context = _DummyContext()
            with mock.patch.object(report.zmq, "Context", return_value=dummy_context):
                report.main()

            if not dummy_context.socket_instance.sent:
                raise AssertionError("Expected report to send a payload")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
