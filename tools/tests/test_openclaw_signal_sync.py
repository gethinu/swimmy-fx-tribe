import json
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tools import openclaw_signal_sync as sync


class TestOpenclawSignalSync(unittest.TestCase):
    def test_parse_signals_stdout_jsonl(self) -> None:
        stdout = "\n".join(
            [
                json.dumps({"market_id": "m1", "p_yes": 0.61, "confidence": 0.8}),
                json.dumps({"market_id": "m2", "prob_yes": 0.42, "confidence": 0.7}),
            ]
        )
        signals = sync.parse_signals_stdout(stdout)
        self.assertEqual(2, len(signals))
        self.assertAlmostEqual(0.61, signals["m1"].p_yes)
        self.assertAlmostEqual(0.42, signals["m2"].p_yes)

    def test_parse_signals_stdout_json_array(self) -> None:
        stdout = json.dumps(
            [
                {"market_id": "m1", "p_yes": 0.61, "confidence": 0.8},
                {"market_id": "m2", "prob_yes": 0.42, "confidence": 0.7},
            ]
        )
        signals = sync.parse_signals_stdout(stdout)
        self.assertEqual(2, len(signals))

    def test_sync_from_command_writes_signals_and_meta(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            cp = mock.Mock()
            cp.stdout = json.dumps([{"market_id": "m1", "p_yes": 0.63, "confidence": 0.9}])
            cp.returncode = 0

            with mock.patch.object(sync.subprocess, "run", return_value=cp):
                result = sync.sync_from_command(
                    openclaw_cmd="openclaw signals --format json",
                    signals_file=signals_file,
                    meta_file=meta_file,
                    min_signals=1,
                    timeout_seconds=5,
                )

            self.assertTrue(result["ok"])
            self.assertTrue(signals_file.exists())
            self.assertTrue(meta_file.exists())
            saved = signals_file.read_text(encoding="utf-8")
            self.assertIn("\"market_id\": \"m1\"", saved)
            meta = json.loads(meta_file.read_text(encoding="utf-8"))
            self.assertEqual(1, meta["signal_count"])

    def test_sync_from_command_rejects_too_few_signals(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            signals_file.write_text(json.dumps({"market_id": "old", "p_yes": 0.5}) + "\n", encoding="utf-8")
            cp = mock.Mock()
            cp.stdout = ""
            cp.returncode = 0

            with mock.patch.object(sync.subprocess, "run", return_value=cp):
                result = sync.sync_from_command(
                    openclaw_cmd="openclaw signals --format json",
                    signals_file=signals_file,
                    meta_file=meta_file,
                    min_signals=1,
                    timeout_seconds=5,
                )

            self.assertFalse(result["ok"])
            self.assertIn("too few", result["error"])
            saved = signals_file.read_text(encoding="utf-8")
            self.assertIn("\"market_id\": \"old\"", saved)

    def test_resolve_openclaw_cmd_uses_heuristic_fallback(self) -> None:
        cmd = sync.resolve_openclaw_cmd(
            provided_cmd="",
            env={
                "POLYCLAW_LIMIT": "123",
                "POLYCLAW_HEURISTIC_QUESTION_KEYWORDS": "nba,nfl",
            },
            base_dir=Path("/repo"),
        )
        self.assertIn("tools/openclaw_signal_heuristic.py", cmd)
        self.assertIn("--limit 123", cmd)
        self.assertIn("--question-keyword nba", cmd)
        self.assertIn("--question-keyword nfl", cmd)

    def test_resolve_openclaw_cmd_no_fallback_returns_empty(self) -> None:
        cmd = sync.resolve_openclaw_cmd(
            provided_cmd="",
            env={"POLYCLAW_USE_HEURISTIC_IF_NO_OPENCLAW_CMD": "0"},
            base_dir=Path("/repo"),
        )
        self.assertEqual("", cmd)


if __name__ == "__main__":
    unittest.main()
