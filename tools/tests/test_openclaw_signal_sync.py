import json
import tempfile
import threading
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

    def test_parse_signals_stdout_jsonl_ignores_non_json_lines(self) -> None:
        stdout = "\n".join(
            [
                "OpenClaw 2026.2.9",
                json.dumps({"market_id": "m1", "p_yes": 0.61, "confidence": 0.8}),
                "warning: config invalid",
                json.dumps({"market_id": "m2", "prob_yes": 0.42, "confidence": 0.7}),
            ]
        )
        signals = sync.parse_signals_stdout(stdout)
        self.assertEqual(2, len(signals))
        self.assertAlmostEqual(0.61, signals["m1"].p_yes)
        self.assertAlmostEqual(0.42, signals["m2"].p_yes)

    def test_parse_signals_stdout_json_array_line_with_noise(self) -> None:
        stdout = "\n".join(
            [
                "OpenClaw startup...",
                json.dumps(
                    [
                        {"market_id": "m1", "p_yes": 0.61, "confidence": 0.8},
                        {"market_id": "m2", "prob_yes": 0.42, "confidence": 0.7},
                    ]
                ),
                "done",
            ]
        )
        signals = sync.parse_signals_stdout(stdout)
        self.assertEqual(2, len(signals))

    def test_summarize_signal_sources_uses_latest_per_market(self) -> None:
        rows = [
            {"market_id": "m1", "p_yes": 0.6, "source": "heuristic_fallback"},
            {"market_id": "m1", "p_yes": 0.7, "source": "openclaw_agent"},
            {"market_id": "m2", "p_yes": 0.4, "source": "openclaw_agent"},
            {"market_id": "m3", "prob_yes": 0.2},
            {"market_id": "m4", "source": "openclaw_agent"},
        ]
        counts = sync.summarize_signal_sources(rows)
        self.assertEqual(2, counts["openclaw_agent"])
        self.assertEqual(1, counts["unknown"])

    def test_sync_from_command_writes_signals_and_meta(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            last_good_signals_file = Path(td) / "signals.last_good.jsonl"
            last_good_meta_file = Path(td) / "signals.last_good.meta.json"
            cp = mock.Mock()
            cp.stdout = json.dumps(
                [
                    {"market_id": "m1", "p_yes": 0.63, "confidence": 0.9, "source": "openclaw_agent"},
                    {"market_id": "m2", "p_yes": 0.51, "confidence": 0.6, "source": "heuristic_fallback"},
                ]
            )
            cp.returncode = 0

            with mock.patch.object(sync.subprocess, "run", return_value=cp):
                result = sync.sync_from_command(
                    openclaw_cmd="openclaw signals --format json",
                    signals_file=signals_file,
                    meta_file=meta_file,
                    last_good_signals_file=last_good_signals_file,
                    last_good_meta_file=last_good_meta_file,
                    min_signals=1,
                    min_agent_signals=0,
                    min_agent_ratio=0.0,
                    timeout_seconds=5,
                )

            self.assertTrue(result["ok"])
            self.assertEqual(1, result["agent_signal_count"])
            self.assertAlmostEqual(0.5, result["agent_signal_ratio"])
            self.assertEqual(1, result["source_counts"]["openclaw_agent"])
            self.assertEqual(1, result["source_counts"]["heuristic_fallback"])
            self.assertTrue(signals_file.exists())
            self.assertTrue(meta_file.exists())
            self.assertTrue(last_good_signals_file.exists())
            self.assertTrue(last_good_meta_file.exists())
            saved = signals_file.read_text(encoding="utf-8")
            self.assertIn("\"market_id\": \"m1\"", saved)
            meta = json.loads(meta_file.read_text(encoding="utf-8"))
            self.assertEqual(2, meta["signal_count"])
            self.assertEqual(1, meta["source_counts"]["openclaw_agent"])
            self.assertEqual(1, meta["source_counts"]["heuristic_fallback"])
            self.assertAlmostEqual(0.5, meta["agent_signal_ratio"])

    def test_sync_from_command_rejects_too_few_signals(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            last_good_signals_file = Path(td) / "signals.last_good.jsonl"
            last_good_meta_file = Path(td) / "signals.last_good.meta.json"
            signals_file.write_text(json.dumps({"market_id": "old", "p_yes": 0.5}) + "\n", encoding="utf-8")
            cp = mock.Mock()
            cp.stdout = ""
            cp.returncode = 0

            with mock.patch.object(sync.subprocess, "run", return_value=cp):
                result = sync.sync_from_command(
                    openclaw_cmd="openclaw signals --format json",
                    signals_file=signals_file,
                    meta_file=meta_file,
                    last_good_signals_file=last_good_signals_file,
                    last_good_meta_file=last_good_meta_file,
                    min_signals=1,
                    min_agent_signals=0,
                    min_agent_ratio=0.0,
                    timeout_seconds=5,
                )

            self.assertFalse(result["ok"])
            self.assertIn("too few", result["error"])
            saved = signals_file.read_text(encoding="utf-8")
            self.assertIn("\"market_id\": \"old\"", saved)

    def test_sync_from_command_rejects_too_few_agent_signals(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            last_good_signals_file = Path(td) / "signals.last_good.jsonl"
            last_good_meta_file = Path(td) / "signals.last_good.meta.json"
            signals_file.write_text(json.dumps({"market_id": "old", "p_yes": 0.5}) + "\n", encoding="utf-8")
            cp = mock.Mock()
            cp.stdout = json.dumps(
                [
                    {"market_id": "m1", "p_yes": 0.62, "confidence": 0.8, "source": "heuristic_fallback"},
                    {"market_id": "m2", "p_yes": 0.48, "confidence": 0.7, "source": "heuristic_fallback"},
                ]
            )
            cp.returncode = 0

            with mock.patch.object(sync.subprocess, "run", return_value=cp):
                result = sync.sync_from_command(
                    openclaw_cmd="bridge",
                    signals_file=signals_file,
                    meta_file=meta_file,
                    last_good_signals_file=last_good_signals_file,
                    last_good_meta_file=last_good_meta_file,
                    min_signals=1,
                    min_agent_signals=1,
                    min_agent_ratio=0.0,
                    timeout_seconds=5,
                )

            self.assertFalse(result["ok"])
            self.assertIn("too few agent signals", result["error"])
            self.assertFalse(meta_file.exists())
            self.assertFalse(last_good_meta_file.exists())
            self.assertFalse(last_good_signals_file.exists())
            saved = signals_file.read_text(encoding="utf-8")
            self.assertIn("\"market_id\": \"old\"", saved)

    def test_sync_from_command_rejects_low_agent_ratio(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            last_good_signals_file = Path(td) / "signals.last_good.jsonl"
            last_good_meta_file = Path(td) / "signals.last_good.meta.json"
            signals_file.write_text(json.dumps({"market_id": "old", "p_yes": 0.5}) + "\n", encoding="utf-8")
            cp = mock.Mock()
            cp.stdout = json.dumps(
                [
                    {"market_id": "m1", "p_yes": 0.62, "confidence": 0.8, "source": "openclaw_agent"},
                    {"market_id": "m2", "p_yes": 0.48, "confidence": 0.7, "source": "heuristic_fallback"},
                    {"market_id": "m3", "p_yes": 0.51, "confidence": 0.7, "source": "heuristic_fallback"},
                ]
            )
            cp.returncode = 0

            with mock.patch.object(sync.subprocess, "run", return_value=cp):
                result = sync.sync_from_command(
                    openclaw_cmd="bridge",
                    signals_file=signals_file,
                    meta_file=meta_file,
                    last_good_signals_file=last_good_signals_file,
                    last_good_meta_file=last_good_meta_file,
                    min_signals=1,
                    min_agent_signals=0,
                    min_agent_ratio=0.5,
                    timeout_seconds=5,
                )

            self.assertFalse(result["ok"])
            self.assertIn("agent signal ratio too low", result["error"])
            self.assertFalse(meta_file.exists())
            self.assertFalse(last_good_meta_file.exists())
            self.assertFalse(last_good_signals_file.exists())
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

    def test_write_atomic_is_safe_under_concurrency(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / "signals.jsonl"
            errors = []

            def worker(idx: int) -> None:
                try:
                    sync._write_atomic(path, f"{idx}\n")
                except Exception as exc:  # pragma: no cover
                    errors.append(exc)

            threads = [threading.Thread(target=worker, args=(i,)) for i in range(100)]
            for thread in threads:
                thread.start()
            for thread in threads:
                thread.join()

            self.assertEqual([], errors)
            self.assertTrue(path.exists())


if __name__ == "__main__":
    unittest.main()
