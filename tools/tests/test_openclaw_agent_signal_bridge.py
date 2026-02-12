import json
import tempfile
import unittest
from pathlib import Path

from tools import openclaw_agent_signal_bridge as bridge


class TestOpenClawAgentSignalBridge(unittest.TestCase):
    def test_extract_agent_json_from_stdout_with_prelude(self) -> None:
        payload = {
            "payloads": [
                {
                    "text": '[{"market_id":"m1","p_yes":0.61,"confidence":0.8}]',
                    "mediaUrl": None,
                }
            ]
        }
        stdout = "[agent/embedded] schema snapshot\n" + json.dumps(payload, ensure_ascii=False)
        parsed = bridge.extract_agent_json_from_stdout(stdout)
        self.assertIsInstance(parsed, dict)
        self.assertIn("payloads", parsed)

    def test_parse_signal_payload_accepts_fenced_json_array(self) -> None:
        text = (
            "```json\n"
            '[{"market_id":"m1","p_yes":0.61,"confidence":0.8},'
            '{"market_id":"m2","p_yes":0.42,"confidence":0.7}]\n'
            "```"
        )
        rows = bridge.parse_signal_payload(text)
        self.assertEqual(2, len(rows))
        self.assertAlmostEqual(0.61, rows["m1"]["p_yes"])
        self.assertAlmostEqual(0.42, rows["m2"]["p_yes"])

    def test_merge_agent_signals_overwrites_fallback(self) -> None:
        fallback = [
            {"market_id": "m1", "p_yes": 0.50, "confidence": 0.60, "question": "Q1"},
            {"market_id": "m2", "p_yes": 0.45, "confidence": 0.65, "question": "Q2"},
        ]
        agent_rows = {
            "m2": {"p_yes": 0.55, "confidence": 0.75},
        }

        merged = bridge.merge_agent_signals(fallback, agent_rows)
        by_id = {row["market_id"]: row for row in merged}
        self.assertAlmostEqual(0.50, by_id["m1"]["p_yes"])
        self.assertAlmostEqual(0.60, by_id["m1"]["confidence"])
        self.assertAlmostEqual(0.55, by_id["m2"]["p_yes"])
        self.assertAlmostEqual(0.75, by_id["m2"]["confidence"])

    def test_find_openclaw_entry_from_explicit_executable_path(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            exe = Path(td) / "npm" / "openclaw"
            exe.parent.mkdir(parents=True, exist_ok=True)
            exe.write_text("#!/bin/sh\n", encoding="utf-8")
            entry = exe.parent / "node_modules" / "openclaw" / "dist" / "index.js"
            entry.parent.mkdir(parents=True, exist_ok=True)
            entry.write_text("// openclaw entry\n", encoding="utf-8")

            found = bridge._find_openclaw_entry(str(exe))

        self.assertEqual(str(entry), found)


if __name__ == "__main__":
    unittest.main()
