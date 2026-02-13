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

    def test_extract_agent_signals_from_payloads(self) -> None:
        payload = {
            "payloads": [
                {"text": '[{"market_id":"m1","p_yes":0.62,"confidence":0.81}]'},
                {"text": ""},
            ]
        }
        rows = bridge.extract_agent_signals(payload)
        self.assertEqual(1, len(rows))
        self.assertAlmostEqual(0.62, rows["m1"]["p_yes"])
        self.assertAlmostEqual(0.81, rows["m1"]["confidence"])

    def test_extract_agent_fatal_error_detects_quota_exhausted(self) -> None:
        payload = {
            "payloads": [
                {"text": "Cloud Code Assist API error (429): You have exhausted your capacity on this model."}
            ]
        }
        message = bridge.extract_agent_fatal_error(payload)
        self.assertIn("exhausted your capacity", message)

    def test_collect_agent_signals_retries_until_success(self) -> None:
        responses = [
            {},
            {"payloads": [{"text": '[{"market_id":"m2","p_yes":0.51,"confidence":0.77}]'}]},
        ]
        sleeps = []

        def fetch_fn():
            return responses.pop(0)

        rows = bridge.collect_agent_signals(
            fetch_fn=fetch_fn,
            retries=2,
            retry_sleep_ms=200,
            sleep_fn=lambda sec: sleeps.append(sec),
        )
        self.assertEqual(1, len(rows))
        self.assertAlmostEqual(0.51, rows["m2"]["p_yes"])
        self.assertEqual([0.2], sleeps)

    def test_collect_agent_signals_handles_fetch_errors(self) -> None:
        calls = {"n": 0}

        def fetch_fn():
            calls["n"] += 1
            if calls["n"] == 1:
                raise RuntimeError("boom")
            return {"payloads": [{"text": '[{"market_id":"m3","p_yes":0.49,"confidence":0.65}]'}]}

        rows = bridge.collect_agent_signals(
            fetch_fn=fetch_fn,
            retries=1,
            retry_sleep_ms=0,
            sleep_fn=lambda _sec: None,
        )
        self.assertEqual(2, calls["n"])
        self.assertEqual(1, len(rows))
        self.assertAlmostEqual(0.49, rows["m3"]["p_yes"])

    def test_collect_agent_signals_returns_empty_when_exhausted(self) -> None:
        rows = bridge.collect_agent_signals(
            fetch_fn=lambda: {},
            retries=2,
            retry_sleep_ms=0,
            sleep_fn=lambda _sec: None,
        )
        self.assertEqual({}, rows)

    def test_collect_agent_signals_with_status_stops_on_fatal_error(self) -> None:
        calls = {"n": 0}

        def fetch_fn():
            calls["n"] += 1
            return {
                "payloads": [
                    {"text": "Cloud Code Assist API error (429): You have exhausted your capacity on this model."}
                ]
            }

        rows, fatal_error = bridge.collect_agent_signals_with_status(
            fetch_fn=fetch_fn,
            retries=3,
            retry_sleep_ms=10,
            sleep_fn=lambda _sec: None,
        )
        self.assertEqual({}, rows)
        self.assertIn("exhausted your capacity", fatal_error)
        self.assertEqual(1, calls["n"])

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

    def test_resolve_windows_cmd_token_prefers_cmd_variant(self) -> None:
        token = bridge._resolve_windows_cmd_token(
            "/mnt/c/Users/example/AppData/Roaming/npm/openclaw",
            exists_fn=lambda path: str(path).endswith(".cmd"),
        )
        self.assertEqual("C:\\Users\\example\\AppData\\Roaming\\npm\\openclaw.cmd", token)

    def test_build_powershell_agent_command_quotes_tokens(self) -> None:
        command = bridge._build_powershell_agent_command(
            openclaw_cmd="C:\\Tools\\openclaw.cmd --profile fast",
            agent="main",
            prompt='return [{"market_id":"m1"}]',
        )
        self.assertTrue(command.startswith("& "))
        self.assertIn("'C:\\Tools\\openclaw.cmd'", command)
        self.assertIn("'--profile'", command)
        self.assertIn("'fast'", command)
        self.assertIn("'return [{\"market_id\":\"m1\"}]'", command)

    def test_build_agent_cap_attempts_filters_invalid_and_duplicates(self) -> None:
        caps = bridge.build_agent_cap_attempts(primary_cap=20, fallback_caps=[20, 12, 8, 8, 0, -1, 5, 30, 3])
        self.assertEqual([20, 12, 8, 5, 3], caps)

    def test_collect_agent_signals_for_caps_falls_back_to_next_cap(self) -> None:
        fallback = [
            {"market_id": "m1", "question": "Q1", "source_yes_price": 0.55},
            {"market_id": "m2", "question": "Q2", "source_yes_price": 0.45},
            {"market_id": "m3", "question": "Q3", "source_yes_price": 0.65},
        ]
        calls = []

        def fetch_for_prompt(prompt: str, cap: int):
            calls.append(cap)
            if cap == 12:
                return {"payloads": [{"text": '[{"market_id":"m1","p_yes":0.61,"confidence":0.8}]'}]}
            return {}

        rows, selected_cap = bridge.collect_agent_signals_for_caps(
            fallback_signals=fallback,
            cap_attempts=[20, 12],
            fetch_for_prompt=fetch_for_prompt,
            primary_retries=0,
            fallback_retries=0,
            retry_sleep_ms=0,
            sleep_fn=lambda _sec: None,
        )

        self.assertEqual([20, 12], calls)
        self.assertEqual(12, selected_cap)
        self.assertEqual(1, len(rows))
        self.assertAlmostEqual(0.61, rows["m1"]["p_yes"])

    def test_collect_agent_signals_for_caps_with_status_stops_on_fatal_error(self) -> None:
        fallback = [
            {"market_id": "m1", "question": "Q1", "source_yes_price": 0.55},
            {"market_id": "m2", "question": "Q2", "source_yes_price": 0.45},
        ]
        calls = []

        def fetch_for_prompt(_prompt: str, cap: int):
            calls.append(cap)
            return {
                "payloads": [
                    {"text": "Cloud Code Assist API error (429): You have exhausted your capacity on this model."}
                ]
            }

        rows, selected_cap, fatal_error = bridge.collect_agent_signals_for_caps_with_status(
            fallback_signals=fallback,
            cap_attempts=[20, 12],
            fetch_for_prompt=fetch_for_prompt,
            primary_retries=3,
            fallback_retries=0,
            retry_sleep_ms=0,
            sleep_fn=lambda _sec: None,
        )
        self.assertEqual({}, rows)
        self.assertEqual(20, selected_cap)
        self.assertIn("exhausted your capacity", fatal_error)
        self.assertEqual([20], calls)

    def test_collect_agent_signals_for_caps_with_status_continues_on_timeout(self) -> None:
        fallback = [
            {"market_id": "m1", "question": "Q1", "source_yes_price": 0.55},
            {"market_id": "m2", "question": "Q2", "source_yes_price": 0.45},
        ]
        calls = []

        def fetch_for_prompt(_prompt: str, cap: int):
            calls.append(cap)
            if cap == 20:
                raise bridge.subprocess.TimeoutExpired(cmd=["openclaw"], timeout=12)
            return {"payloads": [{"text": '[{"market_id":"m1","p_yes":0.6,"confidence":0.8}]'}]}

        rows, selected_cap, fatal_error = bridge.collect_agent_signals_for_caps_with_status(
            fallback_signals=fallback,
            cap_attempts=[20, 12],
            fetch_for_prompt=fetch_for_prompt,
            primary_retries=3,
            fallback_retries=0,
            retry_sleep_ms=0,
            sleep_fn=lambda _sec: None,
        )
        self.assertEqual([20, 12], calls)
        self.assertEqual(12, selected_cap)
        self.assertEqual("", fatal_error)
        self.assertEqual(1, len(rows))


if __name__ == "__main__":
    unittest.main()
