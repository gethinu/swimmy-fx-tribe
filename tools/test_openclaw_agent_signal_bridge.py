import importlib.util
import json
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).with_name("openclaw_agent_signal_bridge.py")


def load_module():
    spec = importlib.util.spec_from_file_location("openclaw_agent_signal_bridge", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


class TestOpenClawAgentSignalBridge(unittest.TestCase):
    def test_parses_payload_list_stdout(self) -> None:
        mod = load_module()
        stdout = json.dumps(
            [
                {
                    "text": '[{"market_id":"123","p_yes":0.6,"confidence":0.8}]',
                }
            ]
        )
        agent_json = mod.extract_agent_json_from_stdout(stdout)
        signals = mod.extract_agent_signals(agent_json)

        self.assertIn("123", signals)
        self.assertAlmostEqual(signals["123"]["p_yes"], 0.6, places=6)
        self.assertAlmostEqual(signals["123"]["confidence"], 0.8, places=6)

    def test_parses_direct_signal_list_stdout(self) -> None:
        mod = load_module()
        stdout = '[{"market_id":"m1","p_yes":0.25,"confidence":0.77}]'
        agent_json = mod.extract_agent_json_from_stdout(stdout)
        signals = mod.extract_agent_signals(agent_json)

        self.assertIn("m1", signals)
        self.assertAlmostEqual(signals["m1"]["p_yes"], 0.25, places=6)
        self.assertAlmostEqual(signals["m1"]["confidence"], 0.77, places=6)

    def test_agent_only_filters_to_agent_rows(self) -> None:
        mod = load_module()
        fallback = [
            {"market_id": "m1", "p_yes": 0.5, "confidence": 0.6},
            {"market_id": "m2", "p_yes": 0.5, "confidence": 0.6},
        ]
        agent = {"m1": {"p_yes": 0.9, "confidence": 0.8}}

        merged = mod.select_output_signals(fallback_signals=fallback, agent_signals=agent, agent_only=False)
        self.assertEqual(len(merged), 2)
        self.assertEqual(merged[0]["market_id"], "m1")
        self.assertEqual(merged[0]["source"], "openclaw_agent")

        filtered = mod.select_output_signals(fallback_signals=fallback, agent_signals=agent, agent_only=True)
        self.assertEqual(len(filtered), 1)
        self.assertEqual(filtered[0]["market_id"], "m1")
        self.assertEqual(filtered[0]["source"], "openclaw_agent")

        empty = mod.select_output_signals(fallback_signals=fallback, agent_signals={}, agent_only=True)
        self.assertEqual(empty, [])


if __name__ == "__main__":
    unittest.main()
