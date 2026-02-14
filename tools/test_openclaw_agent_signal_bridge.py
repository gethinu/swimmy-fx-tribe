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


if __name__ == "__main__":
    unittest.main()
