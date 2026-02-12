#!/usr/bin/env python3
import importlib.util
import tempfile
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).with_name("update_history_smart.py")


def load_module():
    spec = importlib.util.spec_from_file_location("update_history_smart", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


class UpdateHistorySmartTests(unittest.TestCase):
    def test_detect_format_returns_unix_timestamp_and_epoch_kind(self):
        mod = load_module()
        with tempfile.NamedTemporaryFile("w+", delete=False) as fp:
            fp.write("timestamp,open,high,low,close,volume\n")
            fp.write("3977565420,1.34,1.35,1.33,1.34,10\n")
            tmp_path = Path(fp.name)
        try:
            fmt, last_ts_unix, epoch_kind = mod.detect_format(tmp_path)
        finally:
            tmp_path.unlink(missing_ok=True)
        self.assertEqual(fmt, "unix")
        self.assertEqual(last_ts_unix, 1768576620)
        self.assertEqual(epoch_kind, "universal")

    def test_compute_start_request_uses_overlap(self):
        mod = load_module()
        self.assertEqual(mod.compute_start_request(1768576620), 1768573020)
        self.assertEqual(mod.compute_start_request(1200), 0)

    def test_helper_functions_are_defined_before_main_entrypoint(self):
        source = MODULE_PATH.read_text(encoding="utf-8")
        idx_main = source.find('if __name__ == "__main__":')
        idx_helper = source.find("def extract_sexp_value(")
        self.assertGreaterEqual(idx_main, 0)
        self.assertGreaterEqual(idx_helper, 0)
        self.assertGreater(idx_main, idx_helper)


if __name__ == "__main__":
    unittest.main()
