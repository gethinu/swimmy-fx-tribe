#!/usr/bin/env python3
import importlib.util
import sys
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).resolve().parent / "ops" / "order_open_smoke.py"


def load_module():
    spec = importlib.util.spec_from_file_location("order_open_smoke", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class OrderOpenSmokeTests(unittest.TestCase):
    def test_build_order_open_contains_required_fields(self):
        mod = load_module()
        msg = mod.build_order_open(
            order_id="UT-1",
            side="buy",
            instrument="USDJPY",
            lot=0.01,
            sl=1.0,
            tp=2.0,
            magic=123456,
            comment="Strategy|H1",
        )
        self.assertIn('(type . "ORDER_OPEN")', msg)
        self.assertIn('(id . "UT-1")', msg)
        self.assertIn('(side . "BUY")', msg)
        self.assertIn('(instrument . "USDJPY")', msg)
        self.assertIn('(magic . 123456)', msg)
        self.assertIn('(comment . "Strategy|H1")', msg)

    def test_build_smoke_cases_has_expected_contract_cases(self):
        mod = load_module()
        cases = mod.build_smoke_cases(
            symbol="USDJPY",
            side="BUY",
            lot=0.01,
            sl=1.0,
            tp=2.0,
            magic_base=990000,
            comment_tf="H1",
            run_id="UT-RUN",
        )
        by_name = {c.name: c for c in cases}
        self.assertIn(mod.CASE_INVALID_ALL, by_name)
        self.assertIn(mod.CASE_MISSING_INSTRUMENT, by_name)
        self.assertIn(mod.CASE_INVALID_COMMENT, by_name)
        self.assertIn(mod.CASE_VALID_ORDER, by_name)

        self.assertIn("INVALID_INSTRUMENT", by_name[mod.CASE_INVALID_ALL].expected)
        self.assertIn("MISSING_INSTRUMENT", by_name[mod.CASE_MISSING_INSTRUMENT].expected)
        self.assertIn("INVALID_COMMENT_FORMAT", by_name[mod.CASE_INVALID_COMMENT].expected)
        self.assertTrue(by_name[mod.CASE_VALID_ORDER].requires_live_order)

    def test_expand_case_selection_all(self):
        mod = load_module()
        expanded = mod.expand_case_selection([mod.CASE_ALL])
        self.assertEqual(expanded, mod.ALL_CASES)

    def test_expand_case_selection_dedupes(self):
        mod = load_module()
        expanded = mod.expand_case_selection(
            [mod.CASE_INVALID_ALL, mod.CASE_INVALID_ALL, mod.CASE_INVALID_COMMENT]
        )
        self.assertEqual(expanded, [mod.CASE_INVALID_ALL, mod.CASE_INVALID_COMMENT])


if __name__ == "__main__":
    unittest.main()
