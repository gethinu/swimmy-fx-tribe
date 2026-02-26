import unittest
from pathlib import Path
import tempfile

from tools import mt5_ih_promotion_gate as gate


class TestMt5IhPromotionGate(unittest.TestCase):
    def test_evaluate_rejects_low_monthly_return(self) -> None:
        summary = {
            "run_id": "ih_opt_full_rerun_20260222_181920",
            "forward": {
                "top": {
                    "Forward Result": "10127.21",
                    "Profit": "127.21",
                    "Profit Factor": "3.486999",
                    "Equity DD %": "0.7207",
                    "Trades": "5",
                }
            },
        }

        out = gate.evaluate_promotion(
            summary,
            forward_days=366,
            min_monthly_return_pct=10.0,
            min_forward_trades=5,
            min_forward_pf=1.0,
            max_forward_dd_pct=10.0,
        )
        self.assertFalse(out["accept_for_promotion"])
        self.assertIn("monthly_return_below_target", out["reasons"])
        self.assertAlmostEqual(0.1052, float(out["metrics"]["monthly_cagr_pct"]), places=3)

    def test_evaluate_accepts_when_monthly_target_hit(self) -> None:
        summary = {
            "run_id": "ih_opt_full_high_return",
            "forward": {
                "top": {
                    "Forward Result": "32000",
                    "Profit": "22000",
                    "Profit Factor": "1.80",
                    "Equity DD %": "8.50",
                    "Trades": "42",
                }
            },
        }

        out = gate.evaluate_promotion(
            summary,
            forward_days=365,
            min_monthly_return_pct=10.0,
            min_forward_trades=30,
            min_forward_pf=1.2,
            max_forward_dd_pct=10.0,
        )
        self.assertTrue(out["accept_for_promotion"])
        self.assertEqual([], out["reasons"])
        self.assertGreaterEqual(float(out["metrics"]["monthly_cagr_pct"]), 10.0)

    def test_evaluate_rejects_invalid_balance_math(self) -> None:
        summary = {
            "run_id": "ih_opt_bad_numbers",
            "forward": {
                "top": {
                    "Forward Result": "100",
                    "Profit": "200",
                    "Profit Factor": "1.50",
                    "Equity DD %": "2.0",
                    "Trades": "20",
                }
            },
        }

        out = gate.evaluate_promotion(
            summary,
            forward_days=365,
            min_monthly_return_pct=10.0,
            min_forward_trades=10,
            min_forward_pf=1.2,
            max_forward_dd_pct=10.0,
        )
        self.assertFalse(out["accept_for_promotion"])
        self.assertIn("invalid_initial_balance", out["reasons"])

    def test_evaluate_from_rows_selects_high_monthly_candidate(self) -> None:
        rows = [
            {
                "Pass": "10",
                "Forward Result": "10800",
                "Profit": "800",
                "Profit Factor": "1.40",
                "Equity DD %": "8.0",
                "Trades": "30",
                "InpRiskPerTradePct": "1.0",
            },
            {
                "Pass": "22",
                "Forward Result": "32000",
                "Profit": "22000",
                "Profit Factor": "1.50",
                "Equity DD %": "9.0",
                "Trades": "40",
                "InpRiskPerTradePct": "1.5",
            },
        ]
        out = gate.evaluate_promotion_from_rows(
            rows,
            forward_days=365,
            min_monthly_return_pct=10.0,
            min_forward_trades=20,
            min_forward_pf=1.2,
            max_forward_dd_pct=10.0,
        )
        self.assertTrue(out["accept_for_promotion"])
        self.assertEqual("22", out["selected_pass"])
        self.assertGreaterEqual(float(out["metrics"]["monthly_cagr_pct"]), 10.0)
        self.assertEqual("1.5", out["selected_parameters"]["InpRiskPerTradePct"])

    def test_load_rows_from_mt5_xml(self) -> None:
        xml = """<?xml version="1.0"?>
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet">
  <Worksheet>
    <Table>
      <Row>
        <Cell><Data>Pass</Data></Cell>
        <Cell><Data>Forward Result</Data></Cell>
        <Cell><Data>Profit</Data></Cell>
        <Cell><Data>Profit Factor</Data></Cell>
        <Cell><Data>Equity DD %</Data></Cell>
        <Cell><Data>Trades</Data></Cell>
      </Row>
      <Row>
        <Cell><Data>7</Data></Cell>
        <Cell><Data>12000</Data></Cell>
        <Cell><Data>2000</Data></Cell>
        <Cell><Data>1.3</Data></Cell>
        <Cell><Data>7.0</Data></Cell>
        <Cell><Data>22</Data></Cell>
      </Row>
    </Table>
  </Worksheet>
</Workbook>
"""
        with tempfile.TemporaryDirectory() as td:
            p = Path(td) / "forward.xml"
            p.write_text(xml, encoding="utf-8")
            rows = gate.load_rows_from_mt5_xml(p)
            self.assertEqual(1, len(rows))
            self.assertEqual("7", rows[0]["Pass"])
            self.assertEqual("12000", rows[0]["Forward Result"])

    def test_apply_selected_parameters_to_set(self) -> None:
        src = """InpVolumeSmaMult=2.0||1.6||0.1||2.6||Y
InpRiskPerTradePct=0.5||0.3||0.1||3.0||Y
InpVerboseLog=false||false||0||true||N
"""
        with tempfile.TemporaryDirectory() as td:
            p = Path(td) / "test.set"
            p.write_text(src, encoding="utf-8")
            gate.apply_selected_parameters_to_set(
                p,
                {
                    "InpVolumeSmaMult": "1.2",
                    "InpRiskPerTradePct": "1.5",
                },
            )
            out = p.read_text(encoding="utf-8")
            self.assertIn("InpVolumeSmaMult=1.2||1.6||0.1||2.6||Y", out)
            self.assertIn("InpRiskPerTradePct=1.5||0.3||0.1||3.0||Y", out)


if __name__ == "__main__":
    unittest.main()
