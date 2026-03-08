import tempfile
import unittest
from pathlib import Path

from tools import mt5_inventory_tester as tester


class TestMt5InventoryTester(unittest.TestCase):
    def test_select_jobs_returns_expected_legend_inventory(self) -> None:
        jobs = tester.select_jobs("legend")
        self.assertEqual(9, len(jobs))
        experts = {job.expert for job in jobs}
        self.assertIn("Legend_LondonBreakoutV1.ex5", experts)
        self.assertNotIn("Legend_MACDZeroCrossLong.ex5", experts)
        self.assertNotIn("Legend_MACDExpansion.ex5", experts)
        self.assertNotIn("Legend_CrossoverPlusMACD.ex5", experts)
        self.assertIn("Legend_PerfectOrderSMA.ex5", experts)

    def test_select_jobs_returns_expected_historical_inventory(self) -> None:
        jobs = tester.select_jobs("historical_s")
        self.assertEqual(13, len(jobs))
        experts = {job.expert for job in jobs}
        self.assertIn("HistS_Bred222TrendCore.ex5", experts)
        self.assertIn("HistS_Bred139TrendCore_3979972610.ex5", experts)

    def test_render_tester_ini_contains_required_strategy_tester_fields(self) -> None:
        job = tester.select_jobs("legend", ["legend-perfect-order-sma"])[0]
        text = tester.render_tester_ini(
            job,
            report_rel_path="reports\\legend_probe",
            from_date="2025.01.01",
            to_date="2025.01.15",
        )
        self.assertIn("[Tester]", text)
        self.assertIn("Expert=Legend_PerfectOrderSMA.ex5", text)
        self.assertIn("Symbol=USDJPY", text)
        self.assertIn("Period=M30", text)
        self.assertIn("UseLocal=1", text)
        self.assertIn("UseRemote=0", text)
        self.assertIn("UseCloud=0", text)
        self.assertIn("ShutdownTerminal=1", text)

    def test_join_windows_path_keeps_windows_separator_contract(self) -> None:
        joined = tester.join_windows_path(r"C:\Users\stair\AppData\Local\SwimmyMT5Portable\inventory_tester", "terminal64.exe")
        self.assertEqual(
            r"C:\Users\stair\AppData\Local\SwimmyMT5Portable\inventory_tester\terminal64.exe",
            joined,
        )

    def test_wsl_to_windows_path_supports_mnt_paths(self) -> None:
        self.assertEqual(r"C:\Users\stair\AppData", tester.wsl_to_windows_path("/mnt/c/Users/stair/AppData"))

    def test_parse_report_summary_decodes_utf16_html(self) -> None:
        sample = """<!DOCTYPE html><html><body>
        <table>
        <tr><td>初期証拠金:</td><td>10 000.00</td></tr>
        <tr><td>総損益:</td><td>0.70</td></tr>
        <tr><td>プロフィットファクター:</td><td>1.03</td></tr>
        <tr><td>取引数:</td><td>20</td></tr>
        </table>
        </body></html>"""
        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "sample.htm"
            path.write_bytes(sample.encode("utf-16le"))
            summary = tester.parse_report_summary(path)
        self.assertEqual("10 000.00", summary["initial_deposit"])
        self.assertEqual("0.70", summary["total_net_profit"])
        self.assertEqual("1.03", summary["profit_factor"])
        self.assertEqual("20", summary["total_trades"])

    def test_find_report_artifacts_returns_html_and_pngs(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            (root / "legend_probe.htm").write_text("ok", encoding="utf-8")
            (root / "legend_probe.png").write_text("ok", encoding="utf-8")
            (root / "legend_probe-hst.png").write_text("ok", encoding="utf-8")
            (root / "legend_probe.txt").write_text("ignore", encoding="utf-8")

            artifacts = tester.find_report_artifacts(root, "legend_probe")

        self.assertEqual(
            ["legend_probe-hst.png", "legend_probe.htm", "legend_probe.png"],
            [path.name for path in artifacts],
        )


if __name__ == "__main__":
    unittest.main()
