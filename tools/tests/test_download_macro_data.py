import csv
import os
import tempfile
import unittest

from tools.download_macro_data import _extract_rows_from_chart, fetch_data


class TestDownloadMacroData(unittest.TestCase):
    def test_extract_rows_from_chart_filters_missing_values(self):
        payload = {
            "chart": {
                "result": [
                    {
                        "timestamp": [1735689600, 1735693200],
                        "indicators": {
                            "quote": [
                                {
                                    "open": [2900.0, None],
                                    "high": [2910.0, 2912.0],
                                    "low": [2895.0, 2898.0],
                                    "close": [2905.0, 2906.0],
                                    "volume": [100, 200],
                                }
                            ]
                        },
                    }
                ],
                "error": None,
            }
        }

        rows = _extract_rows_from_chart(payload)
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0][1:], [2900.0, 2910.0, 2895.0, 2905.0, 100.0])

    def test_fetch_data_writes_atomic_csv(self):
        def fake_fetch_rows(*, ticker: str, period: str, interval: str, opener=None):
            if ticker == "GOOD":
                return [["2026-02-12 00:00:00+00:00", 1.0, 2.0, 0.5, 1.5, 42.0]]
            return []

        with tempfile.TemporaryDirectory() as tmpdir:
            fetch_data(
                data_dir=tmpdir,
                macro_map={"A": "GOOD", "B": "EMPTY"},
                fetch_rows_fn=fake_fetch_rows,
            )

            a_path = os.path.join(tmpdir, "A.csv")
            b_path = os.path.join(tmpdir, "B.csv")
            self.assertTrue(os.path.exists(a_path))
            self.assertFalse(os.path.exists(b_path))
            self.assertFalse(os.path.exists(os.path.join(tmpdir, "A.tmp")))

            with open(a_path, "r", encoding="utf-8", newline="") as f:
                rows = list(csv.reader(f))
            self.assertEqual(rows[0], ["Datetime", "Open", "High", "Low", "Close", "Volume"])
            self.assertEqual(rows[1], ["2026-02-12 00:00:00+00:00", "1.0", "2.0", "0.5", "1.5", "42.0"])


if __name__ == "__main__":
    unittest.main()
