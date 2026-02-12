import json
import tempfile
import unittest
from pathlib import Path

from tools import polymarket_fetch_settlements as settle


class TestPolymarketFetchSettlements(unittest.TestCase):
    def test_extract_winner_from_explicit_field(self) -> None:
        market = {"id": "m1", "winner": "YES"}
        self.assertEqual("YES", settle.extract_winner_from_market(market))

    def test_extract_winner_from_outcome_prices(self) -> None:
        market = {
            "id": "m2",
            "outcomes": "[\"Yes\", \"No\"]",
            "outcomePrices": "[\"0.999\", \"0.001\"]",
        }
        self.assertEqual("YES", settle.extract_winner_from_market(market, min_price_for_win=0.98))

    def test_extract_winner_returns_none_when_not_confident(self) -> None:
        market = {
            "id": "m3",
            "outcomes": "[\"Yes\", \"No\"]",
            "outcomePrices": "[\"0.55\", \"0.45\"]",
        }
        self.assertIsNone(settle.extract_winner_from_market(market, min_price_for_win=0.98))

    def test_collect_market_ids_from_journal(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / "journal.jsonl"
            rows = [
                {"type": "entry", "date": "2026-02-12", "market_id": "m1"},
                {"type": "entry", "date": "2026-02-12", "market_id": "m2"},
                {"type": "entry", "date": "2026-02-11", "market_id": "m3"},
                {"type": "run_summary", "date": "2026-02-12", "run_id": "r1"},
            ]
            path.write_text("\n".join(json.dumps(row) for row in rows), encoding="utf-8")

            ids = settle.collect_market_ids_from_journal(path, target_date="2026-02-12")

        self.assertEqual(["m1", "m2"], ids)

    def test_collect_market_ids_skip_known(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / "journal.jsonl"
            rows = [
                {"type": "entry", "date": "2026-02-12", "market_id": "m1"},
                {"type": "entry", "date": "2026-02-12", "market_id": "m2"},
            ]
            path.write_text("\n".join(json.dumps(row) for row in rows), encoding="utf-8")

            ids = settle.collect_market_ids_from_journal(path, target_date="2026-02-12", skip_market_ids={"m2"})

        self.assertEqual(["m1"], ids)


if __name__ == "__main__":
    unittest.main()
