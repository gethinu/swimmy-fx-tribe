import json
import unittest
from unittest import mock

from tools import openclaw_signal_heuristic as h


class TestOpenClawSignalHeuristic(unittest.TestCase):
    def test_infer_model_prob_fades_favorite(self) -> None:
        p_yes, confidence = h.infer_model_prob(
            yes_price=0.70,
            question="Will favorite win?",
            favorite_threshold=0.60,
            favorite_fade=0.03,
            underdog_threshold=0.40,
            underdog_lift=0.01,
        )
        self.assertAlmostEqual(0.67, p_yes, places=6)
        self.assertGreaterEqual(confidence, 0.55)

    def test_infer_model_prob_lifts_underdog(self) -> None:
        p_yes, _ = h.infer_model_prob(
            yes_price=0.30,
            question="Will underdog win?",
            favorite_threshold=0.60,
            favorite_fade=0.03,
            underdog_threshold=0.40,
            underdog_lift=0.01,
        )
        self.assertAlmostEqual(0.31, p_yes, places=6)

    def test_build_signals_from_markets(self) -> None:
        markets = [
            {
                "id": "m1",
                "question": "Will Lakers win?",
                "tokens": [
                    {"token_id": "y1", "outcome": "Yes", "price": "0.64"},
                    {"token_id": "n1", "outcome": "No", "price": "0.36"},
                ],
            }
        ]
        signals = h.build_signals_from_markets(
            markets=markets,
            favorite_threshold=0.60,
            favorite_fade=0.03,
            underdog_threshold=0.40,
            underdog_lift=0.01,
            question_keywords=[],
        )
        self.assertEqual(1, len(signals))
        self.assertEqual("m1", signals[0]["market_id"])
        self.assertAlmostEqual(0.605, signals[0]["p_yes"], places=6)

    def test_fetch_markets_merges_query_when_url_has_params(self) -> None:
        seen = {}

        class _FakeResp:
            def __enter__(self):
                return self

            def __exit__(self, exc_type, exc, tb):
                return False

            def read(self) -> bytes:
                return b"[]"

        def _fake_urlopen(req, timeout=20):
            seen["url"] = req.full_url
            return _FakeResp()

        with mock.patch.object(h, "urlopen", side_effect=_fake_urlopen):
            markets = h.fetch_markets(gamma_url="https://gamma-api.polymarket.com/markets?tag=sports", limit=17)

        self.assertEqual([], markets)
        self.assertIn("tag=sports", seen["url"])
        self.assertIn("active=true", seen["url"])
        self.assertIn("closed=false", seen["url"])
        self.assertIn("limit=17", seen["url"])

    def test_render_jsonl(self) -> None:
        text = h.render_jsonl(
            [
                {"market_id": "m1", "p_yes": 0.61, "confidence": 0.8},
                {"market_id": "m2", "p_yes": 0.42, "confidence": 0.7},
            ]
        )
        rows = [json.loads(line) for line in text.splitlines() if line.strip()]
        self.assertEqual(2, len(rows))
        self.assertEqual("m1", rows[0]["market_id"])


if __name__ == "__main__":
    unittest.main()
