import unittest
import os

from tools import trend_arbitrage_engine as e


class TestTrendArbitrageEngine(unittest.TestCase):
    def _with_env_cleared(self, keys):
        backup = {k: os.environ.get(k) for k in keys}
        for k in keys:
            os.environ.pop(k, None)
        return backup

    def _restore_env(self, backup):
        for k, v in backup.items():
            if v is None:
                os.environ.pop(k, None)
            else:
                os.environ[k] = v

    def test_parse_trends_json_guard(self) -> None:
        raw = ")]}',\n{\"default\": {\"trendingSearchesDays\": [{\"trendingSearches\": [{\"title\": {\"query\": \"副業 AI\"}, \"formattedTraffic\": \"20万+\"}]}]}}"
        rows = e.parse_google_daily_trends_response(raw)
        self.assertEqual(1, len(rows))
        self.assertEqual("副業 AI", rows[0]["query"])

    def test_parse_google_trending_rss_response(self) -> None:
        raw = """<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rss xmlns:ht=\"https://trends.google.com/trending/rss\" version=\"2.0\">
  <channel>
    <item>
      <title>副業 AI</title>
      <ht:approx_traffic>2万+</ht:approx_traffic>
    </item>
  </channel>
</rss>"""
        rows = e.parse_google_trending_rss_response(raw)
        self.assertEqual(1, len(rows))
        self.assertEqual("副業 AI", rows[0]["query"])
        self.assertEqual("2万+", rows[0]["traffic"])

    def test_traffic_score_parses_japanese_kilo_notation(self) -> None:
        self.assertGreater(e.traffic_to_score("20万+"), e.traffic_to_score("2万+"))
        self.assertGreater(e.traffic_to_score("2万+"), e.traffic_to_score("5000+"))

    def test_supply_gap_score_rewards_weak_and_stale_serp(self) -> None:
        weak = [
            {"domain": "detail.chiebukuro.yahoo.co.jp", "snippet": "2023"},
            {"domain": "ameblo.jp", "snippet": "2022"},
            {"domain": "note.com", "snippet": "2023"},
        ]
        strong = [
            {"domain": "www.amazon.co.jp", "snippet": "2026"},
            {"domain": "kakaku.com", "snippet": "2026"},
            {"domain": "www.nikkei.com", "snippet": "2026"},
        ]
        self.assertGreater(e.supply_gap_score(weak), e.supply_gap_score(strong))

    def test_intent_score_detects_money_intent(self) -> None:
        self.assertGreater(e.intent_score("副業 AI ツール おすすめ"), 0.7)
        self.assertLess(e.intent_score("副業 AI 歴史"), 0.5)

    def test_opportunity_score_weighting(self) -> None:
        score_a = e.opportunity_score(demand=0.9, gap=0.8, intent=0.7)
        score_b = e.opportunity_score(demand=0.3, gap=0.8, intent=0.7)
        self.assertGreater(score_a, score_b)

    def test_evaluate_candidates_dedupes_case_variants(self) -> None:
        original_search_serp = e.search_serp
        try:
            e.search_serp = lambda *args, **kwargs: []  # type: ignore[assignment]
            rows = e.evaluate_candidates({"副業 AI": 0.7, "副業 ai": 0.6})
            self.assertEqual(1, len(rows))
        finally:
            e.search_serp = original_search_serp

    def test_build_blueprint_and_x_post(self) -> None:
        candidate = {
            "query": "副業 AI ツール おすすめ",
            "demand_score": 0.8,
            "gap_score": 0.7,
            "intent_score": 0.9,
            "opportunity_score": 0.79,
        }
        bp = e.build_blueprint(candidate)
        self.assertIn("offer", bp)
        post = e.render_x_post(bp, "https://example.com/posts/abc")
        self.assertIn("https://example.com/posts/abc", post)
        self.assertLessEqual(len(post), 280)

    def test_render_rss_has_item(self) -> None:
        items = [
            {
                "title": "副業 AI ツール おすすめ",
                "slug": "fukugyo-ai-tools",
                "published_at": "2026-02-20T12:00:00+09:00",
                "summary": "要約",
            }
        ]
        text = e.render_rss(items, base_url="https://example.com")
        self.assertIn("<rss", text)
        self.assertIn("fukugyo-ai-tools", text)

    def test_filter_unpublished_candidates(self) -> None:
        candidates = [
            {"id": "a", "query": "A"},
            {"id": "b", "query": "B"},
        ]
        state = {"published_ids": ["a"]}
        out = e.filter_unpublished_candidates(candidates, state)
        self.assertEqual(["b"], [row["id"] for row in out])

    def test_filter_unpublished_candidates_respects_republish_window(self) -> None:
        keys = ["TREND_ARB_REPUBLISH_HOURS"]
        backup = self._with_env_cleared(keys)
        try:
            os.environ["TREND_ARB_REPUBLISH_HOURS"] = "72"
            candidates = [{"id": "a", "query": "A"}, {"id": "b", "query": "B"}]
            state = {
                "published": {
                    "a": "2099-01-01T00:00:00+00:00",
                    "b": "2000-01-01T00:00:00+00:00",
                }
            }
            out = e.filter_unpublished_candidates(candidates, state)
            self.assertEqual(["b"], [row["id"] for row in out])
        finally:
            self._restore_env(backup)

    def test_publish_x_skips_when_unconfigured(self) -> None:
        keys = [
            "TREND_ARB_PUBLISH_X",
            "TREND_ARB_X_WEBHOOK_URL",
            "TREND_ARB_X_CONSUMER_KEY",
            "TREND_ARB_X_CONSUMER_SECRET",
            "TREND_ARB_X_ACCESS_TOKEN",
            "TREND_ARB_X_ACCESS_TOKEN_SECRET",
        ]
        backup = self._with_env_cleared(keys)
        try:
            out = e.publish_x(
                blueprint={"query": "test", "offer": "offer"},
                candidate={"query": "test"},
                post_url="https://example.com/p",
                dry_run=False,
            )
            self.assertTrue(out["ok"])
            self.assertTrue(out.get("skipped"))
        finally:
            self._restore_env(backup)

    def test_publish_newsletter_skips_when_unconfigured(self) -> None:
        keys = [
            "TREND_ARB_PUBLISH_NEWSLETTER",
            "TREND_ARB_NEWSLETTER_TO",
            "TREND_ARB_SMTP_HOST",
            "TREND_ARB_SMTP_FROM",
            "TREND_ARB_SMTP_USER",
            "TREND_ARB_SMTP_PASS",
        ]
        backup = self._with_env_cleared(keys)
        try:
            out = e.publish_newsletter(
                candidate={"query": "test", "opportunity_score": 0.5},
                blueprint={"query": "test", "offer": "offer", "cta": "cta"},
                post_url="https://example.com/p",
                dry_run=False,
            )
            self.assertTrue(out["ok"])
            self.assertTrue(out.get("skipped"))
        finally:
            self._restore_env(backup)


if __name__ == "__main__":
    unittest.main()
