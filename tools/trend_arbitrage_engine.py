#!/usr/bin/env python3
"""Trend Arbitrage Engine.

Demand surge detection + supply gap scoring + automated publishing for:
- X (Webhook or X API v2)
- Site artifacts (Markdown + JSON index + RSS)
- Newsletter (SMTP)
"""

from __future__ import annotations

import argparse
import base64
import hashlib
import hmac
import json
import logging
import math
import os
import re
import secrets
import smtplib
import time
from datetime import datetime, timezone
from email.message import EmailMessage
from pathlib import Path
from typing import Any, Dict, List, Mapping, Sequence
from urllib.parse import parse_qs, quote, unquote, urlencode, urlparse
from xml.sax.saxutils import escape

import requests

REPO_ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = REPO_ROOT / "data" / "trend_arbitrage"
RUNS_DIR = DATA_DIR / "runs"
SITE_DIR = DATA_DIR / "site"
SITE_POSTS_DIR = SITE_DIR / "posts"
STATE_FILE = DATA_DIR / "state.json"
LATEST_RUN_FILE = DATA_DIR / "latest_run.json"

for path in (DATA_DIR, RUNS_DIR, SITE_DIR, SITE_POSTS_DIR):
    path.mkdir(parents=True, exist_ok=True)

LOG_LEVEL = os.getenv("TREND_ARB_LOG_LEVEL", "INFO").upper()
logging.basicConfig(
    level=LOG_LEVEL,
    format="[%(asctime)s] %(levelname)s: %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
logger = logging.getLogger(__name__)

GOOGLE_DAILY_TRENDS_URL = "https://trends.google.com/trends/api/dailytrends"
GOOGLE_TRENDING_RSS_URL = "https://trends.google.com/trending/rss"
GOOGLE_SUGGEST_URL = "https://suggestqueries.google.com/complete/search"
DUCKDUCKGO_HTML_URL = "https://duckduckgo.com/html/"
GOOGLE_NEWS_RSS_SEARCH_URL = "https://news.google.com/rss/search"

HIGH_INTENT_TERMS = (
    "おすすめ",
    "比較",
    "価格",
    "最安",
    "クーポン",
    "レビュー",
    "ランキング",
    "導入",
    "テンプレ",
    "使い方",
    "収益化",
    "副業",
    "稼ぐ",
    "アフィリエイト",
    "買い方",
    "申し込み",
    "見積",
)

LOW_INTENT_TERMS = (
    "歴史",
    "意味",
    "語源",
    "とは",
    "なぜ",
    "定義",
)

WEAK_SUPPLY_DOMAINS = (
    "chiebukuro.yahoo.co.jp",
    "note.com",
    "ameblo.jp",
    "hatenablog.com",
    "livedoor.jp",
    "fc2.com",
    "reddit.com",
    "5ch.net",
    "qiita.com",
)

STRONG_SUPPLY_DOMAINS = (
    "wikipedia.org",
    "amazon.",
    "rakuten.",
    "kakaku.com",
    "nikkei.com",
    "itmedia.co.jp",
    "impress.co.jp",
    "oricon.co.jp",
)

INTENT_MODIFIERS = (
    "おすすめ",
    "比較",
    "使い方",
    "導入",
)

SESSION = requests.Session()
SESSION.headers.update({"User-Agent": "swimmy-trend-arbitrage/1.0"})
_DDG_DISABLED_UNTIL = 0.0


def parse_csv_env(name: str, default_csv: str = "") -> List[str]:
    raw = os.getenv(name, default_csv)
    return [part.strip() for part in raw.split(",") if part and part.strip()]


def env_bool(name: str, default: bool = False) -> bool:
    raw = os.getenv(name)
    if raw is None:
        return default
    text = raw.strip().lower()
    if text in ("1", "true", "yes", "on"):
        return True
    if text in ("0", "false", "no", "off"):
        return False
    return default


def clamp(value: float, lo: float = 0.0, hi: float = 1.0) -> float:
    return max(lo, min(hi, value))


def now_utc_iso() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat()


def ensure_ascii_slug(text: str, max_len: int = 64) -> str:
    lowered = text.strip().lower()
    cleaned = re.sub(r"[^a-z0-9\s_-]", "", lowered)
    if not cleaned:
        digest = hashlib.sha1(text.encode("utf-8")).hexdigest()[:10]
        return f"topic-{digest}"
    slug = re.sub(r"[\s_-]+", "-", cleaned).strip("-")
    if not slug:
        digest = hashlib.sha1(text.encode("utf-8")).hexdigest()[:10]
        slug = f"topic-{digest}"
    return slug[:max_len]


def atomic_write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(text, encoding="utf-8")
    tmp.replace(path)


def load_json(path: Path, default: Any) -> Any:
    if not path.exists():
        return default
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return default


def save_json(path: Path, payload: Any) -> None:
    atomic_write_text(path, json.dumps(payload, ensure_ascii=False, indent=2))


def parse_google_daily_trends_response(raw_text: str) -> List[Dict[str, Any]]:
    text = (raw_text or "").strip()
    if not text:
        return []
    if text.startswith(")]}',"):
        parts = text.split("\n", 1)
        text = parts[1] if len(parts) > 1 else ""
    if not text:
        return []
    payload = json.loads(text)
    days = payload.get("default", {}).get("trendingSearchesDays", [])
    out: List[Dict[str, Any]] = []
    for day in days:
        for row in day.get("trendingSearches", []):
            query = str(row.get("title", {}).get("query", "")).strip()
            if not query:
                continue
            out.append(
                {
                    "query": query,
                    "traffic": str(row.get("formattedTraffic", "")).strip(),
                    "articles": row.get("articles", []) if isinstance(row.get("articles"), list) else [],
                }
            )
    return out


def parse_google_trending_rss_response(raw_xml: str) -> List[Dict[str, Any]]:
    import xml.etree.ElementTree as ET

    text = (raw_xml or "").strip()
    if not text:
        return []
    try:
        root = ET.fromstring(text)
    except Exception:
        return []

    out: List[Dict[str, Any]] = []
    for item in root.findall(".//item"):
        title = (item.findtext("title") or "").strip()
        if not title:
            continue
        traffic = ""
        for child in list(item):
            tag = child.tag
            if "}" in tag:
                tag = tag.split("}", 1)[1]
            if tag == "approx_traffic":
                traffic = (child.text or "").strip()
                break
        out.append(
            {
                "query": title,
                "traffic": traffic,
                "articles": [],
            }
        )
    return out


def fetch_google_daily_trends(*, geo: str = "JP", hl: str = "ja", tz_minutes: int = -540) -> List[Dict[str, Any]]:
    params = {
        "hl": hl,
        "tz": str(tz_minutes),
        "geo": geo,
        "ns": "15",
    }
    try:
        resp = SESSION.get(GOOGLE_DAILY_TRENDS_URL, params=params, timeout=20)
        resp.raise_for_status()
        rows = parse_google_daily_trends_response(resp.text)
        if rows:
            return rows
    except Exception as exc:
        logger.warning("daily trends fetch failed: %s", exc)
    try:
        resp = SESSION.get(
            GOOGLE_TRENDING_RSS_URL,
            params={"geo": geo},
            timeout=20,
        )
        resp.raise_for_status()
        rows = parse_google_trending_rss_response(resp.text)
        if rows:
            return rows
    except Exception as exc:
        logger.warning("trending rss fetch failed: %s", exc)
    return []


def fetch_google_suggest(query: str, *, hl: str = "ja", gl: str = "jp", timeout_sec: int = 15) -> List[str]:
    params = {
        "client": "firefox",
        "hl": hl,
        "gl": gl,
        "q": query,
    }
    try:
        resp = SESSION.get(GOOGLE_SUGGEST_URL, params=params, timeout=timeout_sec)
        resp.raise_for_status()
        payload = resp.json()
    except Exception:
        return []
    if not isinstance(payload, list) or len(payload) < 2 or not isinstance(payload[1], list):
        return []
    out: List[str] = []
    for item in payload[1]:
        text = str(item).strip()
        if text:
            out.append(text)
    return out


def _parse_ddg_result_url(href: str) -> str:
    if not href:
        return ""
    if "duckduckgo.com/l/?" not in href:
        return href
    parsed = urlparse(href)
    query = parse_qs(parsed.query)
    if "uddg" in query and query["uddg"]:
        return unquote(query["uddg"][0])
    return href


def parse_duckduckgo_results(html_text: str, max_results: int = 10) -> List[Dict[str, str]]:
    html = html_text or ""
    if not html:
        return []
    links = re.findall(r'class="result__a"[^>]*href="([^"]+)"', html)
    snippets = re.findall(r'class="result__snippet"[^>]*>(.*?)</a>', html, flags=re.DOTALL)
    if not snippets:
        snippets = re.findall(r'class="result__snippet"[^>]*>(.*?)</div>', html, flags=re.DOTALL)

    out: List[Dict[str, str]] = []
    for idx, href in enumerate(links[:max_results]):
        url = _parse_ddg_result_url(href)
        domain = urlparse(url).netloc.lower()
        snippet_raw = snippets[idx] if idx < len(snippets) else ""
        snippet = re.sub(r"<[^>]+>", "", snippet_raw)
        out.append({"url": url, "domain": domain, "snippet": snippet.strip()})
    return out


def parse_google_news_rss_results(xml_text: str, max_results: int = 10) -> List[Dict[str, str]]:
    import xml.etree.ElementTree as ET

    text = (xml_text or "").strip()
    if not text:
        return []
    try:
        root = ET.fromstring(text)
    except Exception:
        return []

    out: List[Dict[str, str]] = []
    for item in root.findall(".//item")[:max_results]:
        source = item.find("source")
        source_url = source.get("url") if source is not None else ""
        domain = urlparse(source_url or "").netloc.lower()
        title = (item.findtext("title") or "").strip()
        pub = (item.findtext("pubDate") or "").strip()
        link = (item.findtext("link") or "").strip()
        out.append(
            {
                "url": source_url or link,
                "domain": domain,
                "snippet": f"{title} {pub}".strip(),
            }
        )
    return out


def search_news_rss(query: str, *, lang: str = "ja", geo: str = "JP", max_results: int = 10) -> List[Dict[str, str]]:
    params = {
        "q": query,
        "hl": lang,
        "gl": geo,
        "ceid": f"{geo}:{lang}",
    }
    try:
        resp = SESSION.get(GOOGLE_NEWS_RSS_SEARCH_URL, params=params, timeout=20)
        resp.raise_for_status()
        return parse_google_news_rss_results(resp.text, max_results=max_results)
    except Exception as exc:
        logger.warning("news rss fetch failed for '%s': %s", query, exc)
        return []


def search_serp(query: str, *, max_results: int = 10, timeout_sec: int = 8) -> List[Dict[str, str]]:
    global _DDG_DISABLED_UNTIL
    provider = os.getenv("TREND_ARB_SERP_PROVIDER", "auto").strip().lower()
    timeout_sec = int(os.getenv("TREND_ARB_SERP_TIMEOUT_SEC", str(timeout_sec)))
    rows: List[Dict[str, str]] = []
    if provider in ("auto", "duckduckgo"):
        if provider == "duckduckgo" or time.time() >= _DDG_DISABLED_UNTIL:
            try:
                resp = SESSION.get(
                    DUCKDUCKGO_HTML_URL,
                    params={"q": query, "kl": "jp-jp"},
                    timeout=max(1, timeout_sec),
                )
                resp.raise_for_status()
                rows = parse_duckduckgo_results(resp.text, max_results=max_results)
            except Exception as exc:
                logger.warning("serp fetch failed for '%s': %s", query, exc)
                if provider == "auto":
                    _DDG_DISABLED_UNTIL = time.time() + 600.0

    if rows:
        return rows
    if provider == "duckduckgo":
        return []
    return search_news_rss(query, max_results=max_results)


def _extract_year(text: str) -> int | None:
    m = re.search(r"(20\d{2})", text or "")
    if not m:
        return None
    try:
        return int(m.group(1))
    except ValueError:
        return None


def _domain_matches(domain: str, patterns: Sequence[str]) -> bool:
    value = (domain or "").lower()
    return any(pattern in value for pattern in patterns)


def parse_timestamp_any(text: str) -> datetime | None:
    value = (text or "").strip()
    if not value:
        return None
    if value.endswith("Z"):
        value = value[:-1] + "+00:00"
    try:
        dt = datetime.fromisoformat(value)
    except ValueError:
        return None
    if dt.tzinfo is None:
        dt = dt.replace(tzinfo=timezone.utc)
    return dt.astimezone(timezone.utc)


def supply_gap_score(serp_rows: Sequence[Mapping[str, str]], *, now_year: int | None = None) -> float:
    if not serp_rows:
        return 0.35
    year_now = now_year or datetime.now(timezone.utc).year
    n = float(len(serp_rows))

    weak_count = 0.0
    strong_count = 0.0
    stale_count = 0.0
    domains: set[str] = set()

    for row in serp_rows:
        domain = str(row.get("domain", "")).lower()
        snippet = str(row.get("snippet", ""))
        domains.add(domain)
        if _domain_matches(domain, WEAK_SUPPLY_DOMAINS):
            weak_count += 1.0
        if _domain_matches(domain, STRONG_SUPPLY_DOMAINS):
            strong_count += 1.0

        year = _extract_year(snippet)
        if year is not None and year <= year_now - 2:
            stale_count += 1.0

    weak_ratio = weak_count / n
    strong_ratio = strong_count / n
    stale_ratio = stale_count / n
    diversity = len(domains) / n

    score = 0.50 * weak_ratio + 0.35 * stale_ratio + 0.20 * diversity - 0.45 * strong_ratio
    return clamp(score)


def intent_score(query: str) -> float:
    text = (query or "").lower()
    if not text:
        return 0.0
    matches = sum(1 for term in HIGH_INTENT_TERMS if term in text)
    low_matches = sum(1 for term in LOW_INTENT_TERMS if term in text)

    score = 0.22 + min(0.70, 0.25 * matches)
    score -= 0.20 * low_matches
    return clamp(score)


def _parse_number(text: str) -> float:
    try:
        return float(text.replace(",", ""))
    except ValueError:
        return 0.0


def traffic_to_number(raw: str) -> float:
    text = str(raw or "").strip().lower()
    if not text:
        return 0.0
    m = re.search(r"([0-9][0-9,]*(?:\.[0-9]+)?)\s*(億|万|k|m)?", text)
    if not m:
        return 0.0
    value = _parse_number(m.group(1))
    unit = m.group(2) or ""
    if unit == "億":
        value *= 100_000_000
    elif unit == "万":
        value *= 10_000
    elif unit == "k":
        value *= 1_000
    elif unit == "m":
        value *= 1_000_000
    return value


def traffic_to_score(raw: str) -> float:
    n = traffic_to_number(raw)
    if n <= 0:
        return 0.30
    return clamp(math.log10(n + 1.0) / 6.0)


def opportunity_score(*, demand: float, gap: float, intent: float) -> float:
    return clamp(0.45 * demand + 0.35 * gap + 0.20 * intent)


def filter_unpublished_candidates(
    candidates: Sequence[Mapping[str, Any]],
    state: Mapping[str, Any],
) -> List[Dict[str, Any]]:
    seen = set(str(x) for x in state.get("published_ids", []))
    published_map_raw = state.get("published", {})
    published_map = published_map_raw if isinstance(published_map_raw, dict) else {}
    cooldown_hours = float(os.getenv("TREND_ARB_REPUBLISH_HOURS", "72"))
    cooldown_sec = max(0.0, cooldown_hours) * 3600.0
    now_ts = datetime.now(timezone.utc)
    out: List[Dict[str, Any]] = []
    for row in candidates:
        cid = str(row.get("id", "")).strip()
        if not cid:
            continue
        if cid in seen and cid not in published_map:
            continue
        last = parse_timestamp_any(str(published_map.get(cid, "")))
        if last is not None and cooldown_sec > 0 and (now_ts - last).total_seconds() < cooldown_sec:
            continue
        out.append(dict(row))
    return out


def build_blueprint(candidate: Mapping[str, Any]) -> Dict[str, Any]:
    query = str(candidate.get("query", "")).strip()
    q = query.lower()

    if any(word in q for word in ("価格", "最安", "比較", "レビュー", "おすすめ", "ランキング")):
        offer = "比較表 + 選定基準 + 最安更新"
        monetization = "affiliate"
        cta = "比較表から最短で選ぶ"
    elif any(word in q for word in ("使い方", "設定", "自動化", "テンプレ", "手順")):
        offer = "実装テンプレ + 手順チェックリスト"
        monetization = "digital-product"
        cta = "テンプレを即適用する"
    else:
        offer = "厳選リンク集 + 実行チェック"
        monetization = "newsletter+affiliate"
        cta = "毎日の機会通知を受け取る"

    title = f"{query}: 需要先行の穴場を自動検知"

    return {
        "id": str(candidate.get("id", "")),
        "query": query,
        "title": title,
        "audience": "国内の個人事業者・副業実行者",
        "pain": "伸びる前のテーマを見つけるまでに時間が溶ける",
        "offer": offer,
        "monetization": monetization,
        "cta": cta,
        "opportunity_score": float(candidate.get("opportunity_score", 0.0)),
        "hook": "需要急増 × 供給不足 × 高意図を検知",
    }


def render_x_post(blueprint: Mapping[str, Any], post_url: str) -> str:
    lines = [
        f"{blueprint.get('query', '')} を自動スキャン。",
        "需要急増 × 供給不足を検知。",
        f"{blueprint.get('offer', '')}",
        f"詳細: {post_url}",
    ]
    text = "\n".join(line.strip() for line in lines if line and str(line).strip())
    if len(text) <= 280:
        return text

    suffix = f"\n詳細: {post_url}"
    head = text.replace(suffix, "")
    room = max(0, 280 - len(suffix) - 1)
    trimmed = head[:room].rstrip()
    return f"{trimmed}\n{suffix.strip()}"


def render_site_markdown(
    *,
    candidate: Mapping[str, Any],
    blueprint: Mapping[str, Any],
    published_at: str,
) -> str:
    demand = float(candidate.get("demand_score", 0.0))
    gap = float(candidate.get("gap_score", 0.0))
    intent = float(candidate.get("intent_score", 0.0))
    score = float(candidate.get("opportunity_score", 0.0))

    lines = [
        f"# {blueprint.get('title', '')}",
        "",
        f"公開日時: {published_at}",
        f"キーワード: {candidate.get('query', '')}",
        "",
        "## 機会スコア",
        f"- Demand: {demand:.2f}",
        f"- Gap: {gap:.2f}",
        f"- Intent: {intent:.2f}",
        f"- Opportunity: {score:.2f}",
        "",
        "## 誰のどんな課題か",
        f"- 対象: {blueprint.get('audience', '')}",
        f"- 課題: {blueprint.get('pain', '')}",
        "",
        "## 提供価値",
        f"{blueprint.get('offer', '')}",
        "",
        "## 収益化導線",
        f"- モデル: {blueprint.get('monetization', '')}",
        f"- CTA: {blueprint.get('cta', '')}",
        "",
        "## 1日でやる実行タスク",
        "1. 上位10検索結果を手動で再確認して誤検知を排除",
        "2. 比較表/テンプレの下書きを作成",
        "3. X投稿・メルマガで同日配信",
    ]
    return "\n".join(lines).strip() + "\n"


def render_newsletter_subject(blueprint: Mapping[str, Any]) -> str:
    return f"[Trend Arbitrage] {blueprint.get('query', '')} の先回り機会"


def render_newsletter_body(
    *,
    candidate: Mapping[str, Any],
    blueprint: Mapping[str, Any],
    post_url: str,
) -> str:
    return "\n".join(
        [
            f"テーマ: {blueprint.get('query', '')}",
            f"機会スコア: {float(candidate.get('opportunity_score', 0.0)):.2f}",
            f"提供価値: {blueprint.get('offer', '')}",
            f"実行導線: {blueprint.get('cta', '')}",
            "",
            f"記事: {post_url}",
        ]
    )


def render_rss(items: Sequence[Mapping[str, Any]], *, base_url: str) -> str:
    now = datetime.now(timezone.utc).strftime("%a, %d %b %Y %H:%M:%S +0000")
    channel_title = "Trend Arbitrage Feed"
    channel_link = f"{base_url.rstrip('/')}/"
    channel_desc = "Demand surge + supply gap opportunities"

    chunks = [
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
        "<rss version=\"2.0\">",
        "<channel>",
        f"<title>{escape(channel_title)}</title>",
        f"<link>{escape(channel_link)}</link>",
        f"<description>{escape(channel_desc)}</description>",
        f"<lastBuildDate>{escape(now)}</lastBuildDate>",
    ]

    for item in items[:100]:
        slug = str(item.get("slug", "")).strip()
        if not slug:
            continue
        title = str(item.get("title", slug))
        summary = str(item.get("summary", ""))
        pub = str(item.get("published_at", now))
        link = f"{base_url.rstrip('/')}/posts/{slug}.md"
        guid = str(item.get("id", slug))
        chunks.extend(
            [
                "<item>",
                f"<title>{escape(title)}</title>",
                f"<link>{escape(link)}</link>",
                f"<guid isPermaLink=\"false\">{escape(guid)}</guid>",
                f"<description>{escape(summary)}</description>",
                f"<pubDate>{escape(pub)}</pubDate>",
                "</item>",
            ]
        )

    chunks.extend(["</channel>", "</rss>"])
    return "\n".join(chunks) + "\n"


def is_x_channel_configured() -> bool:
    if os.getenv("TREND_ARB_X_WEBHOOK_URL", "").strip():
        return True
    required = (
        os.getenv("TREND_ARB_X_CONSUMER_KEY", "").strip(),
        os.getenv("TREND_ARB_X_CONSUMER_SECRET", "").strip(),
        os.getenv("TREND_ARB_X_ACCESS_TOKEN", "").strip(),
        os.getenv("TREND_ARB_X_ACCESS_TOKEN_SECRET", "").strip(),
    )
    return all(required)


def is_newsletter_configured() -> bool:
    to_raw = os.getenv("TREND_ARB_NEWSLETTER_TO", "").strip()
    host = os.getenv("TREND_ARB_SMTP_HOST", "").strip()
    sender = os.getenv("TREND_ARB_SMTP_FROM", "").strip() or os.getenv("TREND_ARB_SMTP_USER", "").strip()
    return bool(to_raw and host and sender)


def skipped_channel(reason: str) -> Dict[str, Any]:
    return {"ok": True, "skipped": True, "reason": reason}


def build_candidate_id(query: str) -> str:
    digest = hashlib.sha1(query.strip().lower().encode("utf-8")).hexdigest()[:16]
    return f"trend-{digest}"


def _candidate_summary(candidate: Mapping[str, Any]) -> str:
    return (
        f"Demand {float(candidate.get('demand_score', 0.0)):.2f}, "
        f"Gap {float(candidate.get('gap_score', 0.0)):.2f}, "
        f"Intent {float(candidate.get('intent_score', 0.0)):.2f}, "
        f"Opp {float(candidate.get('opportunity_score', 0.0)):.2f}"
    )


def build_query_pool(
    trends: Sequence[Mapping[str, Any]],
    *,
    max_queries: int,
    lang: str,
    geo: str,
) -> Dict[str, float]:
    pool: Dict[str, tuple[str, float]] = {}

    def _merge(query: str, demand: float) -> None:
        original = query.strip()
        if not original:
            return
        canonical = re.sub(r"\s+", " ", original).strip().lower()
        if not canonical:
            return
        existing = pool.get(canonical)
        if existing is None or demand > existing[1]:
            pool[canonical] = (original, demand)

    for row in trends:
        base_q = str(row.get("query", "")).strip()
        if not base_q:
            continue
        base_demand = traffic_to_score(str(row.get("traffic", "")))
        _merge(base_q, base_demand)

        for suggestion in fetch_google_suggest(base_q, hl=lang, gl=geo.lower())[:5]:
            _merge(suggestion, base_demand * 0.92)

        if intent_score(base_q) < 0.60:
            for mod in INTENT_MODIFIERS[:2]:
                _merge(f"{base_q} {mod}", base_demand * 0.88)

        if len(pool) >= max_queries:
            break

    seed_queries = parse_csv_env(
        "TREND_ARB_SEED_QUERIES",
        "副業 AI,AI ツール 比較,業務自動化 テンプレ,価格比較 ツール,アフィリエイト 収益化",
    )
    for seed in seed_queries:
        _merge(seed, 0.62)
        for suggestion in fetch_google_suggest(seed, hl=lang, gl=geo.lower())[:3]:
            _merge(suggestion, 0.58)

    top = sorted(pool.values(), key=lambda x: x[1], reverse=True)[:max_queries]
    return {query: demand for query, demand in top}


def evaluate_candidates(query_pool: Mapping[str, float], *, serp_top_n: int = 10) -> List[Dict[str, Any]]:
    by_id: Dict[str, Dict[str, Any]] = {}
    for query, demand in query_pool.items():
        serp_rows = search_serp(query, max_results=serp_top_n)
        gap = supply_gap_score(serp_rows)
        intent = intent_score(query)
        opp = opportunity_score(demand=demand, gap=gap, intent=intent)
        row = {
            "id": build_candidate_id(query),
            "query": query,
            "demand_score": round(demand, 6),
            "gap_score": round(gap, 6),
            "intent_score": round(intent, 6),
            "opportunity_score": round(opp, 6),
            "serp_rows": serp_rows,
        }
        current = by_id.get(row["id"])
        if current is None or row["opportunity_score"] > current["opportunity_score"]:
            by_id[row["id"]] = row
    out = list(by_id.values())
    out.sort(key=lambda x: x["opportunity_score"], reverse=True)
    return out


def load_state() -> Dict[str, Any]:
    state = load_json(STATE_FILE, default={})
    if not isinstance(state, dict):
        state = {}
    state.setdefault("published_ids", [])
    state.setdefault("published", {})
    state.setdefault("posts", [])
    return state


def save_state(state: Mapping[str, Any]) -> None:
    save_json(STATE_FILE, state)


def update_site_indexes(state: Mapping[str, Any]) -> None:
    posts = list(state.get("posts", []))
    save_json(SITE_DIR / "index.json", posts)

    base_url = os.getenv("TREND_ARB_SITE_BASE_URL", "https://example.com/trend-arbitrage")
    rss_text = render_rss(posts, base_url=base_url)
    atomic_write_text(SITE_DIR / "rss.xml", rss_text)


def publish_site(
    *,
    candidate: Mapping[str, Any],
    blueprint: Mapping[str, Any],
    dry_run: bool,
    state: Dict[str, Any],
) -> Dict[str, Any]:
    slug = ensure_ascii_slug(str(candidate.get("query", "")))
    candidate_id = str(candidate.get("id", ""))
    suffix = candidate_id.split("-")[-1][:8]
    slug_full = f"{slug}-{suffix}" if suffix else slug

    published_at_iso = now_utc_iso()
    markdown = render_site_markdown(candidate=candidate, blueprint=blueprint, published_at=published_at_iso)
    file_path = SITE_POSTS_DIR / f"{slug_full}.md"

    if not dry_run:
        atomic_write_text(file_path, markdown)

    base_url = os.getenv("TREND_ARB_SITE_BASE_URL", "https://example.com/trend-arbitrage").rstrip("/")
    post_url = f"{base_url}/posts/{slug_full}.md"

    post_record = {
        "id": candidate_id,
        "title": str(blueprint.get("title", candidate.get("query", ""))),
        "slug": slug_full,
        "published_at": published_at_iso,
        "summary": _candidate_summary(candidate),
        "query": str(candidate.get("query", "")),
        "url": post_url,
    }

    if not dry_run:
        posts = list(state.get("posts", []))
        posts.insert(0, post_record)
        state["posts"] = posts[:300]
        published_ids = [str(x) for x in state.get("published_ids", [])]
        if candidate_id not in published_ids:
            published_ids.append(candidate_id)
        state["published_ids"] = published_ids[-2000:]
        published_map_raw = state.get("published", {})
        published_map = published_map_raw if isinstance(published_map_raw, dict) else {}
        published_map[candidate_id] = published_at_iso
        state["published"] = published_map
        update_site_indexes(state)

    return {
        "ok": True,
        "post_url": post_url,
        "path": str(file_path),
        "slug": slug_full,
        "dry_run": dry_run,
    }


def _oauth1_header(
    *,
    method: str,
    url: str,
    consumer_key: str,
    consumer_secret: str,
    token: str,
    token_secret: str,
) -> str:
    ts = str(int(time.time()))
    nonce = secrets.token_hex(12)

    oauth_params = {
        "oauth_consumer_key": consumer_key,
        "oauth_nonce": nonce,
        "oauth_signature_method": "HMAC-SHA1",
        "oauth_timestamp": ts,
        "oauth_token": token,
        "oauth_version": "1.0",
    }

    encoded_pairs = []
    for key in sorted(oauth_params.keys()):
        encoded_pairs.append(f"{quote(key, safe='')}={quote(str(oauth_params[key]), safe='')}")
    param_str = "&".join(encoded_pairs)

    base_elems = [
        method.upper(),
        quote(url, safe=""),
        quote(param_str, safe=""),
    ]
    base_string = "&".join(base_elems)

    signing_key = f"{quote(consumer_secret, safe='')}&{quote(token_secret, safe='')}"
    digest = hmac.new(signing_key.encode("utf-8"), base_string.encode("utf-8"), hashlib.sha1).digest()
    signature = base64.b64encode(digest).decode("utf-8")

    oauth_params["oauth_signature"] = signature

    auth_parts = []
    for key in sorted(oauth_params.keys()):
        val = quote(str(oauth_params[key]), safe="")
        auth_parts.append(f'{key}="{val}"')
    return "OAuth " + ", ".join(auth_parts)


def post_to_x_api(text: str) -> Dict[str, Any]:
    consumer_key = os.getenv("TREND_ARB_X_CONSUMER_KEY", "")
    consumer_secret = os.getenv("TREND_ARB_X_CONSUMER_SECRET", "")
    access_token = os.getenv("TREND_ARB_X_ACCESS_TOKEN", "")
    access_secret = os.getenv("TREND_ARB_X_ACCESS_TOKEN_SECRET", "")

    if not all([consumer_key, consumer_secret, access_token, access_secret]):
        return {"ok": False, "error": "missing x api oauth1 env vars"}

    endpoint = "https://api.twitter.com/2/tweets"
    auth = _oauth1_header(
        method="POST",
        url=endpoint,
        consumer_key=consumer_key,
        consumer_secret=consumer_secret,
        token=access_token,
        token_secret=access_secret,
    )

    try:
        resp = SESSION.post(
            endpoint,
            headers={
                "Authorization": auth,
                "Content-Type": "application/json",
            },
            data=json.dumps({"text": text}, ensure_ascii=False),
            timeout=20,
        )
        ok = 200 <= resp.status_code < 300
        payload = {}
        try:
            payload = resp.json()
        except Exception:
            payload = {"raw": resp.text[:500]}
        return {
            "ok": ok,
            "status_code": resp.status_code,
            "response": payload,
        }
    except Exception as exc:
        return {"ok": False, "error": str(exc)}


def post_to_x_webhook(text: str, post_url: str, candidate: Mapping[str, Any]) -> Dict[str, Any]:
    webhook = os.getenv("TREND_ARB_X_WEBHOOK_URL", "").strip()
    if not webhook:
        return {"ok": False, "error": "TREND_ARB_X_WEBHOOK_URL not set"}

    payload = {
        "text": text,
        "url": post_url,
        "query": candidate.get("query"),
        "score": candidate.get("opportunity_score"),
        "source": "trend_arbitrage_engine",
    }
    try:
        resp = SESSION.post(webhook, json=payload, timeout=20)
        return {
            "ok": 200 <= resp.status_code < 300,
            "status_code": resp.status_code,
            "response": resp.text[:500],
        }
    except Exception as exc:
        return {"ok": False, "error": str(exc)}


def publish_x(
    *,
    blueprint: Mapping[str, Any],
    candidate: Mapping[str, Any],
    post_url: str,
    dry_run: bool,
) -> Dict[str, Any]:
    if not env_bool("TREND_ARB_PUBLISH_X", True):
        return skipped_channel("TREND_ARB_PUBLISH_X=0")

    text = render_x_post(blueprint, post_url)
    if dry_run:
        return {"ok": True, "dry_run": True, "text": text}

    if not is_x_channel_configured():
        return {**skipped_channel("x channel not configured"), "text": text}

    if os.getenv("TREND_ARB_X_WEBHOOK_URL", "").strip():
        result = post_to_x_webhook(text=text, post_url=post_url, candidate=candidate)
    else:
        result = post_to_x_api(text)
    result["text"] = text
    return result


def publish_newsletter(
    *,
    candidate: Mapping[str, Any],
    blueprint: Mapping[str, Any],
    post_url: str,
    dry_run: bool,
) -> Dict[str, Any]:
    if not env_bool("TREND_ARB_PUBLISH_NEWSLETTER", True):
        return skipped_channel("TREND_ARB_PUBLISH_NEWSLETTER=0")

    subject = render_newsletter_subject(blueprint)
    body = render_newsletter_body(candidate=candidate, blueprint=blueprint, post_url=post_url)

    to_raw = os.getenv("TREND_ARB_NEWSLETTER_TO", "")
    to_list = [x.strip() for x in to_raw.split(",") if x.strip()]
    if dry_run:
        return {"ok": True, "dry_run": True, "to": to_list, "subject": subject}

    host = os.getenv("TREND_ARB_SMTP_HOST", "").strip()
    port = int(os.getenv("TREND_ARB_SMTP_PORT", "587"))
    user = os.getenv("TREND_ARB_SMTP_USER", "").strip()
    pwd = os.getenv("TREND_ARB_SMTP_PASS", "").strip()
    sender = os.getenv("TREND_ARB_SMTP_FROM", user).strip()
    use_tls = os.getenv("TREND_ARB_SMTP_TLS", "1").strip() not in ("0", "false", "False")

    if not to_list or not host or not sender:
        return skipped_channel("newsletter channel not configured")

    msg = EmailMessage()
    msg["Subject"] = subject
    msg["From"] = sender
    msg["To"] = ", ".join(to_list)
    msg.set_content(body)

    try:
        with smtplib.SMTP(host, port, timeout=20) as smtp:
            if use_tls:
                smtp.starttls()
            if user:
                smtp.login(user, pwd)
            smtp.send_message(msg)
        return {"ok": True, "to": to_list, "subject": subject}
    except Exception as exc:
        return {"ok": False, "error": str(exc)}


def create_run_report(
    *,
    trends_rows: Sequence[Mapping[str, Any]],
    candidates: Sequence[Mapping[str, Any]],
    eligible: Sequence[Mapping[str, Any]],
    selected: Sequence[Mapping[str, Any]],
    publish_results: Sequence[Mapping[str, Any]],
) -> Dict[str, Any]:
    def _compact(row: Mapping[str, Any]) -> Dict[str, Any]:
        return {
            "id": row.get("id"),
            "query": row.get("query"),
            "demand_score": row.get("demand_score"),
            "gap_score": row.get("gap_score"),
            "intent_score": row.get("intent_score"),
            "opportunity_score": row.get("opportunity_score"),
            "serp_count": len(row.get("serp_rows", []) or []),
        }

    return {
        "ran_at": now_utc_iso(),
        "trends_count": len(trends_rows),
        "candidate_count": len(candidates),
        "eligible_count": len(eligible),
        "selected_count": len(selected),
        "top_candidates": [_compact(row) for row in candidates[:10]],
        "eligible": [_compact(row) for row in eligible[:10]],
        "selected": [_compact(row) for row in selected],
        "publish_results": list(publish_results),
    }


def run_engine(*, dry_run: bool, max_candidates: int, top_n: int, geo: str, lang: str, scan_only: bool) -> Dict[str, Any]:
    trends_rows = fetch_google_daily_trends(geo=geo, hl=lang)
    if not trends_rows:
        logger.warning("no trends fetched, using fallback seeds")
        trends_rows = [
            {"query": "副業 AI", "traffic": "2万+", "articles": []},
            {"query": "自動化 ツール", "traffic": "1万+", "articles": []},
        ]

    query_pool = build_query_pool(
        trends_rows,
        max_queries=max(8, max_candidates),
        lang=lang,
        geo=geo,
    )
    candidates = evaluate_candidates(query_pool)

    state = load_state()
    unpublished = filter_unpublished_candidates(candidates, state)
    min_intent = float(os.getenv("TREND_ARB_MIN_INTENT", "0.55"))
    min_opportunity = float(os.getenv("TREND_ARB_MIN_OPPORTUNITY", "0.50"))
    eligible = [
        row
        for row in unpublished
        if float(row.get("intent_score", 0.0)) >= min_intent
        and float(row.get("opportunity_score", 0.0)) >= min_opportunity
    ]
    selected = eligible[: max(1, top_n)]

    publish_results: List[Dict[str, Any]] = []
    if not scan_only:
        site_enabled = env_bool("TREND_ARB_PUBLISH_SITE", True)
        site_base_url = os.getenv("TREND_ARB_SITE_BASE_URL", "https://example.com/trend-arbitrage").rstrip("/")
        for candidate in selected:
            blueprint = build_blueprint(candidate)
            if site_enabled:
                site_res = publish_site(candidate=candidate, blueprint=blueprint, dry_run=dry_run, state=state)
            else:
                site_res = skipped_channel("TREND_ARB_PUBLISH_SITE=0")
            post_url = str(site_res.get("post_url", f"{site_base_url}/"))
            x_res = publish_x(blueprint=blueprint, candidate=candidate, post_url=post_url, dry_run=dry_run)
            n_res = publish_newsletter(
                candidate=candidate,
                blueprint=blueprint,
                post_url=post_url,
                dry_run=dry_run,
            )
            publish_results.append(
                {
                    "candidate_id": candidate.get("id"),
                    "query": candidate.get("query"),
                    "site": site_res,
                    "x": x_res,
                    "newsletter": n_res,
                }
            )

        if not dry_run:
            save_state(state)

    report = create_run_report(
        trends_rows=trends_rows,
        candidates=candidates,
        eligible=eligible,
        selected=selected,
        publish_results=publish_results,
    )

    ts = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    run_prefix = "run_dry" if dry_run else "run"
    run_path = RUNS_DIR / f"{run_prefix}_{ts}.json"
    save_json(run_path, report)
    save_json(LATEST_RUN_FILE, report)

    logger.info(
        "trend scan: trends=%d candidates=%d eligible=%d selected=%d",
        len(trends_rows),
        len(candidates),
        len(eligible),
        len(selected),
    )
    return report


def main() -> None:
    parser = argparse.ArgumentParser(description="Trend Arbitrage Engine")
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--scan-only", action="store_true")
    parser.add_argument("--max-candidates", type=int, default=int(os.getenv("TREND_ARB_MAX_CANDIDATES", "12")))
    parser.add_argument("--top-n", type=int, default=int(os.getenv("TREND_ARB_TOP_N", "3")))
    parser.add_argument("--geo", default=os.getenv("TREND_ARB_GEO", "JP"))
    parser.add_argument("--lang", default=os.getenv("TREND_ARB_LANG", "ja"))
    parser.add_argument("--write-report", default="")
    parser.add_argument("--print-json", action="store_true", help="Print full JSON report to stdout")
    args = parser.parse_args()

    report = run_engine(
        dry_run=bool(args.dry_run),
        max_candidates=max(4, args.max_candidates),
        top_n=max(1, args.top_n),
        geo=str(args.geo),
        lang=str(args.lang),
        scan_only=bool(args.scan_only),
    )

    if args.write_report:
        save_json(Path(args.write_report), report)

    if args.print_json or os.getenv("TREND_ARB_VERBOSE_JSON", "0") in ("1", "true", "True"):
        print(json.dumps(report, ensure_ascii=False))
    else:
        summary = {
            "ran_at": report.get("ran_at"),
            "trends_count": report.get("trends_count"),
            "candidate_count": report.get("candidate_count"),
            "eligible_count": report.get("eligible_count"),
            "selected_count": report.get("selected_count"),
        }
        print(json.dumps(summary, ensure_ascii=False))


if __name__ == "__main__":
    main()
