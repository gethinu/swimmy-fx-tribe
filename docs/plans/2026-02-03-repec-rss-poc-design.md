# RePEc RSS POC Design

Date: 2026-02-03
Owner: Swimmy / Codex
Status: Draft

## 1. Goal / Scope
- Goal: Add a minimal RePEc intake path that pulls AI/FX-related papers via NEP RSS and feeds them into the existing arXiv scout pipeline.
- Goal: Keep it small, resilient, and consistent with current scoring/Discord output.
- Non-goal: Full RePEc crawl, metadata dumps, or complex adapters.
- Non-goal: Manual external_papers ingestion (removed).

## 2. Key Decisions
- Use NEP RSS feeds as the source (stable and lightweight).
- Use keyword filtering (AI + FX) on title/description to keep scope small.
- Normalize each item into the existing paper schema and reuse the current evaluation/notification flow.
- Deduplicate via arXiv ID when present; otherwise generate a RePEc-scoped ID.

## 3. Feeds and Keywords
### 3.1 Feeds (initial POC)
- Trading/FX-oriented: nep-fmk, nep-mon, nep-rmg
- AI-oriented: nep-big, nep-cmp, nep-ict

### 3.2 Keyword sets
- FX keywords (example): fx, forex, foreign exchange, currency, exchange rate, market microstructure, trading
- AI keywords (example): ai, artificial intelligence, machine learning, deep learning, transformer, llm, reinforcement learning, neural

## 4. Architecture (Minimal)
- Add the following functions to `/home/swimmy/swimmy/tools/arxiv-scout/arxiv_scout.py`:
  - `fetch_repec_rss(feed_url)` -> raw XML or None
  - `parse_repec_rss(xml)` -> list of raw items
  - `normalize_repec_item(item, default_categories)` -> paper dict
  - `fetch_repec_papers()` -> list of normalized papers
- Add constants:
  - `REPEC_RSS_FEEDS` with feed URL and default category (trading/ai)
  - `REPEC_KEYWORDS_FX`, `REPEC_KEYWORDS_AI`
- Integrate in `scout_papers()` after arXiv search loop:
  - iterate RePEc items, keyword filter, dedupe, evaluate, append to pending

## 5. Data Flow
1) Fetch RSS feed -> parse items
2) Normalize item to paper schema
3) Keyword filter by feed type
4) Deduplicate (arXiv ID or repec:<hash>)
5) Evaluate (Gemini or fallback)
6) Save to pending and seen IDs

## 6. Error Handling
- RSS fetch failure: log and skip that feed (do not fail the entire run).
- XML parse failure: log and skip.
- Missing fields: use empty summary and minimal title handling.
- ID strategy:
  - If link contains `RePEc:arx:papers:<id>` map to arXiv ID `<id>`.
  - Else use `repec:<sha1(title+link)>`.

## 7. Testing (TDD)
- Unit: RSS XML -> normalized items.
- Unit: arXiv ID extraction from RePEc links.
- Unit: keyword filter behavior for AI vs FX.
- Unit: feed errors do not raise exceptions.
- Tests use fixed XML strings and no network.

## 8. Rollout
- POC only. Enable by default and rely on logs to confirm intake volume.
- If output volume is too high, adjust keyword lists or feed list.

## 9. Open Questions
- Do we want a simple env toggle (e.g., SWIMMY_REPEC_ENABLED) for quick disable?
- Should we cap RePEc items per feed to reduce noise?
