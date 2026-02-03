#!/usr/bin/env python3
"""
arXiv Scout v2 - 論文自動巡回・通知システム
独立プロジェクト

機能:
- arXiv APIで関連論文を6時間ごとに巡回
- Gemini APIで10段階評価（具体的な活用法を出力）
- 毎朝8時にDiscordへサマリー通知
- HIGH評価論文を永続ストック
- トレーディング/AI部門の2カテゴリ分類
"""

import os
import hashlib
import sys
import json
import time
import logging
import requests
import xml.etree.ElementTree as ET
from datetime import datetime, timedelta
from pathlib import Path

# =============================================================================
# ログ設定
# =============================================================================

PROJECT_DIR = Path(__file__).parent
LOG_DIR = PROJECT_DIR / "logs"
LOG_DIR.mkdir(exist_ok=True)

logging.basicConfig(
    level=logging.INFO,
    format="[%(asctime)s] %(levelname)s: %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
    handlers=[
        logging.FileHandler(LOG_DIR / "arxiv_scout.log", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
logger = logging.getLogger(__name__)

# =============================================================================
# 設定
# =============================================================================

PROJECT_DIR = Path(__file__).parent
DATA_DIR = PROJECT_DIR / "data"
DATA_DIR.mkdir(exist_ok=True)

# Discord Webhook URL (from env)
def _strip_inline_comment(value: str) -> str:
    if not value:
        return value
    for sep in (" #", "\t#"):
        if sep in value:
            return value.split(sep, 1)[0].rstrip()
    return value.rstrip()


def resolve_discord_webhook():
    raw = os.getenv("SWIMMY_ARXIV_REPORT_WEBHOOK", "")
    raw = raw.strip().strip('"').strip("'")
    return _strip_inline_comment(raw)


def mask_webhook(url: str) -> str:
    if not url:
        return "unset"
    tail = url[-6:] if len(url) > 6 else url
    return f"...{tail}"


# Gemini API - .envファイルから読み込み
def load_env_file():
    """Swimmyの.envファイルからAPIキーを読み込む"""
    env_paths = [
        Path("/home/swimmy/swimmy/config/.env"),
        Path("/home/swimmy/swimmy/.env"),
        Path("/home/swimmy/.env"),
    ]
    for env_path in env_paths:
        if not env_path.exists():
            continue
        with open(env_path, "r") as f:
            for line in f:
                line = line.strip()
                if line.startswith("export ") and "=" in line:
                    line = line[7:]  # 'export ' を削除
                if "=" in line and not line.startswith("#"):
                    key, value = line.split("=", 1)
                    value = value.strip('"').strip("'")
                    os.environ[key] = value
        return


load_env_file()
GEMINI_API_KEY = os.getenv("SWIMMY_GEMINI_API_KEY")
GEMINI_AVAILABLE = bool(GEMINI_API_KEY)
_GEMINI_WARNED = False

# =============================================================================
# フィルタリング基準（検索クエリ）
# =============================================================================
# arXivは全論文を取得するAPIはない。カテゴリ/キーワードで検索する仕組み。
# 以下のクエリで関連分野を網羅的にカバー。

SEARCH_QUERIES = [
    # ===== 金融・トレーディング =====
    "cat:q-fin.TR",  # Quantitative Finance - Trading & Market Microstructure
    "cat:q-fin.PM",  # Portfolio Management
    "cat:q-fin.RM",  # Risk Management
    "cat:q-fin.CP",  # Computational Finance
    "cat:q-fin.ST",  # Statistical Finance
    # ===== 強化学習 × 金融 =====
    "all:reinforcement+learning+trading",
    "all:reinforcement+learning+portfolio",
    "all:deep+reinforcement+learning+finance",
    # ===== 時系列予測 =====
    "all:time+series+forecasting+transformer",
    "all:stock+prediction+deep+learning",
    "all:forex+prediction",
    # ===== LLM・エージェント =====
    "all:large+language+model+finance",
    "all:LLM+agent+autonomous",
    "all:multi+agent+trading",
    # ===== 因果推論・意思決定 =====
    "all:causal+inference+decision+making",
]

# =============================================================================
# RePEc RSS (NEP) - lightweight intake
# =============================================================================

REPEC_RSS_FEEDS = [
    ("https://nep.repec.org/rss/nep-fmk.rss.xml", ["q-fin.TR"]),
    ("https://nep.repec.org/rss/nep-mon.rss.xml", ["q-fin.RM"]),
    ("https://nep.repec.org/rss/nep-rmg.rss.xml", ["q-fin.RM"]),
    ("https://nep.repec.org/rss/nep-big.rss.xml", ["cs.LG"]),
    ("https://nep.repec.org/rss/nep-cmp.rss.xml", ["cs.LG"]),
    ("https://nep.repec.org/rss/nep-ict.rss.xml", ["cs.LG"]),
]

REPEC_KEYWORDS_FX = [
    "fx",
    "forex",
    "foreign exchange",
    "currency",
    "exchange rate",
    "market microstructure",
    "trading",
]
REPEC_KEYWORDS_AI = [
    "ai",
    "artificial intelligence",
    "machine learning",
    "deep learning",
    "transformer",
    "llm",
    "reinforcement learning",
    "neural",
]


def _env_int(name: str, default: int) -> int:
    try:
        return int(os.getenv(name, default))
    except Exception:
        return default


# Increase to avoid missing new papers beyond top-10 results per query
MAX_RESULTS_PER_QUERY = _env_int("SWIMMY_ARXIV_MAX_RESULTS", 30)

# ファイルパス
SEEN_PAPERS_FILE = DATA_DIR / "seen_papers.json"
PENDING_PAPERS_FILE = DATA_DIR / "pending_papers.json"
STOCK_PAPERS_FILE = DATA_DIR / "stock_papers.json"  # HIGH評価論文の永続保存
LAST_REPORT_FILE = DATA_DIR / "last_report.json"  # Discord連携用（番号→論文マッピング）

# =============================================================================
# ユーティリティ
# =============================================================================


def load_json(filepath):
    if filepath.exists():
        with open(filepath, "r", encoding="utf-8") as f:
            return json.load(f)
    return {}


def save_json(filepath, data):
    with open(filepath, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2, ensure_ascii=False)


# =============================================================================
# arXiv API
# =============================================================================


def search_arxiv(query, max_results=20):
    """arXiv APIで検索"""
    base_url = "http://export.arxiv.org/api/query"
    params = {
        "search_query": query,
        "start": 0,
        "max_results": max_results,
        "sortBy": "submittedDate",
        "sortOrder": "descending",
    }

    try:
        response = requests.get(base_url, params=params, timeout=30)
        response.raise_for_status()
        return response.text
    except Exception as e:
        logger.info(f" API Error: {e}")
        return None


def parse_arxiv_response(xml_text):
    """arXiv APIレスポンスをパース"""
    papers = []
    try:
        root = ET.fromstring(xml_text)
        ns = {"atom": "http://www.w3.org/2005/Atom"}

        for entry in root.findall("atom:entry", ns):
            paper = {
                "id": entry.find("atom:id", ns).text.split("/")[-1],
                "title": entry.find("atom:title", ns).text.strip().replace("\n", " "),
                "summary": entry.find("atom:summary", ns).text.strip(),  # 全文取得
                "authors": [
                    a.find("atom:name", ns).text
                    for a in entry.findall("atom:author", ns)
                ][:5],
                "published": entry.find("atom:published", ns).text[:10],
                "link": entry.find("atom:id", ns).text,
            }
            categories = entry.findall("atom:category", ns)
            paper["categories"] = [c.get("term") for c in categories][:5]
            papers.append(paper)
    except Exception as e:
        logger.info(f" Parse Error: {e}")
    return papers


# =============================================================================
# RePEc RSS helpers
# =============================================================================


def fetch_repec_rss(feed_url: str):
    try:
        resp = requests.get(feed_url, timeout=20)
        resp.raise_for_status()
        return resp.text
    except Exception as e:
        logger.info(f" RePEc RSS Error: {e}")
        return None


def parse_repec_rss(xml_text: str):
    items = []
    if not xml_text:
        return items
    try:
        root = ET.fromstring(xml_text)
        ns = {
            "rss": "http://purl.org/rss/1.0/",
            "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            "dc": "http://purl.org/dc/elements/1.1/",
        }
        for item in root.findall("rss:item", ns):
            title = item.findtext("rss:title", default="", namespaces=ns)
            link = item.findtext("rss:link", default="", namespaces=ns)
            desc = item.findtext("rss:description", default="", namespaces=ns)
            creators = [c.text for c in item.findall("dc:creator", ns) if c.text]
            date = item.findtext("dc:date", default="", namespaces=ns)
            items.append(
                {
                    "title": title.strip(),
                    "link": link.strip(),
                    "description": (desc or "").strip(),
                    "authors": creators[:5],
                    "published": (date or "").strip(),
                }
            )
    except Exception as e:
        logger.info(f" RePEc RSS Parse Error: {e}")
    return items


def repec_matches_keywords(text: str, keywords):
    if not text:
        return False
    lower = text.lower()
    return any(kw in lower for kw in keywords)


def _extract_arxiv_id_from_repec_link(link: str):
    marker = "RePEc:arx:papers:"
    if marker in link:
        return link.split(marker, 1)[1].split("&", 1)[0]
    return None


def normalize_repec_item(item: dict, default_categories):
    link = item.get("link", "")
    arxiv_id = _extract_arxiv_id_from_repec_link(link)
    if arxiv_id:
        paper_id = arxiv_id
    else:
        base = (item.get("title", "") + link).encode("utf-8")
        paper_id = "repec:" + hashlib.sha1(base).hexdigest()
    return {
        "id": paper_id,
        "title": item.get("title", ""),
        "summary": item.get("description", ""),
        "authors": item.get("authors", [])[:5],
        "published": item.get("published", ""),
        "link": link,
        "categories": default_categories,
    }


def fetch_repec_papers():
    papers = []
    for feed_url, default_categories in REPEC_RSS_FEEDS:
        xml_text = fetch_repec_rss(feed_url)
        if not xml_text:
            continue
        items = parse_repec_rss(xml_text)
        keywords = (
            REPEC_KEYWORDS_FX
            if "q-fin" in " ".join(default_categories)
            else REPEC_KEYWORDS_AI
        )
        for item in items:
            text = f"{item.get('title', '')} {item.get('description', '')}"
            if not repec_matches_keywords(text, keywords):
                continue
            papers.append(normalize_repec_item(item, default_categories))
    return papers


# =============================================================================
# Gemini API 評価（論文内容を深く読んで判断）
# =============================================================================


def evaluate_with_gemini(paper):
    """Gemini APIで論文内容を深く読んで評価"""
    global _GEMINI_WARNED
    if not GEMINI_AVAILABLE:
        if not _GEMINI_WARNED:
            logger.info(" ⚠️ GEMINI_API_KEY未設定: 簡易評価モードにフォールバック")
            _GEMINI_WARNED = True
        return None

    full_abstract = paper.get("summary", "")

    prompt = f"""あなたは金融工学とAI研究の専門家であり、FXトレーダーでもあります。
以下の論文を読んで、実務的な観点から評価してください。

━━━━━━━━━━━━━━━━━━━━━━━
【タイトル】{paper['title']}
【著者】{', '.join(paper['authors'])}
【カテゴリ】{', '.join(paper['categories'])}
【Abstract】
{full_abstract}
━━━━━━━━━━━━━━━━━━━━━━━

以下のJSON形式**のみ**で出力してください（説明文不要）:

{{
  "score": 1〜10の整数,
  "summary_ja": "論文の内容を平易な日本語で説明（80字程度）",
  "key_tech": "核となる技術/手法名（例: LSTM, Transformer, DQN, Kalman Filter等）",
  "how_to_use": "この論文の知見をFXトレーディングにどう活かせるか、具体的なアクションを説明（100字程度）。例: 『この手法をエントリーシグナルに組み込むことで〇〇できる』『〇〇の判断に使える』など"
}}

【スコア基準】
10=即実装可能な革新的手法, 9=強化学習×金融の最先端, 8=時系列予測に直接使える
7=工夫次第で使える, 6=改良だが応用余地あり, 5=間接参考
4=応用困難, 3=理論のみ, 2=ほぼ無関係, 1=無関係

【重要】
- how_to_useは「〜に役立つ可能性がある」のような曖昧な表現ではなく、具体的にどうするかを書くこと
- 例: 「ボラティリティ急変時のロット調整アルゴリズムに組み込める」「トレンド転換の検知精度を上げるフィルターとして使える」"""

    try:
        url = f"https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent?key={GEMINI_API_KEY}"
        response = requests.post(
            url,
            json={
                "contents": [{"parts": [{"text": prompt}]}],
                "generationConfig": {"temperature": 0.5, "maxOutputTokens": 600},
            },
            timeout=90,
        )

        if response.status_code == 200:
            result = response.json()
            text = result["candidates"][0]["content"]["parts"][0]["text"]
            start = text.find("{")
            end = text.rfind("}") + 1
            if start >= 0 and end > start:
                eval_data = json.loads(text[start:end])
                score = int(eval_data.get("score", 5))
                eval_data["score"] = score
                # スコアからrelevanceを自動計算
                if score >= 8:
                    eval_data["relevance"] = "HIGH"
                elif score >= 5:
                    eval_data["relevance"] = "MEDIUM"
                else:
                    eval_data["relevance"] = "LOW"
                return eval_data
        else:
            logger.info(f" Gemini API Error: {response.status_code}")
    except Exception as e:
        logger.info(f" Gemini Error: {e}")

    return None


# =============================================================================
# Discord 通知（2カテゴリ: トレーディング/AI部門）
# =============================================================================

# カテゴリ分類用
TRADING_CATEGORIES = {
    "q-fin.TR",
    "q-fin.PM",
    "q-fin.RM",
    "q-fin.CP",
    "q-fin.ST",
    "q-fin.GN",
    "q-fin.MF",
}
AI_CATEGORIES = {"cs.LG", "cs.AI", "cs.NE", "cs.CL", "stat.ML"}


FALLBACK_TECH_KEYWORDS = [
    ("transformer", "Transformer"),
    ("attention", "Attention"),
    ("lstm", "LSTM"),
    ("gru", "GRU"),
    ("rnn", "RNN"),
    ("cnn", "CNN"),
    ("kalman", "Kalman Filter"),
    ("hmm", "HMM"),
    ("gpt", "GPT/LLM"),
    ("llm", "LLM"),
    ("reinforcement learning", "Reinforcement Learning"),
    ("policy gradient", "Policy Gradient"),
    ("q-learning", "Q-Learning"),
    ("xgboost", "XGBoost"),
    ("graph", "Graph/Network"),
]


def infer_key_tech(text):
    text = text.lower()
    for kw, label in FALLBACK_TECH_KEYWORDS:
        if kw in text:
            return label
    return "-"


def score_to_priority(score):
    try:
        s = int(score)
    except Exception:
        return "C"
    if s >= 9:
        return "S"
    if s >= 8:
        return "A"
    if s >= 6:
        return "B"
    return "C"


def fallback_evaluate(paper):
    """Gemini未使用時の簡易評価"""
    cats = set(paper.get("categories", []))
    if cats & TRADING_CATEGORIES:
        score = 6
        how_to_use = "シグナル生成/リスク管理の仮説として検討。"
    elif cats & AI_CATEGORIES:
        score = 5
        how_to_use = "特徴量設計やモデル選定の参考として検討。"
    else:
        score = 4
        how_to_use = "関連度が低いため参考程度。"

    relevance = "HIGH" if score >= 8 else ("MEDIUM" if score >= 5 else "LOW")

    summary_src = (paper.get("summary") or "").replace("\n", " ").strip()
    if summary_src:
        summary_ja = summary_src[:120] + ("..." if len(summary_src) > 120 else "")
    else:
        summary_ja = (paper.get("title") or "")[:120]

    text_blob = f"{paper.get('title', '')} {paper.get('summary', '')}"
    key_tech = infer_key_tech(text_blob)

    return {
        "score": score,
        "summary_ja": summary_ja,
        "key_tech": key_tech,
        "how_to_use": how_to_use,
        "relevance": relevance,
    }

def classify_paper(paper):
    """論文をトレーディング/AI部門に分類"""
    cats = set(paper.get("categories", []))
    # 金融系カテゴリがあればトレーディング
    if cats & TRADING_CATEGORIES:
        return "trading"
    # AI/ML系カテゴリがあればAI
    if cats & AI_CATEGORIES:
        return "ai"
    # キーワードで判定
    title_lower = paper.get("title", "").lower()
    if any(
        kw in title_lower
        for kw in ["trading", "forex", "portfolio", "stock", "financial", "market"]
    ):
        return "trading"
    return "ai"


def is_repec_paper(paper):
    link = (paper.get("link") or "").lower()
    pid = (paper.get("id") or "").lower()
    return ("repec.org" in link) or pid.startswith("repec:")


def send_summary_to_discord(papers_with_eval):
    """2カテゴリに分けてDiscordに送信"""
    if not papers_with_eval:
        logger.info(" 通知する論文なし")
        return
    webhook_url = resolve_discord_webhook()
    if not webhook_url:
        logger.info(" ⚠️ Discord Webhook未設定: 通知をスキップ")
        return

    # スコア順にソート（降順）
    papers_with_eval.sort(key=lambda x: x.get("score", 0), reverse=True)

    # カテゴリ分類
    trading_papers = []
    ai_papers = []
    repec_papers = []

    for p in papers_with_eval:
        cat = classify_paper(p)
        if cat == "trading":
            trading_papers.append(p)
        else:
            ai_papers.append(p)
        if is_repec_paper(p):
            repec_papers.append(p)

    now = datetime.now().strftime("%Y-%m-%d")

    # Pre-assign global numbers for Discord連携 (must match last_report.json)
    global_num = 1
    for p in trading_papers + ai_papers:
        p["number"] = global_num
        global_num += 1

    # ===== トレーディング部門 =====
    if trading_papers:
        high_count = len([p for p in trading_papers if p.get("relevance") == "HIGH"])
        med_count = len([p for p in trading_papers if p.get("relevance") == "MEDIUM"])

        content = f"# 📈 arXiv Scout - トレーディング部門\n"
        content += f"**{now}** | 🔥 HIGH: {high_count}件 | 📄 MEDIUM: {med_count}件\n"
        content += "━━━━━━━━━━━━━━━━━━━━\n\n"

        for p in trading_papers[:5]:  # TOP 5
            num = p["number"]  # Use global number
            score = p.get("score", 0)
            emoji = "🔥" if score >= 8 else ("📄" if score >= 5 else "⏭️")
            priority = score_to_priority(score)

            content += (
                f"**{num}. [{priority}] {emoji} [{score}/10] {p['title'][:50]}**\n"
            )
            content += f"🔧 **技術:** {p.get('key_tech', '-')}\n"
            content += f"📝 {p.get('summary_ja', '')}\n"
            content += (
                f"💡 **活用法:** {p.get('how_to_use', p.get('usefulness', ''))}\n"
            )
            content += f"🔗 <{p['link']}>\n\n"

        if len(content) > 1950:
            content = content[:1900] + "\n\n...(続きはストック参照)"

        try:
            resp = requests.post(webhook_url, json={"content": content}, timeout=15)
            logger.info(
                f" ✅ トレーディング部門送信: {resp.status_code} ({mask_webhook(webhook_url)})"
            )
        except Exception as e:
            logger.info(f" ❌ Discord Error: {e}")

        time.sleep(2)

    # ===== AI/ML部門 =====
    if ai_papers:
        high_count = len([p for p in ai_papers if p.get("relevance") == "HIGH"])
        med_count = len([p for p in ai_papers if p.get("relevance") == "MEDIUM"])

        content = f"# 🤖 arXiv Scout - AI/ML部門\n"
        content += f"**{now}** | 🔥 HIGH: {high_count}件 | 📄 MEDIUM: {med_count}件\n"
        content += "━━━━━━━━━━━━━━━━━━━━\n\n"

        for p in ai_papers[:5]:  # TOP 5
            num = p["number"]  # Use global number
            score = p.get("score", 0)
            emoji = "🔥" if score >= 8 else ("📄" if score >= 5 else "⏭️")
            priority = score_to_priority(score)

            content += (
                f"**{num}. [{priority}] {emoji} [{score}/10] {p['title'][:50]}**\n"
            )
            content += f"🔧 **技術:** {p.get('key_tech', '-')}\n"
            content += f"📝 {p.get('summary_ja', '')}\n"
            content += (
                f"💡 **活用法:** {p.get('how_to_use', p.get('usefulness', ''))}\n"
            )
            content += f"🔗 <{p['link']}>\n\n"

        if len(content) > 1950:
            content = content[:1900] + "\n\n...(続きはストック参照)"

        try:
            resp = requests.post(webhook_url, json={"content": content}, timeout=15)
            logger.info(
                f" ✅ AI/ML部門送信: {resp.status_code} ({mask_webhook(webhook_url)})"
            )
        except Exception as e:
            logger.info(f" ❌ Discord Error: {e}")

    # ===== RePEc Picks =====
    repec_trading = [p for p in repec_papers if classify_paper(p) == "trading"]
    repec_ai = [p for p in repec_papers if classify_paper(p) != "trading"]

    def _send_repec_section(title, papers):
        high_count = len([p for p in papers if p.get("relevance") == "HIGH"])
        med_count = len([p for p in papers if p.get("relevance") == "MEDIUM"])

        content = f"# 📚 RePEc Picks - {title}\n"
        content += f"**{now}** | 🔥 HIGH: {high_count}件 | 📄 MEDIUM: {med_count}件\n"
        content += "━━━━━━━━━━━━━━━━━━━━\n\n"

        for p in papers[:5]:  # TOP 5 (empty ok)
            num = p["number"]
            score = p.get("score", 0)
            emoji = "🔥" if score >= 8 else ("📄" if score >= 5 else "⏭️")
            priority = score_to_priority(score)

            content += (
                f"**{num}. [{priority}] {emoji} [{score}/10] {p['title'][:50]}**\n"
            )
            content += f"🔧 **技術:** {p.get('key_tech', '-')}\n"
            content += f"📝 {p.get('summary_ja', '')}\n"
            content += (
                f"💡 **活用法:** {p.get('how_to_use', p.get('usefulness', ''))}\n"
            )
            content += f"🔗 <{p['link']}>\n\n"

        if len(content) > 1950:
            content = content[:1900] + "\n\n...(続きはストック参照)"

        try:
            resp = requests.post(webhook_url, json={"content": content}, timeout=15)
            logger.info(
                f" ✅ RePEc Picks送信: {resp.status_code} ({mask_webhook(webhook_url)})"
            )
        except Exception as e:
            logger.info(f" ❌ Discord Error: {e}")

    _send_repec_section("トレーディング", repec_trading)
    _send_repec_section("AI/ML", repec_ai)

    # ===== Discord連携用: 番号→論文マッピング保存 =====
    all_papers = trading_papers + ai_papers
    report_data = {
        "date": now,
        "papers": [],
        "trading_count": len(trading_papers),
        "ai_count": len(ai_papers),
    }
    for i, p in enumerate(all_papers[:20], 1):  # 最大20件
        priority = score_to_priority(p.get("score", 0))
        report_data["papers"].append(
            {
                "number": i,
                "id": p.get("id"),
                "title": p.get("title"),
                "link": p.get("link"),
                "score": p.get("score"),
                "priority": priority,
                "category": classify_paper(p),
                "key_tech": p.get("key_tech"),
                "how_to_use": p.get("how_to_use", p.get("usefulness", "")),
            }
        )
    save_json(LAST_REPORT_FILE, report_data)
    logger.info(f" 📝 last_report.json保存: {len(report_data['papers'])}件")


# =============================================================================
# HIGH論文ストック
# =============================================================================


def stock_high_papers(papers_with_eval):
    """HIGH評価の論文を永続保存"""
    stock_data = load_json(STOCK_PAPERS_FILE)
    stock_papers = stock_data.get("papers", [])
    stock_ids = {p["id"] for p in stock_papers}

    added = 0
    for p in papers_with_eval:
        if p.get("relevance") == "HIGH" and p["id"] not in stock_ids:
            stock_papers.append(
                {
                    "id": p["id"],
                    "title": p["title"],
                    "link": p["link"],
                    "summary_ja": p.get("summary_ja", ""),
                    "usefulness": p.get("usefulness", ""),
                    "key_tech": p.get("key_tech", ""),
                    "score": p.get("score", 0),
                    "published": p["published"],
                    "stocked_at": datetime.now().isoformat(),
                }
            )
            added += 1

    stock_data["papers"] = stock_papers
    stock_data["last_updated"] = datetime.now().isoformat()
    save_json(STOCK_PAPERS_FILE, stock_data)

    logger.info(f" 📦 {added}件のHIGH論文をストック")
    return added


# =============================================================================
# メイン処理
# =============================================================================


def scout_papers():
    """論文巡回＋評価"""
    print(f"\n[Scout] 🔍 巡回開始: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

    seen_data = load_json(SEEN_PAPERS_FILE)
    seen_ids = set(seen_data.get("papers", []))

    pending_data = load_json(PENDING_PAPERS_FILE)
    pending_papers = pending_data.get("papers", [])

    new_count = 0

    for query in SEARCH_QUERIES:
        logger.info(f" 検索: {query[:40]}...")

        xml_response = search_arxiv(query, max_results=MAX_RESULTS_PER_QUERY)
        if not xml_response:
            continue

        papers = parse_arxiv_response(xml_response)

        for paper in papers:
            paper_id = paper["id"]
            if paper_id in seen_ids:
                continue

            logger.info(f" 📄 {paper['title'][:50]}...")

            # Gemini評価（失敗時は簡易評価）
            evaluation = evaluate_with_gemini(paper)
            if not evaluation:
                evaluation = fallback_evaluate(paper)

            paper_with_eval = {**paper, **evaluation}

            # HIGH/MEDIUMのみ保存
            if evaluation.get("relevance") in ["HIGH", "MEDIUM"]:
                pending_papers.append(paper_with_eval)
                new_count += 1
                print(
                    f"[Scout] ✅ {evaluation.get('relevance')} (score {evaluation.get('score')})"
                )
            else:
                logger.info(" ⏭️ LOW - スキップ")

            seen_ids.add(paper_id)
            time.sleep(0.5)  # API制限対策

        time.sleep(1)

    # RePEc RSS (optional, lightweight)
    repec_papers = fetch_repec_papers()
    for paper in repec_papers:
        paper_id = paper["id"]
        if paper_id in seen_ids:
            continue

        evaluation = evaluate_with_gemini(paper)
        if not evaluation:
            evaluation = fallback_evaluate(paper)

        paper_with_eval = {**paper, **evaluation}

        if evaluation.get("relevance") in ["HIGH", "MEDIUM"]:
            pending_papers.append(paper_with_eval)
            new_count += 1
        seen_ids.add(paper_id)

    # 保存
    seen_data["papers"] = list(seen_ids)[-3000:]
    seen_data["last_check"] = datetime.now().isoformat()
    save_json(SEEN_PAPERS_FILE, seen_data)

    pending_data["papers"] = pending_papers
    pending_data["last_updated"] = datetime.now().isoformat()
    save_json(PENDING_PAPERS_FILE, pending_data)

    logger.info(f" ✅ 巡回完了: {new_count}件を評価・保存")
    return new_count


def send_morning_report():
    """朝8時のサマリー通知"""
    print(f"\n[Scout] 📬 朝のレポート: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

    today = datetime.now().date()
    last_sent_date = get_last_sent_date()
    if last_sent_date == today:
        logger.info(" 🔁 本日分は既に送信済み")
        return

    pending_data = load_json(PENDING_PAPERS_FILE)
    pending_papers = pending_data.get("papers", [])

    if pending_papers:
        # HIGH論文をストック
        stock_high_papers(pending_papers)

        # サマリー送信
        send_summary_to_discord(pending_papers)

        # ペンディングクリア
        pending_data["papers"] = []
        pending_data["last_sent"] = datetime.now().isoformat()
        save_json(PENDING_PAPERS_FILE, pending_data)
    else:
        logger.info(" ペンディングなし")
        pending_data["last_sent"] = datetime.now().isoformat()
        save_json(PENDING_PAPERS_FILE, pending_data)


def get_last_sent_date():
    data = load_json(PENDING_PAPERS_FILE)
    last_sent = data.get("last_sent")
    if not last_sent:
        return None
    try:
        return datetime.fromisoformat(last_sent).date()
    except Exception:
        return None


def run_scheduler():
    """スケジューラーモード"""
    logger.info(" 🚀 スケジューラー開始")
    logger.info(" 📅 巡回: 0:00, 6:00, 12:00, 18:00")
    logger.info(" 📬 通知: 毎朝 8:00")

    # 初回巡回
    scout_papers()

    # 08:00を過ぎて起動した場合のキャッチアップ送信
    now = datetime.now()
    last_sent_date = get_last_sent_date()
    if now.hour >= 8 and last_sent_date != now.date():
        logger.info(" ⏱️ 08:00を過ぎて起動: 朝レポートをキャッチアップ送信")
        send_morning_report()

    while True:
        now = datetime.now()

        # 次のイベント計算
        current_hour = now.hour
        next_scout_hour = ((current_hour // 6) + 1) * 6
        if next_scout_hour >= 24:
            next_scout = now.replace(hour=0, minute=0, second=0) + timedelta(days=1)
        else:
            next_scout = now.replace(hour=next_scout_hour, minute=0, second=0)

        if now.hour < 8:
            next_notify = now.replace(hour=8, minute=0, second=0)
        else:
            next_notify = now.replace(hour=8, minute=0, second=0) + timedelta(days=1)

        if next_scout < next_notify:
            wait_seconds = (next_scout - now).total_seconds()
            event = "scout"
            next_time = next_scout
        else:
            wait_seconds = (next_notify - now).total_seconds()
            event = "notify"
            next_time = next_notify

        print(
            f"[Scout] 💤 次回 {event}: {next_time.strftime('%m/%d %H:%M')} ({wait_seconds/3600:.1f}h後)"
        )
        time.sleep(max(wait_seconds, 60))

        if event == "scout":
            scout_papers()
        else:
            send_morning_report()


def show_stock():
    """ストック論文を表示"""
    stock_data = load_json(STOCK_PAPERS_FILE)
    papers = stock_data.get("papers", [])

    print(f"\n📦 HIGH論文ストック ({len(papers)}件)\n")
    for i, p in enumerate(papers[-10:], 1):  # 最新10件
        print(f"{i}. [{p.get('score', '?')}/10] {p['title'][:60]}")
        print(f"   💡 {p.get('usefulness', '')}")
        print(f"   🔗 {p['link']}\n")


if __name__ == "__main__":
    import sys

    if len(sys.argv) > 1:
        cmd = sys.argv[1]
        if cmd == "--scout":
            scout_papers()
        elif cmd == "--notify":
            send_morning_report()
        elif cmd == "--daily":
            scout_papers()
            send_morning_report()
        elif cmd == "--daemon":
            run_scheduler()
        elif cmd == "--stock":
            show_stock()
        else:
            print("Usage: arxiv_scout.py [--scout|--notify|--daily|--daemon|--stock]")
    else:
        # 単発: 巡回→通知
        scout_papers()
        send_morning_report()
