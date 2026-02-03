# 📚 arXiv Scout

arXiv論文自動巡回・通知システム

## 機能

- **定期巡回**: 6時間ごと（0:00, 6:00, 12:00, 18:00）
- **通知**: 毎朝 8:00 にDiscordへまとめて送信
- **自動評価**: Gemini APIで論文の有用性を評価（HIGH/MEDIUM/LOW）
- **フィルタリング**: HIGH/MEDIUM のみ通知

## 検索対象

| カテゴリ | 内容 |
|----------|------|
| `q-fin.TR` | Quantitative Finance - Trading |
| `q-fin.PM` | Portfolio Management |
| `q-fin.RM` | Risk Management |
| `cs.AI` | Artificial Intelligence |
| `cs.LG` | Machine Learning |
| キーワード | reinforcement learning, LLM, time series, etc. |

## セットアップ

```bash
# 依存パッケージ
pip install requests

# 環境変数
export SWIMMY_GEMINI_API_KEY="your-api-key"
export SWIMMY_ARXIV_REPORT_WEBHOOK="your_discord_webhook"
# オプション: 1クエリあたりの取得件数（デフォルト: 30）
export SWIMMY_ARXIV_MAX_RESULTS="30"
```

## 使い方

```bash
# 単発実行（巡回＋即通知）
python arxiv_scout.py

# 巡回のみ（通知しない）
python arxiv_scout.py --scout

# 通知のみ（ペンディング分を送信）
python arxiv_scout.py --notify

# 1日分まとめて実行（巡回＋通知）
python arxiv_scout.py --daily

# デーモンモード（スケジューラー）
python arxiv_scout.py --daemon
```

## screenで常駐

```bash
screen -S arxiv-scout
python arxiv_scout.py --daemon
# Ctrl+A, D で離脱
```

## cron設定（代替）

```bash
# crontab -e
# 6時間ごと巡回
0 0,6,12,18 * * * cd /home/swimmy/arxiv-scout && python arxiv_scout.py --scout

# 毎朝8時に通知
0 8 * * * cd /home/swimmy/arxiv-scout && python arxiv_scout.py --notify
```

## ディレクトリ構成

```
arxiv-scout/
├── arxiv_scout.py      # メインスクリプト
├── README.md
├── run.sh              # 起動スクリプト
└── data/
    ├── seen_papers.json    # 既読論文ID
    └── pending_papers.json # 通知待ち論文
```

---
*Created: 2025-12-28*
