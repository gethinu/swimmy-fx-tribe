#!/bin/bash
# arXiv Scout 起動スクリプト

cd "$(dirname "$0")"

echo "📚 arXiv Scout 起動..."
echo "📅 巡回: 0:00, 6:00, 12:00, 18:00"
echo "📬 通知: 毎朝 8:00"

# Gemini API Key (Swimmyと共有)
export SWIMMY_GEMINI_API_KEY="${SWIMMY_GEMINI_API_KEY:-}"

# デーモンモードで起動
python3 arxiv_scout.py --daemon
