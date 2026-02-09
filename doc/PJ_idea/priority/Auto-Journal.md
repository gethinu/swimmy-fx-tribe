# Swimmy Auto-Journal (自動日報システム)

**コンセプト**: スクリーンショット → OCR → AI日報生成

---

## 1. システム概要

毎分PCのスクリーンショットを撮影し、OCRでテキスト化、AIが1日の作業を自動でまとめる。

```
[毎分] Screenshot → OCR → JSON蓄積
[1日の終わり] JSON一括 → AI分析 → 日報生成
```

---

## 2. アーキテクチャ (Swimmy流)

### The Lisp-Rust Axis

1. **The Eye (Rust)**:
   - **役割**: スクリーンショット撮影 + OCR
   - **技術**: 
     - `screenshots` クレート（スクリーンショット）
     - `tesseract` or macOS Vision API（OCR）
     - `serde_json`（JSON出力）
   - **出力**: `{timestamp, screen_text, active_window}`

2. **The Brain (Common Lisp / Python)**:
   - **役割**: 蓄積されたJSONをAIに投げて日報生成
   - **技術**: 
     - OpenAI API / Gemini API
     - プロンプトエンジニアリング
   - **出力**: Markdown形式の日報

---

## 3. データ構造

### スクリーンショットログ (1分ごと)
```json
{
  "timestamp": "2025-12-26T12:30:00+09:00",
  "active_window": "VS Code - brain.lisp",
  "ocr_text": "defun calculate-opportunity ...",
  "tags": ["coding", "lisp"]
}
```

### 日報出力
```markdown
# 2025-12-26 作業日報

## 📊 サマリー
- 総作業時間: 8時間
- メイン作業: Swimmy FX 開発
- 学び: Rust と Lisp の連携パターン

## 📝 タイムライン
- 09:00-10:30: brain.lisp のリファクタリング
- 10:30-12:00: Discord bot 機能追加
- 13:00-15:00: プロジェクトアイデア整理

## 💡 学んだこと
- ZeroMQ の非同期パターン
- GitHub Push Protection の対応方法

## 🎯 明日やること
- Oshi-Yado MVP 設計開始
```

---

## 4. 実装計画

### Week 1: MVP (macOS版)
- [ ] Python スクリプトでスクリーンショット（`pyscreenshot`）
- [ ] macOS Vision Framework で OCR
- [ ] JSON ファイルに蓄積
- [ ] OpenAI API で日報生成

### Week 2: 自動化
- [ ] cron / launchd で毎分実行
- [ ] 1日の終わりに自動で日報生成
- [ ] Discord / Slack に投稿

### Week 3: Rust化
- [ ] Rust でスクリーンショット + OCR
- [ ] 低リソース常駐プロセス

---

## 5. プライバシー配慮

- **ローカル完結**: 画像はクラウドに送らない
- **フィルタリング**: パスワード入力画面は除外（active_window で判定）
- **自動削除**: 1週間以上前の画像は削除

---

## 6. 拡張アイデア

- **ポモドーロ連携**: 25分ごとに集中度を判定
- **生産性スコア**: コーディング時間 vs SNS時間
- **週報/月報**: 日報を集約

---

*作成日: 2025-12-26*
