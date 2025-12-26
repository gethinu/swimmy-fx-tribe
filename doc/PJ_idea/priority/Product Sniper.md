# Product Sniper (型番スナイパー)

**旧名**: Camp Sniper
**ピボット理由**: キャンプは季節性あり。カメラ・家電は年中需要。

---

## 1. 事業コンセプト

**「型番が決まっている高単価商品」の価格変動を監視し、最安値を通知 → 楽天アフィリエイト**

### ターゲット商品カテゴリ

| カテゴリ | 例 | 楽天アフィリ料率 |
|----------|-----|-----------------|
| **カメラ** | Sony α7IV, Canon R6 | 2-4% |
| **レンズ** | SIGMA 24-70mm | 2-4% |
| **PC周辺機器** | MacBook, ThinkPad | 2-4% |
| **オーディオ** | AirPods Max, Sony WH-1000XM5 | 2-4% |
| **ゲーム機** | PS5, Nintendo Switch | 1-2% |
| **調理家電** | Vitamix, バルミューダ | 2-4% |

**高単価（5万円〜）× 型番検索 = 誤購入リスク低い + アフィリ額大きい**

---

## 2. システム構成 (Swimmy流用)

### Architecture (The Lisp-Rust Axis)

1. **The Price Sentinel (Rust)**:
   - **役割**: 楽天市場APIを定期スキャン、価格変動を検知
   - **技術**: `reqwest`, `tokio`, `serde`
   - **Logic**: 
     - 価格が過去30日平均より10%以上下落 → アラート
     - 在庫復活（品切れ→在庫あり）→ アラート

2. **The Opportunity Brain (Common Lisp)**:
   - **役割**: 商品の「買い時スコア」を算出
   - **Logic**:
     - `(calculate-opportunity product current-price avg-price stock-status)`
     - ポイント還元率、クーポン有無も考慮
     - 「今が底値」「ポイント倍率UP中」などのコンテキストを生成

3. **通知**: Discord Webhook / LINE Bot

---

## 3. 収益モデル

| 段階 | 方法 | 想定月収 |
|------|------|----------|
| **無料** | Twitter/Xで速報 + アフィリリンク | ¥5,000〜¥30,000 |
| **有料** | Discord有料チャンネル（月500円） | 100人 × ¥500 = ¥50,000 |
| **自動化** | 24時間監視Bot | メンテほぼゼロ |

---

## 4. 開発計画

### Week 1: MVP
- [ ] 楽天API連携
- [ ] 価格監視ループ（1時間ごと）
- [ ] Discord通知

### Week 2: 拡張
- [ ] 複数商品対応
- [ ] 価格履歴グラフ生成
- [ ] Twitter自動投稿

### Week 3: マネタイズ
- [ ] 有料Discordチャンネル設置
- [ ] LP作成（Notion or GitHub Pages）

---

## 5. 監視対象商品リスト（初期）

```lisp
(defparameter *watch-list*
  '((:name "Sony α7 IV" :jan "4548736134218" :msrp 328900)
    (:name "Canon EOS R6 Mark II" :jan "4549292197631" :msrp 339900)
    (:name "SIGMA 24-70mm F2.8 DG DN Art" :jan "0085126578657" :msrp 143000)
    (:name "Sony WH-1000XM5" :jan "4548736132610" :msrp 49500)
    (:name "AirPods Pro 2" :jan "0194253388777" :msrp 39800)
    (:name "PlayStation 5" :jan "4948872415552" :msrp 66980)))
```

---

## 6. 差別化ポイント

既存の価格比較サイト（価格.com等）との違い：

| 価格.com | Product Sniper |
|----------|----------------|
| 手動で見に行く | プッシュ通知 |
| 全商品 | 厳選した高単価のみ |
| 広告だらけ | クリーンなUI |
| 遅い | Rust製で爆速 |

---

*作成日: 2025-12-26*
