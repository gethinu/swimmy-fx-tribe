# 🦈 Swimmy FX-Tribe V3.0 システムアーキテクチャ

**更新日**: 2025-12-29 20:50  
**バージョン**: V3.0  

---

## 概要

Lispベースの自律進化型FX取引システム。4つの氏族（Clan）が協力し、61の戦略を基盤に学習・進化を続ける。

```
MCP Host (stdio JSON-RPC) -> MCP Server (Python) -> ZMQ 5559 -> Guardian (Rust)
MT5 <--ZeroMQ--> Guardian (Rust) <--ZeroMQ--> Brain (Lisp)
                     |                            |
                     +--> backtester.rs           +--> school.lisp (戦略)
                     +--> neural.rs               +--> dreamer2.lisp (進化)
                                                  +--> strategies.lisp (61戦略)
```

**プロトコル境界**
- MCP Host/Server: JSON-RPC（外部API）
- Brain/Guardian/MT5: **S式（alist）** が正本（ORDER_OPEN は `instrument` + `side`）

---

## @ 4大氏族 (The Four Great Clans)

| 氏族 | カテゴリ | シグナル関数 | 哲学 |
|------|----------|--------------|------|
| 🏹 Hunters | :trend | `get-hunter-signal` | MACD+ADX+Kalman |
| 🔮 Shamans | :reversion | `get-shaman-signal` | RSI+BB逆張り |
| ⚡ Breakers | :breakout | `get-breaker-signal` | ATRブレイクアウト |
| 🗡️ Raiders | :scalp | `get-raider-signal` | EMAクロス+スキャル |

---

## A 学習フィードバックループ

```
TRADE_CLOSED (brain.lisp:2165)
    │
    ├─→ record-trade-outcome()      → *failure-log* / *success-log*
    │                                      ↓
    │                               dreamer2.analyze-by-session()
    │
    ├─→ learn-from-failure()        → *elder-lessons* (失敗時のみ)
    │       └─ 減衰: 24時間で10%
    │
    ├─→ update-leader-stats()       → リーダー戦略のPnL追跡
    │
    └─→ store-memory()              → トレード記憶保存
```

---

## B 長老の知恵 (Elder Wisdom)

```lisp
;; 学習（失敗時）
(learn-from-failure context pnl)
  → *elder-lessons* にパターン追加
  → 閾値3回以上で警告発動

;; 投票（High Council時）
(elder-vote proposal context)
  → *elder-lessons* を参照
  → :approve / :caution / :reject を返す
  
;; 減衰（1日1回）
(decay-elder-lessons)
  → 全教訓を × 0.9
  → 0.5未満は削除
```

---

## C 61戦略への反映

```
backtest結果受信 (brain.lisp:2065)
    │
    ├─→ *evolved-strategies* から検索
    ├─→ *strategy-knowledge-base* から検索 ← 61戦略!
    │
    └─→ 自動パラメータ調整:
          - Sharpe < 0 → SL -10%, Volume -20%
          - Sharpe > 1.0, Win > 55% → TP +10%, Volume +20%
          - R:R < 2:1 → TP +5%
```

---

## D トレード実行フロー

```
execute-category-trade (school.lisp:2273)
    │
    ├─→ get-category-lot()           基本ロット
    ├─→ get-volatility-lot-multiplier() ボラ調整
    ├─→ get-risk-parity-lot()        リスクパリティ
    ├─→ calculate-rank-multiplier()  階級調整
    │
    ├─→ predict-trade-outcome()      予測
    ├─→ should-take-trade-p()        フィルタ
    ├─→ explain-trade-decision()     説明生成
    │
    └─→ ZeroMQ → Guardian → MT5     実行
```

---

## E 進化システム (Dreamer2)

```
evolve-population (dreamer2.lisp:417)
    │
    ├─→ seed-evolution-from-knowledge-base()  初期シード
    ├─→ get-ecosystem-recommendation()        弱ニッチ特定
    │
    ├─→ crossover-strategies()                交配
    ├─→ mutate-strategy()                     突然変異
    │
    └─→ request-clone-check()                 クローン検出
```

---

## F 朝の礼拝 (Morning Ritual)

```
morning-ritual (brain.lisp:2650)
    │
    ├─→ 憲法朗読
    ├─→ 4氏族の哲学表示
    ├─→ get-failure-summary()        失敗分析
    ├─→ get-hour-patterns()          時間帯パターン
    ├─→ analyze-swarm-accuracy()     群知能精度
    └─→ get-clan-treasury-summary()  財務状況
```

---

## G 主要パラメータ

| パラメータ | 値 | 場所 |
|------------|-----|------|
| `*monthly-goal*` | ¥10,000 | brain.lisp:92 |
| `*elder-decay-rate*` | 0.9 | brain.lisp:980 |
| `*elder-decay-interval*` | 86400秒 | brain.lisp:979 |
| `*swarm-consensus-threshold*` | 0.65 | school.lisp:1299 |
| `*min-sharpe-threshold*` | 1.0 | strategies.lisp:366 |

---

## H ファイル構成

| ファイル | 行数 | 主な責務 |
|----------|------|----------|
| brain.lisp | 2,744 | メイン処理、メッセージハンドラ |
| school.lisp | 2,766 | 戦略実行、氏族システム |
| dreamer2.lisp | 598 | 進化、バックテスト統合 |
| strategies.lisp | 400 | 61基本戦略定義 |
| research.lisp | 400 | 論文実装 |
| dsl.lisp | 200 | インジケータDSL |

**補足（アーカイブ）**
- `data/library/RETIRED/`：Max Age 退役アーカイブ
- `data/memory/retired.sexp`：低ウェイト学習用パターン

---

## I 最近の変更 (V3.0)

| 変更 | 効果 |
|------|------|
| 16ポジション対応 | 4氏族×4戦士 |
| *elder-lessons*減衰 | 古い教訓の忘却 |
| 11未使用関数接続 | 全機能アクティブ化 |
| announce-clan-trade削除 | 重複通知除去 |
| Ramen KPI | 月間目標¥10,000 |

---

## J 関連ドキュメント

- [EXPERT_REVIEW.md](EXPERT_REVIEW.md) - 専門家レビューと批評
