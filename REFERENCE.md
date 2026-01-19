# Swimmy System Reference

## Important Paths

### WSL (Linux)
| Component | Path |
|-----------|------|
| Brain (Lisp) | `/home/swimmy/swimmy/brain.lisp` |
| School | `/home/swimmy/swimmy/src/lisp/school.lisp` |
| DSL | `/home/swimmy/swimmy/src/lisp/dsl.lisp` |
| Guardian (Rust) | `/home/swimmy/swimmy/guardian/src/main.rs` |
| Run Script | `/home/swimmy/swimmy/run.sh` |
| Genome | `/home/swimmy/swimmy/genome.lisp` |
| Discord Bot | `/home/swimmy/swimmy/src/python/discord_bot.py` |
| Config | `/home/swimmy/swimmy/config/.env` |

### 1.2 Tribal Tiers & Rank Mapping (The Hierarchy)

 Strategies are organized into a strict hierarchy based on performance (Sharpe Ratio).

| Tier (Tribal) | Rank (System) | Sharpe | Role | Execution |
| :--- | :--- | :--- | :--- | :--- |
| **Elder** | `:legend` | > 2.0 | Governance | Allowed |
| **Battlefield** | `:veteran` (S) | > 1.0 | Real Trading | **Allowed** |
| **Training** | `:veteran` (A) | > 0.5 | Paper Trading | Blocked |
| **Initiate** | `:scout` (B) | > 0.0 | Monitoring | Blocked |
| **Unworthy** | `:scout` (C) | < 0.0 | Selection | Blocked |

> [!NOTE]
> **Battlefield Exception**: Strategies placed in the `:battlefield` Tier are treated as `:veteran` for execution purposes, even if their Rank database entry hasn't updated yet.


### Windows (MT5)
| Component | Path |
|-----------|------|
| **SwimmyBridge EA** | `C:\Users\stair\AppData\Roaming\MetaQuotes\Terminal\D0E8209F77C8CF37AD8BF550E51FF075\MQL5\Experts\SwimmyBridge.mq5` |
| MT5 Data | MT5 Experts フォルダ内 |

---

## Network Ports

| Port | Direction | Purpose |
|------|-----------|---------|
| 5557 | MT5 → Guardian | Market data (TICK) |
| 5558 | Guardian → Brain | Forwarded data |
| 5559 | Brain → Guardian | Commands |
| 5560 | Guardian → MT5 | Trade commands |

---

## Command Format (JSON)

### Trade Commands (Brain → MT5)
```json
{"action": "BUY", "symbol": "USDJPY", "volume": 0.05, "sl": 155.50, "tp": 156.00}
{"action": "SELL", "symbol": "USDJPY", "volume": 0.05, "sl": 156.00, "tp": 155.50}
{"action": "CLOSE", "symbol": "USDJPY"}
{"action": "CLOSE", "symbol": "USDJPY", "close_all": true}  ← 全ポジションクローズ
```

### Data Messages (MT5 → Brain)
```json
{"type": "TICK", "symbol": "USDJPY", "bid": 155.737}
{"type": "HISTORY", "symbol": "USDJPY", "data": [...]}
```

---

## Key Variables

### brain.lisp
- `*supported-symbols*` - 対応通貨ペア
- `*candle-histories*` - 通貨別キャンドル履歴
- `*daily-pnl*` - 日次損益
- `*daily-loss-limit*` - 日次損失上限 (デフォルト: -500)

### school.lisp
- `*category-trades*` - ウォームアップカウンタ (50まで)
- `*category-positions*` - カテゴリ別ポジション
- `*symbol-exposure*` - 通貨別エクスポージャー
- `*max-symbol-exposure*` - 最大エクスポージャー (0.15 = 15%)

---

## Troubleshooting

### NIL is not of type REAL
- 原因: bid/ask/indicator が NIL の場合
- 対策: `(when (numberp x) ...)` でガード

### コンパイル警告 (undefined variable)
- 原因: グローバル変数の前方宣言不足
- 対策: `(defvar *var-name* nil)` を追加

### ポジション孤児問題
- 原因: Brain再起動時にMT5ポジションを忘れる
- 対策: 起動時CLOSE_ALL送信 (Ver 15.0で実装済み)

---

## Version History
| Ver | Features |
|-----|----------|
| 14.0 | AlphaSwimmy - カテゴリトレード |
| 15.0 | Multi-Currency + Correlation Risk |
| 16.0 | Failure Learning System |
| 17.0 | Autonomous Evolution (全4システム統合) |
| 20.0 | ALL SYSTEMS MAX QUALITY |
| 21.0 | COMPLETE AUTONOMOUS EVOLUTION (全7システム) |
| 22.0 | TRUE SCHOOL OF FISH - 群れで動く |
| 23.0 | SMART ESCAPE SCHOOL - 危険回避 |
| 24.0 | GOAL SEEKER - Intent Trading基盤 |
| 25.0 | LEADER SCHOOL - リーダーフィッシュ |
| 26.0 | STRATEGIC MIND - 投了判断 |
| 27.0 | PREDICTIVE MIND - 評価値揺らぎ+終局予測 |
| 28.0 | EXPLAINABLE AI - 自己説明+リスクパリティ |
| 30.0 | INTENT TRADING - 究極のゴール |
| 31.0 | COMPLETE - 100% 基本思想実現 |
| 32.0 | OPUS INTEGRATION - AIパートナー統合 |
| 33.0 | DAILY HANDOFF - 継続的AIコラボ |
| 34.0 | AUTONOMOUS - 自律運用モード |
| 35.0 | MULTI-AGENT - Evaluator AI導入 |
| 36.0 | 2027 VISION - Constitution + Philosophy + Agent |
| 37.0 | CIVILIZATION - 4大氏族 + 儀式 + 部族文化 |
| 38.0 | QUALITY - Crypto + Tests + Macros + REPL |
| 38.1 | EXCELLENCE - Error Handling + Quality Metrics |
| 39.0 | **🏛️ CIVILIZATION COMPLETE - Hierarchy + Council + Economics** |

---

## Autonomous Evolution Systems (7システム)

### 1. Failure Learning v2.0 ⭐⭐⭐⭐⭐
- **15+コンテキスト変数**: レジーム、RSI、セッション、モメンタム等
- **時間減衰**: 指数減衰（半減期1時間）
- **確信度スコアリング**: 0-100%のリスク評価
- **ファジーマッチング**: 類似パターンも考慮

### 2. Self-Analysis v2.0 ⭐⭐⭐⭐⭐
- **構造化分析**: カテゴリ/セッション/レジーム別
- **戦略ランキング**: Sharpe比でソート
- **アクション可能なインサイト**: 具体的な推奨事項

### 3. Regime Forecast v2.0 ⭐⭐⭐⭐⭐
- **マルチ特徴分析**: トレンド強度、モメンタム
- **信頼度スコア**: 0-100%の予測信頼度
- **遷移マトリックス**: 状態遷移確率を学習

### 4. Meta-Learning v2.0 ⭐⭐⭐⭐⭐
- **Sharpe比追跡**: レジーム別パフォーマンス
- **Kelly基準**: リスク調整済みポジションサイジング
- **連勝/連敗追跡**: ストリーク考慮

### 5. Swarm Intelligence (群れの知恵) 🆕
- **戦略投票**: 全戦略が方向性に投票
- **重み付け投票**: Sharpeに基づく投票力
- **コンセンサス閾値**: 60%の合意で取引
- **マイノリティレポート**: 反対意見を記録

### 6. Memory System (記憶と想起) 🆕
- **エピソード記憶**: 具体的なトレード経験
- **セマンティック記憶**: パターンの一般化
- **類似検索**: 過去の類似状況を検索
- **経験ベース提案**: 過去の成功率に基づく方向提案

### 7. Ecosystem Dynamics (生態系) 🆕
- **多様性スコア**: シャノン指数で計算
- **ニッチバランス**: カテゴリ別の均衡維持
- **自然淘汰**: 低パフォーマー削除、高パフォーマー繁殖
- **生態系健康度**: 0-100%の総合スコア
